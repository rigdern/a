 (ns rigdern.blog.core
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as s]
   [hiccup.page :as hp]
   [hiccup2.core :as h]
   ring.adapter.jetty
   ring.middleware.content-type
   [ring.util.response :as ring-resp])
  (:import
   (com.vladsch.flexmark.ext.footnotes FootnoteExtension)
   (com.vladsch.flexmark.ext.tables TablesExtension)
   (com.vladsch.flexmark.ext.toc TocExtension)
   (com.vladsch.flexmark.html HtmlRenderer)
   (com.vladsch.flexmark.parser Parser)
   (com.vladsch.flexmark.util.data MutableDataSet)))

(defn fail [& strings]
  (throw (Exception. (apply str strings))))

(defn make-path [& path-components]
  (java.nio.file.Paths/get (first path-components) (into-array String (rest path-components))))

(defn file-extension
  "Returns the extension of the file represented by the java.nio.file.Path,
  excluding the leading '.'. Returns nil if the file has no extension."
  [file-path]
  (let [file-name (.toString (.getFileName file-path))]
    (when-let [index (s/last-index-of file-name ".")]
      (subs file-name (inc index)))))

(defn set-file-extension
  "Returns a java.nio.file.Path with `extension` rather than whatever extension
  `file-path` had."
  [file-path extension]
  (let [file-name (.toString (.getFileName file-path))
        extensionless-file-name (if-let [index (s/last-index-of file-name ".")]
                                  (subs file-name 0 index)
                                  file-name)]
    (make-path (if-let [parent-path (.getParent file-path)]
                 (.toString parent-path)
                 "")
               (str extensionless-file-name "." extension))))

(defn git-has-local-changes? []
  (not= 0 (:exit (shell/sh "git" "diff-index" "--quiet" "HEAD"))))

(defn git [& args]
  (let [result (apply shell/sh "git" args)]
    (when (not= 0 (:exit result))
      (throw (Exception. (str "git command failed: " (:exit result) ". " (:err result)))))
    (:out result)))

(defn git-head-symbol []
  (s/trim (git "rev-parse" "--symbolic-full-name" "main")))

(defn git-head-sha []
  (s/trim (git "rev-parse" "HEAD")))

(let [options (doto (MutableDataSet.)
                (.set Parser/EXTENSIONS [(FootnoteExtension/create)
                                         (TablesExtension/create)
                                         (TocExtension/create)]))
      parser (.build (Parser/builder options))
      renderer (.build (HtmlRenderer/builder options))]
  (defn markdown->html [markdown]
    (.render renderer (.parse parser markdown))))

(def css
  (s/join
   "\n"
   ["html,"
    "body {"
    "  margin: 0;"
    "  padding: 0;"
    "}"
    
    "body {"
    "  font-family: \"Computer Modern\", Georgia, \"Times New Roman\", Times, serif;"
    "  font-size: 18px;"
    "}"
    
    ".title {"
    "  background-color: #0f52ba;"
    "  padding: 10px;"
    "  margin-bottom: 5px;"
    "}"
    
    ".title-text {"
    "  display: block;"
    "  margin-left: auto;"
    "  margin-right: auto;"
    "  max-width: 700px;"
    "  color: #ffffff;"
    "  font-size: 20px;"
    "  text-decoration: none;"
    "}"

    ".title-text:hover {"
    "  text-decoration: underline;"
    "}"
    
    ".content {"
    "  max-width: 700px;"
    "  margin-left: auto;"
    "  margin-right: auto;"
    "  padding-left: 10px;"
    "  padding-right: 10px;"
    "}"

    ".comments-link {"
    "  display: inline-block;"
    "  margin-top: 30px;"
    "  margin-bottom: 18px;"
    "}"
    ]))

(defn render-page [title body]
  (str
   (hp/doctype :html5)
   (h/html
    [:html
     [:head
      [:title title]
      [:meta {:charset "UTF-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
      [:style (h/raw css)]]
     [:body
      body]])))

(defn render-article [article]
  (let [meta (:meta article)
        dates-str (->> [(if (:published meta) (str "Published: " (:published meta)))
                        (if (:updated meta) (str "Updated: " (:updated meta)))]
                       (remove nil?)
                       (s/join ", "))
        dates-el (if (not-empty dates-str) [:div dates-str])]
    (render-page
     (str (:title meta) " - Learning About Learning")
     [:div
                                        ;[:a {:href "articles.html"} "On Learning"]
      [:div {:class "title"}
       [:a {:class "title-text" :href "./index.html"} "Learning About Learning"]]
      [:div {:class "content"}
       [:h1 (:title meta)]
       dates-el
       (h/raw (markdown->html (:content article)))
       (if (:comments-id meta)
         [:a {:class "comments-link" :href (str "https://github.com/rigdern/comments/issues/" (:comments-id meta))}
          "View Comments"])]])))

(defn parse-article [content]
  (when (not= 0 (s/index-of content "```")) (throw (Exception. "Article must begin with ```")))
  (let [end-index (s/index-of content "```" 3)]
    (when-not end-index (throw (Exception. "Article missing closing ```")))
    (let [edn-string (subs content 3 end-index)
          body (subs content (+ end-index 3))]
      {:meta (read-string edn-string)
       :content body})))

(defn article-handler [path]
  (fn []
    (-> (slurp path)
        parse-article
        render-article)))

(defn render-nojekyll []
  "")

(defn render-version-txt []
  (str 
   (git-head-sha)
   (when (git-has-local-changes?) " with local changes")))

(defn get-pages
  "Recursively walks `dir-path` returning a map of request paths to functions
   which return the content for that path. Given the path of a request, this map
   enables you to generate the appropriate content for that request."
  [dir-path]
  (let [pages (reduce (fn [acc file]
                        (when (.isFile file)
                          (let [request-path (.relativize dir-path (.toPath file))]
                            (cond
                              (= (file-extension request-path) "md")
                              (assoc acc (.toString (set-file-extension request-path "html")) (article-handler (.getAbsolutePath file)))))))
                      {}
                      (file-seq (.toFile dir-path)))]
    pages))

(defn get-all-pages [dir-path]
  (merge (get-pages dir-path)
         ;; By default, GitHub Pages assumes you're using Jekyll and does some special
         ;; processing. As far as I know, we're not currently affected by this. To
         ;; avoid gotchas in the future, create a .nojekyll file to disable this
         ;; special processing. For more details see:
         ;; https://github.blog/2009-12-29-bypassing-jekyll-on-github-pages/
         {".nojekyll" render-nojekyll
          "version.txt" render-version-txt}))

(defn not-found-response [req]
  (ring-resp/content-type (ring-resp/not-found (str "Not found: " req)) "text/plain"))

(defn make-handler [pages]
  (fn [request]
    (if (s/ends-with? (:uri request) "/")
      {:status 301 ; Moved Permanently
       :headers {"Location" (str (:uri request) "index.html")}}
      (if-let [page-handler (get pages (subs (:uri request) 1))] ; drop leading / from path
        {:status 200 ; OK
         :body (page-handler)}
        (not-found-response (:uri request))))))

(defn delete-directory [^java.io.File file file-filter]
  (when (.isDirectory file)
    (doseq [file-in-dir (.listFiles file file-filter)]
      (delete-directory file-in-dir file-filter)))
  (println (str "Deleting " (.getAbsolutePath file)))
  (io/delete-file file))

(defn empty-directory [^java.io.File directory file-filter]
  (when (.isDirectory directory)
    (doseq [file-in-dir (.listFiles directory file-filter)]
      (delete-directory file-in-dir file-filter))))

(def skip-git-directory-file-filter
  (reify
    java.io.FileFilter
    (accept [this file]
      (not= ".git" (.getName file)))))

(defn write-pages [pages out-dir-file]
  (doseq [[page-path render-page] pages]
    (let [page-file (io/file out-dir-file page-path)]
      (.mkdirs (.getParentFile page-file))
      (println (str "Writing " (.getAbsolutePath page-file)))
      (spit page-file (render-page)))))

;; Public
;;

(def server nil)
(defn start-dev-server [pages port]
  (when server (.stop server))
  (def server (ring.adapter.jetty/run-jetty
               (ring.middleware.content-type/wrap-content-type (make-handler pages))
               {:port port
                :join? false})))


(def current-dir (make-path (System/getProperty "user.dir")))
(def content-dir (make-path (.toString current-dir) "content"))
(def gh-pages-dir (make-path (.toString current-dir) "_gh-pages"))
(def dev-server-port 3000)

(defn prepare-gh-pages
  ([] (prepare-gh-pages {:skip-git-assertions? false}))
  ([opts]
   (let [expected-gh-pages-dir (.toString
                                (make-path (System/getProperty "user.home")
                                           "code/projects/rigdern.github.io/_gh-pages"))]
     (when-not (= (.toString gh-pages-dir) expected-gh-pages-dir)
       ;; Safety net to ensure we don't do destructive actions such as deleting or
       ;; writing to the wrong directory.
       (fail "Unexpected value for `gh-pages-dir`. Expected: "
             expected-gh-pages-dir
             ". Actual: "
             (.toString gh-pages-dir))))
   
   (if-not (:skip-git-assertions? opts)
     (when (git-has-local-changes?)
       (fail "git repo has local changes"))
     (when (not= "refs/heads/main" (git-head-symbol))
       (fail "git repo isn't on main branch")))
   
   (let [gh-pages-dir-file (.toFile gh-pages-dir)]
     (.mkdir gh-pages-dir-file) ; Ensure the directory exists
     (empty-directory gh-pages-dir-file skip-git-directory-file-filter)
     (write-pages (get-all-pages content-dir) gh-pages-dir-file))))

(defn -main []
  (prepare-gh-pages)
  
  ;; Shutdown the thread pools that back the agent system so that the process
  ;; can exit. Without this, it'll take 60 seconds for the process to exit.
  ;; This is due to our usage of clojure.java.shell/sh which uses futures.
  ;; See:
  ;;   - The notes in https://clojuredocs.org/clojure.java.shell/sh
  ;;   - https://clojure.atlassian.net/browse/CLJ-959
  (shutdown-agents))

(comment
  
  ;; Run:
  ;;   - to (re)start the dev server.
  ;;   - after adding/removing files from the content dir.
  (start-dev-server (get-all-pages content-dir) dev-server-port)

  ;; Run to write the static site to the file system to prepare for publication.
  (prepare-gh-pages)

  (prepare-gh-pages {:skip-git-assertions? true})
  
  )
