# My Personal Website

## Cheatsheet for running the site

- `cider-jack-in`
- `cider-load-buffer`
- `C-c C-c`: send defn to REPL
- `C-c C-e`: send last expression to REPL
- `cider-quit`

The bottom of `core.clj` contains some useful forms for evaling:
- **Development.** Start a dev server. Point your browser at it. Refresh the browser to see the latest content.
- **Publication.** Write the site to the `_gh-pages` directory.

## Initializing the `_gh-pages` directory

- Run from the root of the repo:
  - `git worktree add _gh-pages gh-pages`