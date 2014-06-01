(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

;; Package setup.

(require 'anaconda-mode)

(let ((envdir (getenv "ENVDIR")))
  (setq python-shell-virtualenv-path envdir))

(setq anaconda-mode-port 8887)
(setq anaconda-mode-debug t)

;; Helper functions.

(defun load-fixture (filename source)
  (setq buffer-file-name (expand-file-name filename))
  (insert source)
  (search-backward "_|_")
  (delete-char 3))
