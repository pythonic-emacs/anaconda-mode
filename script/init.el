(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

;; Anaconda settings.

(require 'anaconda-mode)

(let ((envdir (getenv "ENVDIR")))
  (setq python-shell-virtualenv-path envdir))

(setq anaconda-mode-port 8887)

(setq anaconda-mode-debug t)

(add-hook 'python-mode-hook 'anaconda-mode)

;; ElDoc settings.

(require 'anaconda-eldoc)

(add-hook 'python-mode-hook 'anaconda-eldoc)

;; Pyenv settings.

(pyenv-mode)
