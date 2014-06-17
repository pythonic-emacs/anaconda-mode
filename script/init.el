(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(require 'anaconda-mode)

(let ((envdir (getenv "ENVDIR")))
  (setq python-shell-virtualenv-path envdir))

(setq anaconda-mode-port 8887)

(setq anaconda-mode-debug t)

(add-hook 'python-mode-hook 'anaconda-mode)

(add-hook 'python-mode-hook 'eldoc-mode)

(pyenv-mode)
