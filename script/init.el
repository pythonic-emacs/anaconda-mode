;;; init --- configuration file

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

;; Anaconda settings.

(let ((envdir (getenv "ENVDIR")))
  (setq python-shell-virtualenv-path envdir))

(setq anaconda-mode-port 8887)

(setq anaconda-mode-debug t)

(require 'anaconda-mode)

(add-hook 'python-mode-hook 'anaconda-mode)

;; Company settings.

(require 'cl)

(global-company-mode)

(add-to-list 'company-backends 'company-anaconda)

;; ElDoc settings.

(require 'anaconda-eldoc)

(add-hook 'python-mode-hook 'anaconda-eldoc)
