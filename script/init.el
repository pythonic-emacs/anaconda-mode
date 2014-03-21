;;; init --- configuration file

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(require 'anaconda-mode)

;; Python settings.

(when (getenv "ENVPYTHON")
  (setq anaconda-mode-python-bin (getenv "ENVPYTHON"))
  (message "Python binary: %s" (getenv "ENVPYTHON")))

(setq anaconda-mode-port 8000)

(setq anaconda-mode-debug t)

(add-hook 'python-mode-hook 'anaconda-mode-start)

;; Company settings.

(require 'cl)

(global-company-mode)

(add-to-list 'company-backends 'company-anaconda)
