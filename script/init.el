;;; init --- configuration file

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

;; Python settings.

(setq company-jedi-port 8000)

(setq company-jedi-debug t)

(add-hook 'python-mode-hook 'company-jedi-start)

;; Company settings.

(require 'cl)

(global-company-mode)

(add-to-list 'company-backends 'company-jedi)
