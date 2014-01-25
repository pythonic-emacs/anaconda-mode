(require 'cl)
(require 'cask)

(cask-initialize
 (file-name-directory
  (directory-file-name
   (file-name-directory load-file-name))))

;; Python settings.

(setq company-jedi-port 8000)

(setq company-jedi-debug t)

(add-hook 'python-mode-hook 'company-jedi-start)

;; Company settings.

(global-company-mode)

(add-to-list 'company-backends 'company-jedi)
