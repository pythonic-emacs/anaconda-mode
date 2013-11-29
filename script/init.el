(require 'cl)
(require 'cask)

(cask-initialize
 (file-name-directory
  (directory-file-name
   (file-name-directory load-file-name))))

(eval-after-load "company"
  '(progn
     (add-to-list 'company-backends 'company-jedi)))

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-d") 'company-jedi-show-doc)
     (define-key python-mode-map (kbd "M-r") 'company-jedi-find-references)
     (define-key python-mode-map (kbd "M-.") 'company-jedi-goto-definition)
     (define-key python-mode-map (kbd "M-,") 'pop-tag-mark)))

(add-hook 'python-mode-hook 'company-jedi-start)

(setq company-jedi-port 8000)

(setq company-jedi-command
      (concat (file-name-as-directory user-emacs-directory)
              "jedi/venv/bin/python3 -m start_jedi --debug"))

(global-company-mode)
