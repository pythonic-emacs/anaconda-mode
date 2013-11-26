(require 'cl)
(require 'company)

(setq company-jedi-port 8000)

(setq company-jedi-command
      (concat (file-name-as-directory user-emacs-directory)
              "jedi/venv/bin/python3 -m start_jedi --debug"))

(add-to-list 'company-backends 'company-jedi)
(add-hook 'python-mode-hook 'company-jedi-start)

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "M-.") 'company-jedi-goto-definition)
     (define-key python-mode-map (kbd "M-,") 'pop-tag-mark)))

(global-company-mode)
