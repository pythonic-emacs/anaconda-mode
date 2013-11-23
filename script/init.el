(require 'cl)
(require 'company)

(add-to-list 'company-backends 'company-jedi)
(add-hook 'python-mode-hook 'company-jedi-start)

(global-company-mode)
