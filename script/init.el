;;; init.el --- minimal anaconda-mode configuration

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(require 'anaconda-mode)

(add-hook 'python-mode-hook 'anaconda-mode)

(add-hook 'python-mode-hook 'eldoc-mode)

(pyenv-mode)

(provide 'init)

;;; init.el ends here
