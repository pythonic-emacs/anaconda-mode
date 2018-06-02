;;; init.el --- minimal anaconda-mode configuration

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(require 'anaconda-mode)

(add-hook 'python-mode-hook 'anaconda-mode)

(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(load-theme 'zenburn t)

(add-to-list 'default-frame-alist '(font . "Liberation Mono-14"))

(menu-bar-mode -1)

(scroll-bar-mode -1)

(tool-bar-mode -1)

(blink-cursor-mode -1)

;;; init.el ends here
