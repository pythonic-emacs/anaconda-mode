;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)

(require 'undercover)
(undercover "anaconda-mode.el" (:report-file "emacs-coveralls.json"))

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(provide 'test-helper)

;;; test-helper.el ends here
