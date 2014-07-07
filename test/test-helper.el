;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(defun load-fixture (filename source)
  "Load FILENAME fixture filled with SOURCE."
  (setq buffer-file-name (expand-file-name filename))
  (insert source)
  (search-backward "_|_")
  (delete-char 3))

(provide 'test-helper)

;;; test-helper.el ends here
