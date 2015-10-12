;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(undercover "anaconda-mode.el" (:report-file "emacs-coveralls.json") (:send-report nil))

(setq python-indent-guess-indent-offset nil)

(when (string= "ipython" (getenv "PYENV_VERSION"))
  (setq python-shell-interpreter "ipython"))

(defun ert-anaconda-mode-message-fail-process-message ()
  "Print failed process output."
  (if (get-buffer anaconda-mode-process-buffer)
      (with-current-buffer anaconda-mode-process-buffer
        (message (buffer-string)))
    (message "No buffer named *anaconda-mode*")))

(add-hook 'anaconda-mode-process-fail-hook 'ert-anaconda-mode-message-fail-process-message)

(provide 'test-helper)

;;; test-helper.el ends here
