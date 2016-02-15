;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(undercover "anaconda-mode.el" (:report-file "emacs-coveralls.json") (:send-report nil))

(require 'anaconda-mode)
(require 'eldoc)

(setq python-indent-guess-indent-offset nil)

(setq tramp-verbose 2)

(defun ert-anaconda-mode-message-fail-process-message ()
  "Print failed process output."
  (if (get-buffer anaconda-mode-process-buffer)
      (with-current-buffer anaconda-mode-process-buffer
        (message (buffer-string)))
    (message "No buffer named *anaconda-mode*")))

(add-hook 'anaconda-mode-process-fail-hook 'ert-anaconda-mode-message-fail-process-message)

(add-hook 'anaconda-mode-response-read-fail-hook 'message)

(defun wait ()
  "Wait for `anaconda-mode' server start."
  (while (not (anaconda-mode-bound-p))
    (sleep-for 0.5)))

(defun run (&rest args)
  "Run python interpreter synchronously with ARGS passed directly to it."
  (call-pythonic :args `("-c" ,@args)))

(defun run-to-string (&rest args)
  "Run python interpreter synchronously with ARGS.
Return process output."
  (let ((buffer (generate-new-buffer-name "*out*")))
    (call-pythonic :buffer buffer :args `("-c" ,@args))
    (with-current-buffer buffer
      (buffer-string))))

(defvar home-directory (f-full "~")
  "User home directory.")

(defun fixture (source line column &optional path)
  "Open SOURCE fixture.
Put point on LINE at COLUMN position.  Set PATH as current file
name."
  (with-current-buffer (generate-new-buffer "*fixture*")
    (python-mode)
    (insert source)
    (goto-char 0)
    (forward-line (1- line))
    (forward-char column)
    (setq buffer-file-name path)
    (switch-to-buffer (current-buffer))
    (current-buffer)))

(defmacro ert-defintegration (testname args doc &rest body)
  "Generate `ert' test with proper setup and teardown machinery."
  (declare (indent 2))
  `(ert-deftest ,testname ,args
     ,doc
     (unwind-protect
         (progn
           ,@body)
       (when anaconda-mode-process
         (anaconda-mode-stop)
         (sleep-for 0.5))
       (mapc 'kill-buffer
             (cl-remove-if-not 'get-buffer
                               '("*anaconda-mode*" "*anaconda-response*" "*Anaconda*" "*Completions*")))
       (shell-command "rm -rf $HOME/.emacs.d/anaconda-mode/")
       (shell-command "rm -rf $HOME/.emacs.d/anaconda_mode/")
       (when (pythonic-remote-p)
         (shell-command "ssh test@127.0.0.1 'rm -rf $HOME/.emacs.d/anaconda-mode/'")
         (shell-command "ssh test@127.0.0.1 'rm -rf $HOME/.emacs.d/anaconda_mode/'"))
       (when tramp-current-connection
         (setq tramp-current-connection nil)
         (sleep-for 0.5)))))

(provide 'test-helper)

;;; test-helper.el ends here
