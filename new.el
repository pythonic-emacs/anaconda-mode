(defun anaconda-mode-call-prompt (command text)
  "Make remote procedure call for COMMAND for some TEXT."
  (unless anaconda-mode-remote-p
    (anaconda-mode-start))
  (anaconda-mode-connect)
  ;; use list since not all dash functions operate on vectors
  (let ((json-array-type 'list)
	(buffer-string (buffer-substring-no-properties (point-min) (point-max)))
	contents line column)
    (with-temp-buffer
      (insert buffer-string "\n" text)
      (goto-char (point-max))
      (setq contents (buffer-substring-no-properties (point-min) (point-max))
	    line (line-number-at-pos (point))
	    column (- (point) (line-beginning-position))))
    (json-rpc
     anaconda-mode-connection
     command
     contents
     line
     column
     (anaconda-mode-file-name))))

(defun anaconda-mode-goto-definitions-prompt ()
  (interactive)
  (anaconda-nav-navigate
   (or (anaconda-mode-call "goto_definitions")
       (anaconda-mode-call-prompt "goto_definitions"
				  (read-string "Object: "))
       (error "No definitions found"))
   t))
