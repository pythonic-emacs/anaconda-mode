;;; anaconda-mode.el --- Code navigation, documentation lookup and completion for Python

;; Copyright (C) 2013, 2014 by Malyshev Artem

;; Authors: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (json-rpc "0.0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'json-rpc)
(require 'etags)
(require 'python)
(require 'anaconda-nav)


;;; Server.

(defvar anaconda-mode-debug nil
  "Turn on anaconda_mode debug logging.")

(defvar anaconda-mode-host "localhost"
  "Target host with anaconda_mode server.")

(defvar anaconda-mode-port 24970
  "Port for anaconda_mode connection.")

(defun anaconda-mode-python ()
  "Detect python executable."
  (let ((virtualenv python-shell-virtualenv-path))
    (if virtualenv
        (concat (file-name-as-directory virtualenv) "bin/python")
      "python")))

(defun anaconda-mode-python-args ()
  "Python arguments to run anaconda_mode server."
  (delq nil (list "anaconda_mode.py"
                  "--ip" anaconda-mode-host
                  "--port" (number-to-string anaconda-mode-port)
                  (when anaconda-mode-debug "--debug"))))

(defun anaconda-mode-command ()
  "Shell command to run anaconda_mode server."
  (cons (anaconda-mode-python)
	(anaconda-mode-python-args)))

(defvar anaconda-mode-directory
  (file-name-directory load-file-name)
  "Directory containing anaconda_mode package.")

(defvar anaconda-mode-process nil
  "Currently running anaconda_mode process.")

(defvar anaconda-mode-connection nil
  "Json Rpc connection to anaconda_mode process.")

(defun anaconda-mode-running-p ()
  "Check for running anaconda_mode server."
  (and anaconda-mode-process
       (not (null (process-live-p anaconda-mode-process)))
       (json-rpc-live-p anaconda-mode-connection)))

(defun anaconda-mode-bootstrap ()
  "Run anaconda-mode-command process."
  (let ((default-directory anaconda-mode-directory))
    (setq anaconda-mode-process
          (apply 'start-process
                 "anaconda_mode"
                 "*anaconda*"
                 (anaconda-mode-python)
                 (anaconda-mode-python-args)))
    (accept-process-output anaconda-mode-process)
    (setq anaconda-mode-connection
          (json-rpc-connect anaconda-mode-host anaconda-mode-port))))

(defun anaconda-mode-start-node ()
  "Start anaconda_mode server."
  (when (anaconda-mode-need-restart)
    (anaconda-mode-stop-node))
  (unless (anaconda-mode-running-p)
    (anaconda-mode-bootstrap)))

(defun anaconda-mode-stop-node ()
  "Stop anaconda-mode server."
  (when (anaconda-mode-running-p)
    (kill-process anaconda-mode-process)
    (json-rpc-close anaconda-mode-connection)
    (setq anaconda-mode-connection nil)))

(defun anaconda-mode-need-restart ()
  "Check if current `anaconda-mode-process' need restart with new args.
Return nil if it run under proper environment."
  (and (anaconda-mode-running-p)
       (not (equal (process-command anaconda-mode-process)
                   (anaconda-mode-command)))))


;;; Interaction.

(defun anaconda-mode-call (command &rest args)
  "Make remote procedure call for COMMAND.
ARGS are COMMAND argument passed to remote call."
  (anaconda-mode-start-node)
  ;; use list since not all dash functions operate on vectors
  (let ((json-array-type 'list))
    (apply 'json-rpc anaconda-mode-connection command args)))

(defun anaconda-mode-call-1 (command)
  ;; TODO: Remove this function ones plugin system will be implemented.
  ;; See #28.
  (anaconda-mode-call
   command
   (buffer-substring-no-properties (point-min) (point-max))
   (line-number-at-pos (point))
   (current-column)
   (or (buffer-file-name) "")))


;;; Minor mode.

(defvar anaconda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-?") 'anaconda-mode-view-doc)
    (define-key map (kbd "M-r") 'anaconda-mode-usages)
    (define-key map [remap find-tag] 'anaconda-mode-goto-definitions)
    (define-key map [remap tags-loop-continue] 'anaconda-mode-goto-assignments)
    (define-key map [remap pop-tag-mark] 'anaconda-nav-pop-marker)
    map)
  "Keymap for `anaconda-mode'.")

;;;###autoload
(define-minor-mode anaconda-mode
  "Code navigation, documentation lookup and completion for Python.

\\{anaconda-mode-map}"
  :lighter " Anaconda"
  :keymap anaconda-mode-map
  (if anaconda-mode
      (add-hook 'completion-at-point-functions
                'anaconda-mode-complete-at-point nil t)
    (remove-hook 'completion-at-point-functions
                 'anaconda-mode-complete-at-point t)))


;;; Code completion.

(defun anaconda-mode-complete-at-point ()
  "Complete at point with anaconda_mode."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (stop (or (cdr bounds) (point))))
    (list start stop
          (completion-table-dynamic
           'anaconda-mode-complete-thing))))

(defun anaconda-mode-complete-thing (&rest ignored)
  "Complete python thing at point.
IGNORED parameter is the string for which completion is required."
  (mapcar (lambda (candidate) (plist-get candidate :name))
          (anaconda-mode-complete)))

(defun anaconda-mode-complete ()
  "Request completion candidates."
  (anaconda-mode-call-1 "complete"))


;;; View documentation.

(defun anaconda-mode-view-doc ()
  "Show documentation for context at point."
  (interactive)
  (display-buffer
   (anaconda-mode-doc-buffer (or (anaconda-mode-call-1 "doc")
                                 (error "No documentation found")))))

(defun anaconda-mode-doc-buffer (doc)
  "Display documentation buffer with contents DOC."
  (with-current-buffer (get-buffer-create "*anaconda-doc*")
    (view-mode -1)
    (erase-buffer)
    (insert doc)
    (view-mode 1)
    (current-buffer)))


;;; Usages.

(defun anaconda-mode-usages ()
  "Show usages for thing at point."
  (interactive)
  (anaconda-nav (or (anaconda-mode-call-1 "usages")
                    (error "No usages found"))))


;;; Definitions.

(defun anaconda-mode-goto-definitions ()
  "Goto definition for thing at point."
  (interactive)
  (anaconda-nav (or (anaconda-mode-call-1 "goto_definitions")
                    (error "No definition found"))
                t))


;;; Assignments.

(defun anaconda-mode-goto-assignments ()
  "Goto assignment for thing at point."
  (interactive)
  (anaconda-nav (or (anaconda-mode-call-1 "goto_assignments")
                    (error "No assignment found"))
                t))

(provide 'anaconda-mode)

;;; anaconda-mode.el ends here
