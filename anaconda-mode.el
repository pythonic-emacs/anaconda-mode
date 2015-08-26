;;; anaconda-mode.el --- Code navigation, documentation lookup and completion for Python  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (pythonic "0.1.0") (dash "2.6.0") (s "1.9") (f "0.16.2"))

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

;; See the README for more details.

;;; Code:

(require 'tramp)
(require 'url)
(require 'json)
(require 'pythonic)
(require 'dash)
(require 's)
(require 'f)

(defgroup anaconda-mode nil
  "Code navigation, documentation lookup and completion for Python."
  :group 'programming)

(defcustom anaconda-mode-eldoc-as-single-line nil
  "If not nil, trim eldoc string to frame width."
  :group 'anaconda-mode
  :type 'boolean)


;;; Server.

(defvar anaconda-mode-server-version "0.1.1"
  "Server version needed to run anaconda-mode.")

(defvar anaconda-mode-server-directory
  (f-join "~" ".emacs.d" "anaconda-mode" anaconda-mode-server-version)
  "Anaconda mode installation directory.")

(defvar anaconda-mode-server-script "anaconda_mode.py"
  "Script file with anaconda-mode server.")

(defvar anaconda-mode-process-name "anaconda-mode"
  "Process name for anaconda-mode processes.")

(defvar anaconda-mode-process-buffer "*anaconda-mode*"
  "Buffer name for anaconda-mode processes.")

(defvar anaconda-mode-process nil
  "Currently running anaconda-mode process.")

(defvar anaconda-mode-port nil
  "Port for anaconda-mode connection.")

(defvar anaconda-mode-ensure-directory-command
  (list
   "-c" "
import os
import sys
directory = sys.argv[1]
if not os.path.exists(directory):
    os.makedirs(directory)
" anaconda-mode-server-directory)
  "Create `anaconda-mode-server-directory' if necessary.")

(defvar anaconda-mode-check-installation-command
  (list "-c" "
from pkg_resources import get_distribution
def check_deps(deps=['anaconda_mode']):
    for each in deps:
        distrib = get_distribution(each)
        requirements = distrib.requires()
        check_deps(requirements)
check_deps()
")
  "Check if `anaconda-mode' server is installed or not.")

(defvar anaconda-mode-install-server-command
  (list "-m" "pip" "install" "-t" "."
        (concat "anaconda_mode" "=="
                anaconda-mode-server-version))
  "Install `anaconda_mode' server.")

(defun anaconda-mode-host ()
  "Target host with anaconda-mode server."
  (if (pythonic-remote-p)
      (tramp-file-name-host
       (tramp-dissect-file-name
        (pythonic-tramp-connection)))
    "127.0.0.1"))

(defun anaconda-mode-start (&optional callback)
  "Start anaconda-mode server.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."
  (when (anaconda-mode-need-restart)
    (anaconda-mode-stop))
  (if (anaconda-mode-running-p)
      (when callback
        (funcall callback))
    (anaconda-mode-ensure-directory callback)))

(defun anaconda-mode-stop ()
  "Stop anaconda-mode server."
  (when (anaconda-mode-running-p)
    (set-process-filter anaconda-mode-process nil)
    (set-process-sentinel anaconda-mode-process nil)
    (kill-process anaconda-mode-process)
    (setq anaconda-mode-process nil
          anaconda-mode-port nil)))

(defun anaconda-mode-running-p ()
  "Is `anaconda-mode' server running."
  (and anaconda-mode-process
       (process-live-p anaconda-mode-process)))

(defun anaconda-mode-bound-p ()
  "Is `anaconda-mode' port bound."
  (numberp anaconda-mode-port))

(defun anaconda-mode-need-restart ()
  "Check if we need to restart `anaconda-mode-server'."
  (when (anaconda-mode-running-p)
    (not (pythonic-proper-environment-p anaconda-mode-process))))

(defun anaconda-mode-ensure-directory (&optional callback)
  "Ensure if `anaconda-mode-server-directory' exists.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."
  (setq anaconda-mode-process
        (start-pythonic :process anaconda-mode-process-name
                        :buffer anaconda-mode-process-buffer
                        :sentinel (lambda (process event) (anaconda-mode-ensure-directory-sentinel process event callback))
                        :args anaconda-mode-ensure-directory-command)))

(defun anaconda-mode-ensure-directory-sentinel (process event &optional callback)
  "Run `anaconda-mode-check' if `anaconda-mode-server-directory' exists.
Raise error otherwise.  PROCESS and EVENT are basic sentinel
parameters.  CALLBACK function will be called when
`anaconda-mode-port' will be bound."
  (if (eq 0 (process-exit-status process))
      (anaconda-mode-check callback)
    (pop-to-buffer anaconda-mode-process-buffer)
    (error "Can't create %s directory" anaconda-mode-server-directory)))

(defun anaconda-mode-check (&optional callback)
  "Check `anaconda-mode' server installation.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."
  (setq anaconda-mode-process
        (start-pythonic :process anaconda-mode-process-name
                        :buffer anaconda-mode-process-buffer
                        :cwd anaconda-mode-server-directory
                        :sentinel (lambda (process event) (anaconda-mode-check-sentinel process event callback))
                        :args anaconda-mode-check-installation-command)))

(defun anaconda-mode-check-sentinel (process event &optional callback)
  "Run `anaconda-mode-bootstrap' if server installation check passed.
Try to install `anaconda-mode' server otherwise.  PROCESS and
EVENT are basic sentinel parameters.  CALLBACK function will be
called when `anaconda-mode-port' will be bound."
  (if (eq 0 (process-exit-status process))
      (anaconda-mode-bootstrap callback)
    (anaconda-mode-install callback)))

(defun anaconda-mode-install (&optional callback)
  "Try to install `anaconda-mode' server.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."
  (setq anaconda-mode-process
        (start-pythonic :process anaconda-mode-process-name
                        :buffer anaconda-mode-process-buffer
                        :cwd anaconda-mode-server-directory
                        :sentinel (lambda (process event) (anaconda-mode-install-sentinel process event callback))
                        :args anaconda-mode-install-server-command)))

(defun anaconda-mode-install-sentinel (process event &optional callback)
  "Run `anaconda-mode-bootstrap' if server installation complete successfully.
Raise error otherwise.  PROCESS and EVENT are basic sentinel
parameters.  CALLBACK function will be called when
`anaconda-mode-port' will be bound."
  (if (eq 0 (process-exit-status process))
      (anaconda-mode-bootstrap callback)
    (pop-to-buffer anaconda-mode-process-buffer)
    (error "Can't install `anaconda-mode' server")))

(defun anaconda-mode-bootstrap (&optional callback)
  "Run `anaconda-mode' server.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."
  (setq anaconda-mode-process
        (start-pythonic :process anaconda-mode-process-name
                        :buffer anaconda-mode-process-buffer
                        :cwd anaconda-mode-server-directory
                        :filter (lambda (process output) (anaconda-mode-bootstrap-filter process output callback))
                        :sentinel 'anaconda-mode-bootstrap-sentinel
                        :query-on-exit nil
                        :args (list anaconda-mode-server-script))))

(defun anaconda-mode-bootstrap-filter (process output &optional callback)
  "Set `anaconda-mode-port' from PROCESS OUTPUT.
Connect to the `anaconda-mode' server.  CALLBACK function will be
called when `anaconda-mode-port' will be bound."
  ;; Mimic default filter.
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point)))))
  (--when-let (s-match "anaconda_mode port \\([0-9]+\\)" output)
    (setq anaconda-mode-port (string-to-number (cadr it)))
    (set-process-filter process nil)
    (when callback
      (funcall callback))))

(defun anaconda-mode-bootstrap-sentinel (process event)
  "Raise error if `anaconda-mode' server exit abnormally.
PROCESS and EVENT are basic sentinel parameters."
  (unless (eq 0 (process-exit-status process))
    (pop-to-buffer anaconda-mode-process-buffer)
    (error "Can't start `anaconda-mode' server")))


;;; Interaction.

(defun anaconda-mode-call (command callback)
  "Make remote procedure call for COMMAND.
Apply CALLBACK to it result."
  (anaconda-mode-start
   (lambda () (anaconda-mode-jsonrpc command callback))))

(defun anaconda-mode-jsonrpc (command callback)
  "Perform JSONRPC call for COMMAND.
Apply CALLBACK to the call result when retrieve it.  Remote
COMMAND must expect four arguments: python buffer content, line
number position, column number position and file path."
  (let ((url-request-method "POST")
        (url-request-data (anaconda-mode-jsonrpc-request command)))
    (url-retrieve
     (format "http://%s:%s" (anaconda-mode-host) anaconda-mode-port)
     (anaconda-mode-create-response-handler command callback)
     nil
     t)))

(defun anaconda-mode-jsonrpc-request (command)
  "Prepare JSON encoded buffer data for COMMAND call."
  (json-encode (anaconda-mode-jsonrpc-request-data command)))

(defun anaconda-mode-jsonrpc-request-data (command)
  "Prepare buffer data for COMMAND call."
  `((jsonrpc . "2.0")
    (id . 1)
    (method . ,command)
    (params . ((source . ,(buffer-substring-no-properties (point-min) (point-max)))
               (line . ,(line-number-at-pos (point)))
               (column . ,(- (point) (line-beginning-position)))
               (path . ,(when (buffer-file-name)
                          (pythonic-file-name (buffer-file-name))))))))

(defun anaconda-mode-create-response-handler (command callback)
  "Create server response handler based on COMMAND and CALLBACK function.
COMMAND argument will be used for response skip message.
Response can be skipped if point was moved sense request was
submitted."
  (let ((anaconda-mode-request-point (point))
        (anaconda-mode-request-buffer (current-buffer))
        (anaconda-mode-request-window (selected-window))
        (anaconda-mode-request-tick (buffer-chars-modified-tick)))
    (lambda (status)
      (unwind-protect
          (if (or (not (equal anaconda-mode-request-window (selected-window)))
                  (with-current-buffer (window-buffer anaconda-mode-request-window)
                    (or (not (equal anaconda-mode-request-buffer (current-buffer)))
                        (not (equal anaconda-mode-request-point (point)))
                        (not (equal anaconda-mode-request-tick (buffer-chars-modified-tick))))))
              (message "Skip anaconda-mode %s response" command)
            (goto-char url-http-end-of-headers)
            (let* ((json-array-type 'list)
                   (response (json-read)))
              (if (assoc 'error response)
                  (error (cdr (assoc 'error response)))
                (with-current-buffer anaconda-mode-request-buffer
                  ;; Terminate `apply' call with empty list so response
                  ;; will be treated as single argument.
                  (apply callback (cdr (assoc 'result response)) nil)))))
        (kill-buffer (current-buffer))))))


;;; Code completion.

(defun anaconda-mode-complete ()
  "Request completion candidates."
  (interactive)
  (unless (python-syntax-comment-or-string-p)
    (anaconda-mode-call "complete" 'anaconda-mode-complete-callback)))

(defun anaconda-mode-complete-callback (result)
  "Start interactive completion on RESULT receiving."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (stop (or (cdr bounds) (point)))
         (collection (anaconda-mode-complete-extract-names result))
         (completion-extra-properties '(:annotation-function anaconda-mode-complete-annotation)))
    (completion-in-region start stop collection)))

(defun anaconda-mode-complete-extract-names (result)
  "Extract completion names from anaconda-mode RESULT."
  (--map (let ((name (cdr (assoc 'name it)))
               (description (s-replace "\n" "" (cdr (assoc 'description it)))))
           (put-text-property 0 1 'description description name)
           name)
         result))

(defun anaconda-mode-complete-annotation (candidate)
  "Get annotation for CANDIDATE."
  (--when-let (get-text-property 0 'description candidate)
    (concat " <" it ">")))


;;; View documentation.

(defun anaconda-mode-view-doc ()
  "Show documentation for context at point."
  (interactive)
  (anaconda-mode-call "goto_definitions" 'anaconda-mode-view-doc-callback))

(defun anaconda-mode-view-doc-callback (result)
  "Process view doc RESULT."
  (when result
    (pop-to-buffer
     (anaconda-mode-create-view-doc-buffer
      (anaconda-mode-format-view-doc-content result)))))

(defun anaconda-mode-create-view-doc-buffer (doc)
  "Display documentation buffer with contents DOC."
  (let ((buf (get-buffer-create "*Anaconda*")))
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer)
      (insert doc)
      (goto-char (point-min))
      (view-mode 1)
      buf)))

(defun anaconda-mode-format-view-doc-content (result)
  "Create content for documentation buffer from RESULT fields."
  (->>
   result
   (--map (anaconda-mode-view-doc-extract-definition it))
   (--map (apply 's-concat it))
   (-map 's-trim)
   (s-join "\n\n")
   (s-append "\n")))

(defun anaconda-mode-view-doc-extract-definition (definition)
  "Extract module DEFINITION from the response structure."
  (let ((map (make-sparse-keymap))
        (handler (anaconda-mode-view-doc-make-click-handler definition)))
    (define-key map (kbd "RET") handler)
    (define-key map (kbd "<mouse-2>") handler)
    (list (propertize
           (cdr (assoc 'module-name definition))
           'face 'bold
           'mouse-face 'highlight
           'help-echo "mouse-2: visit this module"
           'keymap map)
          "\n"
          (cdr (assoc 'docstring definition)))))

(defun anaconda-mode-view-doc-make-click-handler (definition)
  "Create interactive event handler to open DEFINITION module path."
  (lambda ()
    (interactive)
    (find-file (cdr (assoc 'module-path definition)))
    (goto-char 0)
    (forward-line (1- (cdr (assoc 'line definition))))
    (forward-char (cdr (assoc 'column definition)))))


;;; Definitions processing.

(defun anaconda-mode-format-definitions-view (result)
  "Create definitions buffer content from rpc RESULT."
  (->>
   (--group-by (cdr (assoc 'module-name it)) result)
   (--map (anaconda-mode-format-definition-module it))
   (apply 's-concat)))

(defun anaconda-mode-format-definition-module (module)
  "Format MODULE definition view."
  (let ((module-name (car module))
        (definitions (--map (concat "    " (cdr (assoc 'description it)))
                            (cdr module))))
    (->> definitions
         (cons module-name)
         (s-join "\n")
         (s-append "\n"))))


;;; Eldoc.

(defun anaconda-mode-eldoc-function ()
  "Show eldoc for context at point."
  (anaconda-mode-call "eldoc" 'anaconda-mode-eldoc-callback))

(defun anaconda-mode-eldoc-callback (result)
  "Display eldoc from server RESULT."
  (eldoc-message (anaconda-mode-eldoc-format result)))

(defun anaconda-mode-eldoc-format (result)
  "Format eldoc string from RESULT."
  (when result
    (let* ((name (cdr (assoc 'name result)))
           (index (cdr (assoc 'index result)))
           (params (cdr (assoc 'params result)))
           (doc (anaconda-mode-eldoc-format-definition name index params)))
      (if anaconda-mode-eldoc-as-single-line
          (substring doc 0 (min (frame-width) (length doc)))
        doc))))

(defun anaconda-mode-eldoc-format-definition (name index params)
  "Format function definition from NAME, INDEX and PARAMS."
  (concat
   (propertize name 'face 'font-lock-function-name-face)
   "("
   (anaconda-mode-eldoc-format-params params index)
   ")"))

(defun anaconda-mode-eldoc-format-params (args index)
  "Build colorized ARGS string with current arg pointed to INDEX."
  (->>
   args
   (--map-indexed
    (if (= index it-index)
        (propertize it 'face 'eldoc-highlight-function-argument)
      it))
   (-interpose ", ")
   (apply 'concat)))


;;; Anaconda minor mode.

(defvar anaconda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-i") 'anaconda-mode-complete)
    (define-key map (kbd "M-?") 'anaconda-mode-view-doc)
    (define-key map (kbd "M-r") 'anaconda-mode-find-usages)
    (define-key map (kbd "M-.") 'anaconda-mode-find-definition)
    (define-key map (kbd "M-*") 'anaconda-mode-pop-mark)
    map)
  "Keymap for `anaconda-mode'.")

;;;###autoload
(define-minor-mode anaconda-mode
  "Code navigation, documentation lookup and completion for Python.

\\{anaconda-mode-map}"
  :lighter " Anaconda"
  :keymap anaconda-mode-map
  (if anaconda-mode
      (turn-on-anaconda-mode)
    (turn-off-anaconda-mode)))

(defun turn-on-anaconda-mode ()
  "Turn on `anaconda-mode'."
  (make-local-variable 'eldoc-documentation-function)
  (setq-local eldoc-documentation-function 'anaconda-mode-eldoc-function))

(defun turn-off-anaconda-mode ()
  "Turn off `anaconda-mode'."
  (kill-local-variable 'eldoc-documentation-function))

(provide 'anaconda-mode)

;;; anaconda-mode.el ends here
