;;; anaconda-mode.el --- Code navigation, documentation lookup and completion for Python  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2018 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.15
;; Package-Requires: ((emacs "25.1") (pythonic "0.1.0") (dash "2.6.0") (s "1.9") (f "0.16.2"))

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

(require 'ansi-color)
(require 'pythonic)
(require 'tramp)
(require 'xref)
(require 'json)
(require 'dash)
(require 'url)
(require 's)
(require 'f)

(defgroup anaconda nil
  "Code navigation, documentation lookup and completion for Python."
  :group 'programming)

(defcustom anaconda-mode-installation-directory
  (locate-user-emacs-file "anaconda-mode")
  "Installation directory for `anaconda-mode' server."
  :type 'directory)

(defcustom anaconda-mode-eldoc-as-single-line nil
  "If not nil, trim eldoc string to frame width."
  :type 'boolean)

(defcustom anaconda-mode-lighter " Anaconda"
  "Text displayed in the mode line when `anaconda-mode’ is active."
  :type 'sexp)

(defcustom anaconda-mode-localhost-address "127.0.0.1"
  "Address used by `anaconda-mode' to resolve localhost."
  :type 'string)

(defcustom anaconda-mode-doc-frame-background (face-attribute 'default :background)
  "Doc frame background color, default color is current theme's background."
  :type 'string)

(defcustom anaconda-mode-doc-frame-foreground (face-attribute 'default :foreground)
  "Doc frame foreground color, default color is current theme's foreground."
  :type 'string)

(defcustom anaconda-mode-use-posframe-show-doc nil
  "If the value is not nil, use posframe to show eldoc."
  :type 'boolean)

(defcustom anaconda-mode-tunnel-setup-sleep 2
  "Time in seconds `anaconda-mode' waits after tunnel creation before first RPC call."
  :type 'integer)

(defcustom anaconda-mode-sync-request-timeout 2
  "Time in seconds `anaconda-mode' waits for a synchronous response."
  :type 'integer)

;;; Compatibility

;; Functions from posframe which is an optional dependency
(declare-function posframe-workable-p "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-show "posframe")

;;; Server.
(defvar anaconda-mode-server-version "0.1.15"
  "Server version needed to run `anaconda-mode'.")

(defvar anaconda-mode-process-name "anaconda-mode"
  "Process name for `anaconda-mode' processes.")

(defvar anaconda-mode-process-buffer "*anaconda-mode*"
  "Buffer name for `anaconda-mode' process.")

(defvar anaconda-mode-process nil
  "Currently running `anaconda-mode' process.")

(defvar anaconda-mode-response-buffer "*anaconda-response*"
  "Buffer name for error report when `anaconda-mode' fail to read server response.")

(defvar anaconda-mode-socat-process-name "anaconda-socat"
  "Process name for `anaconda-mode' socat companion process.")

(defvar anaconda-mode-socat-process-buffer "*anaconda-socat*"
  "Buffer name for `anaconda-mode' socat companion process.")

(defvar anaconda-mode-socat-process nil
  "Currently running `anaconda-mode' socat companion process.")

(defvar anaconda-mode-ssh-process-name "anaconda-ssh"
  "Process name for `anaconda-mode' ssh port forward companion process.")

(defvar anaconda-mode-ssh-process-buffer "*anaconda-ssh*"
  "Buffer name for `anaconda-mode' ssh port forward companion process.")

(defvar anaconda-mode-ssh-process nil
  "Currently running `anaconda-mode' ssh port forward companion process.")

(defvar anaconda-mode-doc-frame-name "*Anaconda Posframe*"
  "The posframe to show anaconda documentation.")

(defvar anaconda-mode-frame-last-point 0
  "The last point of anaconda doc view frame, use for hide frame after move point.")

(defvar anaconda-mode-frame-last-scroll-offset 0
  "The last scroll offset when show doc view frame, use for hide frame after window scroll.")

(defun anaconda-mode-server-directory ()
  "Anaconda mode installation directory."
  (f-short (f-join anaconda-mode-installation-directory
                   anaconda-mode-server-version)))

(defun anaconda-mode-host ()
  "Target host with `anaconda-mode' server."
  (cond
   ((pythonic-remote-docker-p)
    anaconda-mode-localhost-address)
   ((pythonic-remote-p)
    (pythonic-remote-host))
   (t
    anaconda-mode-localhost-address)))

(defun anaconda-mode-port ()
  "Port for `anaconda-mode' connection."
  (process-get anaconda-mode-process 'port))

(defun anaconda-mode-start (&optional callback)
  "Start `anaconda-mode' server.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."
  (when (anaconda-mode-need-restart)
    (anaconda-mode-stop))
  (if (anaconda-mode-running-p)
      (and callback
           (anaconda-mode-bound-p)
           (funcall callback))
    (anaconda-mode-bootstrap callback)))

(defun anaconda-mode-stop ()
  "Stop `anaconda-mode' server."
  (when (anaconda-mode-running-p)
    (set-process-filter anaconda-mode-process nil)
    (set-process-sentinel anaconda-mode-process nil)
    (kill-process anaconda-mode-process)
    (setq anaconda-mode-process nil))
  (when (anaconda-mode-socat-running-p)
    (kill-process anaconda-mode-socat-process)
    (setq anaconda-mode-socat-process nil))
  (when (anaconda-mode-ssh-running-p)
    (kill-process anaconda-mode-ssh-process)
    (setq anaconda-mode-ssh-process nil)))

(defun anaconda-mode-running-p ()
  "Is `anaconda-mode' server running."
  (and anaconda-mode-process
       (process-live-p anaconda-mode-process)))

(defun anaconda-mode-socat-running-p ()
  "Is `anaconda-mode' socat companion process running."
  (and anaconda-mode-socat-process
       (process-live-p anaconda-mode-socat-process)))

(defun anaconda-mode-ssh-running-p ()
  "Is `anaconda-mode' ssh port forward companion process running."
  (and anaconda-mode-ssh-process
       (process-live-p anaconda-mode-ssh-process)))

(defun anaconda-mode-bound-p ()
  "Is `anaconda-mode' port bound."
  (numberp (anaconda-mode-port)))

(defun anaconda-mode-need-restart ()
  "Check if we need to restart `anaconda-mode-server'."
  (when (and (anaconda-mode-running-p)
             (anaconda-mode-bound-p))
    (not (and (equal (process-get anaconda-mode-process 'interpreter)
                     python-shell-interpreter)
              (equal (process-get anaconda-mode-process 'virtualenv)
                     python-shell-virtualenv-root)
              (equal (process-get anaconda-mode-process 'remote-p)
                     (pythonic-remote-p))
              (if (pythonic-local-p)
                  t
                (equal (process-get anaconda-mode-process 'remote-method)
                       (pythonic-remote-method))
                (equal (process-get anaconda-mode-process 'remote-user)
                       (pythonic-remote-user))
                (equal (process-get anaconda-mode-process 'remote-host)
                       (pythonic-remote-host))
                (equal (process-get anaconda-mode-process 'remote-port)
                       (pythonic-remote-port)))))))

(defun anaconda-mode-get-server-process-cwd ()
  "Get the working directory for starting the anaconda server process.

The current working directory ends up being on sys.path, which may
result in conflicts with stdlib modules.

When running python from the local machine, we start the server
process from `anaconda-mode-installation-directory'.
This function creates that directory if it doesn't exist yet."
  (when (pythonic-local-p)
    (unless (file-directory-p anaconda-mode-installation-directory)
      (make-directory anaconda-mode-installation-directory t))
    anaconda-mode-installation-directory))

(defun anaconda-mode-server-command-args ()
  "Return list of arguments to start anaconda-mode server.

Passes local file anaconda-mode.py if local, or uses python
module as string if connecting through TRAMP.

Arguments are:
1. anaconda-mode.py (local) or -c anaconda-mode.py string (remote)
2. anaconda-mode-server-directory
3. anaconda-mode-localhost-address (local) or 0.0.0.0 (remote)
4. python-shell-virtualenv-root or empty string if not set"
  (let ((server-command-file (concat (file-name-directory (locate-library "anaconda-mode")) "anaconda-mode.py"))
        (arg-list (list (anaconda-mode-server-directory)
                        (if (pythonic-remote-p)
                            "0.0.0.0"
                          anaconda-mode-localhost-address)
                        (or python-shell-virtualenv-root "") ))
        server-command)
    (if (pythonic-remote-p)
        (with-temp-buffer
          (insert-file-contents server-command-file)
          (setq server-command (list "-c" (buffer-string))))
      (setq server-command (list server-command-file)))
    (append server-command arg-list)))


(defun anaconda-mode-bootstrap (&optional callback)
  "Run `anaconda-mode' server.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."
  (setq anaconda-mode-process
        (pythonic-start-process :process anaconda-mode-process-name
                                :cwd (anaconda-mode-get-server-process-cwd)
                                :buffer (get-buffer-create anaconda-mode-process-buffer)
                                :query-on-exit nil
                                :filter (lambda (process output)
                                          (anaconda-mode-bootstrap-filter process output callback))
                                :sentinel (lambda (_process _event))
                                :args (anaconda-mode-server-command-args)))
  (process-put anaconda-mode-process 'interpreter python-shell-interpreter)
  (process-put anaconda-mode-process 'virtualenv python-shell-virtualenv-root)
  (process-put anaconda-mode-process 'port nil)
  (when (pythonic-remote-p)
    (process-put anaconda-mode-process 'remote-p t)
    (process-put anaconda-mode-process 'remote-method (pythonic-remote-method))
    (process-put anaconda-mode-process 'remote-user (pythonic-remote-user))
    (process-put anaconda-mode-process 'remote-host (pythonic-remote-host))
    (process-put anaconda-mode-process 'remote-port (pythonic-remote-port))))

(defun anaconda-jump-proxy-string ()
  "Create -J option string for SSH tunnel."
  (let ((dfn
         (tramp-dissect-file-name (pythonic-aliased-path default-directory))))
    (when (tramp-file-name-hop dfn)
      (let ((hop-list (split-string (tramp-file-name-hop dfn) "|"))
            (result "-J "))
        (delete "" hop-list) ;; remove empty string after final pipe
        (dolist (elt hop-list result)
          ;; tramp-dissect-file-name expects a filename so give it dummy.file
          (let ((ts (tramp-dissect-file-name (concat "/" elt ":/dummy.file"))))
            (setq result (concat result
                                 (format "%s@%s:%s,"
                                         (tramp-file-name-user ts)
                                         (tramp-file-name-host ts)
                                         (or (tramp-file-name-port-or-default ts) 22))))))
        ;; Remove final comma
        (substring result 0 -1)))))

(defun anaconda-mode-bootstrap-filter (process output &optional callback)
  "Set `anaconda-mode-port' from PROCESS OUTPUT.
Connect to the `anaconda-mode' server.  CALLBACK function will be
called when `anaconda-mode-port' will be bound."
  ;; Mimic default filter.
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (process-mark process))
        (insert (ansi-color-apply output))
        (set-marker (process-mark process) (point)))))
  (unless (anaconda-mode-bound-p)
    (--when-let (s-match "anaconda_mode port \\([0-9]+\\)" output)
      (process-put anaconda-mode-process 'port (string-to-number (cadr it)))
      (cond ((pythonic-remote-docker-p)
             (let* ((container-raw-description (with-output-to-string
                                                 (with-current-buffer
                                                     standard-output
                                                   (call-process "docker" nil t nil "inspect" (pythonic-remote-host)))))
                    (container-description (let ((json-array-type 'list))
                                             (json-read-from-string container-raw-description)))
                    (container-ip (cdr (assoc 'IPAddress
                                              (cdadr (assoc 'Networks
                                                            (cdr (assoc 'NetworkSettings
                                                                        (car container-description)))))))))
               (setq anaconda-mode-socat-process
                     (start-process anaconda-mode-socat-process-name
                                    anaconda-mode-socat-process-buffer
                                    "socat"
                                    (format "TCP4-LISTEN:%d" (anaconda-mode-port))
                                    (format "TCP4:%s:%d" container-ip (anaconda-mode-port))))
               (set-process-query-on-exit-flag anaconda-mode-socat-process nil)))
            ((pythonic-remote-ssh-p)
             (let ((jump (anaconda-jump-proxy-string)))
               (message (format "Anaconda Jump Proxy: %s" jump))
               (setq anaconda-mode-ssh-process
                     (if jump
                         (start-process anaconda-mode-ssh-process-name
                                        anaconda-mode-ssh-process-buffer
                                        "ssh" jump "-nNT"
                                        "-L" (format "%s:localhost:%s" (anaconda-mode-port) (anaconda-mode-port))
                                        (format "%s@%s" (pythonic-remote-user) (pythonic-remote-host))
                                        "-p" (number-to-string (or (pythonic-remote-port) 22)))
                       (apply 'start-process
                              anaconda-mode-ssh-process-name
                              anaconda-mode-ssh-process-buffer
                              "ssh" "-nNT"
                              "-L" (format "%s:localhost:%s" (anaconda-mode-port) (anaconda-mode-port))
                              (if (pythonic-remote-user)
                                  (format "%s@%s" (pythonic-remote-user) (pythonic-remote-host))
                                ;; Asssume remote host is an ssh alias
                                (pythonic-remote-host))
                              ;; Pass in port only if it exists (might be included in ssh alias)
                              (when-let ((port (pythonic-remote-port)))
                                '("-p" (number-to-string port))))))
               ;; prevent race condition between tunnel setup and first use
               (sleep-for anaconda-mode-tunnel-setup-sleep)
               (set-process-query-on-exit-flag anaconda-mode-ssh-process nil))))
      (when callback
        (funcall callback)))))


;;; Interaction.

(defun anaconda-mode-call (command callback)
  "Make remote procedure call for COMMAND.
Apply CALLBACK to the result asynchronously."
  (anaconda-mode-start
   (lambda () (anaconda-mode-jsonrpc command callback))))

(defun anaconda-mode-call-sync (command callback)
  "Make remote procedure call for COMMAND.
Apply CALLBACK to the result synchronously."
  (let ((start-time (current-time))
        (result 'pending))
    (anaconda-mode-call
     command
     (lambda (r) (setq result r)))
    (while (eq result 'pending)
      (accept-process-output nil 0.01)
      (when (> (cadr (time-subtract (current-time) start-time))
               anaconda-mode-sync-request-timeout)
        (error "%s request timed out" command)))
    (funcall callback result)))

(defun anaconda-mode-jsonrpc (command callback)
  "Perform JSONRPC call for COMMAND.
Apply CALLBACK to the call result when retrieve it.  Remote
COMMAND must expect four arguments: python buffer content, line
number position, column number position and file path."
  (let ((url-request-method "POST")
        (url-request-data (anaconda-mode-jsonrpc-request command)))
    (url-retrieve
     (format "http://%s:%s" anaconda-mode-localhost-address (anaconda-mode-port))
     (anaconda-mode-create-response-handler callback)
     nil
     t)))

(defun anaconda-mode-jsonrpc-request (command)
  "Prepare JSON encoded buffer data for COMMAND call."
  (encode-coding-string (json-encode (anaconda-mode-jsonrpc-request-data command)) 'utf-8))

(defun anaconda-mode-jsonrpc-request-data (command)
  "Prepare buffer data for COMMAND call."
  `((jsonrpc . "2.0")
    (id . 1)
    (method . ,command)
    (params . ((source . ,(buffer-substring-no-properties (point-min) (point-max)))
               (line . ,(line-number-at-pos (point)))
               (column . ,(- (point) (line-beginning-position)))
               (path . ,(when (buffer-file-name)
                          (pythonic-python-readable-file-name (buffer-file-name))))))))

(defun anaconda-mode-create-response-handler (callback)
  "Create server response handler based on CALLBACK function."
  (let ((anaconda-mode-request-point (point))
        (anaconda-mode-request-buffer (current-buffer))
        (anaconda-mode-request-window (selected-window))
        (anaconda-mode-request-tick (buffer-chars-modified-tick)))
    (lambda (status)
      (let ((http-buffer (current-buffer)))
        (unwind-protect
            (if (or (not (equal anaconda-mode-request-window (selected-window)))
                    (with-current-buffer (window-buffer anaconda-mode-request-window)
                      (or (not (equal anaconda-mode-request-buffer (current-buffer)))
                          (not (equal anaconda-mode-request-point (point)))
                          (not (equal anaconda-mode-request-tick (buffer-chars-modified-tick))))))
                nil
              (search-forward-regexp "\r?\n\r?\n" nil t)
              (let ((response (condition-case nil
                                  (json-read)
                                ((json-readtable-error json-end-of-file end-of-file)
                                 (let ((response (concat (format "# status: %s\n# point: %s\n" status (point))
                                                         (buffer-string))))
                                   (with-current-buffer (get-buffer-create anaconda-mode-response-buffer)
                                     (erase-buffer)
                                     (insert response)
                                     (goto-char (point-min)))
                                   nil)))))
                (if (null response)
                    (message "Cannot read anaconda-mode server response")
                  (if (assoc 'error response)
                      (let* ((error-structure (cdr (assoc 'error response)))
                             (error-message (cdr (assoc 'message error-structure)))
                             (error-data (cdr (assoc 'data error-structure)))
                             (error-template (concat (if error-data "%s: %s" "%s")
                                                     " - see " anaconda-mode-process-buffer
                                                     " for more information.")))
                        (apply 'message error-template (delq nil (list error-message error-data))))
                    (with-current-buffer anaconda-mode-request-buffer
                      (let ((result (cdr (assoc 'result response))))
                        ;; Terminate `apply' call with empty list so response
                        ;; will be treated as single argument.
                        (condition-case nil
                               (apply callback result nil)
                             (quit nil))))))))
          (kill-buffer http-buffer))))))


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
  "Extract completion names from `anaconda-mode' RESULT."
  (--map (let ((name (aref it 0))
               (type (aref it 1)))
           (put-text-property 0 1 'type type name)
           name)
         result))

(defun anaconda-mode-complete-annotation (candidate)
  "Get annotation for CANDIDATE."
  (--when-let (get-text-property 0 'type candidate)
    (concat " <" it ">")))


;;; View documentation.

(defun anaconda-mode-show-doc ()
  "Show documentation for context at point."
  (interactive)
  (anaconda-mode-call "show_doc" 'anaconda-mode-show-doc-callback))

(defun anaconda-mode-show-doc-callback (result)
  "Process view doc RESULT."
  (if (> (length result) 0)
      (if (and anaconda-mode-use-posframe-show-doc
               (require 'posframe nil 'noerror)
               (posframe-workable-p))
          (anaconda-mode-documentation-posframe-view result)
        (pop-to-buffer (anaconda-mode-documentation-view result) t))
    (message "No documentation available")))

(defun anaconda-mode-documentation-view (result)
  "Show documentation view for rpc RESULT, and return buffer."
  (let ((buf (get-buffer-create "*Anaconda*")))
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer)
      (mapc
       (lambda (it)
         (insert (propertize (aref it 0) 'face 'bold))
         (insert "\n")
         (insert (s-trim-right (aref it 1)))
         (insert "\n\n"))
       result)
      (view-mode 1)
      (goto-char (point-min))
      buf)))

(defun anaconda-mode-documentation-posframe-view (result)
  "Show documentation view in posframe for rpc RESULT."
  (with-current-buffer (get-buffer-create anaconda-mode-doc-frame-name)
    (erase-buffer)
    (mapc
     (lambda (it)
       (insert (propertize (aref it 0) 'face 'bold))
       (insert "\n")
       (insert (s-trim-left (aref it 1)))
       (insert "\n\n"))
     result))
  (posframe-show anaconda-mode-doc-frame-name
                 :position (point)
                 :internal-border-width 10
                 :background-color anaconda-mode-doc-frame-background
                 :foreground-color anaconda-mode-doc-frame-foreground)
  (add-hook 'post-command-hook 'anaconda-mode-hide-frame)
  (setq anaconda-mode-frame-last-point (point))
  (setq anaconda-mode-frame-last-scroll-offset (window-start)))

(defun anaconda-mode-hide-frame ()
  "Hide posframe when window scroll or move point."
  (ignore-errors
    (when (get-buffer anaconda-mode-doc-frame-name)
      (unless (and (equal (point) anaconda-mode-frame-last-point)
                   (equal (window-start) anaconda-mode-frame-last-scroll-offset))
        (posframe-hide anaconda-mode-doc-frame-name)
        (remove-hook 'post-command-hook 'anaconda-mode-hide-frame)))))


;;; Find definitions.

(defun anaconda-mode-find-definitions ()
  "Find definitions for thing at point."
  (interactive)
  (anaconda-mode-call
   "infer"
   (lambda (result)
     (anaconda-mode-show-xrefs result nil "No definitions found"))))

(defun anaconda-mode-find-definitions-other-window ()
  "Find definitions for thing at point."
  (interactive)
  (anaconda-mode-call
   "infer"
   (lambda (result)
     (anaconda-mode-show-xrefs result 'window "No definitions found"))))

(defun anaconda-mode-find-definitions-other-frame ()
  "Find definitions for thing at point."
  (interactive)
  (anaconda-mode-call
   "infer"
   (lambda (result)
     (anaconda-mode-show-xrefs result 'frame "No definitions found"))))


;;; Find assignments.

(defun anaconda-mode-find-assignments ()
  "Find assignments for thing at point."
  (interactive)
  (anaconda-mode-call
   "goto"
   (lambda (result)
     (anaconda-mode-show-xrefs result nil "No assignments found"))))

(defun anaconda-mode-find-assignments-other-window ()
  "Find assignments for thing at point."
  (interactive)
  (anaconda-mode-call
   "goto"
   (lambda (result)
     (anaconda-mode-show-xrefs result 'window "No assignments found"))))

(defun anaconda-mode-find-assignments-other-frame ()
  "Find assignments for thing at point."
  (interactive)
  (anaconda-mode-call
   "goto"
   (lambda (result)
     (anaconda-mode-show-xrefs result 'frame "No assignments found"))))


;;; Find references.

(defun anaconda-mode-find-references ()
  "Find references for thing at point."
  (interactive)
  (anaconda-mode-call
   "get_references"
   (lambda (result)
     (anaconda-mode-show-xrefs result nil "No references found"))))

(defun anaconda-mode-find-references-other-window ()
  "Find references for thing at point."
  (interactive)
  (anaconda-mode-call
   "get_references"
   (lambda (result)
     (anaconda-mode-show-xrefs result 'window "No references found"))))

(defun anaconda-mode-find-references-other-frame ()
  "Find references for thing at point."
  (interactive)
  (anaconda-mode-call
   "get_references"
   (lambda (result)
     (anaconda-mode-show-xrefs result 'frame "No references found"))))


;;; Xref.

(defun anaconda-mode-xref-backend ()
  "Integrate `anaconda-mode' with xref."
  'anaconda)

(cl-defmethod xref-backend-definitions ((_backend (eql anaconda)) _identifier)
  "Find definitions for thing at point."
  (anaconda-mode-call-sync
   "infer"
   (lambda (result)
     (if result
         (if (stringp result)
             (progn
               (message result)
               nil)
           (anaconda-mode-make-xrefs result))))))

(cl-defmethod xref-backend-references ((_backend (eql anaconda)) _identifier)
  "Find references for thing at point."
  (anaconda-mode-call-sync
   "get_references"
   (lambda (result)
     (if result
         (if (stringp result)
             (progn
               (message result)
               nil)
           (anaconda-mode-make-xrefs result))))))

(cl-defmethod xref-backend-apropos ((_backend (eql anaconda)) _pattern)
  "Not implemented."
  nil)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql anaconda)))
  "Not implemented."
  nil)

(defun anaconda-mode-show-xrefs (result display-action error-message)
  "Show xref from RESULT using DISPLAY-ACTION.
Show ERROR-MESSAGE if result is empty."
  (if result
      (if (stringp result)
          (message result)
        (let ((xrefs (anaconda-mode-make-xrefs result)))
          (if (not (cdr xrefs))
              (progn
                (xref-push-marker-stack)
                (funcall (if (fboundp 'xref-pop-to-location)
                             'xref-pop-to-location
                           'xref--pop-to-location)
                         (cl-first xrefs)
                         display-action))
            (xref--show-xrefs (if (functionp 'xref--create-fetcher)
                                  (lambda (&rest _) xrefs)
                                xrefs)
                              display-action))))
    (message error-message)))

(defun anaconda-mode-make-xrefs (result)
  "Return a list of x-reference candidates created from RESULT."
  (--map
   (xref-make
    (aref it 3)
    (xref-make-file-location (pythonic-emacs-readable-file-name (aref it 0)) (aref it 1) (aref it 2)))
   result))


;;; Eldoc.

(defun anaconda-mode-eldoc-function ()
  "Show eldoc for context at point."
  (anaconda-mode-call "eldoc" 'anaconda-mode-eldoc-callback)
  ;; Don't show response buffer name as ElDoc message.
  nil)

(defun anaconda-mode-eldoc-callback (result)
  "Display eldoc from server RESULT."
  (eldoc-message (anaconda-mode-eldoc-format result)))

(defun anaconda-mode-eldoc-format (result)
  "Format eldoc string from RESULT."
  (when result
    (let ((doc (anaconda-mode-eldoc-format-definition
                (aref result 0)
                (aref result 1)
                (aref result 2))))
      (if anaconda-mode-eldoc-as-single-line
          (substring doc 0 (min (frame-width) (length doc)))
        doc))))

(defun anaconda-mode-eldoc-format-definition (name index params)
  "Format function definition from NAME, INDEX and PARAMS."
  (when index
    (aset params index (propertize (aref params index) 'face 'eldoc-highlight-function-argument)))
  (concat (propertize name 'face 'font-lock-function-name-face) "(" (mapconcat 'identity params ", ") ")"))


;;; Anaconda minor mode.

(defvar anaconda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-i") 'anaconda-mode-complete)
    (define-key map (kbd "M-.") 'anaconda-mode-find-definitions)
    (define-key map (kbd "C-x 4 .") 'anaconda-mode-find-definitions-other-window)
    (define-key map (kbd "C-x 5 .") 'anaconda-mode-find-definitions-other-frame)
    (define-key map (kbd "M-=") 'anaconda-mode-find-assignments)
    (define-key map (kbd "C-x 4 =") 'anaconda-mode-find-assignments-other-window)
    (define-key map (kbd "C-x 5 =") 'anaconda-mode-find-assignments-other-frame)
    (define-key map (kbd "M-r") 'anaconda-mode-find-references)
    (define-key map (kbd "C-x 4 r") 'anaconda-mode-find-references-other-window)
    (define-key map (kbd "C-x 5 r") 'anaconda-mode-find-references-other-frame)
    (define-key map (kbd "M-,") 'xref-pop-marker-stack)
    (define-key map (kbd "M-?") 'anaconda-mode-show-doc)
    map)
  "Keymap for `anaconda-mode'.")

;;;###autoload
(define-minor-mode anaconda-mode
  "Code navigation, documentation lookup and completion for Python.

\\{anaconda-mode-map}"
  :lighter anaconda-mode-lighter
  :keymap anaconda-mode-map
  (setq-local url-http-attempt-keepalives nil)
  (if anaconda-mode
      (add-hook 'xref-backend-functions #'anaconda-mode-xref-backend nil t)
    (remove-hook 'xref-backend-functions #'anaconda-mode-xref-backend t)))

;;;###autoload
(define-minor-mode anaconda-eldoc-mode
  "Toggle echo area display of Python objects at point."
  :lighter ""
  (if anaconda-eldoc-mode
      (turn-on-anaconda-eldoc-mode)
    (turn-off-anaconda-eldoc-mode)))

(defun turn-on-anaconda-eldoc-mode ()
  "Turn on `anaconda-eldoc-mode'."
  (make-local-variable 'eldoc-documentation-function)
  (setq-local eldoc-documentation-function 'anaconda-mode-eldoc-function)
  (eldoc-mode +1))

(defun turn-off-anaconda-eldoc-mode ()
  "Turn off `anaconda-eldoc-mode'."
  (kill-local-variable 'eldoc-documentation-function)
  (eldoc-mode -1))

(provide 'anaconda-mode)

;;; anaconda-mode.el ends here
