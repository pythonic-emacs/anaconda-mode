;;; anaconda-mode.el --- Code navigation, documentation lookup and completion for Python  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2018 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.12
;; Package-Requires: ((emacs "26.1") (pythonic "0.1.0") (dash "2.6.0") (s "1.9") (f "0.16.2"))

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

(require 'pythonic)
(require 'tramp)
(require 'json)
(require 'dash)
(require 'url)
(require 's)
(require 'f)

(defgroup anaconda-mode nil
  "Code navigation, documentation lookup and completion for Python."
  :group 'programming)

(defcustom anaconda-mode-installation-directory
  "~/.emacs.d/anaconda-mode"
  "Installation directory for `anaconda-mode' server."
  :group 'anaconda-mode
  :type 'directory)

(defcustom anaconda-mode-complete-callback
  'anaconda-mode-complete-callback
  "Callback function used to display `anaconda-mode-complete' result."
  :group 'anaconda-mode
  :type 'function)

(defcustom anaconda-mode-show-doc-callback
  'anaconda-mode-show-doc-callback
  "Callback function used to display `anaconda-mode-show-doc' result."
  :group 'anaconda-mode
  :type 'function)

(defcustom anaconda-mode-find-definitions-callback
  'anaconda-mode-find-definitions-callback
  "Callback function used to display `anaconda-mode-find-definitions' result."
  :group 'anaconda-mode
  :type 'function)

(defcustom anaconda-mode-find-assignments-callback
  'anaconda-mode-find-assignments-callback
  "Callback function used to display `anaconda-mode-find-assignments' result."
  :group 'anaconda-mode
  :type 'function)

(defcustom anaconda-mode-find-references-callback
  'anaconda-mode-find-references-callback
  "Callback function used to display `anaconda-mode-find-references' result."
  :group 'anaconda-mode
  :type 'function)

(defcustom anaconda-mode-eldoc-callback
  'anaconda-mode-eldoc-callback
  "Callback function used to display `anaconda-mode-eldoc-function' result."
  :group 'anaconda-mode
  :type 'function)

(defcustom anaconda-mode-eldoc-as-single-line nil
  "If not nil, trim eldoc string to frame width."
  :group 'anaconda-mode
  :type 'boolean)

(defcustom anaconda-mode-lighter " Anaconda"
  "Text displayed in the mode line when `anaconda-mode’ is active."
  :group 'anaconda-mode
  :type 'sexp)


;;; Server.

(defvar anaconda-mode-server-version "0.1.12"
  "Server version needed to run `anaconda-mode'.")

(defvar anaconda-mode-server-command "
from __future__ import print_function

# CLI arguments.

import sys

assert len(sys.argv) > 3, 'CLI arguments: %s' % sys.argv

server_directory = sys.argv[-3]
server_address = sys.argv[-2]
virtual_environment = sys.argv[-1]

# Ensure directory.

import os

server_directory = os.path.expanduser(server_directory)

if not os.path.exists(server_directory):
    os.makedirs(server_directory)

# Installation check.

def instrument_installation():
    for path in os.listdir(server_directory):
        path = os.path.join(server_directory, path)
        if path.endswith('.egg') and os.path.isdir(path) and path not in sys.path:
            sys.path.insert(0, path)

missing_dependencies = []

instrument_installation()

try:
    import jedi
except ImportError:
    missing_dependencies.append('jedi>=0.12')

try:
    import service_factory
except ImportError:
    missing_dependencies.append('service_factory>=0.1.5')

# Installation.

if missing_dependencies:
    import site
    import setuptools.command.easy_install
    site.addsitedir(server_directory)
    setuptools.command.easy_install.main([
        '--install-dir', server_directory,
        '--site-dirs', server_directory,
        '--always-copy',
        '--always-unzip',
        *missing_dependencies
    ])
    instrument_installation()

# Setup server.

import jedi
import service_factory

assert jedi.__version__, 'Jedi version should be >= 0.12.0, current version: %s' % (
    jedi.__version__,
)

if virtual_environment:
    virtual_environment = jedi.create_environment(virtual_environment, safe=False)
else:
    virtual_environment = None

# Define JSON-RPC application.

import functools

def script_method(f):
    @functools.wraps(f)
    def wrapper(source, line, column, path):
        return f(jedi.Script(source, line, column, path, environment=virtual_environment))
    return wrapper

def process_definitions(f):
    @functools.wraps(f)
    def wrapper(script):
        return [{
            'name': definition.name,
            'type': definition.type,
            'module-name': definition.module_name,
            'module-path': definition.module_path,
            'line': definition.line,
            'column': definition.column,
            'docstring': definition.docstring(),
            'description': definition.description,
            'full-name': getattr(definition, 'full_name', definition.name),
        } for definition in f(script)]
    return wrapper

@script_method
@process_definitions
def complete(script):
    return script.completions()

@script_method
@process_definitions
def goto_definitions(script):
    return script.goto_definitions()

@script_method
@process_definitions
def goto_assignments(script):
    return script.goto_assignments()

@script_method
@process_definitions
def usages(script):
    return script.usages()

@script_method
def eldoc(script):
    signatures = script.call_signatures()
    if len(signatures) == 1:
        signature = signatures[0]
        return {
            'name': signature.name,
            'index': signature.index,
            'params': [param.description[6:] for param in signature.params],
        }

# Run.

app = [complete, goto_definitions, goto_assignments, usages, eldoc]

service_factory.service_factory(app, server_address, 0, 'anaconda_mode port {port}')
" "Run `anaconda-mode' server.")

(defvar anaconda-mode-process-name "anaconda-mode"
  "Process name for `anaconda-mode' processes.")

(defvar anaconda-mode-process-buffer "*anaconda-mode*"
  "Buffer name for `anaconda-mode' process.")

(defvar anaconda-mode-process nil
  "Currently running `anaconda-mode' process.")

(defvar anaconda-mode-port nil
  "Port for `anaconda-mode' connection.")

(defvar anaconda-mode-definition-commands
  '("complete" "goto_definitions" "goto_assignments" "usages")
  "List of `anaconda-mode' rpc commands returning definitions as result.

This is used to prefix `module-path' field with
`pythonic-tramp-connection' in the case of remote interpreter or
virtual environment.")

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

(defun anaconda-mode-server-directory ()
  "Anaconda mode installation directory."
  (f-short (f-join anaconda-mode-installation-directory
                   anaconda-mode-server-version)))

(defun anaconda-mode-host ()
  "Target host with `anaconda-mode' server."
  (cond
   ((pythonic-remote-docker-p)
    "127.0.0.1")
   ((pythonic-remote-p)
    (pythonic-remote-host))
   (t
    "127.0.0.1")))

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
    (setq anaconda-mode-process nil
          anaconda-mode-port nil))
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
  (numberp anaconda-mode-port))

(defun anaconda-mode-need-restart ()
  "Check if we need to restart `anaconda-mode-server'."
  (when (and (anaconda-mode-running-p)
             (anaconda-mode-bound-p))
    (or (not (pythonic-proper-environment-p anaconda-mode-process))
        (not (equal (process-get anaconda-mode-process 'server-directory)
                    (anaconda-mode-server-directory))))))

(defun anaconda-mode-bootstrap (&optional callback)
  "Run `anaconda-mode' server.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."
  (setq anaconda-mode-process
        (start-pythonic :process anaconda-mode-process-name
                        :buffer anaconda-mode-process-buffer
                        :filter (lambda (process output) (anaconda-mode-bootstrap-filter process output callback))
                        :query-on-exit nil
                        :args (list "-c"
                                    anaconda-mode-server-command
                                    (anaconda-mode-server-directory)
                                    (if (pythonic-remote-p) "0.0.0.0" "127.0.0.1")
                                    (or pythonic-environment ""))))
  (process-put anaconda-mode-process 'server-directory (anaconda-mode-server-directory)))

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
                                  (format "TCP4-LISTEN:%d" anaconda-mode-port)
                                  (format "TCP4:%s:%d" container-ip anaconda-mode-port)))
             (set-process-query-on-exit-flag anaconda-mode-socat-process nil)))
          ((pythonic-remote-vagrant-p)
           (setq anaconda-mode-ssh-process
                 (start-process anaconda-mode-ssh-process-name
                                anaconda-mode-ssh-process-buffer
                                "ssh" "-nNT"
                                (format "%s@%s" (pythonic-remote-user) (pythonic-remote-host))
                                "-p" (number-to-string (pythonic-remote-port))
                                "-L" (format "%s:%s:%s" anaconda-mode-port (pythonic-remote-host) anaconda-mode-port)))
           (set-process-query-on-exit-flag anaconda-mode-ssh-process nil)))
    (when callback
      (funcall callback))))


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
                          (if (pythonic-remote-p)
                              (and
                               (tramp-tramp-file-p (buffer-file-name))
                               (equal (tramp-file-name-host
                                       (tramp-dissect-file-name
                                        (pythonic-tramp-connection)))
                                      (tramp-file-name-host
                                       (tramp-dissect-file-name
                                        (buffer-file-name))))
                               (pythonic-file-name (buffer-file-name)))
                            (buffer-file-name))))))))

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
      (let ((http-buffer (current-buffer)))
        (unwind-protect
            (if (or (not (equal anaconda-mode-request-window (selected-window)))
                    (with-current-buffer (window-buffer anaconda-mode-request-window)
                      (or (not (equal anaconda-mode-request-buffer (current-buffer)))
                          (not (equal anaconda-mode-request-point (point)))
                          (not (equal anaconda-mode-request-tick (buffer-chars-modified-tick))))))
                nil
              (search-forward-regexp "\r?\n\r?\n" nil t)
              (let* ((json-array-type 'list)
                     (response (condition-case nil
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
                             (error-template (if error-data "%s: %s" "%s")))
                        (apply 'message error-template (delq nil (list error-message error-data))))
                    (with-current-buffer anaconda-mode-request-buffer
                      (let ((result (cdr (assoc 'result response))))
                        (when (and (pythonic-remote-p)
                                   (member command anaconda-mode-definition-commands))
                          (setq result (--map (--map (let ((key (car it))
                                                           (value (cdr it)))
                                                       (when (and (eq key 'module-path) value)
                                                         (setq value (concat (pythonic-tramp-connection) value)))
                                                       (cons key value))
                                                     it)
                                              result)))
                        ;; Terminate `apply' call with empty list so response
                        ;; will be treated as single argument.
                        (apply callback result nil)))))))
          (kill-buffer http-buffer))))))


;;; Code completion.

(defun anaconda-mode-complete ()
  "Request completion candidates."
  (interactive)
  (unless (python-syntax-comment-or-string-p)
    (anaconda-mode-call "complete" anaconda-mode-complete-callback)))

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
  (--map (let* ((name (cdr (assoc 'name it)))
                (type (cdr (assoc 'type it)))
                (module-path (cdr (assoc 'module-path it)))
                (line (cdr (assoc 'line it)))
                (docstring (cdr (assoc 'docstring it)))
                (description (if (equal type "statement")
                                 "statement"
                               (cdr (assoc 'description it)))))
           (put-text-property 0 1 'description description name)
           (put-text-property 0 1 'module-path module-path name)
           (put-text-property 0 1 'line line name)
           (put-text-property 0 1 'docstring docstring name)
           name)
         result))

(defun anaconda-mode-complete-annotation (candidate)
  "Get annotation for CANDIDATE."
  (--when-let (get-text-property 0 'description candidate)
    (concat " <" it ">")))


;;; View documentation.

(defun anaconda-mode-show-doc ()
  "Show documentation for context at point."
  (interactive)
  (anaconda-mode-call "goto_definitions" anaconda-mode-show-doc-callback))

(defun anaconda-mode-show-doc-callback (result)
  "Process view doc RESULT."
  (if result
      (anaconda-mode-documentation-view result)
    (message "No documentation available")))


;;; Find definitions.

(defun anaconda-mode-find-definitions ()
  "Find definitions for thing at point."
  (interactive)
  (anaconda-mode-call "goto_definitions" anaconda-mode-find-definitions-callback))

(defun anaconda-mode-find-definitions-callback (result)
  "Process find definitions RESULT."
  (if result
      (anaconda-mode-definitions-view result)
    (message "No definitions found")))


;;; Find assignments.

(defun anaconda-mode-find-assignments ()
  "Find assignments for thing at point."
  (interactive)
  (anaconda-mode-call "goto_assignments" anaconda-mode-find-assignments-callback))

(defun anaconda-mode-find-assignments-callback (result)
  "Process find assignments RESULT."
  (if result
      (anaconda-mode-definitions-view result)
    (message "No assignments found")))


;;; Find references.

(defun anaconda-mode-find-references ()
  "Find references for thing at point."
  (interactive)
  (anaconda-mode-call "usages" anaconda-mode-find-references-callback))

(defun anaconda-mode-find-references-callback (result)
  "Process find references RESULT."
  (if result
      (anaconda-mode-definitions-view result)
    (message "No references found")))


;;; Eldoc.

(defun anaconda-mode-eldoc-function ()
  "Show eldoc for context at point."
  (anaconda-mode-call "eldoc" anaconda-mode-eldoc-callback)
  ;; Don't show response buffer name as ElDoc message.
  nil)

(defun anaconda-mode-eldoc-callback (result)
  "Display eldoc from server RESULT."
  (eldoc-message (anaconda-mode-eldoc-format result)))

(defun anaconda-mode-eldoc-format (result)
  "Format eldoc string from RESULT."
  (when result
    (let* ((name (cdr (assoc 'name result)))
           (index (or (cdr (assoc 'index result)) 0))
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


;;; Result view.

(defmacro anaconda-mode-with-view-buffer (&rest body)
  "Create view buffer and execute BODY in it."
  `(let ((buf (get-buffer-create "*Anaconda*")))
     (with-current-buffer buf
       (setq buffer-read-only nil)
       (erase-buffer)
       ,@body
       (goto-char (point-min))
       (anaconda-view-mode)
       buf)))

(defun anaconda-mode-definitions-view (result)
  "Show definitions view for rpc RESULT."
  (if (eq 1 (length result))
      (anaconda-mode-find-file (car result))
    (anaconda-mode-view result 'anaconda-mode-view-definitions-presenter)))

(defun anaconda-mode-documentation-view (result)
  "Show documentation view for rpc RESULT."
  (anaconda-mode-view result 'anaconda-mode-view-documentation-presenter))

(defun anaconda-mode-view (result presenter)
  "Show RESULT to user for future selection.
RESULT must be an RESULT field from json-rpc response.
PRESENTER is the function used to format buffer content."
  (pop-to-buffer
   (anaconda-mode-with-view-buffer
    (funcall presenter result))))

(defun anaconda-mode-view-make-bold (string)
  "Make passed STRING look bold."
  (propertize string 'face 'bold))

(defun anaconda-mode-view-make-source (string)
  "Make passed STRING look like python source."
  (with-temp-buffer
    (insert string)
    (let ((delay-mode-hooks t))
      (python-mode))
    (run-hooks 'font-lock-mode-hook)
    (font-lock-ensure)
    (buffer-string)))

(define-button-type 'anaconda-mode-definition-button
  'action #'anaconda-mode-view-jump
  'face nil)

(defun anaconda-mode-view-jump (button)
  "Jump to definition file saved in BUTTON."
  (let ((definition (button-get button 'definition)))
    (anaconda-mode-find-file definition)))

(defun anaconda-mode-view-jump-other-window (button)
  "Jump to definition file saved in BUTTON."
  (let ((definition (button-get button 'definition)))
    (anaconda-mode-find-file-other-window definition)))

(defun anaconda-mode-view-display-jump (button)
  "Jump to definition file saved in BUTTON."
  (let ((definition (button-get button 'definition)))
    (anaconda-mode-display-buffer definition)))

(defun anaconda-mode-find-file (definition)
  "Find DEFINITION file, go to DEFINITION point."
  (anaconda-mode-find-file-generic definition 'find-file))

(defun anaconda-mode-find-file-other-window (definition)
  "Find DEFINITION file other window, go to DEFINITION point."
  (anaconda-mode-find-file-generic definition 'find-file-other-window))

(defun anaconda-mode-display-buffer (definition)
  "Display DEFINITION file, go to DEFINITION point."
  (let ((buffer (anaconda-mode-find-file-generic definition 'find-file-noselect)))
    (and buffer (display-buffer buffer))))

(defun anaconda-mode-find-file-no-record-definition (definition)
  "Find DEFINITION file, go to DEFINITION point (without recording in the go-back stack)."
  (anaconda-mode-find-file-generic definition 'find-file t))

(defvar-local anaconda-mode-go-back-definitions nil
  "Previous definition from which current buffer was navigated.")

(defun anaconda-mode-find-file-generic (definition find-function &optional no-record)
  "Find DEFINITION with FIND-FUNCTION."
  (let ((backward-navigation (when (buffer-file-name)
                               `((module-path . ,(buffer-file-name))
                                 (line . ,(line-number-at-pos (point)))
                                 (column . ,(- (point) (line-beginning-position)))))))
    (--if-let (cdr (assoc 'module-path definition))
        (progn
          (let ((buffer (funcall find-function it)))
            (with-current-buffer buffer
              (goto-char (point-min))
              (forward-line (1- (cdr (assoc 'line definition))))
              (forward-char (cdr (assoc 'column definition)))
              (when (and (not no-record) backward-navigation)
                (push backward-navigation anaconda-mode-go-back-definitions))
              buffer)))
      (message "Can't open %s module" (cdr (assoc 'module-name definition)))
      nil)))

(defun anaconda-mode-view-insert-button (name definition)
  "Insert text button with NAME opening the DEFINITION."
  (insert-text-button name
                      'type 'anaconda-mode-definition-button
                      'definition definition))

(defun anaconda-mode-view-definitions-presenter (result)
  "Insert definitions from RESULT."
  (->>
   (--group-by (cdr (assoc 'module-name it)) result)
   (--map (anaconda-mode-view-insert-module-definition it))))

(defun anaconda-mode-view-insert-module-definition (module)
  "Insert MODULE definition into current buffer."
  (insert (concat (anaconda-mode-view-make-bold (car module)) "\n"))
  (--map
   (progn
     (insert "    ")
     (anaconda-mode-view-insert-button
      (anaconda-mode-view-make-source (cdr (assoc 'description it)))
      it)
     (insert "\n"))
   (cdr module)))

(defun anaconda-mode-view-documentation-presenter (result)
  "Insert documentation from RESULT."
  (--map
   (progn
     (insert (anaconda-mode-view-make-bold (cdr (assoc 'module-name it))))
     (insert "\n")
     (insert (s-trim-right (cdr (assoc 'docstring it))))
     (insert "\n\n"))
   result))

(defvar anaconda-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'anaconda-view-mode-next-definition)
    (define-key map (kbd "p") 'anaconda-view-mode-previous-definition)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode anaconda-view-mode special-mode "Anaconda-View"
  "Major mode for definitions view and navigation for `anaconda-mode'.

\\{anaconda-view-mode-map}")

(defun anaconda-view-mode-next-definition ()
  "Navigate to the next definition in the view buffer."
  (interactive)
  (forward-button 1)
  (anaconda-mode-view-display-jump (button-at (point))))

(defun anaconda-view-mode-previous-definition ()
  "Navigate to the previous definition in the view buffer."
  (interactive)
  (forward-button -1)
  (anaconda-mode-view-display-jump (button-at (point))))

(defun anaconda-mode-go-back ()
  "Jump backward if buffer was navigated from `anaconda-mode' command."
  (interactive)
  (if anaconda-mode-go-back-definitions
      (anaconda-mode-find-file-no-record-definition (pop anaconda-mode-go-back-definitions))
    (message "No previous buffer")))


;;; Anaconda minor mode.

(defvar anaconda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-i") 'anaconda-mode-complete)
    (define-key map (kbd "M-.") 'anaconda-mode-find-definitions)
    (define-key map (kbd "M-,") 'anaconda-mode-find-assignments)
    (define-key map (kbd "M-r") 'anaconda-mode-find-references)
    (define-key map (kbd "M-*") 'anaconda-mode-go-back)
    (define-key map (kbd "M-?") 'anaconda-mode-show-doc)
    map)
  "Keymap for `anaconda-mode'.")

;;;###autoload
(define-minor-mode anaconda-mode
  "Code navigation, documentation lookup and completion for Python.

\\{anaconda-mode-map}"
  :lighter anaconda-mode-lighter
  :keymap anaconda-mode-map)

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
