;;; anaconda-mode.el --- Code navigation, documentation lookup and completion for Python

;; Copyright (C) 2013, 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))

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

(require 'url)
(require 'json)
(require 'etags)
(require 'python)


;;; Server.

(defvar anaconda-mode-debug nil
  "Turn on anaconda_mode debug logging.")

(defvar anaconda-mode-host "127.0.0.1"       ;; Don't use localhost here due to:
  "Target host with anaconda_mode server.")  ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12893

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
  (delq nil (list "-m" "anaconda_mode.__main__"
                  "--ip" anaconda-mode-host
                  "--port" (number-to-string anaconda-mode-port)
                  (when anaconda-mode-debug "--debug"))))

(defvar anaconda-mode-directory
  (file-name-directory load-file-name)
  "Directory containing anaconda_mode package.")

(defvar anaconda-mode-process nil
  "Currently running anaconda_mode process.")

(defun anaconda-mode-running-p ()
  "Check for running anaconda_mode server."
  (and anaconda-mode-process
       (not (null (process-live-p anaconda-mode-process)))))

(defun anaconda-mode-bootstrap ()
  "Run anaconda-mode-command process."
  (let ((default-directory anaconda-mode-directory))
    (setq anaconda-mode-process
          (apply 'start-process
                 "anaconda_mode" nil
                 (anaconda-mode-python)
                 (anaconda-mode-python-args)))))

(defun anaconda-mode-start-node ()
  "Start anaconda_mode server."
  (when (anaconda-mode-need-restart)
    (anaconda-mode-stop-node))
  (unless (anaconda-mode-running-p)
    (anaconda-mode-bootstrap)))

(defun anaconda-mode-stop-node ()
  "Stop anaconda-mode server."
  (when (anaconda-mode-running-p)
    (kill-process anaconda-mode-process)))

(defun anaconda-mode-need-restart ()
  "Check if current `anaconda-mode-process'.
Return nil if it run under proper environment."
  (and (anaconda-mode-running-p)
       (not (equal (process-command anaconda-mode-process)
                   (cons (anaconda-mode-python)
                         (anaconda-mode-python-args))))))


;;; Interaction.

(defun anaconda-mode-call (command)
  "Make remote procedure call for COMMAND."
  (anaconda-mode-start-node)
  (anaconda-mode-do-request (anaconda-mode-request-json command)))

(defun anaconda-mode-call-debug (command)
  "View anaconda_mode reply for COMMAND call.
Debugging purpose only."
  (switch-to-buffer
   (anaconda-mode-retrive
    (anaconda-mode-request-json command))))

(defun anaconda-mode-do-request (body)
  "Make POST Request to anaconda_mode server with result processing.
BODY mast be encoded json string."
  (let ((response (anaconda-mode-retrive body)))
    (when response
      (prog1
          (with-current-buffer response
            (and (eq 200 url-http-response-status)
                 ;; Workaround: BaseHTTPServer in older versions
                 ;; return wrong ^M newline characters.  Use this hack
                 ;; until switching to tornado in 0.4.0 milestone.
                 (or (search-forward "\n\n" nil t)
                     (search-forward "\r\n\r\n" nil t))
                 (anaconda-mode-decode)))
        (kill-buffer response)))))

(defun anaconda-mode-retrive (body)
  "Make POST request to anaconda_mode server synchronously.
BODY must be encoded json string."
  (let ((url (format "http://%s:%s" anaconda-mode-host anaconda-mode-port))
        (url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")))
        (url-request-data body)
        (url-show-status nil))
    (url-retrieve-synchronously url)))

(defun anaconda-mode-request-json (command)
  "Generate json request for COMMAND.
COMMAND must be one of anaconda_mode command string."
  (anaconda-mode-encode
   (list (cons "command" command)
         (cons "attributes" (anaconda-mode-point)))))

(defun anaconda-mode-point ()
  "Return json compatible buffer point description."
  (list (cons "source" (buffer-substring-no-properties (point-min) (point-max)))
        (cons "line" (line-number-at-pos (point)))
        (cons "column" (current-column))
        (cons "path" (or (buffer-file-name) ""))))

(defun anaconda-mode-encode (arg)
  "Encode ARG to JSON."
  (let ((json-array-type 'list))
    (json-encode arg)))

(defun anaconda-mode-decode ()
  "Decode JSON at point."
  (let ((json-array-type 'list)
        (json-object-type 'hash-table)
        (json-key-type 'string))
    (json-read)))

(defun anaconda-mode-decode-from-string (arg)
  "Decode JSON from ARG."
  (let ((json-array-type 'list)
        (json-object-type 'hash-table)
        (json-key-type 'string))
    (json-read-from-string arg)))


;;; Minor mode.

(defvar anaconda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-?") 'anaconda-mode-view-doc)
    (define-key map [remap find-tag] 'anaconda-mode-find-definition)
    (define-key map [remap find-tag-other-window] 'anaconda-mode-find-definition-other-window)
    (define-key map [remap find-tag-other-frame] 'anaconda-mode-find-definition-other-frame)
    (define-key map (kbd "M-r") 'anaconda-mode-find-reference)
    (define-key map (kbd "C-x 4 R") 'anaconda-mode-find-reference-other-window)
    (define-key map (kbd "C-x 5 R") 'anaconda-mode-find-reference-other-frame)
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
  (mapcar (lambda (h) (gethash "name" h))
          (anaconda-mode-complete)))

(defun anaconda-mode-complete ()
  "Request completion candidates."
  (anaconda-mode-call "complete"))


;;; View documentation.

(defun anaconda-mode-view-doc ()
  "Show documentation for context at point."
  (interactive)
  (let ((doc (anaconda-mode-doc-string)))
    (if doc
        (display-buffer (anaconda-mode-doc-buffer doc))
      (error "Can't find documentation"))))

(defun anaconda-mode-doc-string ()
  "Request documentation string.
Allow user to chose what doc he want to read."
  (anaconda-mode-user-chose
   "Doc: "
   (anaconda-mode-call "doc")))

(defun anaconda-mode-doc-buffer (doc)
  "Create buffer for viewing DOC."
  (let ((buf (get-buffer-create "*anaconda-doc*")))
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer)
      (insert doc)
      (view-mode 1)
      buf)))


;;; Jump to definition.

(defun anaconda-mode-locate-definition ()
  "Request definitions."
  (anaconda-mode-chose-module
   "Definition: "
   (anaconda-mode-call "location")))

(defun anaconda-mode-definition-buffer ()
  "Get definition buffer or raise error."
  (apply #'anaconda-mode-file-buffer
         (or (anaconda-mode-locate-definition)
             (error "Can't find definition"))))

(defun anaconda-mode-find-definition ()
  "Find definition at point."
  (interactive)
  (switch-to-buffer (anaconda-mode-definition-buffer)))

(defun anaconda-mode-find-definition-other-window ()
  "Find definition at point in other window."
  (interactive)
  (switch-to-buffer-other-window (anaconda-mode-definition-buffer)))

(defun anaconda-mode-find-definition-other-frame ()
  "Find definition at point in other frame."
  (interactive)
  (switch-to-buffer-other-frame (anaconda-mode-definition-buffer)))


;;; Find reference.

(defun anaconda-mode-locate-reference ()
  "Request references."
  (anaconda-mode-chose-module
   "Reference: "
   (anaconda-mode-call "reference")))

(defun anaconda-mode-reference-buffer ()
  "Get reference buffer or raise error."
  (apply #'anaconda-mode-file-buffer
         (or (anaconda-mode-locate-reference)
             (error "Can't find references"))))

(defun anaconda-mode-find-reference ()
  "Jump to reference at point."
  (interactive)
  (switch-to-buffer (anaconda-mode-reference-buffer)))

(defun anaconda-mode-find-reference-other-window ()
  "Jump to reference at point in other window."
  (interactive)
  (switch-to-buffer-other-window (anaconda-mode-reference-buffer)))

(defun anaconda-mode-find-reference-other-frame ()
  "Jump to reference at point in other frame."
  (interactive)
  (switch-to-buffer-other-frame (anaconda-mode-reference-buffer)))

(defun anaconda-mode-chose-module (prompt modules)
  "Completing read with PROMPT from MODULES.
Return cons of file name and line."
  (let ((user-chose (anaconda-mode-user-chose prompt modules)))
    (when user-chose
      (list (gethash "module_path" user-chose)
            (gethash "line" user-chose)
            (gethash "column" user-chose)))))

(defun anaconda-mode-user-chose (prompt hash)
  "With PROMPT ask user for HASH value."
  (when hash
    (gethash (anaconda-mode-completing-read prompt (key-list hash))
             hash)))

(defun anaconda-mode-completing-read (prompt collection)
  "Call completing engine with PROMPT on COLLECTION."
  (cond
   ((eq (length collection) 1)
    (car collection))
   ((> (length collection) 1)
    (completing-read prompt collection))))

(defun key-list (hash)
  "Return sorted key list of HASH.
Keys must be a string."
  (let (keys)
    (maphash
     (lambda (k v) (add-to-list 'keys k))
     hash)
    (sort keys 'string<)))

(defun anaconda-mode-file-buffer (file line column)
  "Find FILE no select at specified LINE and COLUMN.
Save current position in `find-tag-marker-ring'."
  (let ((buf (find-file-noselect file)))
    (ring-insert find-tag-marker-ring (point-marker))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column)
      buf)))

(provide 'anaconda-mode)

;;; anaconda-mode.el ends here
