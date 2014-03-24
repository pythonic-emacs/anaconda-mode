;;; anaconda-mode.el --- Code navigation, documentation lookup and completion for Python

;; Copyright (C) 2013, 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.0

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

(defvar anaconda-mode-python-bin "python"
  "Path to python executable.")

(defvar anaconda-mode-debug nil
  "Turn on anaconda_mode debug logging.")

(defvar anaconda-mode-host "127.0.0.1"       ;; Don't use localhost here due to:
  "Target host with anaconda_mode server.")  ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12893

(defvar anaconda-mode-port 24970
  "Port for anaconda_mode connection.")

(defvar anaconda-mode-command
  (mapconcat 'identity
             (list anaconda-mode-python-bin
                   "-m" "anaconda_mode.__main__"
                   "--ip" anaconda-mode-host
                   "--port" (number-to-string anaconda-mode-port)
                   (when anaconda-mode-debug "--debug"))
             " ")
  "Command to run anaconda_mode server.")

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
          (start-process-shell-command "anaconda_mode" nil anaconda-mode-command))
    ;; TODO: Wait here for anaconda_mode will be ready to process requests.
    ))

(defun anaconda-mode-start-node ()
  "Start anaconda_mode server."
  (unless (anaconda-mode-running-p)
    (anaconda-mode-bootstrap)))

(defun anaconda-mode-stop-node ()
  "Stop anaconda-mode server."
  (when (anaconda-mode-running-p)
    (kill-process anaconda-mode-process)))


;;; Interaction.

(defun anaconda-mode-call (command)
  "Make remote procedure call for COMMAND."
  (anaconda-mode-start-node)
  (anaconda-mode-do-request (anaconda-mode-request-json command)))

(defun anaconda-mode-do-request (body)
  "Make POST Request to jedi server with result processing.
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
  "Make POST request to jedi server synchronously.
BODY must be encoded json string."
  (let ((url (format "http://%s:%s" anaconda-mode-host anaconda-mode-port))
        (url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")))
        (url-request-data body)
        (url-show-status nil))
    (url-retrieve-synchronously url)))

(defun anaconda-mode-request-json (command)
  "Generate json request for COMMAND.
COMMAND must be one of Jedi command string.
ARG may come from `company-call-backend' function."
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
    (define-key map [remap find-tag] 'anaconda-mode-goto-definition)
    (define-key map (kbd "M-r") 'anaconda-mode-find-reference)
    map)
  "Keymap for `anaconda-mode'.")

;;;###autoload
(define-minor-mode anaconda-mode
  "Minor mode for Company Jedi interaction.

\\{anaconda-mode-map}"
  :lighter " Anaconda"
  :keymap anaconda-mode-map)


;;; Code completion.

(defun anaconda-mode-candidates ()
  "Request completion candidates."
  (anaconda-mode-call "candidates"))


;;; View documentation.

(defun anaconda-mode-view-doc ()
  "Show documentation for context at point."
  (interactive)
  (let ((doc (anaconda-mode-doc-string)))
    (if doc
        (anaconda-mode-doc-buffer doc)
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
      (display-buffer buf))))


;;; Jump to definition.

(defun anaconda-mode-find-definition ()
  "Find definition at point."
  (interactive)
  (let ((module (anaconda-mode-locate-definition)))
    (if module
        (anaconda-mode-find-file (car module) (cdr module))
      (error "Can't find definition"))))

(defun anaconda-mode-locate-definition ()
  "Request definitions."
  (anaconda-mode-chose-module
   "Definition: "
   (anaconda-mode-call "location")))


;;; Find reference.

(defun anaconda-mode-locate-reference ()
  "Request references."
  (anaconda-mode-chose-module
   "Reference: "
   (anaconda-mode-call "reference")))

(defun anaconda-mode-find-reference ()
  "Jump to reference at point."
  (interactive)
  (let ((module (anaconda-mode-locate-reference)))
    (if module
        (anaconda-mode-find-file (car module) (cdr module))
      (error "Can't find references"))))

(defun anaconda-mode-chose-module (prompt modules)
  "Completing read with PROMPT from MODULES.
Return cons of file name and line."
  (let ((user-chose (anaconda-mode-user-chose prompt modules)))
    (when user-chose
      (cons (gethash "module_path" user-chose)
            (gethash "line" user-chose)))))

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

(defun anaconda-mode-find-file (file line)
  "Find FILE at specified LINE.
Save current position in `find-tag-marker-ring'."
  (ring-insert find-tag-marker-ring (point-marker))
  (find-file file)
  (goto-char (point-min))
  (forward-line (1- line))
  (back-to-indentation))

(provide 'anaconda-mode)

;;; anaconda-mode.el ends here
