;;; anaconda-mode.el --- Code navigation, documentation lookup and completion for Python

;; Copyright (C) 2013, 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.0
;; Package-Requires: ((company "0.6.12"))

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

(require 'company)
(eval-when-compile (require 'cl))
(require 'url)
(require 'json)
(require 'etags)

(defvar anaconda-mode-show-eldoc-as-single-line nil
  "If not nil, trim eldoc string to frame width.")

(defvar anaconda-mode-complete-on-dot t
  "If not nil, invoke jedi completion after dot inserting.")

(defvar anaconda-mode-start-stop-hook
  '(anaconda-mode anaconda-eldoc-mode)
  "Hook runs after `anaconda-mode-start' or `anaconda-mode-stop' call.")

(defvar anaconda-mode-completing-read-function
  (if (or (featurep 'helm) (locate-library "helm"))
      'helm-comp-read
    'ido-completing-read)
  "Completing read function used in anaconda-mode.")

(defvar anaconda-mode-python-bin "python"
  "Python executable with installed dependencies.")

(defvar anaconda-mode-debug nil
  "Turn on anaconda debug logging.")

(defvar anaconda-mode-host "127.0.0.1"   ;; Don't use localhost here due to:
  "Target host with jedi server.")      ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12893

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
          (start-process-shell-command "anaconda_mode" nil anaconda-mode-command))))

;;;###autoload
(defun anaconda-mode-start ()
  "Start remote jedi server."
  (interactive)
  (unless (anaconda-mode-running-p)
    (anaconda-mode-bootstrap))
  (run-hooks 'anaconda-mode-start-stop-hook))

;;;###autoload
(defun anaconda-mode-stop ()
  "Stop remote jedi server."
  (interactive)
  (when (anaconda-mode-running-p)
    (kill-process anaconda-mode-process))
  (run-hook-with-args 'anaconda-mode-start-stop-hook -1))

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

(defun anaconda-mode-request-json (command &optional arg)
  "Generate json request for COMMAND.
COMMAND must be one of Jedi command string.
ARG may come from `company-call-backend' function."
  (anaconda-mode-encode
   (list (cons "command" command)
         (cons "attributes" (anaconda-mode-point arg)))))

(defun anaconda-mode-point (&optional arg)
  "Return json compatible buffer point description.
ARG may come from `company-call-backend' function."
  (list (cons "source" (buffer-substring-no-properties (point-min) (point-max)))
        (cons "line" (line-number-at-pos (point)))
        (cons "column" (current-column))
        (cons "point" (1- (point)))  ;; For python strings index compatibility.
        (cons "path" (or (buffer-file-name) ""))
        (cons "company_prefix" (or company-prefix ""))
        (cons "company_arg" (or arg ""))))

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

(defun anaconda-mode-completing-read (prompt collection)
  "Call completing engine with PROMPT on COLLECTION."
  (cond
   ((eq (length collection) 1)
    (car collection))
   ((> (length collection) 1)
    (funcall anaconda-mode-completing-read-function prompt collection))))

(defun key-list (hash)
  "Return key list of HASH."
  (let (keys)
    (maphash
     (lambda (k v) (add-to-list 'keys k))
     hash)
    (sort keys 'string<)))

(defun anaconda-mode-user-chose (prompt hash)
  "With PROMPT ask user for HASH value."
  (when hash
    (gethash (anaconda-mode-completing-read prompt (key-list hash))
             hash)))

(defun anaconda-mode-chose-module (prompt modules)
  "Completing read with PROMPT from MODULES.
Return cons of file name and line."
  (let ((user-chose (anaconda-mode-user-chose prompt modules)))
    (when user-chose
      (cons (gethash "module_path" user-chose)
            (gethash "line" user-chose)))))


;;; Company backed.

(defun anaconda-mode-prefix ()
  "Grab prefix at point.

 Properly detect strings, comments and attribute access."
  (and (eq major-mode 'python-mode)
       (anaconda-mode-running-p)
       (not (company-in-string-or-comment))
       (let ((symbol (company-grab-symbol)))
         (if symbol
             (if (and anaconda-mode-complete-on-dot
                      (save-excursion
                        (forward-char (- (length symbol)))
                        (looking-back "\\." (- (point) 1))))
                 (cons symbol t)
               symbol)
           'stop))))

(defun anaconda-mode-candidates ()
  "Request completion candidates from jedi."
  (anaconda-mode-do-request (anaconda-mode-request-json "candidates")))

(defun anaconda-mode-location (&optional arg)
  "Request completion location from jedi.

ARG may come from `company-call-backend' function."
  (anaconda-mode-chose-module
   "Location: "
   (anaconda-mode-do-request
    (anaconda-mode-request-json "location" arg))))

(defun anaconda-mode-reference ()
  "Request references from jedi."
  (anaconda-mode-chose-module
   "Reference: "
   (anaconda-mode-do-request
    (anaconda-mode-request-json "reference"))))

(defun anaconda-mode-doc-buffer (&optional arg)
  "Request document buffer for thing at point.
Allow user to chose what doc he want to read.
ARG may come from `company-call-backend' function."
  (let* ((json (anaconda-mode-request-json "doc" arg))
         (response (anaconda-mode-do-request json))
         (doc (anaconda-mode-user-chose "Doc: " response)))
    (when doc
      (company-doc-buffer doc))))

(defun anaconda-mode-meta (&optional arg)
  "Request short documentation for current context.
ARG may come from `company-call-backend' function."
  (anaconda-mode-do-request
   (anaconda-mode-request-json "meta" arg)))

;;;###autoload
(defun company-anaconda (command &optional arg)
  "Jedi backend for company-mode.

See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'anaconda-mode))
    (prefix (anaconda-mode-prefix))
    (candidates (anaconda-mode-candidates))
    (location (anaconda-mode-location arg))
    (reference (anaconda-mode-reference))
    (doc-buffer (anaconda-mode-doc-buffer arg))
    (meta (anaconda-mode-meta arg))
    (eldoc (anaconda-mode-do-request (anaconda-mode-request-json "eldoc")))
    (sorted t)))


;;; Minor mode.

(defvar anaconda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap find-tag] 'anaconda-mode-goto-definition)
    (define-key map (kbd "M-r") 'anaconda-mode-find-references)
    (define-key map (kbd "M-?") 'anaconda-mode-show-doc)
    map)
  "Keymap for Company Jedi mode.")

;;;###autoload
(define-minor-mode anaconda-mode
  "Minor mode for Company Jedi interaction.

\\{anaconda-mode-map}"
  :lighter " Anaconda"
  :keymap anaconda-mode-map)

(defun anaconda-mode-find-file (file line)
  "Find FILE at specified LINE.

Save current position in `find-tag-marker-ring'."
  (ring-insert find-tag-marker-ring (point-marker))
  (find-file file)
  (goto-line line)
  (back-to-indentation))

(defun anaconda-mode-goto-definition ()
  "Jump to definition at point."
  (interactive)
  (let ((module (anaconda-mode 'location)))
    (if module
        (anaconda-mode-find-file (car module) (cdr module))
      (error "Can't find definition"))))

(defun anaconda-mode-find-references ()
  "Jump to reference at point."
  (interactive)
  (let ((module (anaconda-mode 'reference)))
    (if module
        (anaconda-mode-find-file (car module) (cdr module))
      (error "Can't find references"))))

(defun anaconda-mode-show-doc ()
  "Show documentation for context at point."
  (interactive)
  (let ((doc-buffer (anaconda-mode 'doc-buffer)))
    (if doc-buffer
        (display-buffer doc-buffer)
      (error "Can't find documentation"))))


;;; Eldoc mode.

;;;###autoload
(define-minor-mode anaconda-eldoc-mode
  "Eldoc mode for anaconda-mode."
  :lighter ""
  :keymap nil
  (if anaconda-eldoc-mode
      (progn
        (set (make-local-variable 'eldoc-documentation-function)
             'anaconda-mode-eldoc-function)
        (eldoc-mode 1))
    (kill-local-variable 'eldoc-documentation-function)
    (eldoc-mode -1)))

(defun anaconda-mode-eldoc-function ()
  "Show eldoc for context at point."
  (let ((doc (anaconda-mode 'eldoc)))
    (if (and doc anaconda-mode-show-eldoc-as-single-line)
        (substring doc 0 (min (frame-width) (length doc)))
      doc)))

(provide 'anaconda-mode)

;;; anaconda-mode.el ends here
