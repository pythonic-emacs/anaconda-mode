;;; company-jedi.el --- Jedi backend for company-mode

;; Copyright (C) 2013, 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/company-jedi
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

(defvar company-jedi-show-eldoc-as-single-line nil
  "If not nil, trim eldoc string to frame width.")

(defvar company-jedi-complete-on-dot t
  "If not nil, invoke jedi completion after dot inserting.")

(defvar company-jedi-start-stop-hook
  '(company-jedi-mode company-jedi-eldoc-mode)
  "Hook runs after `company-jedi-start' or `company-jedi-stop' call.")

(defvar company-jedi-completing-read-function
  (if (or (featurep 'helm) (locate-library "helm"))
      'helm-comp-read
    'ido-completing-read)
  "Completing read function used in company-jedi.")

(defvar company-jedi-python-bin
  (let ((default-directory user-emacs-directory))
    (file-truename "jedi/venv/bin/python"))
  "Python executable with installed dependencies.")

(defvar company-jedi-debug nil
  "Turn on jedi debug logging.")

(defvar company-jedi-host "127.0.0.1"   ;; Don't use localhost here due to:
  "Target host with jedi server.")      ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12893

(defvar company-jedi-port 24970
  "Port for start_jedi connection.")

(defvar company-jedi-command
  (mapconcat 'identity
             (list company-jedi-python-bin
                   "-m" "start_jedi.__main__" ;; Python 2.6 can't run packages directly.
                   "--ip" company-jedi-host
                   "--port" (number-to-string company-jedi-port)
                   (when company-jedi-debug "--debug"))
             " ")
  "Command to run start_jedi server.")

(defvar company-jedi-dir
  (file-name-directory load-file-name)
  "Directory containing start_jedi package.")

(defvar company-jedi-process nil
  "Currently running start_jedi process.")

(defun company-jedi-running-p ()
  "Check for running start_jedi server."
  (and company-jedi-process
       (not (null (process-live-p company-jedi-process)))))

(defun company-jedi-bootstrap ()
  "Run company-jedi-command process."
  (let ((default-directory company-jedi-dir))
    (setq company-jedi-process
          (start-process-shell-command "start_jedi" nil company-jedi-command))))

;;;###autoload
(defun company-jedi-start ()
  "Start remote jedi server."
  (interactive)
  (unless (company-jedi-running-p)
    (company-jedi-bootstrap))
  (run-hooks 'company-jedi-start-stop-hook))

;;;###autoload
(defun company-jedi-stop ()
  "Stop remote jedi server."
  (interactive)
  (when (company-jedi-running-p)
    (kill-process company-jedi-process))
  (run-hook-with-args 'company-jedi-start-stop-hook -1))

;;;###autoload
(defun company-jedi-install ()
  "Install jedi dependencies with pip.  Virtualenv required."
  (interactive)
  (let ((default-directory company-jedi-dir))
    (compile "make")))

(defun company-jedi-do-request (body)
  "Make POST Request to jedi server with result processing.

BODY mast be encoded json string."
  (let ((response (company-jedi-retrive body)))
    (when response
      (prog1
          (with-current-buffer response
            (and (eq 200 url-http-response-status)
                 ;; Workaround: BaseHTTPServer in older versions
                 ;; return wrong ^M newline characters.  Use this hack
                 ;; until switching to tornado in 0.4.0 milestone.
                 (or (search-forward "\n\n" nil t)
                     (search-forward "\r\n\r\n" nil t))
                 (company-jedi-decode)))
        (kill-buffer response)))))

(defun company-jedi-retrive (body)
  "Make POST request to jedi server synchronously.

BODY must be encoded json string."
  (let ((url (format "http://%s:%s" company-jedi-host company-jedi-port))
        (url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")))
        (url-request-data body)
        (url-show-status nil))
    (url-retrieve-synchronously url)))

(defun company-jedi-request-json (command &optional arg)
  "Generate json request for COMMAND.

COMMAND must be one of Jedi command string.

ARG may come from `company-call-backend' function."
  (company-jedi-encode
   (list (cons "command" command)
         (cons "attributes" (company-jedi-point arg)))))

(defun company-jedi-point (&optional arg)
  "Return json compatible buffer point description."
  (list (cons "source" (buffer-substring-no-properties (point-min) (point-max)))
        (cons "line" (line-number-at-pos (point)))
        (cons "column" (current-column))
        (cons "point" (1- (point)))  ;; For python strings index compatibility.
        (cons "path" (or (buffer-file-name) ""))
        (cons "company_prefix" (or company-prefix ""))
        (cons "company_arg" (or arg ""))))

(defun company-jedi-encode (arg)
  "Encode ARG to JSON."
  (let ((json-array-type 'list))
    (json-encode arg)))

(defun company-jedi-decode ()
  "Decode JSON at point."
  (let ((json-array-type 'list)
        (json-object-type 'hash-table)
        (json-key-type 'string))
    (json-read)))

(defun company-jedi-decode-from-string (arg)
  "Decode JSON from ARG."
  (let ((json-array-type 'list)
        (json-object-type 'hash-table)
        (json-key-type 'string))
    (json-read-from-string arg)))

(defun company-jedi-completing-read (prompt collection)
  "Call completing engine with PROMPT on COLLECTION."
  (cond
   ((eq (length collection) 1)
    (car collection))
   ((> (length collection) 1)
    (funcall company-jedi-completing-read-function prompt collection))))

(defun key-list (hash)
  "Return key list of HASH."
  (let (keys)
    (maphash
     (lambda (k v) (add-to-list 'keys k))
     hash)
    (sort keys 'string<)))

(defun company-jedi-user-chose (prompt hash)
  "With PROMPT ask user for HASH value."
  (when hash
    (gethash (company-jedi-completing-read prompt (key-list hash))
             hash)))

(defun company-jedi-chose-module (prompt modules)
  "Chose module from MODULES.

Return cons of file name and line.

PROMPT will used for completing read function."
  (let ((user-chose (company-jedi-user-chose prompt modules)))
    (when user-chose
      (cons (gethash "module_path" user-chose)
            (gethash "line" user-chose)))))


;;; Company backed.

(defun company-jedi-prefix ()
  "Grab prefix at point.

 Properly detect strings, comments and attribute access."
  (and (eq major-mode 'python-mode)
       (company-jedi-running-p)
       (not (company-in-string-or-comment))
       (let ((symbol (company-grab-symbol)))
         (if symbol
             (if (and company-jedi-complete-on-dot
                      (save-excursion
                        (forward-char (- (length symbol)))
                        (looking-back "\\." (- (point) 1))))
                 (cons symbol t)
               symbol)
           'stop))))

(defun company-jedi-candidates ()
  "Request completion candidates from jedi."
  (company-jedi-do-request (company-jedi-request-json "candidates")))

(defun company-jedi-location (&optional arg)
  "Request completion location from jedi.

ARG may come from `company-call-backend' function."
  (company-jedi-chose-module
   "Location: "
   (company-jedi-do-request
    (company-jedi-request-json "location" arg))))

(defun company-jedi-reference ()
  "Request references from jedi."
  (company-jedi-chose-module
   "Reference: "
   (company-jedi-do-request
    (company-jedi-request-json "reference"))))

(defun company-jedi-doc-buffer (&optional arg)
  "Request document buffer for thing at point.

Allow user to chose what doc he want to read."
  (let* ((json (company-jedi-request-json "doc" arg))
         (response (company-jedi-do-request json))
         (doc (company-jedi-user-chose "Doc: " response)))
    (when doc
      (company-doc-buffer doc))))

(defun company-jedi-meta (&optional arg)
  "Request short documentation for current context."
  (company-jedi-do-request
   (company-jedi-request-json "meta" arg)))

;;;###autoload
(defun company-jedi (command &optional arg)
  "Jedi backend for company-mode.

See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-jedi))
    (prefix (company-jedi-prefix))
    (candidates (company-jedi-candidates))
    (location (company-jedi-location arg))
    (reference (company-jedi-reference))
    (doc-buffer (company-jedi-doc-buffer arg))
    (meta (company-jedi-meta arg))
    (eldoc (company-jedi-do-request (company-jedi-request-json "eldoc")))
    (sorted t)))


;;; Minor mode.

(defvar company-jedi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap find-tag] 'company-jedi-goto-definition)
    (define-key map (kbd "M-r") 'company-jedi-find-references)
    (define-key map (kbd "M-?") 'company-jedi-show-doc)
    map)
  "Keymap for Company Jedi mode.")

;;;###autoload
(define-minor-mode company-jedi-mode
  "Minor mode for Company Jedi interaction.

\\{company-jedi-mode-map}"
  :lighter " Jedi"
  :keymap company-jedi-mode-map)

(defun company-jedi-find-file (file line)
  "Find FILE at specified LINE.

Save current position in `find-tag-marker-ring'."
  (ring-insert find-tag-marker-ring (point-marker))
  (find-file file)
  (goto-line line)
  (back-to-indentation))

(defun company-jedi-goto-definition ()
  "Jump to definition at point."
  (interactive)
  (let ((module (company-jedi 'location)))
    (if module
        (company-jedi-find-file (car module) (cdr module))
      (error "Can't find definition."))))

(defun company-jedi-find-references ()
  "Jump to reference at point."
  (interactive)
  (let ((module (company-jedi 'reference)))
    (if module
        (company-jedi-find-file (car module) (cdr module))
      (error "Can't find references."))))

(defun company-jedi-show-doc ()
  "Show documentation for context at point."
  (interactive)
  (let ((doc-buffer (company-jedi 'doc-buffer)))
    (if doc-buffer
        (display-buffer doc-buffer)
      (error "Can't find documentation."))))


;;; Eldoc mode.

;;;###autoload
(define-minor-mode company-jedi-eldoc-mode
  "Eldoc mode for company-jedi backend.

\\{company-jedi-eldoc-mode-map}"
  :lighter ""
  :keymap nil
  (if company-jedi-eldoc-mode
      (progn
        (set (make-local-variable 'eldoc-documentation-function)
             'company-jedi-eldoc)
        (eldoc-mode 1))
    (kill-local-variable 'eldoc-documentation-function)
    (eldoc-mode -1)))

(defun company-jedi-eldoc ()
  "Show eldoc for context at point."
  (let ((doc (company-jedi 'eldoc)))
    (if (and doc company-jedi-show-eldoc-as-single-line)
        (substring doc 0 (min (frame-width) (length doc)))
      doc)))

(provide 'company-jedi)

;;; company-jedi.el ends here
