;;; company-jedi.el --- Jedi backend for company-mode

;; Copyright (C) 2013 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/company-jedi
;; Version: 0.0.1
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

(defvar company-jedi-host "localhost"
  "Target host with jedi server.")

(defvar company-jedi-port 24970
  "Port for start_jedi connection.")

(defvar company-jedi-command
  (concat (file-name-as-directory user-emacs-directory)
          "jedi/venv/bin/python3 -m start_jedi -p "
          (number-to-string company-jedi-port))
  "Command to run start_jedi server.")

(defvar company-jedi-completing-read-function
  (if (or (featurep 'helm) (locate-library "helm"))
      'helm-comp-read
    'ido-completing-read)
  "Completing read function used in company-jedi.")

(defun company-jedi-completing-read (prompt collection)
  "Call completing engine with PROMPT on COLLECTION."
  (cond
    ((eq (length collection) 1)
     (car collection))
    ((> (length collection) 1)
     (funcall company-jedi-completing-read-function prompt collection))))

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
    (company-jedi-bootstrap)))

(defun company-jedi-do-request (body)
  "Make POST Request to jedi server.

BODY mast be a encoded json string."
  (let ((url (format "http://%s:%s" company-jedi-host company-jedi-port))
        (url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")))
        (url-request-data body))
    (with-current-buffer (url-retrieve-synchronously url)
      (case url-http-response-status
        (200 (progn
               (goto-char url-http-end-of-headers)
               (company-jedi-decode)))
        (500 (error (buffer-string)))))))

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
        (json-key-type 'keyword))
    (json-read)))

(defun company-jedi-decode-from-string (arg)
  "Decode JSON from ARG."
  (let ((json-array-type 'list)
        (json-object-type 'hash-table)
        (json-key-type 'keyword))
    (json-read-from-string arg)))

(defun company-jedi-candidates-json ()
  "Generate json for candidates request."
  (company-jedi-encode
   (list (cons "command" "candidates")
         (cons "attributes" (company-jedi-point)))))

(defun company-jedi-candidates ()
  "Request completion candidates from jedi."
  (company-jedi-do-request (company-jedi-candidates-json)))

(defun company-jedi-location-json (&optional arg)
  "Generate json for location request.

ARG may come from `company-call-backend' function."
  (company-jedi-encode
   (list (cons "command" "location")
         (cons "attributes" (company-jedi-point arg)))))

(defun company-jedi-location (&optional arg)
  "Request completion location from jedi.

ARG may come from `company-call-backend' function."
  (let* ((definitions (company-jedi-do-request (company-jedi-location-json arg)))
         (locations (mapcar (lambda (l) (format "%s:%s"(gethash :module_path l)  (gethash :line l))) definitions))
         (user-chose (company-jedi-completing-read "Location: " locations)))
    (if user-chose
        (let ((chose-list (split-string user-chose ":")))
          (cons (car chose-list) (string-to-number (cadr chose-list))))
      (error "Can't find definition."))))

;;;###autoload
(defun company-jedi (command &optional arg)
  "Jedi backend for company-mode.

See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-jedi))
    (prefix (and (memq major-mode '(python-mode inferior-python-mode))
                 (company-jedi-running-p)
                 (company-grab-symbol)))
    (candidates (company-jedi-candidates))
    (location (company-jedi-location arg))))

;;;###autoload
(defun company-jedi-goto-definition ()
  "Jump to definition at point.

Save current position in `find-tag-marker-ring'."
  (interactive)
  (let ((location (company-jedi-location)))
    (ring-insert find-tag-marker-ring (point-marker))
    (find-file (car location))
    (goto-line (cdr location))
    (back-to-indentation)))

(provide 'company-jedi)

;;; company-jedi.el ends here
