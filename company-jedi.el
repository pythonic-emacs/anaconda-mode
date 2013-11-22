;;; company-jedi.el --- Jedi backend for company-mode

;; Copyright (C) 2013 by Malyshev Artem

;; Authors: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/company-jedi
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

(require 'url)
(require 'json)

(defvar company-jedi-host "localhost"
  "Target host with jedi server.")

(defvar company-jedi-port 24970
  "Port for start_jedi connection.")

(defvar company-jedi-command
  (format "venv/bin/python3 -m start_jedi -p %s" company-jedi-port)
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

(defun company-jedi-start ()
  "Start remote jedi server."
  (interactive)
  (unless (company-jedi-running-p)
    (company-jedi-bootstrap)))

(defun company-jedi-request (body)
  "Make POST Request to jedi server.

BODY mast be a json compatible structure."
  (let ((url (format "http://%s:%s" company-jedi-host company-jedi-port))
        (url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")))
        (url-request-data body))
    (with-current-buffer (url-retrieve-synchronously url)
      (when (eq url-http-response-status 200)
        (goto-char url-http-end-of-headers)
        (let ((json-array-type 'list))
          (json-read))))))

(provide 'company-jedi)

;;; company-jedi ends here
