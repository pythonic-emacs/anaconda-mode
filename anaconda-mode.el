;;; anaconda-mode.el --- Code navigation, documentation lookup and completion for Python

;; Copyright (C) 2013, 2014 by Malyshev Artem

;; Authors: Malyshev Artem <proofit404@gmail.com>
;;          Fredrik Bergroth <fbergroth@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (json-rpc "0.0.1") (cl-lib "0.5.0"))

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


;;; Server.

(defvar anaconda-mode-debug nil
  "Turn on anaconda_mode debug logging.")

(defvar anaconda-mode-host "localhost"
  "Target host with anaconda_mode server.")

(defvar anaconda-mode-port 24970
  "Port for anaconda_mode connection.")

(defvar anaconda-mode-plugins '(company nav doc eldoc))

(defvar anaconda-mode-directory
  (file-name-directory load-file-name)
  "Directory containing anaconda_mode package.")

(defvar anaconda-mode-process nil
  "Currently running anaconda_mode process.")

(defvar anaconda-mode-connection nil
  "Json Rpc connection to anaconda_mode process.")

(defun anaconda-mode-python ()
  "Detect python executable."
  ;; TODO: (f-join it "bin" "python")?
  (--if-let python-shell-virtualenv-path
      (concat (file-name-as-directory it) "bin/python")
    "python"))

(defun anaconda-mode-plugin-paths ()
  "List of paths for anaconda plugins."
  (->> anaconda-mode-plugins
    (--map (->> it
             symbol-name
             (concat "anaconda-")
             locate-library
             file-name-directory))
    -distinct))

(defun anaconda-mode-python-args ()
  "Python arguments to run anaconda_mode server."
  (-flatten
   (list "anaconda_mode.py"
         "--bind" anaconda-mode-host
         "--port" (number-to-string anaconda-mode-port)
         (--mapcat `("--path" ,it) (anaconda-mode-plugin-paths))
         (--mapcat `("--plugin" ,(symbol-name it)) anaconda-mode-plugins)
         (when anaconda-mode-debug "--debug"))))

(defun anaconda-mode-command ()
  "Shell command to run anaconda_mode server."
  (cons (anaconda-mode-python)
        (anaconda-mode-python-args)))

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
          (json-rpc-connect anaconda-mode-host anaconda-mode-port))
    (anaconda-mode-notify-plugins 'start)))

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
    (setq anaconda-mode-connection nil)
    (anaconda-mode-notify-plugins 'stop)))

(defun anaconda-mode-need-restart ()
  "Check if current `anaconda-mode-process' need restart with new args.
Return nil if it run under proper environment."
  (and (anaconda-mode-running-p)
       (not (equal (process-command anaconda-mode-process)
                   (anaconda-mode-command)))))


;;; Interaction.

(defun anaconda-rpc (method &rest params)
  "Call rpc method METHOD with params PARAMS."
  (anaconda-mode-start-node)
  ;; use list since not all dash functions operate on vectors
  (let ((json-array-type 'list))
    (apply 'json-rpc anaconda-mode-connection method params)))

(defun anaconda-rpc-script (method)
  "Call rpc script method METHOD."
  (anaconda-rpc
   method
   (buffer-substring-no-properties (point-min) (point-max))
   (line-number-at-pos (point))
   (current-column)
   (or (buffer-file-name) "")))


;;; Minor mode.

(defvar anaconda-mode-map (make-sparse-keymap)
  "Keymap for `anaconda-mode'.")

;;;###autoload
(define-minor-mode anaconda-mode
  "Code navigation, documentation lookup and completion for Python.

\\{anaconda-mode-map}"
  :lighter " Anaconda"
  :keymap anaconda-mode-map
  (anaconda-mode-start-node)
  (anaconda-mode-notify-plugins
   (if anaconda-mode 'buffer-start 'buffer-stop)))

(defun anaconda-mode-notify-plugins (step)
  "Notify plugins of STEP through their handler."
  (--each anaconda-mode-plugins
    (let* ((library (concat "anaconda-" (symbol-name it)))
           (handler (intern (concat library "-handler"))))
      (require (intern library))
      (when (fboundp handler)
        (funcall handler step)))))

(provide 'anaconda-mode)

;;; anaconda-mode.el ends here
