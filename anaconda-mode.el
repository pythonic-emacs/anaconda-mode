;;; anaconda-mode.el --- Code navigation, documentation lookup and completion for Python

;; Copyright (C) 2013-2015 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (json-rpc "0.0.1") (cl-lib "0.5.0") (dash "2.6.0") (f "0.16.2") (python "0.1.0"))

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
(require 'json-rpc)
(require 'tramp)
(require 'cl-lib)
(require 'dash)
(require 'f)

(defgroup anaconda-mode nil
  "Code navigation, documentation lookup and completion for Python."
  :group 'programming)

(defcustom anaconda-mode-requirements '("anaconda_mode")
  "Requirements needed to run anaconda-mode server."
  :group 'anaconda-mode
  :type '(repeat string))

(defcustom anaconda-eldoc-as-single-line nil
  "If not nil, trim eldoc string to frame width."
  :group 'anaconda-mode
  :type 'boolean)


;;; Server.

(defvar anaconda-mode-directory (f-dirname load-file-name)
  "Anaconda mode installation directory.")

(defvar anaconda-mode-server
  (f-join anaconda-mode-directory "anaconda_mode.py")
  "Script file with anaconda-mode server.")

(defvar anaconda-mode-host "localhost"
  "Target host with anaconda-mode server.")

(defvar anaconda-mode-port nil
  "Port for anaconda-mode connection.")

(defvar anaconda-mode-process nil
  "Currently running anaconda-mode process.")

(defvar anaconda-mode-connection nil
  "Json Rpc connection to anaconda-mode process.")

(defun anaconda-mode-start ()
  "Start anaconda-mode server."
  (when (anaconda-mode-need-restart)
    (anaconda-mode-stop))
  (unless (anaconda-mode-running-p)
    (anaconda-mode-bootstrap)))

(defun anaconda-mode-stop ()
  "Stop anaconda-mode server."
  (when (anaconda-mode-running-p)
    (kill-process anaconda-mode-process)
    (setq anaconda-mode-process nil)))

(defun anaconda-mode-running-p ()
  "Check for running anaconda-mode server."
  (and anaconda-mode-process
       (process-live-p anaconda-mode-process)))

(defun anaconda-mode-need-restart ()
  "Check if current `anaconda-mode-process' need restart with new args.
Return nil if it run under proper environment."
  (and (anaconda-mode-running-p)
       (not (equal (pythonic-process-command anaconda-mode-process)
                   (anaconda-mode-get-process-command)))))

(defun anaconda-mode-bootstrap ()
  "Run anaconda-mode-command process."
  (pythonic-install anaconda-mode-requirements anaconda-mode-directory)
  (setq anaconda-mode-process
        (pythonic-start-process
         :name "anaconda_mode"
         :buffer "*anaconda-mode*"
         :script anaconda-mode-server
         :filter 'anaconda-mode-process-filter))
  (while (null anaconda-mode-port)
    (accept-process-output anaconda-mode-process)
    (unless (anaconda-mode-running-p)
      (error "Unable to run anaconda-mode server")))
  (set-process-filter anaconda-mode-process nil)
  (set-process-query-on-exit-flag anaconda-mode-process nil))

(defun anaconda-mode-process-filter (process output)
  "Set `anaconda-mode-port' from PROCESS OUTPUT."
  (--when-let (s-match "anaconda_mode port \\([0-9]+\\)" output)
    (setq anaconda-mode-port (string-to-number (cadr it))))
  ;; Mimic default filter.
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point))))))


;;; Connection.

(defun anaconda-mode-connect ()
  "Connect to anaconda-mode server."
  (when (anaconda-mode-need-reconnect)
    (anaconda-mode-disconnect))
  (unless (anaconda-mode-connected-p)
    (setq anaconda-mode-connection
          (json-rpc-connect anaconda-mode-host anaconda-mode-port))
    (set-process-query-on-exit-flag
     (json-rpc-process anaconda-mode-connection) nil)))

(defun anaconda-mode-disconnect ()
  "Disconnect from anaconda-mode server."
  (when (anaconda-mode-connected-p)
    (json-rpc-close anaconda-mode-connection)
    (setq anaconda-mode-connection nil)))

(defun anaconda-mode-connected-p ()
  "Check if `anaconda-mode' connected to server."
  (and anaconda-mode-connection
       (json-rpc-live-p anaconda-mode-connection)))

(defun anaconda-mode-need-reconnect ()
  "Check if current `anaconda-mode-connection' need to be reconnected."
  (and (anaconda-mode-connected-p)
       (or (not (equal (json-rpc-host anaconda-mode-connection)
                       anaconda-mode-host))
           (not (equal (json-rpc-port anaconda-mode-connection)
                       anaconda-mode-port)))))


;;; Interaction.

(defun anaconda-mode-call (command)
  "Make remote procedure call for COMMAND."
  (anaconda-mode-start)
  (anaconda-mode-connect)
  (json-rpc
   anaconda-mode-connection
   command
   (buffer-substring-no-properties (point-min) (point-max))
   (line-number-at-pos (point))
   (- (point) (line-beginning-position))
   (anaconda-mode-file-name)))

(defun anaconda-mode-file-name ()
  "Return appropriate buffer file name both for local and tramp files."
  (if (tramp-tramp-file-p (buffer-file-name))
      (tramp-file-name-localname
       (tramp-dissect-file-name
        (buffer-file-name)))
    (buffer-file-name)))


;;; Code completion.

(defun anaconda-mode-complete-at-point ()
  "Complete at point with anaconda-mode."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (stop (or (cdr bounds) (point))))
    (list start stop
          (completion-table-dynamic
           'anaconda-mode-complete-thing))))

(defun anaconda-mode-complete-thing (&rest ignored)
  "Complete python thing at point.
Do nothing in comments block.
IGNORED parameter is the string for which completion is required."
  (unless (python-syntax-comment-or-string-p)
    (--map (plist-get it :name)
           (anaconda-mode-complete))))

(defun anaconda-mode-complete ()
  "Request completion candidates."
  (anaconda-mode-call "complete"))


;;; View documentation.

(defun anaconda-mode-view-doc ()
  "Show documentation for context at point."
  (interactive)
  (pop-to-buffer
   (anaconda-mode-doc-buffer
    (or (anaconda-mode-call "doc")
        (error "No documentation found")))))

(defun anaconda-mode-doc-buffer (doc)
  "Display documentation buffer with contents DOC."
  (let ((buf (get-buffer-create "*anaconda-doc*")))
    (anaconda-mode-insert-doc buf)
    ;; (with-current-buffer buf
    ;;   (view-mode -1)
    ;;   (erase-buffer)
    ;;   (insert doc)
    ;;   (goto-char (point-min))
    ;;   (view-mode 1)
    ;;   buf)
    ))


;;; Usages.

(defun anaconda-mode-usages ()
  "Show usages for thing at point."
  (interactive)
  (anaconda-mode-completing-read
   (or (anaconda-mode-call "usages")
       (error "No usages found"))))


;;; Definitions and assignments.

(defun anaconda-mode-goto ()
  "Goto definition or fallback to assignment for thing at point."
  (interactive)
  (anaconda-mode-completing-read
   (or (anaconda-mode-call "goto_definitions")
       (anaconda-mode-call "goto_assignments")
       (error "No definition found"))))


;;; Eldoc.

(defun anaconda-eldoc-format-params (args index)
  "Build colorized ARGS string with current arg pointed to INDEX."
  (apply
   'concat
   (->> args
        (--map-indexed
         (if (= index it-index)
             (propertize it 'face 'eldoc-highlight-function-argument)
           it))
        (-interpose ", "))))

(cl-defun anaconda-eldoc-format (&key name index params)
  (concat
   (propertize name 'face 'font-lock-function-name-face)
   "("
   (anaconda-eldoc-format-params params index)
   ")"))

(defun anaconda-eldoc-function ()
  "Show eldoc for context at point."
  (ignore-errors
    (-when-let* ((res (anaconda-mode-call "eldoc"))
                 (doc (apply 'anaconda-eldoc-format res)))
      (if anaconda-eldoc-as-single-line
          (substring doc 0 (min (frame-width) (length doc)))
        doc))))


;;; Anaconda minor mode.

(defvar anaconda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-?") 'anaconda-mode-view-doc)
    (define-key map (kbd "M-r") 'anaconda-mode-usages)
    (define-key map [remap find-tag] 'anaconda-mode-goto)
    (define-key map [remap pop-tag-mark] 'anaconda-nav-pop-marker)
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
  (add-hook 'completion-at-point-functions
            'anaconda-mode-complete-at-point nil t)
  (make-local-variable 'eldoc-documentation-function)
  (setq-local eldoc-documentation-function 'anaconda-eldoc-function))

(defun turn-off-anaconda-mode ()
  "Turn off `anaconda-mode'."
  (remove-hook 'completion-at-point-functions
               'anaconda-mode-complete-at-point t)
  (kill-local-variable 'eldoc-documentation-function))

(provide 'anaconda-mode)

;;; anaconda-mode.el ends here
