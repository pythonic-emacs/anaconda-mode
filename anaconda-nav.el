;;; anaconda-nav.el --- Navigating for anaconda-mode

;; Copyright (C) 2014 by Fredrik Bergroth

;; Author: Fredrik Bergroth <fbergroth@gmail.com>
;; URL: https://github.com/anaconda-mode/anaconda-mode
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

;; Major mode for navigating a list of source locations.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'anaconda-mode)

;;; Markers
(defvar anaconda-nav--last-marker nil)
(defvar anaconda-nav--markers ())

(defun anaconda-nav-pop-marker ()
  "Switch to buffer of most recent marker."
  (interactive)
  (unless anaconda-nav--markers
    (error "No marker available"))

  (let* ((marker (pop anaconda-nav--markers))
         (buffer (marker-buffer marker)))
    (unless (buffer-live-p buffer)
      (error "Buffer no longer available"))
    (switch-to-buffer buffer)
    (goto-char (marker-position marker))
    (set-marker marker nil)
    (anaconda-nav--cleanup-buffers)))

(defun anaconda-nav--push-last-marker ()
  "Add last marker to markers."
  (when (markerp anaconda-nav--last-marker)
    (push anaconda-nav--last-marker anaconda-nav--markers)
    (setq anaconda-nav--last-marker nil)))

(defun anaconda-nav--all-markers ()
  "Markers including last-marker."
  (if anaconda-nav--last-marker
      (cons anaconda-nav--last-marker anaconda-nav--markers)
    anaconda-nav--markers))

;;; Window and buffer management
(defvar anaconda-nav--window-configuration nil)
(defvar anaconda-nav--created-buffers ())

(defun anaconda-nav--cleanup-buffers ()
  "Kill unmodified buffers (without markers) created by anaconda-nav."
  (let* ((marker-buffers (-map 'marker-buffer (anaconda-nav--all-markers)))
         (result (--separate (-contains? marker-buffers it)
                             anaconda-nav--created-buffers)))
    (setq anaconda-nav--created-buffers (car result))
    (-each (cadr result) 'kill-buffer-if-not-modified)))

(defun anaconda-nav--get-or-create-buffer (path)
  "Get buffer for PATH, and record if buffer was created."
  (or (find-buffer-visiting path)
      (let ((created-buffer (find-file-noselect path)))
        (anaconda-nav--cleanup-buffers)
        (push created-buffer anaconda-nav--created-buffers)
        created-buffer)))

(defun anaconda-nav--restore-window-configuration ()
  "Restore window configuration."
  (when anaconda-nav--window-configuration
    (set-window-configuration anaconda-nav--window-configuration)
    (setq anaconda-nav--window-configuration nil)))

;;; Main usage
(defun anaconda-nav-navigate (result &optional goto-if-single-item)
  "Navigate RESULT, jump if only one item and GOTO-IF-SINGLE-ITEM is non-nil."
  (setq anaconda-nav--last-marker (point-marker))
  (if (and goto-if-single-item (= 1 (length result)))
      (progn (anaconda-nav--push-last-marker)
             (switch-to-buffer (anaconda-nav--item-buffer (car result))))
    (setq anaconda-nav--window-configuration (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer-other-window (anaconda-nav--prepare-buffer result))))

;;; Rendering results
(defun anaconda-nav--prepare-buffer (result)
  "Render RESULT in the navigation buffer."
  (with-current-buffer (get-buffer-create "*anaconda-nav*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq-local overlay-arrow-position nil)
    (--> result
      (--group-by (cons (plist-get it :module)
                        (plist-get it :path)) it)
      (--each it (apply 'anaconda-nav--insert-module it)))
    (goto-char (point-min))
    (anaconda-nav-mode)
    (current-buffer)))

(defun anaconda-nav--insert-module (header &rest items)
  "Insert a module consisting of a HEADER with ITEMS."
  (insert (propertize (car header)
                      'face 'bold
                      'anaconda-nav-module t)
          "\n")
  (--each items (insert (anaconda-nav--format-item it) "\n"))
  (insert "\n"))

(defun anaconda-nav--format-item (item)
  "Format ITEM as a row."
  (propertize
   (concat (propertize (format "%7d " (plist-get item :line))
                       'face 'compilation-line-number)
           (anaconda-nav--get-item-description item))
   'anaconda-nav-item item
   'follow-link t
   'mouse-face 'highlight))

(defun anaconda-nav--get-item-description (item)
  "Format description of ITEM."
  (cl-destructuring-bind (&key column name description type &allow-other-keys) item
    (cond ((string= type "module") "«module definition»")
          (t (let ((to (+ column (length name))))
               (when (string= name (substring description column to))
                 (put-text-property column to 'face 'highlight description))
               description)))))

;;; Navigating items
(defun anaconda-nav-next-error (&optional argp reset)
  "Move to the ARGP'th next match, searching from start if RESET is non-nil."
  (interactive "p")
  (with-current-buffer (get-buffer "*anaconda-nav*")
    (goto-char (cond (reset (point-min))
                     ((cl-minusp argp) (line-beginning-position))
                     ((cl-plusp argp) (line-end-position))
                     ((point))))

    (--dotimes (abs argp)
      (anaconda-nav--goto-property 'anaconda-nav-item (cl-plusp argp)))

    (setq-local overlay-arrow-position (copy-marker (line-beginning-position)))
    (--when-let (get-text-property (point) 'anaconda-nav-item)
      (pop-to-buffer (anaconda-nav--item-buffer it)))))

(defun anaconda-nav--goto-property (prop forwardp)
  "Goto next property PROP in direction FORWARDP."
  (--if-let (anaconda-nav--find-property prop forwardp)
      (goto-char it)
    (error "No more matches")))

(defun anaconda-nav--find-property (prop forwardp)
  "Find next property PROP in direction FORWARDP."
  (let ((search (if forwardp #'next-single-property-change
                  #'previous-single-property-change)))
    (-when-let (pos (funcall search (point) prop))
      (if (get-text-property pos prop) pos
        (funcall search pos prop)))))

(defun anaconda-nav--item-buffer (item)
  "Get buffer of ITEM and position the point."
  (cl-destructuring-bind (&key line column name path &allow-other-keys) item
    (with-current-buffer (anaconda-nav--get-or-create-buffer path)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char column)
      (anaconda-nav--highlight name)
      (current-buffer))))

(defun anaconda-nav--highlight (name)
  "Highlight NAME or line at point."
  (isearch-highlight (point)
                     (if (string= (symbol-at-point) name)
                         (+ (point) (length name))
                       (point-at-eol)))
  (run-with-idle-timer 0.5 nil 'isearch-dehighlight))

;;; Major mode
(defvar anaconda-nav-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'anaconda-nav-goto-item)
    (define-key map (kbd "RET") 'anaconda-nav-goto-item)
    (define-key map (kbd "n") 'next-error)
    (define-key map (kbd "p") 'previous-error)
    (define-key map (kbd "M-n") 'anaconda-nav-next-module)
    (define-key map (kbd "M-p") 'anaconda-nav-previous-module)
    (define-key map (kbd "q") 'anaconda-nav-quit)
    map)
  "Keymap for `anaconda-nav-mode'.")

(defun anaconda-nav-next-module ()
  "Visit first error of next module."
  (interactive)
  (anaconda-nav--goto-property 'anaconda-nav-module t)
  (next-error))

(defun anaconda-nav-previous-module ()
  "Visit first error of previous module."
  (interactive)
  (anaconda-nav--goto-property 'anaconda-nav-item nil)
  (anaconda-nav--goto-property 'anaconda-nav-module nil)
  (next-error))

(defun anaconda-nav-quit ()
  "Quit `anaconda-nav-mode' and restore window configuration."
  (interactive)
  (quit-window)
  (anaconda-nav--restore-window-configuration))

(defun anaconda-nav-goto-item (&optional event)
  "Go to the location of the item from EVENT."
  (interactive (list last-input-event))
  (when event (goto-char (posn-point (event-end event))))
  (-when-let (buffer (anaconda-nav-next-error 0))
    (anaconda-nav--restore-window-configuration)
    (anaconda-nav--push-last-marker)
    (switch-to-buffer buffer)))

(define-derived-mode anaconda-nav-mode special-mode "anaconda-nav"
  "Major mode for navigating a list of source locations."
  (use-local-map anaconda-nav-mode-map)
  (setq next-error-function 'anaconda-nav-next-error)
  (setq next-error-last-buffer (current-buffer))
  (next-error-follow-minor-mode 1))

(defun anaconda-nav-handler (step)
  (pcase step
    (`start
     (define-key anaconda-mode-map (kbd "M-r") 'anaconda-nav-show-usages)
     (define-key anaconda-mode-map [remap find-tag] 'anaconda-nav-goto-definitions)
     (define-key anaconda-mode-map [remap pop-tag-mark] 'anaconda-nav-pop-marker))))

(defun anaconda-nav-show-usages ()
  "Show usages for thing at point."
  (interactive)
  (anaconda-nav-navigate (or (anaconda-rpc-script "usages")
                             (error "No usages found"))))

(defun anaconda-nav-goto-definitions ()
  "Goto definitions or fallback to assignments for thing at point."
  (interactive)
  (anaconda-nav-navigate (or (anaconda-rpc-script "goto_definitions")
                             (error "No definition found"))
                         t))

(provide 'anaconda-nav)

;;; anaconda-nav.el ends here
