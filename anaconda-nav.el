;;; anaconda-nav.el --- Navigating

;;; Commentary:

;; Major mode for navigating a list of source locations.

;;; Code:

(require 'dash)
(require 'cl-lib)

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
    (set-marker marker nil)))

(defun anaconda-nav--push-last-marker ()
  "Add last marker to markers."
  (when (markerp anaconda-nav--last-marker)
    (push anaconda-nav--last-marker anaconda-nav--markers)
    (setq anaconda-nav--last-marker nil)))

;;; Window configuration
(defvar anaconda-nav--window-configuration nil)

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

;;; Rendering buffer
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
  (insert (car header) "\n")
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
      (--if-let (anaconda-nav--find-item (cl-plusp argp))
          (goto-char it)
        (error "No more matches")))

    (setq-local overlay-arrow-position (copy-marker (line-beginning-position)))
    (--when-let (get-text-property (point) 'anaconda-nav-item)
      (pop-to-buffer (anaconda-nav--item-buffer it)))))

(defun anaconda-nav--find-item (forwardp)
  "Find next item in direction FORWARDP."
  (let ((search (if forwardp #'next-single-property-change
                  #'previous-single-property-change)))
    (-when-let (pos (funcall search (point) 'anaconda-nav-item))
      (if (get-text-property pos 'anaconda-nav-item) pos
        (funcall search pos 'anaconda-nav-item)))))

(defun anaconda-nav--item-buffer (item)
  "Get buffer of ITEM and position the point."
  (cl-destructuring-bind (&key line column name path &allow-other-keys) item
    (with-current-buffer (find-file-noselect path)
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
    (define-key map (kbd "q") 'anaconda-nav-quit)
    map)
  "Keymap for `anaconda-nav-mode'.")

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

(provide 'anaconda-nav)

;;; anaconda-nav.el ends here
