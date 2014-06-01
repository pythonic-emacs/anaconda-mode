;;; anaconda-nav.el --- Navigating

;;; Commentary:

;;; Code:

(require 'dash)

(defvar anaconda-nav-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'anaconda-nav-jump)
    (define-key map (kbd "RET") 'anaconda-nav-jump)
    (define-key map (kbd "n") 'anaconda-nav-next)
    (define-key map (kbd "p") 'anaconda-nav-prev)
    map)
  "Keymap for `anaconda-nav-mode'.")

(defvar anaconda-nav--last-marker nil)
(defvar anaconda-nav--markers ())

(defun anaconda-nav-next ()
  (interactive)
  (anaconda-nav-next-error 1 nil t))

(defun anaconda-nav-prev ()
  (interactive)
  (anaconda-nav-next-error -1 nil t))

(defun anaconda-nav-pop-marker ()
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

(defun anaconda-nav-jump (&optional event)
  (interactive (list last-input-event))
  (when event (goto-char (posn-point (event-end event))))
  (anaconda-nav-next-error 0 nil nil))

(defun anaconda-nav (result &optional jump-if-single-item)
  (setq anaconda-nav--last-marker (point-marker))
  (if (and jump-if-single-item (= 1 (length result)))
      (anaconda-nav-goto-item (car result) nil)
    (with-current-buffer (get-buffer-create "*anaconda-nav*")
      (view-mode -1)
      (erase-buffer)
      (setq-local overlay-arrow-position nil)

      (--> result
        (--group-by (cons (plist-get it :module)
                          (plist-get it :path)) it)
        (--each it (apply 'anaconda-nav--insert-module it)))

      (goto-char (point-min))
      (anaconda-nav-mode)
      (switch-to-buffer-other-window (current-buffer)))))

(defun anaconda-nav--insert-module (header &rest items)
  (insert (car header) "\n")
  (--each items (insert (anaconda-nav--item it) "\n"))
  (insert "\n"))

(defun anaconda-nav--item (item)
  (propertize
   (concat (propertize (format "%7d " (plist-get item :line))
                       'face 'compilation-line-number)
           (anaconda-nav--item-description item))
   'anaconda-nav-item item
   'follow-link t
   'mouse-face 'highlight))

(defun anaconda-nav--item-description (item)
  (destructuring-bind (&key column name description type &allow-other-keys) item
    (cond ((string= type "module") "«module definition»")
          (t (let ((to (+ column (length name))))
               (when (string= name (substring description column to))
                 (put-text-property column to 'face 'highlight description))
               description)))))


(defun anaconda-nav--next-item (next)
  (let ((search (if next #'next-single-property-change
                  #'previous-single-property-change)))
    (-when-let (pos (funcall search (point) 'anaconda-nav-item))
      (if (get-text-property pos 'anaconda-nav-item) pos
        (funcall search pos 'anaconda-nav-item)))))


(defun anaconda-nav-next-error (&optional argp reset preview)
  (interactive "p")
  (with-current-buffer (get-buffer-create "*anaconda-nav*")
    (goto-char (cond (reset (point-min))
                     ((minusp argp) (line-beginning-position))
                     ((plusp argp) (line-end-position))
                     ((point))))

    (--dotimes (abs argp)
      (--if-let (anaconda-nav--next-item (plusp argp))
          (goto-char it)
        (error "No more matches")))

    (setq-local overlay-arrow-position (copy-marker (line-beginning-position)))
    (--when-let (get-text-property (point) 'anaconda-nav-item)
      (anaconda-nav-goto-item it preview))))

(defun anaconda-nav--flash-result (name)
  (isearch-highlight (point)
                     (if (string= (symbol-at-point) name)
                         (+ (point) (length name))
                       (point-at-eol)))
  (run-with-idle-timer 0.5 nil 'isearch-dehighlight))

(defun anaconda-nav-goto-item (item preview)
  (destructuring-bind (&key line column name path &allow-other-keys) item
    (with-current-buffer (find-file-noselect path)
      (goto-line line)
      (forward-char column)
      (anaconda-nav--flash-result name)
      (if preview
          (set-window-point (display-buffer (current-buffer)) (point))

        ;; Push marker
        (when (markerp anaconda-nav--last-marker)
          (push anaconda-nav--last-marker anaconda-nav--markers)
          (setq anaconda-nav--last-marker nil))

        (switch-to-buffer (current-buffer))))))


(define-derived-mode anaconda-nav-mode special-mode "anaconda-nav"
  (use-local-map anaconda-nav-mode-map))

(provide 'anaconda-nav)
;;; anaconda-nav.el ends here
