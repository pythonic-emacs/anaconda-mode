;;; anaconda-eldoc.el --- ElDoc for anaconda-mode

;; Copyright (C) 2013, 2014 by Malyshev Artem

;; Authors: Malyshev Artem <proofit404@gmail.com>
;;          Fredrik Bergroth <fbergroth@gmail.com>
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

(require 'cl-lib)
(require 'dash)
(require 'anaconda-mode)

(defvar anaconda-eldoc-as-single-line nil
  "If not nil, trim eldoc string to frame width.")

(defun anaconda-eldoc--format-params (index params)
  (apply
   'concat
   (->> params
     (--map-indexed
      (if (= index it-index)
          (propertize it 'face 'eldoc-highlight-function-argument)
        it))
     (-interpose ", "))))

(cl-defun anaconda-eldoc--format (&key name index params)
  (concat
   (propertize name 'face 'font-lock-function-name-face)
   "("
   (anaconda-eldoc--format-params index params)
   ")"))

(defun anaconda-eldoc-function ()
  "Show eldoc for context at point."
  (-when-let* ((res (anaconda-mode-call-1 "eldoc"))
               (doc (apply 'anaconda-eldoc--format res)))
    (if anaconda-eldoc-as-single-line
        (substring doc 0 (min (frame-width) (length doc)))
      doc)))

;;;###autoload
(define-minor-mode anaconda-eldoc
  "ElDoc for anaconda-mode."
  :lighter ""
  :keymap nil
  (if anaconda-eldoc
      (progn
        (make-local-variable 'eldoc-documentation-function)
        (setq-local eldoc-documentation-function 'anaconda-eldoc-function)
        (eldoc-mode 1))
    (kill-local-variable 'eldoc-documentation-function)
    (eldoc-mode -1)))

(provide 'anaconda-eldoc)

;;; anaconda-eldoc.el ends here
