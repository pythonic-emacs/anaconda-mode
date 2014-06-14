;;; anaconda-doc.el --- Anaconda plugin for documentation

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

(require 'anaconda-mode)

(defun anaconda-doc-view-doc ()
  "Show documentation for context at point."
  (interactive)
  (display-buffer
   (anaconda-doc-buffer (or (anaconda-rpc-script "doc")
                            (error "No documentation found")))))

(defun anaconda-doc-buffer (doc)
  "Get documentation buffer with contents DOC."
  (with-current-buffer (get-buffer-create "*anaconda-doc*")
    (view-mode -1)
    (erase-buffer)
    (insert doc)
    (view-mode 1)
    (current-buffer)))

(defun anaconda-doc-handler (step)
  "Anaconda doc plugin handler."
  (pcase step
    (`start
     (define-key anaconda-mode-map (kbd "M-?") 'anaconda-doc-view-doc))))

(provide 'anaconda-doc)

;;; anaconda-doc.el ends here
