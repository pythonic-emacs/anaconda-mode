;;; company-anaconda.el --- Anaconda backend for company-mode

;; Copyright (C) 2013, 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
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

(require 'company)
(require 'anaconda-mode)
(eval-when-compile (require 'cl))

(defvar company-anaconda-complete-on-dot t
  "If not nil, invoke `company-anaconda' completion after dot inserting.")

(defun company-anaconda-prefix ()
  "Grab prefix at point.
Properly detect strings, comments and attribute access."
  (and (eq major-mode 'python-mode)
       (anaconda-mode-running-p)
       (not (company-in-string-or-comment))
       (let ((symbol (company-grab-symbol)))
         (if symbol
             (if (and company-anaconda-complete-on-dot
                      (save-excursion
                        (forward-char (- (length symbol)))
                        (looking-back "\\." (- (point) 1))))
                 (cons symbol t)
               symbol)
           'stop))))

;;;###autoload
(defun company-anaconda (command &optional arg)
  "Jedi backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'anaconda-mode))
    (prefix (anaconda-mode-prefix))
    (candidates (anaconda-mode-candidates))
    (location (anaconda-mode-location))
    (doc-buffer (anaconda-mode-doc-buffer))
    (sorted t)))

(provide 'company-anaconda)

;;; company-anaconda.el ends here
