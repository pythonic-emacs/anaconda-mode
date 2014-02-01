;;; test-helper --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

;; Helper functions.

(defun fixture-path (file)
  "Make absolute path from project path."
  (concat company-jedi-dir file))

(defun load-fixture (file line column)
  "Open fixture file."
  (find-file-read-only (fixture-path file))
  (goto-line line)
  (move-to-column column))

(defun mock-completing-read (prompt collection)
  "Emulate user chose."
  (car (sort collection 'string<)))

(when (getenv "ENVPYTHON")
  (setq company-jedi-python-bin (getenv "ENVPYTHON"))
  (message "Python binary: %s" (getenv "ENVPYTHON")))

(setq company-jedi-completing-read-function 'mock-completing-read)

(setq company-jedi-port 8000)

(setq company-jedi-debug t)

;; Server bootstrap.

(require 'cl)
(require 'company-jedi)

(company-jedi-start)

(sleep-for 5) ;; Wait for start_jedi server will ready to work.
