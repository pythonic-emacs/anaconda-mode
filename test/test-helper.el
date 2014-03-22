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
  (concat anaconda-mode-directory file))

(defun load-fixture (file line column)
  "Open fixture file."
  (find-file-read-only (fixture-path file))
  (goto-line line)
  (move-to-column column))

(defun mock-completing-read (prompt collection)
  "Emulate user chose."
  (car (sort collection 'string<)))

(when (getenv "ENVPYTHON")
  (setq anaconda-mode-python-bin (getenv "ENVPYTHON"))
  (message "Python binary: %s" (getenv "ENVPYTHON")))

(setq anaconda-mode-completing-read-function 'mock-completing-read)

(setq anaconda-mode-port 8000)

(setq anaconda-mode-debug t)

;; Server bootstrap.

(require 'anaconda-mode)

(anaconda-mode-start-node)

(sleep-for 5) ;; Wait for anaconda_mode server will ready to work.
