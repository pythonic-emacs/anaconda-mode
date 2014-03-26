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

(defun mock-completing-read (prompt collection &rest ignored)
  "Emulate user chose."
  (car (sort collection 'string<)))

(setq completing-read-function 'mock-completing-read)

(let ((envdir (getenv "ENVDIR")))
  (setq python-shell-virtualenv-path envdir))

(setq anaconda-mode-port 8887)

(setq anaconda-mode-debug t)

;; Server bootstrap.

(require 'anaconda-mode)

(anaconda-mode-start-node)

(sleep-for 5) ;; Wait for anaconda_mode server will ready to work.
