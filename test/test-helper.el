(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

;; Package setup.

(require 'anaconda-mode)

(let ((envdir (getenv "ENVDIR")))
  (setq python-shell-virtualenv-path envdir))

(setq anaconda-mode-port 8887)

(setq anaconda-mode-debug t)

;; Helper functions.

(defun fixture-path (file)
  "Make absolute FILE path from project path."
  (concat anaconda-mode-directory file))

(defun load-fixture (file line column)
  "Open fixture FILE at LINE COLUMN."
  (find-file-read-only (fixture-path file))
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column column))

(defun mock-completing-read (prompt collection &rest ignored)
  "Emulate user chose for PROMPT COLLECTION skipping IGNORED."
  (car (sort collection 'string<)))

(setq completing-read-function 'mock-completing-read)
