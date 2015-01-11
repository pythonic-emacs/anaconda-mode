;;; anaconda-mode-test.el --- anaconda-mode test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'anaconda-mode)

;; Constants.

(defvar project-path (locate-dominating-file load-file-name "anaconda-mode.el"))
(defvar envdir (f-join project-path ".tox" "py34"))
(defvar envpython (f-join envdir "bin" "python"))

;; Helpers.

(defun load-fixture (filename source)
  "Load FILENAME fixture filled with SOURCE."
  (setq buffer-file-name (expand-file-name filename))
  (insert source)
  (search-backward "_|_")
  (delete-char 3))

;; Server.

(ert-deftest test-anaconda-mode-start ()
  "Test if anaconda_mode.py start successfully."
  (anaconda-mode-start)
  (should (anaconda-mode-running-p)))

(ert-deftest test-anaconda-mode-stop ()
  "Test anaconda_mode.py stop successfully."
  (anaconda-mode-stop)
  (should-not (anaconda-mode-running-p)))

(ert-deftest test-anaconda-mode-restart-virtualenv ()
  "Test if anaconda-mode local server react on VIRTUAL_ENV change."
  (anaconda-mode-start)
  (should-not (s-equals? envpython (car (process-command anaconda-mode-process))))
  (let ((python-shell-virtualenv-path envdir))
    (anaconda-mode-start))
  (should (s-equals? envpython (car (process-command anaconda-mode-process)))))

(ert-deftest test-anaconda-mode-restart-python-path ()
  "Check if anaconda-mode server react on PYTHONPATH change."
  (anaconda-mode-start)
  (let ((process-environment '("PYTHONPATH=/usr/lib/python3.4")))
    (should (anaconda-mode-need-restart))))

(ert-deftest test-anaconda-mode-python ()
  "Check that anaconda_mode detect proper python executable."
  (let ((python-shell-virtualenv-path envdir))
    (should (string= envpython (anaconda-mode-python)))))

(ert-deftest test-anaconda-mode-process-filter ()
  "Anaconda mode process filter should detect process port."
  (let* ((process (start-process "*python*" "*python*" "python" "-V"))
         (output "anaconda_mode port 24970\n")
         (anaconda-mode-port nil))
    (anaconda-mode-process-filter process output)
    (should (numberp anaconda-mode-port))))

(ert-deftest test-anaconda-mode-process-filter-error ()
  "Anaconda mode process filter should ignore any trash output."
  (let* ((process (start-process "*python*" "*python*" "python" "-V"))
         (output "Process anaconda_mode finished")
         (anaconda-mode-port nil))
    (anaconda-mode-process-filter process output)
    (should-not anaconda-mode-port)))

;; Connection.

(ert-deftest test-anaconda-mode-connect ()
  "Anaconda mode should successfully connect to server."
  (anaconda-mode-connect)
  (should (anaconda-mode-connected-p)))

(ert-deftest test-anaconda-mode-disconnect ()
  "Anaconda mode should successfully disconnect."
  (anaconda-mode-disconnect)
  (should-not (anaconda-mode-connected-p)))

(ert-deftest test-anaconda-mode-need-reconnect ()
  "Anaconda mode should reconnect when connnection settings changes."
  (anaconda-mode-connect)
  (let ((anaconda-mode-port 21))
    (should (anaconda-mode-need-reconnect))))

(ert-deftest test-anaconda-mode-reconnect ()
  "Anaconda mode need to reconnect on settings change.
Even if settings are broken."
  (anaconda-mode-connect)
  (let ((anaconda-mode-port 21))
    ;; Error must occur here since FTP reject JSONRPC connections.
    ;; But this signify that we really try to restart on settings
    ;; change.
    (should-error (anaconda-mode-connect))))

;; Interaction.

(ert-deftest test-anaconda-mode-remote ()
  "Test anaconda mode setup remote environment properly."
  (let (anaconda-mode-remote-p
        anaconda-mode-host
        anaconda-mode-port)
    (anaconda-mode-remote "127.0.0.1" "24971")
    (should anaconda-mode-remote-p)
    (should (equal anaconda-mode-host "127.0.0.1"))
    (should (equal anaconda-mode-port "24971"))))

(ert-deftest test-anaconda-mode-local ()
  "Test anaconda mode setup local environment properly."
  (let ((anaconda-mode-remote-p t)
        (anaconda-mode-host "127.0.0.1")
        (anaconda-mode-port "24971"))
    (anaconda-mode-local)
    (should-not anaconda-mode-remote-p)
    (should (equal anaconda-mode-host "localhost"))
    (should-not anaconda-mode-port)))

(ert-deftest test-anaconda-mode-file-name-local ()
  (let ((buffer-file-name "test.py"))
    (should (equal "test.py" (anaconda-mode-file-name)))))

(ert-deftest test-anaconda-mode-file-name-tramp ()
  (let ((buffer-file-name "/ssh:news@news.my.domain:/opt/news/etc/test.py"))
    (should (equal "/opt/news/etc/test.py" (anaconda-mode-file-name)))))

;; Completion.

(ert-deftest test-anaconda-mode-complete ()
  "Test completion at point."
  (load-fixture "simple.py" "\
def test1(a, b):
    '''First test function.'''
    pass

def test2(c):
    '''Second test function.'''
    pass
test_|_")
  (should (equal (anaconda-mode-complete-thing)
                 '("test1" "test2"))))

(ert-deftest test-anaconda-mode-complete-with-tabulation ()
  "Test completion at point with tab indentation."
  (load-fixture "simple.py" "\
import sys
if True:
	sys.api_|_")
  (should (equal (anaconda-mode-complete-thing)
                 '("api_version"))))

;; Documentation.

(ert-deftest test-anaconda-mode-doc ()
  "Test documentation string search."
  (load-fixture "simple.py" "\
def f_|_(a, b=1):
    '''Docstring for f.'''
    pass")
  (anaconda-mode-view-doc)
  (should (equal (with-current-buffer (get-buffer "*anaconda-doc*")
                   (buffer-string))
                 "\
simple - def f
========================================
f(a, b = 1)

Docstring for f.")))

(ert-deftest test-anaconda-mode-doc-window-focus ()
  "Test documentation window focus."
  (load-fixture "simple.py" "from os import path_|_")
  (anaconda-mode-view-doc)
  (should (equal "*anaconda-doc*"
                 (buffer-name))))

;; ElDoc.

(ert-deftest test-anaconda-eldoc-existing ()
  (load-fixture "simple.py" "\
def fn(a, b):
    pass
fn(_|_")
  (should (equal (anaconda-eldoc-function)
                 "fn(a, b)")))

(ert-deftest test-anaconda-eldoc-invalid ()
  (load-fixture "simple.py" "invalid(_|_")
  (should-not (anaconda-eldoc-function)))

(ert-deftest test-anaconda-eldoc-ignore-errors ()
  (let ((anaconda-mode-directory (f-root))
        (anaconda-mode-port nil))
    (should-not (anaconda-eldoc-function))))

;; Minor mode.

(ert-deftest test-anaconda-mode-enable ()
  (with-temp-buffer
    (anaconda-mode 1)
    (should (eq (car completion-at-point-functions)
                'anaconda-mode-complete-at-point))
    (should (eq eldoc-documentation-function 'anaconda-eldoc-function))))

(ert-deftest test-anaconda-mode-disable ()
  (with-temp-buffer
    (anaconda-mode 1)
    (anaconda-mode -1)
    (should-not (eq (car completion-at-point-functions)
                    'anaconda-mode-complete-at-point))
    (should-not (eq eldoc-documentation-function 'anaconda-eldoc-function))))

(provide 'anaconda-mode-test)

;;; anaconda-mode-test.el ends here
