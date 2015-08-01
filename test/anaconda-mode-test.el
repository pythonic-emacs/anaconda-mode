;;; anaconda-mode-test.el --- anaconda-mode test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'anaconda-mode)

;;; Helpers.

(defun wait ()
  "Wait for `anaconda-mode' server start."
  (while (not (anaconda-mode-bound-p))
    (sleep-for 0.5)))

;;; Server.

(ert-deftest test-anaconda-mode-start ()
  "`anaconda-mode' server starts successfully."
  (unwind-protect
      (progn
        (anaconda-mode-start)
        (wait)
        (should (anaconda-mode-running-p)))
    (anaconda-mode-stop)))

(ert-deftest test-anaconda-mode-start-bind-port ()
  "`anaconda-mode' server bind port successfully."
  (unwind-protect
      (progn
        (anaconda-mode-start)
        (wait)
        (should (anaconda-mode-bound-p)))
    (anaconda-mode-stop)))

(ert-deftest test-anaconda-mode-stop ()
  "`anaconda-mode' server stops successfully."
  (anaconda-mode-start)
  (wait)
  (anaconda-mode-stop)
  (should-not (anaconda-mode-running-p)))

(ert-deftest test-anaconda-mode-stop-release-port ()
  "`anaconda-mode' server release port successfully on teardown."
  (anaconda-mode-start)
  (wait)
  (anaconda-mode-stop)
  (should-not (anaconda-mode-bound-p)))

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

;;; Completion.

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

(ert-deftest test-anaconda-mode-complete-in-comments ()
  "Completion in comments must be nil."
  (load-fixture "simple.py" "#imp_|_")
  (should (null (anaconda-mode-complete-thing))))

;;; Documentation.

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

;;; ElDoc.

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

;;; Minor mode.

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
