;;; anaconda-mode-test.el --- anaconda-mode test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'anaconda-mode)

;;; Server.

(ert-deftest test-anaconda-mode-start ()
  "Test if anaconda_mode.py start successfully."
  (anaconda-mode-start)
  (should (anaconda-mode-running-p)))

(ert-deftest test-anaconda-mode-stop ()
  "Test anaconda_mode.py stop successfully."
  (anaconda-mode-stop)
  (should-not (anaconda-mode-running-p)))

(ert-deftest test-anaconda-mode-python ()
  "Check that anaconda_mode detect proper python executable."
  (let ((python-shell-virtualenv-path (getenv "ENVDIR")))
    (should (string= (anaconda-mode-python)
                     (getenv "ENVPYTHON")))))

(ert-deftest test-anaconda-mode-process-filter ()
  "Anaconda mode process filter should detect process port."
  (let* ((output "anaconda_mode port 24970\n")
         (anaconda-mode-port nil))
    (anaconda-mode-process-filter nil output)
    (should (numberp anaconda-mode-port))))

(ert-deftest test-anaconda-mode-process-filter-error ()
  "Anaconda mode process filter should ignore any trash output."
  (let* ((output "Process anaconda_mode finished"))
    (should-not (anaconda-mode-process-filter nil output))))

;;; Connection.

(ert-deftest test-anaconda-mode-connect ()
  "Anaconda mode should successfully connect to server."
  (anaconda-mode-connect)
  (should (anaconda-mode-connected-p)))

(ert-deftest test-anaconda-mode-disconnect ()
  "Anaconda mode should successfully disconnect."
  (anaconda-mode-disconnect)
  (should-not (anaconda-mode-connected-p)))

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

(provide 'anaconda-mode-test)

;;; anaconda-mode-test.el ends here
