(require 'ert)

;; Test definitions.

(ert-deftest test-anaconda-mode-running ()
  "Test if anaconda_mode running successfully."
  (anaconda-mode-start-node)
  (should (anaconda-mode-running-p))
  (should (get-buffer "*anaconda*")))

(ert-deftest test-anaconda-mode-virtualenv ()
  "Check that anaconda_mode start with proper python executable."
  (should (string= (anaconda-mode-python)
                   (getenv "ENVPYTHON"))))

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
