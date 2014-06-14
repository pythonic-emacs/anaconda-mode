(require 'ert)

(ert-deftest test-anaconda-doc-view-doc ()
  "Test documentation string search."
  (anaconda-mode-start-node)
  (load-fixture "simple.py" "\
def f_|_(a, b=1):
    '''Docstring for f.'''
    pass")
  (anaconda-doc-view-doc)
  (should (equal (with-current-buffer (get-buffer "*anaconda-doc*")
                   (buffer-string))
                 "\
simple - def f
========================================
f(a, b = 1)

Docstring for f.")))
