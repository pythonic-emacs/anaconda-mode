(require 'ert)

(require 'anaconda-eldoc)

(ert-deftest test-anaconda-eldoc-init ()
  (anaconda-eldoc 1)
  (should (eq eldoc-documentation-function
              'anaconda-eldoc-function)))

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
