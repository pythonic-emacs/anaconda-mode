(require 'ert)

(ert-deftest test-anaconda-company-candidates ()
  "Test completion at point."
  (anaconda-mode-start-node)
  (load-fixture "simple.py" "\
def test1(a, b):
    '''First test function.'''
    pass

def test2(c):
    '''Second test function.'''
    pass
test_|_")
  (should (equal (company-anaconda-candidates)
                 '("test1" "test2"))))
