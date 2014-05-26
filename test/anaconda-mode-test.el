(require 'ert)

;; Test definitions.

(ert-deftest test-anaconda-mode-running ()
  "Test if anaconda_mode running successfully."
  (should (anaconda-mode-running-p)))

(ert-deftest test-anaconda-mode-virtualenv ()
  "Check that anaconda_mode start with proper python executable."
  (should (string= (anaconda-mode-python)
                   (getenv "ENVPYTHON"))))

(ert-deftest test-anaconda-mode-complete ()
  "Test completion at point."
  (load-fixture "test/jedi/fixtures/candidates/simple.py" 14 4)
  (should (equal (anaconda-mode-complete-thing)
                 '("test1" "test2"))))

(ert-deftest test-anaconda-mode-location ()
  "Test find definition at point."
  (let ((fixture "test/jedi/fixtures/location/simple.py"))
    (load-fixture fixture 8 1)
    (should (equal (anaconda-mode-locate-definition)
                   (list (fixture-path fixture) 1 0)))))

(ert-deftest test-anaconda-mode-multiple-location ()
  "Test non determined locations."
  (let ((fixture "test/jedi/fixtures/location/builtins.py"))
    (load-fixture fixture 6 1)
    (should (equal (anaconda-mode-locate-definition)
                   (list (fixture-path fixture) 2 4)))))

(ert-deftest test-anaconda-mode-reference ()
  "Test fund references."
  (let ((fixture "test/jedi/fixtures/reference/simple.py"))
    (load-fixture fixture 1 4)
    (should (equal (anaconda-mode-locate-reference)
                   (list (fixture-path fixture) 4 4)))))

(ert-deftest test-anaconda-mode-doc ()
  "Test documentation string search."
  (load-fixture "test/jedi/fixtures/doc/simple.py" 1 4)
  (should (equal (anaconda-mode-doc-string)
                 "f(a, b = 1)

Document for function f.")))

(ert-deftest test-key-list ()
  "Should obtain keys from hash."
  (should (equal '("a" "b")
                 (let ((hash (make-hash-table)))
                   (puthash "a" "bar" hash)
                   (puthash "b" "foo" hash)
                   (key-list hash)))))
