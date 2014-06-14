(require 'ert)

(ert-deftest test-anaconda-mode-running ()
  "Test if anaconda_mode running successfully."
  (anaconda-mode-start-node)
  (should (anaconda-mode-running-p))
  (should (get-buffer "*anaconda*")))

(ert-deftest test-anaconda-mode-virtualenv ()
  "Check that anaconda_mode start with proper python executable."
  (should (string= (anaconda-mode-python)
                   (getenv "ENVPYTHON"))))
