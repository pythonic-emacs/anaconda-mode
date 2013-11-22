(require 'ert)
(require 'company-jedi)

(company-jedi-start)

(ert-deftest test-company-jedi-candidates ()
  "Test jedi completion."
  (with-temp-buffer
    (insert "import json\n")
    (insert "json.lo")
    (should (equal '("load", "loads")
                   (company-jedi-candidates)))))

(ert-run-tests-batch-and-exit)
