(require 'ert)
(require 'company-jedi)

(ert-deftest test-jedi-running ()
  "Test if jedi running successfully."
  (should (company-jedi-running-p)))

(ert-deftest test-jedi-candidates-request ()
  "Test jedi completion json request string generator."
  (with-temp-buffer
    (insert "import json; json.l")
    (should (equal "{\"command\":\"candidates\", \"args\":{\"source\":\"import json; json.l\", \"line\":1, \"column\":19, \"path\":\"\"}}"
                   (company-jedi-candidates-request)))))

(ert-deftest test-jedi-candidates ()
  "Test jedi completion candidates."
  (with-temp-buffer
    (insert "import json; json.l")
    (should (equal '("load" "loads")
                   (company-jedi-candidates)))))

(company-jedi-start)
(ert-run-tests-batch-and-exit)
