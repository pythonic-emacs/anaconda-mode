(require 'ert)

;; Test definitions.

(ert-deftest test-jedi-running ()
  "Test if jedi running successfully."
  (should (company-jedi-running-p)))

(ert-deftest test-jedi-candidates-json ()
  "Test jedi completion json request string generator."
  (load-fixture "test/jedi_candidates/fixtures/simple.py" 2 11)
  (should (equal (company-jedi-request-json "candidates")
                 (concat "{"
                         ""    "\"command\":\"candidates\", "
                         ""    "\"attributes\":{"
                         ""          "\"source\":" (company-jedi-encode (buffer-string)) ", "
                         ""          "\"line\":2, "
                         ""          "\"column\":11, "
                         ""          "\"point\":27, "
                         ""          "\"path\":" (company-jedi-encode (buffer-file-name)) ", "
                         ""          "\"company_prefix\":\"\", "
                         ""          "\"company_arg\":\"\""
                         ""    "}"
                         "}"))))

(ert-deftest test-jedi-candidates ()
  "Completion request must return candidates list."
  (load-fixture "test/jedi_candidates/fixtures/simple.py" 2 11)
  (should (equal (company-jedi-candidates)
                 '("date" "datetime" "datetime_CAPI"))))

(ert-deftest test-jedi-location-json ()
  "Test doto_definition json generator."
  (load-fixture "test/jedi_location/fixtures/simple.py" 8 1)
  (should (equal (company-jedi-request-json "location")
                 (concat "{"
                         ""    "\"command\":\"location\", "
                         ""    "\"attributes\":{"
                         ""          "\"source\":" (company-jedi-encode (buffer-string)) ", "
                         ""          "\"line\":8, "
                         ""          "\"column\":1, "
                         ""          "\"point\":103, "
                         ""          "\"path\":" (company-jedi-encode (buffer-file-name)) ", "
                         ""          "\"company_prefix\":\"\", "
                         ""          "\"company_arg\":\"\""
                         ""    "}"
                         "}"))))

(ert-deftest test-jedi-location ()
  "Test find definition at point."
  (let ((fixture "test/jedi_location/fixtures/simple.py"))
    (load-fixture fixture 8 1)
    (should (equal (company-jedi-location)
                   (cons (fixture-path fixture) 1))))) ; Fist definition will chose by mocked user.

(ert-deftest test-jedi-multiple-location ()
  "Test non determined locations."
  (let ((fixture "test/jedi_location/fixtures/builtins.py"))
    (load-fixture fixture 6 1)
    (should (equal (company-jedi-location)
                   (cons (fixture-path fixture) 2)))))

(ert-deftest test-jedi-reference ()
  "Test fund references."
  (let ((fixture "test/jedi_reference/fixtures/simple.py"))
    (load-fixture fixture 1 4)
    (should (equal (company-jedi-reference)
                   (cons (fixture-path fixture) 4)))))

(ert-deftest test-jedi-doc ()
  "Test found documentation string."
  (load-fixture "test/jedi_doc/fixtures/simple.py" 1 4)
  (should (equal "Document for function f."
                 (with-current-buffer (company-jedi-doc-buffer)
                   (buffer-string)))))

(ert-deftest test-key-list ()
  "Should obtain keys from hash."
  (should (equal '("a" "b")
                 (let ((hash (make-hash-table)))
                   (puthash "a" "bar" hash)
                   (puthash "b" "foo" hash)
                   (key-list hash)))))

(ert-deftest test-jedi-meta ()
  "Company must ignore empty doc strings."
  (load-fixture "test/jedi_doc/fixtures/docless.py" 1 4)
  (should (null (company-jedi-meta))))
