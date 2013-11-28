(require 'ert)
(require 'company-jedi)

;; Test definitions.

(ert-deftest test-jedi-running ()
  "Test if jedi running successfully."
  (should (company-jedi-running-p)))

(ert-deftest test-jedi-candidates-json ()
  "Test jedi completion json request string generator."
  (make-test-file "import json; json.l" 1 19)
  (should (equal (company-jedi-request-json "candidates")
                 (concat "{"
                         ""    "\"command\":\"candidates\", "
                         ""    "\"attributes\":{"
                         ""          "\"source\":\"import json; json.l\", "
                         ""          "\"line\":1, "
                         ""          "\"column\":19, "
                         ""          "\"point\":19, "
                         ""          "\"path\":" (company-jedi-encode default-path) ", "
                         ""          "\"company_prefix\":\"\", "
                         ""          "\"company_arg\":\"\""
                         ""    "}"
                         "}"))))

(ert-deftest test-jedi-candidates ()
  "Completion request must return candidates list."
  (make-test-file "import json; json.l" 1 19)
  (should (equal (company-jedi-candidates)
                 '("load" "loads"))))

(ert-deftest test-jedi-location-json ()
  "Test doto_definition json generator."
  (make-test-file)
  (should (equal (company-jedi-request-json "location")
                 (concat "{"
                         ""    "\"command\":\"location\", "
                         ""    "\"attributes\":{"
                         ""          "\"source\":" (company-jedi-encode default-content) ", "
                         ""          "\"line\":8, "
                         ""          "\"column\":1, "
                         ""          "\"point\":103, "
                         ""          "\"path\":" (company-jedi-encode default-path) ", "
                         ""          "\"company_prefix\":\"\", "
                         ""          "\"company_arg\":\"\""
                         ""    "}"
                         "}"))))

(ert-deftest test-jedi-location ()
  "Test find definition at point."
  (make-test-file)
  (should (equal (company-jedi-location)
                 (cons default-path 6))))

(ert-deftest test-jedi-multiple-location ()
  "Test non determined locations."
  (make-test-file "if True:
    a = 1
else:
    a = \"a\"

a" 6 1)
  (should (equal (company-jedi-location)
                 (cons default-path 2))))

;; Helper functions.

(defvar root-directory (file-name-directory load-file-name))

(defvar default-content
  "def my_func():
    print 'called'

alias = my_func
my_list = [1, None, alias]
inception = my_list[2]

inception()")

(defvar default-line 8)

(defvar default-column 1)

(defvar default-path (concat root-directory "log/simple.py"))

(defun make-test-file (&optional content line column path)
  "Create test file."
  (find-file (or path default-path))
  (erase-buffer)                        ; Remove side effect from multiple open-file operations.
  (insert (or content default-content))
  (goto-line (or line default-line))
  (move-to-column (or column default-column)))

(defun mock-completing-read (prompt collection)
  "Emulate user chose."
  (car collection))

(setq company-jedi-completing-read-function 'mock-completing-read)

(setq company-jedi-port 8000)

(setq company-jedi-command
      (concat (file-name-as-directory user-emacs-directory)
              "jedi/venv/bin/python3 -m start_jedi --debug"))

(company-jedi-start)

(when noninteractive
  (sleep-for 5) ;; Wait for start_jedi server will ready to work.
  (ert-run-tests-batch-and-exit))
