(require 'ert)
(require 'company-jedi)

(ert-deftest test-jedi-running ()
  "Test if jedi running successfully."
  (should (company-jedi-running-p)))

(ert-deftest test-jedi-candidates-json ()
  "Test jedi completion json request string generator."
  (with-temp-buffer
    (insert "import json; json.l")
    (should (equal "{\"command\":\"candidates\", \"attributes\":{\"source\":\"import json; json.l\", \"line\":1, \"column\":19, \"path\":\"\"}}"
                   (company-jedi-candidates-json)))))

(ert-deftest test-jedi-location-json ()
  "Test doto_definition json generator."
  (find-file (concat root-directory "log/simple.py"))
  (insert "def my_func():
    print 'called'

alias = my_func
my_list = [1, None, alias]
inception = my_list[2]

inception()")
  (beginning-of-line)
  (forward-char)
  (should (equal (concat "{\"command\":\"location\", \"attributes\":{\"source\":\"def my_func():\\n    print 'called'\\n\\nalias = my_func\\nmy_list = [1, None, alias]\\ninception = my_list[2]\\n\\ninception()\", \"line\":8, \"column\":1, \"path\":\""
                         (replace-regexp-in-string "/" "\\\\/" root-directory)
                         "log\\/simple.py\"}}")
                 (company-jedi-location-json))))

(ert-deftest test-jedi-request-candidates ()
  "Completion request must return candidates list."
  (should (equal '("load" "loads")
                 (company-jedi-do-request "{\"command\":\"candidates\", \"attributes\":{\"source\":\"import json; json.l\", \"line\":1, \"column\":19, \"path\":\"\"}}"))))

(ert-deftest test-jedi-request-location ()
  "Location request must return location pairs."
  (should (equal (sort
                  (list
                   (cons 'module_path (expand-file-name "log/simple.py" root-directory))
                   (cons 'line 6)
                   (cons 'column 0))
                  'alist<)
                 (sort
                  (car (company-jedi-do-request
                        "{\"command\":\"location\", \"attributes\":{\"source\":\"def my_func():\\n    print 'called'\\n\\nalias = my_func\\nmy_list = [1, None, alias]\\ninception = my_list[2]\\n\\ninception()\", \"line\":8, \"column\":1, \"path\":\"log\\/simple.py\"}}"))
                  'alist<))))

(ert-deftest test-jedi-location ()
  "Test find definition at point."
  (find-file (concat root-directory "log/simple.py"))
  (insert "def my_func():
    print 'called'

alias = my_func
my_list = [1, None, alias]
inception = my_list[2]

inception()")
  (beginning-of-line)
  (forward-char)
  (should (equal (company-jedi-location)
                 (cons (expand-file-name "log/simple.py" root-directory) 6))))

(defvar root-directory (file-name-directory load-file-name))

(company-jedi-start)

(defun alist< (a b)
  "Compare A and B alists by it key name."
  (string< (symbol-name (car a))
           (symbol-name (car b))))

(when noninteractive
  (sleep-for 5) ;; Wait for start_jedi server will ready to work.
  (ert-run-tests-batch-and-exit))
