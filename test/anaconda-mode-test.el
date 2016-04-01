;;; anaconda-mode-test.el --- anaconda-mode test suite

;;; Commentary:

;;; Code:


;;; Server.

(ert-defintegration test-anaconda-mode-start ()
  "`anaconda-mode' server starts successfully."
  (anaconda-mode-start)
  (wait)
  (should (anaconda-mode-running-p)))

(ert-defintegration test-anaconda-mode-start-bind-port ()
  "`anaconda-mode' server bind port successfully."
  (anaconda-mode-start)
  (wait)
  (should (anaconda-mode-bound-p)))

(ert-defintegration test-anaconda-mode-stop ()
  "`anaconda-mode' server stops successfully."
  (anaconda-mode-start)
  (wait)
  (anaconda-mode-stop)
  (should-not (anaconda-mode-running-p)))

(ert-defintegration test-anaconda-mode-stop-release-port ()
  "`anaconda-mode' server release port successfully on teardown."
  (anaconda-mode-start)
  (wait)
  (anaconda-mode-stop)
  (should-not (anaconda-mode-bound-p)))

(ert-defintegration test-anaconda-mode-create-server-directory ()
  "`anaconda-mode-ensure-directory-code' must create
`anaconda-mode-server-directory'."
  (run anaconda-mode-ensure-directory-command
       (anaconda-mode-server-directory))
  (should (zerop (run "
import sys, os
if not os.path.isdir(os.path.expanduser(sys.argv[-1])):
    os._exit(1)  # IPython again.
" (anaconda-mode-server-directory)))))

(ert-defintegration test-anaconda-mode-install-server ()
  "`anaconda-mode-install-server-code' must install `anaconda-mode' server."
  (run anaconda-mode-ensure-directory-command
       (anaconda-mode-server-directory))
  (run anaconda-mode-install-server-command
       (anaconda-mode-server-directory)
       anaconda-mode-server-version)
  (should (zerop (run anaconda-mode-check-installation-command
                      (anaconda-mode-server-directory)))))

(ert-defintegration test-anaconda-mode-restart-on-environment-change ()
  "`anaconda-mode' server will be restarted if any variable of
the pythonic environment (for example `python-shell-interpreter')
was changed."
  (let (id1 id2)
    (anaconda-mode-start)
    (wait)
    (setq id1 (process-id anaconda-mode-process))
    (let ((python-shell-interpreter "python3"))
      (anaconda-mode-start)
      (wait)
      (setq id2 (process-id anaconda-mode-process)))
    (should-not (equal id1 id2))))

(ert-defintegration test-anaconda-mode-restart-on-installation-directory-change ()
  "`anaconda-mode' server will be restarted if user change server
installation directory."
  (let (id1 id2)
    (anaconda-mode-start)
    (wait)
    (setq id1 (process-id anaconda-mode-process))
    (let ((anaconda-mode-installation-directory "~/.emacs.d/anaconda_mode"))
      (anaconda-mode-start)
      (wait)
      (setq id2 (process-id anaconda-mode-process)))
    (should-not (equal id1 id2))
    (should (zerop (run "
import sys, os
if not os.path.isdir(os.path.expanduser(sys.argv[-1])):
    os._exit(1)  # IPython again.
" "~/.emacs.d/anaconda_mode")))))

(ert-defintegration test-anaconda-mode-not-restart-in-the-same-environment ()
  "`anaconda-mode' server will not be restarted if pythonic
environment keeps the same."
  (let (id1 id2)
    (anaconda-mode-start)
    (wait)
    (setq id1 (process-id anaconda-mode-process))
    (anaconda-mode-start)
    (wait)
    (setq id2 (process-id anaconda-mode-process))
    (should (equal id1 id2))))

(ert-defintegration test-anaconda-mode-on-start-callback ()
  "Run callback passed on server start."
  (let (var)
    (anaconda-mode-start (lambda () (setq var t)))
    (wait)
    (should var)))

(ert-defintegration test-anaconda-mode-on-start-concurrent-callback ()
  "Ignore any callback started during anaconda-mode installation
process before anaconda-mode successfully bound its port."
  (let (foo bar)
    (anaconda-mode-start (lambda () (setq foo t)))
    (anaconda-mode-start (lambda () (setq bar t)))
    (wait)
    (should foo)
    (should-not bar)))

(ert-defintegration test-anaconda-mode-after-start-callback ()
  "Run callback passed after server start."
  (let (var)
    (anaconda-mode-start)
    (wait)
    (anaconda-mode-start (lambda () (setq var t)))
    (should var)))

(ert-deftest test-anaconda-mode-server-directory ()
  "Calculate server directory.
File name expansion should not be done.  It must happens inside
pythonic library with tramp connection add as necessary."
  (should (equal "~/.emacs.d/anaconda-mode/0.1.1"
                 (anaconda-mode-server-directory))))

(ert-deftest test-anaconda-mode-host ()
  "Get server address from `python-shell-interpreter' value."
  (let ((python-shell-interpreter "python"))
    (should (equal "127.0.0.1" (anaconda-mode-host)))))

(ert-deftest test-anaconda-mode-host-remote ()
  "Get server address from tramp `python-shell-interpreter' value."
  (let ((python-shell-interpreter "/ssh:test@10.120.4.4:/usr/bin/python"))
    (should (equal "10.120.4.4" (anaconda-mode-host)))))

(ert-deftest test-anaconda-mode-host-remote-with-port ()
  "Get server address from tramp `python-shell-interpreter' with port specified."
  (let ((python-shell-interpreter "/ssh:test@10.120.4.4#2222:/usr/bin/python"))
    (should (equal "10.120.4.4" (anaconda-mode-host)))))


;;; JSONRPC implementation.

(ert-defintegration test-anaconda-mode-call ()
  "Perform remote procedure call without already started server.
We make request knowingly so response shouldn't be null."
  (let (result)
    (with-current-buffer (fixture "import sys" 1 10)
      (anaconda-mode-call "complete" (lambda (res) (setq result res)))
      (wait)
      (sleep-for 1)
      (should (< 0 (length result))))))

(ert-defintegration test-anaconda-mode-call-callback-current-buffer ()
  "Run response callback in the request buffer."
  (with-current-buffer (fixture "import sys" 1 10)
    (let ((request-buffer (current-buffer))
          response-buffer)
      (anaconda-mode-call "complete" (lambda (res) (setq response-buffer (current-buffer))))
      (wait)
      (sleep-for 1)
      (should (equal request-buffer response-buffer)))))

(ert-defintegration test-anaconda-mode-jsonrpc ()
  "Perform remote procedure call.
We make request knowingly so response shouldn't be null."
  (let (result)
    (with-current-buffer (fixture "import sys" 1 10)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc "complete" (lambda (res) (setq result res)))
      (sleep-for 1)
      (should (< 0 (length result))))))

(ert-defintegration test-anaconda-mode-jsonrpc-error-response ()
  "Skip callback execution if rpc call return error response."
  (let (result)
    (with-current-buffer (fixture "import sys" 1 10)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc "wrong_method" (lambda (res) (setq result res)))
      (sleep-for 1)
      (should-not result))))

(ert-defintegration test-anaconda-mode-jsonrpc-remove-http-buffer ()
  "Remove *http* buffer leaved after `url-retrieve' function call."
  (with-current-buffer (fixture "import sys" 1 10)
    (anaconda-mode-start)
    (wait)
    (anaconda-mode-jsonrpc "complete" (lambda (res)))
    (sleep-for 1)
    (should-not
     (--filter (s-starts-with? " *http" (buffer-name it))
               (buffer-list)))))

(ert-defintegration test-anaconda-mode-jsonrpc-remove-http-buffer-on-callback-error ()
  "Remove *http* buffer leaved after `url-retrieve' function call
even if an error occurs in response callback."
  (with-current-buffer (fixture "import sys" 1 10)
    (anaconda-mode-start)
    (wait)
    (anaconda-mode-jsonrpc "complete" (lambda (res) (error "Shit happens")))
    (ignore-errors (sleep-for 1))
    (should-not
     (--filter (s-starts-with? " *http" (buffer-name it))
               (buffer-list)))))

(ert-deftest test-anaconda-mode-jsonrpc-show-server-response-on-unreadable-response ()
  "Show server HTTP response if parser meat unexpected character."
  (with-current-buffer (fixture "" 1 0)
    (let ((handler (anaconda-mode-create-response-handler nil nil)))
      (with-temp-buffer
        (insert "I'm not a JSON")
        (goto-char (point-min))
        (should (equal "Can not read anaconda-mode server response"
                       (funcall handler nil)))
        (should (equal anaconda-mode-response-buffer
                       (buffer-name (window-buffer (selected-window)))))
        (should (equal "# status: nil\n# point: 1\nI'm not a JSON"
                       (buffer-string)))))))

(ert-deftest test-anaconda-mode-jsonrpc-show-server-response-on-json-end-of-file ()
  "Show server HTTP response if parser meat end of file."
  (with-current-buffer (fixture "" 1 0)
    (let ((handler (anaconda-mode-create-response-handler nil nil)))
      (with-temp-buffer
        (insert "I'm not a JSON")
        (should (equal "Can not read anaconda-mode server response"
                       (funcall handler nil)))
        (should (equal anaconda-mode-response-buffer
                       (buffer-name (window-buffer (selected-window)))))
        (should (equal "# status: nil\n# point: 15\nI'm not a JSON"
                       (buffer-string)))))))

(ert-deftest test-anaconda-mode-jsonrpc-common-error-message ()
  "JSONRPC specification allow to pass common structure within error filed."
  (with-current-buffer (fixture "" 1 0)
    (let ((handler (anaconda-mode-create-response-handler nil nil)))
      (with-temp-buffer
        (insert "HTTP/1.1 400 Bad Request
Server: BaseHTTP/0.6 Python/3.4.3
Date: Sun, 14 Feb 2016 11:15:38 GMT
Content-Length: 85

{\"id\": 1, \"error\": {\"code\": -32601, \"message\": \"Method not found\"}, \"jsonrpc\": \"2.0\"}")
        (goto-char (point-min))
        (should (equal "Method not found" (funcall handler nil)))))))

(ert-deftest test-anaconda-mode-jsonrpc-error-message-with-data-field ()
  "JSONRPC specification allow to pass additional structure within error filed."
  (with-current-buffer (fixture "" 1 0)
    (let ((handler (anaconda-mode-create-response-handler nil nil)))
      (with-temp-buffer
        (insert "HTTP/1.1 400 Bad Request
Server: BaseHTTP/0.6 Python/3.4.3
Date: Sun, 14 Feb 2016 11:15:38 GMT
Content-Length: 113

{\"id\": 1, \"error\": {\"code\": -32601, \"message\": \"Method not found\", \"data\": \"Nice try, but no\"}, \"jsonrpc\": \"2.0\"}")
        (goto-char (point-min))
        (should (equal "Method not found: Nice try, but no" (funcall handler nil)))))))

(ert-defintegration test-anaconda-mode-jsonrpc-skip-response-on-point-movement ()
  "Don't run response callback if point position was changed."
  (let (result)
    (with-current-buffer (fixture "import s " 1 8)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc "complete" (lambda (res) (setq result res)))
      (forward-char)
      (sleep-for 1)
      (should-not result))))

(ert-defintegration test-anaconda-mode-jsonrpc-skip-response-on-buffer-switch ()
  "Don't run response callback if user switch the buffer."
  (let (result)
    (with-current-buffer (fixture "import s" 1 8)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc "complete" (lambda (res) (setq result res)))
      (switch-to-buffer "*scratch*")
      ;; Avoid false positive test pass in the case point were
      ;; set to different places in different buffers.
      (erase-buffer)
      (insert "import s")
      (sleep-for 1)
      (should-not result))))

(ert-defintegration test-anaconda-mode-jsonrpc-skip-response-on-window-switch ()
  "Don't run response callback if user switch the window."
  (let (result)
    (with-current-buffer (fixture "import s" 1 8)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc "complete" (lambda (res) (setq result res)))
      (switch-to-buffer-other-window (current-buffer))
      (sleep-for 1)
      (should-not result))))

(ert-defintegration test-anaconda-mode-jsonrpc-skip-response-on-modified-tick-change ()
  "Don't run response callback if the `buffer-chars-modified-tick' was changed."
  (let (result)
    (with-current-buffer (fixture "import s" 1 8)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc "complete" (lambda (res) (setq result res)))
      (just-one-space)
      (backward-delete-char 1)
      (sleep-for 1)
      (should-not result))))

(ert-deftest test-anaconda-mode-jsonrpc-request ()
  "Prepare JSON encoded data for procedure call."
  (with-current-buffer (fixture "import sys" 1 10)
    (equal (anaconda-mode-jsonrpc-request "echo")
           "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"echo\",\"params\":{\"source\":\"import sys\",\"line\":1,\"column\":10,\"path\":null}}")))

(ert-deftest test-anaconda-mode-jsonrpc-request-data ()
  "Prepare data for remote procedure call."
  (with-current-buffer (fixture "import datetime" 1 15 (concat home-directory "simple.py"))
    (should (equal (anaconda-mode-jsonrpc-request-data "echo")
                   `((jsonrpc . "2.0")
                     (id . 1)
                     (method . "echo")
                     (params . ((source . "import datetime")
                                (line . 1)
                                (column . 15)
                                (path . ,(pythonic-file-name (concat home-directory "simple.py"))))))))))

(ert-deftest test-anaconda-mode-jsonrpc-request-data-with-tabulation ()
  "Prepare data for remote procedure call from buffer contained
tabulation characters."
  (with-current-buffer (fixture "
if True:
	sys.api" 3 8 (concat home-directory "simple.py"))
    (should (equal (anaconda-mode-jsonrpc-request-data "echo")
                   `((jsonrpc . "2.0")
                     (id . 1)
                     (method . "echo")
                     (params . ((source . "
if True:
	sys.api")
                                (line . 3)
                                (column . 8)
                                (path . ,(pythonic-file-name (concat home-directory "simple.py"))))))))))

(ert-deftest test-anaconda-mode-jsonrpc-request-data-tramp-file ()
  "Prepare data for remote procedure call from the tramp buffer."
  (let ((python-shell-interpreter "/ssh:test@10.120.4.4:/usr/bin/python"))
    (with-current-buffer
        (fixture "imp" 1 3 "/ssh:test@10.120.4.4:/home/test/simple.py")
      (should (equal (anaconda-mode-jsonrpc-request-data "echo")
                     `((jsonrpc . "2.0")
                       (id . 1)
                       (method . "echo")
                       (params . ((source . "imp")
                                  (line . 1)
                                  (column . 3)
                                  (path . "/home/test/simple.py")))))))))

(ert-deftest test-anaconda-mode-jsonrpc-request-data-tramp-file-mismatch-interpreter ()
  "Prepare data for remote procedure call from the tramp buffer
if interpreter doesn't match."
  (let ((python-shell-interpreter "/ssh:test@10.120.4.5:/usr/bin/python"))
    (with-current-buffer
        (fixture "imp" 1 3 "/ssh:test@10.120.4.4:/home/test/simple.py")
      (should (equal (anaconda-mode-jsonrpc-request-data "echo")
                     `((jsonrpc . "2.0")
                       (id . 1)
                       (method . "echo")
                       (params . ((source . "imp")
                                  (line . 1)
                                  (column . 3)
                                  (path . nil)))))))))

(ert-defintegration test-anaconda-mode-jsonrpc-add-path-prefix ()
  "The `module-path' fields should point to the same host as python interpreter."
  (let (module-path)
    (with-current-buffer (fixture "from os import getenv" 1 21)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc
       "complete"
       (lambda (res)
         (setq module-path (cdr (assoc 'module-path (car res))))))
      (sleep-for 1)
      (if (pythonic-remote-p)
          (should (s-starts-with-p
                   (pythonic-tramp-connection)
                   module-path))
        (should-not (tramp-tramp-file-p module-path))))))

(ert-defintegration test-anaconda-mode-jsonrpc-skip-path-prefix-on-builtins ()
  "The `module-path' of builtins equals `nil'.  We shouldn't add
tramp prefix for this situation."
  (let (module-path)
    (with-current-buffer (fixture "None" 1 4)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc
       "goto_definitions"
       (lambda (res)
         (setq module-path (cdr (assoc 'module-path (car res))))))
      (sleep-for 1)
      (should-not module-path))))


;;; Completion.

(ert-deftest test-anaconda-mode-complete-extract-names ()
  "Extract names from complete response."
  (let ((result '(((module-path . "/vagrant/simple.py")
                   (docstring . "test1(a, b)")
                   (line . 1)
                   (module-name . "simple")
                   (column . 4)
                   (type . "function")
                   (name . "test1")
                   (full-name . "simple.test1")
                   (description . "function: simple.test1"))
                  ((module-path . "/vagrant/simple.py")
                   (docstring . "test2(c)")
                   (line . 5)
                   (module-name . "simple")
                   (column . 4)
                   (type . "function")
                   (name . "test2")
                   (full-name . "simple.test2")
                   (description . "function: simple.test2")))))
    (should (equal '("test1" "test2")
                   (anaconda-mode-complete-extract-names result)))))

(ert-deftest test-anaconda-mode-complete-extract-names-properties ()
  "Set properties on each completion candidate name."
  (let* ((result '(((module-path . "/vagrant/simple.py")
                    (docstring . "test1(a, b)")
                    (line . 1)
                    (module-name . "simple")
                    (column . 4)
                    (type . "function")
                    (name . "test1")
                    (full-name . "simple.test1")
                    (description . "function: simple.test1"))))
         (name (car (anaconda-mode-complete-extract-names result))))
    (should (get-text-property 0 'description name))
    (should (get-text-property 0 'module-path name))
    (should (get-text-property 0 'line name))
    (should (get-text-property 0 'docstring name))))

(ert-deftest test-anaconda-mode-complete-extract-description ()
  "Set description property on each completion candidate name."
  (let ((result '(((module-path . "/vagrant/simple.py")
                   (docstring . "test1(a, b)")
                   (line . 1)
                   (module-name . "simple")
                   (column . 4)
                   (type . "function")
                   (name . "test1")
                   (full-name . "simple.test1")
                   (description . "function: simple.test1"))
                  ((module-path . "/vagrant/simple.py")
                   (docstring . "test2(c)")
                   (line . 5)
                   (module-name . "simple")
                   (column . 4)
                   (type . "function")
                   (name . "test2")
                   (full-name . "simple.test2")
                   (description . "function: simple.test2")))))
    (should (equal "function: simple.test1"
                   (get-text-property
                    0 'description
                    (car (anaconda-mode-complete-extract-names result)))))))

(ert-deftest test-anaconda-mode-complete-extract-description-statement ()
  "Don't extract whole statement source code as its definition property."
  (let ((result '(((description . "statement: \napilevel = \"2.0\"")
                   (type . "statement")
                   (module-path . "/home/vagrant/.pyenv/versions/3.4.4/lib/python3.4/sqlite3/dbapi2.py")
                   (docstring . "")
                   (column . 0)
                   (module-name . "dbapi2")
                   (line . 33)
                   (name . "apilevel")
                   (full-name . "dbapi2")))))
    (should (equal "statement"
                   (get-text-property
                    0 'description
                    (car (anaconda-mode-complete-extract-names result)))))))

(ert-deftest test-anaconda-mode-complete-callback ()
  "Completion function must insert common candidates base."
  (let ((result '(((module-path . "/vagrant/simple.py")
                   (docstring . "test1(a, b)")
                   (line . 1)
                   (module-name . "simple")
                   (column . 4)
                   (type . "function")
                   (name . "test1")
                   (full-name . "simple.test1")
                   (description . "function: simple.test1"))
                  ((module-path . "/vagrant/simple.py")
                   (docstring . "test2(c)")
                   (line . 5)
                   (module-name . "simple")
                   (column . 4)
                   (type . "function")
                   (name . "test2")
                   (full-name . "simple.test2")
                   (description . "function: simple.test2")))))
    (with-current-buffer (fixture "t" 1 1)
      (anaconda-mode-complete-callback result)
      (should (looking-back "test")))))

(ert-deftest test-anaconda-mode-complete-callback-completions-buffer ()
  "Completion must show *Completions* buffer if candidates doesn't have same base."
  (let ((result '(((full-name . "bool")
                   (line)
                   (module-name . "builtins")
                   (description . "instance: builtins.bool")
                   (module-path)
                   (name . "True")
                   (docstring . "bool(x) -> bool

Returns True when the argument x is true, False otherwise.
The builtins True and False are the only two instances of the class bool.
The class bool is a subclass of the class int, and cannot be subclassed.")
                   (column)
                   (type . "instance"))
                  ((full-name . "try")
                   (line)
                   (module-name . "builtins")
                   (description . "keyword: builtins.try")
                   (module-path)
                   (name . "Try")
                   (docstring . "")
                   (column)
                   (type . "keyword")))))
    (with-current-buffer (fixture "Tr" 1 2)
      (anaconda-mode-complete-callback result)
      (should (get-buffer "*Completions*")))))

(ert-deftest test-anaconda-mode-complete-callback-completions-annotations ()
  "Completion must show candidate description as annotations in the *Completions* buffer."
  (let ((result '(((full-name . "bool")
                   (line)
                   (module-name . "builtins")
                   (description . "instance: builtins.bool")
                   (module-path)
                   (name . "True")
                   (docstring . "bool(x) -> bool

Returns True when the argument x is true, False otherwise.
The builtins True and False are the only two instances of the class bool.
The class bool is a subclass of the class int, and cannot be subclassed.")
                   (column)
                   (type . "instance"))
                  ((full-name . "try")
                   (line)
                   (module-name . "builtins")
                   (description . "keyword: builtins.try")
                   (module-path)
                   (name . "Try")
                   (docstring . "")
                   (column)
                   (type . "keyword")))))
    (with-current-buffer (fixture "Tr" 1 2)
      (anaconda-mode-complete-callback result)
      (should (equal "In this buffer, type RET to select the completion near point.

Possible completions are:
True <instance: builtins.bool>
Try <keyword: builtins.try>"
                     (with-current-buffer "*Completions*"
                       (buffer-string)))))))

(ert-deftest test-anaconda-mode-complete-callback-completions-annotations-statements ()
  "Don't show statements description in the annotations since it maybe really large."
  (let ((result '(((description . "statement:
__all__ = [\"normcase\",\"isabs\",\"join\",\"splitdrive\",\"split\",\"splitext\",
           \"basename\",\"dirname\",\"commonprefix\",\"getsize\",\"getmtime\",
           \"getatime\",\"getctime\",\"islink\",\"exists\",\"lexists\",\"isdir\",\"isfile\",
           \"ismount\", \"expanduser\",\"expandvars\",\"normpath\",\"abspath\",
           \"samefile\",\"sameopenfile\",\"samestat\",
           \"curdir\",\"pardir\",\"sep\",\"pathsep\",\"defpath\",\"altsep\",\"extsep\",
           \"devnull\",\"realpath\",\"supports_unicode_filenames\",\"relpath\"]")
                   (type . "statement")
                   (line . 19)
                   (full-name . "os.path")
                   (column . 0)
                   (module-name . "posixpath")
                   (name . "__all__")
                   (module-path . "/home/vagrant/.pyenv/versions/3.4.4/lib/python3.4/posixpath.py"))
                  ((description . "function: ntpath._get_altsep")
                   (type . "function")
                   (line . 47)
                   (full-name . "os.path._get_altsep")
                   (column . 4)
                   (module-name . "ntpath")
                   (name . "_get_altsep")
                   (module-path . "/home/vagrant/.pyenv/versions/3.4.4/lib/python3.4/ntpath.py")))))
    (with-current-buffer (fixture "from os.path import _" 1 21)
      (anaconda-mode-complete-callback result)
      (should (equal "In this buffer, type RET to select the completion near point.

Possible completions are:
__all__ <statement>
_get_altsep <function: ntpath._get_altsep>"
                     (with-current-buffer "*Completions*"
                       (buffer-string)))))))

(ert-defintegration test-anaconda-mode-complete ()
  "Test completion at point."
  (with-current-buffer (fixture "t" 1 1)
    (anaconda-mode-complete)
    (wait)
    (sleep-for 1)
    (should (get-buffer "*Completions*"))))

(ert-defintegration test-anaconda-mode-complete-unicode ()
  "Test completion at point works fine with unicode."
  (with-current-buffer (fixture "'你好 世界!'." 1 9)
    (anaconda-mode-complete)
    (wait)
    (sleep-for 1)
    (should (get-buffer "*Completions*"))))

(ert-defintegration test-anaconda-mode-complete-insert-candidates-base ()
  "Completion must insert common candidates base."
  (with-current-buffer (fixture "
def teardown(): pass
tear" 3 4)
    (anaconda-mode-complete)
    (wait)
    (sleep-for 1)
    (should (looking-back "teardown"))))

(ert-defintegration test-anaconda-mode-does-not-complete-in-comments ()
  "Don't run interactive completion inside comment block."
  (with-current-buffer (fixture "#im" 1 3)
    (anaconda-mode-complete)
    (should-not (anaconda-mode-running-p))))


;;; Documentation.

(ert-defintegration test-anaconda-mode-show-doc ()
  "Show documentation buffer on documentation lookup."
  (with-current-buffer (fixture "
def f(a, b=1):
    '''Docstring for f.'''
    pass" 2 5 (concat home-directory "simple.py"))
    (anaconda-mode-show-doc)
    (wait)
    (sleep-for 1)
    (should (equal "*Anaconda*"
                   (buffer-name (window-buffer (selected-window)))))))

(ert-defintegration test-anaconda-mode-show-doc-not-found ()
  "Don't show documentation buffer in the case of missing docs."
  (with-current-buffer (fixture "" 1 0 (concat home-directory "simple.py"))
    (anaconda-mode-show-doc)
    (wait)
    (sleep-for 1)
    (should (equal "simple.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))

(ert-defintegration test-anaconda-mode-show-doc-content ()
  "Format documentation buffer from rpc response."
  (with-current-buffer (fixture "
def f(a, b=1):
    '''Docstring for f.'''
    pass" 2 5 (concat home-directory "simple.py"))
    (anaconda-mode-show-doc)
    (wait)
    (sleep-for 1)
    (should (equal "simple
f(a, b=1)

Docstring for f.

"
                   (with-current-buffer (window-buffer (selected-window))
                     (buffer-string))))))


;;; ElDoc.

(ert-defintegration test-anaconda-mode-eldoc ()
  "`anaconda-mode-eldoc-function' will run `anaconda-mode' server."
  (let (eldoc-last-message)
    (with-current-buffer (fixture "
def test(one, other):
    '''Test if one is other'''
    return one is other

test(" 6 5 (concat home-directory "simple.py"))
      (anaconda-mode-eldoc-function)
      (wait)
      (sleep-for 1)
      (should (equal "test(one, other)" eldoc-last-message)))))

(ert-deftest test-anaconda-mode-eldoc-callback ()
  "Format eldoc string from response."
  (let ((result '((params "one" "other")
                  (name . "test")
                  (index . 0))))
    (should (equal "test(one, other)"
                   (anaconda-mode-eldoc-callback result)))))

(ert-defintegration test-anaconda-mode-eldoc-empty-response ()
  "Don't try to show eldoc on response with empty result."
  (let (eldoc-last-message)
    (with-current-buffer (fixture "invalid(" 1 8 (concat home-directory "simple.py"))
      (anaconda-mode-eldoc-function)
      (wait)
      (sleep-for 1)
      (should-not eldoc-last-message))))

(ert-defintegration test-anaconda-mode-eldoc-no-params ()
  "Show eldoc message for function without arguments."
  (let (eldoc-last-message)
    (with-current-buffer (fixture "
def test(): pass
test(" 3 5 (concat home-directory "simple.py"))
      (anaconda-mode-eldoc-function)
      (wait)
      (sleep-for 1)
      (should (equal "test()" eldoc-last-message)))))

(ert-deftest test-anaconda-mode-eldoc-format-as-single-line ()
  "Format eldoc string as single line."
  (let ((anaconda-mode-eldoc-as-single-line t)
        (result `((params ,@(--iterate it "a" 1000))
                  (name . "test")
                  (index . 0))))
    (should (equal (frame-width)
                   (length (anaconda-mode-eldoc-format result))))))

(ert-defintegration test-anaconda-mode-eldoc-no-index-on-set-spec ()
  "Call signatures on `set' builtin some time return result without index field."
  (let (eldoc-last-message)
    (with-current-buffer (fixture "
data = set([
    1,
    2,
])" 4 0 (concat home-directory "simple.py"))
      (anaconda-mode-eldoc-function)
      (wait)
      (sleep-for 1)
      (should (equal "set()" eldoc-last-message)))))

(ert-defintegration test-anaconda-mode-eldoc-sigrature ()
  "Asynchronous eldoc function must return `nil'."
  (let (eldoc-last-message)
    (with-current-buffer (fixture "
data = set([
    1,
    2,
])" 4 0 (concat home-directory "simple.py"))
      (should-not (anaconda-mode-eldoc-function))
      (wait)
      (sleep-for 1))))


;;; View buffer.

(ert-deftest test-anaconda-mode-view ()
  "Create view buffer filled with content."
  (let ((result "we are here")
        (presenter (lambda (result) (insert result))))
    (anaconda-mode-view result presenter)
    (should (equal "we are here"
                   (with-current-buffer (window-buffer (selected-window))
                     (buffer-string))))))

(ert-deftest test-anaconda-mode-view-definitions-presenter ()
  "Format definitions buffer from rpc call result."
  (let ((result '(((column . 19)
                   (type . "import")
                   (docstring . "")
                   (full-name . "views.views")
                   (name . "views")
                   (module-name . "app")
                   (module-path . "/vagrant/app/__init__.py")
                   (description . "from .views import views")
                   (line . 2))
                  ((column . 0)
                   (type . "statement")
                   (docstring . "")
                   (full-name . "views")
                   (name . "views")
                   (module-name . "views")
                   (module-path . "/vagrant/app/views.py")
                   (description . "views = Blueprint('views', __name__)")
                   (line . 4))
                  ((column . 1)
                   (type . "decorator")
                   (docstring . "")
                   (full-name . "views")
                   (name . "views")
                   (module-name . "views")
                   (module-path . "/vagrant/app/views.py")
                   (description . "@views.route('/')")
                   (line . 7)))))
    (anaconda-mode-view result 'anaconda-mode-view-definitions-presenter)
    (should (equal "app
    from .views import views
views
    views = Blueprint('views', __name__)
    @views.route('/')
"
                   (with-current-buffer (window-buffer (selected-window))
                     (buffer-string))))))

(ert-deftest test-anaconda-mode-view-definitions-presenter-next-error ()
  "Use `next-error' to navigate next definition."
  (let* ((ntpath (run-to-string "
from __future__ import print_function
import ntpath
module_path = ntpath.__file__.replace('.pyc', '.py')
print(module_path, end='')"))
         (posixpath (run-to-string "
from __future__ import print_function
import posixpath
module_path = posixpath.__file__.replace('.pyc', '.py')
print(module_path, end='')"))
         (result `(((description . "def join")
                    (full-name . "os.path.join")
                    (type . "function")
                    (docstring . "join(path, *paths)

")
                    (module-path . ,ntpath)
                    (column . 4)
                    (line . 104)
                    (name . "join")
                    (module-name . "ntpath"))
                   ((description . "def join")
                    (full-name . "os.path.join")
                    (type . "function")
                    (docstring . "join(a, *p)

Join two or more pathname components, inserting '/' as needed.
If any component is an absolute path, all previous path components
will be discarded.  An empty last part will result in a path that
ends with a separator.")
                    (module-path . ,posixpath)
                    (column . 4)
                    (line . 70)
                    (name . "join")
                    (module-name . "posixpath")))))
    (anaconda-mode-view result 'anaconda-mode-view-definitions-presenter)
    (next-error-no-select)
    (should (equal 12 (point)))))

(ert-deftest test-anaconda-mode-view-definitions-presenter-next-error-no-select ()
  "Show definition at point in the no selected buffer."
  (let* ((ntpath (run-to-string "
from __future__ import print_function
import ntpath
module_path = ntpath.__file__.replace('.pyc', '.py')
print(module_path, end='')"))
         (posixpath (run-to-string "
from __future__ import print_function
import posixpath
module_path = posixpath.__file__.replace('.pyc', '.py')
print(module_path, end='')"))
         (result `(((description . "def join")
                    (full-name . "os.path.join")
                    (type . "function")
                    (docstring . "join(path, *paths)

")
                    (module-path . ,ntpath)
                    (column . 4)
                    (line . 104)
                    (name . "join")
                    (module-name . "ntpath"))
                   ((description . "def join")
                    (full-name . "os.path.join")
                    (type . "function")
                    (docstring . "join(a, *p)

Join two or more pathname components, inserting '/' as needed.
If any component is an absolute path, all previous path components
will be discarded.  An empty last part will result in a path that
ends with a separator.")
                    (module-path . ,posixpath)
                    (column . 4)
                    (line . 70)
                    (name . "join")
                    (module-name . "posixpath")))))
    (anaconda-mode-view result 'anaconda-mode-view-definitions-presenter)
    (next-error-no-select)
    (should (equal ntpath
                   (with-current-buffer (window-buffer (previous-window))
                     (buffer-file-name))))))

(ert-deftest test-anaconda-mode-view-insert-module-definition ()
  "Insert definition of single module."
  (anaconda-mode-view-insert-module-definition
   '("module.foo"
     ((description . "definition"))
     ((description . "definition"))))
  (should (equal "module.foo
    definition
    definition
" (buffer-string))))

(ert-deftest test-anaconda-mode-view-insert-module-definition-bold-module-name ()
  "Insert module name with bold font."
  (anaconda-mode-view-insert-module-definition
   '("module.foo"
     ((description . "definition"))
     ((description . "definition"))))
  (should (equal 'bold (get-char-property (point-min) 'face))))

(ert-deftest test-anaconda-mode-view-insert-module-definition-source-face ()
  "Insert definition with source fontification."
  (anaconda-mode-view-insert-module-definition
   '("module.foo"
     ((description . "from foo import bar"))
     ((description . "bar(1)"))))
  (should (equal 'font-lock-keyword-face (get-char-property 18 'face))))

(ert-deftest test-anaconda-mode-view-insert-module-definition-click ()
  "Click on the definition must open desired file."
  (let* ((ntpath (run-to-string "
from __future__ import print_function
import ntpath
module_path = ntpath.__file__.replace('.pyc', '.py')
print(module_path, end='')"))
         (line (string-to-number (run-to-string "
from __future__ import print_function
import re
import sys
filename = sys.argv[-1]
with open(filename) as f:
    text = f.read()
match = re.search(r'^def join\\(', text, re.M)
point = match.start()
line_number = len(text[:point].split('\\n'))
print(line_number, end='')" ntpath)))
         (definition `((description . "def join")
                       (full-name . "os.path.join")
                       (type . "function")
                       (docstring . "join(path, *paths)

")
                       (module-path . ,ntpath)
                       (column . 4)
                       (line . ,line)
                       (name . "join")
                       (module-name . "ntpath"))))
    (anaconda-mode-view-insert-module-definition
     `("module.foo"
       ,definition))
    (push-button 18)
    (should (equal ntpath (buffer-file-name)))
    (should (equal line (line-number-at-pos (point))))
    (should (equal 4 (- (point) (line-beginning-position))))))

(ert-deftest test-anaconda-mode-view-documentation-presenter ()
  "Insert documentation in the view buffer."
  (let ((result '(((type . "function")
                   (docstring . "f(a, b)

I'm documentation string.")
                   (name . "f")
                   (module-name . "simple")
                   (column . 4)
                   (module-path . "/vagrant/simple.py")
                   (full-name . "simple.f")
                   (description . "def f")
                   (line . 1)))))
    (anaconda-mode-view result 'anaconda-mode-view-documentation-presenter)
    (should (equal "simple
f(a, b)

I'm documentation string.

"
                   (with-current-buffer (window-buffer (selected-window))
                     (buffer-string))))))

(ert-deftest test-anaconda-mode-view-documentation-presenter-bold-module-name ()
  "Insert module name with bold font in the documentation view."
  (let ((result '(((description . "def join")
                   (full-name . "os.path.join")
                   (type . "function")
                   (docstring . "join(path, *paths)

")
                   (module-path . "/home/vagrant/.pyenv/versions/3.4.4/lib/python3.4/ntpath.py")
                   (column . 4)
                   (line . 104)
                   (name . "join")
                   (module-name . "ntpath")))))
    (anaconda-mode-view result 'anaconda-mode-view-documentation-presenter)
    (should (equal 'bold (get-char-property (point-min) 'face)))))

(ert-deftest test-anaconda-mode-view-documentation-presenter-multiple-modules ()
  "Format doc buffer for multiple modules."
  (let ((result '(((description . "def join")
                   (full-name . "os.path.join")
                   (type . "function")
                   (docstring . "join(path, *paths)

")
                   (module-path . "/home/vagrant/.pyenv/versions/3.4.4/lib/python3.4/ntpath.py")
                   (column . 4)
                   (line . 104)
                   (name . "join")
                   (module-name . "ntpath"))
                  ((description . "def join")
                   (full-name . "os.path.join")
                   (type . "function")
                   (docstring . "join(a, *p)

Join two or more pathname components, inserting '/' as needed.
If any component is an absolute path, all previous path components
will be discarded.  An empty last part will result in a path that
ends with a separator.")
                   (module-path . "/home/vagrant/.pyenv/versions/3.4.4/lib/python3.4/posixpath.py")
                   (column . 4)
                   (line . 70)
                   (name . "join")
                   (module-name . "posixpath")))))
    (anaconda-mode-view result 'anaconda-mode-view-documentation-presenter)
    (should (equal "ntpath
join(path, *paths)

posixpath
join(a, *p)

Join two or more pathname components, inserting '/' as needed.
If any component is an absolute path, all previous path components
will be discarded.  An empty last part will result in a path that
ends with a separator.

"
                   (with-current-buffer (window-buffer (selected-window))
                     (buffer-string))))))

(ert-deftest test-anaconda-mode-view-make-bold ()
  "Make bold string."
  (should (equal 'bold
                 (get-text-property 0 'face
                                    (anaconda-mode-view-make-bold "test")))))

(ert-deftest test-anaconda-mode-view-make-source ()
  "Make string string fontified as python source."
  (should (equal 'font-lock-keyword-face
                 (get-text-property 0 'face
                                    (anaconda-mode-view-make-source "from")))))

(ert-deftest test-anaconda-mode-view-insert-button ()
  "Insert text button."
  (with-temp-buffer
    (anaconda-mode-view-insert-button "text" nil)
    (should (looking-back "text"))))

(ert-deftest test-anaconda-mode-view-insert-button-click ()
  "Go to definition if click on button."
  (let* ((ntpath (run-to-string "
from __future__ import print_function
import ntpath
module_path = ntpath.__file__.replace('.pyc', '.py')
print(module_path, end='')"))
         (line (string-to-number (run-to-string "
from __future__ import print_function
import re
import sys
filename = sys.argv[-1]
with open(filename) as f:
    text = f.read()
match = re.search(r'^def join\\(', text, re.M)
point = match.start()
line_number = len(text[:point].split('\\n'))
print(line_number, end='')" ntpath)))
         (definition `((description . "def join")
                       (full-name . "os.path.join")
                       (type . "function")
                       (docstring . "join(path, *paths)

")
                       (module-path . ,ntpath)
                       (column . 4)
                       (line . ,line)
                       (name . "join")
                       (module-name . "ntpath"))))
    (anaconda-mode-view-insert-button "text" definition)
    (goto-char (point-min))
    (push-button (point))
    (should (equal ntpath (buffer-file-name)))
    (should (equal line (line-number-at-pos (point))))
    (should (equal 4 (- (point) (line-beginning-position))))))

(ert-deftest test-anaconda-mode-view-insert-button-face ()
  "Insert buttons without any face."
  (anaconda-mode-view-insert-button "text" nil)
  (goto-char (point-min))
  (should-not (get-char-property (point) 'face)))

(ert-deftest test-anaconda-mode-view-mode ()
  "Test anaconda-view major mode."
  (with-temp-buffer
    (anaconda-mode-view-mode)
    (should next-error-function)
    (should buffer-read-only)
    (should (equal 'next-error-no-select (key-binding "n")))
    (should (equal 'previous-error-no-select (key-binding "p")))
    (should (equal 'quit-window (key-binding "q")))))

(ert-deftest test-anaconda-mode-go-back ()
  "Jump backward if buffer was navigated from `anaconda-mode' command."
  (with-current-buffer (fixture "
test

one" 4 3 (concat home-directory "initial.py"))
    (anaconda-mode-find-file '((module-path . "simple.py")
                               (line . 1)
                               (column . 0)))
    (anaconda-mode-go-back)
    (should (equal "initial.py" (f-filename (buffer-file-name))))))

(ert-deftest test-anaconda-mode-go-back-no-backward-file ()
  "Show error if there is no previous navigation buffer."
  (should (equal "No previous buffer" (anaconda-mode-go-back))))

(ert-deftest test-anaconda-mode-find-file-not-set-go-back-definitions ()
  "`anaconda-mode-find-file-generic' doesn't set go back
definition if current buffer doesn't has file name."
  (with-current-buffer (fixture "test" 1 3)
    (anaconda-mode-find-file '((module-path . "simple.py")
                               (line . 1)
                               (column . 0)))
    (should-not anaconda-mode-go-back-definitions)))

(ert-deftest test-anaconda-mode-find-file-builtins ()
  "Show description message if user try to open definition without module name."
  (with-current-buffer (fixture "test" 1 4 (concat home-directory "simple.py"))
    (anaconda-mode-find-file '((column)
                               (description . "class int")
                               (line)
                               (full-name . "int")
                               (module-path)
                               (module-name . "builtins")
                               (name . "int")
                               (type . "instance")))
    (should (equal "simple.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))

(ert-deftest test-anaconda-mode-go-back-twice-no-loop ()
  "Going back twice won't put in a loop"
  (with-current-buffer (fixture "test" 1 4 (concat home-directory "initial.py"))
    (anaconda-mode-find-file '((module-path . "step1.py")
                               (line . 1)
                               (column . 0)
                               ))
    (anaconda-mode-find-file '((module-path . "step2.py")
                               (line . 1)
                               (column . 0)
                               ))
    (anaconda-mode-go-back)
    (anaconda-mode-go-back)
    (should (equal "initial.py" (f-filename (buffer-file-name))))))

(ert-deftest test-anaconda-mode-with-view-buffer-multiple-times ()
  "It is possible to reuse *Anaconda* buffer multiple times without errors."
  (anaconda-mode-with-view-buffer
   (insert "a"))
  (anaconda-mode-with-view-buffer
   (insert "b")))


;;; Definitions.

(ert-defintegration test-anaconda-mode-find-definitions ()
  "Show definitions buffer on documentation lookup."
  (with-current-buffer (fixture "
import random
if random.randint(0, 1):
    def test():
        pass
else:
    def test(a):
        return a

test" 10 3 (concat home-directory "simple.py"))
    (anaconda-mode-find-definitions)
    (wait)
    (sleep-for 1)
    (should (equal "*Anaconda*"
                   (buffer-name (window-buffer (selected-window)))))))

(ert-defintegration test-anaconda-mode-find-definitions-not-found ()
  "Don't show definitions buffer in the case of missing definitions."
  (with-current-buffer (fixture "" 1 0 (concat home-directory "simple.py"))
    (anaconda-mode-find-definitions)
    (wait)
    (sleep-for 1)
    (should (equal "simple.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))

(ert-defintegration test-anaconda-mode-find-definitions-single-definition ()
  "Jump to definition immediately in the case of single definition."
  (with-current-buffer (fixture "from os import getenv" 1 21)
    (anaconda-mode-find-definitions)
    (wait)
    (sleep-for 1)
    (should (equal "os.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))


;;; Assignments.

(ert-defintegration test-anaconda-mode-find-assignments ()
  "Show assignments buffer on documentation lookup."
  (with-current-buffer (fixture "
import random
if random.randint(0, 1):
    def test():
        pass
else:
    def test(a):
        return a

test" 10 3 (concat home-directory "simple.py"))
    (anaconda-mode-find-assignments)
    (wait)
    (sleep-for 1)
    (should (equal "*Anaconda*"
                   (buffer-name (window-buffer (selected-window)))))))

(ert-defintegration test-anaconda-mode-find-assignments-not-found ()
  "Don't show assignments buffer in the case of missing assignments."
  (with-current-buffer (fixture "" 1 0 (concat home-directory "simple.py"))
    (anaconda-mode-find-assignments)
    (wait)
    (sleep-for 1)
    (should (equal "simple.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))

(ert-defintegration test-anaconda-mode-find-assignments-single-assignment ()
  "Jump to assignment immediately in the case of single assignment."
  (with-current-buffer (fixture "from os import getenv" 1 21)
    (anaconda-mode-find-assignments)
    (wait)
    (sleep-for 1)
    (should (equal "os.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))


;;; References.

(ert-defintegration test-anaconda-mode-find-references ()
  "Show references buffer on documentation lookup."
  (with-current-buffer (fixture "
def test():
    pass

if one:
    test()
elif two:
    test()
else:
    test()" 2 6 (concat home-directory "simple.py"))
    (anaconda-mode-find-references)
    (wait)
    (sleep-for 1)
    (should (equal "*Anaconda*"
                   (buffer-name (window-buffer (selected-window)))))))

(ert-defintegration test-anaconda-mode-find-references-not-found ()
  "Don't show references buffer in the case of missing references."
  (with-current-buffer (fixture "" 1 0 (concat home-directory "simple.py"))
    (anaconda-mode-find-references)
    (wait)
    (sleep-for 1)
    (should (equal "simple.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))


;;; Minor mode.

(ert-deftest test-anaconda-mode-enable ()
  "Enable `anaconda-mode'."
  (with-temp-buffer
    (anaconda-mode 1)
    (should anaconda-mode)))

(ert-deftest test-anaconda-mode-disable ()
  "Disable `anaconda-mode'."
  (with-temp-buffer
    (anaconda-mode 1)
    (anaconda-mode -1)
    (should-not anaconda-mode)))

(ert-deftest test-anaconda-eldoc-mode-enable ()
  "Enable `anaconda-eldoc-mode'."
  (with-temp-buffer
    (anaconda-eldoc-mode 1)
    (should (eq eldoc-documentation-function 'anaconda-mode-eldoc-function))
    (should eldoc-mode)))

(ert-deftest test-anaconda-eldoc-mode-disable ()
  "Disable `anaconda-eldoc-mode'."
  (with-temp-buffer
    (anaconda-eldoc-mode 1)
    (anaconda-eldoc-mode -1)
    (should-not (eq eldoc-documentation-function 'anaconda-mode-eldoc-function))
    (should-not eldoc-mode)))

(provide 'anaconda-mode-test)

;;; anaconda-mode-test.el ends here
