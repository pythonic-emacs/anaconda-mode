;;; anaconda-mode-test.el --- anaconda-mode test suite

;;; Commentary:

;;; Todo:

;; * test `anaconda-mode-host' function
;; * test server start chain behavior
;; * test server start doesn't reinstall server

;;; Code:

(require 'ert)
(require 'eldoc)
(require 'f)
(require 'anaconda-mode)

;;; Helpers.

(defun wait ()
  "Wait for `anaconda-mode' server start."
  (while (not (anaconda-mode-bound-p))
    (sleep-for 0.5)))

(defun run (args)
  "Run python interpreter synchronously with ARGS passed directly to it."
  (call-pythonic :args args))

(defun fixture (source line column &optional path)
  "Open SOURCE fixture.
Put point on LINE at COLUMN position.  Set PATH as current file
name."
  (with-current-buffer (generate-new-buffer "*fixture*")
    (python-mode)
    (insert source)
    (goto-char 0)
    (forward-line (1- line))
    (forward-char column)
    (setq buffer-file-name (and path (f-full path)))
    (switch-to-buffer (current-buffer))
    (current-buffer)))

;;; Server.

(ert-deftest test-anaconda-mode-start ()
  "`anaconda-mode' server starts successfully."
  (unwind-protect
      (progn
        (anaconda-mode-start)
        (wait)
        (should (anaconda-mode-running-p)))
    (anaconda-mode-stop)))

(ert-deftest test-anaconda-mode-start-bind-port ()
  "`anaconda-mode' server bind port successfully."
  (unwind-protect
      (progn
        (anaconda-mode-start)
        (wait)
        (should (anaconda-mode-bound-p)))
    (anaconda-mode-stop)))

(ert-deftest test-anaconda-mode-stop ()
  "`anaconda-mode' server stops successfully."
  (anaconda-mode-start)
  (wait)
  (anaconda-mode-stop)
  (should-not (anaconda-mode-running-p)))

(ert-deftest test-anaconda-mode-stop-release-port ()
  "`anaconda-mode' server release port successfully on teardown."
  (anaconda-mode-start)
  (wait)
  (anaconda-mode-stop)
  (should-not (anaconda-mode-bound-p)))

(ert-deftest test-anaconda-mode-create-server-directory ()
  "`anaconda-mode-ensure-directory-code' must create
`anaconda-mode-server-directory'."
  (run anaconda-mode-ensure-directory-command)
  (should (f-dir? anaconda-mode-server-directory)))

(ert-deftest test-anaconda-mode-install-server ()
  "`anaconda-mode-install-server-code' must install `anaconda-mode' server."
  (run anaconda-mode-install-server-command)
  (should (zerop (run anaconda-mode-check-installation-command))))

(ert-deftest test-anaconda-mode-restart-on-environment-change ()
  "`anaconda-mode' server will be restarted if any variable of
the pythonic environment (for example `python-shell-interpreter')
was changed."
  (unwind-protect
      (let (id1 id2)
        (anaconda-mode-start)
        (wait)
        (setq id1 (process-id anaconda-mode-process))
        (let ((python-shell-interpreter "python3"))
          (anaconda-mode-start)
          (wait)
          (setq id2 (process-id anaconda-mode-process)))
        (should-not (equal id1 id2)))
    (anaconda-mode-stop)))

(ert-deftest test-anaconda-mode-not-restart-in-the-same-envinment ()
  "`anaconda-mode' server will not be restarted if pythonic
environment keeps the same."
  (unwind-protect
      (let (id1 id2)
        (anaconda-mode-start)
        (wait)
        (setq id1 (process-id anaconda-mode-process))
        (anaconda-mode-start)
        (wait)
        (setq id2 (process-id anaconda-mode-process))
        (should (equal id1 id2)))
    (anaconda-mode-stop)))

(ert-deftest test-anaconda-mode-on-start-callback ()
  "Run callback passed on server start."
  (unwind-protect
      (let (var)
        (anaconda-mode-start (lambda () (setq var t)))
        (wait)
        (should var))
    (anaconda-mode-stop)))

(ert-deftest test-anaconda-mode-after-start-callback ()
  "Run callback passed after server start."
  (unwind-protect
      (let (var)
        (anaconda-mode-start)
        (wait)
        (anaconda-mode-start (lambda () (setq var t)))
        (should var))
    (anaconda-mode-stop)))

;;; JSONRPC implementation.

(ert-deftest test-anaconda-mode-call ()
  "Perform remote procedure call without already started server."
  (let (response)
    (with-current-buffer (fixture "import sys" 1 10)
      (unwind-protect
          (progn
            (anaconda-mode-call "complete" (lambda (resp) (setq response resp)))
            (wait)
            (sleep-for 1)
            (should (assoc 'id response)))
        (anaconda-mode-stop)))))

(ert-deftest test-anaconda-mode-call-callback-current-buffer ()
  "Run response callback in the request buffer."
  (with-current-buffer (fixture "import sys" 1 10)
    (let ((request-buffer (current-buffer))
          response-buffer)
      (unwind-protect
          (progn
            (anaconda-mode-call "complete" (lambda (resp) (setq response-buffer (current-buffer))))
            (wait)
            (sleep-for 1)
            (should (equal request-buffer response-buffer)))
        (anaconda-mode-stop)))))

(ert-deftest test-anaconda-mode-jsonrpc ()
  "Perform remote procedure call."
  (let (response)
    (with-current-buffer (fixture "import sys" 1 10)
      (unwind-protect
          (progn
            (anaconda-mode-start)
            (wait)
            (anaconda-mode-jsonrpc "complete" (lambda (resp) (setq response resp)))
            (sleep-for 1)
            (should (assoc 'id response)))
        (anaconda-mode-stop)))))

(ert-deftest test-anaconda-mode-jsonrpc-remove-http-buffer ()
  "Remove *http* buffer leaved after `url-retrieve' function call."
  (with-current-buffer (fixture "import sys" 1 10)
    (unwind-protect
        (progn
          (anaconda-mode-start)
          (wait)
          (anaconda-mode-jsonrpc "complete" (lambda (resp)))
          (sleep-for 1)
          (should-not
           (--filter (s-starts-with? " *http" (buffer-name it))
                     (buffer-list))))
      (anaconda-mode-stop))))

(ert-deftest test-anaconda-mode-jsonrpc-remove-http-buffer-on-callback-error ()
  "Remove *http* buffer leaved after `url-retrieve' function call
even if an error occurs in response callback."
  (with-current-buffer (fixture "import sys" 1 10)
    (unwind-protect
        (progn
          (anaconda-mode-start)
          (wait)
          (anaconda-mode-jsonrpc "complete" (lambda (resp) (error "Shit happens")))
          (ignore-errors (sleep-for 1))
          (should-not
           (--filter (s-starts-with? " *http" (buffer-name it))
                     (buffer-list))))
      (anaconda-mode-stop))))

(ert-deftest test-anaconda-mode-jsonrpc-skip-response-on-point-movement ()
  "Don't run response callback if point position was changed."
  (let (response)
    (with-current-buffer (fixture "import s " 1 8)
      (unwind-protect
          (progn
            (anaconda-mode-start)
            (wait)
            (anaconda-mode-jsonrpc "complete" (lambda (resp) (setq response resp)))
            (forward-char)
            (sleep-for 1)
            (should-not response))
        (anaconda-mode-stop)))))

(ert-deftest test-anaconda-mode-jsonrpc-skip-response-on-buffer-switch ()
  "Don't run response callback if user switch the buffer."
  (let (response)
    (with-current-buffer (fixture "import s" 1 8)
      (unwind-protect
          (progn
            (anaconda-mode-start)
            (wait)
            (anaconda-mode-jsonrpc "complete" (lambda (resp) (setq response resp)))
            (switch-to-buffer "*scratch*")
            ;; Avoid false positive test pass in the case point were
            ;; set to different places in different buffers.
            (erase-buffer)
            (insert "import s")
            (sleep-for 1)
            (should-not response))
        (anaconda-mode-stop)))))

(ert-deftest test-anaconda-mode-jsonrpc-skip-response-on-window-switch ()
  "Don't run response callback if user switch the window."
  (let (response)
    (with-current-buffer (fixture "import s" 1 8)
      (unwind-protect
          (progn
            (anaconda-mode-start)
            (wait)
            (anaconda-mode-jsonrpc "complete" (lambda (resp) (setq response resp)))
            (switch-to-buffer-other-window (current-buffer))
            (sleep-for 1)
            (should-not response))
        (anaconda-mode-stop)))))

(ert-deftest test-anaconda-mode-jsonrpc-skip-response-on-modified-tick-change ()
  "Don't run response callback if the `buffer-chars-modified-tick' was changed."
  (let (response)
    (with-current-buffer (fixture "import s" 1 8)
      (unwind-protect
          (progn
            (anaconda-mode-start)
            (wait)
            (anaconda-mode-jsonrpc "complete" (lambda (resp) (setq response resp)))
            (just-one-space)
            (backward-delete-char 1)
            (sleep-for 1)
            (should-not response))
        (anaconda-mode-stop)))))

(ert-deftest test-anaconda-mode-jsonrpc-request ()
  "Prepare JSON encoded data for procedure call."
  (with-current-buffer (fixture "import sys" 1 10)
    (equal (anaconda-mode-jsonrpc-request "echo")
           "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"echo\",\"params\":{\"source\":\"import sys\",\"line\":1,\"column\":10,\"path\":null}}")))

(ert-deftest test-anaconda-mode-jsonrpc-request-data ()
  "Prepare data for remote procedure call."
  (with-current-buffer (fixture "import datetime" 1 15 "simple.py")
    (should (equal (anaconda-mode-jsonrpc-request-data "echo")
                   `((jsonrpc . "2.0")
                     (id . 1)
                     (method . "echo")
                     (params . ((source . "import datetime")
                                (line . 1)
                                (column . 15)
                                (path . ,(f-full "simple.py")))))))))

(ert-deftest test-anaconda-mode-jsonrpc-request-data-with-tabulation ()
  "Prepare data for remote procedure call from buffer contained
tabulation characters."
  (with-current-buffer (fixture "
if True:
	sys.api" 3 8 "simple.py")
    (should (equal (anaconda-mode-jsonrpc-request-data "echo")
                   `((jsonrpc . "2.0")
                     (id . 1)
                     (method . "echo")
                     (params . ((source . "
if True:
	sys.api")
                                (line . 3)
                                (column . 8)
                                (path . ,(f-full "simple.py")))))))))

;;; Completion.

(ert-deftest test-anaconda-mode-complete-extract-names ()
  "Extract names from complete response."
  (let ((response '((result ((module-path . "/vagrant/simple.py")
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
                             (description . "function: simple.test2")))
                    (jsonrpc . "2.0")
                    (id . 1))))
    (should (equal '("test1" "test2")
                   (anaconda-mode-complete-extract-names response)))))

(ert-deftest test-anaconda-mode-complete-callback ()
  "Completion function must insert common candidates base."
  (let ((response '((result ((module-path . "/vagrant/simple.py")
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
                             (description . "function: simple.test2")))
                    (jsonrpc . "2.0")
                    (id . 1))))
    (with-current-buffer (fixture "t" 1 1)
      (anaconda-mode-complete-callback response)
      (should (looking-back "test")))))

(ert-deftest test-anaconda-mode-complete-callback-completions-buffer ()
  "Completion must show *Completions* buffer if candidates doesn't have same base."
  (unwind-protect
      (let ((response '((id . 1)
                        (result ((full-name . "bool")
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
                                 (type . "keyword")))
                        (jsonrpc . "2.0"))))
        (with-current-buffer (fixture "Tr" 1 2)
          (anaconda-mode-complete-callback response)
          (should (get-buffer "*Completions*"))))
    (kill-buffer "*Completions*")))

(ert-deftest test-anaconda-mode-complete ()
  "Test completion at point."
  (unwind-protect
      (with-current-buffer (fixture "t" 1 1)
        (anaconda-mode-complete)
        (wait)
        (sleep-for 1)
        (should (get-buffer "*Completions*")))
    (anaconda-mode-stop)
    (kill-buffer "*Completions*")))

(ert-deftest test-anaconda-mode-complete-insert-candidates-base ()
  "Completion must insert common candidates base."
  (unwind-protect
      (with-current-buffer (fixture "
def teardown(): pass
tear" 3 4)
        (anaconda-mode-complete)
        (wait)
        (sleep-for 1)
        (should (looking-back "teardown")))
    (anaconda-mode-stop)))

(ert-deftest test-anaconda-mode-does-not-complete-in-comments ()
  "Don't run interactive completion inside comment block."
  (with-current-buffer (fixture "#im" 1 3)
    (anaconda-mode-complete)
    (should-not (anaconda-mode-running-p))))

;;; Documentation.

(ert-deftest test-anaconda-mode-view-doc ()
  "Show documentation buffer on documentation lookup."
  (unwind-protect
      (with-current-buffer (fixture "
def f(a, b=1):
    '''Docstring for f.'''
    pass" 2 5 "simple.py")
        (anaconda-mode-view-doc)
        (wait)
        (sleep-for 1)
        (should (equal "*anaconda-doc*"
                       (buffer-name (window-buffer (selected-window))))))
    (anaconda-mode-stop)))

(ert-deftest test-anaconda-mode-view-doc-callback ()
  "Fill documentation buffer content from response."
  (let ((response '((result ((type . "function")
                             (docstring . "f(a, b)

I'm documentation string.")
                             (name . "f")
                             (module-name . "simple")
                             (column . 4)
                             (module-path . "/vagrant/simple.py")
                             (full-name . "simple.f")
                             (description . "def f")
                             (line . 1)))
                    (jsonrpc . "2.0")
                    (id . 1))))
    (anaconda-mode-view-doc-callback response)
    (should (equal "simple.f
f(a, b)

I'm documentation string.
" (buffer-string)))))

(ert-deftest test-anaconda-mode-view-doc-format-multiple-modules ()
  "Format doc buffer for multiple modules."
  (let ((response '((id . 1)
                    (result ((description . "def join")
                             (full-name . "os.path.join")
                             (type . "function")
                             (docstring . "join(path, *paths)

")
                             (module-path . "/home/vagrant/.pyenv/versions/3.4.3/lib/python3.4/ntpath.py")
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
                             (module-path . "/home/vagrant/.pyenv/versions/3.4.3/lib/python3.4/posixpath.py")
                             (column . 4)
                             (line . 70)
                             (name . "join")
                             (module-name . "posixpath")))
                    (jsonrpc . "2.0"))))
    (should (equal "ntpath.join
join(path, *paths)

posixpath.join
join(a, *p)

Join two or more pathname components, inserting '/' as needed.
If any component is an absolute path, all previous path components
will be discarded.  An empty last part will result in a path that
ends with a separator.
"
                   (anaconda-mode-format-view-doc-content response)))))

(ert-deftest test-anaconda-mode-view-doc-extract-definition-bold-module-name ()
  "Extract module definition will use bold font for module name."
  (let* ((definition '((description . "def join")
                       (full-name . "os.path.join")
                       (type . "function")
                       (docstring . "join(path, *paths)

")
                       (module-path . "/home/vagrant/.pyenv/versions/3.4.3/lib/python3.4/ntpath.py")
                       (column . 4)
                       (line . 104)
                       (name . "join")
                       (module-name . "ntpath")))
         (module (anaconda-mode-view-doc-extract-definition definition))
         (module-name (car module)))
    (should (equal 'bold (get-text-property 0 'face module-name)))))

;;; ElDoc.

(ert-deftest test-anaconda-mode-eldoc ()
  "`anaconda-mode-eldoc-function' will run `anaconda-mode' server."
  (let (eldoc-last-message)
    (unwind-protect
        (with-current-buffer (fixture "
def test(one, other):
    '''Test if one is other'''
    return one is other

test(" 6 5 "simple.py")
          (anaconda-mode-eldoc-function)
          (wait)
          (sleep-for 1)
          (should (equal "test(one, other)" eldoc-last-message)))
      (anaconda-mode-stop))))

(ert-deftest test-anaconda-mode-eldoc-callback ()
  "Format eldoc string from response."
  (let ((response '((id . 1)
                    (result (params "one" "other")
                            (name . "test")
                            (index . 0))
                    (jsonrpc . "2.0"))))
    (should (equal "test(one, other)"
                   (anaconda-mode-eldoc-callback response)))))

(ert-deftest test-anaconda-mode-eldoc-empty-response ()
  "Don't try to show eldoc on response with empty result."
  (let (eldoc-last-message)
    (unwind-protect
        (with-current-buffer (fixture "invalid(" 1 8 "simple.py")
          (anaconda-mode-eldoc-function)
          (wait)
          (sleep-for 1)
          (should-not eldoc-last-message))
      (anaconda-mode-stop))))

(ert-deftest test-anaconda-mode-eldoc-no-params ()
  "Show eldoc message for function without arguments."
  (let (eldoc-last-message)
    (unwind-protect
        (with-current-buffer (fixture "
def test(): pass
test(" 3 5 "simple.py")
          (anaconda-mode-eldoc-function)
          (wait)
          (sleep-for 1)
          (should (equal "test()" eldoc-last-message)))
      (anaconda-mode-stop))))

(ert-deftest test-anaconda-mode-eldoc-format-as-single-line ()
  "Format eldoc string as single line."
  (let ((anaconda-mode-eldoc-as-single-line t)
        (response `((id . 1)
                    (result (params ,@(--iterate it "a" 1000))
                            (name . "test")
                            (index . 0))
                    (jsonrpc . "2.0"))))
    (should (equal (frame-width)
                   (length (anaconda-mode-eldoc-format response))))))

;;; Minor mode.

(ert-deftest test-anaconda-mode-enable ()
  "Enable `anaconda-mode'."
  (with-temp-buffer
    (anaconda-mode 1)
    (should (eq eldoc-documentation-function 'anaconda-mode-eldoc-function))))

(ert-deftest test-anaconda-mode-disable ()
  "Disable `anaconda-mode'."
  (with-temp-buffer
    (anaconda-mode 1)
    (anaconda-mode -1)
    (should-not (eq eldoc-documentation-function 'anaconda-mode-eldoc-function))))

(provide 'anaconda-mode-test)

;;; anaconda-mode-test.el ends here
