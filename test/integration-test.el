;;; integration-test.el --- integration test suite

;;; Commentary:

;;; Code:


;;; Server.

(ert-integration test-anaconda-mode-start ()
  "`anaconda-mode' server starts successfully."
  (anaconda-mode-start)
  (wait)
  (should (anaconda-mode-running-p)))

(ert-integration test-anaconda-mode-start-bind-port ()
  "`anaconda-mode' server bind port successfully."
  (anaconda-mode-start)
  (wait)
  (should (anaconda-mode-bound-p)))

(ert-integration test-anaconda-mode-stop ()
  "`anaconda-mode' server stops successfully."
  (anaconda-mode-start)
  (wait)
  (anaconda-mode-stop)
  (should-not (anaconda-mode-running-p)))

(ert-integration test-anaconda-mode-stop-release-port ()
  "`anaconda-mode' server release port successfully on teardown."
  (anaconda-mode-start)
  (wait)
  (anaconda-mode-stop)
  (should-not (anaconda-mode-bound-p)))

(ert-integration test-anaconda-mode-create-server-directory ()
  "`anaconda-mode-ensure-directory-code' must create
`anaconda-mode-server-directory'."
  (run anaconda-mode-ensure-directory-command
       (anaconda-mode-server-directory))
  (should (zerop (run "
import sys, os
if not os.path.isdir(os.path.expanduser(sys.argv[-1])):
    os._exit(1)  # IPython again.
" (anaconda-mode-server-directory)))))

(ert-integration test-anaconda-mode-install-server ()
  "`anaconda-mode-install-server-code' must install `anaconda-mode' server."
  (run anaconda-mode-ensure-directory-command
       (anaconda-mode-server-directory))
  (run anaconda-mode-install-server-command
       (anaconda-mode-server-directory)
       anaconda-mode-server-version)
  (should (zerop (run anaconda-mode-check-installation-command
                      (anaconda-mode-server-directory)))))

(ert-integration test-anaconda-mode-restart-on-environment-change ()
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

(ert-integration test-anaconda-mode-restart-on-installation-directory-change ()
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

(ert-integration test-anaconda-mode-not-restart-in-the-same-envinment ()
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

(ert-integration test-anaconda-mode-on-start-callback ()
  "Run callback passed on server start."
  (let (var)
    (anaconda-mode-start (lambda () (setq var t)))
    (wait)
    (should var)))

(ert-integration test-anaconda-mode-after-start-callback ()
  "Run callback passed after server start."
  (let (var)
    (anaconda-mode-start)
    (wait)
    (anaconda-mode-start (lambda () (setq var t)))
    (should var)))


;;; JSONRPC implementation.

(ert-integration test-anaconda-mode-call ()
  "Perform remote procedure call without already started server.
We make request knowingly so response shouldn't be null."
  (let (result)
    (with-current-buffer (fixture "import sys" 1 10)
      (anaconda-mode-call "complete" (lambda (res) (setq result res)))
      (wait)
      (sleep-for 1)
      (should (< 0 (length result))))))

(ert-integration test-anaconda-mode-call-callback-current-buffer ()
  "Run response callback in the request buffer."
  (with-current-buffer (fixture "import sys" 1 10)
    (let ((request-buffer (current-buffer))
          response-buffer)
      (anaconda-mode-call "complete" (lambda (res) (setq response-buffer (current-buffer))))
      (wait)
      (sleep-for 1)
      (should (equal request-buffer response-buffer)))))

(ert-integration test-anaconda-mode-jsonrpc ()
  "Perform remote procedure call.
We make request knowingly so response shouldn't be null."
  (let (result)
    (with-current-buffer (fixture "import sys" 1 10)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc "complete" (lambda (res) (setq result res)))
      (sleep-for 1)
      (should (< 0 (length result))))))

(ert-integration test-anaconda-mode-jsonrpc-error-response ()
  "Raise error if rpc call return error response."
  (with-current-buffer (fixture "import sys" 1 10)
    (anaconda-mode-start)
    (wait)
    (anaconda-mode-jsonrpc "wrong_method" (lambda (res)))
    (should-error (sleep-for 1))))

(ert-integration test-anaconda-mode-jsonrpc-remove-http-buffer ()
  "Remove *http* buffer leaved after `url-retrieve' function call."
  (with-current-buffer (fixture "import sys" 1 10)
    (anaconda-mode-start)
    (wait)
    (anaconda-mode-jsonrpc "complete" (lambda (res)))
    (sleep-for 1)
    (should-not
     (--filter (s-starts-with? " *http" (buffer-name it))
               (buffer-list)))))

(ert-integration test-anaconda-mode-jsonrpc-remove-http-buffer-on-callback-error ()
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

(ert-integration test-anaconda-mode-jsonrpc-skip-response-on-point-movement ()
  "Don't run response callback if point position was changed."
  (let (result)
    (with-current-buffer (fixture "import s " 1 8)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc "complete" (lambda (res) (setq result res)))
      (forward-char)
      (sleep-for 1)
      (should-not result))))

(ert-integration test-anaconda-mode-jsonrpc-skip-response-on-buffer-switch ()
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

(ert-integration test-anaconda-mode-jsonrpc-skip-response-on-window-switch ()
  "Don't run response callback if user switch the window."
  (let (result)
    (with-current-buffer (fixture "import s" 1 8)
      (anaconda-mode-start)
      (wait)
      (anaconda-mode-jsonrpc "complete" (lambda (res) (setq result res)))
      (switch-to-buffer-other-window (current-buffer))
      (sleep-for 1)
      (should-not result))))

(ert-integration test-anaconda-mode-jsonrpc-skip-response-on-modified-tick-change ()
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

(ert-integration test-anaconda-mode-jsonrpc-add-path-prefix ()
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


;;; Completion.

(ert-integration test-anaconda-mode-complete ()
  "Test completion at point."
  (with-current-buffer (fixture "t" 1 1)
    (anaconda-mode-complete)
    (wait)
    (sleep-for 1)
    (should (get-buffer "*Completions*"))))

(ert-integration test-anaconda-mode-complete-unicode ()
  "Test completion at point works fine with unicode."
  (with-current-buffer (fixture "'你好 世界!'." 1 9)
    (anaconda-mode-complete)
    (wait)
    (sleep-for 1)
    (should (get-buffer "*Completions*"))))

(ert-integration test-anaconda-mode-complete-insert-candidates-base ()
  "Completion must insert common candidates base."
  (with-current-buffer (fixture "
def teardown(): pass
tear" 3 4)
    (anaconda-mode-complete)
    (wait)
    (sleep-for 1)
    (should (looking-back "teardown"))))

(ert-integration test-anaconda-mode-does-not-complete-in-comments ()
  "Don't run interactive completion inside comment block."
  (with-current-buffer (fixture "#im" 1 3)
    (anaconda-mode-complete)
    (should-not (anaconda-mode-running-p))))


;;; Documentation.

(ert-integration test-anaconda-mode-show-doc ()
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

(ert-integration test-anaconda-mode-show-doc-not-found ()
  "Don't show documentation buffer in the case of missing docs."
  (with-current-buffer (fixture "" 1 0 (concat home-directory "simple.py"))
    (anaconda-mode-show-doc)
    (wait)
    (sleep-for 1)
    (should (equal "simple.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))

(ert-integration test-anaconda-mode-show-doc-content ()
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

(ert-integration test-anaconda-mode-eldoc ()
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

(ert-integration test-anaconda-mode-eldoc-empty-response ()
  "Don't try to show eldoc on response with empty result."
  (let (eldoc-last-message)
    (with-current-buffer (fixture "invalid(" 1 8 (concat home-directory "simple.py"))
      (anaconda-mode-eldoc-function)
      (wait)
      (sleep-for 1)
      (should-not eldoc-last-message))))

(ert-integration test-anaconda-mode-eldoc-no-params ()
  "Show eldoc message for function without arguments."
  (let (eldoc-last-message)
    (with-current-buffer (fixture "
def test(): pass
test(" 3 5 (concat home-directory "simple.py"))
      (anaconda-mode-eldoc-function)
      (wait)
      (sleep-for 1)
      (should (equal "test()" eldoc-last-message)))))

(ert-integration test-anaconda-mode-eldoc-no-index-on-set-spec ()
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

(ert-integration test-anaconda-mode-eldoc-sigrature ()
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


;;; Definitions.

(ert-integration test-anaconda-mode-find-definitions ()
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

(ert-integration test-anaconda-mode-find-definitions-not-found ()
  "Don't show definitions buffer in the case of missing definitions."
  (with-current-buffer (fixture "" 1 0 (concat home-directory "simple.py"))
    (anaconda-mode-find-definitions)
    (wait)
    (sleep-for 1)
    (should (equal "simple.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))

(ert-integration test-anaconda-mode-find-definitions-single-definition ()
  "Jump to definition immediately in the case of single definition."
  (with-current-buffer (fixture "from os import getenv" 1 21)
    (anaconda-mode-find-definitions)
    (wait)
    (sleep-for 1)
    (should (equal "os.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))


;;; Assignments.

(ert-integration test-anaconda-mode-find-assignments ()
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

(ert-integration test-anaconda-mode-find-assignments-not-found ()
  "Don't show assignments buffer in the case of missing assignments."
  (with-current-buffer (fixture "" 1 0 (concat home-directory "simple.py"))
    (anaconda-mode-find-assignments)
    (wait)
    (sleep-for 1)
    (should (equal "simple.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))

(ert-integration test-anaconda-mode-find-assignments-single-assignment ()
  "Jump to assignment immediately in the case of single assignment."
  (with-current-buffer (fixture "from os import getenv" 1 21)
    (anaconda-mode-find-assignments)
    (wait)
    (sleep-for 1)
    (should (equal "os.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))


;;; References.

(ert-integration test-anaconda-mode-find-references ()
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

(ert-integration test-anaconda-mode-find-references-not-found ()
  "Don't show references buffer in the case of missing references."
  (with-current-buffer (fixture "" 1 0 (concat home-directory "simple.py"))
    (anaconda-mode-find-references)
    (wait)
    (sleep-for 1)
    (should (equal "simple.py"
                   (f-filename (buffer-file-name (window-buffer (selected-window))))))))

(provide 'integration-test)

;;; integration-test.el ends here
