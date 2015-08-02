;;; anaconda-mode-test.el --- anaconda-mode test suite

;;; Commentary:

;;; Todo:

;; * test `anaconda-mode-host' function
;; * test server start chain behavior
;; * test server start doesn't reinstall server

;;; Code:

(require 'ert)
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

(defun fixture (source line column path)
  "Open SOURCE fixture.
Put point on LINE at COLUMN position.  Set PATH as current file
name."
  (with-current-buffer (generate-new-buffer "*fixture*")
    (insert source)
    (goto-char 0)
    (forward-line (1- line))
    (forward-char column)
    (set-visited-file-name path)
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

(ert-deftest test-anaconda-mode-jsonrpc ()
  "Perform remote procedure call.")

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

;;; Completion.

(ert-deftest test-anaconda-mode-complete ()
  "Test completion at point."
  (load-fixture "simple.py" "\
def test1(a, b):
    '''First test function.'''
    pass

def test2(c):
    '''Second test function.'''
    pass
test_|_")
  (should (equal (anaconda-mode-complete-thing)
                 '("test1" "test2"))))

(ert-deftest test-anaconda-mode-complete-with-tabulation ()
  "Test completion at point with tab indentation."
  (load-fixture "simple.py" "\
import sys
if True:
	sys.api_|_")
  (should (equal (anaconda-mode-complete-thing)
                 '("api_version"))))

(ert-deftest test-anaconda-mode-complete-in-comments ()
  "Completion in comments must be nil."
  (load-fixture "simple.py" "#imp_|_")
  (should (null (anaconda-mode-complete-thing))))

;;; Documentation.

(ert-deftest test-anaconda-mode-doc ()
  "Test documentation string search."
  (load-fixture "simple.py" "\
def f_|_(a, b=1):
    '''Docstring for f.'''
    pass")
  (anaconda-mode-view-doc)
  (should (equal (with-current-buffer (get-buffer "*anaconda-doc*")
                   (buffer-string))
                 "\
simple - def f
========================================
f(a, b = 1)

Docstring for f.")))

(ert-deftest test-anaconda-mode-doc-window-focus ()
  "Test documentation window focus."
  (load-fixture "simple.py" "from os import path_|_")
  (anaconda-mode-view-doc)
  (should (equal "*anaconda-doc*"
                 (buffer-name))))

;;; ElDoc.

(ert-deftest test-anaconda-eldoc-existing ()
  (load-fixture "simple.py" "\
def fn(a, b):
    pass
fn(_|_")
  (should (equal (anaconda-eldoc-function)
                 "fn(a, b)")))

(ert-deftest test-anaconda-eldoc-invalid ()
  (load-fixture "simple.py" "invalid(_|_")
  (should-not (anaconda-eldoc-function)))

(ert-deftest test-anaconda-eldoc-ignore-errors ()
  (let ((anaconda-mode-directory (f-root))
        (anaconda-mode-port nil))
    (should-not (anaconda-eldoc-function))))

;;; Minor mode.

(ert-deftest test-anaconda-mode-enable ()
  (with-temp-buffer
    (anaconda-mode 1)
    (should (eq (car completion-at-point-functions)
                'anaconda-mode-complete-at-point))
    (should (eq eldoc-documentation-function 'anaconda-eldoc-function))))

(ert-deftest test-anaconda-mode-disable ()
  (with-temp-buffer
    (anaconda-mode 1)
    (anaconda-mode -1)
    (should-not (eq (car completion-at-point-functions)
                    'anaconda-mode-complete-at-point))
    (should-not (eq eldoc-documentation-function 'anaconda-eldoc-function))))

(provide 'anaconda-mode-test)

;;; anaconda-mode-test.el ends here
