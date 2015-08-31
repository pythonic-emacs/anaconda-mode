;;; do_goto.el --- run goto rpc call

;;; Commentary:

;;; Code:

(find-file "simple.py")

(insert "from os.path import join")

(anaconda-mode-call "goto_definitions"
                    (lambda (res)
                      (anaconda-mode-view res 'anaconda-mode-view-definitions-presenter)))
