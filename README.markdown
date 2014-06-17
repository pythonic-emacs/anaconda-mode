# anaconda-mode [![Build Status](https://travis-ci.org/anaconda-mode/anaconda-mode.png?branch=master)](https://travis-ci.org/anaconda-mode/anaconda-mode) [![Dependency Status](https://gemnasium.com/anaconda-mode/anaconda-mode.png)](https://gemnasium.com/anaconda-mode/anaconda-mode) [![Coverage Status](https://coveralls.io/repos/anaconda-mode/anaconda-mode/badge.png)](https://coveralls.io/r/anaconda-mode/anaconda-mode)

Code navigation, documentation lookup and completion for Python.

## Requirements

* emacs 24.3
* python 2.6, 2.7, 3.3 or 3.4

## Features

* context-sensitive code completion for Python
* jump to definition
* find references
* view documentation
* virtualenv

## Install

All you need is install the package from [Melpa](http://melpa.milkbox.net/).

    M-x package-install RET anaconda-mode RET

## Usage

By default `anaconda-mode` starts its HTTP server on port `24970` for
interaction with Python process.  You may want to close this port for
incoming network connections.  This server will start automatically
when you call any anaconda command like reference search or
documentation lookup.  To start `anaconda-mode` automatically in all
python buffers add following to your configuration.

```lisp
(add-hook 'python-mode-hook 'anaconda-mode)
```

Anaconda mode detect active virtual environment through value of
`python-shell-virtualenv-path` variable defined in `python.el`
library.  When you set it to actual virtualenv path next anaconda-mode
command you call will restart its python process in proper environment
before performing this call.  This allow anaconda processing
virtualenv site-packages with minimum number of actions from your
side.  I strongly recommended you to use package like
[pyenv-mode](https://github.com/proofit404/pyenv-mode) or similar
package to hold `python-shell-virtualenv-path` in actual state.

#### Interactive commands

Here ara interactive command available with anaconda-mode

Keybinding  | Description
------------|--------------------------------------------
M-.         | anaconda-mode-goto-definitions
M-,         | anaconda-mode-goto-assignments
M-*         | anaconda-nav-pop-marker
M-?         | anaconda-mode-view-doc
M-r         | anaconda-mode-usages

If goto definitions, assignments or usages cause multiple candidates
you'll see advanced anaconda navigator buffer.

![navigator](static/navigator.png)

## Contribs

Anaconda doesn't has internal contrib interface yet.  But any way you
can use additionally packages with manual setup.  To turn on any of
its add corresponding code snippet to your configuration.

#### anaconda-eldoc

ElDoc documentation lookup with `anaconda-mode`.

```lisp
(add-hook 'python-mode-hook 'anaconda-eldoc)
```

## Known Issues

If you're using proxy server, you have to make sure that the proxy is
not used for communicating with anaconda-mode:

```shell
export no_proxy="localhost,127.0.0.1"
```

## Contributions

Are very welcome.  But any significant change has to be accompanied
with tests, both for Emacs Lisp and Python code.  To run the test
suite, call:

    tox

## Thanks

* Dmitry Gutov **@dgutov**
* Bo Lin **@sadboy**
* Vasilij Schneidermann **@wasamasa**
* Fredrik Bergroth **@fbergroth**
