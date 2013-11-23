# company-jedi [![Build Status](https://travis-ci.org/proofit404/company-jedi.png?branch=master)](https://travis-ci.org/proofit404/company-jedi)

[Jedi](https://github.com/davidhalter/jedi) backend for [company-mode](https://github.com/company-mode/company-mode).

![screenshot](screenshots/snapshot1.png)

## Requirements

* emacs 24.3
* python3
* virtualenv

## Features

* Context-sensitive code completion for Python

## Future work

* jump to definition
* find references
* view documentation
* asynchronous task processing
* python2 support

## Install

Setting up `company-jedi` isn't as simple as with Elisp-only packages, due to Python dependencies.
First of all you need to install the package from [Melpa](http://melpa.milkbox.net/).

    M-x package-install RET company-jedi RET

Now you need resolve Jedi dependencies with Python's [PyPI](https://pypi.python.org/pypi).
By default you need virtualenv installed before you get company-jedi frontend to work.

    cd $HOME/.emacs.d/elpa/company-jedi-*
    make

But using `company-jedi` without virtualenv is still possible with additional setup.
For now you need to run the following command from the package directory:

    pip install -r requirements.txt

For now `company-jedi` runs on Python 3 only. But Jedi work with any Python sources. So you'll need to
install the Python versions corresponding to your projects in your virtual environment.

You must perform the above actions after each `company-jedi` package update because it appears in a new location each time.

## Usage

By default `company-jedi` starts its HTTP server on port `24970` for interaction with Python process.
You may want to close this port for incoming network connections.

To automatically start the server when you open any python-mode buffer, add the following code to your emacs configuration:

```lisp
(add-to-list 'company-backends 'company-jedi)
(add-hook 'python-mode-hook 'company-jedi-start)
```

If you've installed Python dependencies without virtualenv, you need to customize the command that starts the server:

```lisp
(setq company-jedi-command (format "python3 -m start_jedi -p %s" company-jedi-port))
```

## Contributions

Are very welcome. But any significant change has to be accompanied with tests, both for Emacs Lisp and Python code.
To run the test suite, call:

    make check
