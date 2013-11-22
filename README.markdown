# company-jedi [![Build Status](https://travis-ci.org/proofit404/company-jedi.png?branch=master)](https://travis-ci.org/proofit404/company-jedi)

[Jedi](https://github.com/davidhalter/jedi) backend for [company-mode](https://github.com/company-mode/company-mode).

![screenshot](screenshots/snapshot1.png)

**Warning:** poor english below, sorry for this...

## Requirements

* emacs 24.3
* python3
* virtualenv

## Features

* In buffer auto-completion for python

## Future work

* jump to definition
* find references
* view documentation
* asynchronous tasks processing
* python2 support

## Install

Set up company-jedi package isn't such trivial as elisp only packages due to python dependencies.
First of all you need obtain package sources with [Melpa](http://melpa.milkbox.net/).

    M-x package-install RET company-jedi RET

Now you need resolve jedi dependencies with python's [PyPI](https://pypi.python.org/pypi).
By default you need virtualenv installed before you get company-jedi frontend to work.

    cd $HOME/.emacs.d/elpa/company-jedi-*
    make

But use company-jedi without virtualenv is still possible with extra settings required. For now you need to run
following command from the package directly

    pip install -r requirements.txt

For now company-jedi run on the python3 only. But jedi work with any python sources. So you need install properly
python version for you virtual environment.

You must perform actions above on each company-jedi package update because it appears in the new location each time.

## Usage

By default company-jedi start HTTP server on `24970` port for emacs-python interaction. You may want to close
it for incoming network connection. It dosen't read any data on your pc except parsing python sources.

For automatically start server when you open any python buffer add following code to your emacs configuration

```lisp
(add-to-list 'company-backends 'company-jedi)
(add-hook 'python-mode-hook 'company-jedi-start)
```

If you install python dependencies with pip's method so you need some additional customizations. Mainly for server
starting command

```lisp
(setq company-jedi-command (format "python3 -m start_jedi -p %s" company-jedi-port))
```

## Contribution

Much welcome. But you must write all necessary tests for any significant change.
Both for emacs lisp and python code.

    make check
