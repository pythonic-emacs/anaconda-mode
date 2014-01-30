# company-jedi [![Build Status](https://travis-ci.org/proofit404/company-jedi.png?branch=master)](https://travis-ci.org/proofit404/company-jedi) [![Dependency Status](https://gemnasium.com/proofit404/company-jedi.png)](https://gemnasium.com/proofit404/company-jedi)

[Jedi](https://github.com/davidhalter/jedi) backend for [company-mode](https://github.com/company-mode/company-mode).

![screenshot1](screenshots/snapshot1.png)
![screenshot2](screenshots/snapshot2.png)
![screenshot3](screenshots/snapshot3.png)
![screenshot4](screenshots/snapshot4.png)

## Requirements

* emacs 24.3
* python3
* virtualenv

## Features

* context-sensitive code completion for Python
* jump to definition
* find references
* view documentation
* eldoc

## Future work

#### 0.1 milestone:

* 2.6, 2.7, 3.2, 3.3 pythons support

#### 0.2 milestone:

* virtualenv support

#### 0.3 milestone:

* non blocking client

#### 0.4 milestone:

* concurrent server side

## Install

Setting up `company-jedi` isn't as simple as with Elisp-only packages, due to Python dependencies.
First of all you need to install the package from [Melpa](http://melpa.milkbox.net/).

    M-x package-install RET company-jedi RET

Now you need resolve Jedi dependencies with Python's [PyPI](https://pypi.python.org/pypi).
By default you need virtualenv installed before you get company-jedi frontend to work.

    M-x company-jedi-install

But using `company-jedi` without virtualenv is still possible with additional setup.
For now you need to run the following command from the package directory:

    pip install -r requirements.txt

Jedi works with any Python sources, but `company-jedi` currently runs on Python 3 only.
So you'll need to install that version in your virtual environment.

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
(setq company-jedi-python-bin "python3")
```

## Contributions

Are very welcome. But any significant change has to be accompanied with tests, both for Emacs Lisp and Python code.
To run the test suite, call:

    make check
