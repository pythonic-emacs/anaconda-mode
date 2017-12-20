# <img align="right" src="static/logo.png"> Anaconda mode [![MELPA](https://melpa.org/packages/anaconda-mode-badge.svg)](https://melpa.org/#/anaconda-mode) [![MELPA Stable](https://stable.melpa.org/packages/anaconda-mode-badge.svg)](https://stable.melpa.org/#/anaconda-mode)

Code navigation, documentation lookup and completion for Python.

![Completion screenshot](static/completion.png)

![Reference search screenshot](static/reference.png)

## Features

Anaconda mode provides the following features

* context-sensitive code completion
* jump to definitions
* find references
* view documentation
* virtual environment
* eldoc mode
* all this stuff inside vagrant, docker and remote hosts

## Supported Python Versions

2.6, 2.7, 3.3, 3.4, 3.5, 3.6

## Installation

To use this package you need to install `setuptools`.

#### package.el

All you need to do is install the package from
[Melpa](https://melpa.org/)

    M-x package-install RET anaconda-mode RET

#### Manual

Clone this repository somewhere and add this directory to your
`load-path`.

#### Prelude

`anaconda-mode` is included in the [Emacs
Prelude](https://github.com/bbatsov/prelude) distribution.  You can
use it as well.  Look at the `prelude-python` module to see more
details.

#### Spacemacs

`anaconda-mode` is included in the
[Spacemacs](https://github.com/syl20bnr/spacemacs) distribution.  You
can use it as well.  Look at the `python` language layer to see more
details.

## Configuration

You can automatically enable `anaconda-mode` in all python buffers
with following code in your configuration:

```lisp
    (add-hook 'python-mode-hook 'anaconda-mode)
```

#### ElDoc

`anaconda-eldoc-mode` provide document function to `eldoc-mode`.  All
you need is to enable `anaconda-eldoc-mode` in addition to the
previous setup.

```lisp
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
```

## Usage

To start a completion press `C-M-i`.  This is the standard emacs
binding for `complete-at-point` function.  You can use
[company-mode](http://company-mode.github.io/) with
[company-anaconda](https://github.com/proofit404/company-anaconda)
backend to get more intelligent ui.  Or
[auto-complete-mode](https://github.com/auto-complete/auto-complete)
with [ac-anaconda](https://github.com/proofit404/ac-anaconda) as a
last try.

#### Interactive commands

Here is a list of interactive commands available with anaconda-mode

|Keybinding | Description                    |
|-----------|--------------------------------|
|C-M-i      | anaconda-mode-complete         |
|M-.        | anaconda-mode-find-definitions |
|M-,        | anaconda-mode-find-assignments |
|M-r        | anaconda-mode-find-references  |
|M-*        | anaconda-mode-go-back          |
|M-?        | anaconda-mode-show-doc         |

If multiple candidates are found for definitions, assignments or
usages, you'll see an advanced anaconda navigator buffer.

#### PYTHONPATH

You can add your project to the Emacs `PYTHONPATH`.  If you store
project dependencies somewhere on your machine, you can add them as
well.

```lisp
    (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/project")
    (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/dependency")
```

#### Virtual environment

Use a virtual environment to isolate your project dependencies from
others on the system.  You can additionally install your project in an
editable mode in the virtual environment.  This will improve search
functionality.  Then activate this virtual environment inside Emacs.

    M-x pythonic-activate RET /path/to/virtualenv RET

Also you can use
[pyenv-mode](https://github.com/proofit404/pyenv-mode) or similar
package to hold virtual environment in actual state.

Each action above applies to `anaconda-mode` immediately.  The next
`anaconda-mode` command you call will use this environment for search
completion candidates.

#### Tramp

It's possible to use anaconda-mode on a remote server when you connect
to it using tramp.  Anaconda-mode can search for completion candidates
and all other stuff on remote server while you're running Emacs
locally.  First of all open interesting remote file.

    C-x C-f /ssh:remote_host:project/__init__.py RET

After tramp successfully connects and you see actual buffer content,
activate the remote virtual environment.

    M-x pythoninc-activate RET /ssh:remote_host:/home/user/venv RET

Now any anaconda-mode command will use `/home/user/venv/bin/python`
interpreter running on `remote_host` over ssh.  If you don't use the
virtual environment remotely then you have an option to specify the
remote interpreter directly.

```lisp
    (setq python-shell-interpreter "/ssh:remote_host:/usr/bin/python")
```

It is important to remember that `remote_host` must be a real host
name or an IP address.  SSH aliases not allowed to be used with
anaconda-mode.  All kinds of searching from inside the virtual
environment are available from any buffer.  However searching inside
your project is available only if you open it on the same machine as
the interpreter.

#### Vagrant

You can get all the intelligent features of anaconda-mode with virtual
environments deployed on your vagrant box. Fire up the vagrant machine
as usual.  The easiest way to handle authentication is to copy your
public ssh key to the vagrant box.

    ssh-copy-id vagrant@localhost -p 2222

Now open your project inside the vagrant box.

    C-x C-f /ssh:vagrant@localhost#2222:/vagrant/polls/views.py

Check the `*anaconda-mode*` buffer for the port number, and forward
that port from vagrant.

    ssh -nNT vagrant@localhost -p 2222 -L <port number>:localhost:<port number>

Then activate your project environment installed inside vagrant.

    M-x pythonic-activate RET /ssh:vagrant@localhost#2222:/vagrant/polls/venv RET

Now you are ready to go. If you have random connection errors during
interaction with running server, try replacing the host name with the
IP address, for example, `localhost` with `127.0.0.1`.

#### Docker

You can use definition, reference and auto-completion search for
dependencies installed in the Docker container.  Let suppose your
project layout is the same as in [Compose and
Django](https://docs.docker.com/compose/django/) tutorial.  To use
`anaconda-mode` together with Docker you need to install two
additional external dependencies.  First of all, you need to install
[docker-tramp](https://github.com/emacs-pe/docker-tramp.el) Emacs
package.  The second step is to install Unix `socat` utility.  You
should have your containers up and running

    docker-compose up web

After that, you can set Python interpreter to one installed inside
container

```lisp
    (setq python-shell-interpreter "/docker:root@django_web_1:/usr/local/bin/python")
```

Now you can open some file inside the project running inside a
container

    C-x C-f /docker:root@django_web_1:/code/manage.py

If you try to find the definition of the Django module imported in
this buffer, `anaconda-mode` will jump to the package installed inside
this Docker container.

## Bug Reports

Please attach `*anaconda-mode*`, `*anaconda-response*` and
`*anaconda-socat*` buffer content to every created issue.

## Thanks

* Dmitry Gutov **@dgutov**
* Bo Lin **@sadboy**
* Vasilij Schneidermann **@wasamasa**
* Fredrik Bergroth **@fbergroth**
* Fabio Corneti **@fabiocorneti**
* Tom Davis **@tdavis**
* Sviridov Alexander **@sviridov**
* Mario Rodas **@marsam**
