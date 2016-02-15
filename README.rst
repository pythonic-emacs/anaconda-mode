
.. |travis| image:: https://img.shields.io/travis/proofit404/anaconda-mode.svg?style=flat-square
    :target: https://travis-ci.org/proofit404/anaconda-mode
    :alt: Build Status

.. |coveralls| image:: https://img.shields.io/coveralls/proofit404/anaconda-mode.svg?style=flat-square
    :target: https://coveralls.io/r/proofit404/anaconda-mode
    :alt: Coverage Status

.. |requires| image:: https://img.shields.io/requires/github/proofit404/anaconda-mode.svg?style=flat-square
    :target: https://requires.io/github/proofit404/anaconda-mode/requirements
    :alt: Requirements Status

.. |melpa| image:: http://melpa.org/packages/anaconda-mode-badge.svg
    :target: http://melpa.org/#/anaconda-mode
    :alt: Melpa

.. |melpa-stable| image:: http://stable.melpa.org/packages/anaconda-mode-badge.svg
    :target: http://stable.melpa.org/#/anaconda-mode
    :alt: Melpa Stable

.. image:: static/logo.png
    :align: right
    :alt: Logo

===============
 Anaconda mode
===============

|travis| |coveralls| |requires| |melpa| |melpa-stable|

Code navigation, documentation lookup and completion for Python.

.. figure:: static/completion.png

.. figure:: static/reference.png

This package support 2.6, 2.7, 3.3 and 3.4 Python versions and provide
following features

* context-sensitive code completion
* jump to definitions
* find references
* view documentation
* virtual environment
* eldoc mode
* all this staff inside vagrant and remote hosts

Installation
------------

To use this package you need to install ``pip``.

package.el
``````````

All you need is install the package from Melpa_::

    M-x package-install RET anaconda-mode RET

manual
``````

Clone this repository somewhere and add this directory to you
``load-path``.

prelude
```````

``anaconda-mode`` included into `Emacs Prelude`_ distribution.  You
can use it as well.  Look at ``prelude-python`` module to see more
details.

spacemacs
`````````

``anaconda-mode`` included into Spacemacs_ distribution.  You can use
it as well.  Look at ``python`` language layer to see more details.

Configuration
-------------

You can automatically enable ``anaconda-mode`` in all python buffers
with following code in your configuration:

.. code:: lisp

    (add-hook 'python-mode-hook 'anaconda-mode)

ElDoc
`````

``anaconda-eldoc-mode`` provide document function to ``eldoc-mode``.  All
you need is enable ``anaconda-eldoc-mode`` in addition to previous setup.

.. code:: lisp

    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

Usage
-----

To start completion press ``C-M-i``.  This is standard emacs binding
for ``complete-at-point`` function.  You can use company-mode_ with
company-anaconda_ backend to get more intelligent ui.  Or
auto-complete-mode_ with ac-anaconda_ as last try.

Interactive commands
````````````````````

There is a list of interactive commands available with anaconda-mode

==========  ==============================
Keybinding  Description
==========  ==============================
C-M-i       anaconda-mode-complete
M-.         anaconda-mode-find-definitions
M-,         anaconda-mode-find-assignments
M-r         anaconda-mode-find-references
M-*         anaconda-mode-go-back
M-?         anaconda-mode-show-doc
==========  ==============================

If goto definitions, assignments or usages cause multiple candidates
you'll see advanced anaconda navigator buffer.

PYTHONPATH
``````````

You can add your project to Emacs ``PYTHONPATH``.  If you store project
dependencies somewhere on you machine add its too.

.. code:: lisp

    (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/project")
    (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/dependency")

Virtual environment
```````````````````

Use virtual environment to isolate your project dependencies form
other system.  You can additionally install you project in editable
mode into virtual environment.  This will improve usage references
search.  Then activate this virtual environment inside Emacs.

::

    M-x pythonic-activate RET /path/to/virtualenv RET

Also you can use `pyenv-mode`_ or similar package to hold virtual
environment in actual state.

Each action above applies to ``anaconda-mode`` immediately.  Next
``anaconda-mode`` command you call will use this environment for
completion candidates search.

Tramp
`````

It's possible to use anaconda-mode on remote server when you connect
to it using tramp.  Anaconda-mode can search for completion candidates
and all other stuff on remote server while you running Emacs locally.
First of all open interesting remote file.

::

    C-x C-f /ssh:remote_host:project/__init__.py RET

After tramp connection successfully applies and your see actual buffer
content activate remote virtual environment.

::

    M-x pythoninc-activate RET /ssh:remote_host:/home/user/venv RET

Now any anaconda-mode command will use ``/home/user/venv/bin/python``
interpreter running on ``remote_host`` over ssh.  If you don't use
virtual environment remotely then you have an option to specify remote
interpreter directly.

.. code:: lisp

    (setq python-shell-interpreter "/ssh:remote_host:/usr/bin/python")

It is important to remember that ``remote_host`` must be a real host
name or an IP address.  SSH aliases not allowed to be used with
anaconda-mode.  Also 9000 port on remote host should be open to
incoming connections from your local machine.  The final not I want to
say here is about project scope.  All kind of search inside the
virtual environment available from any buffer.  But search inside your
project is available only if you open it on the same machine as
interpreter.

Vagrant
```````

You can get all intelligent features of anaconda-mode with virtual
environment deployed on your vagrant box.  Add port forwarding line to
your Vagrantfile.

::

   config.vm.network "forwarded_port", guest: 9000, host: 9000

Fire up vagrant machine as usual and open your project inside vagrant
box.

::

    C-x C-f /ssh:vagrant@localhost#2222:/vagrant/polls/views.py

Then activate your project environment installed inside vagrant.

::

    M-x pythonic-activate RET /ssh:vagrant@localhost#2222:/vagrant/polls/venv RET

Remember that standard password for vagrant user is ``vagrant``. It is
too annoying to type this password each time you want to connect.  I
use ``ssh-copy-id`` to upload my public ssh key the box.

::

    ssh-copy-id vagrant@localhost -p 2222

If you have random connection errors during interaction with running
server - try to replace host name with IP address.  For example
``localhost`` with ``127.0.0.1``.

Now you are ready to go.

Implementation details
----------------------

Anaconda mode comes with ``anaconda_mode.py`` server.  This server
allow you to use jedi_ python library over jsonrpc api.  Server choice
first available port starting from 9000.  Anaconda mode will run this
server automatically on first call of any anaconda-mode command.

This mean that completion results and reference search depends on your
project installation.

Bug Reports
-----------

Please attach ``*anaconda-mode*`` buffer content to every created
issue.

Issues
------

DistutilsOptionError
````````````````````

::

    DistutilsOptionError: must supply either home or prefix/exec-prefix -- not both

This occurs due to `distutils bug
<http://bugs.python.org/issue22269>`_ when ``pip -t`` option conflict
with ``distutils.cfg`` ``prefix`` option.  If you install ``pip`` with
``homebrew`` you are on fire.  There are few options to avoid this
issue.

- install ``anaconda-mode`` `dependencies
  <https://github.com/proofit404/anaconda-mode/blob/master/requirements.txt>`_
  manually
- remove ``prefix`` option from ``distutils.cfg``

AttributeError and KeyError randomly happens
````````````````````````````````````````````

This kind of problems were reported with jedi 0.9 version.  You can
try to downgrade jedi version down to 0.8.

::

   M-x find-library RET anaconda-mode RET
   M-! rm -rf jedi* RET
   M-! pip install "jedi<0.9" -t . RET

Contributions
-------------

Are very welcome.  But any significant change has to be accompanied
with tests, both for Emacs Lisp and Python code.  To run the test
suite, call:

.. code:: shell

    tox

Thanks
------

* Dmitry Gutov **@dgutov**
* Bo Lin **@sadboy**
* Vasilij Schneidermann **@wasamasa**
* Fredrik Bergroth **@fbergroth**
* Fabio Corneti **@fabiocorneti**
* Tom Davis **@tdavis**
* Sviridov Alexander **@sviridov**
* Mario Rodas **@marsam**

.. _Melpa: http://melpa.milkbox.net/
.. _pyenv-mode: https://github.com/proofit404/pyenv-mode
.. _jedi: http://jedi.jedidjah.ch/en/latest/
.. _emacs prelude: https://github.com/bbatsov/prelude
.. _spacemacs: https://github.com/syl20bnr/spacemacs
.. _company-mode: http://company-mode.github.io/
.. _company-anaconda: https://github.com/proofit404/company-anaconda
.. _auto-complete-mode: https://github.com/auto-complete/auto-complete
.. _ac-anaconda: https://github.com/proofit404/ac-anaconda
