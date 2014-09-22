.. |travis| image:: https://travis-ci.org/proofit404/anaconda-mode.png
    :target: https://travis-ci.org/proofit404/anaconda-mode
    :alt: Build Status

.. |gemnasium| image:: https://gemnasium.com/proofit404/anaconda-mode.png
    :target: https://gemnasium.com/proofit404/anaconda-mode
    :alt: Dependency Status

.. |coveralls| image:: https://coveralls.io/repos/proofit404/anaconda-mode/badge.png
    :target: https://coveralls.io/r/proofit404/anaconda-mode
    :alt: Coverage Status

================================================
 Anaconda mode |travis| |gemnasium| |coveralls|
================================================

.. image:: static/logo.png
    :align: center

Code navigation, documentation lookup and completion for Python.

.. figure:: static/goto-definitions.png

This mode support 2.6, 2.7, 3.3 and 3.4 Python versions and provide
following features

* context-sensitive code completion
* jump to definitions
* find references
* view documentation
* virtual environment
* eldoc mode

Installation
------------

package.el
``````````

All you need is install the package from Melpa_::

    M-x package-install RET anaconda-mode RET

manual
``````

``vendor`` directory used to build Melpa_ package only.  If you setup
from git you can't use git submodules for dependency resolution.
Clone this repository somewhere, add it to you ``load-path`` and do
following command from project root::

    pip install -r requirements.txt -t .

prelude
```````

``anaconda-mode`` included into `Emacs Prelude`_ distribution.  You
can use it as well.  Look at ``prelude-python`` module to see more
details.

Usage
-----

Anaconda mode comes with ``anaconda_mode.py`` server.  This server
allow you to use jedi_ package over jsonrpc api.  Server choice first
available port starting from 24970.  If no host was specified loopback
interface will be used.  Anaconda mode will run this server
automatically on first call of any anaconda-mode command.  Anaconda
mode detect active virtual environment through value of
``python-shell-virtualenv-path`` variable defined in ``python.el``
library.  When you set it to actual virtualenv path next anaconda-mode
command you call will restart server process in proper environment
before performing this call.  This allow anaconda processing virtual
environment site-packages with minimum number of actions from your
side.  I strongly recommended you to use `pyenv-mode`_ or similar
package to hold ``python-shell-virtualenv-path`` in actual state.

Tramp and vagrant
`````````````````

It's possible to use anaconda-mode on remote server when you connect
to it using tramp.  Setup process differs from local usage.  You need
to login on remote machine and install anaconda-mode server.  You can
download it directly from Melpa_ and unpack it to some directory.  If
you use virtual environment on remote server then you need to activate
it before we continue.  Now run ``anaconda_mode.py`` from shell::

    . venv/bin/activate
    python anaconda_mode.py 0.0.0.0

To tell anaconda-mode to connect to that server you need to run
following command::

    M-x anaconda-mode-remote

It will ask you to enter host and port information for remote server.
In case of vagrant you can specify ``127.0.0.1`` as host part.  To get
proper completion you need to open your files with tramp addresses
even for vagrant setup.  This caused because of different relative
paths on your local machine and inside vagrant.  To stop this behavior
and enable ``anaconda_mode.py`` server starts locally run command
below::

    M-x anaconda-mode-local

Interactive commands
````````````````````

Here are interactive commands available with anaconda-mode

==========  ==============================
Keybinding  Description
==========  ==============================
M-.         anaconda-mode-goto-definitions
M-*         anaconda-nav-pop-marker
M-?         anaconda-mode-view-doc
M-r         anaconda-mode-usages
==========  ==============================

If goto definitions, assignments or usages cause multiple candidates
you'll see advanced anaconda navigator buffer.  You can automatically
enable ``anaconda-mode`` in all python buffers with following code in
your configuration

.. code:: lisp

    (add-hook 'python-mode-hook 'anaconda-mode)

ElDoc
`````

``anaconda-mode`` provide document function to ``eldoc-mode``.  All
you need is enable ``eldoc-mode`` in addition to previous setup.

.. code:: lisp

    (add-hook 'python-mode-hook 'eldoc-mode)

Known Issues
------------

If you're using proxy server, you have to make sure that the proxy is
not used for communication with anaconda-mode:

.. code:: shell

    export no_proxy="localhost,127.0.0.1"

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

.. _Melpa: http://melpa.milkbox.net/
.. _pyenv-mode: https://github.com/proofit404/pyenv-mode
.. _jedi: http://jedi.jedidjah.ch/en/latest/
.. _emacs prelude: https://github.com/bbatsov/prelude
