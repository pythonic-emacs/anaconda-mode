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

This package support 2.6, 2.7, 3.3 and 3.4 Python versions and provide
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

Configuration
-------------

You can automatically enable ``anaconda-mode`` in all python buffers
with following code in your configuration:

.. code:: lisp

    (add-hook 'python-mode-hook 'anaconda-mode)

ElDoc
`````

``anaconda-mode`` provide document function to ``eldoc-mode``.  All
you need is enable ``eldoc-mode`` in addition to previous setup.

.. code:: lisp

    (add-hook 'python-mode-hook 'eldoc-mode)

Usage
-----

To start completion press ``C-M-i``.  This is standard emacs binding
for ``complete-at-point`` function.  You can use company-mode_ with
company-anaconda_ backend to get more intelligent ui.  Or
auto-complete-mode_ with ac-anaconda_ as last try.

Anaconda mode detect active virtual environment through value of
``python-shell-virtualenv-path`` variable defined in ``python.el``
library.  When you set it to actual virtualenv path next anaconda-mode
command you call will use this environment for completion candidates
search.  I strongly recommended you to use `pyenv-mode`_ or similar
package to hold ``python-shell-virtualenv-path`` in actual state.

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
you'll see advanced anaconda navigator buffer.

Tramp
`````

**Not properly implemented yet**

It's possible to use anaconda-mode on remote server when you connect
to it using tramp.  In case of vagrant you need to use ``127.0.0.1``
as tramp address.

Implementation details
----------------------

Anaconda mode comes with ``anaconda_mode.py`` server.  This server
allow you to use jedi_ python library over jsonrpc api.  Server choice
first available port starting from 24970.  Anaconda mode will run this
server automatically on first call of any anaconda-mode command.

This mean that usage results depends on your project installation.  To
make it available for ``anaconda-mode`` you have two options:

* add your project to Emacs ``PYTHONPATH``:

::

   M-x setenv RET PYTHONPATH RET /path/to/project

* install your project into its virtual environment

.. code:: shell

    . env/bin/activate
    pip install -e .

Bug Reports
-----------

Please attach ``*anaconda-mode*`` buffer content to every created issue.

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
.. _company-mode: http://company-mode.github.io/
.. _company-anaconda: https://github.com/proofit404/company-anaconda
.. _auto-complete-mode: https://github.com/auto-complete/auto-complete
.. _ac-anaconda: https://github.com/proofit404/ac-anaconda
