#!/bin/bash -e

function run_ert() {
    for emacs in emacs-24.3 emacs-24.4 emacs-24.5
    do
        echo "On boot cleanup."
        rm -rf $HOME/.emacs.d/anaconda-mode/
        evm use $emacs
        emacs --version
        echo "Python version:"
        pyenv version
        cask exec ert-runner $@
    done
}

unset PYENV_VERSION
run_ert $@

export PYENV_VERSION=ipython
run_ert $@
