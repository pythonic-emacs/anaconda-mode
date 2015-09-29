#!/bin/bash -e

function run_ert() {
    for emacs in emacs-24.3 emacs-24.4 emacs-24.5
    do
        echo -ne '\e[01;34m'"On boot cleanup... "'\e[0m'
        rm -rf $HOME/.emacs.d/anaconda-mode/
        echo done
        echo -ne '\e[01;34m'"Emacs version: "'\e[0m'
        echo $emacs
        echo -ne '\e[01;34m'"Python version: "'\e[0m'
        python -V
        EMACS=$emacs cask exec ert-runner $@
    done
}

unset PYENV_VERSION
run_ert $@

export PYENV_VERSION=ipython
run_ert $@
