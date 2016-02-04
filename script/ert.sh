#!/bin/bash -e

function cleanup() {
    echo -ne '\e[01;34m'"On boot cleanup... "'\e[0m'
    rm -rf $HOME/.emacs.d/anaconda-mode/
    ssh test@localhost 'rm -rf $HOME/.emacs.d/anaconda-mode/'
    echo done
}

function info() {
    echo -ne '\e[01;34m'"Emacs version: "'\e[0m'
    echo $emacs
}

function info_interpreter() {
    echo -ne '\e[01;34m'"Python version: "'\e[0m'
    echo $interpreter
}

function run_unit() {
    echo -e '\e[01;34m'"Run unit tests."'\e[0m\n'
    for emacs in emacs-24.3 emacs-24.4 emacs-24.5 emacs-25.0
    do
        info
        EMACS=$emacs cask exec ert-runner test/unit-test.el
    done
}

function run_integration() {
    echo -e '\e[01;34m'"Run integration tests."'\e[0m\n'
    for emacs in emacs-24.3 emacs-24.4 emacs-24.5 emacs-25.0
    do
        for interpreter in test/interpreters/*.el
        do
            cleanup
            info
            info_interpreter
            EMACS=$emacs cask exec ert-runner test/integration-test.el -l $interpreter
        done
    done
}

cleanup
echo

run_unit
echo

run_integration
