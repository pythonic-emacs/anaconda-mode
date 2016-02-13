#!/bin/bash -e

function info_emacs() {
    echo -ne '\e[01;34m'"Emacs version: "'\e[0m'
    echo $emacs
}

function info_interpreter() {
    echo -ne '\e[01;34m'"Python version: "'\e[0m'
    echo $interpreter
}

function run() {
    for emacs in emacs-24.3 emacs-24.4 emacs-24.5 emacs-git-snapshot
    do
        for interpreter in test/interpreters/*.el
        do
            info_emacs
            info_interpreter
            evm use $emacs
            cask exec ert-runner -l $interpreter
        done
    done
}

run
