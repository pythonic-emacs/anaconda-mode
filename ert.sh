#!/bin/bash -e

for emacs in emacs-24.3 emacs-24.4 emacs-24.5
do
    evm use $emacs
    emacs --version
    cask exec ert-runner
done
