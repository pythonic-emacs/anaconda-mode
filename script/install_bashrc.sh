#!/bin/bash -e

if [ -n "$TRAVIS_BUILD_DIR" ]
then
    PROJECT_ROOT=$TRAVIS_BUILD_DIR
else
    PROJECT_ROOT=/vagrant
fi

cp $PROJECT_ROOT/script/bashrc $HOME/.bashrc
