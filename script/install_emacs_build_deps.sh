#!/bin/bash -e

# Update mirrors list.

sudo apt-get update

# Install Emacs build dependencies.

sudo apt-get install -y autoconf automake build-essential git      \
     libgif-dev libncurses5-dev libpng-dev libtiff4-dev libxpm-dev \
     libXaw7-dev texinfo
