#!/bin/bash -e

if [ -n "$TRAVIS_BUILD_DIR" ]
then
    PROJECT_ROOT=$TRAVIS_BUILD_DIR
else
    PROJECT_ROOT=/vagrant
fi

# Update mirrors list.

sudo apt-get update

# Install Emacs build dependencies.

sudo apt-get install -y autoconf automake build-essential git \
     libgif-dev libncurses5-dev libpng-dev libtiff4-dev texinfo

# Build Emacs.

EMACS_DIR=$HOME/emacs
EMACS_SRC=$EMACS_DIR/src
EMACS_VERSIONS=(emacs-24.3 emacs-24.4 emacs-24.5)

mkdir -p $EMACS_DIR $EMACS_SRC

for VERSION in ${EMACS_VERSIONS[@]}
do
    FILE=${VERSION}.tar.xz
    PREFIX=$EMACS_DIR/$VERSION
    cd $EMACS_SRC
    wget -q http://ftp.gnu.org/gnu/emacs/$FILE
    tar xvJf $FILE
    cd $VERSION
    ./configure --prefix=$PREFIX
    make
    make install
    rm $PREFIX/bin/emacs
done

# Build Emacs master.

VERSION=emacs-25.0
PREFIX=$EMACS_DIR/$VERSION
cd $EMACS_SRC
git clone https://github.com/emacs-mirror/emacs $VERSION
cd $VERSION
git checkout emacs-25
./autogen.sh
./configure --prefix=$PREFIX
make
make install
rm $PREFIX/bin/emacs
ln -s $PREFIX/bin/emacs-25.0.* $PREFIX/bin/emacs-25.0

# Install cask.

EMACS_VERSIONS=(emacs-24.3 emacs-24.4 emacs-24.5 emacs-25.0)
CASK_DIR=$HOME/.cask

git clone https://github.com/cask/cask $CASK_DIR

cd $PROJECT_ROOT

export PATH=$PATH:$HOME/.cask/bin:$EMACS_DIR/emacs-24.3/bin:$EMACS_DIR/emacs-24.4/bin:$EMACS_DIR/emacs-24.5/bin:$EMACS_DIR/emacs-25.0/bin

for VERSION in ${EMACS_VERSIONS[@]}
do
    EMACS=$VERSION cask install
    EMACS=$VERSION cask update
done
