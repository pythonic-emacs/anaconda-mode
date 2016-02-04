#!/bin/bash -e

if [ -n "$TRAVIS_BUILD_DIR" ]
then
    PROJECT_ROOT=$TRAVIS_BUILD_DIR
else
    PROJECT_ROOT=/vagrant
fi

# Create tramp user.

TRAMP_USER=test
TRAMP_HOME=/home/$TRAMP_USER

sudo useradd --home $TRAMP_HOME --create-home --user-group $TRAMP_USER

# Register tramp host.

ssh-keygen -t rsa -b 4096 -f $HOME/.ssh/id_rsa -N ''
touch $HOME/.ssh/known_hosts
ssh-keygen -R localhost
ssh-keyscan -H localhost > $HOME/.ssh/known_hosts

# Authorize localhost for tramp user.

sudo -u $TRAMP_USER mkdir -p $TRAMP_HOME/.ssh
sudo -u $TRAMP_USER sh -c "echo '$(cat $HOME/.ssh/id_rsa.pub)' > $TRAMP_HOME/.ssh/authorized_keys"

# Update mirrors list.

sudo apt-get update

# Install Emacs build dependencies.

sudo apt-get install -y autoconf automake build-essential curl git    \
     libbz2-dev libgif-dev libncurses5-dev libpng-dev libreadline-dev \
     libsqlite3-dev libssl-dev libtiff4-dev libxaw7-dev libxpm-dev    \
     llvm make texinfo wget zlib1g-dev

# Build Emacs.

EMACS_DIR=$HOME/.emacs
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

# Install pyenv.

PYENV_DIR=$HOME/.pyenv

rm -rf $PYENV_DIR  # Clean up Travis-CI cache.

git clone https://github.com/yyuu/pyenv $PYENV_DIR

PYENV_VIRTUALENV_DIR=$PYENV_DIR/plugins/pyenv-virtualenv

git clone https://github.com/yyuu/pyenv-virtualenv $PYENV_VIRTUALENV_DIR

export PATH=$PATH:$HOME/.pyenv/bin:$HOME/.pyenv/shims

pyenv install 2.6.9
pyenv install 2.7.10
pyenv install 3.3.6
pyenv install 3.4.3
pyenv virtualenv 3.4.3 ipython

pyenv global 3.4.3
pip install -U setuptools pip wheel
pip install tox coveralls coveralls-merge
PYENV_VERSION=ipython pip install ipython
pyenv rehash

# Copy bashrc.

cp $PROJECT_ROOT/script/bashrc $HOME/.bashrc
