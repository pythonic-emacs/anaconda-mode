#!/bin/bash -e

# Install evm.

if [ ! -d $HOME/.evm ]
then
    git clone https://github.com/rejeep/evm $HOME/.evm
else
    git -C $HOME/.evm pull
fi

EVM_DIR=$HOME/.evm/versions

mkdir -p $EVM_DIR

PATH=$HOME/.evm/bin:$HOME/.cask/bin:$PATH

evm config path $EVM_DIR

evm install emacs-24.3 --skip
evm install emacs-24.4 --skip
evm install emacs-24.5 --skip
evm install emacs-git-snapshot --skip

# Install cask.

if [ ! -d $HOME/.cask ]
then
    git clone https://github.com/cask/cask $HOME/.cask
else
    git -C $HOME/.cask pull
fi

evm use emacs-24.3
cask --path /vagrant/ install
cask --path /vagrant/ update

evm use emacs-24.4
cask --path /vagrant/ install
cask --path /vagrant/ update

evm use emacs-24.5
cask --path /vagrant/ install
cask --path /vagrant/ update

evm use emacs-git-snapshot
cask --path /vagrant/ install
cask --path /vagrant/ update

# Add deadsnakes mirror.

sudo sh -c 'echo "deb http://ppa.launchpad.net/fkrull/deadsnakes/ubuntu precise main" > /etc/apt/sources.list.d/python.list'
wget --quiet -O - "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0xFF3997E83CD969B409FB24BC5BB92C09DB82666C" | sudo apt-key add -

# Update mirrors list.

sudo apt-get update

# Install python.

sudo apt-get install -y python2.6 python2.7 python3.3 python3.4

# Install setuptools and pip.

wget https://bootstrap.pypa.io/ez_setup.py -O /tmp/ez_setup.py
sudo python2.6 /tmp/ez_setup.py
sudo python2.7 /tmp/ez_setup.py
sudo python3.3 /tmp/ez_setup.py
sudo python3.4 /tmp/ez_setup.py
sudo easy_install-2.6 pip
sudo easy_install-2.7 pip
sudo easy_install-3.3 pip
sudo easy_install-3.4 pip

# Install python packages.

sudo pip install -r /vagrant/requirements/ci.txt
sudo pip install ipython

# Copy bashrc.

cp /vagrant/script/bashrc $HOME/.bashrc
