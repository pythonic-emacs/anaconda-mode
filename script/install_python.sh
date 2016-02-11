#!/bin/bash -e

# Add deadsnakes mirror.

echo "deb http://ppa.launchpad.net/fkrull/deadsnakes/ubuntu precise main" > /etc/apt/sources.list.d/python.list
wget --quiet -O - "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0xFF3997E83CD969B409FB24BC5BB92C09DB82666C" | apt-key add -

# Update mirrors list.

apt-get update

# Install python.

apt-get install -y python2.6 python2.7 python3.3 python3.4

# Install setuptools and pip.

wget https://bootstrap.pypa.io/ez_setup.py -O /tmp/ez_setup.py
python3.4 /tmp/ez_setup.py
easy_install-3.4 pip

# Install python packages.

pip install -r requirements/qa.txt

# TODO: install ipython.
