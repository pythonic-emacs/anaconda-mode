#!/bin/bash -e

# Create tramp user.

TRAMP_USER=test
TRAMP_HOME=/home/$TRAMP_USER

if ! id -u $TRAMP_USER 1>/dev/null 2>&1
then
    sudo useradd --home $TRAMP_HOME --create-home --user-group $TRAMP_USER
fi

# Register tramp host.

rm -f $HOME/.ssh/id_rsa*
ssh-keygen -t rsa -b 4096 -f $HOME/.ssh/id_rsa -N ''
touch $HOME/.ssh/known_hosts
ssh-keygen -R localhost
ssh-keyscan -H localhost > $HOME/.ssh/known_hosts

# Authorize localhost for tramp user.

sudo -u $TRAMP_USER mkdir -p $TRAMP_HOME/.ssh
sudo -u $TRAMP_USER sh -c "echo '$(cat $HOME/.ssh/id_rsa.pub)' > $TRAMP_HOME/.ssh/authorized_keys"
