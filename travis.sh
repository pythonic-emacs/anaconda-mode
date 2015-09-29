#!/bin/bash

while true
do
    sleep 9m
    echo -n .
done &

ansible-playbook -i deploy/inventories/travis deploy/playbook.yml
