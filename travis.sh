#!/bin/bash

while true
do
    sleep 9m
    echo -n .
done &

ansible-playbook -i inventories/travis playbook.yml
