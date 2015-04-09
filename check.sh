#!/bin/bash

USER=$(cat /etc/passwd | grep chemist)
USER_ID=$(echo $USER | awk -F':' '{ print $3 }')

echo -n $USER_ID
exit 0
