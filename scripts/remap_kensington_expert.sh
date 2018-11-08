#!/bin/zsh

name="Kensington Expert Wireless TB Mouse"
id=`xinput list | grep "$name" | cut -f 2 | cut -f 2 -d =`
echo "Remapping kensington keys (id=$id)"
xinput --set-button-map $id 2 1 8 4 5 6 7 3 9
