#!/bin/bash

function enable_keyboard()
{
    id=`xinput list | grep 'AT Translated Set 2' | cut -f 2 | cut -f 2 -d =`
    core=`xinput list | grep 'Virtual core keyboard' | cut -f 2 | cut -f 2 -d =`
    xinput reattach $id $core
    echo "Keyboard enabled"; 
}

function disable_keyboard()
{
    id=`xinput list | grep 'AT Translated Set 2' | cut -f 2 | cut -f 2 -d =`
    xinput float $id
    echo "Keyboard disabled"
}

case $1 in
    enable)
        enable_keyboard
        ;;
    disable)
        disable_keyboard
        ;;
    *)
        ;;
esac
