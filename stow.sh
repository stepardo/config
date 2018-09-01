#!/bin/bash

CONFIGDIR=`dirname $0`

for project in "zsh" "emacs" "herbstluftwm" "x11" "git" "tmux" "systemd" "scripts";
do
  stow --dir="$CONFIGDIR" --target="$HOME" -vv "$project";
done
