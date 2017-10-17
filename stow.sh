#!/bin/bash

CONFIGDIR=`dirname $0`

for project in "zsh" "emacs" "herbstluftwm";
do
  stow --dir="$CONFIGDIR" --target="$HOME" -vv "$project";
done

