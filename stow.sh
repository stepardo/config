#!/bin/bash

CONFIGDIR=`dirname $0`

for project in "zsh" "emacs" "herbstluftwm" "x11" "git" "tmux" "systemd" "scripts";
do
  stow --dir="$CONFIGDIR" --target="$HOME" -vv "$project";
done

for service in nm-applet mate-settings-daemon mate-power-manager ssh-agent planck compton;
do
	systemctl --user enable $service
done

