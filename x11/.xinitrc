#!/bin/bash

local hostname=$(hostname)

# if we have an $HOME/opt/bin dir, prepend it to PATH
if [ -d "$HOME/opt/bin" ];
then
  export PATH="$HOME/opt/bin:$PATH"
fi

# ensure emacs server is started
systemctl --user start emacs

xsetroot -cursor_name left_ptr

xset b off
numlockx off

# tap to click on thinkpads
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Tapping Enabled" 1
# make middle button scroll on IBM trackpoint
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200

# use us intl layout with altgr for umlauts, caps lock is escape
setxkbmap -option
setxkbmap -layout 'us(altgr-intl)'
setxkbmap -option
setxkbmap -option caps:escape

if [ "$hostname" == "pluto" ];
then
	# on pluto run mate
	exec dbus-run-session mate-session
else
	# run slock on suspend
	xss-lock slock &
	# run compositor with vsync
	compton --backend glx --vsync opengl-swc &

	# infrastructure
	mate-settings-daemon &
	mate-power-manager &
	nm-applet &

	# use emacs as window manager on all other systems
	exec dbus-launch --exit-with-session emacsclient -c
fi

