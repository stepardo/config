#!/bin/bash

# if we have an $HOME/opt/bin dir, prepend it to PATH
if [ -d "$HOME/opt/bin" ];
then
  export PATH="$HOME/opt/bin:$PATH"
fi

systemctl --user start agents.target
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
systemctl --user set-environment SSH_AUTH_SOCK=$SSH_AUTH_SOCK
systemctl --user set-environment PATH=$PATH

# ensure emacs server is started
systemctl --user start emacs

start_emacs=yes
if [ $(hostname) == "pluto" ];
then
  start_emacs=no
fi

# check if emacs server started correctly
systemctl --user status emacs > /dev/null 2>&1
if [ $? != 0 ];
then
  start_emacs=no
fi

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

if [ $start_emacs == "no" ];
then
	# on pluto run mate
	exec dbus-run-session mate-session
else
	# run slock on suspend
	xss-lock slock &

	# infrastructure
        systemctl --user start mate-utils.target

	# use emacs as window manager on all other systems
	exec dbus-launch --exit-with-session emacsclient -c
fi

