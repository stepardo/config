#!/bin/sh

xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200

# ensure emacs server is started
systemctl --user start emacs

xsetroot -cursor_name left_ptr

xset b off
numlockx off

# tap to click on thinkpads
xinput set-prop SynPS/2 Synaptics TouchPad libinput Tapping Enabled 1

# use us intl layout with altgr for umlauts, swap caps lock and escape
setxkbmap -layout 'us(altgr-intl)' -option caps:swapescape

# use emacs as window manager
exec dbus-launch --exit-with-session herbstluftwm
