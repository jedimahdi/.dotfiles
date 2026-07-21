#!/bin/bash

dbus-update-activation-environment --systemd DISPLAY XAUTHORITY

xset r rate 200 35
setxkbmap -layout us,ir -option 'grp:alt_shift_toggle,caps:escape,shift:both_capslock_cancel'

exec dwm
