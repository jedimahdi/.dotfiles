#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
[ -f ~/.xprofile ] && . ~/.xprofile

DRI_PRIME=1 exec xmonad
