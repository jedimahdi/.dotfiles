#!/bin/bash
ID=$(xdpyinfo | grep focus | cut -f4 -d " ")
PID=$(($(xprop -id $ID | grep -m 1 PID | cut -d " " -f 3) + 2))
if [ -e "/proc/$PID/cwd" ]
then
    termite -d $(readlink /proc/$PID/cwd) &
else
    termite
fi
