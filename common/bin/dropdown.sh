#!/bin/bash
#
# Tries to find a terminal window and brings it to top. If
# not found, a new terminal is started.
#
# The program `wmctrl` is required for this. On Debian, use
#     apt-get install wmctrl
#
TERMCMD=termite
WINID=`xwininfo -root -tree | grep $TERMCMD | tail -n1 | awk '{print $1}'`
if [ -z "$WINID" ]; then
    $TERMCMD&
else
    wmctrl -ia $WINID
fi
