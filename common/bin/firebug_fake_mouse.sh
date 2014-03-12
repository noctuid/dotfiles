#!/bin/sh
# start video and wait for firebug to load it
xdotool mousemove 519 326 && xdotool click 1 && sleep 3
xdotool key -window Firefox Escape
# use firebug to copy content of direct vid link
xdotool mousemove 54 683 && xdotool click 3 && sleep 0.2 && xdotool mousemove 151 430 && xdotool click 1
xdotool key Escape
# stop video playing in broswser
sleep 0.2
xdotool mousemove 519 326 && xdotool click 1 && sleep 0.2
# open link with mpv fullscreen
clipboard_contents=$(xsel -b)
# & so can still use ff while video playing; otherwise must close video first
mpv --fs $clipboard_contents &
