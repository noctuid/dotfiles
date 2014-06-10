#!/bin/sh
# [4.15.14] edit click locations to make work in monocle and normal
# start video and wait for firebug to load it
xdotool mousemove 519 326 && xdotool click 1 && sleep 3

# to prevent other menu from coming up, return to normal mode
xdotool key Escape && sleep 0.2
# open menu and copy link
xdotool mousemove 284 693 && xdotool click 3
sleep 0.4
xdotool mousemove 376 430 && xdotool click 1
# stop video playing in broswser
xdotool mousemove 519 326 && xdotool click 1 && sleep 0.2
xdotool key Escape
# open link with mpv fullscreen
clipboard_contents=$(xsel -b)
# & so can still use ff while video playing; otherwise must close video first
mpv $clipboard_contents &
