#!/bin/sh
# [4.15.14] edit click locations to make work in monocle and normal
# start video and wait for firebug to load it
xdotool mousemove 519 326 && xdotool click 1 && sleep 3

# use firebug to copy content of direct vid link
# click twice (required in insert mode to open menu)
xdotool mousemove 282 685 && xdotool click 3 && xdotool click 3
sleep 0.2
xdotool mousemove 367 422 && xdotool click 1
# stop video playing in broswser
xdotool mousemove 519 326 && xdotool click 1 && sleep 0.2
xdotool key Escape
# open link with mpv fullscreen
clipboard_contents=$(xsel -b)
# & so can still use ff while video playing; otherwise must close video first
mpv --fs $clipboard_contents &
