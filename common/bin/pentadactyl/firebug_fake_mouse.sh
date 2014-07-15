#!/bin/sh
# start video and wait for firebug to load it
xdotool mousemove 519 326 click 1 && sleep 3

# to prevent other menu from coming up, return to normal mode
xdotool key Escape && sleep 0.2
# open menu and copy link
xdotool mousemove 284 713 click 3
sleep 0.5
xdotool mousemove_relative -- 97 -262  click 1
# stop video playing in broswser
xdotool mousemove 519 326 click 1 && sleep 0.2
xdotool key Escape
# open link with mpv fullscreen
clipboard_contents=$(xsel -b)
mpv $clipboard_contents &
