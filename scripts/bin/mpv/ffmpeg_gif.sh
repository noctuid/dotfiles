#!/bin/bash

# make directory if doesn't exist:
mkdir -p ~/database/Move/gif/frames
cd ~/database/Move/gif
# remove a previous mkv
rm output.mkv
# https://gist.github.com/anonymous/3927068
# http://askubuntu.com/questions/20530/how-can-i-find-the-location-on-the-desktop-of-a-window-on-the-command-line
active_winfo=$(xwininfo -id $(xprop -root | awk '/_NET_ACTIVE_WINDOW\(WINDOW\)/{print $NF}'))
win_geo=$(echo $active_winfo | grep -oEe 'geometry [0-9]+x[0-9]+' | grep -oEe '[0-9]+x[0-9]+')
win_xy=$(echo $active_winfo | grep -oEe 'Corners:\s+\+[0-9]+\+[0-9]+' | grep -oEe '[0-9]+\+[0-9]+' | sed -e 's/+/,/')
# wintitle=$(xtitle)

# convert 
# no audio; frame rate of 30; record active window
ffmpeg -f x11grab -r 30 -s $win_geo -i :0.0+$win_xy -vcodec libx264 -preset ultrafast -crf 0 -threads 2 output.mkv

# if want to go ahead and make a gif with default settings
if [ "$1" == "do_it_now"]
then
	~/bin/mpv/gifconv output.mkv default &
fi
