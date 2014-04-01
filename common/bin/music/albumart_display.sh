#!/bin/sh
# script to open tmux split and display album art of currently playing song in
# TODO:
# add if statement to check if split next to is ranger and if is don't split
# add artget.py to download photo only if one isn't in folder

artget.py -n localhost -p 6600 -r ~/Music -t -o cover.png
# open split
tmux split-window -h
# get back to vimus
tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l
# swap to left
tmux swap-pane -U

escaped=$(printf '%q' "$1")
dirname=${escaped%/*}

# select last thing in folder (photo) and display
# choosedir doesn't work; get around problems if apostrophe in file names:
tmux send-keys "cd $dirname && ranger --selectfile='cover.png' --cmd='display_file'"
# tmux send-keys "echo $dirname"
tmux send-keys Enter
