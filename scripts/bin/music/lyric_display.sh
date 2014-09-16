#!/bin/sh
# Since vimus doesn't support lyric fetching, I'll just bind a key to open ncmpcpp in a split and fetch the lyrcs for the song

# open split
tmux split-window -h
# get vimus back
tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l
tmux send-keys "ncmpcpp" && tmux send-keys "Enter" && tmux send-keys "l"
