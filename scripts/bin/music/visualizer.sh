#!/bin/sh
# Since vimus doesn't support visualization, I just bind a key to open ncmpcpp in a split and do the visualizatoin

# open split
tmux split-window -h
# get vimus back
tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l
# open visualizer
tmux send-keys "ncmpcpp" && tmux send-keys "Enter" && tmux send-keys "8"
