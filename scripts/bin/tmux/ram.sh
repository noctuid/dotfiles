#!/bin/sh
# for tmux statusline
echo $(($(free | grep buffers/cache | awk '{print $3}')*100/$(free | grep Mem | awk '{print $2}')))
