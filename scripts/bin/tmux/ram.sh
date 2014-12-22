#!/bin/sh
# for tmux statusline
free | awk '/Mem/ {printf("%d", $3/$2 * 100)}'
