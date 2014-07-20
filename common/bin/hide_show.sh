#!/bin/bash
# https://bbs.archlinux.org/viewtopic.php?id=149444&p=54

if [ -n "$1" ]; then
	if [ "$1" == "termite" ]; then
		bspc rule -a termite -o floating=on
		xdotool search --onlyvisible --classname $1 windowunmap || xdotool search --classname $1 windowmap || termite -e "/bin/zsh -c 'sleep 0.01 && xdo move -x 0 -y 16 && xdo resize -w +800 && tmux attach-session -dt dropdown || tmuxinator dropdown'" &
	else
		xdotool search --onlyvisible --classname $1 windowunmap || xdotool search --classname $1 windowmap || exec $1 &
	fi
else
    echo "Error: no argument supplied. Aborting."
fi
