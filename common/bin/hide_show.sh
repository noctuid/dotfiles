#!/bin/bash
# https://bbs.archlinux.org/viewtopic.php?id=149444&p=54

if [ -n "$1" ]; then
	if [ "$1" == "termite" ]; then
		bspc rule -a termite -o floating=on
		xdotool search --onlyvisible --classname $1 windowunmap || xdotool search --classname $1 windowmap || $1 -e "/bin/zsh -c 'sleep 0.01 && xdo move -x 0 -y 16 && xdo resize -w +800 && tmux attach-session -dt dropdown || tmuxinator dropdown'" &

	elif [ "$1" == "Terminology" ]; then
		bspc rule -a terminology -o floating=on
		# inconsolata font; no visual bell; 256 colors
		# window class is main for some bizzare reason
		xdotool search --onlyvisible --classname main windowunmap || xdotool search --classname main windowmap || terminology -f=Inconsolata/12 -G=false -2 -e "/bin/zsh -c 'sleep 0.01 && xdo move -x 0 -y 14 && xdo resize -w +880 && tmux attach-session -dt pretty || tmux new-session -s pretty'"

	else

		xdotool search --onlyvisible --classname $1 windowunmap || xdotool search --classname $1 windowmap || exec $1 &
	fi
else
    echo "Error: no argument supplied. Aborting."
fi
