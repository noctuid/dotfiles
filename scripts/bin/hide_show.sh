#!/bin/bash
# https://bbs.archlinux.org/viewtopic.php?id=149444&p=54
# http://angelic-sedition.github.io/blog/2014/07/19/make-any-terminal-emulator-a-quake-style-dropdown/

win_name=$(xprop -id $(xprop -root _NET_ACTIVE_WINDOW | awk '{print $5}') WM_CLASS | awk '{ gsub(/"/, ""); print $4}')

generic_toggle() {
	(xdotool search --onlyvisible --classname $1 windowactivate && xdo hide -n $1) ||\
	(bspc rule -a $1 -o floating=on && xdotool search --classname $1 windowmap) ||\
	(bspc rule -a $1 -o floating=on && $1 -e "/bin/zsh -c 'sleep 0.01 && xdo move -x 0 -y 14 && xdo resize -w +800 && tmux attach-session -dt dropdown || tmuxinator dropdown'" &)
}

if [ -n "$1" ]; then
	if [ "$1" == "termite" ]; then
		generic_toggle termite
	elif [ "$1" == "xterm" ]; then
		generic_toggle xterm
	elif [ "$1" == "Terminology" ]; then
		# inconsolata font; no visual bell; 256 colors
		# window class is main for some bizzare reason
		(xdotool search --onlyvisible --classname main windowactivate && xdo hide -n main) ||\
		(bspc rule -a main -o floating=on && xdotool search --classname main windowmap) ||\
		(bspc rule -a main -o floating=on && terminology -f=Inconsolata/12 -G=false -2 -e "/bin/zsh -c 'sleep 0.01 && xdo move -x 0 -y 14 && xdo resize -w +880 && tmux attach-session -dt pretty || tmux new-session -s pretty'" &)
	# doing something different for urxvt (not floating); open on first desktop and switch to, then switch back to last window
	elif [ "$1" == "urxvt" ]; then
		if [ "$win_name" == "URxvt" ]; then
			bspc window -f last
		else
			xdotool search --classname urxvt windowactivate || (bspc desktop -f ^1 && urxvt &)
		fi
	else
		xdotool search --onlyvisible --classname $1 windowunmap || xdotool search --classname $1 windowmap || exec $1 &
	fi
else
    echo "Error: no argument supplied. Aborting."
fi
