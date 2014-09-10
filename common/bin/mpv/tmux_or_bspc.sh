#!/bin/sh
# for different bindings depending on whether mpv is running in terminal (audio)
# or for video

# don't normally fold manually in scripts.. but since this is just an ungodly mile long if elif chain, I'm going to

# i set title to mpv - ${filename} in .mpv/config
title=$(xtitle | awk '{ print $1 }')
if [ "$title" == "mpv" ]; then
	# r bindings#{{{
	# desktop switching#{{{
	if [ "$1" == "a" ]; then
		bspc desktop -f ^1
	elif [ "$1" == "r" ]; then
		bspc desktop -f ^2
	elif [ "$1" == "s" ]; then
		bspc desktop -f ^3
	elif [ "$1" == "t" ]; then
		bspc desktop -f ^4
	elif [ "$1" == "d" ]; then
		bspc desktop -f ^5
	elif [ "$1" == "h" ]; then
		bspc desktop -f ^6
	elif [ "$1" == "n" ]; then
		bspc desktop -f ^7
	elif [ "$1" == "e" ]; then
		bspc desktop -f ^8
	elif [ "$1" == "i" ]; then
		bspc desktop -f ^9
	elif [ "$1" == "o" ]; then
		bspc desktop -f ^10
	#}}}
	# move to desktop#{{{
	elif [ "$1" == "A" ]; then
		bspc window -d ^1
	elif [ "$1" == "R" ]; then
		bspc window -d ^2
	elif [ "$1" == "S" ]; then
		bspc window -d ^3
	elif [ "$1" == "t" ]; then
		bspc window -d ^4
	elif [ "$1" == "D" ]; then
		bspc window -d ^5
	elif [ "$1" == "H" ]; then
		bspc window -d ^6
	elif [ "$1" == "N" ]; then
		bspc window -d ^7
	elif [ "$1" == "E" ]; then
		bspc window -d ^8
	elif [ "$1" == "I" ]; then
		bspc window -d ^9
	elif [ "$1" == "O" ]; then
		bspc window -d ^10
	#}}}

	# resize#{{{
	elif [ "$1" == "rmh" ]; then
		bspc window --edge right -40 || bspc window --edge left -40
	elif [ "$1" == "rmn" ]; then
		bspc window --edge up +30 || bspc window --edge down +30
	elif [ "$1" == "rme" ]; then
		bspc window --edge up -30 || bspc window --edge down -30
	elif [ "$1" == "rmi" ]; then
		bspc window --edge right +40 || bspc window --edge left +40
	#}}}
	# cycle focus#{{{
	elif [ "$1" == "r," ]; then
		bspc desktop -C forward
	elif [ "$1" == "r." ]; then
		bspc desktop -C backward
	#}}}
	# close window
	elif [ "$1" == "rx" ]; then
		bspc winow -c
	#}}}

	# s bindings#{{{
	# directional select#{{{
	elif [ "$1" == "sh" ]; then
		bspc window -f left
	elif [ "$1" == "sn" ]; then
		bspc window -f down
	elif [ "$1" == "se" ]; then
		bspc window -f up
	elif [ "$1" == "si" ]; then
		bspc window -f right
	#}}}

	# select last pane
	elif [ "$1" == "sl" ]; then
		bspc window -f last
	# monocle toggle
	elif [ "$1" == "st" ]; then
		bspc desktop -l next
	elif [ "$1" == "su" ]; then
		bspc config -d focused window_gap $((`bspc config -d focused window_gap` - 4 ))
	elif [ "$1" == "sU" ]; then
		bspc config -d focused window_gap $((`bspc config -d focused window_gap` + 4 ))

	#}}}

	fi
else
# tmux
# just copied this from vimus one
	# "r" bindings#{{{
	# window switching#{{{
	if [ "$1" == "a" ]; then
		tmux select-window -t 1
	elif [ "$1" == "r" ]; then
		tmux select-window -t 2
	elif [ "$1" == "s" ]; then
		tmux select-window -t 3
	elif [ "$1" == "t" ]; then
		tmux select-window -t 4
	elif [ "$1" == "d" ]; then
		tmux select-window -t 5
	elif [ "$1" == "h" ]; then
		tmux select-window -t 6
	elif [ "$1" == "n" ]; then
		tmux select-window -t 7
	elif [ "$1" == "e" ]; then
		tmux select-window -t 8
	elif [ "$1" == "i" ]; then
		tmux select-window -t 9
	elif [ "$1" == "o" ]; then
		tmux select-window -t 10
	#}}}

	# resize#{{{
	elif [ "$1" == "rmh" ]; then
		tmux resize-pane -L 5
	elif [ "$1" == "rmn" ]; then
		tmux resize-pane -D 3
	elif [ "$1" == "rme" ]; then
		tmux resize-pane -U 3
	elif [ "$1" == "rmi" ]; then
		tmux resize-pane -R 5
	#}}}
	# cycle#{{{
	elif [ "$1" == "r," ]; then
		tmux swap-pane -U
	elif [ "$1" == "r." ]; then
		tmux swap-pane -D
	#}}}

	# new session
	elif [ "$1" == "r_" ]; then
		tmux new-session
	# new window
	elif [ "$1" == "rc" ]; then
		tmux new-window
	# kill pane
	elif [ "$1" == "rx" ]; then
		tmux kill-pane
	# last window
	elif [ "$1" == "rl" ]; then
		tmux select-window -l
	#split /
	elif [ "$1" == "r/" ]; then
		tmux split-window -h
	# split -
	elif [ "$1" == "r-" ]; then
		tmux split-window
	# break pane
	elif [ "$1" == "r!" ]; then
		tmux break-pane
	#}}}
	# s bindings#{{{
	# directional select#{{{
	elif [ "$1" == "sh" ]; then
		tmux select-pane -L
	elif [ "$1" == "sn" ]; then
		tmux select-pane -D
	elif [ "$1" == "se" ]; then
		tmux select-pane -U
	elif [ "$1" == "si" ]; then
		tmux select-pane -R
	#}}}

	# select last pane
	elif [ "$1" == "sl" ]; then
		tmux select-pane -l
	elif [ "$1" == "sv" ]; then
		tmux select-layout main-vertical

	# zoom/monocle
	elif [ "$1" == "st" ]; then
		tmux resize-pane -Z
	fi
	#}}}
fi
