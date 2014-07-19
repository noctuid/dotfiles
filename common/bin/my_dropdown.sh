#!/bin/sh
# for making a dropdown term with tmux (potentially usable with any window manager)
# Requirements: tmux, xprop, bspwm (or wm of choice), termite (or terminal of choice), xdotool (and/or xdo), and a hotkey program (i.e. default of DE or sxhkd, xchainkeys, xbindkeys, etc.)

# modified from this to get window class https://bbs.archlinux.org/viewtopic.php?id=156660; probably more elegant to just use awk
win_name=$(xprop -id $(xprop -root 32x '\t$0' _NET_ACTIVE_WINDOW | cut -f 2) WM_CLASS | cut -d '=' -f 2 | cut -d ',' -f 2 | sed 's/[\"\ ]//g')

if [ "$win_name" == "Termite" ]; then
	# if the current window is termite, kill it (tmux session remains)
	bspc window -c
else
	# open termite floating next time
	bspc rule -a termite -o floating=on
	# open termite, resize it, and attach to the "dropdown" tmux session or create it if it doesn't exist
	termite -e "/bin/zsh -c 'sleep 0.01 && xdo move -x 0 -y 16 && xdo resize -w +800 && tmux attach-session -dt dropdown || tmuxinator dropdown'"
	# last resort example of resizing:
	# termite -e "/bin/zsh -c 'xdotool mousemove 520 60 && xdotool keydown super && sleep 0.2 && xdotool mousedown 3 && sleep 0.1 && xdotool mousemove 1320 60 && xdotool mouseup 3 && xdotool keyup super && tmux attach-session -dt dropdown || tmux new-session -s dropdown'"
fi

