#!/bin/sh
# Make Any Terminal a Dropdown Terminal (using tmux sessions) 
# (made by Lit Wakefield)
# Requirements: tmux, xprop, bspwm (or wm of choice), termite (or terminal of choice), xdotool (and/or xdo), and a hotkey program (i.e. default of DE or sxhkd, xchainkeys, xbindkeys, etc.)
# SHOULD WORK BY DEFAULT WITH BSPWM AND TERMITE (when bound to a hotkey with sxhkd)
# Limitations:
# .Script will need to be adapted if you are not using bspwm and termite
# .Uses tmux sessions to save state (meaning, it's killing the terminal, not just hiding it and bringing it back)
# .I've never tested opening something with a sxhkd and a floating window manager (resizing may be more difficult)
#+
# Changelog
# [3.24.13] modified to move to 0 0 before resizing because now bspwm opens floating windows centered

#+
# Instructions for Adaptation (if you don't understand the script):#{{{
# .first consider using yeahconsole or an existing wmctrl, scratchpad, or WM dependent script for dropdown (I know awesome has this.. though I could never get any working); urxvt has the -pe kuake option
# .replace "Termite" with the terminal window class of your choice (run xprop and click on the terminal; enter the second name by WM_CLASS (should have caps); if there isn't a second name (never seen there not be), you will have to adapt win_name
# .also if you are not using termite, replace termite -e with the command for your terminal to open and run something in it; might have to change this around a bit; keep the 'sleep.. xdo resize.." but maybe the /bin/zsh.. is not necessary; I think the -e flag is usually the same as this (i.e. with xterm)
# .if you are not using bspwm, replace "bspc window -c" with the command or hotkey to kill/close a window (i.e. xsendkey Alt+F4) 
# .also, if you are not using bspwm, remove "bspc rule..."
# .also, if you are not using bspwm, xdo resize may not work (I have not tested); if it doesn't, see the two options below; using xdotool was the first thing I tried (and it works about 95% of the time with enough sleep time and a quick hotkeypress); note that you will have to fake your keybindings for resizing windows
# .it should work now; chmod +x the script and bind "sh /Path/to/scriptname.sh" to a hotkey with sxhkd (or xbindkeys, xchainkeys, etc.)
# #}}}

# If you want a fullscreen toggle, you can add a separate binding in your sxhkdrc: i.e:#{{{
# if you have a bar you don't want to cover:
# F10
#   desktop -l next && bspc window -t floating
# above won't work if you're already in monocle; can do desktop -l monocle if don't care about returning to T
# alternatively for actual fullscreen:
# F10
#   bspc window -t fullscreen
# to decide to start fullscreen or halfscreen, take out the "bspc rule..." part and put it in the keybinding before running the script "sh /path/to/my_dropdown.sh" except with fullscreen=on

# for other WMs, try xdo resize -h +(length to the bottom of your screen) ; for toggle can, for example, do if statement based on window size to determine whether to do a + or - length
# if that doesn't work, look at the wmctrl and xdotool solutions below
#}}}


# Start of actual script
# modified from this to get window class https://bbs.archlinux.org/viewtopic.php?id=156660
win_name=$(xprop -id $(xprop -root 32x '\t$0' _NET_ACTIVE_WINDOW | cut -f 2) WM_CLASS | cut -d '=' -f 2 | cut -d ',' -f 2 | sed 's/[\"\ ]//g')

# to use with a different terminal; change to output of above (basically capitalized wm class name when you run xprop)
if [ "$win_name" == "Termite" ]; then
	# if the current window is termite, kill it (tmux session remains)
	bspc window -c # or could do xsendkey (keycombo to kill the window with your window manager or DE.. i.e. xsendkey Alt+F4)
else
	# if termite is not open, open it
	bspc rule -a termite -o floating=on
	# open termite, resize it/move it how you want (best way; fast), and attach to the "dropdown" tmux session or create it if it doesn't exist; without the sleep, it seems to not work about 7% of the time
	termite -e "/bin/zsh -c 'sleep 0.01 && xdo move -x 0 -y 0 && xdo resize -w +800 && tmux attach-session -dt dropdown || tmuxinator dropdown'"

	# I have only tested xdo resize with bspwm; if it doesn't work try wmctrl as detailed here: https://bbs.archlinux.org/viewtopic.php?pid=1351807#p1351807

	# Last Resort if the above two don't work for resizing, try something like below with xdotool (will have to adjust lengths depending on screen size; to get mouse location: "xdotool getmouselocation"); also, will have to change to your resizing combo (i.e. if just click and drag, will have to get precise location)
	# Downsides of below: slower because requires sleeping to work consistently; you can see it resize

	# termite -e "/bin/zsh -c 'xdotool mousemove 520 60 && xdotool keydown super && sleep 0.2 && xdotool mousedown 3 && sleep 0.1 && xdotool mousemove 1320 60 && xdotool mouseup 3 && xdotool keyup super && tmux attach-session -dt dropdown || tmux new-session -s dropdown'"
	# xdotool mousemove 520 60 && sleep 0.2 && xdotool keydown super && xdotool mousedown 3 && sleep 0.2 && xdotool mousemove 870 60 && xdotool mouseup 3 && xdotool keyup super
	# termite -e "/bin/zsh -c 'tmux attach-session -dt dropdown || tmux new-session -s dropdown'"
fi

