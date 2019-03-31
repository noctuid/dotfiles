#!/bin/bash
# for bspwm and maybe eventually herbstluftwm, i3, etc.
# so can setup bindings in one place for different programs and WMs

#
# Wm Detection
#

wm=$(< /tmp/window_manager)
if [[ -z $wm ]] || [[ $(tty) != "$(< /tmp/last_tty)" ]]; then
	wm=$(xprop -notype -id "$(xprop -root -notype | awk '$1=="_NET_SUPPORTING_WM_CHECK:"{print $5}')" -f _NET_WM_NAME 8u | awk -F "\"" '/WM_NAME/ {print $2}' | head -n 1)
	echo "$wm" > /tmp/window_manager
	echo "$tty" > /tmp/last_tty
fi

win_action() {
	if [[ $wm == bspwm ]]; then
		bspc_action "$@"
	elif [[ $wm == i3 ]]; then
		i3_action "$@"
	fi
}

#
# Different Wm/Environment Functions
#

bspc_state_tiled() {
	bspc query -d focused -T | jq .layout | grep -q "tiled"
}

bspc_action() {
case $1 in
	dsel)
		if [[ $2 =~ ^[0-9]+$ ]]; then
			bspc desktop -f ^"$2"
		else
			bspc desktop -f "$2"
		fi
		;;
	dcycle)
		bspc node @/ -C "$2"
		;;
	drotate)
		if [[ $2 == backward ]]; then
			bspc node @/ -R 270
		elif [[ $2 == forward ]]; then
			bspc node @/ -R 90
		fi
		;;
	dbalance)
		bspc node @/ -B
		;;
	wsel)
		if [[ $2 ==  left ]] || [[ $2 == down ]]; then
			if bspc_state_tiled; then
				if [[ $2 == down ]]; then
					# "cw"
					bspc node -f south || bspc node -f next.local
				else
					bspc node -f west
				fi
			else
				bspc node -f next.local
			fi
		elif [[ $2 == up ]] || [[ $2 == right ]]; then
			if bspc_state_tiled; then
				if [[ $2 == up ]]; then
					# "ccw"
					bspc node -f north || bspc node -f prev.local
				else
					bspc node -f east
				fi
			else
				bspc node -f prev.local
			fi
		else
			bspc node -f "$2"
		fi
		;;
	wmove)
		local dir
		case $2 in
			left) dir=west;;
			down) dir=south;;
			up) dir=north;;
			right) dir=east;;
			*) dir="$2"
		esac
		bspc node -s "$dir"
		;;
	wmove_tod)
		bspc node -d ^"$2"
		;;
	wresize)
		get_win_prop() { # wid prop
			xwininfo -id "$1" | awk "/$2/ {print \$2}"
		}
		get_mon_percent_prop() { # (width|height) percent
			bspc query -T -m | jq .rectangle."$1" | \
				awk "{printf(\"%.0f\", \$1/100 * $2)}"
		}
		local wid width height percent width_change height_change
		wid=$(xdotool getactivewindow)
		width=$(get_win_prop "$wid" Width)
		height=$(get_win_prop "$wid" Height)
		percent=5
		width_change=$(get_mon_percent_prop width $percent)
		height_change=$(get_mon_percent_prop height $percent)
		# using -z works for floating windows too
		# unfortunately can't use "||" with if they don't resize
		# so I have to manually check
		case $2 in
			left)
				# bspc node @east -r -40 || bspc node @west -r -40
				bspc node -z left -"$width_change" 0
				if [[ $width -eq $(get_win_prop "$wid" Width) ]]; then
					bspc node -z right -"$width_change" 0
				fi
				;;
			down)
				# bspc node @south -r +35 || bspc node @north -r +35
				bspc node -z bottom 0 +"$height_change"
				if [[ $height -eq $(get_win_prop "$wid" Height) ]]; then
					bspc node -z top 0 +"$height_change"
				fi
				;;
			up)
				# bspc node @north -r -35 || bspc node @south -r -35
				bspc node -z top 0 -"$height_change"
				if [[ $height -eq $(get_win_prop "$wid" Height) ]]; then
					bspc node -z bottom 0 -"$height_change"
				fi
				;;
			right)
				# bspc node @west -r +40 || bspc node @east -r +40
				bspc node -z right +"$width_change" 0
				if [[ $width -eq $(get_win_prop "$wid" Width) ]]; then
					bspc node -z left +"$width_change" 0
				fi
				;;
		esac
		;;
	sticky)
		bspc node -g sticky
		;;
	wtoggle)
		bspc node -t \~"$2"
		;;
	wclose)
		bspc node -c
		;;
	wkill)
		bspc node -k
		;;
	lsel)
		bspc desktop -l "$2"
		;;
	psel)
		case $2 in
			canceld) bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel ;;
			*) bspc node --presel-dir "$2" ;;
		esac
		;;
	gapc)
		bspc config -d focused window_gap $(($(bspc config -d focused window_gap) + $2 ))
		;;
	splitr)
		bspc node --presel-ratio "$2"
		;;
	hide_all)
		# only for current desktop
		bspc node @/ -g hidden
		;;
	reload)
		bspc wm --restart
		;;
	quit)
		bspc quit 1
		;;
	*)
		echo "not a valid action"
		exit 1
esac
}

i3_action() {
case $1 in
	dsel)
		if [[ $2 =~ ^[1-9]$ ]]; then
			i3-msg "workspace $2"
		fi
		;;
	wsel)
		i3-msg "focus $2"
		;;
	wmove)
		i3-msg "move $2"
		;;
	wmove_tod)
		i3-msg "move container to workspace $2"
		;;
	wresize)
		if [[ $2 == left ]]; then
			result=$(i3-msg "resize grow left 10 px or 10 ppt" | \
				awk -F ':' '/success/ {gsub("}]",""); print $2}')
			if ! $result; then
				i3-msg "resize shrink right 10 px or 10 ppt"
			fi
		elif [[ $2 == right ]]; then
			result=$(i3-msg "resize grow right 10 px or 10 ppt" | \
				awk -F ':' '/success/ {gsub("}]",""); print $2}')
			if ! $result; then
				i3-msg "resize shrink left 10 px or 10 ppt"
			fi
		elif [[ $2 == down ]]; then
			result=$(i3-msg "resize grow down 10 px or 10 ppt" | \
				awk -F ':' '/success/ {gsub("}]",""); print $2}')
			if ! $result; then
				i3-msg "resize shrink up 10 px or 10 ppt"
			fi
		elif [[ $2 = up ]]; then
			result=$(i3-msg "resize grow up 10 px or 10 ppt" | \
				awk -F ':' '/success/ {gsub("}]",""); print $2}')
			if ! $result; then
				i3-msg "resize shrink down 10 px or 10 ppt"
			fi
		fi
		;;
	wtoggle)
		if [[ $2 == fullscreen ]]; then
			i3-msg "fullscreen"
		elif [[ $2 == floating ]]; then
			i3-msg "floating toggle"
		fi
		;;
	wclose)
		i3-msg "kill"
		;;
	wkill)
		i3-msg "kill"
		;;
	lsel)
		if [[ $2 = next ]]; then
			i3-msg "layout toggle"
		fi
		;;
	psel)
		if [[ $2 == right ]]; then
			i3-msg "split h"
		elif [[ $2 == down ]]; then
			i3-msg "split v"
		fi
		;;
	reload)
		i3-msg "reload"
		;;
	quit)
		i3-msg "exit"
		;;
	*)
		echo "not a valid action"
		exit 1
esac
}

tmux_action() {
case $1 in
	# wm desktop is equivalent to tmux window
	dsel)
		if [[ $2 == last ]]; then
			tmux select-window -l
		else
			tmux select-window -t "$2"
		fi
		;;
	dbalance)
		tmux select-layout even-horizontal
		;;
	# pane selection
	wsel)
		pane_id1=$(tmux display-message -p '#D')
		# if no pane left, go to previous window
		if [[ $2 == left ]]; then
			tmux select-pane -L
			pane_id2=$(tmux display-message -p '#D')
			if [[ $pane_id1 == "$pane_id2" ]]; then
				tmux select-window -p
			fi
		# if no pane below, go left
		elif [[ $2 == down ]]; then
			tmux select-pane -D
			pane_id2=$(tmux display-message -p '#D')
			if [[ $pane_id1 == "$pane_id2" ]]; then
				tmux select-pane -L
			fi
		elif [[ $2 == up ]]; then
			tmux select-pane -U && \
			pane_id2=$(tmux display-message -p '#D')
			if [[ $pane_id1 == "$pane_id2" ]]; then
				tmux select-pane -R
			fi
		elif [[ $2 == right ]]; then
			tmux select-pane -R
			pane_id2=$(tmux display-message -p '#D')
			if [[ $pane_id1 == "$pane_id2" ]]; then
				tmux select-window -n
			fi
		elif [[ $2 == last ]]; then
			tmux select-pane -l
		fi
		;;
	wmove)
		if [[ $2 == prev ]]; then
			tmux swap-pane -U
		elif [[ $2 == next ]]; then
			tmux swap-pane -D
		fi
		;;
	wresize)
		if [[ $2 == left ]]; then
			tmux resize-pane -L 5
		elif  [[ $2 == down ]]; then
			tmux resize-pane -D 3
		elif  [[ $2 == up ]]; then
			tmux resize-pane -U 3
		elif  [[ $2 == right ]]; then
			tmux resize-pane -R 5
		fi
		;;
	wcreate)
		tmux new-window
		;;
	split_top)
		tmux split-window -h
		;;
	split_left)
		tmux split-window
		;;
	wclose)
		tmux kill-pane
		;;
	pbreak)
		tmux break-pane
		;;
	lsel)
		# 'monocle'
		if [[ $2 == next ]]; then
			tmux resize-pane -Z
		else
			tmux select-layout "$2"
		fi
		;;
	*)
		echo "not a valid action"
		exit 1
esac
}

prefix_in_program_maps() {
case $2 in
	# 'r' is prefix key
	# desktop/tmux win switching
	ra) $1 dsel 1;;
	rr) $1 dsel 2;;
	rs) $1 dsel 3;;
	rt) $1 dsel 4;;
	rd) $1 dsel 5;;
	rh) $1 dsel 6;;
	# down or left
	rn) $1 wsel down;;
	# up or right
	re) $1 wsel up;;
	# more desktop/tmux win switching
	ri) $1 dsel 9;;
	ro) $1 dsel 10;;
	rb) $1 dbalance;;
	# last desktop/tmux win
	rl) $1 dsel last;;
	# last win/tmux pane
	ru) $1 wsel last;;
	# move to desktop
	Ra) $1 wmove_tod 1;;
	Rr) $1 wmove_tod 2;;
	Rs) $1 wmove_tod 3;;
	Rt) $1 wmove_tod 4;;
	Rd) $1 wmove_tod 5;;
	Rh) $1 wmove_tod 6;;
	Rn) $1 wmove_tod 7;;
	Re) $1 wmove_tod 8;;
	Ri) $1 wmove_tod 9;;
	Ro) $1 wmove_tod 10;;
	# window movement
	rch) $1 wmove left;;
	rcn) $1 wmove down;;
	rce) $1 wmove up;;
	rci) $1 wmove right;;
	rcm) $1 wmove biggest;;
	# cycle move windows
	r,) $1 wmove prev;;
	r.) $1 wmove next;;
	# window/tmux pane resizing
	rmh) $1 wresize left;;
	rmn) $1 wresize down;;
	rme) $1 wresize up;;
	rmi) $1 wresize right;;
	# win/tmux pane close
	rx) $1 wclose;;
	# monocle toggle
	rk) $1 lsel next;;
	# for wm even if tmux
	ry) win_action wtoggle sticky;;
	rf) win_action wtoggle fullscreen;;
	# presel/tmux spltting
	r/|r\')
		if [[ $1 == win_action ]]; then
			 win_action psel right
		elif [[ $1 == tmux_action ]]; then
			tmux split-window -h
		fi
		;;
	r-)
		if [[ $1 == win_action ]]; then
			 win_action psel up
		elif [[ $1 == tmux_action ]]; then
			tmux split-window
		fi
		;;
	# (tmux only)
	rc) $1 wcreate;;
	rbang) $1 pbreak;;
	rv) $1 lsel main-vertical;;
	*)
		echo "not a valid action"
		exit 1
esac
}

#
# Main
#

if [[ $1 == mpv ]]; then
	# different bindings depending on whether mpv is running in terminal (audio)
	# i set title for gui mpv to 'mpv - ${filename}' in .mpv/config
	title=$(xtitle | awk '{ print $1 }')
	if [[ $title == mpv ]]; then
		prefix_in_program_maps win_action "${@:2}"
	else
		prefix_in_program_maps tmux_action "${@:2}"
	fi
elif [[ $1 == inpt ]]; then
	prefix_in_program_maps tmux_action "${@:2}"
elif [[ $1 == inpw ]]; then
	prefix_in_program_maps win_action "${@:2}"
elif [[ $1 == tmux ]]; then
	tmux_action "${@:2}"
else
	win_action "$@"
fi

# vim: set ft=sh:
