#!/bin/sh

file_name=$(xclip -o -selection clipboard)

# don't open the gui; just run with your magical hidden libraries that can't be installed with pip; thank god for this
if [ "$1" == "exec" ]; then
	# myro
	bspc window --presel right && ~/_school/Calico/StartCalico --exec "$file_name"
elif [ "$1" == "exec_graphics" ]; then
	# going to be creating a window whose size can be specified; float it
	bspc rule -a Calico -o floating=on && ~/_school/Calico/StartCalico --graphics --exec "$file_name"

elif [ "$1" == "mirror_line" ]; then
	# because initializing rfcomm0 every time want to run a script is slow; FUCK you calico
	# position of shell
	xdotool mousemove 140 128 click 1
	# 133 108
	bspc window -f left && xsendkey Control+v && xdotool key Return && bspc window -f right && xdotool key Escape

elif [ "$1" == "run_focus_shell" ]; then
	bspc window -r 0.2
	bspc window --presel left && ~/_school/Calico/StartCalico
	sleep 7 && xdotool mousemove 133 108 click 1 && bspc window -f right
	
# hopefully will never have to use these:

elif [ "$1" == "open_run" ]; then
	# open and run in calico gui
	xdotool mousemove 857 36 click 1 && sleep 0.1
	xdotool mousemove_relative -- 0 +30 && sleep 0.1
	xdotool click 1 && sleep 0.4
	# wait for open file prompt to opene
	xdotool key Down
	xdotool key Down
	xdotool key Left
	xdotool key Down
	sleep 0.2
	# open from clipboard
	xdotool key Control+v
	sleep 0.1
	xdotool key Return
	# sleep 0.2
	# xdotool mousemove 1035 728 click 1
	sleep 0.4
	xdotool key Escape
	sleep 0.2
	# assumes vim window is active
	bspc window -f right
	xdotool mousemove 1340 208 click 1
	xdotool mousemove 1191 72 click 1
elif [ "$1" == "run" ]; then
	# run from in calico
	xdotool mousemove 1340 208 click 1
	xdotool mousemove 1191 72 click 1
elif [ "$1" == "start" ]; then
	bspc window --presel right && ~/_school/Calico/StartCalico && sleep 6
	# now get me back to vim..
	xdotool mousemove 1026 388 click 1
	xdotool mousemove 261 543 click 1 && sleep 0.2
	xdotool key Escape
fi
