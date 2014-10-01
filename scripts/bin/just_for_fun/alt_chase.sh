#!/bin/sh
# chase on same line

function back_space() {
for i in `seq 1 $1`;
do
	xdotool key BackSpace
	sleep 0.0001
done
}

function cat_chase_mouse() {
for i in `seq 1 $1`;
do
	if [ $i -gt 1 ];then
		xdotool type --delay 3 "    "
	fi
	xdotool type --delay 3 "~^3^ -@"
	sleep 0.2
	back_space 7
done
xdotool type "reverse!!!"
sleep 1
back_space 10
for i in `seq $1 -1 1`
do
	if [ $i -lt $1 ];then
		back_space 4
	fi
	xdotool type --delay 3 "@- ^"
	# this char needs more delay
	xdotool type --delay 15 "Ɛ"
	xdotool type --delay 3 "^~"
	sleep 0.2
	back_space 7
done
}

sleep 2
cat_chase_mouse 20
