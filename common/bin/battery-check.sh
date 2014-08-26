#!/bin/sh
# https://bbs.archlinux.org/viewtopic.php?pid=1372810#p1372810
export XAUTHORITY=~/.Xauthority
export DISPLAY=":0"

battery_level=`acpi -b | cut -d ' ' -f 4 | grep -o '[0-9]*'`
if [ $battery_level -le 10 ];
then
	notify-send "Battery low" "Battery level is ${battery_level}%!"
fi

if [ $battery_level -le 5 ];
then
	# your custom actions here e.g.,
	# systemctl hibernate
	notify-send -u low "Battery low"
	notify-send -u low "Battery low"
	notify-send -u low "Battery low"
	notify-send -u critical "Battery low"
fi
