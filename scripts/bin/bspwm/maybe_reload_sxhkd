#!/bin/bash
# switch bindings when changing from empty to occupied desktop and vice versa

touch /tmp/bspwm-last-desktop-empty
previously_empty=$(< /tmp/bspwm-last-desktop-empty)
# helps when rapid close then open (e.g. tdrop's auto_hide):
sleep 0.2

nodes=$(bspc query --desktop --nodes)
if [[ -n "$nodes" ]]; then
	empty=false
else
	empty=true
fi
echo "$empty" > /tmp/bspwm-last-desktop-empty

if [[ $previously_empty != "$empty" ]]; then
	# since using mouse buttons as modifiers; see .sxhkdrc
	xdotool keyup control shift alt super
	# this is still hackish but probably less so than before
	if $empty; then
		ln -f -s ~/.config/sxhkd/empty_sxhkdrc ~/.config/sxhkd/extra_sxhkdrc
	else
		ln -f -s /dev/null ~/.config/sxhkd/extra_sxhkdrc
	fi
	# tell sxhkd to reload its config
	pkill -USR1 -x sxhkd
fi
