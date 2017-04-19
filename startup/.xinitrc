#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
if [ -d /etc/X11/xinit/xinitrc.d ]; then
        for f in /etc/X11/xinit/xinitrc.d/*; do
                [ -x "$f" ] && . "$f"
        done
        unset f
fi

#==============================
# General (for any X session) {{{
#==============================
xrdb -load ~/.Xresources

# setup keyboard layout (~/bin/rldkbd)
rldkbd

# restore background image/settings
if [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/setroot/.setroot-restore" ]; then
	setroot --restore
else
	setroot --bg-color "#00ff00"
fi

# font stuff
xset +fp /usr/share/fonts/local
xset +fp ~/.fonts
xset fp rehash
# faster key repeat rate and shorter delay (default is 660 25); messing around with
xset r rate 300 35
# die bell
xset -b
# energy star features on
xset +dpms

# disable system mouse acceleration
xset mouse 0 0
# not necesssary with xset I think:
# xinput list | awk -F "id=" '/slave *pointer/ && !/XTEST pointer/ {gsub(" *\\[.*",""); system("xinput --set-prop " $2 " \"Device Accel Velocity Scaling\" 1.00")}'

# disable mousepad when typing
# i- disable for x second(s) after last keypress
# k- ignore modifier keys (i.e. control left click)
# t- only disable scrolling and clicking
syndaemon -k -i 1 -d &

# set cursor look
xsetroot -cursor_name left_ptr &

# conky with stats
conky &

# redshift; better options at this point than xflux
redshift -c ~/.config/redshift.conf &

if ! tmux has-session -t dropdown 2> /dev/null; then
	tmux new-session -s dropdown -d
fi

# }}}
#==============================
# TTY Specific {{{
#==============================
if [ "$(tty)" = "/dev/tty1" ]; then
	# compositing.. transparency, fade-in, etc.
	# will mess things up if run with a DE like gnome or cinnamon
	compton --config ~/.compton.conf -b &

	# japanese input
	export XMODIFIERS=@im=ibus
	export GTK_IM_MODULE=ibus
	export QT_IM_MODULE=ibus
	ibus-daemon -d -x

	# sleep after 20 minutes of inactivity
	xautolock -time 20 -locker "pm-suspend" -detectsleep &

	# notification daemon
	dunst &
	# notifications on mpd song changes
	# https://github.com/vehk/mpdnotify works well with mpdcron also
	mpd-notification -m "${XDG_MUSIC_DIR:-$HOME/music}" &

	# backup hotkey program
	xchainkeys &
	# hotkeys
	sxhkd &

	# start window manager
	case $1 in
		hlwm) exec herbstluftwm --locked;;
		bspwm) exec bspwm;;
		i3) exec i3;;
		# vb) VirtualBox -startvm "Windows 7" -fullscreen;;
		*) exec wm;;
	esac
elif [ "$(tty)" = "/dev/tty2" ]; then
	sxhkd &
	exec i3
fi
# }}}
#==============================
