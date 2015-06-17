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

# TODO:
# .start a window instead of wm maximized without using geometry
# .slimlock for multiple xsessions? does that even make sense.. or maybe just disable tty switching?
#==============================
# General (for any X session) {{{
#==============================
# setup keyboard layout
setxkbmap us -variant colemak
xmodmap ~/.Xmodmap_wide
xmodmap ~/.Xmodmap
xcape -e 'Mode_switch=Escape;Alt_L=Return;Hyper_R=cedilla' &

# restore background image/settings
setroot --restore

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
# standby suspend and off; no difference for lcds apparently; blank screen after 5 minutes of inactivity
xset dpms 0 0 300

# disable mouse when typing
# i flag
# will disable for one second after last keypress; k ignore modifier keys (i.e. control left click) ; t will only disable scrolling and clicking
syndaemon -k -i 1 -d &

# set cursor look
xsetroot -cursor_name left_ptr &

# compositing.. transparency
compton --config ~/.compton.conf -b &
# conky with stats
conky &

# redshift; better options at this point than xflux
redshift -c ~/.config/redshift.conf &

# }}}
#==============================
# TTY Specific# {{{
#==============================
if [ $(tty) = "/dev/tty1" ]; then
	# japanese input
	export XMODIFIERS=@im=SCIM
	export GTK_IM_MODULE="scim"
	export QT_IM_MODULE="scim"
	scim -d

	# another dropdown
	guake &

	# sleep after 20 minutes activity
	xautolock -time 20 -locker "pm-suspend" -detectsleep &

	# notifications on mpd song changes
	# https://github.com/vehk/mpdnotify works well with mpdcron also
	mpd-notification -m ~/Music &

	# backup hotkey program
	xchainkeys &
	# hotkeys
	sxhkd &

	# start window manager
	case $1 in
		hlwm) exec herbstluftwm --locked;;
		bspwm) exec bspwm;;
		# vb) VirtualBox -startvm "Windows 7" -fullscreen;;
		*) exec wm;;
	esac
else
	# add battery info, etc. since no panel
	# start emacs maximized
	exec emacs -g 1366x768
fi
#  }}}
#==============================
