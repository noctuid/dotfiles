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

# rldkbd (keyboard remapping)
setxkbmap us -variant colemak
xmodmap ~/.Xmodmap_wide
xmodmap ~/.Xmodmap
# set background image
setroot -z /home/angelic_sedition/wallpaper/_games/disgaea/287-disgaea-wallpaper-1680x1050-customity.jpg

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
# sleep after 20 minutes activity
xautolock -time 20 -locker "pm-suspend" -detectsleep &

# disable mouse when typing
# i flag will disable for one second after last keypress; k ignore modifier keys (i.e. control left click) ; t will only disable scrolling and clicking
syndaemon -k -i 1 -d &

# set cursor look 
xsetroot -cursor_name left_ptr &

# look sexy
# compositing.. transparency
compton --config ~/.compton.conf -b &
conky &
guake &

xcape -e 'Mode_switch=Escape;Alt_L=Return;Hyper_R=cedilla' &

# japanese input
export XMODIFIERS=@im=SCIM
export GTK_IM_MODULE="scim"
export QT_IM_MODULE="scim"
scim -d

# redshift; better options at this point than xflux
redshift -c ~/.config/redshift.conf &

# notifications on mpd song changes
# https://github.com/vehk/mpdnotify works well with mpdcron also
mpd-notification -m ~/Music &

# backup hotkey program
xchainkeys &
sxhkd &

# start window manager
case $1 in
	xless) termite --geometry=1366x768 --exec="zsh -c '. ~/.zshrc && xless'";;
	hlwm) exec herbstluftwm --locked;;
	bspwm) exec bspwm;;
	# cinnamon) exec cinnamon;;
	# ratpoison) exec /usr/bin/ratpoison;;
	# vb) VirtualBox -startvm "Windows 7" -fullscreen;;
	*) exec wm;;
esac
