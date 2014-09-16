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
imlibsetroot -s w -p c /home/angelic_sedition/wallpaper/anime/yosuga_no_sora/605007.jpg

# font stuff
xset +fp /usr/share/fonts/local
xset +fp ~/.fonts
xset fp rehash
# faster key repeat rate and shorter delay (default is 660 25); messing around with
xset r rate 300 35
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

# redshift; better options at this point than xflux
redshift -c ~/.config/redshift.conf &
# auto mount usbs and such
devmon &
# start mpd and mpdscribble
# MPD daemon start (if no other user instance exists)
# https://wiki.archlinux.org/index.php/Music_Player_Daemon
[ ! -s ~/.config/mpd/pid ] && mpd &
pidof mpdscribble >& /dev/null
if [ $? -ne 0 ]; then
 mpdscribble &
fi
# backup hotkey program
xchainkeys &
sxhkd &

# start window manager
case $1 in
	xless) termite --geometry=1366x768 --exec="zsh -c '. ~/.zshrc && xless'";;
	hlwm) exec herbstluftwm --locked;;
	# cinnamon) exec cinnamon;;
	# ratpoison) exec /usr/bin/ratpoison;;
	# vb) VirtualBox -startvm "Windows 7" -fullscreen;;
	*) exec wm;;
esac
