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

# run stuff in background# {{{
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
# virtual modal interfaces
xchainkeys &
# auto mount usbs and such
devmon &
# preload &
# }}}
 
sxhkd &
# start window manager
# exec bspwm
# for reloading
# exec wm
case $1 in
	# xless) termite -e "/bin/zsh -c 'xless'";;
	# xless) termite -e "xwit -resize 1366 768";;
	xless) termite --geometry=1366x768 --exec="zsh -c '. ~/.zshrc && xless'";;
	# cinnamon) exec cinnamon;;
	# ratpoison) exec /usr/bin/ratpoison;;
	# vb) VirtualBox -startvm "Windows 7" -fullscreen;;
	*) exec wm;;
esac
