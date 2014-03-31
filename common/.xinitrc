#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# if [ -s ~/.Xmodmap ]; then
#     xmodmap ~/.Xmodmap
# fi

if [ -d /etc/X11/xinit/xinitrc.d ]; then
        for f in /etc/X11/xinit/xinitrc.d/*; do
                [ -x "$f" ] && . "$f"
        done
        unset f
fi

# rldkbd (keyboard remapping)
setxkbmap us -variant colemak
xmodmap ~/.Xmod_wide
xmodmap ~/.Xmodmap
# set background image
feh --bg-scale "/home/angelic_sedition/wallpaper/tumblr_static_makise.kurisu.full.798633.jpg" 

# fonts for bar and dzen2
xset +fp /usr/share/fonts/local
xset +fp ~/.fonts
xset fp rehash
# run stuff in background# {{{
# disable mouse when typing
# i flag will disable for two seconds after last keypress; k ignore modifier keys (i.e. control left click) ; t will only disable scrolling and clicking
syndaemon -k -i 2 -d &

# set cursor look 
xsetroot -cursor_name left_ptr &

# look sexy
# compositing.. transparency
compton --config ~/.compton.conf -b &
conky &
guake &
# changed Mode_switch to Control_L
xcape -e 'Mode_switch=Escape;Alt_L=Return;Hyper_R=cedilla' &

# redshift; better options at this point than xflux
redshift -c ~/.config/redshift.conf &
# xscreensaver for locking and power management
xrdb -merge ~/.Xresources ; /usr/bin/xscreensaver -no-splash &
xss-lock -- xscreensaver-command -lock &
# virtual modal interfaces
xchainkeys &
# }}}
 
sxhkd &
# start window manager
# exec bspwm
# for reloading
exec ~/bin/wm
#case $1 in
#	gnome) exec gnome-session;;
#	kde) exec startkde;;
#	xfce) exec startxfce4;;
#	awesome) exec awesome;;
#	cinnamon) exec cinnamon;;
#	ratpoison) exec /usr/bin/ratpoison;;
#	bspwm) sxhkd && exec bspwm;;
#	*) exec startxfce4;;
#esac
