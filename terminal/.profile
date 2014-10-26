# export for bspwm reloading
export BSPWM_TREE=/tmp/bspwm.tree
export BSPWM_HISTORY=/tmp/bspwm.history
export BSPWM_STACK=/tmp/bspwm.stack
# panel
export PANEL_HEIGHT=14
export PANEL_FIFO=/tmp/panel-fifo
export PATH=$PATH:~/.config/bspwm/panel
# scripts
export PATH=$PATH:~/bin

# start mpd, mpdscribble, and devmon (if not already running)
# https://wiki.archlinux.org/index.php/Music_Player_Daemon#Autostart_on_tty_login
[ ! -s ~/.mpd/pid ] && mpd
pidof mpdscribble >& /dev/null
if [ $? -ne 0 ]; then
	# scrobble to last.fm/libre.fm
	mpdscribble &
fi
if ! pgrep devmon; then
	# auto mount usbs and such
	devmon &
fi
