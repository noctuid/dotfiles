# export for bspwm reloading
export BSPWM_TREE=/tmp/bspwm.tree
export BSPWM_HISTORY=/tmp/bspwm.history
export BSPWM_STACK=/tmp/bspwm.stack
# panel
export PANEL_HEIGHT=14
export PANEL_FIFO=/tmp/panel-fifo

# only load ~/.config/ranger/rc.conf
export RANGER_LOAD_DEFAULT_RC=FALSE

# add to PATH
pathdirs="
# personal scripts
~/bin
~/.local/bin
~/bin/mpv
~/.config/bspwm/panel
# for scut
~/bin/not_mine
# for vimus
~/.cabal/bin
# for adb
/opt/android-sdk
"

while read -r dir; do
	# http://serverfault.com/a/252406
	# posix check if starts with #
	if [ "$dir" != "" ] && [ ! "${dir%${dir#?}}"x = '#x' ]; then
		# eval so expands tilde
		eval PATH=$PATH:$dir
	fi
done <<< "$pathdirs"

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
