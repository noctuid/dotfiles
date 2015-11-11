export XDG_CONFIG_HOME="$HOME/.config"

# export for bspwm reloading
export BSPWM_TREE=/tmp/bspwm.tree
export BSPWM_HISTORY=/tmp/bspwm.history
export BSPWM_STACK=/tmp/bspwm.stack
# panel
export PANEL_HEIGHT=14
export PANEL_FIFO=/tmp/panel-fifo

export EDITOR="emacsclient -t -a """
export PAGER=vimpager
export BROWSER=firefox
export PACMAN=powerpill
export SHELL=/bin/zsh

# stderr in red
# https://github.com/sickill/stderred
if [ -f "/usr/lib/libstderred.so" ]; then
	export LD_PRELOAD="/usr/lib/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
fi

# only load ~/.config/ranger/rc.conf
export RANGER_LOAD_DEFAULT_RC=FALSE

# blog dir
export BLOG=$HOME/dev/blog/noctuid.github.io


# add to PATH
pathdirs="
# personal scripts
~/bin
~/.local/bin
~/bin/mpv
~/bin/not_mine
# for writing related script
~/bin/writing
~/.panel_scripts
# for vimus
~/.cabal/bin
# for adb
/opt/android-sdk
# for octopress and other gems (e.g. for tmuxinator)
~/.gem/ruby/2.1.0/bin
~/.gem/ruby/2.2.0/bin
"

while read -r dir; do
	# http://serverfault.com/a/252406
	# posix check if starts with #
	if [ "$dir" != "" ] && [ ! "${dir%${dir#?}}"x = '#x' ]; then
		# eval so expands tilde
		eval PATH="$PATH":"$dir"
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
if ! pidof devmon; then
	# auto mount usbs and such
	devmon &
fi

# startx on login if tty1
if [ "$(tty)" == "/dev/tty1" ]; then
	startx
else
	# change caps from backspace to escape
	loadkeys ~/.vt_caps_esc
	zsh
fi
