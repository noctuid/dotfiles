export XDG_CONFIG_HOME="$HOME/.config"
export XDG_MUSIC_DIR="$HOME/music"

# export for bspwm reloading
export BSPWM_STATE=/tmp/bspwm-state.json
# panel
export PANEL_HEIGHT=15
export PANEL_FIFO=/tmp/panel-fifo

# many programs allow EDITOR to contain options, but some expect a path
# e.g. with zsh '$EDITOR file' will fail (though 'eval $EDITOR file' will work)
export EDITOR="emacsclient -t -a \"\""
export PAGER=vimpager
export BROWSER=firefox
export PACMAN=powerpill
export SHELL=/bin/zsh

# stderr in red
# https://github.com/sickill/stderred
if [[ -f "/usr/lib/libstderred.so" ]]; then
	export LD_PRELOAD="/usr/lib/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
fi

export SOURCE=$HOME/src
# blog dir
export BLOG=$SOURCE/noctuid.github.io


# add to PATH
pathdirs="
# personal scripts
$HOME/bin
$HOME/bin/mpv
$HOME/bin/not_mine
$HOME/bin/dunst
$HOME/bin/bspwm
$HOME/.panel_scripts
# for vimus
$HOME/.cabal/bin
# for adb
/opt/android-sdk
# for octopress and other gems (e.g. for tmuxinator)
$HOME/.gem/ruby/2.1.0/bin
$HOME/.gem/ruby/2.2.0/bin
"

while read -r dir; do
	if [[ -n $dir ]] && [[ ! ${dir:0:1} == '#' ]]; then
		PATH="$PATH":"$dir"
	fi
done <<< "$pathdirs"

# start mpd, mpdscribble, and devmon (if not already running)
# https://wiki.archlinux.org/index.php/Music_Player_Daemon#Autostart_on_tty_login
[[ ! -s ~/.mpd/pid ]] && mpd
if ! pidof mpdscribble; then
	# scrobble to last.fm/libre.fm
	mpdscribble &
fi
if ! pidof devmon; then
	# auto mount usbs and such
	# using variables to prevent long unbreakable lines and to re-use text
	notify="notify-send --icon=media-removable"
	summary="'Devmon Notification'"
	# FIXME: %l and %d do not work on unmount with devmon version 1.1.8
	devmon --no-gui \
		--exec-on-drive "$notify $summary \"Volume %l has been mounted to %d.\"" \
		--exec-on-unmount "$notify $summary \"Volume %l has been unmounted from %d.\"" &
fi
if ! pgrep emacs; then
	emacs --daemon &
fi

# startx on login if tty1
if [[ "$(tty)" == "/dev/tty1" ]]; then
	startx
else
	# change caps from backspace to escape
	loadkeys ~/.vt_caps_esc
	zsh
fi
