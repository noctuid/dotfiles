# * Exports
# xdg
export XDG_MUSIC_DIR=$HOME/music
export XDG_DOWNLOAD_DIR=$HOME/move

# for bspwm reloading
export BSPWM_STATE=/tmp/bspwm-state.json
# panel
export PANEL_HEIGHT=15
export PANEL_FIFO=/tmp/panel-fifo

# many programs allow EDITOR to contain options, but some expect a path
# e.g. with zsh '$EDITOR file' will fail (though 'eval $EDITOR file' will work)
# a workaround for zsh is to put emacsclient -t "$@" in a script and use that
# having options works for sudoedit
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=
export PAGER=vimpager
export BROWSER=firefox
export PACMAN=powerpill
export SHELL=/bin/zsh

export SOURCE=$HOME/src
export NIXPKGS=$SOURCE/nixpkgs
# blog dir
export BLOG=$SOURCE/noctuid.github.io

# gpg-agent says to do this
GPG_TTY=$(tty)
export GPG_TTY

# stderr in red
# https://github.com/sickill/stderred
# https://github.com/NixOS/nix/issues/527 (unset LD_PRELOAD when installing nix)
if [[ -f "/usr/lib/libstderred.so" ]]; then
	export LD_PRELOAD="/usr/lib/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
fi

# for java
export CLASSPATH=$CLASSPATH:/usr/share/java/junit.jar:/usr/share/java/hamcrest-core.jar:.

# * Setting PATH
pathdirs="
# personal scripts
$HOME/bin
$HOME/bin/video
$HOME/bin/not_mine
$HOME/bin/dunst
$HOME/bin/bspwm
$HOME/bin/optirun
$HOME/.panel_scripts
# for vimus
$HOME/.cabal/bin
# for adb
/opt/android-sdk
# for octopress and other gems (e.g. for tmuxinator)
$HOME/.gem/ruby/2.1.0/bin
$HOME/.gem/ruby/2.2.0/bin
$HOME/.gem/ruby/2.3.0/bin
# more versions of emacs for testing
$HOME/.cask/bin
# roswell scripts (common lisp)
$HOME/.roswell/bin
"

while read -r dir; do
	if [[ -n $dir ]] && [[ ! ${dir:0:1} == '#' ]]; then
		PATH="$PATH":"$dir"
	fi
done <<< "$pathdirs"

# nix
if [[ -f ~/.nix-profile/etc/profile.d/nix.sh ]]; then
	cd ~/ && source .nix-profile/etc/profile.d/nix.sh
fi

# * Automatic Program Startup
# https://wiki.archlinux.org/index.php/Music_Player_Daemon#Autostart_on_tty_login
[[ ! -s ~/.mpd/pid ]] && mpd
if ! pidof mpdscribble > /dev/null; then
	# scrobble to last.fm/libre.fm
	mpdscribble &
fi
if ! pgrep devmon > /dev/null; then
	# auto mount usbs and such
	# using variables to prevent long unbreakable lines and to re-use text
	notify="notify-send --icon=media-removable"
	summary="'Devmon Notification'"
	# FIXME: %l and %d do not work on unmount with devmon version 1.1.8
	# https://github.com/IgnorantGuru/udevil/issues/67
	devmon --no-gui \
		   --exec-on-drive "$notify $summary \"Volume %l has been mounted to %d.\"" \
		   --exec-on-unmount "$notify $summary \"Volume %l has been unmounted from %d.\"" &
fi
emacsclient -e 0 &> /dev/null &
if ! tmux has-session -t dropdown 2> /dev/null; then
	tmux new-session -s dropdown -d
fi

# startx on login if tty1
if [[ $(tty) == /dev/tty1 ]]; then
	startx
elif [[ $- == *i* ]]; then
	# check if interactive
	# change caps from backspace to escape
	# TODO add actual keymap to correct location
	# loadkeys ~/.vt_caps_esc
	zsh
fi

# not posix compliant; check as bash with shellcheck
# /* Local Variables:
# /* sh-shell: bash
# /* End:
