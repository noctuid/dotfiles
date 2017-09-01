# * Exports
# xdg
export XDG_MUSIC_DIR=$HOME/music
export XDG_DOWNLOAD_DIR=$HOME/move

# for bspwm reloading
export BSPWM_STATE=/tmp/bspwm-state.json

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

# Japanese input
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

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

# import environment variables for use with user units
# NOTE: will give failure message "Invalid environment assigments"
# all my important env variables show up in show-environment though
dbus-update-activation-environment --systemd --all
# with --enable, these will block login
# also want evironment variables available for emacs
# starting emacs (in any way) slows down loading after login a little
systemctl --user --no-block start emacs
systemctl --user --no-block start mpd

# * Startx on login if tty1
if [[ $(tty) == /dev/tty1 ]]; then
	startx
elif [[ $- == *i* ]]; then
	# check if interactive
	zsh
fi

# not posix compliant; check as bash with shellcheck
# /* Local Variables:
# /* sh-shell: bash
# /* End:
