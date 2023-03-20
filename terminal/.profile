# * Exports
# xdg
export XDG_MUSIC_DIR=$HOME/music
export XDG_DOWNLOAD_DIR=$HOME/move

# many programs allow EDITOR to contain options, but some expect a path
# e.g. with zsh '$EDITOR file' will fail (though 'eval $EDITOR file' will work)
# a workaround for zsh is to put emacsclient -t "$@" in a script and use that
# having options works for sudoedit
export ALTERNATE_EDITOR=
# using no flags actually seems preferable; use GUI with X running and also
# works in framebuffer (emacsclient -t doesn't work in framebuffer terminal)
export EDITOR=emacsclient
# doesn't work for python (pyright) so commented for now
# export LSP_USE_PLISTS=true
export PAGER=vimpager
# export BROWSER=qutebrowser
export BROWSER=firefox
# keep SHELL as bash (e.g. Emacs tries to execute commands with zsh otherwise)
# export SHELL=/bin/zsh

export SOURCE=$HOME/src

# fix locale issues for packages installed with nix (e.g. rofi and xtitle)
# https://github.com/NixOS/nixpkgs/issues/6878
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
# https://github.com/NixOS/nixpkgs/issues/34603#issuecomment-1242546619
# fix cursor size when cursor on nix GUI applications; doesn't fix for root
# window when herbstluftwm installed through nix
export XCURSOR_PATH=$HOME/.local/share/icons:$HOME/.nix-profile/share/icons:/usr/share/icons

export GUIX_PROFILE=$HOME/.guix-profile
# prevent guix from breaking a lot of things (e.g. finding icons in firefox) by
# explicitly setting
export XDG_DATA_DIRS="$GUIX_PROFILE"/share:/usr/local/share:/usr/share

# blog dir
export BLOG=$SOURCE/blog/pir-hana

# so pinentry knows the current tty
# shellcheck disable=SC2155
export GPG_TTY=$(tty)
# use gpg for ssh agent
# export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"
# need to run if trying to use agent on different terminal/display from where it
# was started:
# gpg-connect-agent updatestartuptty /bye >/dev/null

# Japanese input
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# TODO stderr in red
# https://github.com/sickill/stderred
# https://github.com/NixOS/nix/issues/527 (unset LD_PRELOAD when installing nix)
# if [[ -f "/usr/lib/libstderred.so" ]]; then
# 	export LD_PRELOAD="/usr/lib/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
# fi

# for java
export CLASSPATH=$CLASSPATH:/usr/share/java/junit.jar:/usr/share/java/hamcrest-core.jar:.

# for python
export WORKON_HOME=~/.virtualenvs
# put virtualenvs in current directory
export HATCH_ENV_TYPE_VIRTUAL_PATH=.venv

# temporary for testing
export PATH="$HOME/.tmuxifier/bin:$PATH"

# * Setting PATH
pathdirs="
# personal scripts
$HOME/bin
$HOME/ebin
$HOME/bin/dunst
$HOME/bin/net
$HOME/bin/not_mine
$HOME/bin/media
$HOME/bin/music
$HOME/bin/video
$HOME/bin/wm
# for vimus
$HOME/.cabal/bin
# for adb
/opt/android-sdk
# for octopress and other gems (e.g. for tmuxinator)
$HOME/.gem/ruby/2.1.0/bin
$HOME/.gem/ruby/2.2.0/bin
$HOME/.gem/ruby/2.3.0/bin
$HOME/.gem/ruby/2.4.0/bin
$HOME/go/bin
# more versions of emacs for testing
$HOME/.cask/bin
# roswell scripts (common lisp)
$HOME/.roswell/bin
$HOME/.evm/bin
$HOME/dotfiles/root
# some pip packages install executables here (e.g. pywalfox)
$HOME/.local/bin
"

while read -r dir; do
	if [[ -n $dir ]] && [[ ! ${dir:0:1} == '#' ]]; then
		PATH="$PATH":"$dir"
	fi
done <<< "$pathdirs"

# import environment variables for use with user units
# NOTE: will give failure message "Invalid environment assigments"
# some important env variables show up in show-environment though
# dbus-update-activation-environment --systemd --all

# with --enable, user services can block login
# also want environment variables available for emacs
# starting emacs with systemd (in any way) slows down loading after login
# systemctl --user --no-block start emacs
# systemctl --user --no-block start mpd

[[ ! -s ~/.mpd/pid ]] && nohup mpd &> /tmp/mpd.log &

# * Startx on login if tty1
if [[ $(tty) == /dev/tty1 ]]; then
	startx
elif [[ $- == *i* ]]; then
	# check if interactive
	zsh
fi

# not posix compliant; check as bash with shellcheck
# Local Variables:
# sh-shell: bash
# End:
