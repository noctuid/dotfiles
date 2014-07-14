#
# ~/.bashrc
#

# export#{{{
# export for bspwm reloading
export BSPWM_TREE=/tmp/bspwm.tree
export BSPWM_HISTORY=/tmp/bspwm.history
export BSPWM_STACK=/tmp/bspwm.stack

export PANEL_FIFO="/tmp/panel-fifo"
export PANEL_HEIGHT=20
export PANEL_FIFO=/tmp/panel-fifo
export PANEL2_FIFO=/tmp/panel-fifo2
export PATH=$PATH:~/.config/bspwm/panel
export PATH=$PATH:~/bin

export BROWSER="firefox"

# For gnome keyring
export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK
#}}}

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

alias restow='cd ~/dotfiles ; stow -Rvt ~/ common terminal private vim'
alias rld='echo "The Matrix has been reloaded" && . ~/.bashrc'

function xverymuchless() {
	X=$( pidof X )
	if [ ${#X} -gt 0 ]
	then
		# http://raspberrypi.stackexchange.com/questions/5494/check-if-x-is-already-running
		echo "X has already been started"
	else
		devmon &
		zsh -c "tmux attach-session -dt xless || tmuxinator xless"
	fi
}
