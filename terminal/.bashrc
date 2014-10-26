#
# ~/.bashrc
#
# still have bash as startup shell

alias restow='cd ~/dotfiles ; stow -Rvt ~/ common terminal private vim'
alias rld='echo "The Matrix has been reloaded" && . ~/.bashrc'

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
