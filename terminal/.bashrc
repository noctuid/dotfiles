# still have bash as startup shell

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

export HISTCONTROL=ignoreboth:erasedups

# direnv
if hash direnv 2> /dev/null; then
	eval "$(direnv hook bash)"
fi
