# still have bash as startup shell

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

source ~/.zsh/stow_functions.sh
