#to reload .zshrc :
# . ~/.zshrc which I made alias for: rld
# set as default shell by changing /etc/zsh/zprofile ... /etc/profile or just do chsh -s /bin/zsh
# TODO:
# .maybe stop using gvfs-mount and start mounting to media.. simple global search and replace with confirmation for /run/media..etc
# .stop using usb as main

# on startup
echo "On startup, mount usb, connect, reload time (no more), and powerdown".

# Sources:# {{{
# Some settings or stuff got from here:
# otherwise I probably got it from here (have tried to put links by specific functions):
#https://wiki.archlinux.org/index.php/Zsh
#http://zsh.sourceforge.net/Doc/Release/Zsh-Modules.html
#sources- https://github.com/Adalan/dotfiles/blob/master/zsh/setopt.zsh#L33
#http://www.bash2zsh.com/zsh_refcard/refcard.pdf
#http://stackoverflow.com/questions/171563/whats-in-your-zshrc
#https://github.com/Adalan/dotfiles/blob/master/zsh/functions.zsh
#https://wiki.gentoo.org/wiki/Zsh/HOWTO#
# https://github.com/phallus/arch-files/blob/master/config/.zshrc
#}}}

# dissolve {{{
# Aliases# {{{
# alias gd='git diff'
# # Will return the current branch name
# # Usage example: git pull origin $(current_branch)
# #
# function current_branch() {
#   ref=$(git symbolic-ref HEAD 2> /dev/null) || \
#   ref=$(git rev-parse --short HEAD 2> /dev/null) || return
#   echo ${ref#refs/heads/}
# }
# 
# function current_repository() {
#   ref=$(git symbolic-ref HEAD 2> /dev/null) || \
#   ref=$(git rev-parse --short HEAD 2> /dev/null) || return
#   echo $(git remote -v | cut -d':' -f 2)
# }
# 
# # these aliases take advantage of the previous function
# alias ggpull='git pull origin $(current_branch)'
# compdef ggpull=git
# alias ggpur='git pull --rebase origin $(current_branch)'
# compdef ggpur=git
# alias ggpush='git push origin $(current_branch)'
# compdef ggpush=git
# alias ggpnp='git pull origin $(current_branch) && git push origin $(current_branch)'
# compdef ggpnp=git
# 
# # Pretty log messages
# function _git_log_prettily(){
#   if ! [ -z $1 ]; then
#     git log --pretty=$1
#   fi
# }
# alias glp="_git_log_prettily"
# compdef _git glp=git-log
# 
# # Work In Progress (wip)
# # These features allow to pause a branch development and switch to another one (wip)
# # When you want to go back to work, just unwip it
# #
# # This function return a warning if the current branch is a wip
# function work_in_progress() {
#   if $(git log -n 1 2>/dev/null | grep -q -c "\-\-wip\-\-"); then
#     echo "WIP!!"
#   fi
# }
# # these alias commit and uncomit wip branches
# alias gwip='git add -A; git ls-files --deleted -z | xargs -0 git rm; git commit -m "--wip--"'
# alias gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
# 
# # these alias ignore changes to file
# alias gignore='git update-index --assume-unchanged'
# alias gunignore='git update-index --no-assume-unchanged'
# # list temporarily ignored files
# alias gignored='git ls-files -v | grep "^[[:lower:]]"'
# }}}

# }}}

#Defaults# {{{
export EDITOR=gvim
export PAGER=less
# verbose and smart case search
export LESS="-iM"
# less > more.. and most doesn't wrap?
# consider https://github.com/rkitover/vimpager at some point; less works great so probably uneccesary

# export BROWSER=dwb
export BROWSER=firefox

# export XDG_CONFIG_HOME=~/.config

# for bar for bspwm
export PANEL_FIFO="/tmp/panel-fifo"
export PANEL_HEIGHT=20
# export BSPWM_TREE=/tmp/bspwm.tree
# export BSPWM_HISTORY=/tmp/bspwm.history
# export BSPWM_STACK=/tmp/bspwm.stack
# }}}

# add to path# {{{
# #add ~/bin to $path for any scripts
export PATH=$PATH:~/bin
export PATH=$PATH:~/bin/artget.py
export PATH=$PATH:~/.config/bspwm/panel

# from https://github.com/windelicato/dotfiles/blob/master/.zshrc
# pathdirs=(
#     ~/scripts
# )
# for dir in $pathdirs; do
#     if [ -d $dir ]; then
#         path+=$dir
#     fi
# done

# so can use adb
export PATH=$PATH:/opt/android-sdk
# }}}
#========================================
# Appearance {{{
##========================================
# Color Manpages
# similar to things like colored-man
export LESS_TERMCAP_mb=$'\E[01;31m'             # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'             # begin bold
export LESS_TERMCAP_me=$'\E[0m'                 # end mode
export LESS_TERMCAP_se=$'\E[0m'                 # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m'          # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'                 # end underline
export LESS_TERMCAP_us=$'\E[01;32m'             # begin underline

# Prompt/Theme (my custom)
# see antigen

# alt syntax highlighting# {{{
# github.com/phallus/arch-files/blob/master/config/.zshrc
# ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
# ZSH_HIGHLIGHT_STYLES[globbing]='fg=yellow,none'
# ZSH_HIGHLIGHT_STYLES[path]='fg=red'
# ZSH_HIGHLIGHT_STYLES[function]='fg=blue'
# ZSH_HIGHLIGHT_STYLES[builtin]='fg=blue'
# ZSH_HIGHLIGHT_STYLES[command]='fg=blue'
# ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=blue'
# ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red,bold'
# ZSH_HIGHLIGHT_STYLES[default]='fg=black'
# ZSH_HIGHLIGHT_STYLES[alias]='fg=cyan,bold'
# ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=magenta,none'
# ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=cyan,bold'
# ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=red'
# ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=magenta,bold'
# ZSH_HIGHLIGHT_STYLES[single-hyphen-opton]='fg=red'
# }}}
#========== Prompt ==========
# using custom with antigen (see antigen)

# show vi mode
# THIS WORKS WITH AUTO SUGGESTIONS: http://hamberg.no/erlend/posts/2010-10-17-show-current-vi-mode-in-zsh-prompt.html
# see my custom theme in /terminal/.zsh/themes
# 
# http://unix.stackexchange.com/questions/547/make-my-zsh-prompt-show-mode-in-vi-mode
# http://stackoverflow.com/questions/3622943/zsh-vi-mode-status-line/3791786#3791786
# this doesn't work with autosuggestions# {{{
# show vim status; widget for normal v insert (http://zshwiki.org/home/examples/zlewidgets)
# zle auto suggest kills this
# zle-keymap-select() {
#     RPS1="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
#     RPS2=$RPS1
#     zle reset-prompt
# }
# zle -N zle-line-init
# zle -N zle-keymap-select

# for prompt below: still doesn't work with autosuggest
# terminfo_down_sc=$terminfo[cud1]$terminfo[cuu1]$terminfo[sc]$terminfo[cud1]
# function zle-line-init zle-keymap-select {
#     PS1_2="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
# 	PS1='%{$terminfo_down_sc$PS1_2$terminfo[rc]%} $PROOMPT' 
#     zle reset-prompt
# }
# preexec () { print -rn -- $terminfo[el]; }
# }}}

#00C2D4 (color using in guake for text)
#see Xdefaults for colours
#}}}
#========================================
# Plugins {{{
#========================================

# Antigen  {{{
# takes care of installation n sourcing
# installed from aur
source /usr/share/zsh/scripts/antigen/antigen.zsh
# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
# antigen bundle git
# antigen bundle heroku
# antigen bundle pip
# antigen bundle lein
antigen bundle command-not-found

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# up and down will go through history based on what you have typed
antigen bundle zsh-users/zsh-history-substring-search
# bind UP and DOWN arrow keys (on caps layer)
bindkey -M viins '^[[A' history-substring-search-up
bindkey -M viins '^[[B' history-substring-search-down
# HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND=none

# antigen bundle tarruda/zsh-autosuggestions
# antigen messes up when loading zsh autosuggesions; install manually

# Load the theme (frome omz/themes folder)
antigen theme fox-mod

# Tell antigen that you're done.
antigen apply
# }}}

#omz plugins:
# lol (aliases)
#look into:colorize
#may be usefel in future: celery, bundler, cake (coffeescript build tool), coffee (completion for coffeescript), nanoc, rake, per-directory-history,etc.
#chruby (simple ruby version manager), gem (completieon), zeus (aliases for zeus; speedier rails), rails, etc.
#plyint and autopep8 (aliases and completion for python, pylint, pep8)
# git (aliases and functions), gitfast (git completion); other git completion ones

# looked into and not interested in:
# archlinux (a bunch of pacman aliases)
#
#}}}
#========================================
#Options http://linux.die.net/man/1/zshoptions# {{{
#========================================
#===Looking into===
# ===== Scripts and Functions
# perform implicit tees or cats when multiple redirections are attempted; default for zsh; not totally familiar with redirection
#http://zsh.sourceforge.net/Doc/Release/Redirection.html
#allows piping to multiple inputs
#setopt multios

#http://zsh.sourceforge.net/Doc/Release/Expansion.html#Filename-Expansion
#normally cd ~1 will go to 1 in dir stack; if argument for cd is not a dir and does not try to start with a / will try to put a ~ to see if works; I thought this worked but no success
#http://zsh.sourceforge.net/Intro/intro_16.html
setopt cdablevarS

# no_clobber; no_case_glob; numeric_glob_sort; no_flow_control; ignore_eof; rc_expand_param

#================General===================
#tab autocomplete.. shows available commands, contents of directory, etc. tab again then can scroll through
#up gives previous commands
#sexy vi mode

#==Saftey# {{{
# 10 second wait if you do something that will delete everything
# so can control c to cancel I assume
setopt RM_STAR_WAIT

#allow comments with ; why I put this in safety? Like I fucking know
setopt interactive_comments

#no beeps
setopt no_beep
# }}}
# ===== Prompt# {{{
# Enable parameter expansion, command substitution, and arithmetic expansion in the prompt
#without, theme doesn't work.. just get $fg.. crap
setopt prompt_subst
# only show the rprompt on the current prompt
# unset will keep showing "insert" on each line as the rprompt for mode
setopt transient_rprompt
# }}}
# ========== Correction ==========# {{{
#spell check commands and offer correction (pdw > pwd)
setopt correct
#spell check arguments
setopt correctall
# }}}
# ========== Completion ==========# {{{
# complete aliases (default I think)
setopt completealiases

# When completing from the middle of a word, move the cursor to the end of the word; haven't noticed staying in middle of word even when unset; confusion
unsetopt always_to_end

# do not autoselect the first completion entry
unsetopt menu_complete

# show completion menu on successive tab press. needs unsetop menu_complete to work
setopt auto_menu

# will show name for directory if you have it set; ex if have DS=~/Desktop (ex with '%~' in prompt sequence), it will show DS instead of desktop if you cd into it; see custom names
# Now no matter what dir you're in, you can "cd ~D(tab)" for example to get cd ~DS/ (see custom dir names) and go there (also have auto cd)
setopt auto_name_dirs

#default; will put / instead of space when autocomplete for param (ex ~D(tab) puts ~D/)
setopt auto_param_slash

#default; lists choiches on ambiguous completion; won't go to first item (except with wildcard and glob_complete)
setopt auto_list

# Allow completion from within a word/phrase
# ex: completes to Desktop/ from Dktop with cursor before k)
setopt complete_in_word
# }}}
# ========== Globbing ==========# {{{
# treat #, ~, and ^ as part of patterns for filename generation; ex ^ negates following pattern (ls -d ^*.c)
#ls *.png~Selection_005.png now will exclude that file frome results
#http://www.refining-linux.org/archives/37/ZSH-Gem-2-Extended-globbing-and-expansion/
#http://zsh.sourceforge.net/Doc/Release/Expansion.html
#http://www.linuxjournal.com/content/bash-extended-globbing
# @ for symbolic links; ** expand all subfolders; http://pastebin.com/QGu9hmtN

setopt extended_glob

#I don't know if I want this unset or set; if unset and do something like ls D* it will add anything that matches that to the line (ex ls Desktop/ Downloads/); with it set, it will behave like ls D(tab) except it will move to the choices immediately without another tab.. evn though have it set otherwise so maybe if want that option instead of setting menu_complete can add a wildcard?; does the same with param stuff; not sure if it's supposed to be like this
setopt GLOB_COMPLETE
# }}}
#==========Pushd Stuff and Dir Stack=====see dh alias# {{{
#http://zsh.sourceforge.net/Intro/intro_6.html
DIRSTACKSIZE=8

# why would you type 'cd dir' if you could just type 'dir'?
setopt AUTO_CD

# This makes cd=pushd (also have auto cd); might want to remove depending on your workflow
setopt auto_pushd

# blank pushd goes to home; default already I think
setopt PUSHD_TO_HOME

#swap meaning of cd -num and cd +; dirs -v then cd -num of directory you want to switch to
setopt PUSHD_MINUS

# don't push multiple copies of the same directory onto the directory stack
setopt pushd_ignore_dups

# No more annoying pushd messages
setopt PUSHD_SILENT
# }}}
# ========== History ==========# {{{
# HISTORY
# lots of it
HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zsh_history

# remove command line from history list when first character on the line is a space
setopt hist_ignore_space

# Remove extra blanks from each command line being added to history
setopt hist_reduce_blanks

# don't execute, just expand history
setopt hist_verify

# Allow multiple terminal sessions to all append to one zsh command history instead of replacing file; default
setopt append_history

#save timestamp of command and duration; begginingtime:elapsedseconds
setopt extended_history

# Add comamnds as they are typed, don't wait until shell exit
setopt inc_append_history

# when trimming history, lose oldest duplicates first
setopt hist_expire_dups_first

# Do not write events to history that are duplicates of previous events
# haven't looked to see if this only applies to sequential commands or not; pretty sure only subsequent; otherwise would be hist_ignore_all_dups
setopt hist_ignore_dups

# When searching history don't display results already cycled through twice
#setopt hist_find_no_dups

# imports new commands and appends typed commands to history
# not useful to me at this point.. most of this isn't
setopt share_history

# }}}
# }}}
#========================================
# Bindings ( _Vim Stuff )# {{{
#======================================== 
# enable vi mode on commmand line; no visual
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets
bindkey -v

#fix home, end, etc. keys (with vim mode)
bindkey -M viins "^[[1~" beginning-of-line
bindkey -M viins "^[[4~" end-of-line
bindkey -M viins "^[[5~" beginning-of-history
bindkey -M viins "^[[6~" end-of-history
bindkey -M viins "^[[3~" delete-char
bindkey -M viins "^[[2~" quoted-insert
# can also do with zkbd and "${key[Home]}" stuff

# no delay entering normal mode
# https://coderwall.com/p/h63etq
# https://github.com/pda/dotzsh/blob/master/keyboard.zsh#L10
# 10ms for key sequences
KEYTIMEOUT=1

# add missing vim hotkeys
# fixes backspace deletion issues
# http://zshwiki.org/home/zle/vi-mode
# -a is same as -M vicmd..
bindkey -a u undo
bindkey -a U redo
bindkey '^?' backward-delete-char
bindkey '^H' backward-delete-char

# colemak (https://github.com/bunnyfly/dotfiles/blob/master/zshrc)
bindkey -M vicmd "h" backward-char
# bindkey -M vicmd "i" forward-char
bindkey -M vicmd "n" down-line-or-history
bindkey -M vicmd "e" up-line-or-history
# bindkey -M vicmd "s" vi-insert
# bindkey -M vicmd "S" vi-insert-bol
bindkey -M vicmd "k" vi-repeat-search
bindkey -M vicmd "K" vi-rev-repeat-search
# bindkey -M vicmd "l" beginning-of-line
# bindkey -M vicmd "L" end-of-line
bindkey -M vicmd "j" vi-forward-word-end
bindkey -M vicmd "J" vi-forward-blank-word-end

# control backspace
bindkey -M viins "¸" backward-kill-word


# http://unix.stackexchange.com/questions/25765/pasting-from-clipboard-to-vi-enabled-zsh-or-bash-shell
# paste from the system clipboard with p
vi-append-x-selection () { RBUFFER=$(xsel -ob </dev/null)$RBUFFER; }
zle -N vi-append-x-selection
bindkey -M vicmd p vi-append-x-selection

# change yank to yank to clipboard.. but i never yank with zsh so maybe not and i don't want dd storing text
# vi-yank-x-selection () { print -rn -- $CUTBUFFER | xsel -i -p; }
# zle -N vi-yank-x-selection
# bindkey -a '^Y' vi-yank-x-selection

# for termite link mode from vi cmd mode..
enter-url-hint() { xdotool key --window Termite control+shift+x }
zle -N enter-url-hint
bindkey -a f enter-url-hint

# for re-entering ranger 
enter-ranger() { xdotool key control+d }
zle -N enter-ranger
bindkey -a r enter-ranger

# for tmux copy mode
enter-copy-mode() { tmux copy-mode }
zle -N enter-copy-mode
bindkey -a t enter-copy-mode

# re-enter vim
#  http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
    zle redisplay
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
# v to re-enter vim
bindkey -a v fancy-ctrl-z

# history search in vim mode; not sure why I have this or what use it is
# http://zshwiki.org./home/zle/bindkeys#why_isn_t_control-r_working_anymore
bindkey -M viins '^s' history-incremental-search-backward
bindkey -M vicmd '^s' history-incremental-search-backward
# }}}
#========================================
# Custom Names/ Param Stuff {{{
#========================================
#benefit over alias is will show up as new name and fact that can use in aliases and functions
Desk=~/Desktop
# now music is ~/Music
# /Music=/run/media/angelic_sedition/Windows7/Users/Admin/Music/Winamp
# add one for books:  books=/path/to/books

# will autocomplete from any directory

#}}}
# #========================================
# _Aliases# {{{# {{{
#========================================
# remember that using && will break when one doesn't succesfully complete
# future naming convention: verb goes first
# 
# have yet to test this 
alias nitro='nitrogen "/ag-sys/Customization/wallpaper and icons/wallpaper/#used"'

# Add to Post-Install Setup Automation Script:# {{{
# install:
# feh, xscreensaver, compton, beets 
# python2-pip
# xterm
# apvlv, calibre
# ditched finch for bitlbee for now.. 
# quvi
# for mail:
#  offlineimap

# to get bar working with bspwn
# bar that stole from earsplit requires acpi
# need from aur: bar-aint-recursive, sutils-git, xtitle-git, wkline-git (doesn't work)

# yas luakit; then cp /etc/xdg/luakit ~/dotfiles/common/.config

# non-dotfiles that need editing:
# sudo permission stuff (i.e. fstab.. sudo/wheel/group stuff.. truecrypt without sudo)

# aur
# compton-git ; conky-lua ; bspwm-git ; 
# termite-git
# weechat-git
# preload
# xscreensaver-arch-logo
# spideroak (backup
# chromium-pepper-flash (not in next install)
# python2-keepass-git (for offlineimap to get pass from keypass)
# mutt-sidebar

# aur with setup:
# yas grive ; mkdir ~/grive ; cd ~/grive ; grive -a

# pip stuff
# python-mpd, pylast
# pip2 install discogs
# pip2 install requests (for artget)
# _ easy_isntall requests
# yas python-mpd2 (for artget

# }}}

# Startup and Shutdown# {{{
# startup stuff 
alias -g st='rldtime ; mountusb ; df -H'
# get this working without pass
alias panic='umountusb && umountalltc && poweroff'

alias reboot="umountalltc && sudo systemctl reboot"
alias poweroff='sudo systemctl poweroff'
alias umountalltc='truecrypt -d'

# get suspend working.. powerdown
# alias -g sus='systemctl suspend'
# }}}

# symlink all dotfiles with stow
alias deploy='cd ~/dotfiles ; stow -vt ~/ common terminal private vim'
alias restow='cd ~/dotfiles ; stow -Rvt ~/ common terminal private vim'

# Mounting and External Drives# {{{
# fdisk -l or mountie to find drive to mount; get mountie working


#rename
alias -g renameusb='ntfslabel /dev/sdb1'
# }}}

# git aliases# {{{
gin='git init'
alias -g gcl='git clone'
alias -g g='git'
# alias ga='git add *'
# alias gc='git commit -m' #remember to put commit in quotes
# alias gl='git clone'
# alias gp='git push origin'
# alias gh='git checkout'
alias -g gc='git commit'
alias gca='git commit -a'
# git commit -a (autostage all tracked files)

# send to github
# git remote add origin https://github.com/angelic-sedition/
alias togh='git push -u origin master'
# show changes that r unstaged
alias gd='git diff'
alias gds='git diff --staged'
function ga {
	git add "$1"
}
alias gs='git status'
alias gconf='git config --list'
alias -g gr="git rm --cached"
# }}}
#===============
# Plugin Aliases {{{
#===============
# z works.. thank god
# unite > v
# only for dir jumping
# source ~/bin/z.sh

# Fasd stuff (not working)# {{{
# tried a bunch of different stuff.. installed manually and from repos..
# 
# eval "$(fasd --init posix-alias zsh-hook zsh-ccomp zsh-ccomp-install)"
# _FASD_SINK="$HOME/.fasd.log"
# # # cache for faster startup
# if [ $commands[fasd] ]; then # check if fasd is installed
#   fasd_cache="$HOME/.fasd-init-cache"
#   if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
#     fasd --init auto >| "$fasd_cache"
#   fi
#   source "$fasd_cache"
#   unset fasd_cache
# fi
# 
# if [ $commands[fasd] ]; then # check if fasd is installed
#   fasd_cache="$HOME/.fasd-init-cache"
#   if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
#     fasd --init auto >| "$fasd_cache"
#   fi
#   source "$fasd_cache"
#   unset fasd_cache
#   alias v='f -e vim'
#   alias o='a -e open'
# fi
# _FASD_SINK="$HOME/.fasd.log"


# alias a='fasd -a'        # any
# alias s='fasd -si'       # show / search / select
# alias d='fasd -d'        # directory
# alias f='fasd -f'        # file
# alias sd='fasd -sid'     # interactive directory selection
# alias sf='fasd -sif'     # interactive file selection
# alias z='fasd_cd -d'     # cd, same functionality as j in autojump
# alias zz='fasd_cd -d -i' # cd with interactive selection
# # alias o='a -e open'
# alias v='f -e vim' # quick opening files with vim
# alias m='f -e mplayer' # quick opening files with mplayer
# alias o='a -e xdg-open' # quick opening files with xdg-open# }}}

#}}}
#===============
# Reloading things {{{
#===============
# zshrc
alias rld='echo "The Matrix has been reloaded" && . ~/.zshrc'
alias rld2='echo "Test reload" && . ~/.zshrctest'

alias rldurxvt='xrdb -load ~/.Xdefaults'
alias rldxdef='rldurxvt'

# xscreensaver after changing xresources
alias rldxscreen='xrdb -merge ~/.Xresources ; killall xscreensaver ; xscreensaver -no-splash &'

# keyboard stuff# {{{
alias rldxmd='xmodmap ~/.Xmodmap'
# necessary because when plug in keyboard, it goes to qwerty
alias rldusbkeyboard='setxkbmap us -variant colemak ; xmodmap ~/.Xmodmaptest2 ; xmodmap ~/.Xmodmap'
alias rldkbd='rldusbkeyboard'
# when using my japanese keyboard with double wide mod n extra thumbkeys
alias rldjp='xmodmap ~/.Xmodmapjp'
# test moving everything up:
alias tallmod='xmodmap ~/.Xmodmapqftk' 
# set caps to escape on tap if have to kill xcape
alias rldxcape="xcape -e 'Mode_switch=Escape'"
alias rldxcapejp="xcape -e 'Alt_L=Return'"
# }}}

#update font caches
alias rldfonts='fc-cache -vf'
# #add pretty much any font in ~/.fonts
# alias fonts='mkfontdir ~/.fonts;mkfontscale ~/.fonts;xset +fp ~/.fonts;xset fp rehash;fc-cache;fc-cache -fv'

#reload time
alias rldtime='sudo /usr/sbin/ntpdate us.pool.ntp.org'
# alias rldxmon='xmonad --recompile ; xmonad --restart'
# I'd add one for bspwm, but I could never get the loop example working
# if can't use binding for some reason
alias rldsxhkd='pkill -USR1 -x sxhkd'

#}}}
#===============
# General/Random {{{
#===============
# Super user
alias _='sudo'
alias please='sudo'
alias fucking='sudo'

alias -g pk='pkill'

# change hostname
alias -g chhost='hostnamectl set-hostname'

# Show history
alias history='fc -l 1'

# wm stuff
alias checkclass='xprop | grep WM_CLASS'
alias -g bsw='bspc window'
alias bsr='bspc window -R 90'
alias bslogout='pkill -x panel; bspc quit'

# Package Management {{{
alias pacss="pacman -Ss"
alias -g pacs='pacman -S'
alias pacq='pacman -Q'
alias pacqm="pacman -Qm"
alias -g pacupd='pacman -Syu'
alias -g pacr="pacman -Rns"

alias yass='yaourt -Ss'
alias yas='yaourt -S'
alias yapd='yaourt -Syu'
alias yaqm='yaourt -Qm'

#}}}
# Tmux {{{
# new session without nesting; type name afterwards
alias tmnew="TMUX= tmux new-session -d -s"
alias tmnest='unset TMUX ; tmux new-session -s'
alias tmkill='tmux kill-session -t'
alias ta='tmux attach -t'
alias ts='tmux new-session -s'
alias tl='tmux list-sessions'
# }}}
# Other Program Aliases
alias -g wifi='wifi-menu'
alias sfh='screenfetch'
alias wee='weechat'
alias bitl='sudo bitlbee -D'
alias mutt='cd ~/Mover && mutt'

# check ip; default gateway / router
alias gateip='ip route show'

# check what DNS using
alias showdns='cat /etc/resolv.conf'

# empty trash
alias -g rmtrash='rm -rf ~/.local/share/Trash'

# for opening in already open gvim session
alias -g gvir="gvim --remote"

define() {
 curl dict://dict.org/d:$1
}

# useless/fun/crap stolen from other people:# {{{
alias starwars='telnet towel.blinkenlights.nl'
alias fort='fortune -a'
alias gfort='fort | gayt'
# matrix like visual thing
alias hack='cat /dev/urandom | hexdump -c'

alias fuck='echo;gayt "fuck you too";echo'
#toilet
alias gaym='toilet --gay -f mono9 -t'
alias gayf='toilet --gay -f future -t'
alias gayt='toilet --gay -f term -t'
alias gayp='toilet --gay -f pagga -t'
alias metm='toilet --metal -f mono9 -t'
alias metf='toilet --metal -f future -t'
alias mett='toilet --metal -f term -t'
alias metp='toilet --metal -f pagga -t'


#============ lol aliases# {{{
alias wtf='dmesg'
alias onoz='cat /var/log/errors.log'
alias rtfm='man'

alias :3='echo'
alias visible='echo'
alias invisible='cat'
alias moar='more'
alias tldr='less'
alias alwayz='tail -f'

alias icanhas='mkdir'
alias gimmeh='touch'
alias donotwant='rm'
alias dowant='cp'
alias gtfo='mv'
alias nowai='chmod'

alias hai='cd'
alias iz='ls'
alias plz='pwd'
alias ihasbucket='df -h'

alias inur='locate'
alias iminurbase='finger'

alias btw='nice'
alias obtw='nohup'

alias nomz='ps aux'
alias nomnom='killall'

alias byes='exit'
alias cya='reboot'
alias kthxbai='halt'
# }}}
# }}}

#}}}
#===============
# Screen Shot/Record {{{
#===============
# Screen Recording; automatically works with external mic; q to quit and save to current dir
alias srec=" ffmpeg -f alsa -ac 2 -i hw:0,0 -f x11grab -r 30 -s 1366x768 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -threads 0 output.mkv"
# alias srec="ffmpeg -f alsa -ac 2 -i hw:0,0 -f x11grab -r 30 -s $(xwininfo -root | grep 'geometry' | awk '{print $2;}') -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -threads 0 -y screencast_out.avi"

# eventually actually get the wintitle in the name; I don't know where I got $name from, but it doesn't fucking work
alias zshot="scrot ~/Move/Screenshots/'$name_%m.%d.%y_-_%H:%M:%S_$wx$h.png'"

# upload to imgur: http://planspace.blogspot.com/2013/01/upload-images-to-your-imgur-account.html
# to deviantart through penta..jump the gap to the upload page

# old# {{{
# shutter, gnome sshot, or imagemagick..
# shutter has grab win tittle .. gnome can remove borders..
# http://www.thegeekstuff.com/2012/08/screenshot-ubuntu/

# shutter .. n flag doesn't add to the current session
# active:
# not working right now either
# alias capa="shutter -a -n -o '~/Move/Screenshots/$name_%m.%d.%y_$t$wx$h'"
# }}}
#}}}
#===============
# Directory Stuff {{{
#===============
# ls stuff {{{
#alias lsa='ls -lah'
#alias l='ls -la'
alias ll='ls -l'
#alias la='ls -lA'
#keep sl for steam locomotive choochoo
alias lsa='ls -a'
alias lsd='ls -d'
#ALIASES http://jeff.robbins.ws/reference/my-zshrc-file
##ls, the common ones I use a lot shortened for rapid fire usage
alias ls='ls --color' #I like color
alias l='ls -lFh'     #size,show type,human readable
alias la='ls -lAFh'   #long list,show almost all,show type,human readable
alias lr='ls -tRFh'   #sorted by date,recursive,show type,human readable
alias lt='ls -ltFh'   #long list,sorted by date,show type,human readable

# these require zsh
#alias ltd='ls *(m0)' # files & directories modified in last day
#alias lt='ls *(.m0)' # files (no directories) modified in last day
#alias lnew='ls *(.om[1,3])' # list three newest

#alias lh='ls -d .* --color' # show hidden files/directories only
#alias lsd='ls -aFhlG --color'
#alias l='ls -al --color'
#alias ls='ls -GFh --color' # Colorize output, add file type indicator, and put sizes in human readable format
#alias ll='ls -GFhl --color' # Same as above, but in long listing format
#alias tree="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"

#}}}
# cd stuff {{{
# custom cds
alias home='cd ~/'
alias dh='dirs -v'

# ### not needed due to ZSH autocd opt
# alias ..='cd ..'
# alias cd..='cd ..'
alias cd...='cd ../..'
alias cd....='cd ../../..'
alias cd.....='cd ../../../..'

alias -- -='cd -'
alias 1='cd -'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

#}}}
# size management in the terminal{{{
#directories sorted by size
alias 'dus=du -sckx * | sort -nr'

# colored disk usage; colour intelligently; sort; human readable sizes
alias diskspace='cdu -isdh'
alias dis='diskspace'
# check free space
alias fspace='df -h'
# consider using ncurses

#}}}

# number of files (not directories)
#alias 'filecount=find . -type f | wc -l'

# (need to install xclip)
alias -g pbcopy='xclip -selection clipboard'
# copy working directory to clipboard; requires pbcopy alias and xclip
alias cpwd="pwd | tr -d '\n' | pbcopy"
# the copydir plugin way of doing it:
# function copydir {
#   pwd | tr -d "\r\n" | pbcopy
# }
# copyfile omz plugin:
# function copyfile {
#   [[ "$#" != 1 ]] && return 1
#   local file_to_copy=$1
#   cat $file_to_copy | pbcopy
# }

# -p: make dirs if don't exist
alias md='mkdir -p'
#alias d='dirs -v | head -10'

#}}}
#==============================
# Ranger # {{{
#==============================
#use special names
# For downloading/saving things
alias fm='cd ~/Downloads ; ranger'
alias sc='cd ~/Move/Screenshots ; ranger'
# find a better way to connect pentadactyl and ranger; launch ranger in floating urxvt from penta

# alias rn='ranger'
rn() {
 case $1 in     
  dwn) ranger ~/Downloads ;;
  vim)     ranger ~/.vim ;;
  *)       ranger ;;
 esac
}

# Image Rotation (imagemagick)
imrotate() {
	mogrify -rotate 90 $1
}

# made this to extract file and then delete the archive afterwards; I use it in ranger a lot
exd() {
	atool -x $1 && rm $1
}

# }}}
#===============
# _Music & Sound {{{
#===============
# Alarm clock test
aclock() {
	echo 'cvlc "~/Music/Chelsea Grin/Evolve/Chelsea_Grin-Evolve-EP-2012-FiH/05_chelsea_grin-dont_ask_dont_tell-fih.mp3"' | at $1
}
# https://bbs.archlinux.org/viewtopic.php?id=128276
# usage:
#     alarm <description> <minutes>
alarm() {
	echo  "alarm set for: $1 in $2 minutes" &
    sleep $2  $(($2 * 60))  && notify-send -t 0  -i gtk-dialog-info alarm  "$1" 
}

# shorter alsamixer
alias als='alsamixer'

# use my config and enable scrobbling when starting mpd
alias mpd='mpd ~/.mpd/mpd.conf ; mpdscribble'
# kill ncmpcpp and mpd
alias -g mpkill='pkill mpd ; pkill ncmpcpp'

# pointing to config after just created; uneccesar 
alias nm='ncmpcpp -c ~/.ncmpcpp/config'

mu() {
	mpd
	sleep 1
	nm
}

#cd ripping (force use config file..; probably not necessary but wasn't automatically using first time tried and haven't changed it since)
alias -g rip='abcde -c $HOME/.abcde.conf'

# fix music tagging with beets
# beet import -W to not write tags to file

# can run beet import on Downloads folder after extracting to automatically
# have set move to yes; turn off when using in library
alias musimp='cd ~/Move ; beet import .'

# beet stats
# beet lyrics artist (album, etc.)
# same for fetchart

# beet mpdstats; not working

#}}}
#===============
# Android Music Syncing # {{{
#===============
# require sudo
alias -g mountand='jmtpfs -o allow_other ~/and'
alias -g umountand='fusermount -u ~/and'

# restart adb
alias adbrestart='adb kill-server ; adb start-server'
# reload udev rules: (sudo)
alias -g andudevreload='udevadm control --reload-rules'

alias -g andsnmus='andmusconfig1'
alias -g andmusconfig1='sync30sec ; syncimogen ; syncadtr ; syncbaao ; syncblonde ; syncbrandnew ; syncbmth ; syncchevelle ; synccoldplay ; syncdgd ; syncdeadmau5 ; syncdeftones ; syncfob ; syncffaf ; syncgreenday ; syncimogen ; synckeane ; synclydia ; syncmuse ; syncmcr ; syncofmm ; syncparamore ; syncptv ; syncradiohead ; syncrage ; syncra ; syncsenses ; syncskrillex ; syncsmashingpumpkins ; syncsoad ; synckillers ; syncrja ; syncsleeping ; synctssf ; syncstrokes ; syncthrice ; syncuverworld'

# by bands:# {{{
alias -g sync30sec='rsync -avr --delete ~/Music/30\ Seconds\ To\ Mars ~/and/Card/Music'
alias -g syncadtr='rsync -avr ~/Music/A\ Day\ To\ Remember ~/and/Card/Music'
alias -g syncalexd='rsync -avr --delete ~/Music/Asking\ Alexandria ~/and/Card/Music'
alias -g syncbaao='rsync -avr --delete ~/Music/Being\ As\ An\ Ocean ~/and/Card/Music'
alias -g syncblonde='rsync -avr --delete ~/Music/Blonde\ Redhead ~/and/Card/Music'
alias -g syncbrandnew='rsync -avr --delete ~/Music/Brand\ New ~/and/Card/Music'
alias -g syncbmth='rsync -avr --delete ~/Music/Bring\ Me\ The\ Horizon ~/and/Card/Music'
alias -g syncchevelle='rsync -avr --delete ~/Music/Chevelle ~/and/Card/Music'
alias -g synccoldplay='rsync -avr --delete ~/Music/Coldplay ~/and/Card/Music'
alias -g syncdgd='rsync -avr --delete ~/Music/Dance\ Gavin\ Dance ~/and/Card/Music'
alias -g syncdeadmau5='rsync -avr --delete ~/Music/Deadmau5 ~/and/Card/Music'
alias -g syncdeftones='rsync -avr --delete ~/Music/Deftones ~/and/Card/Music'
alias -g syncfob='rsync -avr --delete ~/Music/Fall\ Out\ Boy ~/and/Card/Music'
alias -g syncffaf='rsync -avr --delete ~/Music/Funeral\ For\ A\ Friend ~/and/Card/Music'
alias -g syncgreenday='rsync -avr --delete ~/Music/Green\ Day ~/and/Card/Music'
alias -g syncimogen='rsync -avr --delete ~/Music/Imogen\ Heap ~/and/Card/Music'
alias -g synckeane='rsync -avr --delete ~/Music/Keane ~/and/Card/Music'
alias -g synclydia='rsync -avr --delete ~/Music/Lydia ~/and/Card/Music'
alias -g syncmuse='rsync -avr --delete ~/Music/Muse ~/and/Card/Music'
alias -g syncmcr='rsync -avr --delete ~/Music/My\ Chemical\ Romance ~/and/Card/Music'
alias -g syncofmm='rsync -avr --delete ~/Music/Of\ Mice\ &\ Men ~/and/Card/Music'
alias -g syncparamore='rsync -avr --delete ~/Music/Paramore ~/and/Card/Music'
alias -g syncptv='rsync -avr --delete ~/Music/Pierce\ The\ Veil ~/and/Card/Music'
alias -g syncradiohead='rsync -avr --delete ~/Music/Radiohead ~/and/Card/Music'
alias -g syncrage='rsync -avr --delete ~/Music/Rage\ Against\ The\ Machine ~/and/Card/Music'
alias -g syncra='rsync -avr --delete ~/Music/Rise\ Against ~/and/Card/Music'
alias -g syncsenses='rsync -avr --delete ~/Music/Senses\ Fail ~/and/Card/Music'
alias -g syncskrillex='rsync -avr --delete ~/Music/Skrillex ~/and/Card/Music'
alias -g syncsmashpumpkins='rsync -avr --delete ~/Music/Smashing\ Pumkins ~/and/Card/Music'
alias -g syncsoad='rsync -avr --delete ~/Music/System\ Of\ A\ Down ~/and/Card/Music'
alias -g synckillers='rsync -avr --delete ~/Music/The\ Killers ~/and/Card/Music'
alias -g syncrja='rsync -avr --delete ~/Music/The\ Red\ Jumpsuit\ Apparatus ~/and/Card/Music'
alias -g syncsleeping='rsync -avr --delete ~/Music/The\ Sleeping ~/and/Card/Music'
alias -g synctssf='rsync -avr --delete ~/Music/The\ Story\ So\ Far ~/and/Card/Music'
alias -g syncstrokes='rsync -avr --delete ~/Music/The\ Strokes ~/and/Card/Music'
alias -g syncthrice='rsync -avr --delete ~/Music/Thrice ~/and/Card/Music'
alias -g syncuverworld='rsync -avr --delete ~/Music/UVERworld ~/and/Card/Music'
# }}}
# }}}
#==============================
# _Backup & Mounting # {{{
#==============================
# All smaller syncs are useless at this point (unless just do to spideroak); test sync time
# now mount to ag-sys-bk
alias mountbkdrive='truecrypt /run/media/angelic_sedition/ag-sys/soma ~/ag-sys-bk'
alias umountbkdrive='truecrypt -d /run/media/angelic_sedition/ag-sys/soma'

# no longer mount soma on usb
alias mountusb='gvfs-mount -d /dev/sdb1'
alias umountusb='gvfs-mount -u /run/media/angelic_sedition/ag-sys'

# external backup harddrive
alias mountexthdd='gvfs-mount -d /dev/sdd1'

# moved main soma off usb to computer (no more annoyances if usb comes out)
# since mounting to same location (~/ag-sys) don't have to change much
alias mountsoma='truecrypt ~/grive/soma_ ~/ag-sys/'
alias umountsoma='truecrypt -d ~/grive/soma_'

# in return added this:
# full backup to drive
alias bahamut='mountsoma && sncall && mountbkdrive && fullbktousb && umountbkdrive'
alias fullbktousb='rsync -avr --delete ~/ag-sys/ ~/ag-sys-bk/'

# Common:
alias sngdrive='cd ~/grive/ ; grive -V'
# no longer to usb
alias syncconfigusb='rsync -avr --delete ~/dotfiles "~/ag-sys/Backup/A#config_files/"'
alias sncall='syncconfigusb'

# other truecrypt mounting:# {{{
# mount accts
alias -g mountacct='truecrypt ~/ag-sys/Else/ACCTS ~/blemish'
                              # /run/media/angelic_sedition/ag-sys/Else/ACCTS
alias umountacct='truecrypt -d ~/ag-sys/Else/ACCTS'
# }}}


# big backup
# rsync -avr /home/angelic_sedition /run/media/angelic_sedition/HD-CEU2\ Backup 

# usb drive must be mounted to truecrypt18.. now ~/ag-sys
# levels: full, mid , all txt and frequently changing files

# psp backup 16gb mem card...# {{{
# make sure connected to sdd1.. bottom right usb slot
alias mountpsp='gvfs-mount -d /dev/sdd1'
alias bktehpsp='rsync -avr --delete /run/media/angelic_sedition/MS0/ /media/truecrypt5/database/Gaming/PSP\ and\ Vita/PSP/PSP\ Backup/'
# alias umountpsp='gvfs-mount u /run/media'
# }}}

# full backup to google drive# {{{
# this is no longer necessary.. full backup goes reverse direction now
# alias bahamut='mountdrive && sncall && mountfullusbbk && fullusbbk && umountfullusbbk && sngdrive'
# alias mountfullusbbk='truecrypt ~/grive/soma /media/truecrypt21'
# alias umountfullusbbk='truecrypt -d ~/grive/soma'
# alias fullusbbk='rsync -avr --delete ~/ag-sys/ /media/truecrypt21/'

# if keep the main location in the grive folder; all have to do is umount; depending on how quick this is may 
alias fullsomabk='umountsoma && sngdrive'


# }}}

# about 70 megs; 120meg tc up to date basic backup; second smallest backup but with odt and rtf (rtf is all old stuff)
# singluttony# {{{

# added html stuff (forums/) and weechat because doesn't fit in other (including logs)
alias sngriveusb2="rsync -avr --include='*/' --include='*.txt' --include='*.rb' --include='*.py' --include='*.pl' --include='*.odt' --include='.weechat/**' --include='forums/**' --include='*.rtf' --include='*.doc' --include='*.docx' --exclude='*' --prune-empty-dirs ~/ag-sys/ /media/truecrypt58"

# part of incremental restore I did on data loss
# alias otrestore="rsync -avr --include='*/' --include='*.odt' --include='*.rtf' --exclude='*' /media/truecrypt58/Else/everything/ ~/temptstow/Else/everything"
# sync all that hasn't been taken care of
# alias ot2restore="rsync -auvr --include='*/' --exclude='*.odt' --exclude='*.rb' --exclude='*.py' --exclude='*.txt' --exclude='*.rtf' include='*' /media/truecrypt3/Else/everything/ ~/temptstow/Else/everything"

alias mountgriveusb2='truecrypt ~/grive/usb_bk /media/truecrypt58'
alias umountgriveusb2='truecrypt -d ~/grive/usb_bk'


alias singluttony='mountgriveusb2 ; sngriveusb2 ; umountgriveusb2 ; sngdrive'

# chron and --delete... accidental deletion; don't do
# }}}

#don't worry too much about dotfiles.. got the important stuff with this; the rest on github and in full backup
# smallest/quick encrypted backup to harddrive and gdrive and spider oak
# 11mb and 18mb tc# {{{

# pretty much all space taken up now.. make bigger at some point.. be more agressive; going to need to delete ag-sys on next sync; needs some fixing
#including mpd database (record of all music have); add newest dotfiles that don't already fit characteristics
alias sngriveusb="rsync -avr --exclude='Portable/*' --exclude='Gaming/*' --exclude='Too_Large/*' --exclude='.themes/*' --exclude='tmux-powerline/*' --exclude='bundle/*' --exclude='.weechat/*' --include='*/' --include='*.txt' --include='*.rb' --include='*.py' --include='*.pl' --include='*.hs' --include='*.lua' --include='*.ini' --include='*.conf' --include='*config*' --include='*.zsh_history' --include='*.yaml' --include='*.Xmodmap*' --include='*.Xdefaults' --include='.Xresources' --include='.emacs' --include='.gitconfig' --include='.lesskey' --include='*xscreensaver*' --include='.mpd/database' --include='dotfiles/**rc' --include='.unite/*' --include='sessions/*' --include='TO\ Backup/*' --include='bin/**sh' --include='termite/*' --include='ncmpcpp/*' --exclude='*' --prune-empty-dirs ~/ag-sys/ /media/truecrypt59"

# takes about 3 and a half minutes to sync
alias snspideroakusb="SpiderOak --batchmode --backup='~/grive/smaller_usb_bk'"

# to add: dropboxsync

alias mountgriveusb='truecrypt ~/grive/smaller_usb_bk /media/truecrypt59'
alias umountgriveusb='truecrypt -d ~/grive/smaller_usb_bk'

# final product for quick backup to harddrive and 
alias sin='sncall ; mountgriveusb ; sngriveusb ; umountgriveusb ; sngdrive ; snspideroakusb'

# }}}

# old:
# don't put files with passwords into cloud without encrypting
alias syncconfigdbox='rsync -avr --delete --exclude="private" ~/dotfiles ~/Dropbox/'
# Sync USB to dropbox# {{{
# idea for just syncind specifc files like txt, pdf, odt, etc.
# http://credentiality2.blogspot.com/2011/02/using-rsync-to-copy-only-certain.html
# alias syncsmalldbx='rsync -avr --include='*/' --include='*.txt' --include='*.rb' --include='*.py' --include='*.rb' --exclude='*' --prune-empty-dirs "/run/media/angelic_sedition/ag-sys/Else" /media/truecrypt3/'

#MAKE SURE usb backup is open in truecrypt 3 before running

# # mount, sync, dismount
# alias -g syncdbox='mountdboxusb ; snctodbx ' 
# 
# alias -g mountdboxusb='truecrypt ~/Dropbox/corebackup/USB_Backup /media/truecrypt3'
# alias -g umountdboxusb='truecrypt -d ~/Dropbox/corebackup/USB_Backup'
# 
# # Core Backup ; as of 1.11.14 about 1.8+ gigs
# # truecrypt container is 2.44gb.. ensure have enough room on dbox
# alias snctodbx='syncelse ; syncbackup ; syncportable ; syncschool'
# 
# alias syncelse='rsync -avr --delete "~/ag-sys/Else" /media/truecrypt3/'
# alias syncbackup='rsync -avr --delete "~/ag-sys/Backup" /media/truecrypt3/'
# alias syncportable='rsync -avr --delete "~/ag-sys/Portable/Sync" /media/truecrypt3/'
# alias syncschool='rsync -avr --delete "~/ag-sys/School" /media/truecrypt3/'

# }}}

# backup database to external harddrive# {{{
alias -g syncdatab='mountexthdd ; mountdatab; mountbkdatab ; sndatab ; umountbkdatab'

alias -g mountdatab='truecrypt /run/media/angelic_sedition/Windows7/Users/Admin/Documents/Datab /media/truecrypt5'
alias -g mountbkdatab='truecrypt /run/media/angelic_sedition/HD-CEU2\ Backup/Datab /media/truecrypt6'
alias sndatab='rsync -avr --delete /media/truecrypt5/ /media/truecrypt6/'
alias unmountbkdatab='truecrypt -d /run/media/angelic_sedition/HD-CEU2\ Backup/Datab'

# for easy unmount from term
alias undatab'truecrypt -d /run/media/angelic_sedition/Windows7/Users/Admin/Documents/Datab'

alias -g mountdatab2='truecrypt /run/media/angelic_sedition/Windows7/Users/Admin/Documents/Database2 /media/truecrypt12'
alias undatab2='truecrypt -d /run/media/angelic_sedition/Windows7/Users/Admin/Documents/Database2'
# }}}


# }}}
#========================================
# Other Functions# {{{
#========================================
# my conversion function # {{{
# [1.11.14]
#requirements: 
# unoconv
# directories should not have . in the name

# run and will convert all rtfs and move them to folder you ran it in/graveyard in parallel folder structure
# ifs is internal field separator.. turn into array
# http://stackoverflow.com/questions/11393817/bash-read-lines-in-file-into-an-array
# for removing text from string: % 
# http://tldp.org/LDP/abs/html/string-manipulation.html

# exlude graveyard from search 
# maybe add flag for exclude
rtf2txt() {
	# create an array for all rtf files (recrusive)
	IFS=$'\r\n' files=($(find . -name "*.rtf" -not -path "*/graveyard*" -not -path "*/zDissolve*"))
	basedir=$(pwd)
	bin="/graveyard"
	crap="."
	# -p creates directory and all needed ones up to it and won't fail if exists
	# http://stackoverflow.com/questions/59838/how-to-check-if-a-directory-exists-in-a-shell-script
	mkdir -p graveyard
	movedir=$basedir$bin
	for file in $files
	do
		# convert rtf to txt file
		unoconv -f txt $file
		# get the parallel location to move the rtf file to by removing filename.rtf 
		subdir=$(echo ${file%/*.rtf})
		finaldir=$movedir$subdir
		# remove period at end
		finaldir=$(echo ${finaldir//[.]/})
		# create finaldir if doesn't exist:
		mkdir -p $finaldir
		# move the file to graveyard
		mv $file $finaldir
		# ## removes longest match from beginning 
		filename=$(echo ${file##*/})
		echo "Converted and moved $filename"
	done
}
# }}}

# from: https://github.com/z1lt0id/awesome/blob/master/.bashrc 
# sanitize - set file/directory owner and permissions to normal values (644/755)
# Usage: sanitize <file>
sanitize() {
    chmod -R u=rwX,go=rX "$@"
    chown -R ${USER}.users "$@"
}

# Random# {{{
# --------------------------------------------------------------
# Find files and exec commands at them.
# $ find-exec .coffee cat | wc -l
# # => 9762
# from https://github.com/paulmillr/dotfiles
# --------------------------------------------------------------
function find-exec() {
  find . -type f -iname "*${1:-}*" -exec "${2:-file}" '{}' \;
}

# --------------------------------------------------------------
# Show how much RAM application uses.
# $ ram safari
# # => safari uses 154.69 MBs of RAM.
# from https://github.com/paulmillr/dotfiles
# --------------------------------------------------------------
function ram() {
  local sum
  local items
  local app="$1"
  if [ -z "$app" ]; then
    echo "First argument - pattern to grep from processes"
  else
    sum=0
    for i in `ps aux | grep -i "$app" | grep -v "grep" | awk '{print $6}'`; do
      sum=$(($i + $sum))
    done
    sum=$(echo "scale=2; $sum / 1024.0" | bc)
    if [[ $sum != "0" ]]; then
      echo "${fg[blue]}${app}${reset_color} uses ${fg[green]}${sum}${reset_color} MBs of RAM."
    else
      echo "There are no processes with pattern '${fg[blue]}${app}${reset_color}' are running."
    fi
  fi
}

# --------------------------------------------------------------
# any function from http://onethingwell.org/post/14669173541/any
# search for running processes
# --------------------------------------------------------------
any() {
    emulate -L zsh
    unsetopt KSH_ARRAYS
    if [[ -z "$1" ]] ; then
        echo "any - grep for process(es) by keyword" >&2
        echo "Usage: any " >&2 ; return 1
    else
        ps xauwww | grep -i --color=auto "[${1[1]}]${1[2,-1]}"
    fi
}

# --------------------------------------------------------------
# display a neatly formatted path
# --------------------------------------------------------------
path() {
  echo $PATH | tr ":" "\n" | \
    awk "{ sub(\"/usr\",   \"$fg_no_bold[green]/usr$reset_color\"); \
           sub(\"/bin\",   \"$fg_no_bold[blue]/bin$reset_color\"); \
           sub(\"/opt\",   \"$fg_no_bold[cyan]/opt$reset_color\"); \
           sub(\"/sbin\",  \"$fg_no_bold[magenta]/sbin$reset_color\"); \
           sub(\"/local\", \"$fg_no_bold[yellow]/local$reset_color\"); \
           sub(\"/.rvm\",  \"$fg_no_bold[red]/.rvm$reset_color\"); \
           print }"
}

# --------------------------------------------------------------
# nice mount (http://catonmat.net/blog/another-ten-one-liners-from-commandlingfu-explained)
# displays mounted drive information in a nicely formatted manner
# --------------------------------------------------------------
function nicemount() { (echo "DEVICE PATH TYPE FLAGS" && mount | awk '$2="";1') | column -t ; }

# --------------------------------------------------------------
# (s)ave or (i)nsert a directory.
# --------------------------------------------------------------
s() { pwd > ~/.save_dir ; }
i() { cd "$(cat ~/.save_dir)" ; }

# --------------------------------------------------------------
# rk Checks the key value in the riak cluster
# --------------------------------------------------------------
rk() { curl -v -s -o/dev/null http://10.22.40.35:8098/riak/$1 }
# }}}

# }}}
# }}}

# autorun tmux; If not running interactively, do not do anything; not necessary with dropdown script
# [[ $- != *i* ]] && return
# [[ -z "$TMUX" ]] && exec tmux

# Other crap# {{{
# alt prompts# {{{
# export PROMPT="\[\e[01;31m\]┌─[\t]──[\[\e[01;31m\u\e[01;31m\]]──[\[\e[00;31m\]${HOSTNAME%%.*}\[\e[01;31m\]]:\w$\[\e[01;31m\]\n\[\e[01;37m\]└──\[\e[01;37m\](\[\e[32;1m\]\$(/bin/ls -1 | /usr/bin/wc -l | /bin/sed 's: ::g') files, \$(/usr/bin/ls -lah | /usr/bin/grep -m 1 total | /usr/bin/sed 's/total //')b\[\e[01;37m\])>>\[\e[0m\]"

# zsh equivalent of above

# export PS1="%{$bold_color$fg[red]%}┌─[%t]──[%{$bold_color$fg[red]%n$bold_color$fg[red]%}]──[%{$reset_color$fg[red]%}${HOSTNAME%%.*}%{$bold_color$fg[red]%}]:%~$%{$bold_color$fg[red]%}$prompt_newline%{$bold_color$fg[white]%}└──%{$bold_color$fg[white]%}(%{$fg[green]$bold_color%}\$(/bin/ls -1 | /usr/bin/wc -l | /bin/sed 's: ::g') files, \$(/usr/bin/ls -lah | /usr/bin/grep -m 1 total | /usr/bin/sed 's/total //')b%{$bold_color$fg[white]%})>>%{$reset_color%}"

# from https://github.com/phallus/arch-files/blob/master/config/.zshrc
# export PS1="%{[38;05;0;48;05;7m%} %3~ %{[38;05;7;48;05;4m%}⮀%{[00m%}%{[38;05;8;48;05;4m%}%{[00m%}%{[38;05;4m%}⮀ %{[00m%}"

# from http://crunchbang.org/forums/viewtopic.php?id=31216 
# export PS1="┌─[ \[\e[1;32m\]\w\[\e[0m\] ]\n└─$ "
# zsh version
# export PS1="┌─[ %{$bold_color$fg[green]%}%~%{$reset_color%} ]$prompt_newline└─$"


# from here https://bbs.archlinux.org/viewtopic.php?id=159209
# export PS1='\[\033[0;36m\]╔═(\[\033[0m\033[0;36m\]\u\[\033[0m\]@\[\033[0;32m\]\h\[\033[0m\033[0;36m\])────(\[\033[0m\]\t \d\[\033[0;36m\])────(\[\033[0m\]\w\[\033[0;36m\])\n\[\033[0;36m\]╚═══[ \[\033[0m\033[0;36m\]\$\[\033[0m\033[0;36m\]]>\[\033[0m\] '
# export PS1='%{$reset_color$fg[cyan]%}╔═(%{$reset_color$reset_color$fg[cyan]%}%n%{$reset_color%}@%{$reset_color$fg[green]%}%m%{$reset_color$reset_color$fg[cyan]%})────(%{$reset_color%}%t %D{%a %b %d}%{$reset_color$fg[cyan]%})────(%{$reset_color%}%~%{$reset_color$fg[cyan]%})$prompt_newline%{$reset_color$fg[cyan]%}╚═══[ %{$reset_color$reset_color$fg[cyan]%}%#%{$reset_color$reset_color$fg[cyan]%}]>%{$reset_color%} '
# export PS1='%{$fg[magenta]%}%n%{$reset_color%} at %{$fg[yellow]%}%m%{$reset_color%}'
# }}}
# graveyard
# # {{{
# 
# old/obsolete things# {{{
# sync usb folder to dropbox
# alias isnconf'='
# 
# sync all to usb
# alias sncall='syncvim ; syncvimrc ; syncsubl3 ; syncxin ; synczhist ; synczsh ; syncxmod ; synctmux ; synccmus ; syncranger ; syncautostart ; syncranger ; syncmpd ; syncmpdscribble ; syncncmpcpp ; syncurxvt ; syncawesome'
# 
# awesome
# alias syncawesome='rsync -avr ~/.config/awesome "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# vimrc
# alias syncvimrc='rsync -av ~/.vimrc "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# vim foldear
# alias syncvim='rsync -avr ~/.vim "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# subl3
# alias syncsubl3='rsync -av ~/.config/sublime-text-3/Packages/User/Preferences.sublime-settings "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# xinitrc
# alias syncxin='rsync -av ~/.xinitrc "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# zsh_history
# alias synczhist='rsync -av ~/.zsh_history "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# zsh
# alias synczsh='rsync -av ~/.zshrc "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# xmodmap
# alias syncxmod='rsync -av ~/.Xmodmap "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# tmux
# alias synctmux='rsync -av ~/.tmux.conf "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# cmus
# alias synccmus='rsync -avr ~/.cmus "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# ranger
# alias syncranger='rsync -avr ~/.config/ranger "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# autostart
# alias syncautostart='rsync -avr ~/.config/autostart "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# autokey
# alias syncautokey='rsync -avr ~/.config/autokey "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# ncmpcpp and mpd and mpdscribble
# alias syncmpd='rsync -avr ~/.mpd "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# alias syncmpdscribble='rsync -avr ~/.mpdscribble "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# alias syncncmpcpp='rsync -avr ~/.ncmpcpp "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# 
# urxvt settings
# alias syncurxvt='rsync -av ~/.Xdefaults "/run/media/angelic_sedition/ag-sys/Backup/A#config_files/"'
# }}}
# put ascii here:
# read -r -d '' VAR <<'EOF'
# ascii
# EOF
# echo "$VAR" | lolcat

# test() {
# 	# TASK 1
# 	while :
# 	do
# 		date
# 		read -t 1 -n 1 key
# 		echo $key
# 	if [[ $key = s ]]
# 	then
# 		break
# 	fi
# 	done
# }
# 
# 
# conf() {# {{{
#         case $1 in
#                 xmonad)                vim ~/.xmonad/xmonad.hs ;;
#                 bspwm)                vim ~/.config/bspwm/bspwmrc ;;
#                 sxhkd)                vim ~/.config/sxhkd/sxhkdrc ;;
#                 conky)                vim ~/.xmonad/.conky_dzen ;;
#                 homepage)        olddir=$(pwd) && cd ~/scripts/homepage.py && vim homepage.py && ./homepage.py; cd $olddir ;;
#                 menu)                vim ~/scripts/menu ;;
#                 mpd)                vim ~/.mpdconf ;;
#                 mutt)                vim ~/.mutt/acct/wei001 ;;
#                 ncmpcpp)        vim ~/.ncmpcpp/config ;;
#                 pacman)                svim /etc/pacman.conf ;;
#                 ranger)                vim ~/.config/ranger/rc.conf ;;
#                 rifle)                vim ~/.config/ranger/rifle.conf ;;
#                 tmux)                vim ~/.tmux.conf ;;
#                 vim)                vim ~/.vimrc ;;
#                 xinit)                vim ~/.xinitrc ;;
#                 xresources)        vim ~/.Xresources && xrdb ~/.Xresources ;;
#                 zathura)        vim ~/.config/zathura/zathurarc ;;
#                 theme2)                vim ~/.themes/FlatStudioCustom/gtk-2.0/gtkrc ;;
#                 theme3)                vim ~/.themes/FlatStudioCustom/gtk-3.0/gtk.css ;;
#                 gtk2)                vim ~/.gtkrc-2.0 ;;
#                 gtk3)                vim ~/.config/gtk-3.0/settings.ini ;;
#                         tint2)                vim ~/.config/tint2/xmonad.tint2rc ;;
#                 zsh)                vim ~/.zshrc && source ~/.zshrc ;;
#                 hosts)                sudoedit /etc/hosts ;;
#                 vhosts)                sudoedit /etc/httpd/conf/extra/httpd-vhosts.conf ;;
#                 httpd)                sudoedit /etc/httpd/conf/httpd.conf ;;
#                 *)                        echo "Unknown application: $1" ;;
#         esac
# }
# # }}}
# graveyard.. management from within vim using unite and bookmarks is much faster
# #vim - thanks to earsplit for this!
# conf() {
#  case $1 in
#   mpd)     vim ~/.mpdconf ;;
#   conky)   vim ~/.conkyrc ;;
#   ncp)     vim ~/.ncmpcpp/config ;;
#   rn)      vim ~/.config/ranger/rc.conf ;;
#   tmux)    vim ~/.tmux.conf ;;
#   vim)     vim ~/.vimrc ;;
#   xi)      vim ~/.xinitrc ;;
#   xd)      vim ~/.Xdefaults ;;
#   zsh)     vim ~/.zshrc && source ~/.zshrc ;;
#   uc)      vim ~/.mozilla/firefox/*.default/chrome/userChrome.css ;;
#   i3)      vim ~/.i3/config ;;
#   irssi)   vim ~/.irssi/config ;;
#   crshd)   vim ~/.irssi/phallus.theme ;;
#   lemon)   ~/.fonts/bdf;gbdfed;vim lemon.bdf;bdftopcf -o ../lemon.pcf lemon.bdf ;;
#   uushi)   ~/.fonts/bdf;gbdfed;bdftopcf -o ../uushi.pcf uushi.bdf ;;
#   awesome) vim ~/.config/awesome/rc.lua ;;
#   gtk)     vim ~/.themes/Kvtie/gtk-2.0/gtkrc ;;
#   pacman)  sudo vim /etc/pacman.conf ;;
#   vimp)    vim ~/.vimperatorrc ;;
#   mutt)    vim ~/.muttrc ;;
#   vimmode) vim ~/.irssi/vim_moderc ;;
#   *)       echo "unknown conf: $1" ;;  
#  esac
# }


# --------------------------------------------------------------
# compressed file expander
# (from https://github.com/myfreeweb/zshuery/blob/master/zshuery.sh)
# --------------------------------------------------------------
# ex() {
#     if [[ -f $1 ]]; then
#         case $1 in
#           *.tar.bz2) tar xvjf $1;;
#           *.tar.gz) tar xvzf $1;;
#           *.tar.xz) tar xvJf $1;;
#           *.tar.lzma) tar --lzma xvf $1;;
#           *.bz2) bunzip $1;;
#           *.rar) unrar $1;;
#           *.gz) gunzip $1;;
#           *.tar) tar xvf $1;;
#           *.tbz2) tar xvjf $1;;
#           *.tgz) tar xvzf $1;;
#           *.zip) unzip $1;;
#           *.Z) uncompress $1;;
#           *.7z) 7z x $1;;
#           *.dmg) hdiutul mount $1;; # mount OS X disk images
#           *) echo "'$1' cannot be extracted via >ex<";;
#     esac
#     else
#         echo "'$1' is not a valid file"
#     fi
# }

# }}}
# }}}
#  
# Enable autosuggestions automatically# {{{
# got messed up again 
# zle-line-init() {
#     zle autosuggest-start
# }
# zle -N zle-line-init
#
# source ~/.zsh-autosuggestions/autosuggestions.zsh
#
# # use ctrl+t to toggle autosuggestions(hopefully this wont be needed as
# # zsh-autosuggestions is designed to be unobtrusive)
# bindkey '^T' autosuggest-toggle
# }}}
