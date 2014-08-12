# set as default shell by changing /etc/zsh/zprofile ... /etc/profile or just do chsh -s /bin/zsh
# changed back to bash as login shell; history lock problem
# TODO:
# make paste binding escape chars like c-s-v does
# fix android mounting
# rewrite entire backup section

# Sources:# {{{
# Some settings or stuff got from here:
#https://wiki.archlinux.org/index.php/Zsh
#http://zsh.sourceforge.net/Doc/Release/Zsh-Modules.html
#sources- https://github.com/Adalan/dotfiles/blob/master/zsh/setopt.zsh#L33
#http://www.bash2zsh.com/zsh_refcard/refcard.pdf
#http://stackoverflow.com/questions/171563/whats-in-your-zshrc
#https://github.com/Adalan/dotfiles/blob/master/zsh/functions.zsh
#https://wiki.gentoo.org/wiki/Zsh/HOWTO#
# https://github.com/phallus/arch-files/blob/master/config/.zshrc
#}}}
#==============================
# Export# {{{
#==============================
export EDITOR=vim
export PAGER=less
# verbose and smart case search
export LESS="-iM"
# less > more.. and most doesn't wrap?
# consider https://github.com/rkitover/vimpager at some point; less works great so probably uneccesary

export BROWSER=firefox

# for bar for bspwm
export PANEL_FIFO="/tmp/panel-fifo"
export PANEL2_FIFO="/tmp/panel-fifo2"
export PANEL_HEIGHT=20
# export BSPWM_TREE=/tmp/bspwm.tree
# export BSPWM_HISTORY=/tmp/bspwm.history
# export BSPWM_STACK=/tmp/bspwm.stack

# For gnome keyring; using gpg for passwords for getting/sending mail
export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

# }}}
#==============================
# Path {{{
#==============================
# add ~/bin to $path for any scripts
export PATH=$PATH:~/bin
# export PATH=$PATH:~/bin/artget.py
export PATH=$PATH:~/bin/mpv
export PATH=$PATH:~/.config/bspwm/panel
# for vimus
export PATH=$PATH:/home/angelic_sedition/.cabal/bin
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

# for rbenv
eval "$(rbenv init -)"
# gem
export PATH=$PATH:~/.gem/ruby/2.1.0/bin

# }}}
#==============================
# Appearance {{{
#==============================
# Color Manpages
# similar to things like colored-man
export LESS_TERMCAP_mb=$'\E[01;31m'             # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'             # begin bold
export LESS_TERMCAP_me=$'\E[0m'                 # end mode
export LESS_TERMCAP_se=$'\E[0m'                 # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m'          # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'                 # end underline
export LESS_TERMCAP_us=$'\E[01;32m'             # begin underline

#========== Prompt ==========
# using custom with antigen (see antigen)

# show vi mode
# this works with autosuggestions (though I don't autosuggestions anymore): http://hamberg.no/erlend/posts/2010-10-17-show-current-vi-mode-in-zsh-prompt.html
# see my custom theme in /terminal/.zsh/themes

#see termite config for term colours
#}}}
#==============================
# Antigen (Plugins) {{{
#==============================
# takes care of installation and sourcing
# installed from aur
source /usr/share/zsh/scripts/antigen/antigen.zsh
# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle command-not-found

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# up and down will go through history based on what you have typed
antigen bundle zsh-users/zsh-history-substring-search

# text objects for normal mode
# antigen bundle hchbaw/opp.zsh

# Load the theme (frome omz/themes folder)
antigen theme fox-mod

# Tell antigen that you're done.
antigen apply
#}}}
#==============================
# Options {{{
#==============================
#http://linux.die.net/man/1/zshoptions

# perform implicit tees or cats when multiple redirections are attempted; default for zsh
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
# 10 second wait if you do something that will delete everything; then will prompt
setopt RM_STAR_WAIT

#allow comments 
setopt interactive_comments

#no beeps
setopt no_beep
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

#I don't know if I want this unset or set; if unset and do something like ls D* it will add anything that matches that to the line (ex ls Desktop/ Downloads/); with it set, it will behave like ls D(tab) except it will move to the choices immediately without another tab.. even though have it set otherwise so maybe if want that option instead of setting menu_complete can add a wildcard?; does the same with param stuff; not sure if it's supposed to be like this
setopt GLOB_COMPLETE
# }}}
#==========Pushd Stuff and Dir Stack=====see dh alias# {{{
#http://zsh.sourceforge.net/Intro/intro_6.html
DIRSTACKSIZE=8

# if type the dir, will cd to it
setopt AUTO_CD

# This makes cd=pushd (also have auto cd)
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
setopt share_history

# }}}
# }}}
#==============================
# Bindings ( _Vim Stuff )# {{{
#==============================
. ~/.navigation.zsh
# enable vi mode on commmand line; no visual
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets
# link viins to main.. no option to link vicmd to main?
bindkey -v

# bind UP and DOWN arrow keys (caps)
bindkey -M viins '^[[A' history-substring-search-up
bindkey -M viins '^[[B' history-substring-search-down
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

# colemak (https://github.com/bunnyfly/dotfiles/blob/master/zshrc)# {{{
bindkey -M vicmd "h" backward-char
# bindkey -M vicmd "i" forward-char
bindkey -M vicmd "n" history-substring-search-down
bindkey -M vicmd "e" history-substring-search-up
# bindkey -M vicmd "s" vi-insert
# bindkey -M vicmd "S" vi-insert-bol
bindkey -M vicmd "k" vi-repeat-search
bindkey -M vicmd "K" vi-rev-repeat-search
bindkey -M vicmd "l" beginning-of-line
bindkey -M vicmd "L" end-of-line
bindkey -M vicmd "j" vi-forward-word-end
bindkey -M vicmd "J" vi-forward-blank-word-end
# }}}

# control backspace
bindkey -M viins "¸" backward-kill-word
# http://zshwiki.org./home/zle/bindkeys#why_isn_t_control-r_working_anymore
bindkey -M vicmd 't?' history-incremental-search-backward

# Clipboard# {{{
# http://unix.stackexchange.com/questions/25765/pasting-from-clipboard-to-vi-enabled-zsh-or-bash-shell
# paste from the system clipboard with p
vi-append-x-selection () { RBUFFER=$(xsel -ob </dev/null)$RBUFFER; }
zle -N vi-append-x-selection
bindkey -M vicmd p vi-append-x-selection

# change yank to yank to clipboard.# {{{
# http://zshwiki.org/home/zle/vi-mode
[[ -n $DISPLAY ]] && (( $+commands[xclip] )) && {

  function cutbuffer() {
    zle .$WIDGET
    echo $CUTBUFFER | xclip -selection clipboard
  }

  zle_cut_widgets=(
    vi-backward-delete-char
    vi-change
    vi-change-eol
    vi-change-whole-line
    vi-delete
    vi-delete-char
    vi-kill-eol
    vi-substitute
    vi-yank
    vi-yank-eol
  )
  for widget in $zle_cut_widgets
  do
    zle -N $widget cutbuffer
  done

  function putbuffer() {
    zle copy-region-as-kill "$(xclip -o)"
    zle .$WIDGET
  }

  zle_put_widgets=(
    vi-put-after
    vi-put-before
  )
  for widget in $zle_put_widgets
  do
    zle -N $widget putbuffer
  done
}

# }}}
# yank current dir to clipboard (not working)
# yank-cur-dir() { pwd | tr -d '\n' | xclip -selection clipboard }
# zle -N yank-cur-dir
# bindkey -a yp yank-cur-dir

# }}}

# general additional bindings
# for termite link mode from vi cmd mode..
enter-url-hint() { xdotool key --window Termite control+shift+x }
zle -N enter-url-hint
bindkey -a f enter-url-hint

# for tmux copy mode
enter-copy-mode() { tmux copy-mode }
zle -N enter-copy-mode
bindkey -a v enter-copy-mode

# 'leader' bindings
bindkey -a -r t
# for re-entering ranger # {{{
enter-ranger() { xdotool key control+d }
zle -N enter-ranger
bindkey -a tr enter-ranger
# }}}
# re-enter vim# {{{
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
bindkey -a tv fancy-ctrl-z
# }}}

# FZF# {{{
# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  local file
  file=$(fzf --query="$1" --select-1 --exit-0)
  [ -n "$file" ] && ${EDITOR:-vim} "$file"
}

# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-*} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}
# fda - including hidden directories
fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}
zle -N fda
bindkey -a td fd

# fkill - kill process
fkill() {
  ps -ef | sed 1d | fzf -m | awk '{print $2}' | xargs kill -${1:-9}
}
zle -N fkill
bindkey -a tk fkill

# select session with FZF
fs() {
  local session
  session=$(tmux list-sessions -F "#{session_name}" | \
    fzf --query="$1" --select-1 --exit-0) &&
  tmux switch-client -t "$session"
}
zle -N fs
bindkey sS fs
# }}}
 
# tmux experimentation {{{
# key sequences won't work if the first key is bound; it won't wait long enough to see if you've pressed the first key
# Thanks to Raphael Ahrens for explaining this to me: 
# http://unix.stackexchange.com/questions/122078/how-to-bind-a-key-sequence-to-a-widget-in-vi-cmd-mode-zsh/122088?noredirect=1#122088
bindkey -a -r r
bindkey -a -r s

# need to check if some of symbol bindings working
# "r" is redraw"{{{
# window switching"{{{
r-a() {tmux select-window -t 1}
zle -N r-a
bindkey -a ra r-a 

r-r() {tmux select-window -t 2}
zle -N r-r
bindkey -a rr r-r

r-s() {tmux select-window -t 3}
zle -N r-s
bindkey -a rs r-s

r-t() {tmux select-window -t 4}
zle -N r-t
bindkey -a rt r-t

r-d() {tmux select-window -t 5}
zle -N r-d
bindkey -a rd r-d

r-h() {tmux select-window -t 6}
zle -N r-h
bindkey -a rh r-h

r-n() {tmux select-window -t 7}
zle -N r-n
bindkey -a rn r-n

r-e() {tmux select-window -t 8}
zle -N r-e
bindkey -a re r-e

r-e() {tmux select-window -t 9}
zle -N r-i
bindkey -a ri r-i

r-o() {tmux select-window -t 10}
zle -N r-o
bindkey -a ro r-o
#}}}
# resize panes"{{{
r-m-h() {tmux resize-pane -L 10}
zle -N r-m-h
bindkey -a rmh r-m-h

r-m-n() {tmux resize-pane -D 5}
zle -N r-m-n
bindkey -a rmn r-m-n

r-m-e() {tmux resize-pane -U 5}
zle -N r-m-e
bindkey -a rme r-m-e

r-m-i() {tmux resize-pane -R 10}
zle -N r-m-i
bindkey -a rmi r-m-i
#}}}
# circulate# {{{
# previous
r-.() {tmux swap-pane -U}
zle -N r-.
bindkey -a 'r.' r-.

# next
r-,() {tmux swap-pane -D}
zle -N r-,
bindkey -a 'r,' r-,
# }}}
# new session
r-_() {tmux new-session}
zle -N r-_
bindkey -a 'r_' r-_

# new window 
r-c() {tmux new-window}
zle -N r-c
bindkey -a rc r-c

# kill pane
r-x() {tmux kill-pane}
zle -N r-x
bindkey -a rx r-x

# last window 
r-l() {tmux last-window}
zle -N r-l
bindkey -a rl r-l

# split windows
r-/() {tmux split-window -h}
zle -N r-/
bindkey -a 'r/' r-/

r--() {tmux split-window}
zle -N r--
bindkey -a 'r-' r--

# break pane
r-!() {tmux break-pane}
zle -N r-!
bindkey -a 'r!' r-!
#}}}

# "s" is select"{{{
# panes"{{{
# directions

s-h() {tmux select-pane -L}
zle -N s-h
bindkey -a sh s-h

s-n() {tmux select-pane -D}
zle -N s-n
bindkey -a sn s-n

s-e() {tmux select-pane -U}
zle -N s-e
bindkey -a se s-e

s-i() {tmux select-pane -R}
zle -N s-i
bindkey -a si s-i

# last
s-l() {tmux select-pane -l}
zle -N s-l
bindkey -a sl s-l

# select layout
s-v() {tmux select-layout main-vertical}
zle -N s-v
bindkey -a sv s-v

# toggle "monocle" (zoom)
monocle-toggle() {tmux resize-pane -Z}
zle -N monocle-toggle
bindkey -a st monocle-toggle
#}}}

# toggle bspwm monocle 
bspwm-monocle() {bspc desktop -l monocle && bspc window -t floating} 
zle -N bspwm-monocle
bindkey -a sm bspwm-monocle

# fullscreen
bspwm-fullscreen() {bspc window -t fullscreen} 
zle -N bspwm-fullscreen
bindkey -a sf bspwm-fullscreen

# select session
s-s() {tmux choose-client}
zle -N s-s
bindkey -a ss s-s

#}}}
#}}}

# }}}
#==============================
# Custom Dir Names {{{
#==============================
# benefit over alias is will show up as new name and fact that can use in aliases and functions
# will autocomplete from any directory
Desk=~/Desktop
books=~/ag-sys/Large/Library/Books

#}}}
#==============================
# _Aliases {{{
#==============================
#===============
# Startup and Shutdown# {{{
#===============
function xless() {
	tmux attach-session -dt xless || tmuxinator xless
}

reboot() {
	truecrypt -d ~/ag-sys/Else/ACCTS ; truecrypt -d ; devmon --unmount-all --no-gui
	sudo systemctl reboot
}

alias poweroff='sudo systemctl poweroff'

# get suspend working.. powerdown
# alias -g sus='systemctl suspend'
# }}}
#===============
# Symlink All Dotfiles With Stow# {{{
#===============
alias deploy='cd ~/dotfiles ; stow -vt ~/ common terminal private vim remap music aesthetics mail news browsing'
alias restow='cd ~/dotfiles ; stow -Rvt ~/ common terminal private vim remap music aesthetics mail news browsing'
# }}}
#===============
# Git Aliases# {{{
#===============
alias gin='git init'
alias -g gcl='git clone'
alias -g g='git'
alias gaa='git add -A'
alias gc='git commit'
alias gcm='git commit -m'
alias gl='git clone'
alias gp='git push'
# alias gh='git checkout'
alias -g gc='git commit'
alias -g gcm='git commit -m'
alias -g gcam='git commit -am'

# push to github (or whatever else)
# git remote add origin (link)
alias togh='git push -u origin master'
alias gd='git diff'
alias gds='git diff --staged'
function ga {
	git add "$1"
}
alias gs='git status'
alias gconf='git config --list'
# remove file from index but don't touch disk
alias -g gr="git rm --cached"
# }}}
#===============
# Reloading things {{{
#===============
# zshrc
alias rld='echo "The Matrix has been reloaded" && . ~/.zshrc'

# keyboard stuff# {{{
alias rldxmd='xmodmap ~/.Xmodmap'
# necessary because when plug in keyboard, it goes to qwerty
alias rldusbkeyboard='setxkbmap us -variant colemak && xmodmap ~/.Xmodmap_wide && xmodmap ~/.Xmodmap'
alias rldkbd='rldusbkeyboard'
# when using japanese keyboard with double wide mod and extra thumbkeys
alias rldjp='xmodmap ~/.Xmodmapjp'
# set caps to escape on tap if have to kill xcape
alias rldxcape="pkill xcape && xcape -e 'Mode_switch=Escape;Alt_L=Return;Shift_L=Return;ISO_Level3_Shift=cedilla'"
# alias rldxcape="xcape -e 'Mode_switch=Escape;Alt_L=Return;Hyper_R=cedilla;Super_L=grave'"
alias rldjcape="pkill xcape && xcape -e 'Alt_L=Escape;Shift_L=Return;Mode_switch=Escape;Hyper_R=cedilla'"
# }}}

alias rldurxvt='xrdb -load ~/.Xdefaults'
alias rldxdef='rldurxvt'
alias rldless='lesskey ~/.lesskey'

# xscreensaver after changing xresources
alias rldxscreen='xrdb -merge ~/.Xresources ; killall xscreensaver ; xscreensaver -no-splash &'

#update font caches
alias rldfonts='fc-cache -vf'
# add pretty much any font in ~/.fonts
# alias fonts='mkfontdir ~/.fonts;mkfontscale ~/.fonts;xset +fp ~/.fonts;xset fp rehash;fc-cache;fc-cache -fv'

#reload time
alias rldtime='sudo /usr/sbin/ntpdate us.pool.ntp.org'
# set hardware clock to system time
alias fixhwclock='rldtime && hwclock --systohc'
# alias rldxmon='xmonad --recompile ; xmonad --restart'

alias rldsxhkd='pkill -USR1 -x sxhkd'

#}}}
#===============
# General/Random {{{
#===============
# Super user
alias _='sudo'
alias please='sudo'
alias fucking='sudo'

# change hostname
alias -g chhost='hostnamectl set-hostname'

# pk thunder
alias -g pk='pkill'

# vim as password manager
alias vpass="mountacct ; vim -u ~/.encrypted_vimrc -x ~/blemish/accts.vault"

# create file then open with vim
tv() {
	touch $1
	vim $1
}
# for opening in already open gvim session
# alias -g gvir="gvim --remote"
gvir() {
	gvim --remote $1
}

# set wallpaper
alias bgset='feh --bg-scale'

# Show history
alias history='fc -l 1'

# wm stuff
alias checkclass='xprop | grep WM_CLASS'

# Package Management {{{
alias pacss="pacman -Ss"
alias -g pacs='pacman -S'
alias pacq='pacman -Q'
alias pacqm="pacman -Qm"
alias -g pacupd='pacman -Syu'
alias -g pacr="pacman -Rns"
alias -g pacrefls='pacman -Syy'

alias yass='yaourt -Ss'
alias yas='yaourt -S'
alias yapd='yaourt -Syu --aur'
alias yaqm='yaourt -Qm'

alias aur='sudo aura -A'
alias aurs='sudo aura -As'
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
alias nb='newsbeuter'
alias bitl='sudo bitlbee -D'
alias mutt='cd ~/Move && mutt'
alias ppss='PPSSPPSDL &'

# check ip; default gateway / router
alias gateip='ip route show'

# check what DNS using
alias showdns='cat /etc/resolv.conf'

# empty trash
# alias -g rmtrash='rm -rf ~/.local/share/Trash'
alias rmtrash='trash-empty'
alias lstrash='trash-list'

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

# lol aliases {{{
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
# Screen Shot/Record# {{{
#===============
# Screen Recording; automatically works with external mic; q to quit and save
alias srec="ffmpeg -f alsa -ac 2 -i hw:0,0 -f x11grab -r 30 -s 1366x768 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -threads 0 output.mkv"

alias zshot="scrot -q 75 ~/Move/Screenshots/'%m.%d.%y_%H:%M:%S_-_$wx$h.png'"
# }}}
#===============
# Directory Stuff {{{
#===============
# copying dir with shell command
# (need to install xclip or alternatively use xsel)
# copy working directory to clipboard
alias cpwd="pwd | tr -d '\n' | xclip -selection clipboard"

# -p: make dirs if don't exist
alias mdp='mkdir -p'

# ls stuff {{{
alias l='ls -a'
alias lsa='ls -lah'
alias ll='ls -l'
# keep sl for steam locomotive choochoo (sl-patched)
# http://jeff.robbins.ws/reference/my-zshrc-file
alias lsd='ls -d'
alias lr='ls -tRFh'   #sorted by date,recursive,show type,human readable
alias lst='ls -ltFh'   #long list,sorted by date,show type,human readable
# no group names
# alias lsd='ls -ahlG'

# these require zsh
alias ltd='ls *(m0)' # files & directories modified in last day
alias lt='ls *(.m0)' # files (no directories) modified in last day
alias lnew='ls *(.om[1,3])' # list three newest
# most recent subdir
alias lsrdir='ls -d *(/om[1])'

alias lh='ls -d .*' # show hidden files/directories only
alias tree="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
#}}}
# cd stuff {{{
# custom cds alias home='cd ~/'
alias dot='cd ~/dotfiles'
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
# check free space
alias fspace='df -h'
# finding largest dirs:
# directories sorted by size
alias dus='du -sckx * | sort -nr'
# colored disk usage; colour intelligently; sort; human readable sizes
alias diskspace='cdu -isdh'
alias dis='diskspace'
# there's also ncdu.. but it doesn't support key rebinding I believe, so fuck it
# number of files (not directories)
alias filecount='find . -type f | wc -l'
#}}}

#}}}
#===============
# Ranger # {{{
#===============
# alias rn='ranger'
rn() {
 case $1 in     
  dwn) ranger ~/Move ;;
  vim) ranger ~/.vim ;;
  *)   ranger ;;
 esac
}

# Image Rotation (imagemagick)
imrotate() {
	mogrify -rotate 90 $1
}

# extract archive then delete it
exd() {
	atool -x $1 && rm $1
}

# }}}
#===============
# _Music, Video, & Sound {{{
#===============
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
alias mpdst='mpd ~/.mpd/mpd.conf ; mpdscribble'
# kill vimus and mpd
alias -g mpkill='pkill mpd ; pkill vimus'

# pointing to config after just created; uneccesary
alias nm='ncmpcpp -c ~/.ncmpcpp/config'

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

# connecting to tv
alias hdmiin='xrandr --output HDMI1 --auto && ponymix set-profile output:hdmi-stereo'
alias hdmiout='xrandr --output HDMI1 --off && ponymix set-profile output:analog-stereo'
alias hdmiadd='xrandr --output HDMI1 --auto --right-of LVDS1 && bspc monitor HDMI1 -a X && ponymix set-profile output:hdmi-stereo'
#}}}
#===============
# _Gaming {{{
#===============
# alias xbox='sudo xboxdrv --silent --detach-kernel-driver'
# alias controller='xbox'
#}}}
#===============
# Android Music Syncing # {{{
#===============
# alias -g mountand='jmtpfs -o allow_other ~/samsung'
alias mountand='jmtpfs ~/samsung'
alias -g umountand='fusermount -u ~/samsung'

# restart adb
alias adbrestart='adb kill-server ; adb start-server'
# reload udev rules: (sudo)
alias -g andudevreload='udevadm control --reload-rules'

alias -g snandmus='andmusconfig1'
alias -g andmusconfig1='sync30sec && syncimogen && syncadtr && syncbaao && syncblonde && syncbrandnew && syncbmth && syncchevelle && synccoldplay && syncdgd && syncdeadmau5 && syncdeftones && syncfob && syncffaf && syncgreenday && syncimogen && synckeane && synclydia && syncmuse && syncmcr && syncofmm && syncparamore && syncptv && syncradiohead && syncrage && syncra && syncsenses && syncskrillex && syncsmashingpumpkins && syncsoad && synckillers && syncrja && syncsleeping && synctssf && syncstrokes && syncthrice && syncuverworld'

# by bands:# {{{
alias -g sync30sec='rsync -vr --delete ~/Music/30\ Seconds\ to\ Mars ~/and/Card/Music'
alias -g syncadtr='rsync -vr --delete ~/Music/A\ Day\ To\ Remember ~/and/Card/Music'
alias -g syncalexd='rsync -vr --delete ~/Music/Asking\ Alexandria ~/and/Card/Music'
alias -g syncbaao='rsync -vr --delete ~/Music/Being\ As\ An\ Ocean ~/and/Card/Music'
alias -g syncblonde='rsync -vr --delete ~/Music/Blonde\ Redhead ~/and/Card/Music'
alias -g syncbrandnew='rsync -vr --delete ~/Music/Brand\ New ~/and/Card/Music'
alias -g syncbmth='rsync -vr --delete ~/Music/Bring\ Me\ the\ Horizon ~/and/Card/Music'
alias -g syncchevelle='rsync -vr --delete ~/Music/Chevelle ~/and/Card/Music'
alias -g synccoldplay='rsync -vr --delete ~/Music/Coldplay ~/and/Card/Music'
alias -g syncdgd='rsync -vr --delete ~/Music/Dance\ Gavin\ Dance ~/and/Card/Music'
alias -g syncdeadmau5='rsync -vr --delete ~/Music/Deadmau5 ~/and/Card/Music'
alias -g syncdeftones='rsync -vr --delete ~/Music/Deftones ~/and/Card/Music'
alias -g syncfob='rsync -vr --delete ~/Music/Fall\ Out\ Boy ~/and/Card/Music'
alias -g syncffaf='rsync -vr --delete ~/Music/Funeral\ For\ A\ Friend ~/and/Card/Music'
alias -g syncgreenday='rsync -vr --delete ~/Music/Green\ Day ~/and/Card/Music'
alias -g syncimogen='rsync -vr --delete ~/Music/Imogen\ Heap ~/and/Card/Music'
alias -g synckeane='rsync -vr --delete ~/Music/Keane ~/and/Card/Music'
alias -g synclydia='rsync -vr --delete ~/Music/Lydia ~/and/Card/Music'
alias -g syncmuse='rsync -vr --delete ~/Music/Muse ~/and/Card/Music'
alias -g syncmcr='rsync -vr --delete ~/Music/My\ Chemical\ Romance ~/and/Card/Music'
alias -g syncofmm='rsync -vr --delete ~/Music/Of\ Mice\ \&\ Men ~/and/Card/Music'
alias -g syncparamore='rsync -vr --delete ~/Music/Paramore ~/and/Card/Music'
alias -g syncptv='rsync -vr --delete ~/Music/Pierce\ The\ Veil ~/and/Card/Music'
alias -g syncradiohead='rsync -vr --delete ~/Music/Radiohead ~/and/Card/Music'
alias -g syncrage='rsync -vr --delete ~/Music/Rage\ Against\ The\ Machine ~/and/Card/Music'
alias -g syncra='rsync -vr --delete ~/Music/Rise\ Against ~/and/Card/Music'
alias -g syncsenses='rsync -vr --delete ~/Music/Senses\ Fail ~/and/Card/Music'
alias -g syncskrillex='rsync -vr --delete ~/Music/Skrillex ~/and/Card/Music'
alias -g syncsmashpumpkins='rsync -vr --delete ~/Music/Smashing\ Pumkins ~/and/Card/Music'
alias -g syncsoad='rsync -vr --delete ~/Music/System\ Of\ A\ Down ~/and/Card/Music'
alias -g synckillers='rsync -vr --delete ~/Music/The\ Killers ~/and/Card/Music'
alias -g syncrja='rsync -vr --delete ~/Music/The\ Red\ Jumpsuit\ Apparatus ~/and/Card/Music'
alias -g syncsleeping='rsync -vr --delete ~/Music/The\ Sleeping ~/and/Card/Music'
alias -g synctssf='rsync -vr --delete ~/Music/The\ Story\ So\ Far ~/and/Card/Music'
alias -g syncstrokes='rsync -vr --delete ~/Music/The\ Strokes ~/and/Card/Music'
alias -g syncthrice='rsync -vr --delete ~/Music/Thrice ~/and/Card/Music'
alias -g syncuverworld='rsync -vr --delete ~/Music/UVERworld ~/and/Card/Music'
# }}}
# }}}
#===============
# _Backup & Mounting # {{{
#===============
# add back dropbox
function bkhelp() {
	echo "
	sndot            - sync dotfiles & vimwiki to ag-sys
	bahamut (online) - backup soma to usb (or ~/grive and sync to google drive)
	sin              - small/quick online encrypted backup
	singluttony      - larger minimal online backup
	snhometoexternal - sync ~/ to external hard drive
	syncdatab        - sync Datab to external hard drive
	snpspbk          - backup psp memcard to database
	snpspdata        - backup psp savedata to database
	"
}

# Mounting# {{{
# unmount most recent external drive
alias uned="devmon -c"
# unmount all external
alias uneda="devmon -u"
# eject disk
alias ej="sudo eject /dev/sr0"
alias umountalltc="truecrypt -t -d"
# }}}

# TC mounting# {{{
function mount_tc() {
	tc_volume=$1
	mount_point=$2
	if [ "$(truecrypt -t -l | grep $tc_volume)" != "" ];then
		echo "Already mounted."
	else
		if [ ! -f $tc_volume ];then
			echo "Error. The specified path ($tc_volume) for the tc volume does not exist."
			kill -INT $$
		elif [ "$(ls -A $mount_point)" ];then
			echo "Error. Files exist in ($mount_point). Move/delete them."
			kill -INT $$
		else
			mkdir -p $mount_point
			truecrypt -t $tc_volume $mount_point
		fi
	fi
}

# alias -g mountacct='truecrypt ~/ag-sys/Else/ACCTS ~/blemish'
# may do for other mounting w/ pass
mountacct() {
	truecrypt -p "$(gpg2 --for-your-eyes-only --no-tty -d ~/.pass.gpg | grep accts | awk '{print $2}')" ~/ag-sys/Else/ACCTS ~/blemish
}

alias umountacct="truecrypt -t -d ~/ag-sys/Else/ACCTS"

# }}}

# Shared# {{{
alias mountsoma="mount_tc $HOME/soma_ $HOME/ag-sys/"
alias umountsoma="truecrypt -t -d ~/soma_"
# sync ~/grive to google drive
alias sngdrive="cd ~/grive/ ; grive -V"

function sndot() {
	rsync -avrh --progress --delete --exclude={".antigen/*","bundle/*","elpa/*",".mpd/log"} ~/dotfiles ~/ag-sys/Backup/
	rsync -avrh --progress --delete ~/vimwiki ~/ag-sys/Backup/
}
# }}}

# soma backup# {{{
# now mount to ag-sys-bk; change from run media
alias mountbkdrivesoma="mount_tc /media/ag-sys/soma $HOME/ag-sys-bk"
alias umountbkdrivesoma="truecrypt -t -d /media/ag-sys/soma"

alias mountbkonlsoma="mount_tc $HOME/grive/soma_bk $HOME/ag-sys-bk-onl"
alias umountbkonlsoma="truecrypt -t -d ~/grive/soma_bk"

function bahamut() {
	# http://stackoverflow.com/questions/1885525/how-do-i-prompt-a-user-for-confirmation-in-bash-script
	echo "Using --delete with rsync. Continue? (y/n)"
	# read is different for zsh
	read -q REPLY
	echo
	if [[ ! $REPLY =~ ^[Yy]$ ]]
	then
		kill -INT $$
	fi
	mountsoma && sndot
	if [ "$1" == "online" ];then
		mountbkonlsoma && \
		rsync -avrh --progress --delete ~/ag-sys/ ~/ag-sys-bk-onl
		umountbkonlsoma
		sngdrive
	else
		# to usb
		mountbkdrivesoma && \
		rsync -avrh --progress --delete ~/ag-sys/ ~/ag-sys-bk
		umountbkdrivesoma
	fi
}

# }}}

# quick net backup# {{{
# add grive option
# ~9mb
 
alias mountsmallestbktc="mount_tc $HOME/grive/smallest_soma_bk $HOME/smallest_bk"
alias umountsmallestbktc="truecrypt -t -d ~/grive/smallest_soma_bk"

function sin() {
	mountsoma && sndot ; mountsmallestbktc && \
	rsync -avrh --exclude={".Trash/*",".git/*","Portable/*","Gaming/*","Large/*",".themes/*","tmux-powerline/*","bundle/*",".weechat/*","undo/*","Customization/win/*","chats/*"} --include={"*/","*.txt","*.rb","*.py","*.pl","*.hs","*.lua","*.el","*.cpp","*.js","*.penta","*.vim","*.ini","*.xml","*.conf","*.json","*.ytcs","*.tex","*.md","*.mkd","*.mkdn","*config*","*.zsh_history","*.yaml","*.Xmodmap*","*.Xdefaults",".Xresources",".emacs",".gitconfig",".lesskey","xboxdrv",".gtkrc-2.0","*xscreensaver*","*tmuxline*","*keymap*","*navigation*","dotfiles/**rc",".unite/*","TO\ Backup/*","bin/**sh","termite/*","ncmpcpp/*","herbstluftwm/*","ranger/*","surfraw/*",".abook/*","root/*","sxiv/**","panel/**"} --exclude='*' --prune-empty-dirs ~/ag-sys/ ~/smallest_bk
	umountsmallestbktc && SpiderOak --batchmode --backup='~/grive/smallest_soma_bk'
}
# }}}

# singluttony# {{{
# next most frequently changed/ updated filetypes
# about 80 megs; 120meg tc up to date basic backup; second smallest backup but with odt, rtf, html, and some pdf (rtf is all old stuff)
alias mountsmallbktc="mount_tc $HOME/grive/small_soma_bk $HOME/small_bk"
alias umountsmallbktc="truecrypt -d ~/grive/small_soma_bk"

function singluttony() {
	mountsoma && sndot ; mountsmallbktc && \
	rsync -avrh --exclude={".Trash/*","School/older/*","immunization/*","Library/*","Dictionary/*","*other/**.pdf","Gaming/**.pdf","Programming/**.pdf","Languages/*","\#\#*/**.pdf"} --include={"*/","*.txt","*.odt",".weechat/**","forums/**","*.rtf","*.doc","*.docx","*.pdf","*.html","_used_pics/*"} --exclude='*' --prune-empty-dirs ~/ag-sys/ ~/small_bk
	umountsmallbktc && SpiderOak --batchmode --backup='~/grive/small_soma_bk'
}

# }}}

# home backup# {{{
# add music back; smarter soma sync; add important from mail, wine, etc.
# add more useless dirs to exclude
function snhometoexternal() {
	echo "Using --delete with rsync. Continue? (y/n)"
	read -q REPLY
	echo
	if [[ ! $REPLY =~ ^[Yy]$ ]]
	then
		echo
	else
		if [ -d /media/HD-CEU2\ Backup/home_folder ];then
			rsync -avrh --progress --delete --exclude={".cache/*","grive/*","Dropbox/*","Datab","soma*","Steam/*",".mail/*",".wine/*",".wine_64/*","Music/*","VirtualBox\ VMs/*","ag-sys/*","database/*","blemish/*","private/*","*bk/*","Games/*",".local/share/*"} --prune-empty-dirs ~/ /media/HD-CEU2\ Backup/home_folder
		else
			echo "Error. External hard drive is not mounted or directory is named differently."
		fi
	fi
}
 # }}}

# backup database to external harddrive# {{{
alias mountdatab="mount_tc $HOME/Datab $HOME/database"
alias umountdatab="truecrypt -d ~/Datab"

alias mountbkdatab="mount_tc /media/HD-CEU2\ Backup/Datab $HOME/database-bk"
alias umountbkdatab="truecrypt -d /media/HD-CEU2\ Backup/Datab"

function syncdatab() {
	echo "Using --delete with rsync. Continue? (y/n)"
	read -q REPLY
	echo
	if [[ ! $REPLY =~ ^[Yy]$ ]]
	then
		echo
	else
		mountdatab && mountbkdatab && \
		rsync -avrh --progress --delete ~/database/ ~/database-bk
		umountbkdatab
	fi
}

# }}}

# psp backup {{{
# vita
alias cm="qcma --verbose"

# leave out psp isos?
snpspbk() {
	echo "Using --delete with rsync. Continue? (y/n)"
	read -q REPLY
	echo
	if [[ ! $REPLY =~ ^[Yy]$ ]]
	then
		echo
	else
		if [ -d /media/MS0/ ];then
			mountdatab && \
			rsync -avrh --progress --delete /media/MS0/ ~/database/database/Gaming/handheld/PSP/PSP\ Backup/
		else
			echo "Error. PSP is not mounted or directory is named differently."
		fi
	fi
}

# save databackup
snpspdata() {
	echo "Using --delete with rsync. Continue? (y/n)"
	read -q REPLY
	echo
	if [[ ! $REPLY =~ ^[Yy]$ ]]
	then
		echo
	else
		if [ -d /media/MS0/ ];then
			mountdatab && \
			rsync -avrh --progress --delete /media/MS0/SAVEDATA ~/database/database/Gaming/handheld/PSP/savedata/
		else
			echo "Error. PSP is not mounted or directory is named differently."
		fi
	fi
}
# }}}

# }}}
#===============
# Other Functions# {{{
#===============
# my conversion function # {{{
# [1.11.14]
# requirements: 
# unoconv
# directories should not have . in the name

# run and convert all rtf files to txt and moves them to folder you ran it in/graveyard in parallel folder structure
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

# }}}

# }}}
# }}}
#==============================

# for tmux powerline vcs stuff
export PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'
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
