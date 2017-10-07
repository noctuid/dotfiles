# for testing startup time
# zmodload zsh/zprof
# also see ./.profile
# TODO:
# switch to aura or pacaur
# if start using other package managers frequently, make generalized functions (e.g. 'install','search')
# consider removing stuff don't use or putting in another file
# (never use pushd/popd/dir stack stuff or really fzf or fasd)
# clean guix and nix aliases

# interesting:
# expanding global aliases; http://blog.patshead.com/2011/07/automatically-expanding-zsh-global-aliases-as-you-type.html?r=related

# progress bars:
# https://github.com/DeeNewcum/dotfiles/blob/master/bin/eta
# http://www.theiling.de/projects/bar.html
# https://github.com/Xfennec/cv
# e.g. dd if=$source | pv | dd of=$target

# which, whence, type -a, whereis, whatis
#==============================
# * Sources
#==============================
# Some settings or stuff got/find out about from here:
# https://wiki.archlinux.org/index.php/Zsh
# http://zsh.sourceforge.net/Doc/Release/Zsh-Modules.html
# https://github.com/Adalan/dotfiles/blob/master/zsh/setopt.zsh#L33
# http://www.bash2zsh.com/zsh_refcard/refcard.pdf
# http://stackoverflow.com/questions/171563/whats-in-your-zshrc
# https://github.com/Adalan/dotfiles/blob/master/zsh/functions.zsh
# https://wiki.gentoo.org/wiki/Zsh/HOWTO#
# https://github.com/phallus/arch-files/blob/master/config/.zshrc

#==============================
# * Plugins
#==============================
if [[ ! -f ~/.zplug/init.zsh ]]; then
	git clone https://github.com/b4b4r07/zplug ~/.zplug
fi

# in case clone fails
if [[ -f ~/.zplug/init.zsh ]] ; then
	source ~/.zplug/init.zsh

	# theme
	zplug "denysdovhan/spaceship-zsh-theme", use:spaceship.zsh, as:theme, \
		hook-load:spaceship_vi_mode_enable
	# above has to be run before loading zsh-autopair
	export SPACESHIP_DIR_TRUNC=5

	# syntax highlighting
	# zplug "jimmijj/chromatic-zsh"
	# load after compinit and sourcing other plugins
	# (should be loaded after autosuggestions)
	# zplug "zsh-users/zsh-syntax-highlighting", defer:2
	zplug "zdharma/fast-syntax-highlighting", defer:3
	# color parens and highlight matching paren
	export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

	# autosuggestions
	zplug "tarruda/zsh-autosuggestions", use:"zsh-autosuggestions.zsh"

	# history substring search (should be loaded after syntax highlighting)
	# TODO https://github.com/zplug/zplug/issues/314
	zplug "zsh-users/zsh-history-substring-search", defer:3

	# completion stuff
	zplug "lib/completion", from:oh-my-zsh, ignore:oh-my-zsh.sh
	zplug "zsh-users/zsh-completions"
	zplug "aramboi/zsh-ipfs"
	# provides ** tab completion for various commands:
	zplug "junegunn/fzf", use:"shell/completion.zsh"

	# prettier ls
	zplug "supercrabtree/k"

	# good interactive (by default when more than 1 match) alternative to fasd
	zplug "b4b4r07/enhancd", use:enhancd.sh

	zplug "hlissner/zsh-autopair", defer:2

	# notifications when long running commands complete
	# https://gist.github.com/jpouellet/5278239
	# also see https://gist.github.com/oknowton/8346801
	zbell_ignore=(man less vimpager vim emacs rn ranger mutt nm ncmpcpp vimus weechat)
	zplug "jpouellet/5278239", from:gist, use:zbell.sh

	# interesting but not useful for me at the moment
	# Tarrasch/zsh-autoenv
	# hchbaw/auto-fu.zsh
	# https://github.com/joepvd/zsh-hints
	# https://github.com/hchbaw/zce.zsh
	# https://github.com/willghatch/zsh-snippets
	# maybe use at some point
	# https://github.com/bric3/nice-exit-code
	# I think doing these in emacs would be preferable
	# https://github.com/voronkovich/gitignore.plugin.zsh
	# https://github.com/peterhurford/git-it-on.zsh
	# https://github.com/adolfoabegg/browse-commit
	# https://github.com/Tarrasch/zsh-functional

	# install plugins if not all are installed
	if ! zplug check; then
		zplug install
	fi

	# source plugins and add commands to $PATH
	zplug load
	# prevents up/down on empty line from causing crash when using history substring search
	ZSH_AUTOSUGGEST_CLEAR_WIDGETS=("${(@)ZSH_AUTOSUGGEST_CLEAR_WIDGETS:#(up|down)-line-or-history}")
	ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(history-substring-search-up history-substring-search-down)
fi

#==============================
# Completion Settings {{{
#==============================
# http://askql.wordpress.com/2011/01/11/zsh-writing-own-completion/
# add custom completion scripts
fpath=(~/.zsh/completion $fpath)

# compsys initialization
autoload -U compinit
compinit

# show completion menu when number of options is at least 2
zstyle ':completion:*' menu select=2

# }}}
#==============================
# Appearance {{{
#==============================
# colored less manpages
# similar to things like colored-man; don't remember where found this
export LESS_TERMCAP_mb=$'\E[01;31m'             # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'             # begin bold
export LESS_TERMCAP_me=$'\E[0m'                 # end mode
export LESS_TERMCAP_se=$'\E[0m'                 # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m'          # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'                 # end underline
export LESS_TERMCAP_us=$'\E[01;32m'             # begin underline

# for if have entered shell from ranger
# https://github.com/hut/ranger/blob/bdd6bf407ab22782f7ddb3a1dd24ffd9c3361a8d/examples/bash_subshell_notice.sh
[[ -n $RANGER_LEVEL ]] && export PS1="$PS1"'%{$fg[red]%}ranger> '

# }}}
#==============================
# Options {{{
#==============================
# http://linux.die.net/man/1/zshoptions ($ man zshoptions)
# rc_expand_param is interesting
#===============
# General {{{
#===============
# 10 second wait then prompt if use rm with *
setopt RM_STAR_WAIT
# prompt if removing more than 3 files; don't delete anything from ~/.config/safe-rm
# trying to almost always use trash-cli instead of rm
alias rm='safe-rm -I'

# show nonzero exit codes
setopt print_exit_value

# allow comments with #
setopt interactive_comments

# no beeps
setopt no_beep

# default
# http://zsh.sourceforge.net/Doc/Release/Redirection.html#Multios
# setopt multios

# }}}
#===============
# Prompt {{{
#===============
# enable parameter expansion, command substitution, and arithmetic expansion in the prompt
# without, theme doesn't work.. just get $fg.. crap
setopt prompt_subst
# only show the rprompt on the current prompt
# unsetting will keep showing "insert" on each line as the rprompt for mode
setopt transient_rprompt

# }}}
#===============
# Correction {{{
#===============
# spell check commands and offer correction (pdw > pwd)
setopt correct
# spell check arguments
setopt correct_all

# }}}
#===============
# Completion {{{
#===============
# if on, tab completion will just show files/dirs instead of completion would do if typed out alias contents
unsetopt complete_aliases

# when completing from the middle of a word, move the cursor to the end of the word; haven't noticed staying in middle of word even when unset; confusion
unsetopt always_to_end

# do not autoselect the first completion entry
unsetopt menu_complete

# show completion menu on successive tab press (menu_complete overrides)
setopt auto_menu

# default; lists choiches on ambiguous completion; won't go to first item (except with wildcard and glob_complete)
setopt auto_list

# will show name for directory if you have it set; ex if have DS=~/Desktop (ex with '%~' in prompt sequence), it will show DS instead of desktop if you cd into it; see CDable Vars section
setopt auto_name_dirs

# default; will put / instead of space when autocomplete for param (ex ~D(tab) puts ~D/)
setopt auto_param_slash

# allow completion from within a word/phrase
# ex: completes to Desktop/ from Dktop with cursor before k)
setopt complete_in_word

# }}}
#===============
# Globbing {{{
#===============
# treat #, ~, and ^ as part of patterns for filename generation; ex ^ negates following pattern (ls -d ^*.c)
# ls *.png~Selection_005.png now will exclude that file frome results
# http://www.refining-linux.org/archives/37/ZSH-Gem-2-Extended-globbing-and-expansion/
# http://zsh.sourceforge.net/Doc/Release/Expansion.html
# http://www.linuxjournal.com/content/bash-extended-globbing
# probably will never use
setopt extended_glob

# if unset and do something like ls D* it will add anything that matches that to the line (ex ls Desktop/ Downloads/); with it set, will act like menu_complete; uses pattern matching; can use wih complete_in_word
setopt glob_complete

# }}}
#===============
# Pushd Stuff and Dir Stack {{{
#===============
# see dh alias
# http://zsh.sourceforge.net/Intro/intro_6.html
DIRSTACKSIZE=8

# if type the dir, will cd to it
setopt auto_cd

# http://zsh.sourceforge.net/Intro/intro_16.html
# if a var is a directory, can cd to it
setopt cdable_vars

# This makes cd=pushd (also have auto cd)
setopt auto_pushd

# blank pushd goes to home; default already I think
setopt pushd_to_home

# swap meaning of cd -num and cd +; dirs -v then cd -num of directory you want to switch to
setopt pushd_minus

# don't push multiple copies of the same directory onto the directory stack
setopt pushd_ignore_dups

# no pushd messages
setopt pushd_silent

# }}}
#===============
# History {{{
#===============
# lots of it
HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zsh_history

# remove command from history list when first character on the line is a space
setopt hist_ignore_space

# remove extra blanks from each command line being added to history
setopt hist_reduce_blanks

# don't execute, just expand history
setopt hist_verify

# allow multiple terminal sessions to all append to one zsh command history instead of replacing file; default
setopt append_history

# save timestamp of command and duration; begginingtime:elapsedseconds
setopt extended_history

# add comamnds as they are typed, don't wait until shell exit
setopt inc_append_history

# when trimming history, lose oldest duplicates first
setopt hist_expire_dups_first

# do not write events to history that are duplicates of previous command
setopt hist_ignore_dups

# imports new commands and appends typed commands to history
setopt share_history

# }}}
#===============
# }}}
#==============================
# Key Bindings {{{
#==============================
# enable vi mode on commmand line; no visual
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html
# link viins to main.. no option to link vicmd to main?
# for bash there is https://github.com/ardagnir/athame
# alternatively run shell in neovim or emacs
bindkey -v

# bind UP and DOWN arrow keys (on caps or thumbkey)
bindkey -M viins '^[[A' history-substring-search-up
bindkey -M viins '^[[B' history-substring-search-down
# fix home, end, etc. keys (with vim mode)
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
# -a is same as -M vicmd
bindkey -a u undo
bindkey -a U redo
bindkey '^?' backward-delete-char
bindkey '^H' backward-delete-char
# swap
bindkey -a a vi-add-eol
bindkey -a A vi-add-next

# colemak
# https://github.com/bunnyfly/dotfiles/blob/master/zshrc
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

# control backspace
bindkey -M viins "¸" backward-kill-word
# http://zshwiki.org./home/zle/bindkeys#why_isn_t_control-r_working_anymore
bindkey -M vicmd 't?' history-incremental-pattern-search-backward

# clipboard {{{
# http://unix.stackexchange.com/questions/25765/pasting-from-clipboard-to-vi-enabled-zsh-or-bash-shell
# paste from the system clipboard with p
function vi-append-x-selection() {
	RBUFFER=$(xsel -ob </dev/null)$RBUFFER
}
zle -N vi-append-x-selection
bindkey -M vicmd p vi-append-x-selection

# ecaped paste
function escape-paste-clipboard() {
	RBUFFER=$(printf '%q' "$(xsel -ob </dev/null)")$RBUFFER
}
zle -N escape-paste-clipboard
bindkey -M vicmd P escape-paste-clipboard

# change yank to yank to clipboard. {{{
# http://zshwiki.org/home/zle/vi-mode
[[ -n $DISPLAY ]] && (( $+commands[xsel] )) && {

  function cutbuffer() {
    zle .$WIDGET
    echo $CUTBUFFER | xsel -ib
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
    zle copy-region-as-kill "$(xsel -ob)"
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

# }}}

# general additional bindings
# for termite link mode from vi cmd mode..
function enter-url-hint() {
	xdotool key --window Termite control+shift+x
}
zle -N enter-url-hint
bindkey -a f enter-url-hint

# for tmux copy mode
function enter-copy-mode() {
	tmux copy-mode
}
zle -N enter-copy-mode
bindkey -a v enter-copy-mode

# 'leader' bindings
bindkey -a -r t

# run man page of current command on cli and keep there when exit manpage
bindkey -a tm run-help

# for re-entering ranger {{{
function enter-ranger() {
	xdotool key control+d
}
zle -N enter-ranger
bindkey -a tr enter-ranger

# }}}

# re-enter vim {{{
# may never use..
# http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
function fancy-ctrl-z () {
	if [[ $#BUFFER == 0 ]]; then
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

# tmux experimentation {{{
# TODO: if keep this, see if can setup keybindings with loop

# key sequences won't work if the first key is bound; it won't wait long enough to see if you've pressed the first key
# Thanks to Raphael Ahrens for explaining this to me: 
# http://unix.stackexchange.com/questions/122078/how-to-bind-a-key-sequence-to-a-widget-in-vi-cmd-mode-zsh/122088?noredirect=1#122088
bindkey -a -r r
bindkey -a -r s

# need to check if some of symbol bindings working
# "r" is redraw {{{
# window switching {{{
function r-a() { wm_action inpt ra; }
zle -N r-a
bindkey -a ra r-a 

function r-r() { wm_action inpt rr; }
zle -N r-r
bindkey -a rr r-r

function r-s() { wm_action inpt rs; }
zle -N r-s
bindkey -a rs r-s

function r-t() { wm_action inpt rt; }
zle -N r-t
bindkey -a rt r-t

function r-d() { wm_action inpt rd; }
zle -N r-d
bindkey -a rd r-d

function r-h() { wm_action inpt rh; }
zle -N r-h
bindkey -a rh r-h

function r-n() { wm_action inpt rn; }
zle -N r-n
bindkey -a rn r-n

function r-e() { wm_action inpt re; }
zle -N r-e
bindkey -a re r-e

function r-i() { wm_action inpt ri; }
zle -N r-i
bindkey -a ri r-i

function r-o() { wm_action inpt ro; }
zle -N r-o
bindkey -a ro r-o

# }}}

# resize panes {{{
function r-m-h() { wm_action inpt rmh; }
zle -N r-m-h
bindkey -a rmh r-m-h

function r-m-n() { wm_action inpt rmn; }
zle -N r-m-n
bindkey -a rmn r-m-n

function r-m-e() { wm_action inpt rme; }
zle -N r-m-e
bindkey -a rme r-m-e

function r-m-i() { wm_action inpt rmi; }
zle -N r-m-i
bindkey -a rmi r-m-i

# }}}

# circulate {{{
# previous
function r-.() { wm_action inpt r.; }
zle -N r-.
bindkey -a 'r.' r-.

# next
function r-comma() { wm_action inpt r,; }
zle -N r-comma
bindkey -a 'r,' r-comma

# }}}

# new window
function r-c() { wm_action inpt rc; }
zle -N r-c
bindkey -a rc r-c

# kill pane
function r-x() { wm_action inpt rx; }
zle -N r-x
bindkey -a rx r-x

# last window 
function r-l() { wm_action inpt rl; }
zle -N r-l
bindkey -a rl r-l

# split windows
function r-slash() { wm_action inpt r/; }
zle -N r-slash
bindkey -a 'r/' r-slash

function r--() { wm_action inpt r-; }
zle -N r--
bindkey -a 'r-' r--

# break pane
function r-bang() { wm_action inpt rbang; }
zle -N r-bang
bindkey -a 'r!' r-bang

# from old "s" {{{
# last pane
function r-u() { wm_action inpt ru; }
zle -N r-u
bindkey -a ru r-u

# zoomed pane toggle
function r-k() { wm_action inpt rk; }
zle -N r-k
bindkey -a rk r-k

# fullscreen
function r-f() { wm_action inpt rf; }
zle -N r-f
bindkey -a rf r-f

# sticky
function r-y() { wm_action inpt ry; }
zle -N r-y
bindkey -a ry r-y

# main-vertical layout
function r-v() { wm_action inpt rv; }
zle -N r-v
bindkey -a rv r-v

# }}}
# }}}
# }}}
# }}}
#==============================
# FASD and FZF / Navigation {{{
#==============================
# Programs like fasd and shell fuzzy finders like FZF and percol are awesome.
# I don't end up using them much though since they aren't very useful
# with a workflow based inside the editor (e.g. magit/fugitive over cli git,
# helm/unite instead of fzf).
# Note: there are fzf and fasd plugins for ranger; helm can be used with dired
# order of preference:
# 1. quickmark for dir/file if exists (fastest but requires memorization)
# 2. (maybe fuzzy) search for recent/most visited/locate/find
# manual navigation is hardly ever necessary-
# 3. f<keys> auto-enter navigation in file manager (e.g. deer and blscd) or tab
#    completion

# have a (any), s (show), z (cd), etc.
eval "$(fasd --init posix-alias zsh-hook zsh-ccomp zsh-ccomp-install)"

# use ag by default instead of find:
export FZF_DEFAULT_COMMAND='ag -l -g ""'
# using git ls-tree can be faster in large repos according to fzf README
# only shows tracked files though
# export FZF_DEFAULT_COMMAND='
# (git ls-tree -r --name-only HEAD ||
# ag -l -g "") 2> /dev/null'

# using FZF completion script (see plugins section) for the following:
# $ cd ** # dirs
# $ kill -9 ** # processes
# $ other_command ** # files
# etc.

function fzf-fasd-dir() {
	# otherwise will end up as a cdable var
	local dir
	dir="$(fasd -ds | fzf --tac | awk '{print $2}')" && \
	cd "$dir"
}
zle -N fzf-fasd-dir
bindkey -M viins "^[w" fzf-fasd-dir

# possibility if zsh-history-substring-search doesn't cut it
# https://github.com/junegunn/fzf/wiki/examples#command-history
# fh - repeat history
function fh() {
	eval "$( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | \
		sed 's/ *[0-9]* *//')"
}
# fhe - repeat history edit
function fhe() {
	print -z "$( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | \
		sed 's/ *[0-9]* *//')"
}

# idea suggested by gotbletu
function fzf-locate() {
	local selected
	selected=$(locate "${1:-*}" | fzf -e)
	if [[ -d $selected ]]; then
		cd "$selected"
	else
		rifle "$selected"
	fi
}

function fl() {
	fzf-locate "$PWD"/*
}

# https://github.com/b4b4r07/enhancd
# cd ..<TAB> could potentially be useful

# }}}
#==============================
# CDable Vars {{{
#==============================
# benefit over alias is will show up as new name and fact that can use in aliases and functions
# can tab complete from any directory
books=~/ag-sys/Large/Library/Books
alias books='nocorrect books'

# }}}
#==============================
# Aliases/Functions {{{
#==============================
source ~/.zsh/.private_zshrc

function alias_value() {
	alias "$1" | sed "s/^$1='\(.*\)'$/\1/"
}
alias showa='alias_value'
alias countalias='alias | wc -l'

#===============
# Backup & Mounting {{{
#===============
source ~/.zsh/backup_functions.zsh

alias mountefi='sudo mount /dev/disk/by-label/SYSTEM /boot/efi'
alias umountefi='sudo umount /dev/disk/by-label/SYSTEM'

# }}}
#===============
# Startup and Shutdown {{{
#===============
# if ever start using another init: cat /proc/1/comm
function poweroff() {
	pkill -x mpd
	emacsclient --eval "(let (kill-emacs-hook) (kill-emacs))"
	# never had corruption with tc volumes
	# maybe this is overly paranoid, but it seems like a good idea
	truecrypt --text --dismount || return 1
	udiskie-umount --all || return 1
	if command -v tmsu &> /dev/null; then
		tmsu unmount -a || return 1
	fi
	if [[ $1 == reboot ]]; then
		systemctl reboot
	else
		systemctl poweroff
	fi
}

alias reboot='poweroff reboot'

# }}}
#===============
# Symlink All Dotfiles With Stow {{{
#===============
source ~/.zsh/stow_functions

# }}}
#===============
# Git {{{
#===============
# general
alias g='git'
alias gin='git init'
alias gs='git status'
alias gl='git log'
alias glast='git log -1 HEAD'
alias gln='git log --name-status'
# http://fredkschott.com/post/2014/02/git-log-is-so-2005/
alias glg='git log --color --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset" --abbrev-commit'
alias gd='git diff'
alias gcl='git clone' # url
alias gconf='git config --list'
# remove file from index but don't touch disk
alias gr='git rm --cached' # file
# count commits; from alias.sh
alias gcn='git rev-list --count HEAD'
function gsearch() {
	git log --grep="$1"
}

# commiting
alias gc='git commit'
alias gcm='git commit -m' # "<commit message>"
alias gcam='git commit --amend'
alias gsoft='git reset --soft HEAD~1'
# squashing (may need to stash first)
alias grebase='git rebase -i'

# pushing and pulling
# (with default set to simple, will push current branch to corresponding remote branch for pulling)
alias gp='git push'
alias gpom='git push origin master'
alias gpomf='git push origin +master'
alias gpoc='git push -u origin $(current_branch)'
alias gpocf='git push -u origin +$(current_branch)'
alias gpall='git push origin --all'
alias gpot='git push origin --tags'
alias gpos='git push origin source'
alias grv='git remote -v'
# e.g. for forked repo, pull or in steps:
# git fetch upstream
# git merge upstream/master
alias grau='git remote add upstream' # url
# manually add origin url (if didn't clone)
alias grao='git remote add origin' # url
alias gr='git remote rename' # oldname newname
alias grs'git remote show' # remotename

# branching
function current_branch() {
	git branch | awk '{print $2}'
}
alias gh='git checkout'
# create branch and checkout
alias gcnb='git checkout -b'
# create an empty branch
alias gce='git checkout --orphan'
# delete local branch
# git branch -d
# delete remote branch
# git push origin --delete <branchName>
# save changes
alias gstash='git stash'
alias gstashl='git stash list'
# revert to most recent (or stash@{num}) in stash (apply then drop)
alias gpop='git stash pop'

# }}}
#===============
# Pacman/AUR {{{
#===============
# make new mirror list
alias rldmirrors='sudo reflector --verbose -l 200 --sort rate --save /etc/pacman.d/mirrorlist'

# find out what package contains a command
alias pkgfile='nocorrect pkgfile'
alias pacss='nocorrect pacman -Ss'
alias -g pacs='nocorrect sudo powerpill -S'
alias pacq='pacman -Q'
alias pacqm='pacman -Qm'
alias pacupd='sudo pacnanny -Syu'
alias pacrns='sudo pacman -Rns'
alias -g rldpac='pacman -Syy'
# will keep 3 most recent versions and remove all versions of uninstalled
alias cleanpaccache='paccache -r && paccache -ruk0'

alias yass='nocorrect yaourt -Ss'
alias yas='nocorrect yaourt -S'
alias yapd='yaourt -Syu --aur'
alias yaqm='yaourt -Qm'

alias aur='sudo aura --hotedit -A'
alias aurh='sudo aur'
alias aurs='aura -As'
alias auri='aura -Ai'
alias aurupd='sudo aura -Au'

# remove orphans yaourt -Qtd; aura -Oj

# }}}
#===============
# Reloading things {{{
#===============
# zshrc
alias rld='echo "The Matrix has been reloaded" && source ~/.zshrc'

# xrdb -query -all
alias rldxres='xrdb -load ~/.Xresources'

alias rldless='lesskey ~/.lesskey'

# recreate grub config file; after editing /etc/default/grub
alias -g regrub='grub-mkconfig -o /boot/grub/grub.cfg'

# update font caches
alias rldfonts='fc-cache -vf'
# add pretty much any font in ~/.fonts; don't remember where stole from
alias fonts='mkfontdir ~/.fonts ; mkfontscale ~/.fonts ; xset +fp ~/.fonts ; xset fp rehash ; fc-cache ; fc-cache -fv'

# reload time
alias rldtime='sudo ntpdate us.pool.ntp.org'
# set hardware clock to system time
alias fixhwclock='rldtime && sudo hwclock --systohc'

alias rldsxhkd='pkill -USR1 -x sxhkd'

alias rldconky='conky_switcher'

# battery saving; kill panel, conky, and compton
alias nopanel='rldbspc nopanel'

# manually reload udev rules
alias -g rldudev='udevadm control --reload-rules'

# }}}
#===============
# System Information {{{
#===============
function cpuinfo() {
	${PAGER:-less} /proc/cpuinfo
}

# check drive mount options
function mountopts() {
	grep -i "$1" /proc/mounts
}
# e.g.
# $ mountopts sdb1
# $ mountopts discard

# http://catonmat.net/blog/another-ten-one-liners-from-commandlingfu-explained
function nicemount() {
	(echo "DEVICE PATH TYPE FLAGS" && mount | awk '$2="";1') | column -t
}

# }}}
#===============
# Group Stuff {{{
#===============
# add current user to group
function gadd() {
	sudo gpasswd -a "$USER" "$1"
}

# list what groups current user is in
function grouplist() {
	groups "$USER"
}

# }}}
#===============
# General/Random {{{
#===============
# function so that will work even if EDITOR happens to be changed
function ed() {
	"$EDITOR" "$@"
}

function take() {
	mkdir -p "$1"
	cd "$1"
}

# b is for bang!
function b() {
	sudo "$(fc -ln -1)"
}

function def(){
	echo "$1" | festival --tts &
	sdcv "$1"
	sdcv "$1" | ${PAGER:-less}
}

# switch conky theme
alias conkywhite='conky_switcher white'
alias conkyblack='conky_switcher black'

# for opening in already open gvim session
function gvir() {
	gvim --remote "$1"
}

# vim as password manager (switched to gpg file with emacs)
# alias vpass='mountacct && vim -u ~/.encrypted_vimrc -x ~/blemish/accts.vault'
# edit without backups, undo, history, etc.
alias vimsens='vim -u ~/.encrypted_vimrc'

# telnet/fun
alias starwars='telnet towel.blinkenlights.nl'
alias fun='telnet sdf.org'
alias fort='fortune -a'
alias forto='fortune -o'
alias hack='hexdump -c /dev/urandom'

# other program aliases
alias -g pkill='nocorrect pkill'
alias pgrep='nocorrect pgrep'
alias pg='pgrep -l'
alias man='nocorrect man'
alias sfh='screenfetch'
alias wee='weechat'
alias bitl='sudo bitlbee -D'
alias mutt='cd ~/move && mutt'
alias ppss='PPSSPPSDL &'
alias stl='sudo systemctl'
# use spacemacs config (not symlinking to home with stow)
# alias spacemacs='env HOME=/home/angelic_sedition/dotfiles/spacemacs emacs'
alias enw='emacs -nw'

# change hostname
alias -g chhost='hostnamectl set-hostname'

alias checkclass='xprop | grep WM_CLASS'

# empty trash
alias rmtrash='trash-empty'
alias emptytrash='trash-empty'
alias lstrash='trash-list'

# http://alias.sh/strip-comments-and-blank-lines-file
# strip comments and blanks lines from file
function confcat() {
	sed -e 's/[#;].*//;/^\s*$/d' "$@"
}
# show lines that are not blank or commented out
alias active='grep -v -e "^$" -e"^ *#"'

# }}}
#===============
# Tmux {{{
#===============
# new session but don't attach
alias tmnew='TMUX= tmux new-session -d -s' # <session name>
alias tmnest='unset TMUX ; tmux new-session -s' # <session name>
# can tab complete
alias tmkill='tmux kill-session -t'
alias ta='tmux attach -t'
alias tl='tmux list-sessions'
alias tswitch='tmux switch -t'

# }}}
#===============
# Directory Stuff {{{
#===============
# copy jump paste instead of cp ./.. /fullpath
# http://youtu.be/qTl7vzL_vDU?list=UUkf4VIqu3Acnfzuk3kRIFwA
# I had no idea this existed; haven't ever used though
alias c='xclip-copyfile'
alias y='xclip-copyfile'
alias p='xclip-pastefile'
alias d='xclip-cutfile'

# copy working directory to clipboard
alias cpwd='pwd | tr -d "\n" | xsel -ib'

# ls stuff {{{
# alternatives/improved:
# https://github.com/ogham/exa
# https://github.com/supercrabtree/k
# k is beautiful
alias kh='k -h'
alias ka='k -ah'
# can actually just list dirs
alias kd='k -dh'

alias l='ls -alh'
alias lsa='ls -a'
alias lsl='ls -lh'
# http://jeff.robbins.ws/reference/my-zshrc-file
# these require zsh
alias ltd='ls *(m0)' # files & directories modified in last day
alias lt='ls *(.m0)' # files (no directories) modified in last day
alias lnew='ls *(.om[1,3])' # list three newest
# most recent subdir
alias lsrdir='ls -d *(/om[1])'
 # show hidden files/directories only
alias lh='ls -d .*'

# }}}

# cd stuff {{{
# custom cds
alias home='cd ~/'
alias cdot='cd ~/dotfiles'
# view dir stack
alias dv='dirs -v'
# exit symlinks; http://alias.sh/exit-symlinks
function xs() {
	cd "$(pwd -P)"
}

# not needed due to ZSH autocd opt
# alias ..='cd ..'
alias ...='cd ../..'
alias .3='cd ../../..'
alias .4='cd ../../../..'
alias .5='cd ../../../../..'
alias .6='cd ../../../../../..'
alias .7='cd ../../../../../../..'
alias .8='cd ../../../../../../../..'

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

# push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# }}}

# size management {{{
# baobab or something graphical is preferable, but these are nice in simple cases
# check free space
alias fspace='df -h'
# colored disk usage; colour intelligently; sort; human readable sizes
alias diskspace='cdu -isdh'
alias dis='diskspace'
# there's also ncdu.. but it doesn't support key rebinding I believe, so fuck it
# dfc is also nice for whole filesystem
# number of files (not directories)
alias filecount='find . -type f | wc -l'

# }}}

# searching {{{
# use silver (or plat) searcher for contents
alias ags='ag -S'

function rs(){
	find . -name "*$1*"
}
alias -g rf='rs'

alias rsl='find . -name'

# }}}

# }}}
#===============
# Ranger {{{
#===============
function ranger-cd {
	# from https://github.com/hut/ranger/blob/bdd6bf407ab22782f7ddb3a1dd24ffd9c3361a8d/examples/bash_automatic_cd.sh
	# with minor modifications
	# change the directory to the last visited one after ranger quits.
	# "-" to return to the original directory.
	local tempfile
	tempfile=/tmp/ranger/chosendir
	mkdir -p /tmp/ranger
	ranger --choosedir="$tempfile" "${@:-$(pwd)}"
	if [[ -f $tempfile ]] && \
		[[ $(< $tempfile) != $(pwd | tr -d '\n') ]]; then
		cd "$(< $tempfile)"
	fi
	rm -f "$tempfile"
}

function rn() {
	if [[ ! -z $RANGER_LEVEL ]]; then
		# https://wiki.archlinux.org/index.php/Ranger
		# if a ranger session exists, restore it
		exit
	fi
	case $1 in
		dwn) ranger-cd ~/move ;;
		vim) ranger-cd ~/.vim ;;
		*)   ranger-cd ;;
	esac
}

# }}}
#===============
# Music, Video/Images, Sound, Etc. {{{
#===============
# program aliases
alias als='alsamixer'
alias nm='ncmpcpp'
# cd ripping
alias -g rip='abcde'

# could potentially use this instead of mpdcron
alias mpdstats='beet mpdstats'

# get image dimensions with the imagemagick
alias dim='identify -format "%wx%h"'
# image rotation
alias imrotate='mogrify -rotate 90'
# to create an image that will work as grub background
function grubify() {
	local extension
	extension=${1##*.}
	convert "$1" -colorspace rgb "${1%.*}_rgb.$extension"
}

# connecting to tv/monitor with hmdi
function hdmiin() {
	xrandr --output HDMI1 --auto && \
		ponymix set-profile output:hdmi-stereo
}
function hdmiadd() {
	# rename newly added desktop to X
	# setroot --restore so that newly added screen is black
	xrandr --output HDMI1 --auto --right-of LVDS1 && \
		bspc monitor HDMI1 --reset-desktops X && \
		setroot --restore
	if [[ $1 != noa ]]; then
		ponymix set-profile output:hdmi-stereo
	fi
}
function hdmiout() {
	xrandr --output HDMI1 --off && \
		bspc desktop X -r && \
		bspc monitor HDMI1 -r && \
		ponymix set-profile output:analog-stereo
}


# Thinkpad p50
# alias discrete="sudo cp ~/dotfiles/20-nvidia.conf /etc/X11/xorg.conf.d/"
# alias hybrid="sudo rm /etc/X11/xorg.conf.d/20-nvidia.conf"
# this requires discrete graphics enabled in bios
# dpadd() {
# 	xrandr --output DP-1 --auto --right-of DP-4 && \
# 		bspc monitor DP-1 --reset-desktops X && \
# 		setroot --restore
# }
# dpout() {
# 	xrandr --output DP-1 --off && bspc monitor DP-1 -r
# }

# For hybrid graphics:
# Option "UseDisplayDevice" "none" must be commented in
# /etc/bumblebee/xorg.conf.nvidia
# intel-virtual-output will only work if already plugged in!
addvirth() {
	intel-virtual-output
	xrandr --output VIRTUAL2 --auto --right-of eDP1
	bspc monitor VIRTUAL2 --reset-desktops X
	setroot --restore
}

offvirth() {
	xrandr --output VIRTUAL1 --off
	bspc monitor VIRTUAL2 -r
	pkill -x intel-virtual-o
	# will kill extra X server and reset bbswitch (nvidia card now OFF)
	# a more direct way to do this?
	sudo systemctl restart bumblebeed
}

# vga
function vgain() {
	geometry=$(xwininfo -root | awk '/geometry/ {gsub("\\+.*",""); print $2}')
	xrandr --output VGA1 --auto --scale-from "$geometry"
}
function vgaadd() {
	geometry=$(xwininfo -root | awk '/geometry/ {gsub("\\+.*",""); print $2}')
	xrandr --output VGA1 --auto --scale-from "$geometry" --right-of LVDS1 && \
		bspc monitor VGA1 --reset-desktops X && setroot --restore
}
alias vgaout='xrandr --output VGA1 --off && bspc monitor -r X'

# play clipboard (url)
function mpgo() {
	mkdir -p /tmp/mpv
	clipboard=$(xsel -b)
	if [[ $clipboard =~ ^http ]] || [[ -f $clipboard ]]; then
		echo "$clipboard" > /tmp/mpv/last_link
		mpv --screenshot-template="./%tY.%tm.%td_%tH:%tM:%tS" "$(xsel -b)"
	fi
}

function mplast() {
	mpv --screenshot-template="./%tY.%tm.%td_%tH:%tM:%tS" "$(< /tmp/mpv/last_link)"
}

# youtube downloading
alias ytvid='youtube-dl --restrict-filenames -o "~/move/%(title)s_%(width)sx%(height)s_%(upload_date)s.%(ext)s"'
alias ytaudio='youtube-dl --restrict-filenames --extract-audio -o "~/move/%(title)s_%(width)sx%(height)s_%(upload_date)s.%(ext)s"'

# record audio
function arec() { # filename
	if [[ $# -ne 1 ]]; then
		return 1
	fi
	arecord -vv -f wav "$1"
}
# see ~/bin/srec for ffmpeg video recording

# }}}
#===============
# Octopress Blog {{{
#===============
# work without publishing:
function genbl() {
	cd "$BLOG" && bundle exec rake generate
}

function prevbl() {
	cd "$BLOG" && bundle exec rake preview
}

# update site on github pages
function pushblog() {
	cd "$BLOG" && bundle exec rake gen_deploy
}

# update at posts and push to source
function pushposts() {
	cd "$BLOG"/source/_posts && git add . && gpos
}

function newbpost() {
	cd "$BLOG" && bundle exec rake new_post\["$1"\]
}

# }}}
#===============
# Internet, VPN, Firewall, and Torrenting {{{
#===============
alias dl='aria2c -x 4'
# check ip; default gateway / router
alias gateip='ip route show'
# check what DNS servers have in conf and which one using
alias showdns='cat /etc/resolv.conf && echo "\n--DIG OUTPUT:" && dig fsf.org | grep SERVER'
alias checkdns='showdns'
# identify active network connections; http://alias.sh/identify-and-search-active-network-connections
alias spy='lsof -i -P +c 0 +M'
alias netlist='lsof -i -P | grep LISTEN'

# block unproductive sites
alias focus="sudo block_sites block"

# clean firefox profile
alias cleanff='profile-cleaner f'

# firewall
alias ufws='sudo ufw status'
alias ufwd='sudo ufw delete'

# pastebin
# https://unix.stackexchange.com/questions/108493/easy-way-to-paste-command-line-output-to-paste-bin-services
# "You can send to an enhanced URL if you would like syntax highlighting for your code paste. For ix, you append either /ID/ to the URL (http://ix.io/ID/) for default syntax based on auto-detection, or /ID/<language>/ to explicitly set the language for pygments highlighting."
function ix() {
	local url
	url=$(curl -F 'f:1=<-' ix.io <"$1")
	# copy url to clipboard
	echo "${url}/" | xsel -ib
	echo "$url"
}

# connecting {{{
# netctl (if no connman)
alias wifi='sudo wifi-menu'
alias nts='sudo netctl switch-to'
alias stopnetctl='sudo systemctl stop netctl'
alias startnetctl='sudo systemctl start netctl'
# connman
alias stopcon='sudo systemctl stop connman'
alias startcon='sudo systemctl start connman'
# sometimes necessary for reconnecting with spotty connection; doesn't always work
# https://bbs.archlinux.org/viewtopic.php?id=188825
# systemctl restart connman
alias rldcon='sudo systemctl stop connman && sleep 3 && sudo systemctl start connman'
alias conenwifi='connmanctl enable wifi'
alias conlist='connmanctl scan wifi && connmanctl services'
alias con='connmanctl connect'
alias swcon='sudo systemctl stop NetworkManager && sudo systemctl start connman'
alias swnm='sudo systemctl stop connman && sudo systemctl start NetworkManager'

# show active device
function ipup() {
	ip link show up | awk -F ":" '/state UP/ {print $2}'
}

# in case something goes wrong
fixresolv() {
	sudo chattr -i /etc/resolv.conf
	sudo cp ~/dotfiles/.root/etc/resolv.conf.backup /etc/resolv.conf
	sudo chattr +i /etc/resolv.conf
}

restoreresolv() {
	sudo chattr -i /etc/resolv.conf
	echo "nameserver 127.0.0.1" | sudo tee /etc/resolv.conf
	sudo chattr +i /etc/resolv.conf
}

# }}}

# vpn {{{
# https://github.com/pschmitt/pia-tools
# list vpn connections
alias vl='systemctl list-units | grep pia@'
# vpn connect; pia-tools will start transmission service
# see my pia-up and pia-down files (deny by default and stop transmission after)
# see completion file (for tab completion)
function vc() {
	sudo systemctl stop transmission && sudo systemctl start pia@"$1"
}
alias vcs='vc Sweden'
alias vcc='vc CA_Toronto'
# stop any pia service
alias stopv='sudo systemctl stop pia@\*'
alias voff='stopv'
alias piai="pia-tools --info"
alias piac="pia-tools -c"
alias piar="pia-tools --restore-dns"
alias piad="pia-tools --disallow"
alias piaa="pia-tools --allow"

# }}}

# torrents {{{
alias starttr='sudo systemctl start transmission'
alias stoptr='sudo systemctl stop transmission'

# https://github.com/gotbletu/shownotes/blob/e6fe01c4567a4129558c3911a412cf5af4448cf9/transmission-cli.txt
# manually add a torrent file or magnent link
alias toa='transmission-remote -a'

# remove torrent; leaves data alone; give id (e.g. 1) or "all"
function todd() {
	transmission-remote -t "$1" --remove
}

# remove completed torrents (but without a bunch of greps and xargs)
function tord() {
	transmission-remote -l | \
		awk '/100%.*Done/ {system("transmission-remote -t "$1" -r")}'
}

# pause torrent
function topp() {
	transmission-remote -t "$1" --stop
}

# pause all
alias stopto='transmission-remote -t all --stop'

# unpause
function toup() {
	transmission-remote -t "$1" --start
}

# list
alias tol='transmission-remote -l'
# tell number of seeders
alias -g tons='transmission-show --scrape' # <torrent file>
alias tocs='tons'

# continuously show speed
function toss() {
	while true; do
		clear
		transmission-remote -t "$1" -i | grep Speed
		sleep 1
	done
}

# }}}

# }}}
#===============
# Gaming {{{
#===============
# alias xbox='sudo xboxdrv --silent --detach-kernel-driver'
# alias controller='xbox'
# experimenting:
# sudo xboxdrv --deadzone 4000 --dpad-as-button --trigger-as-button --ui-axismap "x2=REL_X:10,y2=REL_Y:10,x1=KEY_A:KEY_D,y1=KEY_W:KEY_S" --ui-buttonmap "tl=KEY_LEFTSHIFT,tr=KEY_LEFTCTRL" --ui-buttonmap "a=KEY_SPACE,b=KEY_C,x=KEY_1,y=KEY_R" --ui-buttonmap "lb=KEY_Q,rb=KEY_E" --ui-buttonmap "lt=BTN_LEFT,rt=BTN_RIGHT" --ui-buttonmap "dl=KEY_4,dr=KEY_B,du=BTN_MIDDLE,dd=KEY_TAB" --ui-buttonmap "back=KEY_ESC,start=KEY_ENTER"

# }}}
#===============
# Android Music Syncing {{{
#===============
# alias -g mountand='jmtpfs -o allow_other ~/samsung'
# restart adb
alias adbrestart='adb kill-server ; adb start-server'

export MTP_MOUNT_DIR="$HOME/mtp"

function mountand() {
	mkdir -p "$MTP_MOUNT_DIR" && jmtpfs "$MTP_MOUNT_DIR"
}
function umountand() {
	fusermount -u "$MTP_MOUNT_DIR"
}

# http://www.arachnoid.com/android/SSHelper/index.html
# with ssh: I tried but it was much slower :( even with fast internets
# (sshdroid, sshelper, zshaolin, etc.)
#  rsync -azvr --no-perms --no-times --size-only --progress --delete --rsh="ssh -p 2222" /path root@hostip:/path

# MTP is an abomination
alias android_rsync='rsync -azvP --no-perms --no-times --size-only'

function syncandmus() {
	# add auto-mounting and checking
	android_rsync --delete --include-from="$HOME/.zsh/rsync_bandlist.txt" \
		"${XDG_MUSIC_DIR:-$HOME/music}/" "$MTP_MOUNT_DIR/Card/Music"
	android_rsync ~/.mpd/playlists/ "$MTP_MOUNT_DIR/Card/Music"
}

# tophone
function tophone() {
	android_rsync --delete ~/wallpaper/phone/ "$MTP_MOUNT_DIR"/Card/wallpaper
	if [[ -d ~/database/ringtones ]]; then
		android_rsync --delete ~/database/ringtones/ "$MTP_MOUNT_DIR"/Card/ringtones
	fi
}

# }}}
#===============
# GPG {{{
#===============
alias gencrypt="gpg -e -r"

# }}}
#===============
# Other Functions {{{
#===============
source ~/.config/ranger/ranger_functions

# from omz I think; command usage statistics
function zsh_stats() {
	fc -l 1 | \
		awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | \
		grep -v "./" | column -c3 -s " " -t | sort -nr | nl | head -n20
}

# pipe into head; optionally specify line number
function he() {
	# http://stackoverflow.com/questions/806906/how-do-i-test-if-a-variable-is-a-number-in-bash/806923#806923
	if [[ $1 =~ ^[0-9]+$ ]]; then
		"${@:2}" | head -n "$1"
	else
		"$@" | head -n 15
	fi
}
alias he="nocorrect he"

# send stdout and stderr to /dev/null
function qt() {
	"$@" &> /dev/null &
}

# save errors
function qte() {
	"$@" 2> "${1}_error.log" &
}

# Random {{{
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


# http://alias.sh/merge-pdfs
function pdfmerge() {
	local tomerge
	tomerge=""
	for file in "$@"; do
		tomerge="$tomerge $file"
	done
	pdftk "$tomerge" cat output mergd.pdf
}

# }}}

# }}}
#===============
# }}}
#==============================
# for testing startup time; uncomment above as well
# zprof
