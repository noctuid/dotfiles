# This is my modified version of the for theme from omz
# vcs stuff from here:
# http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/

PROMPT='%{$fg[magenta]%}┌[%{$fg[blue]%}%~%{$reset_color%}%{$fg[magenta]%}]$(git_prompt_info)$(file_number)
%{$fg[magenta]%}└$(prompt_char)%{$fg_bold[cyan]%}% % %{$reset_color%} '
RPS1='${VIMODE}'

function file_number() {
	# http://www.maketecheasier.com/8-useful-and-interesting-bash-prompts
	echo "%{$fg[magenta]%}-|%{$fg[green]%}$(/bin/ls -1 | /usr/bin/wc -l | /bin/sed 's: ::g') files%{$fg[cyan]%},%{$fg[green]%} $(/bin/ls -lah | /bin/grep -m 1 total | /bin/sed 's/total //')%{$fg[magenta]%}|"
}

function prompt_char {
    git branch >/dev/null 2>/dev/null && echo ' ±' && return
    hg root >/dev/null 2>/dev/null && echo '☿' && return
    echo '>'
}

function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/[-- NORMAL --]}/(main|viins)/-- INSERT --}"
    RPS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# vcs# {{{
ZSH_THEME_GIT_PROMPT_PREFIX="-[%{$reset_color%}%{$fg[white]%}git://%{$fg_bold[white]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}%{$fg[magenta]%}]"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg[green]%}✔%{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[green]%}?"
# I don't use mercurial# {{{
function hg_prompt_info {
    hg prompt --angle-brackets "\
< on %{$fg[magenta]%}<branch>%{$reset_color%}>\
< at %{$fg[yellow]%}<tags|%{$reset_color%}, %{$fg[yellow]%}>%{$reset_color%}>\
%{$fg[green]%}<status|modified|unknown><update>%{$reset_color%}<
patches: <patches|join( → )|pre_applied(%{$fg[yellow]%})|post_applied(%{$reset_color%})|pre_unapplied(%{$fg_bold[black]%})|post_unapplied(%{$reset_color%})>>" 2>/dev/null
}
# }}}
# }}}

# pretty much the only way I've found to get this working with zsh autosuggestions (which I no longer use)# {{{
# http://hamberg.no/erlend/posts/2010-10-17-show-current-vi-mode-in-zsh-prompt.html
# doesn't use zle-line-init part which seems to be the conflict with autosuggestions
# problem is that doesn't update all the time (sometimes will say normal when it's insert)
# set VIMODE according to the current mode (default “[i]”)
# VIMODE='[-- INSERT --]'
# function zle-keymap-select {
#  VIMODE="${${KEYMAP/vicmd/[-- NORMAL --]}/(main|viins)/[-- INSERT --]}"
#  zle reset-prompt
# }
# zle -N zle-keymap-select
# }}}
# vim :set ft=zsh:
