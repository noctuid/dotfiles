# This is my modified version of the for theme from omz
# vcs stuff from here:
# http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/
# the way to do the [INSERT] indicator without messing up autosuggestions:
#http://hamberg.no/erlend/posts/2010-10-17-show-current-vi-mode-in-zsh-prompt.html

#%{$fg_bold[blue]%}%n%{$reset_color%}%{$fg[magenta]%}☮%{$fg_bold[red]%}%M%{$reset_color%}%{$fg[cyan]%}]%{$fg[white]%}-%{$fg[cyan]%}(
PROMPT='%{$fg[magenta]%}┌[%{$fg[blue]%}%~%{$reset_color%}%{$fg[magenta]%}]%{$reset_color%}$(git_prompt_info)
%{$fg[magenta]%}└$(prompt_char)%{$fg_bold[cyan]%}% % %{$reset_color%} '
RPS1='${VIMODE}'

# pretty much the only way I've found to get this working with zsh autosuggestions# {{{
# set VIMODE according to the current mode (default “[i]”)
VIMODE='[-- INSERT --]'
function zle-keymap-select {
 VIMODE="${${KEYMAP/vicmd/[-- NORMAL --]}/(main|viins)/[-- INSERT --]}"
 zle reset-prompt
}
zle -N zle-keymap-select

function prompt_char {
    git branch >/dev/null 2>/dev/null && echo ' ±' && return
    hg root >/dev/null 2>/dev/null && echo '☿' && return
    echo '>'
}
# }}}
# vcs# {{{
ZSH_THEME_GIT_PROMPT_PREFIX="-[%{$reset_color%}%{$fg[white]%}git://%{$fg_bold[white]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}%{$fg[cyan]%}]-"
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
# vim :set ft=zsh:
