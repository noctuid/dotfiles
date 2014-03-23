# [3.21.14] created in addition to to.sh
# open saved urls from pentadactyl

# one time open urxvt as floating window containing vimwiki structure
bspc rule -a termite -o floating=true center=true
termite -e "/bin/zsh -c 'vim ~/vimwiki/index.wiki'" &

# if ever get tabgroups working or start doing stuff like that (probably won't); add auto open based on name
