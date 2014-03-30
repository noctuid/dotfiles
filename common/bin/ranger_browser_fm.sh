#!/bin/sh
# see corresponding pentadactylrc section
# use ranger to save files (actually just save the file to a folder and then dd it with ranger to move to desired destination)

# open ranger with downloaded file selected

# need to get rid of apostrophies and deal with spaces
no_ap=$(echo "$1" | sed "s/'//g")
mv "$1" "$no_ap"
escaped=$(printf '%q' "$no_ap")
bspc rule -a termite -o floating=true center=true
termite -e "/bin/zsh -c 'xdo resize -w +300 && xdo move -x -150 && xdo resize -h +200 && xdo move -y -100 && ranger --selectfile=""$escaped"" --cmd="cut"'" &

# crap:#{{{
# haven't found a good way to get ranger to deal with names with spaces and certain characters
# escaping the path name or putting "" will fix if putting in actual terminal, but in script --selectfile=.. doesn't work the same; even if rename with _ instead of spaces and such, giving --selectfile the variable doesn't work, so this requires that things be named without spaces and apostrophies and such


# try_again=\"$(xsel -b)\"
#
# # no_null=$(echo $1 | sed 's/\x0//g')
# stripd=$(echo $1 | tr -d '\n')
# escaped=$(printf '%q' "$1")
#
# # echo $escaped | xsel -bi
# # echo $escaped
#
# no_spc=$(echo "$1" | sed 's/\ /\_/g')
# no_ap=$(echo "$no_spc" | sed "s/'//g")
# final=$no_ap
# mv "$1" "$final"
# echo $final | xsel -bi

# no_fspc=$(echo "$1" | sed -e 's/^ *//' -e 's/ *$//')
# no_spc=$(echo "$no_fspc" | sed 's/\ /\_/g')
# no_ap=$(echo "$no_spc" | sed "s/'//g")
#
# final=$no_ap
# mv "$no_fspc" "$final"
# cp "$no_fspc" "$final"
# rm $final
#}}}
