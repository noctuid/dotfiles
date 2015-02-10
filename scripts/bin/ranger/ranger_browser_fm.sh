#!/bin/bash
# see corresponding pentadactylrc section: ../../../browsing/.pentadactylrc:727
# open ranger with downloaded file cut

window_title=$(xtitle)
# remove everything until last hyphen; results in titlestring (mine is Pentadactyl; (Private Browsing) added if private win)
browser_type=${window_title##*-}

mkdir -p ~/bin/ranger
# toggle script
if [[ $1 == "toggle" ]]; then
	if [[ -f ~/bin/ranger/off.txt ]]; then
		rm ~/bin/ranger/off.txt
	else
		touch ~/bin/ranger/off.txt
	fi
else
	if [[ ! -f ~/bin/ranger/off.txt ]]; then
		if [[ $browser_type == " Pentadactyl" ]]; then
			# need to get rid of apostrophies and deal with spaces
			no_ap=${1//\'/}
			mv "$1" "$no_ap"
			escaped=$(printf '%q' "$no_ap")
			bspc rule -a termite -o floating=true center=true
			termite -e "/bin/bash -c 'xdo resize -w +300 && xdo move -x -150 && xdo resize -h +200 && xdo move -y -100 && ranger --selectfile=""$escaped"" --cmd=cut'" &
		# else
		# otherwise, a private window; do nothing
		fi
	fi
fi
