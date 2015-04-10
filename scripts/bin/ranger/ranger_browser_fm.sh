#!/bin/bash
# open ranger with downloaded file cut
# see corresponding pentadactylrc section: ../../../browsing/.pentadactylrc:727

mkdir -p /tmp/ranger_fm
# toggle script
if [[ $1 == "toggle" ]]; then
	if [[ -f /tmp/ranger_fm/off ]]; then
		rm /tmp/ranger_fm/off
	else
		touch /tmp/ranger_fm/off
	fi
else
	# conditionally executing the autocmd on the pentadactyl side is useless because of autocmd pileup
	# until this is fixed; I'm using this instead of manually adding the DownloadPost autocmd
	is_private=$(xtitle | grep "Private Browsing")
	if [[ ! -f /tmp/ranger_fm/off ]] && [[ -z $is_private ]]; then
		filename="$1"
		# this is also to deal with autocmd pileup
		last_filename="$(< /tmp/ranger_fm/last_filename)"
		echo "$filename" > /tmp/ranger_fm/last_filename
		if [[ ! $filename =~ ^/tmp.* ]] && [[ ! $last_filename == "$filename" ]]; then
			# need to get rid of apostrophies and deal with spaces
			no_ap=${filename//\'/}
			mv "$filename" "$no_ap"
			escaped=$(printf '%q' "$no_ap")
			bspc rule -a termite -o floating=true center=true
			termite -e "/bin/bash -c 'xdo resize -w +300 && xdo move -x -150 && xdo resize -h +200 && \
				xdo move -y -100 && ranger --selectfile=""$escaped"" --cmd=cut'" &
		fi
	fi
fi
