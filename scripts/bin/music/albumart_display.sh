#!/bin/sh
# script to open tmux split and display album art of currently playing song
# other required programs: mpc, artget, xdotool, w3m with option to display images externally turned off
# https://github.com/berkoz/artget/blob/master/artget.py
# had install python-mpd2 with pip2 and uninstalall aur version first
# TODO:
# make it so that it update automatically on song change instead of sleeping and checking
# make it less of a mess
# see if it works much better in session with only 1 window that is persistent/not closed and reattached to

if [ ! "$1" == "run_script_in_split" ]; then
	# called from vimus; create a new split
	tmux split-window -h
	# get back to vimus
	tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l
	# swap to left
	tmux swap-pane -U

	# run this script in the new split
	tmux send-keys "~/bin/music/albumart_display.sh run_script_in_split &"
	tmux send-keys Enter
else
	while true; do
		# when called from vimus; can use % to send current file location; one problem is that after exiting vimus and starting it again, it will no longer send the correct file path (will send mpd music directory)
		# instead, this will work with ncmpcpp or any other terminal mpd client:
		# use mpc to get current file
		file=$(mpc current -f "%file%")
		# need to add back on mpd's music dir
		full_file_path="$HOME/Music/$file"
		dirname=${full_file_path%/*}
		cover_file="${dirname}/cover.jpg"
		escaped=$(printf '%q' "$dirname")

		# download album art if cover.jpg doen't already exist
		if [ ! -f "$cover_file" ]; then
			python2 ~/bin/artget.py -n localhost -p 6600 -r ~/Music -t -s 4 -o cover.jpg
		fi

		# tmux send-keys "cd $escaped && ranger --selectfile='cover.png' --cmd='display_file'"
		# use w3m directly instead; display is somewhat glitchy in termite/tmux, but won't crash like ranger when closing
		# turn off use external image viewer in w3m settings (o)

		# prevent tmux from sending keys to wrong tmux window or a window that isn't termite
		if [ "$(xtitle)" == "termite" ] && [ "$(tmux display-message -p '#I')" == "1" ]; then
			tmux send-keys -t 1 "cd $escaped && w3m cover.jpg"
			tmux send-keys -t 1 Enter
			# w3m image display with tmux and termite is somewhat glitchy; sometimes need to press another key to get the picture to show:
			tmux send-keys -t 1 h

			# wait.. song will end at some point
			sleep 20
			# wait until on the right tmux window to quit and re-enter w3m for possible new album art
			while [ ! "$(tmux display-message -p '#I')" = "1" ]; do
				sleep 5
			done
			tmux send-keys -t 1 Q
			tmux send-keys -t 1 Enter
		fi
	done
fi
