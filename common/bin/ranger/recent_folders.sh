#!/bin/sh
# switch to most recently modified directory (i.e. file saved in)
# don't think there's any way to evaluate a shell command in combination with a ranger command (i.e. :cd $(something))
# chaining looks like it might allow for executing ranger commands on output of others ( [comamnd 1[2]]), but it doesn't

if [ "$1" == "ag-sys" ]; then
	cd ~/ag-sys
	# most_recent_file=$(find ~/ag-sys -mtime -1 -type f -printf "%T+\t%p\n" | sort | tail -n 1)
	# file=$(echo $most_recent_file | sed 's/[^ ]* //g')
	# dir=${file%/*}

	# simpler; requires zsh
	dir=$(zsh -c 'ls -d **/*(/om[1])')
	tmux send-keys ":"
	tmux send-keys "cd ~/ag-sys/$dir" "Enter"
elif [ "$1" == "database" ]; then
	cd ~/database
	# most_recent_file=$(find ~/database -mtime -1 -type f -printf "%T+\t%p\n" | sort | tail -n 1)
	# file=$(echo $most_recent_file | sed 's/[^ ]* //g')
	# dir=${file%/*}

	dir=$(zsh -c 'ls -d **/*(/om[1])')
	tmux send-keys ":"
	tmux send-keys "cd ~/database/$dir" "Enter"
fi
