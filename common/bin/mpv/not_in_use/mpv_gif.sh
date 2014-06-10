#!/bin/sh
# make gifs from video file using mpv to mark start and end times
# requires bindings in mpv and an actual video file as well as gifmaker.pl in path; also gifmaker.pl doesn't seem to like spaces in filenames, even when in "" or escaped
# http://www.leshylabs.com/blog/dev/2013-08-04-Making_Animated_GIFs_from_the_Linux_Command_Line.html

if [ "$1" == "1" ]; then
	# wait for end of gif
	sleep 10
	starttime=$2
	path=$3
	dir=${path%/*}
	file=$(echo $path | xargs -I{} basename {})
	endtime=$(cat ~/bin/mpv/duration.txt)
	# strip of colons and zeros
	starttimez=$(echo $starttime | sed 's/\://g')
	starttimey="$(echo $starttimez | sed 's/0*//')"
	endtimez=$(echo $endtime | sed 's/\://g')
	endtimey="$(echo $endtimez | sed 's/0*//')"
	# subtract for duration
	let "duration = $endtimey - $starttimey"
	# I was having a hard time getting this to work, so just sending it to another shell script and running that
	echo cd \"$dir\" "&&" FFMPEG_ARGS=\"-ss $starttime -t $duration\" gifmaker.pl \"$file\" ~/Move/out.gif > ~/bin/mpv/make_gif.sh
	sh ~/bin/mpv/make_gif.sh
	# clear duration.txt
	echo "" > ~/bin/mpv/duration.txt
else
	if ! [ -f ~/bin/mpv/duration.txt ]
	then
	# create file if doesn't exist
		touch ~/bin/mpv/duration.txt
	fi
	echo $2 > ~/bin/mpv/duration.txt
fi
