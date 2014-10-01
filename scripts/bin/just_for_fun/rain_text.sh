#!/bin/sh
# this does the chase on multiple lines

function ellipse() {
for i in `seq 1 $1`;
do
	xdotool type --delay 750 ". . ."
	for i in `seq 1 $2`;
	do
		xdotool key BackSpace
		sleep 0.005
	done
done
}

function back_space() {
for i in `seq 1 $1`;
do
	xdotool key BackSpace
	sleep 0.0001
done
}

function cat_chase_mouse() {
for i in `seq 1 $1`;
do
	j=$i
	while [ $j -gt 1 ]; do
		xdotool type --delay 3 "    "
		j=$((j-1))
	done
	xdotool type --delay 3 "~^3^ -@"
	sleep 0.2
	# num=$((7+$i))
	back_space 7
	xdotool key Return
done
xdotool type "reverse!!!"
sleep 1
back_space 10
xdotool key Return
for i in `seq $1 -1 1`
do
	j=$i
	while [ $j -gt 1 ]; do
		xdotool type --delay 3 "    "
		j=$((j-1))
	done
	xdotool type --delay 3 "@- ^"
	# this char needs more delay
	xdotool type --delay 15 "Ɛ"
	xdotool type --delay 3 "^~"
	sleep 0.2
	# num=$((7+$i))
	back_space 7
	xdotool key Return
done
}

mouse() {
# http://www.retrojunkie.com/asciiart/animals/mice.htm
cat <<"EOT"
             _-~~~-_       _-~~~-_
            /~       ~\    :    ,  \
           '           ~   ,   |:  :
          {      /~~\  :--~""""\.:  :
           \    (... :   /^\  /^\\ ;
            ~\_____     |   | | |:~
                  /     |__O|_|O|;
                 (     /       O \
                  \   ( `\_______/)
                   `\  \         /
                     )  ~-------~'\
                    /              \
                   :               ||
                   |  |            ||
                   |  |.======[]==+'|
                  (~~~~)       |   |~)
                  /    \       |   | \
      ~\          \___/)______/^\__|_/
        `\\      //    |  |      | |
          `\\__//'     |  |      | |
             ~~       (~~~~)    (~~~)
                     /     =\..'    =_
                    |__________)________)-jurcy
EOT
}

sleep 2
xterm -e "nano" &
sleep 2
xdotool type --delay 100 "Loading"
ellipse 5 5
back_space 7

xdotool type --delay 500 "Wait....."
xdotool key Return
date_var=$(date)
xdotool type "$date_var"
xdotool key Return
sleep 2
xdotool type --delay 500 "Error!"
xdotool key Return
xdotool type --delay 100 "THE Linux Rain Automatic Writing Contest is already over!"
sleep 0.005
xdotool key Return
xdotool type --delay 100 "Well"
ellipse 1 0
xdotool key Return
sleep 3
xdotool type --delay 100 "Initiate self destruct sequence!!!!!!!!!"
xdotool key Return

IFS=$'\n'; mouse_array=($(mouse))
for line in "${mouse_array[@]}";do
	xdotool type --delay 5 $line
	sleep 0.005
	xdotool key Return
done

xdotool type --delay 50 "WTF Is THAT?!?!?"
sleep 0.4
xdotool key Return
xdotool type --delay 50 "THE CAT ARE ATTACKING!"
sleep 0.4
xdotool key Return
cat_chase_mouse 20

xdotool type --delay 25 "Fatal Error."
xdotool key Return
xdotool type --delay 25 "rm -rf /* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
xdotool key Return
xdotool type --delay 25 "Destroying harddrive"
ellipse 3 5
xdotool key Return

IFS=$'\n'; error_array=($(hexdump -c /dev/urandom | head -n 50))
for line in "${error_array[@]}";do
	xdotool type --delay 5 $line
	sleep 0.005
	xdotool key Return
done

xdotool key Return
xdotool key Return
xdotool type --delay 700 "Abort, Retry, Fail?"
