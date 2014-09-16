# this is because tmux commands will exit vimus interface; can be re-enterd with escape then control+d.. so I'm doing this to change windows without exiting vimus
# not really visually noticeable; some flashing on occasion

# "r" bindings#{{{
# window switching#{{{
if [ "$1" == "a" ]; then
	tmux select-window -t 1 && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -t 1
elif [ "$1" == "r" ]; then
	tmux select-window -t 2 && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -t 2
elif [ "$1" == "s" ]; then
	tmux select-window -t 3 && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -t 3
elif [ "$1" == "t" ]; then
	tmux select-window -t 4 && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -t 4
elif [ "$1" == "d" ]; then
	tmux select-window -t 5 && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -t 5
elif [ "$1" == "h" ]; then
	tmux select-window -t 6 && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -t 6
elif [ "$1" == "n" ]; then
	tmux select-window -t 7 && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -t 7
elif [ "$1" == "e" ]; then
	tmux select-window -t 8 && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -t 8
elif [ "$1" == "i" ]; then
	tmux select-window -t 9 && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -t 9
elif [ "$1" == "o" ]; then
	tmux select-window -t 10 && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -t 10
#}}}

# resize#{{{
elif [ "$1" == "rmh" ]; then
	tmux resize-pane -L 5 && xdotool key Escape && xdotool key control+d && tmux resize-pane -L 5
elif [ "$1" == "rmn" ]; then
	tmux resize-pane -D 3 && xdotool key Escape && xdotool key control+d && tmux resize-pane -D 2
elif [ "$1" == "rme" ]; then
	tmux resize-pane -U 3 && xdotool key Escape && xdotool key control+d && tmux resize-pane -U 2
elif [ "$1" == "rmi" ]; then
	tmux resize-pane -R 5 && xdotool key Escape && xdotool key control+d && tmux resize-pane -R 5
#}}}
# cycle#{{{
elif [ "$1" == "r," ]; then
	tmux swap-pane -U && xdotool key Escape && xdotool key control+d
elif [ "$1" == "r." ]; then
	tmux swap-pane -D && xdotool key Escape && xdotool key control+d
#}}}

# new session
elif [ "$1" == "r_" ]; then
	tmux new-session && xdotool key Escape && xdotool key control+d
# new window
elif [ "$1" == "rc" ]; then
	tmux new-window && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -l
# kill pane
elif [ "$1" == "rx" ]; then
	tmux kill-pane
# last window
elif [ "$1" == "rl" ]; then
	tmux select-window -l && tmux select-window -l && xdotool key Escape && xdotool key control+d && tmux select-window -l

#split /
elif [ "$1" == "r/" ]; then
	tmux split-window -h && tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l
# split -
elif [ "$1" == "r-" ]; then
	tmux split-window && tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l

# break pane
elif [ "$1" == "r!" ]; then
	tmux break-pane && xdotool key Escape && xdotool key control+d
#}}}
# s bindings#{{{
# directional select#{{{
elif [ "$1" == "sh" ]; then
	tmux select-pane -L && tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l
elif [ "$1" == "sn" ]; then
	tmux select-pane -D && tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l
elif [ "$1" == "se" ]; then
	tmux select-pane -U && tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l
elif [ "$1" == "si" ]; then
	tmux select-pane -R && tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l
#}}}

# select last pane
elif [ "$1" == "sl" ]; then
	tmux select-pane -l && tmux select-pane -l && xdotool key Escape && xdotool key control+d && tmux select-pane -l
elif [ "$1" == "sv" ]; then
	tmux select-layout main-vertical  && xdotool key Escape && xdotool key control+d

# zoom/monocle
elif [ "$1" == "st" ]; then
	tmux resize-pane -Z && xdotool key Escape && xdotool key control+d
#}}}
fi
