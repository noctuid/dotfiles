# dropdown for terminology
win_name=$(xprop -id $(xprop -root 32x '\t$0' _NET_ACTIVE_WINDOW | cut -f 2) WM_CLASS | cut -d '=' -f 2 | cut -d ',' -f 2 | sed 's/[\"\ ]//g')

if [ "$win_name" == "terminology" ]; then
	bspc window -c
else
	bspc rule -a terminology -o floating=on
	# inconsolata font; no visual bell; 256 colors
	terminology -f=Inconsolata/12 -G=false -2 -e "/bin/zsh -c 'sleep 0.01 && xdo move -x 0 -y 14 && xdo resize -w +880 && tmux attach-session -dt pretty || tmux new-session -s pretty'"
fi
