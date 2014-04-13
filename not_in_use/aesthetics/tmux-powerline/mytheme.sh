# Default Theme

if patched_font_in_use; then
	TMUX_POWERLINE_SEPARATOR_LEFT_BOLD="⮂"
	TMUX_POWERLINE_SEPARATOR_LEFT_THIN="⮃"
	TMUX_POWERLINE_SEPARATOR_RIGHT_BOLD="⮀"
	TMUX_POWERLINE_SEPARATOR_RIGHT_THIN="⮁"
else
	TMUX_POWERLINE_SEPARATOR_LEFT_BOLD="◀"
	TMUX_POWERLINE_SEPARATOR_LEFT_THIN="❮"
	TMUX_POWERLINE_SEPARATOR_RIGHT_BOLD="▶"
	TMUX_POWERLINE_SEPARATOR_RIGHT_THIN="❯"
fi

TMUX_POWERLINE_DEFAULT_BACKGROUND_COLOR=${TMUX_POWERLINE_DEFAULT_BACKGROUND_COLOR:-'235'}
TMUX_POWERLINE_DEFAULT_FOREGROUND_COLOR=${TMUX_POWERLINE_DEFAULT_FOREGROUND_COLOR:-'255'}

TMUX_POWERLINE_DEFAULT_LEFTSIDE_SEPARATOR=${TMUX_POWERLINE_DEFAULT_LEFTSIDE_SEPARATOR:-$TMUX_POWERLINE_SEPARATOR_RIGHT_BOLD}
TMUX_POWERLINE_DEFAULT_RIGHTSIDE_SEPARATOR=${TMUX_POWERLINE_DEFAULT_RIGHTSIDE_SEPARATOR:-$TMUX_POWERLINE_SEPARATOR_LEFT_BOLD}


# Format: segment_name background_color foreground_color [non_default_separator]

#took out hostname for more space.. computer not wide enough so shortening stuff
# took out rainbarf until can set up to just show ram

if [ -z $TMUX_POWERLINE_LEFT_STATUS_SEGMENTS ]; then
	TMUX_POWERLINE_LEFT_STATUS_SEGMENTS=(
		"tmux_session_info 252 234" \
		"hostname 33 0" \
		# "ifstat 30 255" \
		# "ifstat_sys 30 255" \
		#"lan_ip 24 255 ${TMUX_POWERLINE_SEPARATOR_RIGHT_THIN}" \
		#"wan_ip 24 255" \
		# "vcs_branch 29 88" \
		# "vcs_compare 60 255" \
		"vcs_staged 64 255" \
		"vcs_modified 9 255" \
		# "vcs_others 245 0" \
		# "now_playing 234 37" \
	)
fi

if [ -z $TMUX_POWERLINE_RIGHT_STATUS_SEGMENTS ]; then
	TMUX_POWERLINE_RIGHT_STATUS_SEGMENTS=(
		#"earthquake 3 0" \
		#"pwd 89 211" \
		"mailcount 9 255" \
		#"cpu 240 136" \
		#"load 237 167" \
		#"tmux_mem_cpu_load 234 136" \
		"battery 137 127" \
		#"weather 37 255" \
		#"rainbarf 0 0" \
		#"xkb_layout 125 117" \
		#"time 235 136"\
		# replaced time and date with just date as "%a %D %l:%M:%p %Z"\
		# "now_playing 234 37"\
		"date 235 136"\
		#"date_day 235 136" "date 235 136" "time 235 136" \
		#"utc_time 235 136 ${TMUX_POWERLINE_SEPARATOR_LEFT_THIN}" \
		# "${TMUX_POWERLINE_SEPARATOR_LEFT_THIN}" \
	)
fi
