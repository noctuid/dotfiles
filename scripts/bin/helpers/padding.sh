# shellcheck disable=SC1090
. ~/bin/helpers/env.sh

# get size of padding above the top of a window (including polybar)
padding_top() {
	polybar_wid=$(xdotool search --classname polybar 2> /dev/null)
	# pixels from top of screen to bottom of polybar
	polybar_padding=$(xprop -id "$polybar_wid"  _NET_WM_STRUT_PARTIAL | \
						  awk -F "," '{gsub(/ /, ""); print $3}' 2> /dev/null)
	bspc_padding=$(bspc config top_padding)
	# gap between bar and top of windows in monocle layout
	monocle_padding=$(bspc config top_monocle_padding)
	padding=$((${polybar_padding:-$bspc_padding} + monocle_padding))
	echo "$padding"
}

padding_side() {
	bspc config window_gap || echo "${BSPWM_GAP:-0}"
}

# Local Variables:
# sh-shell: sh
# End:
