# environment variables that require X to be running or only matter for X

# shellcheck disable=SC1090
source ~/bin/helpers/monitor.sh

# * HiDPI
# honor screen DPI
# deprecated
# export QT_AUTO_SCREEN_SCALE_FACTOR=1

if monitor_is_hidpi; then
	export MONITOR_IS_HIDPI=true

	export QT_ENABLE_HIGHDPI_SCALING=1

	# NOTE not setting since now using Xft.dpi
	# scale UI lements 2x
	# export GDK_SCALE=2

	# TODO this just messes things up for chromium and makes emacs tooltips too
	# small; is there (stil?) a case where this actually prevents some issue
	# prevents double scaling of of text according to archwiki
	# export GDK_DPI_SCALE=0.5
else
	export MONITOR_IS_HIDPI=false
fi

# * WM and Polybar
width=$(monitor_get_width)
height=$(monitor_get_height)
# 15 pixels on FHD; 30 on 4k
WM_GAP=$(monitor_fraction_of 0.0078125 "$width")
export WM_GAP

# 3 pixels on FHD; 7 on 4k
WM_BORDER=$(monitor_fraction_of 0.001822917 "$width")
export WM_BORDER

# 3 pixels on 4k
HALF_WM_BORDER=$(monitor_fraction_of 0.5 "$WM_BORDER")
export HALF_WM_BORDER

# 15 pixels on4k
CORNER_RADIUS=$(monitor_fraction_of 0.5 "$WM_GAP")
export CORNER_RADIUS

BAR_WIDTH=$((width - 2 * WM_GAP))
# 65 on 4k (slightly bigger than 60 that is 2 * WM_GAP)
BAR_HEIGHT=$(monitor_fraction_of 0.0305 "$height")
BAR_X_OFFSET=$WM_GAP
BAR_Y_OFFSET=$WM_GAP
# 3 pixels FHD; 6 on 4k
BAR_BORDER=$(monitor_fraction_of 0.0015625 "$width")
TRAY_HEIGHT=$WM_GAP
export BAR_WIDTH BAR_HEIGHT BAR_X_OFFSET BAR_Y_OFFSET BAR_BORDER TRAY_HEIGHT

# pixels before top of windows
# first part is just 4 WM_GAP, but could be different
TOP_PADDING=$((2 * WM_GAP + BAR_HEIGHT + 2 * BAR_BORDER))
export TOP_PADDING

# * Network Interface
# not perfect but will generally work
WIRELESS_INTERFACE=$(ip link \
						 | awk -F ":" '/^[0-9]: wl/ {sub(" ",""); print $2}')
ETHERNET_INTERFACE=$(ip link \
						 | awk -F ":" '/^[0-9]: en/ {sub(" ",""); print $2}')

export WIRELESS_INTERFACE ETHERNET_INTERFACE

# Local Variables:
# sh-shell: bash
# End:
