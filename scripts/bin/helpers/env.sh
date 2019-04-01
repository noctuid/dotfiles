# TODO move any environment variables that it would make sense to resource when
# reloading something here; maybe just move all environment variables here

# * BSPWM and Polybar
# shellcheck disable=SC1090
. ~/bin/helpers/monitor.sh

# 15 pixels on FHD; 30 on 4k
BSPWM_GAP=$(monitor_fraction_of_width 0.0078125)
# 3 pixels on FHD; 6 on 4k
BSPWM_BORDER=$(monitor_fraction_of_width 0.0015625)

BAR_WIDTH=$(($(monitor_fraction_of_width 1) - 2 * BSPWM_GAP))
BAR_HEIGHT=$((2 * BSPWM_GAP))
BAR_X_OFFSET=$BSPWM_GAP
BAR_Y_OFFSET=$BSPWM_GAP
BAR_BORDER=$BSPWM_BORDER

export BSPWM_GAP BSPWM_BORDER
export BAR_WIDTH BAR_HEIGHT BAR_X_OFFSET BAR_Y_OFFSET BAR_BORDER
