#!/bin/sh
xsendkey Control+t && sleep 0.2 && xdotool type "zathura --reparent" && xdotool key Return
