#!/bin/sh
# to have window title in screenshot name

# delete everything after a period; probably not necessary.. maybe truncate
stripped_title=$(xtitle | sed 's/\..*$//')

# figure out how to keep $w and $h; won't work like normal
scrot -q 75 ~/Move/Screenshots/"%m.%d.%y_%H:%M:%S_-_${w}x${h}_${stripped_title}.png"
# scrot -q 75 ~/Move/Screenshots/"%m.%d.%y_%H:%M:%S_-_$wx$h_$(xtitle | sed 's/\..*$//').png"
