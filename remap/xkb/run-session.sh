#!/bin/sh
set -eu

xkb_dir=$(dirname "$0")
layout="colemak_dh_custom"
mod=""
warning_level=0

OPTIND=1

while getopts "i:l:m:w:" opt; do
  case "$opt" in
    i) xkb_dir="$OPTARG";;
    l) layout="$OPTARG";;
    m) mod="$OPTARG";;
    w) warning_level="$OPTARG";;
    *) exit 1;;
  esac
done

if [ -z "$layout" ]; then
  echo "Empty layout"
  exit 2
fi

if [ -z "$mod" ]; then
  extra_keycodes=""
else
  extra_keycodes="+$layout($mod)"
fi

setxkbmap \
    -I "$xkb_dir" \
    -layout "$layout" \
    -keycodes "evdev+$extra_keycodes" \
    -types "complete+$layout" \
    -compat "complete" \
    -print \
| xkbcomp \
    -w "$warning_level" \
    -I"$xkb_dir" \
    -o /tmp/temp.xkb \
    - -xkb
#    -compat "complete+$layout" \
#    -compat "basic+iso9995+level5" \
#    - "$DISPLAY"
xkbcomp /tmp/temp.xkb -w "$warning_level" -o "$DISPLAY"
rm /tmp/temp.xkb
