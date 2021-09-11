#!/bin/sh
set -eu

xkb_dir=$(dirname "$0")
layout="colemak_dh_custom"
mod=""
variant="basic"
warning_level=0

OPTIND=1

while getopts "i:l:m:v:w:" opt; do
  case "$opt" in
    i) xkb_dir="$OPTARG";;
    l) layout="$OPTARG";;
    m) mod="$OPTARG";;
    v) variant="$OPTARG";;
    w) warning_level="$OPTARG";;
    *) exit 1;;
  esac
done

if [ -z "$layout" ]; then
  echo "Empty layout"
  exit 2
fi

keycodes="evdev"
types="complete"

if ! [ -z "$mod" ]; then
  keycodes="$keycodes+$layout($mod)"
fi

if [ -f "$xkb_dir/types/$layout" ]; then
  types="$types+$layout"
fi

setxkbmap \
    -I "$xkb_dir" \
    -layout "$layout" \
    -variant "$variant" \
    -keycodes "$keycodes" \
    -types "$types" \
    -print \
| xkbcomp \
    -w "$warning_level" \
    -I"$xkb_dir" \
    -o /tmp/temp.xkb \
    - -xkb
xkbcomp /tmp/temp.xkb -w "$warning_level" -o "$DISPLAY"
rm /tmp/temp.xkb

"$xkb_dir/scripts/install-xcompose.sh" "$layout"
