#!/bin/sh
set -eu

xkb_dir_from=$(dirname "$0")
xkb_dir_to="/usr/share/X11/xkb"
layout="colemak_dh_custom"
description="Colemak-DvbgHk Angle(z) Wide Custom"
mods="Wide"

OPTIND=1

while getopts "i:o:l:d:m:" opt; do
  case "$opt" in
    i) xkb_dir_from="$OPTARG";;
    o) xkb_dir_to="$OPTARG";;
    l) layout="$OPTARG";;
    d) description="$OPTARG";;
    m) mods="$OPTARG";;
    *) exit 1;;
  esac
done

if [ -z "$layout" ]; then
  echo "Empty layout"
  exit 2
fi

. "$xkb_dir_from/scripts/functions.sh"

if [ "$(id -u)" -ne 0 ] && command -v sudo > /dev/null 2>&1; then
  sudo klfc_child=true "$0" "$@"
else
  remove_file "$xkb_dir_to/symbols/$layout"
  remove_file "$xkb_dir_to/types/$layout"
  remove_file "$xkb_dir_to/keycodes/$layout"

  remove_type "$xkb_dir_to/rules/base" "$layout"
  remove_type "$xkb_dir_to/rules/evdev" "$layout"

  remove_models "$xkb_dir_to/rules/base" "$mods" "$layout"
  remove_models "$xkb_dir_to/rules/evdev" "$mods" "$layout"

  remove_description "$xkb_dir_to/rules/base.lst" "$layout" "$description"
  remove_description "$xkb_dir_to/rules/evdev.lst" "$layout" "$description"

  "$xkb_dir_from/scripts/remove-layout-from-xml.py" "$xkb_dir_to/rules/base.xml" "$layout"
  "$xkb_dir_from/scripts/remove-layout-from-xml.py" "$xkb_dir_to/rules/evdev.xml" "$layout"

  "$xkb_dir_from/scripts/remove-models-from-xml.py" "$xkb_dir_to/rules/base.xml" "$mods"
  "$xkb_dir_from/scripts/remove-models-from-xml.py" "$xkb_dir_to/rules/evdev.xml" "$mods"
fi

if [ "$(id -u)" -eq 0 ]; then
  if [ "${klfc_child-false}" = false ] && [ -f "$xkb_dir_from/XCompose" ]; then
    echo "Run scripts/uninstall-xcompose.sh as user to uninstall the XCompose file."
  fi
else
  "$xkb_dir_from/scripts/uninstall-xcompose.sh" "$layout"
fi
