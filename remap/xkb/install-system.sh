#!/bin/sh
set -eu

xkb_dir_from=$(dirname "$0")
xkb_dir_to="/usr/share/X11/xkb"
layout="colemak_dh_custom"
description="Colemak-DvbgHk Angle(z) Wide Custom"
mods="Wide"
description_mods="Wide"
variants=""
description_variants=""

OPTIND=1

while getopts "i:o:l:d:m:v:" opt; do
  case "$opt" in
    i) xkb_dir_from="$OPTARG";;
    o) xkb_dir_to="$OPTARG";;
    l) layout="$OPTARG";;
    d) description="$OPTARG";;
    m) mods="$OPTARG";;
    v) variants="$OPTARG";;
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
  copy_file "$xkb_dir_from/symbols/$layout" "$xkb_dir_to/symbols/$layout"
  copy_file "$xkb_dir_from/types/$layout" "$xkb_dir_to/types/$layout"
  copy_file "$xkb_dir_from/keycodes/$layout" "$xkb_dir_to/keycodes/$layout"

  if [ -f "$xkb_dir_from/types/$layout" ]; then
    add_type "$xkb_dir_to/rules/base" "$layout"
    add_type "$xkb_dir_to/rules/evdev" "$layout"
  else
    remove_type "$xkb_dir_to/rules/base" "$layout"
    remove_type "$xkb_dir_to/rules/evdev" "$layout"
  fi

  add_models "$xkb_dir_to/rules/base" "$mods" "$layout"
  add_models "$xkb_dir_to/rules/evdev" "$mods" "$layout"

  add_description "$xkb_dir_to/rules/base.lst" "$layout" "$description"
  add_description "$xkb_dir_to/rules/evdev.lst" "$layout" "$description"

  "$xkb_dir_from/scripts/remove-layout-from-xml.py" "$xkb_dir_to/rules/base.xml" "$layout"
  "$xkb_dir_from/scripts/add-layout-to-xml.py" "$xkb_dir_to/rules/base.xml" "$layout" "$description" "$variants" "$description_variants"
  "$xkb_dir_from/scripts/remove-layout-from-xml.py" "$xkb_dir_to/rules/evdev.xml" "$layout"
  "$xkb_dir_from/scripts/add-layout-to-xml.py" "$xkb_dir_to/rules/evdev.xml" "$layout" "$description" "$variants" "$description_variants"

  "$xkb_dir_from/scripts/remove-models-from-xml.py" "$xkb_dir_to/rules/base.xml" "$mods"
  "$xkb_dir_from/scripts/add-models-to-xml.py" "$xkb_dir_to/rules/base.xml" "$mods" "$description_mods"
  "$xkb_dir_from/scripts/remove-models-from-xml.py" "$xkb_dir_to/rules/evdev.xml" "$mods"
  "$xkb_dir_from/scripts/add-models-to-xml.py" "$xkb_dir_to/rules/evdev.xml" "$mods" "$description_mods"
fi

if [ "$(id -u)" -eq 0 ]; then
  if [ "${klfc_child-false}" = false ] && [ -f "$xkb_dir_from/XCompose" ]; then
    echo "Run scripts/install-xcompose.sh as user to install the XCompose file."
    echo "This is needed to make ligatures and custom dead keys work correctly."
  fi
else
  "$xkb_dir_from/scripts/install-xcompose.sh" "$layout"
fi
