#!/bin/sh
set -eu

xkb_dir_from=$(dirname "$0")
xkb_dir_to="/usr/share/X11/xkb"
layout="colemak_dh_custom"
description="Colmeak-DvbgHm Angle(z) Wide Custom"
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

confirm () {
  # call with a prompt string or use a default
  printf "%s [y/N] " "${1:-Are you sure?}"
  read -r response
  case "${response:-${2:-}}" in
    [yY][eE][sS]|[yY]) true;;
    [nN][oO]|[nN]) false;;
    *) confirm "${1:-}" "${2:-}";;
  esac
}

copy_file () {
  file_from=$1
  file_to=$2

  if [ ! -e "$file_to" ] || confirm "$file_to already exists. Overwrite?"; then
    cp "$file_from" "$file_to"
  fi
}

add_type () {
  file=$1
  layout=$2

  for group in 0 1 2 3 4; do
    if [ "$group" -eq 0 ]; then
      header="! layout	=	types"
      line="  $layout	=	+$layout"
    else
      header="! layout[$group]	=	types"
      line="  $layout	=	+$layout:$group"
    fi

    if grep -qFx "$line" "$file"; then
      return
    fi

    echo "" >> "$file"
    echo "$header" >> "$file"
    echo "$line" >> "$file"
  done
}

add_models () {
  file=$1
  mods=$2
  layout=$3

  for group in 0 1 2 3 4; do
    if [ "$group" -eq 0 ]; then
      header="! model	=	keycodes"
    else
      header="! model[$group]	=	keycodes"
    fi

    header_written=false
    for mod in $mods; do
      if [ "$group" -eq 0 ]; then
        line="  mod_$mod	=	+$layout($mod)"
      else
        line="  mod_$mod	=	+$layout($mod):$group"
      fi

      if grep -qFx "$line" "$file"; then
        continue
      fi

      if [ "$header_written" = false ]; then
        echo "" >> "$file"
        echo "$header" >> "$file"
        header_written=true
      fi
      echo "$line" >> "$file"
    done
  done
}

add_description () {
  file=$1
  layout=$2
  description=$3
  header="! layout"
  line="  $layout	$description"

  if grep -qFx "$line" "$file"; then
    return
  fi

  echo "" >> "$file"
  echo "$header" >> "$file"
  echo "$line" >> "$file"
}

copy_file "$xkb_dir_from/symbols/$layout" "$xkb_dir_to/symbols/$layout"
copy_file "$xkb_dir_from/types/$layout" "$xkb_dir_to/types/$layout"
copy_file "$xkb_dir_from/keycodes/$layout" "$xkb_dir_to/keycodes/$layout"

add_type "$xkb_dir_to/rules/base" "$layout"
add_type "$xkb_dir_to/rules/evdev" "$layout"

add_models "$xkb_dir_to/rules/base" "$mods" "$layout"
add_models "$xkb_dir_to/rules/evdev" "$mods" "$layout"

add_description "$xkb_dir_to/rules/base.lst" "$layout" "$description"
add_description "$xkb_dir_to/rules/evdev.lst" "$layout" "$description"

"$xkb_dir_from/scripts/add-layout-to-xml.py" "$xkb_dir_to/rules/base.xml" "$layout" "$description"
"$xkb_dir_from/scripts/add-layout-to-xml.py" "$xkb_dir_to/rules/evdev.xml" "$layout" "$description"
