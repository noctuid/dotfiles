#!/bin/sh
set -eu

dir=$(dirname "$0")
layout="colemak_dh_custom"
layout=${1:-$layout}
xcompose_file=${XCOMPOSEFILE:-$HOME/.XCompose}
include_klfc="include \"%H/.XCompose-klfc-$layout\""

if ! [ -f "$dir/../XCompose" ]; then
  "$dir/uninstall-xcompose.sh" "$layout"
  exit
fi

if ! [ -f "$xcompose_file" ]; then
  echo 'include "%L"' > "$xcompose_file"
fi

if ! grep -qFx "$include_klfc" "$xcompose_file"; then
  echo "$include_klfc" >> "$xcompose_file"
fi

cp "$dir/../XCompose" "$HOME/.XCompose-klfc-$layout"
