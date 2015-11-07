#!/bin/bash
# script to prepare icon for dunst:
# 1. convert to png (dunst only works with png currently)
# 2. resize if icon too large

# appname="$1"
summary="$2"
body="$3"
image="$4"
urgency="$5"

if [[ -f "$image" ]] && \
	! file --mime-type "$image" | grep -q "image/png" || \
		[[ $(identify "$image"| awk 'sub("x.*","",$3) {print $3}') -gt 200 ]]; then
		file_name=$(basename "$image")
		file_name=${file_name%.*}
		mkdir -p /tmp/dunst-images
		new_image=/tmp/dunst-images/"${file_name}.png"
		convert -resize 200x -format png "$image" "$new_image"
else
	# no changes necessary
	new_image="$album_art"
fi

# send notification with compatible image
notify-send -a "prepare_image" -u "$urgency" -i "$new_image" "$summary" "$body"
