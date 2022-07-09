#!/usr/bin/env bash
# TODO only allow one of these to run at a time
# keeps running if X crashes for example
# update: is that really a big deal? doesn't happen if quit X normally at least

# old amixer commands:
# status=$(amixer get Master | awk 'END { gsub(/[\[\]]/, ""); print $6 }')
# percent=$(amixer get Master | awk 'END { gsub(/[%\[\]]/, ""); print $5 }')

# shellcheck disable=SC1090
source ~/.cache/wal/colors.sh

print_volume() {
	headphones=$(amixer -c 0 contents | grep "Headphone Jack'" -A 2 \
					 | awk -F "=" 'NR==3 { print $2 }')

	percent=$(pamixer --get-volume)

	if [[ -v color1 ]] && [[ -v color12 ]]; then
		# dark red
		# shellcheck disable=SC2154
		dark_red=$color1
		# shellcheck disable=SC2154
		blue=$color12
	else
		dark_red=$(xrdb -query | awk '/*color1:/ {print $2}')
		blue=$(xrdb -query | awk '/*color12:/ {print $2}')
	fi

	color=$blue

	if [[ $(pamixer --get-mute) == true ]]; then
		color=$dark_red
		icon=""
	elif pactl info | grep --quiet "Default Sink:.*hdmi"; then
		icon=""
	elif [[ $headphones == on ]]; then
		icon=""
	else
		if [[ $percent -lt 34 ]]; then
			icon="🔈"
		elif [[ $percent -lt 67 ]]; then
			icon="🔉"
		else
			icon="🔊"
		fi
	fi

	echo "%{F$color}$icon $percent%%{F-}"
}

print_volume

while read -r event; do
	if echo "$event" | grep --quiet --invert-match --ignore-case "client"; then
		print_volume
	fi
done < <(pactl subscribe)
