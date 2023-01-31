#!/usr/bin/env bash
# show icon depending on whether or not wireguard is in use

# shellcheck disable=SC1090
source ~/.cache/wal/colors.sh
# shellcheck disable=SC2154
dark_red=$color1
# shellcheck disable=SC2154
green=$color10

# networkctl doesn't require sudo
wireguard_interface=$(networkctl 2> /dev/null | awk '$3 ~ /wireguard/ {print $2}')

if [[ -n $wireguard_interface ]]; then
	echo "%{F$green}%{F-}"
else
	echo "%{F$dark_red}%{F-}"
fi
