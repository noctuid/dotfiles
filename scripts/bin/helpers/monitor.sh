monitor_get_primary() {
	xrandr --current | awk '/primary/ {print $1}'
}

monitor_get_geometry() { # [monitor]
	if [[ -n $1 ]]; then
		xrandr --current \
			| awk "/^$1/ {gsub(\"primary \",\"\"); print \$3}"
	else
		xrandr --current \
			| awk '/primary/ {print $4}'
	fi
}

monitor_get_dimensions() { # [monitor]
	if [[ -n $1 ]]; then
		xrandr --current \
			| awk -F '+| ' "/^$1/ {gsub(\"primary \",\"\"); print \$3}"
	else
		xrandr --current \
			| awk -F '+| ' '/primary/ {print $4}'
	fi
}

monitor_get_width() { # [monitor]
	local geometry
	geometry=$(monitor_get_geometry "$1")
	echo "${geometry%x*}"
}

monitor_fraction_of() { # <decimal number> <total>
	# awk "BEGIN {printf(\"%.0f\", $1 * $2)}"
	# slightly faster
	echo "($1 * $2) / 1" | bc
}

monitor_is_hidpi() { # monitor
	local geometry width
	geometry=$(monitor_get_geometry "$1")
	width=${geometry%x*}
	# not perfect but sufficient
	[[ $width -gt 3000 ]]
}

# Local Variables:
# sh-shell: bash
# End:
