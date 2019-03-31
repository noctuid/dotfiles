monitor_get_primary() {
	xrandr --listmonitors | awk '/0:/ {print $NF}'
}

monitor_get_geometry() { # monitor
	xrandr --query \
		| awk "/^$1/ {gsub(\"primary \",\"\"); print \$3}"
}

monitor_get_dimensions() { # monitor
	monitor=$1
	xrandr --query \
		| awk -F '+| ' "/^$monitor/ {gsub(\"primary \",\"\"); print \$3}"
}

monitor_percent_of_width() { # percentage
	percentage=$1
	monitor_get_dimensions "$(monitor_get_primary)" \
		| awk -F 'x' "{printf \"%.0f\n\", \$1*$percentage}"
}

# Local Variables:
# sh-shell: bash
# End:
