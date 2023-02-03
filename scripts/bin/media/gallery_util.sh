takedir() {
	mkdir -p "$1"
	cd "$1" || exit 1
}

BASE_GALLERY_DIR=$HOME/database/move/gallery/

gallery_dl_command() {
	local cookies_file user_agent
	cookies_file=$BASE_GALLERY_DIR/cookies.txt
	# NOTE: need to check headers to get user agent (can differ from what
	# shown in javascript console)
	# cat instead of < to silence errors
	user_agent=$(cat "$BASE_GALLERY_DIR"/user-agent.txt 2> /dev/null)
	if [[ -f $cookies_file ]] && [[ -n $user_agent ]]; then
		gallery-dl -o user-agent="$user_agent" --cookies "$cookies_file" "$@"
	else
		gallery-dl "$@"
	fi
}

take_gallery_dir() { # <url>
	local url info site title tags
	url=$1
	takedir "$BASE_GALLERY_DIR"

	info=$(gallery_dl_command --range 0 --dump-json "$url")
	site=$(echo "$info" | jq --raw-output '.[] | .[] | .category?')
	title=$(echo "$info" | jq -r '.[] | .[] | .title?')
	tags=$(echo "$info" | jq -r '.[] | .[] | .search_tags?')

	if [[ $title == null ]]; then
		title=
	fi
	if [[ $tags == null ]]; then
		tags=
	fi
	takedir "./$site/$title/$tags"
}

# Local Variables:
# sh-shell: bash
# End:
