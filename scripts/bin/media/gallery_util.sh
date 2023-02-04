takedir() {
	mkdir -p "$1"
	cd "$1" || exit 1
}

BASE_GALLERY_DIR=$HOME/database/move/gallery/
COOKIES_FILE=$BASE_GALLERY_DIR/cookies.txt
# NOTE: need to check headers to get user agent (can differ from what
# shown in javascript console)
# cat instead of < to silence errors
USER_AGENT=$(cat "$BASE_GALLERY_DIR"/user-agent.txt 2> /dev/null)

gallery_dl_command() {
	if [[ -f $COOKIES_FILE ]] && [[ -n $USER_AGENT ]]; then
		gallery-dl -o user-agent="$USER_AGENT" --cookies "$COOKIES_FILE" "$@"
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
