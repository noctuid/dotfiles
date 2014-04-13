# This is modified from the tmux powerline segment
# requires the following in bashrc or zshrc:
# export PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'

# Get the current path in the segment.
get_tmux_cwd() {
	local env_name=$(tmux display -p "TMUXPWD_#D" | tr -d %)
	local env_val=$(tmux show-environment | grep --color=never "$env_name")
	# The version below is still quite new for tmux. Uncomment this in the future :-)
	#local env_val=$(tmux show-environment "$env_name" 2>&1)

	if [[ ! $env_val =~ "unknown variable" ]]; then
		local tmux_pwd=$(echo "$env_val" | sed 's/^.*=//')
		echo "$tmux_pwd"
	fi
}

# switch to current path
tmux_path=$(get_tmux_cwd)
cd "$tmux_path"

__parse_git_stats(){
	type git >/dev/null 2>&1
	if [ "$?" -ne 0 ]; then
		return
	fi

	# Check if git.
	[[ -z $(git rev-parse --git-dir 2> /dev/null) ]] && return

	# Return the number of staged items.
	staged=$(git diff --staged --name-status | wc -l)
	echo "$staged"
}

git_stats=$(__parse_git_stats)

# don't echo anything if blank
if [ "$git_stats" == "" ];then
	echo ''
else
	echo ⊕ $git_stats
fi
