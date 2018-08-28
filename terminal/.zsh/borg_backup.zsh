#!/usr/bin/env bash
# TODO reduce code duplication
# TODO consider using --verify-data always (at least for minimal backup)
# TODO fix luks/lvm mounting (stop using sleep)

# TODO mount with discard for database (--allow-discards)

# Some notes about borg:
# - borg supports encryption, compression, deduplication, and incremental
#	backups
# - encryption is done locally (borg is suitable for remote backups)
# - borg supports resuming after failure
# - borg's chunking works on large files (e.g. VMs) even if they are moved
# - borg supports mounting, extracting from, and restoring backup archives
# - borg uses /tmp or you can set TMPDIR
# - borg stores symlinks as symlinks by default (wanted behavior); see
#	https://github.com/borgbackup/borg/issues/1003
# - hardlinks and most useful attributes are stored as well
#	(https://borgbackup.readthedocs.io/en/stable/usage/general.html)

# * Helpers
# ** General
confirm_do() {
	if [[ $# -lt 1 ]]; then
		echo 'An argument corresponding to the action to take is required.'
		return 1
	fi
	# NOTE read is different for zsh
	if read -q REPLY; then
		"$@"
	else
		return 1
	fi
}

remove_trailing_slash() {
	echo "$1" | sed -r 's:/$::'
}

convert_slashes() {
	echo "$1" | sed -r 's:/:\!:g'
}

# ** LUKS
luks_backup_dir=~/ag-sys/backup/luks

luks_header_backup_file() {
	echo "$luks_backup_dir/$1_$(date +%Y.%m.%d)"_header.img
}

# https://gitlab.com/cryptsetup/cryptsetup/wikis/FrequentlyAskedQuestions
luks_create() {
	if [[ $# -ne 2 ]]; then
		echo 'Two arguments required: /path/to/partition new-label'
		return 1
	elif [[ ! -e $1 ]];then
		echo "Error. $1 does not exist."
		return 1
	fi
	echo "Wipe file system and then run cryptsetup luksFormat on $1? (y/n)"
	confirm_do echo || return 1
	partition=$1
	label=$2

	# https://gitlab.com/cryptsetup/cryptsetup/wikis/FrequentlyAskedQuestions#2-setup
	# eventually wiping will be implemented in cryptsetup directly
	if ! sudo wipefs "$partition"; then
		echo "Failed to wipe $partition."
		return 1
	fi

	# overkill (doesn't hurt)
	# verify-password is the default
	# argon2i is the default for luks2; aes-xts-plain64 is default for luks
	# https://security.stackexchange.com/questions/40208/recommended-options-for-luks-cryptsetup/40218#40218
	# sha512 (about as fast as sha256); --key-size 512 is overkil
	# longer iteration time (tradeoff is that it takes longer to open)
	# use /dev/random instead of /dev/urandom (will block if not enough entropy;
	# using rngd)
	if sudo cryptsetup --verbose --verify-passphrase --use-random --type luks2 \
			--key-size 256 --hash sha512 --cipher aes-xts-plain64 \
			--pbkdf argon2i --iter-time 3000 --label "$label" \
			luksFormat "$partition"
	then
		echo "Successfully encrypted $partition."
		# since backing up current header (which could potentially be used to
		# gain access with old password) and for SSDs (because of problems with
		# secure disk erasure)
		echo 'Remember that you should avoid changing the password.'
	else
		echo 'Encryption failed.'
		return 1
	fi

	if ! mkdir -p "$luks_backup_dir"; then
		echo "Failed to create $luks_backup_dir."
		return 1
	fi
	if sudo cryptsetup luksHeaderBackup \
			--header-backup-file "$(luks_header_backup_file "$label")" \
			"$partition" \
			&& sudo chown -R "$USER":users "$luks_backup_dir"
	then
		echo 'Successfully backed up header.'
	else
		echo 'Header backup failed.'
		return 1
	fi

	echo "Create an lvm volume group on $partition? (y/n)"
	confirm_do echo || return 0
	# deactivate current volume group so not created as part of
	sudo vgchange -a n || return 1
	if sudo cryptsetup open "$partition" luks_"$label" \
			&& sudo vgcreate "$label"_group /dev/mapper/luks_"$label"
	then
		echo "New LUKS partition is mounted at /dev/mapper/luks_$label."
		echo "New volume group is named ${label}_group."
		echo 'Use lvcreate to create volumes.'
	else
		echo 'Failed to create volume group.'
	fi
}

mount_luks_lvm() {
	label=$1
	volume=$2
	dir=$3
	if ! sudo cryptsetup status luks_"$label" | grep -q "in use"; then
		# Note that --allow-discards makes it potentially possible to find out
		# fs type, used space, etc.
		sudo cryptsetup open --allow-discards \
			 /dev/disk/by-label/"$label" luks_"$label"
	fi
	if ! mkdir -p "$dir"; then
		echo "Failed to create mount point $dir."
	fi
	# TODO unfortunately neither of these block until the symlinks are created
	sudo vgchange --activate y
	# sudo lvchange --activate y "$label"_group 
	# horrible workaround for now
	sleep 2
	if mountpoint -q "$dir"; then
		echo "$dir is already a mountpoint."
	else
		sudo mount /dev/"$label"_group/"$volume" "$dir"
	fi
}


mount_luks_lvm_backup() {
	label=$1
	mount_luks_lvm "$label" backup ~/backup_mount/"$label"
}

umount_luks_lvm() {
	label=$1
	# don't fail if some aren't mounted (next command will already fail)
	sudo umount /dev/"$label"_group/*
	sudo vgchange --activate n "$label"_group \
		&& sudo cryptsetup close luks_"$label"
}

alias mountbigseagate='mount_luks_lvm_backup bigseagate'
alias umountbigseagate='umount_luks_lvm bigseagate'

alias mountdatab="mount_luks_lvm cryptother database ~/database"
# jus tunmount lvm only
alias umountdatab='sudo umount /dev/cryptother_group/*'

# ** Borg
# lz4 is the default
alias borg_bk='borg create -v --stats --progress --compression lz4'

borg_prune() {
	# - only delete archives made by the current host (probably will never be an
	#	issue)
	# - backups selected by rules for shorter time periods do not counts towards
	#	those for larger time periods
	# - keep everything within past 7 days; the last 4 weekly (latest each week)
	#	backups (one could be more than 4 weeks old, for example, if there was a
	#	week without backups), etc.
	# TODO if size becomes an issue, lower monthly
	local repo
	repo="$1"
	borg prune -v --list --prefix '{hostname}-' --keep-within 7d \
		 --keep-weekly=4 --keep-monthly=24 --keep-yearly=-1 "$repo"
}

# same datetime format as borg 1.1 will have as default
backup_format='{hostname}-{user}-{now:%Y-%m-%dT%H:%M:%S}'

# directory for backing up repository keys and config files
borg_backup_dir=~/ag-sys/backup/borg

borg_key_backup_file() {
	echo "$borg_backup_dir/$(convert_slashes "$1")_$(date +%Y.%m.%d)"_key
}

borg_config_backup_file() {
	echo "$borg_backup_dir/$(convert_slashes "$1")_$(date +%Y.%m.%d)"_config
}

# ** Rsync
backup_rsync() {
	# long flags to make easier to read
	# no --delete or --delete-excluded; --update
	# not using --numeric-ids for now
	rsync --verbose --info=progress2 --human-readable --ignore-errors \
		  --recursive --links --hard-links \
		  --perms --times --group --owner --acls --xattrs \
		  --update --prune-empty-dirs \
		  --partial --whole-file --sparse "$@"
}

# can't pass options
# rsync_home() {
# 	backup_rsync ~/ "$1"
# }

# ** Drive Unmounting
maybe_eject() {
	echo 'Unmount last mounted external drive? (y/n)'
	# confirm_do devmon --unmount-recent --no-gui || return 1
	confirm_do udiskie-umount "$1" || return 1
}

# * Minimal Backup
# For things that change most frequently and for most important smaller things
borg_small() {
	if [[ $# -ne 1 ]]; then
		echo 'One argument is required: /path/to/backup_drive_dir'
		return 1
	elif [[ ! -d $1 ]];then
		echo "Error. Directory $1 does not exist."
		return 1
	fi

	local backup_dir
	backup_dir="$(remove_trailing_slash "$1")"/minimal_backup
	mkdir -p "$backup_dir"
	mkdir -p "$borg_backup_dir"
	# password only (key stored in repo)
	# backup key and config file when first creating repo (will not run if
	# repository exists); backing up the key with repokey isn't really necessary
	# but protects against case where only key is corrupted
	borg init --encrypt=repokey-blake2 "$backup_dir" 2> /dev/null \
		&& borg key export "$backup_dir" "$(borg_key_backup_file "$backup_dir")" \
		&& cp "$backup_dir"/config "$(borg_config_backup_file "$backup_dir")"

	# NOTE
	# - check --repair is still experimental
	# - check --verify-data is slow (not that bad for this small backup though)
	if ! borg check -v "$backup_dir"; then
		echo 'Repository corrupted or initialization failed.'
		return 1
	fi

	cd ~/ || return 1
	if borg_bk --patterns-from='.zsh/borg_minimal.txt' \
			   "$backup_dir"::"$backup_format" ./
	then
		notify-send --icon=tropy-gold 'Minimal backup completed successfully.'
	else
		notify-send --icon=face-angry 'Minimal backup failed.'
	fi

	# if/when size becomes an issue, start pruning:
	# borg_prune "$backup_dir"

	maybe_eject "$1"
}

# * Full Home Backup
borg_home() {
	if [[ $# -ne 1 ]]; then
		echo 'One argument is required: /path/to/backup_drive_dir'
		return 1
	elif [[ ! -d $1 ]];then
		echo "Error. Directory $1 does not exist."
		return 1
	fi

	local backup_dir
	backup_dir="$(remove_trailing_slash "$1")"/home
	mkdir -p "$backup_dir"
	mkdir -p "$borg_backup_dir"
	borg init --encryption=none "$backup_dir" 2> /dev/null \
		&& cp "$backup_dir"/config "$(borg_config_backup_file "$backup_dir")"

	if ! borg check -v "$backup_dir"; then
		echo 'Repository corrupted or initialization failed.'
		return 1
	fi

	cd ~/ || return 1
	if borg_bk --patterns-from=".zsh/borg_full.txt" \
			   "$backup_dir"::"$backup_format" ./
	then
		notify-send --icon=trophy-gold 'Home backup completed successfully.'
	else
		notify-send --icon=face-angry 'Home backup failed.'
	fi
}

# * PSP
# vita
# alias cm="qcma --verbose"

# tree "$psp_dir"/ISO > "$psp_dir"/isos.txt
# ls "$psp_dir"/PSP/GAME/PSX > "$psp_dir"/psx.txt
# trailing slash is important
# backup_rsync --exclude={"ISO/*","PSX/*"} "$psp_dir"/ ~/database/gaming/psp/backup/

# * Specific
# ** Minimal
kingston() {
	borg_small /media/kingston
}

samsung() {
	borg_small /media/samsung
}

# ** Home
# backup to main external drive
bigseagate() {
	mountdatab || return 1
	mountbigseagate || return 1
	borg_home ~/backup_mount/bigseagate
}

# * Remote Backup
# consider using append-only? (probably not)
# use stronger compression
# sshfs or usre@host:path
# have to use rclone for most cloud services (or a vps)
# rsync.net has a borg deal but is comparatively expensive

# * Restoration
# (when) will it overwrite files?

