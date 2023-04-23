#!/usr/bin/env bash
# TODO reduce code duplication
# TODO fix luks/lvm mounting (stop using sleep)
# TODO keep eye on rustic, kopia, and bupstash
# TODO below about backing up config


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

# Some notes about restic:
# - Restic supports these same features (encryption, deduplication, resume,
#   restoring hardlinks, symlinks as symlinks, etc.)
# - Restic now supports compression and more complex include/exclude patterns
# - Restic has much better support for things like backblaze (borg has no
#   support except through rclone)

# * Helpers
# ** General
confirm_do() {
	if [[ $# -lt 1 ]]; then
		echo 'An argument corresponding to the action to take is required.'
		return 1
	fi
	# NOTE read is different for zsh
	# shellcheck disable=SC2162
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

alias luks_defaults='cryptsetup --help'

luks_header_backup_file() {
	echo "$luks_backup_dir/$1_$(date +%Y.%m.%d)"_header.img
}

# https://gitlab.com/cryptsetup/cryptsetup/wikis/FrequentlyAskedQuestions
luks_create() {
	partition=$1
	label=$2
	if [[ $# -ne 2 ]]; then
		echo 'Two arguments required: /path/to/disk-or-partition new-label'
		echo 'e.g. luks_create /dev/sdz some-label'
		return 1
	elif [[ ! -e $partition ]]; then
		echo "Error: $partition does not exist."
		return 1
	elif [[ -e /dev/mapper/luks_"$label" ]]; then
		echo "Error: A LUKS partition is already open with that label/name."
		return 1
	fi
	echo "Wipe file system and then run cryptsetup luksFormat on $partition? (y/n)"
	confirm_do echo || return 1

	# https://gitlab.com/cryptsetup/cryptsetup/wikis/FrequentlyAskedQuestions#2-setup
	# eventually wiping will be implemented in cryptsetup directly
	if sudo wipefs "$partition"; then
		echo "Successfully wiped $partition."
	else
		echo "Failed to wipe $partition."
		return 1
	fi

	# can check defaults each time use to ensure still using more secure settings
	echo "Do you want to continue without updating crypsetup flags? (y/n)"
	confirm_do echo || return 1

	# overkill (doesn't hurt)
	# https://wiki.archlinux.org/title/Dm-crypt/Device_encryption#Encryption_options_for_LUKS_mode
	# https://security.stackexchange.com/questions/40208/recommended-options-for-luks-cryptsetup/40218#40218
	# can check defaults with cryptsetup --help
	# - --verify-passphrase (2.5.0 default) - query for passwords twice
	# - --use-random (not default) - use /dev/random instead of /dev/urandom
	#    when creating volume master key (will block if not enough entropy;
	#    using rngd)
	# - --hash - sha256 is default; using sha512 since it's about as fast
	# - --cipher aes-xts-plain64 is default
	# - --pbkdf argon2id is default for luks2
	# - --key-size - not explicitly specifying; default of 256 is fine
	# - --iter-time - increase from default 2000 to 3000
	if sudo cryptsetup --verbose --verify-passphrase --use-random --type luks2 \
			--hash sha512 --cipher aes-xts-plain64 --pbkdf argon2id \
			--iter-time 3000 --label "$label" \
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
	if ! confirm_do echo; then

		echo 'Make the new LUKS partition a single ext4 partition? (y/n)'
		confirm_do echo || return 0
		if ! sudo cryptsetup open "$partition" luks_"$label"; then
			echo 'Failed to open the LUKS partition'
			return 1
		fi
		sudo mkfs.ext4 /dev/mapper/luks_"$label" || return 1
		echo "Successfully created an ext4 partition"
		echo "After mounting, chown:"
		# shellcheck disable=SC2016
		echo '# chown -R "$USER" /path/to/mountpoint'

		return 0
	fi

	if sudo cryptsetup open "$partition" luks_"$label" \
			&& sudo vgcreate "$label"_group /dev/mapper/luks_"$label"
	then
		echo "New LUKS partition is mounted at /dev/mapper/luks_$label."
		echo "New volume group is named ${label}_group."
		echo 'Use lvcreate to create volumes.'
		echo 'Example:'
		echo '# lvcreate -l 100%FREE eightseagate_group -n backup'
		echo '# mkfs.ext4 /dev/eightseagate_group/backup'
		echo '# mount /dev/eightseagate_group/backup /path/to/mountpoint'
		# shellcheck disable=SC2016
		echo '# chown -R "$USER" /path/to/mountpoint'
	else
		echo 'Failed to create volume group.'
	fi
}

cryptsetup_open() { # <label>
	label=$1
	if sudo cryptsetup status luks_"$label" | grep -q "in use"; then
		echo "A luks partition named/labeled $label has already been opened"
		echo "Skipping opening"
	else
		# https://wiki.archlinux.org/title/Dm-crypt/Specialties#Discard/TRIM_support_for_solid_state_drives_(SSD)
		# Note that --allow-discards makes it potentially possible to find out
		# fs type, used space, etc.
		sudo cryptsetup open --allow-discards \
			 /dev/disk/by-label/"$label" luks_"$label"
	fi

}

mount_luks() { # <label> <mountdir>
	label=$1
	mountdir=$2
	if ! cryptsetup_open "$label"; then
		echo "Failed to open LUKS container $label"
		return 1
	fi
	if ! mkdir -p "$mountdir"; then
		echo "Failed to create mount point $dir."
		return 1
	fi
	if mountpoint -q "$mountdir"; then
		echo "$mountdir is already a mountpoint."
	else
		sudo mount /dev/mapper/luks_"$label" "$mountdir"
	fi
}

umount_luks() { # <label> <mountdir>
	label=$1
	mountdir=$2
	sudo umount "$mountdir" && sudo cryptsetup close luks_"$label"
}

mount_luks_lvm() { # <label> <volume> <mountdir>
	label=$1
	volume=$2
	dir=$3
	if ! cryptsetup_open "$label"; then
		echo "Failed to open LUKS container $label"
		return 1
	fi

	if ! mkdir -p "$dir"; then
		echo "Failed to create mount point $dir."
		return 1
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

alias mounteightseagate='mount_luks_lvm_backup eightseagate'
alias umounteightseagate='umount_luks_lvm eightseagate'

mountdatab() {
	mount_luks database ~/database
}
umountdatab() {
	umount_luks database ~/database
}

mountolddatab() {
	if ! mountpoint -q ~/database; then
		sudo mount /dev/cryptlinux_group/database ~/database
	else
		echo "$HOME/database is already a mountpoint. Not mounting again."
	fi
}
umountolddatab() {
	sudo umount /dev/cryptlinux_group/database
}

# ** Borg
# lz4 is the default
borg_bk() {
	borg create --verbose --stats --progress "$@"
}
borg_bk_online() {
	# use more compression for online backup
	# level 6 is default; higher is pointless according to borg help compression
	borg create -v --stats --progress --compression lzma "$@"
}

borg_prune() {
	# - only delete archives made by the current host (probably will never be an
	#   issue)
	# - backups selected by prior rules do not counts towards those for later
	#   rules (e.g. if a backup is kept because of the 7d rule, it won't be
	#   considered for the weekly rule; the weekly rule will start keeping
	#   backups before the oldest backup kept by the 7d rule)
	# - keep everything within 7 days before most recent snapshot; keep the last
	#   4 weekly (latest each week with a backup) backups (one could be more
	#   than 4 weeks old, for example, if there was a week without backups),
	#   etc.
	# - negative argument means to keep a backup for every time duration (here,
	#   keep a backup from the end of every year)
	# TODO if size becomes an issue, lower monthly and maybe yearly
	local repo
	repo="$1"
	borg prune -v --list --prefix '{hostname}-' --keep-within=7d \
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

# ** Restic
# initialize restic repo if it does not exist
# backup up key for borg; key is stored in the repo so only potential use case
# is if just the key in the repo gets corrupted; unlikely so not doing for
# restic currently
restic_init() { # <backup dir>
	local backup_dir
	backup_dir=$1
	# this is what is recommended, but it requires password...
	# if ! restic --repo "$backup_dir" snapshots 2> /dev/null; then
	# 	restic init --repo "$backup_dir"
	# fi

	# https://github.com/restic/restic/issues/3688
	if [[ ! -d $backup_dir/data ]] || [[ ! -d $backup_dir/snapshots ]] \
		   || [[ ! -f $backup_dir/config ]]; then
		restic init --repo "$backup_dir"
	fi
}

restic_bk() {
	restic --verbose backup "$@"
}

restic_prune() {
	# rules are essentially the same for restic as for borg
	# the main difference is that borg will go backwards since last kept backup;
	# for example, with --keep-yearly 1, if the last backup of the year is
	# already included because of another rule, borg will keep a snapshot from
	# the previous year but restic considers the rule already satisfied
	repo=$1
	# TODO make consistent with borg?
	restic forget --repo "$repo" --prune --host "$(hostname)" \
		   --keep-within-duration 7d --keep-weekly 5 --keep-monthly 2 \
		   --keep-yearly 4
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

sudo_backup_rsync() {
	sudo rsync --verbose --info=progress2 --human-readable --ignore-errors \
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

# ** File Copying and Tree to File Backups
backup_tree_dirs="
$HOME/anime
# won't include in full remote backup; can rerip if necessary
$HOME/music
"

backup_tree_dirs() {
	# -J
	local backup_dir
	backup_dir="$HOME/database/backup/trees"
	mkdir -p "$backup_dir"

while read -r dir; do
	if [[ -n $dir ]] && [[ ! ${dir:0:1} == '#' ]]; then
		name=${dir//\//-}
		tree -a --dirsfirst -o "$backup_dir/$name".txt "$dir"

	fi
done <<< "$backup_tree_dirs"
}

pre_backup() {
	backup_tree_dirs
	mkdir -p ~/ag-sys/backup/sdvx
	cp /opt/unnamed-sdvx-clone/maps.db ~/ag-sys/backup/sdvx/
}

# * Minimal Backup
# For things that change most frequently and for most important smaller things
borg_small() {
	check=true
	if [[ $1 == -n ]]; then
		check=false
		shift
	fi
	if [[ $# -ne 1 ]]; then
		echo 'One argument is required: /path/to/backup_drive_dir'
		return 1
	fi

	local borgbase
	borgbase=false
	if echo "$1" | grep --quiet "repo.borgbase.com"; then
		borgbase=true
	fi

	if ! $borgbase && [[ ! -d $1 ]]; then
		echo "Error. Directory $1 does not exist."
		return 1
	fi

	# TODO don't think can copy "$backup_dir"/config for remote repo but see
	# what happens; can retrieve with rsync and key?

	local backup_dir
	backup_dir=$1
	if ! $borgbase; then
		backup_dir="$(remove_trailing_slash "$1")"/minimal_backup
		mkdir -p "$backup_dir"
	fi
	mkdir -p "$borg_backup_dir"
	# password only (key stored in repo)
	# backup key and config file when first creating repo (will not run if
	# repository exists); backing up the key with repokey isn't really necessary
	# but protects against case where only key is corrupted
	if borg init --encryption=repokey-blake2 "$backup_dir"; then
		borg key export "$backup_dir" "$(borg_key_backup_file "$backup_dir")" \
			&& cp "$backup_dir"/config "$(borg_config_backup_file "$backup_dir")"
	else
		echo "Repo exists already or initialization failed."
	fi

	# NOTE
	# - check --repair is still experimental
	# - check --verify-data is slow (not that bad for this small backup though)
	if $check; then
		if ! borg check -v "$backup_dir"; then
			echo 'Repository corrupted or initialization failed.'
			return 1
		fi
	fi

	cd ~/ || return 1
	bk_command=borg_bk
	if $borgbase; then
		bk_command=borg_bk_online
	fi
	if $bk_command --patterns-from='.zsh/borg_minimal.txt' \
				   "$backup_dir"::"$backup_format"

	then
		notify-send --icon=tropy-gold 'Minimal backup completed successfully.'
	else
		notify-send --icon=face-angry 'Minimal backup failed.'
	fi

	# if/when size becomes an issue, start pruning:
	# borg_prune "$backup_dir"

	if ! $borgbase; then
		maybe_eject "$1"
	fi
}

# * PSP
# vita
# alias cm="qcma --verbose"

# tree "$psp_dir"/ISO > "$psp_dir"/isos.txt
# ls "$psp_dir"/PSP/GAME/PSX > "$psp_dir"/psx.txt
# trailing slash is important
# backup_rsync --exclude={"ISO/*","PSX/*"} "$psp_dir"/ ~/database/gaming/psp/backup/

# * Restic
# ** Full Backup
restic_full() {
	local check_data
	check_data=false
	if [[ $1 =~ ^(-c|--check)$ ]]; then
		check_data=false
		shift
	fi
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

	# restic does not support unencrypted backups, but this is not a must have
	# https://github.com/restic/restic/issues/1018
	# would prefer to just use LUKS on the entire backup drive so can unlock
	# once instead of having to type password every time want to do anything
	restic_init "$backup_dir"

	# TODO test how long normal check takes
	if $check_data; then
		# this will require reading the entire repository, which will be
		# extremely slow for an external hdd
		if ! restic --repo "$backup_dir" check --read-data; then
			echo 'Repository corrupted or initialization failed.'
			return 1
		fi
	else
		if ! restic --repo "$backup_dir" check; then
			echo 'Repository corrupted or initialization failed.'
			return 1
		fi
	fi

	cd ~/ || return 1
	pre_backup
	# TODO if remote --exclude-if-present=".no_remote_backup"
	if restic_bk --repo "$backup_dir" \
				 --exclude-if-present=".backup_tree_only" \
				 --exclude-if-present=".no_backup" \
				 --exclude-file=".zsh/restic_full_exclude.txt" \
				 ~/
	then
		notify-send --icon=trophy-gold 'Full backup completed successfully.'
	else
		notify-send --icon=face-angry 'Full backup failed.'
	fi
}

# * Specific
# ** Minimal
kingston() {
	borg_small /media/kingston
}

samsung() {
	borg_small /media/samsung
}

borgbase_small() {
	borg_small "$@" "nwikooyy@nwikooyy.repo.borgbase.com:repo"
}

# ** Home
# bigseagate() {
# 	mountdatab || return 1
# 	mountbigseagate || return 1
# 	borg_ome ~/backup_mount/bigseagate
# }

fivewd() {
	mountdatab || return 1
	restic_full /media/fivewd
}

# * Remote Backup
# consider using append-only? (probably not)
# use stronger compression
# sshfs or usre@host:path
# have to use rclone for most cloud services (or a vps)
# rsync.net has a borg deal but is comparatively expensive

# * External Drives/Smartctl
# may not use since the drive died
# https://www.backblaze.com/blog/hard-drive-smart-stats/
tempfixseagateuas() {
	echo "0x0bc2:0x3343:u" | sudo tee /sys/module/usb_storage/parameters/quirks
}

# * Restoration
# (when) will it overwrite files?
