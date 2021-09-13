mountefi() {
	if [[ ! -d /boot/efi/EFI ]]; then
		sudo mount /dev/disk/by-label/SYSTEM /boot/efi
	else
		echo "EFI partition already mounted."
	fi
}

mountesp() {
	mountefi
}


umountefi() {
	sudo umount /dev/disk/by-label/SYSTEM
}

umountesp() {
	umountefi
}
