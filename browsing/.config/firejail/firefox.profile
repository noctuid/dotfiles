include /etc/firejail/firefox.profile
# already whitelisted
# whitelist ~/.pentadactylrc
# whitelist ~/.pentadactyl
# necessary because these are symlinks it seems
whitelist ~/.pentadactyl/groups.penta
whitelist ~/.pentadactyl/.privatepentadactylrc
whitelist ~/.pentadactyl/colors/custom.penta
whitelist ~/.pentadactyl/plugins
