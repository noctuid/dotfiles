# services have enabled (I don't actually run this)

# internet
systemctl enable netctl-auto@wlp3s0.service
systemctl enable netctl-ifplugd@enp2s0.service

# after properly configuring:
systemctl enable ufw.service

# automatically start bitlbee daemon on startup
systemctl enable bitlbee

# for slimlock on sleep (see root/etc/systemd)
systemctl enable slimlock.service

# for printing
systemctl enable cups.service

# preloading for commonly used appliations
systemctl enable preload.service

# who doesn't love cron?
systemctl enable cronie.service
