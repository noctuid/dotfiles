# TODO support for --user services

# * Internet
netctl:
  service.dead:
    - enable: False

NetworkManager:
  service.dead:
    - enable: False
  cmd.run:
    # TODO override tlp service instead
    # prevents tlp from starting networkmanager
    - name: systemctl mask NetworkManager

connman:
  service.running:
    - enable: True
    # - watch:
    #     - file: /etc/connman/main.conf

# using with hostsblock
kwakd:
  service.running:
    - enable: True

# * Time Syncing
ntpd:
  service.running:
    - enable: True

tzupdate:
  service.running:
    - enable: True

# * Power Mangement
tlp:
  service.running:
    - enable: True
  # TODO this only needed for tlp-rdw right?
  # service.dead:
  #   - name: systemd-rfkill
  #   - enable: False

tlp-sleep:
  service.enabled: []

# * Bumblebee
bumblebeed:
  service.running:
    - enable: True

# * Cron
fcron:
  service.running:
    - enable: True

# * Firewall
ufw:
  service.running:
    - enable: True

slimlock@noctuid:
  service.enabled: []

resume@noctuid:
  service.enabled: []

# * Printing
org.cups.cupsd:
  service.running:
    - enable: True

# * Loading into RAM
preload:
  service.running:
    - enable: True

# psd:
#   service.running:
#     - enable: True

# * Modules
systemd-modules-load:
  service.running:
    - enable: True

# * DNS
# both of these have DNSSEC validation and no logs
# none of the anycast servers have DNSSEC validation (impossible?)
dnscrypt-proxy@ventricle.us:
  service.running:
    - enable: True

dnscrypt-proxy@dnscrypt.org-fr:
  service.running:
    - enable: True

unbound:
  service.running:
    - enable: True
