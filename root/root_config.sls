/etc/crypttab:
  file.managed:
    - source: salt://etc/crypttab
    - user: root
    - group: root
    - mode: 644

/etc/fstab:
  file.managed:
    - source: salt://etc/fstab
    - user: root
    - group: root
    - mode: 644

# TODO
# /etc/resolv.conf:

# /etc/sudoers:
#     file.append:
#         - name: /etc/sudoers
#         - text:
#           - "%truecrypt ALL=(root) NOPASSWD:/usr/bin/truecrypt"

/etc/pacman.conf:
  file.managed:
    - source: salt://etc/pacman.conf
    - user: root
    - group: root
    - mode: 644

/etc/connman/main.conf:
  file.managed:
    - source: salt://etc/connman/main.conf
    - user: root
    - group: root
    - mode: 644

/etc/NetworkManager/NetworkManager.conf:
  file.managed:
    - source: salt://etc/NetworkManager/NetworkManager.conf
    - user: root
    - group: root
    - mode: 644

/etc/default/grub:
  file.managed:
    - source: salt://etc/default/grub
    - user: root
    - group: root
    - mode: 644

# /etc/modules-load.d/overlay.conf:
#   file.managed:
#     - source: salt://etc/modules-load.d/overlay.conf
#     - user: root
#     - group: root
#     - mode: 644

# locate ignored directories
/etc/updatedb.conf:
  file.managed:
    - source: salt://etc/updatedb.conf
    - user: root
    - group: root
    - mode: 644

# slimlock service
/etc/systemd/system/slimlock@.service:
  file.managed:
    - source: salt://etc/systemd/system/slimlock@.service
    - user: root
    - group: root
    - mode: 644

# DNS settings
# /etc/resolv.conf:
#   file.managed:
#     - source: salt://etc/pacman.conf
#     - user: root
#     - group: root
#     - mode: 644
# /etc/unbound/unbound.conf:
#   file.managed:
#     - source: salt://etc/unbound/unbound.conf
#     - user: root
#     - group: root
#     - mode: 644
# /etc/systemd/system/dnscrypt-proxy.service.d/override.conf:
#   file.managed:
#     - source: salt://etc/systemd/system/dnscrypt-proxy.service.d/override.conf
#     - user: root
#     - group: root
#     - mode: 644
# /etc/systemd/system/dnscrypt-proxy.socket.d/override.conf:
#   file.managed:
#     - source: salt://etc/systemd/system/dnscrypt-proxy.socket.d/override.conf
#     - user: root
#     - group: root
#     - mode: 644
# /etc/systemd/system/connman.service.d/override.conf:
#   file.managed:
#     - source: salt://etc/systemd/system/connman.service.d/override.conf
#     - user: root
#     - group: root
#     - mode: 644
