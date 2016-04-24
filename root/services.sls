# for enabling systemd stuff
netctl:
  service.running:
    - enable: False

NetworkManager:
  service.running:
    - enable: True
    # - watch:
    #     - file: /etc/NetworkManager/NetworkManager.conf 

# connman:
#   service.running:
#     - enable: True

ntpd:
  service.running:
    - enable: True

# note that network manager should be masked if installed and not in use
tlp:
  service.running:
    - enable: True
tlp-sleep:
  service.running:
    - enable: True

fcron:
  service.running:
    - enable: True

ufw:
  # doesn't work; fix; may have to do with ufw update
  # cmd.run:
  #   - name: |
  #       ufw default deny incoming
  #       ufw default deny outgoing
  #       ufw allow from 192.168.0.0/24
  #       ufw enable
  service.running:
    - enable: True

slimlock@noctuid:
  service.running:
    - enable: True

# printing
org.cups.cupsd:
  service.running:
    - enable: True

preload:
  service.running:
    - enable: True

# using with hostsblock
kwakd:
  service.running:
    - enable: True
