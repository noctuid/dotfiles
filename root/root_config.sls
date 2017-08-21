# TODOS:
# - refind.conf already in place, but for updates:
#   mount efi to replace refind.conf

/etc:
  file.recurse:
    - source: salt://etc

/usr:
  file.recurse:
    - source: salt://usr

# /etc/sudoers:
#     file.append:
#         - name: /etc/sudoers
#         - text:
#           - "veracrypt ALL=(root) NOPASSWD:/usr/bin/veracrypt"
