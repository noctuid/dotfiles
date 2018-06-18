/etc:
  file.recurse:
    - source: salt://etc

/usr:
  file.recurse:
    - source: salt://usr

/boot:
  file.recurse:
    - source: salt://boot

/var/lib/transmission/.config/transmission-daemon/settings.json:
  file.managed:
    - source: salt://var/lib/transmission/.config/transmission-daemon/settings.json
    - user: transmission
    - group: transmission
