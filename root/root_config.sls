/etc:
  file.recurse:
    - source: salt://etc

/usr:
  file.recurse:
    - source: salt://usr

/boot:
  file.recurse:
    - source: salt://boot
