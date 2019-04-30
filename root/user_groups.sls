noctuid:
  user.present:
    # - shell: /bin/zsh
    - home: /home/noctuid
    - groups:
        - wheel
        - uucp
        # wait for these to be created
        # - transmission
        - games
        # lockfile access
        - lock
        # pre-systemd
        # breaks fast switching user and allows applications to block software
        # mixing
        # - audio
        # - storage
        # - video
