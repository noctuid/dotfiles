noctuid:
  user.present:
    # - shell: /bin/zsh
    # I prefer to keep bash as my login shell
    # (nice on the off chance that I break my zsh config)
    - shell: /bin/bash
    - home: /home/noctuid
    - uid: 1000
    - gid: users
    - groups:
        - wheel
        - games
        - video
        - audio
        - storage
        - power
        - fuse
