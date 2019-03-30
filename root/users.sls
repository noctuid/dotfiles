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

# https://wiki.archlinux.org/index.php/DNSCrypt#dnscrypt_runs_with_root_privileges
dnscrypt-user:
  user.present:
    - name: dnscrypt
    - system: True
    - home: /var/dnscrypt
    - createhome: True
    - shell: /sbin/nologin

# don't need to manually create the unbound user
