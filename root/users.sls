# https://wiki.archlinux.org/index.php/DNSCrypt#dnscrypt_runs_with_root_privileges
dnscrypt-user:
  user.present:
    - name: dnscrypt
    - system: True
    - home: /var/dnscrypt
    - createhome: True
    - shell: /sbin/nologin

# don't need to manually create the unbound user
