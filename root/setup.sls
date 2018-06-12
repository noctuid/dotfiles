# for additional package setup

# meant to be changed before running
{% set user = %}
# not available
# xdg
# not used
# display
# dbus

user services setup:
  cmd.script:
    - name: extra-setup user_services_setup
    - source: salt://extra-setup
    # TODO was this ever needed for anything?
    # - env:
    #     - XDG_RUNTIME_DIR: xdg
    - runas: {{ user }}

refind setup:
  cmd.script:
    - name: extra-setup refind_setup
    - source: salt://extra-setup

initramfs setup:
  cmd.script:
    - name: extra-setup initramfs_setup
    - source: salt://extra-setup

# TODO does this last after restart?
set vt colors:
  cmd.script:
    - name: extra-setup vt_colors_setup
    - env:
        - HOME: "/home/{{ user }}"
    - source: salt://extra-setup

freetype setup:
  cmd.script:
    - name: extra-setup freetype_setup
    - source: salt://extra-setup


lambda txt setup:
  cmd.script:
    - name: extra-setup lambda_txt_setup
    - env:
        - HOME: "/home/{{ user }}"
    - source: salt://extra-setup

fcron setup:
  cmd.script:
    - name: extra-setup fcron_setup
    - env:
        - HOME: "/home/{{ user }}"
    - source: salt://extra-setup

fcron user setup:
  cmd.script:
    - name: extra-setup fcron_user_setup
    - source: salt://extra-setup
    - runas: {{ user }}

ufw setup:
  cmd.script:
    - name: extra-setup ufw_setup
    - source: salt://extra-setup

bumblebee setup:
  cmd.script:
    - name: extra-setup bumblebee_setup
    - env:
        - USER: "{{ user }}"
    - source: salt://extra-setup

transmission setup:
  cmd.script:
    - name: extra-setup transmission_setup
    - env:
        - USER: "{{ user }}"
    - source: salt://extra-setup

dns setup:
  cmd.script:
    - name: extra-setup dns_setup
    - source: salt://extra-setup

ranger devicons setup:
  cmd.script:
    - name: extra-setup ranger_devicons_setup
    - env:
        - HOME: "/home/{{ user }}"
    - source: salt://extra-setup

service setup:
  cmd.script:
    - name: extra-setup service_setup
    - env:
        - USER: "{{ user }}"
    - source: salt://extra-setup
