# for additional package setup

# meant to be changed before running
{% set user = %}
{% set xdg = %}
{% set display = %}
{% set dbus = %}

user services setup:
  cmd.script:
    - name: extra-setup user_services_setup
    - source: salt://extra-setup
    - env:
        - XDG_RUNTIME_DIR: {{ xdg }}
    - runas: {{ user }}

# TODO does this last after restart?
set vt colors:
  cmd.script:
    - name: extra-setup vt_colors_setup
    - source: salt://extra-setup
    - runas: {{ user }}

freetype setup:
  cmd.script:
    - name: extra-setup freetype_setup
    - source: salt://extra-setup


lambda txt setup:
  cmd.script:
    - name: extra-setup lambda_txt_setup
    - source: salt://extra-setup
    - runas: {{ user }}

ibus setup:
  cmd.script:
    - name: extra-setup ibus_setup
    - source: salt://extra-setup
    - env:
        - DISPLAY: {{ display }}
        - DBUS_SESSION_BUS_ADDRESS: {{ dbus }}
    - runas: {{ user }}

fcron setup:
  cmd.script:
    - name: extra-setup fcron_setup
    - source: salt://extra-setup
    - runas: {{ user }}

ufw setup:
  cmd.script:
    - name: extra-setup ufw_setup
    - source: salt://extra-setup

bumblebee setup:
  cmd.script:
    - name: extra-setup bumblebee_setup
    - source: salt://extra-setup
    - runas: {{ user }}

transmission setup:
  cmd.script:
    - name: extra-setup transmission_setup
    - source: salt://extra-setup
    - runas: {{ user }}
