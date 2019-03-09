pkgbuild-introspection-pkg:
  cmd.run:
    - name: aura -A --noconfirm pkgbuild-introspection
    - unless: pacman -Q pkgbuild-introspection

reflector-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm reflector
    - unless: pacman -Q reflector

pkgfile-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm pkgfile
    - unless: pacman -Q pkgfile

pacman-contrib-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm pacman-contrib
    - unless: pacman -Q pacman-contrib

yay-pkg:
  cmd.run:
    - name: aura -A --noconfirm yay
    - unless: pacman -Q yay

pacnanny-pkg:
  cmd.run:
    - name: aura -A --noconfirm pacnanny
    - unless: pacman -Q pacnanny

refind-efi-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm refind-efi
    - unless: pacman -Q refind-efi

connman-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm connman
    - unless: pacman -Q connman

networkmanager-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm networkmanager
    - unless: pacman -Q networkmanager

network-manager-applet-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm network-manager-applet
    - unless: pacman -Q network-manager-applet

nm-connection-editor-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm nm-connection-editor
    - unless: pacman -Q nm-connection-editor

linux-headers-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm linux-headers
    - unless: pacman -Q linux-headers

linux-ck-skylake-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm linux-ck-skylake
    - unless: pacman -Q linux-ck-skylake

linux-ck-skylake-headers-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm linux-ck-skylake-headers
    - unless: pacman -Q linux-ck-skylake-headers

broadcom-wl-ck-skylake-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm broadcom-wl-ck-skylake
    - unless: pacman -Q broadcom-wl-ck-skylake

bspwm-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm bspwm
    - unless: pacman -Q bspwm

ntfs-3g-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ntfs-3g
    - unless: pacman -Q ntfs-3g

dosfstools-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm dosfstools
    - unless: pacman -Q dosfstools

exfat-utils-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm exfat-utils
    - unless: pacman -Q exfat-utils

xf86-input-synaptics-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xf86-input-synaptics
    - unless: pacman -Q xf86-input-synaptics

fcitx-im-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm fcitx-im
    - unless: pacman -Q fcitx-im

fcitx-mozc-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm fcitx-mozc
    - unless: pacman -Q fcitx-mozc

fcitx-table-other-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm fcitx-table-other
    - unless: pacman -Q fcitx-table-other

steam-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm steam
    - unless: pacman -Q steam

lib32-curl-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm lib32-curl
    - unless: pacman -Q lib32-curl

mesa-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm mesa
    - unless: pacman -Q mesa

lib32-mesa-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm lib32-mesa
    - unless: pacman -Q lib32-mesa

steam-native-runtime-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm steam-native-runtime
    - unless: pacman -Q steam-native-runtime

intel-ucode-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm intel-ucode
    - unless: pacman -Q intel-ucode

bumblebee-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm bumblebee
    - unless: pacman -Q bumblebee

bbswitch-dkms-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm bbswitch-dkms
    - unless: pacman -Q bbswitch-dkms

primus-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm primus
    - unless: pacman -Q primus

xf86-video-intel-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xf86-video-intel
    - unless: pacman -Q xf86-video-intel

nvidia-dkms-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm nvidia-dkms
    - unless: pacman -Q nvidia-dkms

nvidia-utils-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm nvidia-utils
    - unless: pacman -Q nvidia-utils

nvidia-ck-skylake-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm nvidia-ck-skylake
    - unless: pacman -Q nvidia-ck-skylake

wireguard-tools-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm wireguard-tools
    - unless: pacman -Q wireguard-tools

wireguard-dkms-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm wireguard-dkms
    - unless: pacman -Q wireguard-dkms

secure-delete-pkg:
  cmd.run:
    - name: aura -A --noconfirm secure-delete
    - unless: pacman -Q secure-delete

safe-rm-pkg:
  cmd.run:
    - name: aura -A --noconfirm safe-rm
    - unless: pacman -Q safe-rm

ufw-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ufw
    - unless: pacman -Q ufw

pass-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm pass
    - unless: pacman -Q pass

passff-host-pkg:
  cmd.run:
    - name: aura -A --noconfirm passff-host
    - unless: pacman -Q passff-host

firejail-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm firejail
    - unless: pacman -Q firejail

git-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm git
    - unless: pacman -Q git

openssh-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm openssh
    - unless: pacman -Q openssh

acpi-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm acpi
    - unless: pacman -Q acpi

tlp-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm tlp
    - unless: pacman -Q tlp

acpi_call-dkms-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm acpi_call-dkms
    - unless: pacman -Q acpi_call-dkms

powertop-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm powertop
    - unless: pacman -Q powertop

preload-pkg:
  cmd.run:
    - name: aura -A --noconfirm preload
    - unless: pacman -Q preload

cups-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm cups
    - unless: pacman -Q cups

libcups-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm libcups
    - unless: pacman -Q libcups

splix-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm splix
    - unless: pacman -Q splix

kitty-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm kitty
    - unless: pacman -Q kitty

xterm-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xterm
    - unless: pacman -Q xterm

rxvt-unicode-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm rxvt-unicode
    - unless: pacman -Q rxvt-unicode

urxvt-perls-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm urxvt-perls
    - unless: pacman -Q urxvt-perls

zsh-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm zsh
    - unless: pacman -Q zsh

tmux-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm tmux
    - unless: pacman -Q tmux

ranger-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ranger
    - unless: pacman -Q ranger

ffmpegthumbnailer-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ffmpegthumbnailer
    - unless: pacman -Q ffmpegthumbnailer

mediainfo-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm mediainfo
    - unless: pacman -Q mediainfo

python-pillow-simd-pkg:
  cmd.run:
    - name: aura -A --noconfirm python-pillow-simd
    - unless: pacman -Q python-pillow-simd

trash-cli-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm trash-cli
    - unless: pacman -Q trash-cli

pv-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm pv
    - unless: pacman -Q pv

progress-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm progress
    - unless: pacman -Q progress

htop-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm htop
    - unless: pacman -Q htop

fasd-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm fasd
    - unless: pacman -Q fasd

cowsay-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm cowsay
    - unless: pacman -Q cowsay

fortune-mod-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm fortune-mod
    - unless: pacman -Q fortune-mod

borg-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm borg
    - unless: pacman -Q borg

detox-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm detox
    - unless: pacman -Q detox

gvim-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm gvim
    - unless: pacman -Q gvim

neovim-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm neovim
    - unless: pacman -Q neovim

vim-dein-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm vim-dein-git
    - unless: pacman -Q vim-dein-git

vimpager-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm vimpager-git
    - unless: pacman -Q vimpager-git

emacs-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm emacs-git
    - unless: pacman -Q emacs-git

cask-pkg:
  cmd.run:
    - name: aura -A --noconfirm cask
    - unless: pacman -Q cask

emacs-25.1-pkg:
  cmd.run:
    - name: evm install emacs-25.1
    - unless: evm list | grep "emacs-25.1 \[I\]"

qutebrowser-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm qutebrowser
    - unless: pacman -Q qutebrowser

qt5-webengine-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm qt5-webengine
    - unless: pacman -Q qt5-webengine

firefox-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm firefox
    - unless: pacman -Q firefox

firefox-esr-bin-pkg:
  cmd.run:
    - name: aura -A --noconfirm firefox-esr-bin
    - unless: pacman -Q firefox-esr-bin

profile-cleaner-pkg:
  cmd.run:
    - name: aura -A --noconfirm profile-cleaner
    - unless: pacman -Q profile-cleaner

profile-sync-daemon-pkg:
  cmd.run:
    - name: aura -A --noconfirm profile-sync-daemon
    - unless: pacman -Q profile-sync-daemon

chromium-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm chromium
    - unless: pacman -Q chromium

w3m-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm w3m
    - unless: pacman -Q w3m

mpv-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm mpv
    - unless: pacman -Q mpv

pqiv-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm pqiv-git
    - unless: pacman -Q pqiv-git

gallery-dl-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm gallery-dl
    - unless: pacman -Q gallery-dl

gpick-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm gpick
    - unless: pacman -Q gpick

wine-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm wine
    - unless: pacman -Q wine

winetricks-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm winetricks
    - unless: pacman -Q winetricks

libreoffice-fresh-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm libreoffice-fresh
    - unless: pacman -Q libreoffice-fresh

unoconv-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm unoconv
    - unless: pacman -Q unoconv

pandoc-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm pandoc
    - unless: pacman -Q pandoc

wkhtmltopdf-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm wkhtmltopdf
    - unless: pacman -Q wkhtmltopdf

poppler-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm poppler
    - unless: pacman -Q poppler

odt2txt-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm odt2txt
    - unless: pacman -Q odt2txt

livedown-pkg:
  cmd.run:
    - name: npm install -g livedown
    - unless: command -v livedown
    - require:
        - npm-pkg

vmd-pkg:
  cmd.run:
    - name: npm install -g vmd
    - unless: command -v vmd
    - require:
        - npm-pkg

gimp-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm gimp
    - unless: pacman -Q gimp

imagemagick-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm imagemagick
    - unless: pacman -Q imagemagick

gifsicle-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm gifsicle
    - unless: pacman -Q gifsicle

graphicsmagick-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm graphicsmagick
    - unless: pacman -Q graphicsmagick

transmission-cli-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm transmission-cli
    - unless: pacman -Q transmission-cli

peerflix-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm peerflix-git
    - unless: pacman -Q peerflix-git

zathura-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm zathura
    - unless: pacman -Q zathura

zathura-pdf-mupdf-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm zathura-pdf-mupdf
    - unless: pacman -Q zathura-pdf-mupdf

zathura-djvu-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm zathura-djvu
    - unless: pacman -Q zathura-djvu

ppsspp-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ppsspp
    - unless: pacman -Q ppsspp

dolphin-emu-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm dolphin-emu
    - unless: pacman -Q dolphin-emu

mlocate-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm mlocate
    - unless: pacman -Q mlocate

lsof-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm lsof
    - unless: pacman -Q lsof

lshw-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm lshw
    - unless: pacman -Q lshw

dmidecode-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm dmidecode
    - unless: pacman -Q dmidecode

sharutils-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm sharutils
    - unless: pacman -Q sharutils

ntp-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ntp
    - unless: pacman -Q ntp

chrony-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm chrony
    - unless: pacman -Q chrony

tzupdate-pkg:
  cmd.run:
    - name: aura -A --noconfirm tzupdate
    - unless: pacman -Q tzupdate

udiskie-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm udiskie
    - unless: pacman -Q udiskie

smartmontools-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm smartmontools
    - unless: pacman -Q smartmontools

hdparm-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm hdparm
    - unless: pacman -Q hdparm

rng-tools-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm rng-tools
    - unless: pacman -Q rng-tools

xcape-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xcape
    - unless: pacman -Q xcape

klfc-pkg:
  cmd.run:
    - name: aura -A --noconfirm klfc
    - unless: pacman -Q klfc

arduino-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm arduino
    - unless: pacman -Q arduino

fcron-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm fcron
    - unless: pacman -Q fcron

baobab-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm baobab
    - unless: pacman -Q baobab

ncdu-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ncdu
    - unless: pacman -Q ncdu

cdu-pkg:
  cmd.run:
    - name: aura -A --noconfirm cdu
    - unless: pacman -Q cdu

virtualbox-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm virtualbox
    - unless: pacman -Q virtualbox

virtualbox-host-dkms-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm virtualbox-host-dkms
    - unless: pacman -Q virtualbox-host-dkms

hunspell-en_US-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm hunspell-en_US
    - unless: pacman -Q hunspell-en_US

aspell-en-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm aspell-en
    - unless: pacman -Q aspell-en

enchant-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm enchant
    - unless: pacman -Q enchant

sdcv-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm sdcv
    - unless: pacman -Q sdcv

words-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm words
    - unless: pacman -Q words

languagetool-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm languagetool
    - unless: pacman -Q languagetool

github.com/errata-ai/vale-pkg:
  cmd.run:
    - runas: noctuid
    - name: go get github.com/errata-ai/vale
    - unless: go list '...' | grep github.com/errata-ai/vale
    - require:
        - go-pkg

isync-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm isync
    - unless: pacman -Q isync

msmtp-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm msmtp
    - unless: pacman -Q msmtp

msmtp-mta-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm msmtp-mta
    - unless: pacman -Q msmtp-mta

mu-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm mu-git
    - unless: pacman -Q mu-git

nspluginwrapper-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm nspluginwrapper
    - unless: pacman -Q nspluginwrapper

procmail-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm procmail
    - unless: pacman -Q procmail

alsa-utils-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm alsa-utils
    - unless: pacman -Q alsa-utils

pulseaudio-alsa-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm pulseaudio-alsa
    - unless: pacman -Q pulseaudio-alsa

pavucontrol-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm pavucontrol
    - unless: pacman -Q pavucontrol

ponymix-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ponymix
    - unless: pacman -Q ponymix

mpd-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm mpd
    - unless: pacman -Q mpd

mpc-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm mpc
    - unless: pacman -Q mpc

ncmpcpp-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ncmpcpp
    - unless: pacman -Q ncmpcpp

abcde-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm abcde
    - unless: pacman -Q abcde

glyr-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm glyr
    - unless: pacman -Q glyr

perl-musicbrainz-discid-pkg:
  cmd.run:
    - name: aura -A --noconfirm perl-musicbrainz-discid
    - unless: pacman -Q perl-musicbrainz-discid

perl-webservice-musicbrainz-pkg:
  cmd.run:
    - name: aura -A --noconfirm perl-webservice-musicbrainz
    - unless: pacman -Q perl-webservice-musicbrainz

cdrtools-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm cdrtools
    - unless: pacman -Q cdrtools

lib32-alsa-plugins-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm lib32-alsa-plugins
    - unless: pacman -Q lib32-alsa-plugins

beets-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm beets
    - unless: pacman -Q beets

pyacoustid-pkg:
  cmd.run:
    - name: pip install pyacoustid
    - unless: pip list | grep ^pyacoustid
    - require:
        - python-pip-pkg

requests-pkg:
  cmd.run:
    - name: pip install requests
    - unless: pip list | grep ^requests
    - require:
        - python-pip-pkg

python-mpd-pkg:
  cmd.run:
    - name: pip install python-mpd
    - unless: pip list | grep ^python-mpd
    - require:
        - python-pip-pkg

pylast-pkg:
  cmd.run:
    - name: pip install pylast
    - unless: pip list | grep ^pylast
    - require:
        - python-pip-pkg

youtube-dl-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm youtube-dl
    - unless: pacman -Q youtube-dl

aria2-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm aria2
    - unless: pacman -Q aria2

texlive-bin-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm texlive-bin
    - unless: pacman -Q texlive-bin

texlive-latexextra-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm texlive-latexextra
    - unless: pacman -Q texlive-latexextra

python-pygments-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-pygments
    - unless: pacman -Q python-pygments

sxhkd-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm sxhkd
    - unless: pacman -Q sxhkd

xautolock-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xautolock
    - unless: pacman -Q xautolock

redshift-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm redshift
    - unless: pacman -Q redshift

compton-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm compton
    - unless: pacman -Q compton

libnotify-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm libnotify
    - unless: pacman -Q libnotify

dunst-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm dunst-git
    - unless: pacman -Q dunst-git

rofi-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm rofi
    - unless: pacman -Q rofi

fonts-meta-extended-lt-pkg:
  cmd.run:
    - name: aura -A --noconfirm fonts-meta-extended-lt
    - unless: pacman -Q fonts-meta-extended-lt

ttf-dejavu-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ttf-dejavu
    - unless: pacman -Q ttf-dejavu

otf-fira-sans-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm otf-fira-sans
    - unless: pacman -Q otf-fira-sans

otf-fira-mono-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm otf-fira-mono
    - unless: pacman -Q otf-fira-mono

ttf-inconsolata-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ttf-inconsolata
    - unless: pacman -Q ttf-inconsolata

ttf-inconsolata-g-pkg:
  cmd.run:
    - name: aura -A --noconfirm ttf-inconsolata-g
    - unless: pacman -Q ttf-inconsolata-g

office-code-pro-pkg:
  cmd.run:
    - name: aura -A --noconfirm office-code-pro
    - unless: pacman -Q office-code-pro

phallus-fonts-git-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm phallus-fonts-git
    - unless: pacman -Q phallus-fonts-git

otf-fura-mono-powerline-git-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm otf-fura-mono-powerline-git
    - unless: pacman -Q otf-fura-mono-powerline-git

noto-fonts-cjk-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm noto-fonts-cjk
    - unless: pacman -Q noto-fonts-cjk

otf-ipafont-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm otf-ipafont
    - unless: pacman -Q otf-ipafont

adobe-source-han-sans-jp-fonts-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm adobe-source-han-sans-jp-fonts
    - unless: pacman -Q adobe-source-han-sans-jp-fonts

ttf-symbola-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ttf-symbola
    - unless: pacman -Q ttf-symbola

noto-fonts-emoji-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm noto-fonts-emoji
    - unless: pacman -Q noto-fonts-emoji

siji-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm siji-git
    - unless: pacman -Q siji-git

ttfautohint-pkg:
  cmd.run:
    - name: aura -A --noconfirm ttfautohint
    - unless: pacman -Q ttfautohint

yargs-pkg:
  cmd.run:
    - name: npm install -g yargs
    - unless: command -v yargs
    - require:
        - npm-pkg

pad-pkg:
  cmd.run:
    - name: npm install -g pad
    - unless: command -v pad
    - require:
        - npm-pkg

patel-pkg:
  cmd.run:
    - name: npm install -g patel
    - unless: command -v patel
    - require:
        - npm-pkg

cubic2quad-pkg:
  cmd.run:
    - name: npm install -g cubic2quad
    - unless: command -v cubic2quad
    - require:
        - npm-pkg

bezier-js-pkg:
  cmd.run:
    - name: npm install -g bezier-js
    - unless: command -v bezier-js
    - require:
        - npm-pkg

libspiro-js-pkg:
  cmd.run:
    - name: npm install -g libspiro-js
    - unless: command -v libspiro-js
    - require:
        - npm-pkg

topsort-pkg:
  cmd.run:
    - name: npm install -g topsort
    - unless: command -v topsort
    - require:
        - npm-pkg

toml-pkg:
  cmd.run:
    - name: npm install -g toml
    - unless: command -v toml
    - require:
        - npm-pkg

caryll-shapeops-pkg:
  cmd.run:
    - name: npm install -g caryll-shapeops
    - unless: command -v caryll-shapeops
    - require:
        - npm-pkg

otfcc-c2q-pkg:
  cmd.run:
    - name: npm install -g otfcc-c2q
    - unless: command -v otfcc-c2q
    - require:
        - npm-pkg

unorm-pkg:
  cmd.run:
    - name: npm install -g unorm
    - unless: command -v unorm
    - require:
        - npm-pkg

setroot-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm setroot-git
    - unless: pacman -Q setroot-git

xtitle-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm xtitle-git
    - unless: pacman -Q xtitle-git

numix-gtk-theme-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm numix-gtk-theme
    - unless: pacman -Q numix-gtk-theme

numix-icon-theme-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm numix-icon-theme-git
    - unless: pacman -Q numix-icon-theme-git

setcolors-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm setcolors-git
    - unless: pacman -Q setcolors-git

unbound-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm unbound
    - unless: pacman -Q unbound

expat-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm expat
    - unless: pacman -Q expat

dnscrypt-proxy-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm dnscrypt-proxy
    - unless: pacman -Q dnscrypt-proxy

hostblock-bin-pkg:
  cmd.run:
    - name: aura -A --noconfirm hostblock-bin
    - unless: pacman -Q hostblock-bin

kwakd-pkg:
  cmd.run:
    - name: aura -A --noconfirm kwakd
    - unless: pacman -Q kwakd

unrar-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm unrar
    - unless: pacman -Q unrar

unzip-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm unzip
    - unless: pacman -Q unzip

zip-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm zip
    - unless: pacman -Q zip

p7zip-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm p7zip
    - unless: pacman -Q p7zip

atool-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm atool
    - unless: pacman -Q atool

jmtpfs-pkg:
  cmd.run:
    - name: aura -A --noconfirm jmtpfs
    - unless: pacman -Q jmtpfs

r-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm r
    - unless: pacman -Q r

tk-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm tk
    - unless: pacman -Q tk

ripgrep-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ripgrep
    - unless: pacman -Q ripgrep

fzf-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm fzf
    - unless: pacman -Q fzf

fd-rs-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm fd-rs-git
    - unless: pacman -Q fd-rs-git

harvey-pkg:
  cmd.run:
    - name: pip install harvey
    - unless: pip list | grep ^harvey
    - require:
        - python-pip-pkg

clojure-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm clojure
    - unless: pacman -Q clojure

gdb-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm gdb
    - unless: pacman -Q gdb

go-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm go
    - unless: pacman -Q go

nodejs-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm nodejs
    - unless: pacman -Q nodejs

npm-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm npm
    - unless: pacman -Q npm

python-pip-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-pip
    - unless: pacman -Q python-pip

python-jedi-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-jedi
    - unless: pacman -Q python-jedi

python-rope-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-rope
    - unless: pacman -Q python-rope

ipython-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ipython
    - unless: pacman -Q ipython

ptpython-pkg:
  cmd.run:
    - name: aura -A --noconfirm ptpython
    - unless: pacman -Q ptpython

python-ipdb-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-ipdb
    - unless: pacman -Q python-ipdb

python-virtualenv-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-virtualenv
    - unless: pacman -Q python-virtualenv

python-virtualenvwrapper-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-virtualenvwrapper
    - unless: pacman -Q python-virtualenvwrapper

python-pytest-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-pytest
    - unless: pacman -Q python-pytest

python-epc-pkg:
  cmd.run:
    - name: aura -A --noconfirm python-epc
    - unless: pacman -Q python-epc

python-importmagic-pkg:
  cmd.run:
    - name: aura -A --noconfirm python-importmagic
    - unless: pacman -Q python-importmagic

python-language-server-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-language-server
    - unless: pacman -Q python-language-server

python-pyflakes-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-pyflakes
    - unless: pacman -Q python-pyflakes

python-pycodestyle-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-pycodestyle
    - unless: pacman -Q python-pycodestyle

python-mccabe-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-mccabe
    - unless: pacman -Q python-mccabe

python-pydocstyle-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-pydocstyle
    - unless: pacman -Q python-pydocstyle

flake8-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm flake8
    - unless: pacman -Q flake8

python-pylint-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-pylint
    - unless: pacman -Q python-pylint

python-isort-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-isort
    - unless: pacman -Q python-isort

python-pyls-isort-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm python-pyls-isort-git
    - unless: pacman -Q python-pyls-isort-git

python-autoflake-pkg:
  cmd.run:
    - name: aura -A --noconfirm python-autoflake
    - unless: pacman -Q python-autoflake

python-black-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm python-black
    - unless: pacman -Q python-black

python-language-server-black-pkg:
  cmd.run:
    - name: aura -A --noconfirm python-language-server-black
    - unless: pacman -Q python-language-server-black

python-pre-commit-pkg:
  cmd.run:
    - name: aura -A --noconfirm python-pre-commit
    - unless: pacman -Q python-pre-commit

ruby-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm ruby
    - unless: pacman -Q ruby

roswell-pkg:
  cmd.run:
    - name: aura -A --noconfirm roswell
    - unless: pacman -Q roswell

sbcl-bin-pkg:
  cmd.run:
    - name: ros install sbcl-bin
    - unless: ros list installed | grep sbcl-bin
    - require:
        - roswell-pkg

racket-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm racket
    - unless: pacman -Q racket

shellcheck-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm shellcheck
    - unless: pacman -Q shellcheck

jq-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm jq
    - unless: pacman -Q jq

maim-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm maim
    - unless: pacman -Q maim

slop-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm slop
    - unless: pacman -Q slop

xdotool-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xdotool
    - unless: pacman -Q xdotool

xorg-server-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xorg-server
    - unless: pacman -Q xorg-server

xorg-xinit-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xorg-xinit
    - unless: pacman -Q xorg-xinit

xorg-xinput-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xorg-xinput
    - unless: pacman -Q xorg-xinput

xorg-xprop-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xorg-xprop
    - unless: pacman -Q xorg-xprop

xorg-xwininfo-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xorg-xwininfo
    - unless: pacman -Q xorg-xwininfo

xorg-xfontsel-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xorg-xfontsel
    - unless: pacman -Q xorg-xfontsel

xorg-xlsfonts-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xorg-xlsfonts
    - unless: pacman -Q xorg-xlsfonts

xorg-xsetroot-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xorg-xsetroot
    - unless: pacman -Q xorg-xsetroot

xorg-xev-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xorg-xev
    - unless: pacman -Q xorg-xev

xorg-xbacklight-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xorg-xbacklight
    - unless: pacman -Q xorg-xbacklight

xsel-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xsel
    - unless: pacman -Q xsel

xclip-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm xclip
    - unless: pacman -Q xclip

android-tools-pkg:
  cmd.run:
    - name: powerpill -S --noconfirm android-tools
    - unless: pacman -Q android-tools

