pkgbuild-introspection-pkg:
  pkg.installed:
    - pkgs:
        - pkgbuild-introspection

reflector-pkg:
  pkg.installed:
    - pkgs:
        - reflector

pkgfile-pkg:
  pkg.installed:
    - pkgs:
        - pkgfile

connman-pkg:
  pkg.installed:
    - pkgs:
        - connman

networkmanager-pkg:
  pkg.installed:
    - pkgs:
        - networkmanager

network-manager-applet-pkg:
  pkg.installed:
    - pkgs:
        - network-manager-applet

nm-connection-editor-pkg:
  pkg.installed:
    - pkgs:
        - nm-connection-editor

linux-headers-pkg:
  pkg.installed:
    - pkgs:
        - linux-headers

linux-ck-ivybridge-pkg:
  pkg.installed:
    - pkgs:
        - linux-ck-ivybridge

linux-ck-ivybridge-headers-pkg:
  pkg.installed:
    - pkgs:
        - linux-ck-ivybridge-headers

broadcom-wl-ck-ivybridge-pkg:
  pkg.installed:
    - pkgs:
        - broadcom-wl-ck-ivybridge

ntfs-3g-pkg:
  pkg.installed:
    - pkgs:
        - ntfs-3g

dosfstools-pkg:
  pkg.installed:
    - pkgs:
        - dosfstools

exfat-utils-pkg:
  pkg.installed:
    - pkgs:
        - exfat-utils

xf86-input-synaptics-pkg:
  pkg.installed:
    - pkgs:
        - xf86-input-synaptics

fcitx-im-pkg:
  pkg.installed:
    - pkgs:
        - fcitx-im

fcitx-mozc-pkg:
  pkg.installed:
    - pkgs:
        - fcitx-mozc

steam-pkg:
  pkg.installed:
    - pkgs:
        - steam

lib32-curl-pkg:
  pkg.installed:
    - pkgs:
        - lib32-curl

mesa-pkg:
  pkg.installed:
    - pkgs:
        - mesa

lib32-mesa-pkg:
  pkg.installed:
    - pkgs:
        - lib32-mesa

bumblebee-pkg:
  pkg.installed:
    - pkgs:
        - bumblebee

bbswitch-dkms-pkg:
  pkg.installed:
    - pkgs:
        - bbswitch-dkms

xf86-video-intel-pkg:
  pkg.installed:
    - pkgs:
        - xf86-video-intel

nvidia-dkms-pkg:
  pkg.installed:
    - pkgs:
        - nvidia-dkms

nvidia-ck-ivybridge-pkg:
  pkg.installed:
    - pkgs:
        - nvidia-ck-ivybridge

veracrypt-pkg:
  pkg.installed:
    - pkgs:
        - veracrypt

secure-delete-pkg:
  cmd.run:
    - name: aura -A --noconfirm secure-delete
    - unless: pacman -Q secure-delete

safe-rm-pkg:
  cmd.run:
    - name: aura -A --noconfirm safe-rm
    - unless: pacman -Q safe-rm

ufw-pkg:
  pkg.installed:
    - pkgs:
        - ufw

pass-pkg:
  pkg.installed:
    - pkgs:
        - pass

git-pkg:
  pkg.installed:
    - pkgs:
        - git

acpi-pkg:
  pkg.installed:
    - pkgs:
        - acpi

tlp-pkg:
  pkg.installed:
    - pkgs:
        - tlp

acpi_call-dkms-pkg:
  cmd.run:
    - name: aura -A --noconfirm acpi_call-dkms
    - unless: pacman -Q acpi_call-dkms

powertop-pkg:
  pkg.installed:
    - pkgs:
        - powertop

cups-pkg:
  pkg.installed:
    - pkgs:
        - cups

libcups-pkg:
  pkg.installed:
    - pkgs:
        - libcups

splix-pkg:
  pkg.installed:
    - pkgs:
        - splix

termite-ranger-fix-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm termite-ranger-fix-git
    - unless: pacman -Q termite-ranger-fix-git

termite-ranger-fix-terminfo-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm termite-ranger-fix-terminfo-git
    - unless: pacman -Q termite-ranger-fix-terminfo-git

xterm-pkg:
  pkg.installed:
    - pkgs:
        - xterm

rxvt-unicode-pkg:
  pkg.installed:
    - pkgs:
        - rxvt-unicode

urxvt-perls-pkg:
  pkg.installed:
    - pkgs:
        - urxvt-perls

zsh-pkg:
  pkg.installed:
    - pkgs:
        - zsh

tmux-pkg:
  pkg.installed:
    - pkgs:
        - tmux

ranger-pkg:
  pkg.installed:
    - pkgs:
        - ranger

ffmpegthumbnailer-pkg:
  pkg.installed:
    - pkgs:
        - ffmpegthumbnailer

mediainfo-pkg:
  pkg.installed:
    - pkgs:
        - mediainfo

pv-pkg:
  pkg.installed:
    - pkgs:
        - pv

progress-pkg:
  pkg.installed:
    - pkgs:
        - progress

gvim-pkg:
  pkg.installed:
    - pkgs:
        - gvim

emacs-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm emacs-git
    - unless: pacman -Q emacs-git

evm-pkg:
  cmd.run:
    - runas: noctuid
    - name: gem install evm
    - unless: which evm
    - require:
        - ruby-pkg

emacs-25.1-pkg:
  cmd.run:
    - runas: noctuid
    - name: evm install emacs-25.1
    - unless: which emacs-25.1
    - require:
        - evm-pkg

qutebrowser-pkg:
  pkg.installed:
    - pkgs:
        - qutebrowser

firefox-pkg:
  pkg.installed:
    - pkgs:
        - firefox

firejail-pkg:
  pkg.installed:
    - pkgs:
        - firejail

profile-cleaner-pkg:
  cmd.run:
    - name: aura -A --noconfirm profile-cleaner
    - unless: pacman -Q profile-cleaner

profile-sync-daemon-pkg:
  cmd.run:
    - name: aura -A --noconfirm profile-sync-daemon
    - unless: pacman -Q profile-sync-daemon

chromium-pkg:
  pkg.installed:
    - pkgs:
        - chromium

w3m-pkg:
  pkg.installed:
    - pkgs:
        - w3m

wine-pkg:
  pkg.installed:
    - pkgs:
        - wine

winetricks-pkg:
  pkg.installed:
    - pkgs:
        - winetricks

libreoffice-fresh-pkg:
  pkg.installed:
    - pkgs:
        - libreoffice-fresh

unoconv-pkg:
  pkg.installed:
    - pkgs:
        - unoconv

pandoc-pkg:
  pkg.installed:
    - pkgs:
        - pandoc

livedown-pkg:
  cmd.run:
    - name: npm install -g livedown
    - unless: which livedown
    - require:
        - npm-pkg

gimp-pkg:
  pkg.installed:
    - pkgs:
        - gimp

imagemagick-pkg:
  pkg.installed:
    - pkgs:
        - imagemagick

gifsicle-pkg:
  pkg.installed:
    - pkgs:
        - gifsicle

graphicsmagick-pkg:
  pkg.installed:
    - pkgs:
        - graphicsmagick

transmission-cli-pkg:
  pkg.installed:
    - pkgs:
        - transmission-cli

zathura-pkg:
  pkg.installed:
    - pkgs:
        - zathura

zathura-pdf-mupdf-pkg:
  pkg.installed:
    - pkgs:
        - zathura-pdf-mupdf

zathura-djvu-pkg:
  pkg.installed:
    - pkgs:
        - zathura-djvu

apvlv-pkg:
  pkg.installed:
    - pkgs:
        - apvlv

ppsspp-pkg:
  pkg.installed:
    - pkgs:
        - ppsspp

mlocate-pkg:
  pkg.installed:
    - pkgs:
        - mlocate

lsof-pkg:
  pkg.installed:
    - pkgs:
        - lsof

lshw-pkg:
  pkg.installed:
    - pkgs:
        - lshw

dmidecode-pkg:
  pkg.installed:
    - pkgs:
        - dmidecode

ntp-pkg:
  pkg.installed:
    - pkgs:
        - ntp

tzupdate-pkg:
  cmd.run:
    - name: aura -A --noconfirm tzupdate
    - unless: pacman -Q tzupdate

udevil-pkg:
  pkg.installed:
    - pkgs:
        - udevil

smartmontools-pkg:
  pkg.installed:
    - pkgs:
        - smartmontools

sharutils-pkg:
  pkg.installed:
    - pkgs:
        - sharutils

fcron-pkg:
  pkg.installed:
    - pkgs:
        - fcron

baobab-pkg:
  pkg.installed:
    - pkgs:
        - baobab

ncdu-pkg:
  pkg.installed:
    - pkgs:
        - ncdu

cdu-pkg:
  cmd.run:
    - name: aura -A --noconfirm cdu
    - unless: pacman -Q cdu

virtualbox-pkg:
  pkg.installed:
    - pkgs:
        - virtualbox

virtualbox-host-dkms-pkg:
  pkg.installed:
    - pkgs:
        - virtualbox-host-dkms

virtualbox-ck-host-modules-ivybridge-pkg:
  pkg.installed:
    - pkgs:
        - virtualbox-ck-host-modules-ivybridge

hunspell-en-pkg:
  pkg.installed:
    - pkgs:
        - hunspell-en

aspell-en-pkg:
  pkg.installed:
    - pkgs:
        - aspell-en

sdcv-pkg:
  pkg.installed:
    - pkgs:
        - sdcv

words-pkg:
  pkg.installed:
    - pkgs:
        - words

isync-pkg:
  pkg.installed:
    - pkgs:
        - isync

msmtp-pkg:
  pkg.installed:
    - pkgs:
        - msmtp

msmtp-mta-pkg:
  pkg.installed:
    - pkgs:
        - msmtp-mta

mu-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm mu-git
    - unless: pacman -Q mu-git

nspluginwrapper-pkg:
  pkg.installed:
    - pkgs:
        - nspluginwrapper

procmail-pkg:
  pkg.installed:
    - pkgs:
        - procmail

alsa-utils-pkg:
  pkg.installed:
    - pkgs:
        - alsa-utils

pulseaudio-alsa-pkg:
  pkg.installed:
    - pkgs:
        - pulseaudio-alsa

pavucontrol-pkg:
  pkg.installed:
    - pkgs:
        - pavucontrol

ponymix-pkg:
  pkg.installed:
    - pkgs:
        - ponymix

mpd-pkg:
  pkg.installed:
    - pkgs:
        - mpd

mpc-pkg:
  pkg.installed:
    - pkgs:
        - mpc

ncmpcpp-pkg:
  pkg.installed:
    - pkgs:
        - ncmpcpp

abcde-pkg:
  pkg.installed:
    - pkgs:
        - abcde

glyr-pkg:
  pkg.installed:
    - pkgs:
        - glyr

perl-musicbrainz-discid-pkg:
  cmd.run:
    - name: aura -A --noconfirm perl-musicbrainz-discid
    - unless: pacman -Q perl-musicbrainz-discid

perl-webservice-musicbrainz-pkg:
  cmd.run:
    - name: aura -A --noconfirm perl-webservice-musicbrainz
    - unless: pacman -Q perl-webservice-musicbrainz

cdrtools-pkg:
  pkg.installed:
    - pkgs:
        - cdrtools

lib32-alsa-plugins-pkg:
  pkg.installed:
    - pkgs:
        - lib32-alsa-plugins

beets-pkg:
  pkg.installed:
    - pkgs:
        - beets

pyacoustid-pkg:
  cmd.run:
    - name: pip install pyacoustid
    - unless: which pyacoustid
    - require:
        - python-pip-pkg

requests-pkg:
  cmd.run:
    - name: pip install requests
    - unless: which requests
    - require:
        - python-pip-pkg

python-mpd-pkg:
  cmd.run:
    - name: pip install python-mpd
    - unless: which python-mpd
    - require:
        - python-pip-pkg

pylast-pkg:
  cmd.run:
    - name: pip install pylast
    - unless: which pylast
    - require:
        - python-pip-pkg

texlive-bin-pkg:
  pkg.installed:
    - pkgs:
        - texlive-bin

texlive-latexextra-pkg:
  pkg.installed:
    - pkgs:
        - texlive-latexextra

python-pygments-pkg:
  pkg.installed:
    - pkgs:
        - python-pygments

sxhkd-pkg:
  pkg.installed:
    - pkgs:
        - sxhkd

xautolock-pkg:
  pkg.installed:
    - pkgs:
        - xautolock

redshift-pkg:
  pkg.installed:
    - pkgs:
        - redshift

compton-pkg:
  pkg.installed:
    - pkgs:
        - compton

libnotify-pkg:
  pkg.installed:
    - pkgs:
        - libnotify

dunst-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm dunst-git
    - unless: pacman -Q dunst-git

fonts-meta-extended-lt-pkg:
  cmd.run:
    - name: aura -A --noconfirm fonts-meta-extended-lt
    - unless: pacman -Q fonts-meta-extended-lt

ttf-dejavu-pkg:
  pkg.installed:
    - pkgs:
        - ttf-dejavu

otf-fira-mono-pkg:
  pkg.installed:
    - pkgs:
        - otf-fira-mono

ttf-inconsolata-pkg:
  pkg.installed:
    - pkgs:
        - ttf-inconsolata

ttf-inconsolata-g-pkg:
  cmd.run:
    - name: aura -A --noconfirm ttf-inconsolata-g
    - unless: pacman -Q ttf-inconsolata-g

noto-fonts-cjk-pkg:
  pkg.installed:
    - pkgs:
        - noto-fonts-cjk

otf-ipafont-pkg:
  pkg.installed:
    - pkgs:
        - otf-ipafont

adobe-source-han-sans-jp-fonts-pkg:
  pkg.installed:
    - pkgs:
        - adobe-source-han-sans-jp-fonts

bdf-unifont-pkg:
  pkg.installed:
    - pkgs:
        - bdf-unifont

siji-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm siji-git
    - unless: pacman -Q siji-git

ttfautohint-pkg:
  cmd.run:
    - name: aura -A --noconfirm ttfautohint
    - unless: pacman -Q ttfautohint

otfcc-pkg:
  cmd.run:
    - name: aura -A --noconfirm otfcc
    - unless: pacman -Q otfcc

yargs-pkg:
  cmd.run:
    - name: npm install -g yargs
    - unless: which yargs
    - require:
        - npm-pkg

pad-pkg:
  cmd.run:
    - name: npm install -g pad
    - unless: which pad
    - require:
        - npm-pkg

patel-pkg:
  cmd.run:
    - name: npm install -g patel
    - unless: which patel
    - require:
        - npm-pkg

cubic2quad-pkg:
  cmd.run:
    - name: npm install -g cubic2quad
    - unless: which cubic2quad
    - require:
        - npm-pkg

bezier-js-pkg:
  cmd.run:
    - name: npm install -g bezier-js
    - unless: which bezier-js
    - require:
        - npm-pkg

libspiro-js-pkg:
  cmd.run:
    - name: npm install -g libspiro-js
    - unless: which libspiro-js
    - require:
        - npm-pkg

topsort-pkg:
  cmd.run:
    - name: npm install -g topsort
    - unless: which topsort
    - require:
        - npm-pkg

toml-pkg:
  cmd.run:
    - name: npm install -g toml
    - unless: which toml
    - require:
        - npm-pkg

caryll-shapeops-pkg:
  cmd.run:
    - name: npm install -g caryll-shapeops
    - unless: which caryll-shapeops
    - require:
        - npm-pkg

otfcc-c2q-pkg:
  cmd.run:
    - name: npm install -g otfcc-c2q
    - unless: which otfcc-c2q
    - require:
        - npm-pkg

unorm-pkg:
  cmd.run:
    - name: npm install -g unorm
    - unless: which unorm
    - require:
        - npm-pkg

polybar-git-pkg:
  cmd.run:
    - name: aura -A --noconfirm polybar-git
    - unless: pacman -Q polybar-git

xtitle-git-pkg:
  pkg.installed:
    - pkgs:
        - xtitle-git

conky-lua-nv-pkg:
  cmd.run:
    - name: aura -A --noconfirm conky-lua-nv
    - unless: pacman -Q conky-lua-nv

numix-gtk-theme-pkg:
  pkg.installed:
    - pkgs:
        - numix-gtk-theme

unbound-pkg:
  pkg.installed:
    - pkgs:
        - unbound

expat-pkg:
  pkg.installed:
    - pkgs:
        - expat

dnscrypt-proxy-pkg:
  pkg.installed:
    - pkgs:
        - dnscrypt-proxy

rar-pkg:
  cmd.run:
    - name: aura -A --noconfirm rar
    - unless: pacman -Q rar

unzip-pkg:
  pkg.installed:
    - pkgs:
        - unzip

zip-pkg:
  pkg.installed:
    - pkgs:
        - zip

p7zip-pkg:
  pkg.installed:
    - pkgs:
        - p7zip

atool-pkg:
  pkg.installed:
    - pkgs:
        - atool

htop-pkg:
  pkg.installed:
    - pkgs:
        - htop

fasd-pkg:
  pkg.installed:
    - pkgs:
        - fasd

jmtpfs-pkg:
  cmd.run:
    - name: aura -A --noconfirm jmtpfs
    - unless: pacman -Q jmtpfs

r-pkg:
  pkg.installed:
    - pkgs:
        - r

tk-pkg:
  pkg.installed:
    - pkgs:
        - tk

ripgrep-pkg:
  pkg.installed:
    - pkgs:
        - ripgrep

fzf-pkg:
  pkg.installed:
    - pkgs:
        - fzf

cowsay-pkg:
  pkg.installed:
    - pkgs:
        - cowsay

fortune-mod-pkg:
  pkg.installed:
    - pkgs:
        - fortune-mod

harvey-pkg:
  cmd.run:
    - name: pip install harvey
    - unless: which harvey
    - require:
        - python-pip-pkg

nodejs-pkg:
  pkg.installed:
    - pkgs:
        - nodejs

npm-pkg:
  pkg.installed:
    - pkgs:
        - npm

python-pip-pkg:
  pkg.installed:
    - pkgs:
        - python-pip

ruby-pkg:
  pkg.installed:
    - pkgs:
        - ruby

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

phantomjs-pkg:
  pkg.installed:
    - pkgs:
        - phantomjs

racket-pkg:
  pkg.installed:
    - pkgs:
        - racket

shellcheck-pkg:
  pkg.installed:
    - pkgs:
        - shellcheck

jq-pkg:
  pkg.installed:
    - pkgs:
        - jq

maim-pkg:
  pkg.installed:
    - pkgs:
        - maim

slop-pkg:
  pkg.installed:
    - pkgs:
        - slop

xdotool-pkg:
  pkg.installed:
    - pkgs:
        - xdotool

xorg-server-pkg:
  pkg.installed:
    - pkgs:
        - xorg-server

xorg-xinit-pkg:
  pkg.installed:
    - pkgs:
        - xorg-xinit

xorg-xprop-pkg:
  pkg.installed:
    - pkgs:
        - xorg-xprop

xorg-xwininfo-pkg:
  pkg.installed:
    - pkgs:
        - xorg-xwininfo

xorg-xfontsel-pkg:
  pkg.installed:
    - pkgs:
        - xorg-xfontsel

xorg-xlsfonts-pkg:
  pkg.installed:
    - pkgs:
        - xorg-xlsfonts

xorg-xsetroot-pkg:
  pkg.installed:
    - pkgs:
        - xorg-xsetroot

xorg-xev-pkg:
  pkg.installed:
    - pkgs:
        - xorg-xev

xsel-pkg:
  pkg.installed:
    - pkgs:
        - xsel

xclip-pkg:
  pkg.installed:
    - pkgs:
        - xclip

