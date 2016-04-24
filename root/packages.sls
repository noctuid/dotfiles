pacman_related:
  pkg.installed:
    - pkgs:
        # for generating .SRCINFO from PKGBUILD
        - pkgbuild-introspection
        # updating pacman mirror list
        - reflector

general:
  pkg.installed:
    - pkgs:
        - linux-ck-ivybridge
        - linux-ck-ivybridge-headers
        - broadcom-wl-ck-ivybridge 
        - virtualbox-ck-host-modules-ivybridge 
        - git
        - ntp
        - wine
        - winetricks
        - virtualbox
        - virtualbox-guest-iso
        - virtualbox-guest-utils
        - steam
        # automatic mounting
        - udevil
        # disk health monitoring
        - smartmontools
        # filesystem support
        - ntfs-3g
        - dosfstools
        - exfat-utils
        # when opening man pages complain wanting uudecode
        - sharutils
        # for cl minispec
        - phantomjs
        # power management
        - tlp
        # firewall
        - ufw
# because can't have multiple "cmd.run"s
# mtp must die
general_jmptpfs:
  cmd.run:
    - name: aura -A --noconfirm jmtpfs-git
    - unless: pacman -Q jmtpfs-git
general_roccat:
  cmd.run:
    - name: aura -A --noconfirm roccat-tools-konepuremilitary
    - unless: pacman -Q roccat-tools-konepuremilitary
# launcher
general_dmenu:
  cmd.run:
    - name: aura -A --noconfirm dmenu-git
    - unless: pacman -Q dmenu-git
general_bemenu:
  cmd.run:
    - name: aura -A --noconfirm bemenu-git
    - unless: pacman -Q bemenu-git
# general_grive:
  # backup to google drive
  # cmd.run:
  #   - name: aura -A --noconfirm grive
  #   - unless: pacman -Q grive
general_spideroak:
  # backup to spideroak
  cmd.run:
    - name: aura -A --noconfirm spideroak-one
    - unless: pacman -Q spideroak-one
general_connman:
  cmd.run:
    - name: nix-env -iA nixpkgs.connman
    - unless: nix-env -q connman
general_preload:
  cmd.run:
    - name: aura -A --noconfirm preload
    - unless: pacman -Q preload

# https://wiki.archlinux.org/index.php/CUPS
printing:
  pkg.installed:
    - pkgs:
        - cups
        - libcups
        - splix
        # - hplip
        # - gutenprint
  # necessary still?
  # cmd.run:
  #   - name: aura -A --noconfirm samsung-unified-driver
  #   - unless: samsung-unified-driver

gui:
  pkg.installed:
    - pkgs:
        - gvim
        - libreoffice-fresh
        - mpv
        - gimp
        # - flashplugin
        # pdf viewers
        - zathura
        # for column selection/copying, poppler will result in every item in the
        # first column being put before every item in the second
        # mupdf will keep the rows on the same line
        - zathura-pdf-mupdf
        - zathura-djvu
        - apvlv
        # psp emulator
        - ppsspp
gui_emacs:
  cmd.run:
    - name: aura -A --noconfirm emacs-git
    - unless: pacman -Q emacs-git
gui_emacs-24.5:
  cmd.run:
    # TODO prevent this from making a symlink to "emacs" and "emacsclient"
    - name: nix-env -iA nixpkgs.emacs-24.5
    - unless: nix-env -q emacs-24.5
gui_qcma:
  # vita file transfer
  cmd.run:
    - name: aura -A --noconfirm qcma-git
    - unless: pacman -Q qcma-git

browser:
  pkg.installed:
    - pkgs:
        - firefox
        - profile-cleaner
        - profile-sync-daemon
        - chromium
browser_freshplayer:
  cmd.run:
    - name: aura -A --noconfirm freshplayerplugin-git
    - unless: pacman -Q freshplayerplugin-git

terminal/commandline:
  pkg.installed:
    - pkgs:
        - zsh
        - tmux
        - mlocate
        - shellcheck
        # fcron is best cron
        - fcron
        - xterm
        - rxvt-unicode
        - urxvt-perls
        - xsel
        - xclip
        - w3m
        - surfraw
        - lsof
        - lshw
        - cdu
        # - ncdu
        # spell checking
        - hunspell-en
        - aspell-en
        # X utils for scripting
        - xdotool
        - xorg-xprop
        - xorg-xwininfo
        # font lookup
        - xorg-xfontsel
        - xorg-xlsfonts
        # interactive
        - xorg-xev
        # battery info
        - acpi
        # file conversion using libreoffice
        - unoconv
        # - pandoc
        # - weechat
terminal_tmuxinator:
  cmd.run:
    - name: nix-env -iA nixpkgs.tmuxinator
    - unless: nix-env -q tmuxinator
terminal_termite:
  cmd.run:
    - name: aura -A --noconfirm termite-ranger-fix-git
    - unless: pacman -Q termite-ranger-fix-git
terminal_trash-cli:
  cmd.run:
    - name: aura -A --noconfirm trash-cli-git
    - unless: pacman -Q trash-cli-git
terminal_vimpager:
  cmd.run:
    - name: aura -A --noconfirm vimpager-git
    - unless: pacman -Q vimpager-git
terminal_ranger:
  cmd.run:
    - name: aura -A --noconfirm ranger-git
    - unless: pacman -Q ranger-git
# TODO get rid of any uses of xsendkey and maybe xdo
terminal_xdo:
  cmd.run:
    - name: aura -A --noconfirm xdo-git
    - unless: pacman -Q xdo-git
terminal_xsendkey:
  cmd.run:
    - name: aura -A --noconfirm xsendkey
    - unless: pacman -Q xsendkey
terminal_light:
  # controlling backlight
  cmd.run:
    - name: aura -A --noconfirm light-git
    - unless: pacman -Q light-git
terminal_imgur-cli:
  cmd.run:
    - name: aura -A --noconfirm imgur-cli-svn
    - unless: pacman -Q imgur-cli-svn
terminal_stderred:
  cmd.run:
    - name: aura -A --noconfirm stderred-git
    - unless: pacman -Q stderred-git
terminal_fzf:
  cmd.run:
    - name: nix-env -iA nixpkgs.fzf
    - unless: nix-env -q fzf
terminal_tmsu:
  cmd.run:
    - name: nix-env -iA nixpkgs.tmsu
    - unless: nix-env -q tmsu

mail:
  pkg.installed:
    - pkgs:
        # getting mail
        - isync
        # sending mail
        - msmtp
        # symlinks to sendmail
        - msmtp-mta
        # for mutt contacts
        - abook
  cmd.run:
    - name: aura -A --noconfirm mu-git
    - unless: pacman -Q mu-git
  # cmd.run:
  #   - name: aura -A --noconfirm neomutt
  #   - unless: pacman -Q neomutt

audio/music:
  pkg.installed:
    - pkgs:
        - mpd
        # backup client
        - ncmpcpp
        - mpc
        # cd ripping
        - abcde
        # controlling output
        - ponymix
        # I think this fixed wine audio for me (don't remember for certain)
        - lib32-alsa-plugins
        - glyr
music_vimus:
  cmd.run:
    - name: nix-env -iA nixpkgs.haskellPackages.vimus
    - unless: nix-env -q vimus
music_beets:
  cmd.run:
    # installs discogs, pyacoustid, fetchart, etc. by default
    - name: nix-env -iA nixpkgs.beets
    - unless: nix-env -q beets
music_mpdscribble:
  cmd.run:
    - name: aura -A --noconfirm mpdscribble
    - unless: pacman -Q mpdscribble
music_mpd_notification:
  cmd.run:
    - name: aura -A --noconfirm mpd-notification
    - unless: pacman -Q mpd-notification
audio_pacmixer:
  cmd.run:
    - name: aura -A --noconfirm pacmixer
    - unless: pacman -Q pacmixer

other_media:
  pkg.installed:
    - pkgs:
        - imagemagick
        - graphicsmagick
        - youtube-dl
        # screenshots
        - maim
        # selection
        - slop
        # for ranger video preview
        - ffmpegthumbnailer
        # ranger info
        - mediainfo
  cmd.run:
    - name: nix-env -iA nixpkgs.ffmpeg-full
    - unless: nix-env -q ffmpeg-full

# # sile
# # typesetting:
# #   pkg.installed:
# #     - pkgs:
#         # - texlive-bin
#         # - texlive-latexextra

input/keyboard:
  pkg.installed:
    - pkgs:
        - sxhkd
input_mozc:
  cmd.run:
    - name: aura -A --noconfirm mozc
    - unless: pacman -Q mozc
input_ibus:
  cmd.run:
    - name: aura -A --noconfirm ibus-mozc
    - unless: pacman -Q ibus-mozc
keyboard_xcape:
  cmd.run:
    - name: aura -A --noconfirm xcape
    - unless: pacman -Q xcape
keyboard_xchainkeys:
  cmd.run:
    - name: nix-env -iA nixpkgs.xchainkeys
    - unless: nix-env -q xchainkeys

visual:
  pkg.installed:
    - pkgs:
        - xautolock
        - redshift
        - compton
        # notifications
        - libnotify
        # - dunst
visual_slimlock:
  cmd.run:
    - name: aura -A --noconfirm slimlock-git
    - unless: pacman -Q slimlock-git
visual_dunst:
  cmd.run:
    - name: aura -A --noconfirm dunst-git
    - unless: pacman -Q dunst-git
visual_lemonbar:
  cmd.run:
    - name: aura -A --noconfirm lemonbar-xft-git
    - unless: pacman -Q lemonbar-xft-git
visual_setroot:
  cmd.run:
    - name: aura -A --noconfirm setroot-git
    - unless: pacman -Q setroot-git
visual_conky:
  cmd.run:
    - name: aura -A --noconfirm conky-lua
    - unless: pacman -Q conky-lua
visual_xtitle:
  cmd.run:
    - name: aura -A --noconfirm xtitle-git
    - unless: pacman -Q xtitle-git

fonts/themes:
  pkg.installed:
    - pkgs:
        # - infinality-bundle
        - cairo-infinality-ultimate
        - fontconfig-infinality-ultimate
        - freetype2-infinality-ultimate
        - ibfonts-meta-base
        - ibfonts-meta-extended
        - ttf-inconsolata
        - adobe-source-han-sans-jp-fonts
        - otf-fira-mono-ibx
        # japanese font
        - otf-ipafont
        # unicode support
        - bdf-unifont
        # using siji now
        # - stlarch-font-ibx
font_powerline:
  cmd.run:
    - name: aura -A --noconfirm powerline-fonts-git
    - unless: pacman -Q powerline-fonts-git
font_inconsolata-g:
  cmd.run:
    - name: aura -A --noconfirm ttf-inconsolata-g
    - unless: pacman -Q ttf-inconsolata-g
font_siji:
  cmd.run:
    - name: aura -A --noconfirm siji-git
    - unless: pacman -Q siji-git
font_ms:
  cmd.run:
    - name: aura -A --noconfirm ttf-ms-fonts
    - unless: pacman -Q ttf-ms-fonts
theme_numix-shine:
  cmd.run:
    - name: aura -A --noconfirm numix-shine-icon-theme-git
    - unless: pacman -Q numix-shine-icon-theme-git
theme_numix-archblue:
  cmd.run:
    - name: aura -A --noconfirm numix-themes-archblue
    - unless: pacman -Q numix-themes-archblue

npm:
  pkg.installed:
    - pkgs:
        - nodejs
        - npm
npm_packages:
  cmd.run:
    - name: npm install livedown
    - unless: which livedown
    # this is slow
    # - unless: npm list | grep livedown
    - require:
        # not name of actual package
        - pkg: npm

pip:
  pkg.installed:
    - pkgs:
        - python-pip
python_packages:
  cmd.run:
    - name: pip install harvey
    - unless: which harvey
    # - unless: pip list | grep harvey
    - require:
        - pkg: pip
python_flake8:
  cmd.run:
    - name: nix-env -iA nixpkgs.pythonPackages.flake8
    - unless: nix-env -q flake8

dns:
  pkg.installed:
    - pkgs:
        # resolve
        - unbound
        # dnssec validation with unbound
        - expat
        # https://wiki.archlinux.org/index.php/DNSCrypt
        - dnscrypt-proxy

hostsblock:
  cmd.run:
    - name: aura -A --noconfirm hostsblock
    - unless: pacman -Q hostsblock
kwakd-package:
  cmd.run:
    - name: aura -A --noconfirm kwakd
    - unless: pacman -Q kwakd

lisp/scheme/etc:
  pkg.installed:
    - pkgs:
        - racket
    # I think guile is in the base install
    # - guile
    # - clojure
lisp_roswell:
  cmd.run:
    - name: aura -A --noconfirm roswell
    - unless: pacman -Q roswell
lisp_sbcl:
  cmd.run:
    - name: ros install sbcl-bin
    - unless: ros list installed | grep sbcl-bin
# rust
  # cmd.run:
  #   - name: aura -A --noconfirm rust-nightly-bin
  #   - unless: pacman -Q rust-nightly-bin
# leiningen
  # cmd.run:
  #   - name: nix-env -iA nixpkgs.leiningen
  #   - unless: nix-env -q leiningen

ghc:
  cmd.run:
    - name: nix-env -iA nixpkgs.haskellPackages.ghc
    - unless: nix-env -q ghc

haskell-stack:
  cmd.run:
    - name: nix-env -iA nixpkgs.haskellPackages.stack
    - unless: nix-env -q stack

stylish-haskell:
  cmd.run:
    - name: nix-env -iA nixpkgs.haskellPackages.stylish-haskell
    - unless: nix-env -q stylish-haskell

hlint:
  cmd.run:
    - name: nix-env -iA nixpkgs.haskellPackages.hlint
    - unless: nix-env -q hlint

hindent:
  cmd.run:
    - name: nix-env -iA nixpkgs.haskellPackages.hindent
    - unless: nix-env -q hindent

ghc-mod:
  cmd.run:
    - name: nix-env -iA nixpkgs.haskellPackages.ghc-mod
    - unless: nix-env -q ghc-mod

hoogle:
  cmd.run:
    - name: nix-env -iA nixpkgs.haskellPackages.hoogle
    - unless: nix-env -q hoogle

happy:
  cmd.run:
    - name: nix-env -iA nixpkgs.haskellPackages.happy
    - unless: nix-env -q happy

alex:
  cmd.run:
    - name: nix-env -iA nixpkgs.haskellPackages.alex
    - unless: nix-env -q alex

dict:
  pkg.installed:
    - pkgs:
        - sdcv
        - words
stardict:
  cmd.run:
    - name: aura -A --noconfirm stardict-oald
    - unless: pacman -Q stardict-oald
stardict_thesaurus:
  cmd.run:
    - name: aura -A --noconfirm stardict-thesaurus-ee
    - unless: pacman -Q stardict-thesaurus-ee

archive_related:
  pkg.installed:
    - pkgs:
        - unrar
        - unzip
        - zip
        - p7zip
        # frontend for archive extraction, creation, etc.
        - atool

# https://bugs.archlinux.org/task/47325?project=1
encryption:
  cmd.run:
    - name: nix-env -iA nixpkgs.truecrypt
    - unless: nix-env -q truecrypt
secure_delete:
  cmd.run:
    - name: aura -A --noconfirm secure-delete
    - unless: pacman -Q secure-delete
