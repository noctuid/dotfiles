{ pkgs }:

# packages for personal laptop

with pkgs;
let
  # https://pmiddend.github.io/posts/nixgl-on-ubuntu/
  # https://github.com/NixOS/nixpkgs/issues/9415
  # https://nixos.wiki/wiki/Nixpkgs_with_OpenGL_on_non-NixOS
  # this approach: https://github.com/guibou/nixGL/issues/44#issuecomment-1361524862
  # alt approach: https://github.com/guibou/nixGL/issues/16#issuecomment-903188923
  # need prime-run when using Nvidia and optimus-manager
  nixGLWrap = pkg: pkgs.runCommand "${pkg.name}-nixgl-wrapper" {} ''
    mkdir "$out"
    ln -s ${pkg}/* "$out"
    rm -f "$out"/bin
    mkdir "$out"/bin
    for binary in ${pkg}/bin/*; do
        wrapped_binary=$out/bin/$(basename "$binary")
        echo "exec prime-run \"${lib.getExe nixgl.auto.nixGLDefault}\" \
             \"$binary\" \"\$@\"" > "$wrapped_binary"
        chmod +x "$wrapped_binary"
    done
  '';
  nixGLIntelWrap = pkg: pkgs.runCommand "${pkg.name}-nixgl-wrapper" {} ''
    mkdir "$out"
    ln -s ${pkg}/* "$out"
    rm -f "$out"/bin
    mkdir "$out"/bin
    for binary in ${pkg}/bin/*; do
        wrapped_binary=$out/bin/$(basename "$binary")
        echo "exec \"${lib.getExe nixgl.nixGLIntel}\" \"$binary\" \"\$@\"" \
             > "$wrapped_binary"
        chmod +x "$wrapped_binary"
    done
  '';
  common-packages = import ../common/packages.nix { pkgs = pkgs; };
  # packages to remove since we need to wrap them with nixGL
  remove-packages = [ kitty mpv ];
  filtered-packages =
    builtins.filter (x: !builtins.elem x remove-packages) common-packages;
in
filtered-packages ++ [
  # required to run a lot of things...
  nixgl.auto.nixGLDefault
  nixgl.nixGLIntel

  (nixGLIntelWrap kitty)

# * Filesystem Support
  exfatprogs

# * Window Manger
  # TODO cursor too small on root (no window), and .profile fix that works for
  # other programs doesn't fix this
  # herbstluftwm
  # bspwm

# * Input
  fcitx5
  fcitx5-mozc
  # latex input
  fcitx5-table-other

# * Graphics, Drivers, Games, Emulators
  # TODO missing
  # unnamed sdvx clone

  # missing fonts and other issues
  # (nixGLWrap steam)
  # (nixGLWrap lutris)

  ppsspp
  dolphin-emu

# * Security
  wireguard-tools

  firejail

# * Shell
  # only for linux
  # for coreutils commands (e.g. cp, dd)
  progress

  # sanitize file names
  detox

# * Browsers
  # breaks some tridactyl scripts
  # (nixGLIntelWrap firefox)
  # TODO still crashes
  # (nixGLIntelWrap ungoogled-chromium)
  (nixGLIntelWrap qutebrowser)

# * Wine
  wine
  winetricks

# * Doc, PPT, etc. Editing/File Conversion
  libreoffice
  unoconv
  # depends on inscure package (qtwebkit)
  # wkhtmltopdf

# * Blog/Static Websites
  hugo

# * Image Editing/Conversion
  gimp-with-plugins
  imagemagick
  gifsicle
  graphicsmagick
  waifu2x-converter-cpp
  # TODO waifu2x-ncnn-vulka
  # TODO soryu

  # png compression
  oxipng
  # lossy png compression (huge size savings)
  pngquant
  # cwebp - convert to webp
  libwebp

# * Torrents
  transmission
  nodePackages.webtorrent-cli

# * General Utilities
  mlocate

  # to find what files in use that are preventing unmounting
  lsof
  # hardware info
  lshw
  # for sysinfo script
  dmidecode

  # when opening man pages, complains wanting uudecode
  sharutils

# * Time Syncing
  # time syncing
  ntp
  # recommended over ntpd but unnecessarily complicated to set up
  # chrony

# * Disk Utilities
  gparted
  gnome.gnome-disk-utility
  # disk health monitoring
  smartmontools
  # SATA/IDE device parameters (e.g. check for trim support)
  hdparm

# * Entropy
  rng-tools

# * Keyboard Remapping
  xcape
  # TODO klfc
  # for keyboardio model 01
  arduino

# * Mouse Tools
  # prefer to keynav though don't really use
  warpd

# * Virtualization
  # doesn't work on new macs (and no nix darwin version)
  virtualbox

# * Mail
  # getting mail
  isync
  # sending mail
  msmtp
  # TODO symlinks sendmail to msmtp
  # msmtp-mta
  # mu and mu4e (email client)
  mu
  # used for formail to get header information
  procmail

# * Sound/Music
  # TODO should be system level
  # pipewire
  # TODO pipewire-pulse, pipewire-alsa?
  jamesdsp
  # jamesdsp-puls

  # for amixer and alsamixer
  alsa-utils
  pavucontrol
  pamixer

  # mpd
  # mpc-cli
  # TODO mpd-notification

  # TODO just fails to find extensions
  # mopidy
  # mopidy-local
  # mopidy-mpd
  # TODO mopidy-spotify

  tidal-hifi

  # cd ripping
  abcde
  # abcde optional dependences needed
  glyr
  # TODO is this right?
  perl536Packages.MusicBrainz
  # perl-musicbrainz-discid aur
  # perl-webservice-musicbrainz aur
  cdrtools

  beets
  # is this still needed with nix
  python311Packages.pyacoustid
  python311Packages.requests
  # python-mpd

# * Video/Images/Other Media
  # control mpd, spotify, browser video/audo/etc.
  playerctl

  # no video track...
  # (nixGLWrap mpv)

  # linux only
  pqiv

  # color picker
  gpick

# * Hotkey Daemon
  sxhkd
  xchainkeys

# * Screen locker
  xss-lock
  # wrapper around i3lock-color
  # TODO password fails
  # betterlockscreen

# * Compositing
  # will completely mess up screen with nixGLWrap!
  (nixGLIntelWrap picom)

# * Notifications
  libnotify
  dunst

# * Launchers
  # bemenu
  rofi

# * Fonts
  # TODO system level?

# * Appearance/Theming
  polybarFull
  setroot
  xtitle

  # live reload gtk theme
  xsettingsd

  # for framebuffer colors
  # TODO setcolors

  # colorscheme generation/setup from wallpaper
  pywal
  wpgtk
  # TODO flatcolor included? wpg-install.sh?

  # TODO python-wal-steam-git aur
  # TODO pywalfox

  # display colors and system statistics
  neofetch
  disfetch
  # TODOfails to build
  # uwufetch
  # TODO sysfetch
  # TODO nerdfetch


  # ascii art
  cbonsai
  asciiquarium
  # TODO pokemon-colorscripts-git

# * Screenshots
  # maim is in common
  slop

# * Bookmarking
  buku

# * X11
  # automatic xrandr setup based on connected devices
  autorandr

  xdotool
  # TODO not here
  # xorg-server
  # xorg-xinit

  xorg.xinput
  xorg.xprop
  xorg.xwininfo
  xorg.xfontsel
  xorg.xlsfonts
  xorg.xsetroot
  xorg.xev
  xorg.xbacklight
  # TODO orphaned; git version works but with, it overlay doesn't build
  # light

  # clipboard
  xsel
  xclip

  # TODO start using
  unclutter-xfixes

# * Android
  # for adb
  android-tools
  adb-sync

# * Chat
  discord
  # TODO test zoom-us
  # teams for linux is dead
  element-desktop

]
