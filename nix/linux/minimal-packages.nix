{ pkgs }:

# have had too many problems using GUI (opengl) nix packages and with mixing and
# matching arch and nix packages

# so only install these types of packages:
# - nix-related packages
# - any packages not in AUR
# - some packages in AUR if don't need latest version
# - emacs (for patches, exact control over which version, etc.)

with pkgs; [
# * Nix-Related
  rnix-lsp
  statix
  # to index store to be able to use nix-locate
  nix-index

# * Emacs
  emacsNoctuidWithPackages
  # not whether this is needed
  jansson

# * CLI
  direnv
  nix-direnv
  lorri
  # aur package and out-of-date
  pistol

  # aur package doesn't build
  setroot

# * Matrix
  # non-git aur package is orhpaned
  # will need to verify all other devices through panctl
  # > list-devices - see available devices
  # > start-verification tab tab <device to verify>
  # confirm on other device
  # > confirm verification <same args>
  pantalaimon

# * Torrents
  # nodePackages.webtorrent-cli

# * Mouse Tools
  # prefer warpd over keynav though don't really use
  # it's too old
  # warpd

# * Sound/Music
  # jamesdsp

# * Hotkey Daemon
  # have had issues with aur package in past
  xchainkeys

# * Screen locker
  # wrapper around i3lock-color
  # TODO password fails
  # betterlockscreen

# * Appearance/Theming
  # wpgtk is aur only; makes sense to also use pywal from nix
  pywal
  wpgtk

# * X11
  unclutter-xfixes

]
