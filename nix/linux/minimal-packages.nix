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
  nil
  statix
  # to index store to be able to use nix-locate
  nix-index

# * Emacs
  # emacsNoctuidWithPackages
  # this doesn't really work well due to the multiple binaries
  # https://www.danielcorin.com/til/nix/managing-multiple-tool-versions/
  # https://discourse.nixos.org/t/install-multiple-versions-of-a-package-each-with-its-own-binary/19287
  # (writeShellScriptBin "x_emacs" ''
  #   exec "${emacsNoctuidWithPackages}/bin/emacs" "$@"
  # '')
  # # seems this can end up using the pgtk emacs
  # (writeShellScriptBin "x_emacsclient" ''
  #   exec "${emacsNoctuidWithPackages}/bin/emacsclient" "$@"
  # '')

  emacsPgtkWithPackages

  # json parsing is now builtin (>=30.1); don't need jansson anymore
  # jansson

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
  # currently a security error
  # pantalaimon

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

# * X11
  unclutter-xfixes

]
