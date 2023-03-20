{ pkgs }:

with pkgs;
let common = import ../common/packages.nix { pkgs = pkgs; }; in
common ++ [
  exfat

  # backup terminal
  iterm2

  # A wrapper that provides access to the Mac OS X pasteboard service
  # used by tmux for example
  reattach-to-user-namespace

  # for ranger previews
  # not in common since included in unoconv (in linux packages)
  odt2txt

  # hotkey daemon
  skhd

  karabiner-elements

  dockutil
]
