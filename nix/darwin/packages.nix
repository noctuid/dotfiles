{ pkgs }:

with pkgs;
let
  common-packages = import ../common/packages.nix { pkgs = pkgs; };
  remove-packages = [
    # keybase fails to build
    keybase
    kbfs
  ];
  filtered-packages =
    builtins.filter (x: !builtins.elem x remove-packages) common-packages;
in
filtered-packages ++ [

  exfat

  # backup terminal
  iterm2

  # A wrapper that provides access to the Mac OS X pasteboard service
  # used by tmux for example
  reattach-to-user-namespace

  # for ranger previews
  # not in common since included in unoconv (in linux packages)
  odt2txt

  # wm
  yabai
  # hotkey daemon
  skhd
  # panel
  sketchybar

  karabiner-elements

  dockutil
]
