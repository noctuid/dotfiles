{ pkgs }:

# packages for wsl

with pkgs;
let
  common-packages = import ../common/packages.nix { pkgs = pkgs; };
  remove-packages = [
    # replace with pgtk emacs
    emacsNoctuidWithPackages
    # need to run f.lux or whatever on system side
    redshift
    kanata
    maim
    # don't currently need and probably won't need
    borgbackup
    restic
    gallery-dl
    # easier to use distro package than wrap with nixGL
    kitty
  ];
  filtered-packages =
    builtins.filter (x: !builtins.elem x remove-packages) common-packages;
in
filtered-packages ++ [
  emacsPgtkWithPackages
]
