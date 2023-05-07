{ config, pkgs, ... }:

# NOTE: only using for packages; stow more convenient for batch symlinking

{
  # TODO use separate files for separate users; can't do this:
  # home.username = builtins.getEnv "USER";
  # home.homeDirectory = builtins.getEnv "HOME";
  home.username = "fox";
  home.homeDirectory = "/home/fox";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  # let home-manager install and manage itself
  programs.home-manager.enable = true;

  home.packages = pkgs.callPackage ./packages.nix {};
}
