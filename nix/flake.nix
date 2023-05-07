{
  description = "My home-manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # specify user packages and home configuration
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # manage system level software and configuration on OSX
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # many GL applications are broken when not using NixOS...
    # https://github.com/guibou/nixGL
    nixgl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # https://nixos.wiki/wiki/Emacs
    # https://nixos.wiki/wiki/Overlays#In_a_Nix_flake
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs@{ nixpkgs, darwin, home-manager, nixgl, emacs-overlay, ... }:
    let
      # impure
      # system = builtins.currentSystem;
      pkg-config = {
        allowUnfree = true;
        allowBroken = true;
        allowInsecure = false;
        # allowUnsupportedSystem = true;
      };
      common-overlays =
        [
          # default emacs-overlay (overriden by ./overlays/emacs.nix)
          (import emacs-overlay)
        ] ++
        # https://github.com/dustinlyons/nixos-config/blob/fe39468c0bf5e3e31842aeecc3748eedce8a4321/common/default.nix#L17
        # apply each overlay found in the /overlays directory
        (let path = ./overlays; in with builtins;
               map (n: import (path + ("/" + n)))
                 (filter (n: match ".*\\.nix" n != null ||
                             pathExists (path + ("/" + n + "/default.nix")))
                   (attrNames (readDir path))));
      linux-pkgs = import nixpkgs {
        system = "x86_64-linux";
        config = pkg-config;
        overlays = common-overlays ++ [ nixgl.overlays.default ];
      };
      darwin-pkgs = import nixpkgs {
        system = "aarch64-darwin";
        config = pkg-config;
        overlays = common-overlays;
      };
    in {
      # nix-darwin configuration
      darwinConfigurations.default = darwin.lib.darwinSystem {
        pkgs = darwin-pkgs;
        system = "aarch64-darwin";
        modules = [ ./darwin ];
      };

      homeConfigurations.noct = home-manager.lib.homeManagerConfiguration {
        pkgs = linux-pkgs;
        modules = [ ./linux ];
      };

      homeConfigurations.wsl = home-manager.lib.homeManagerConfiguration {
        pkgs = linux-pkgs;
        modules = [ ./wsl ];
      };
    };
}
