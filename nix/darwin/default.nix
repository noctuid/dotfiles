{ config, pkgs, ... }:

# settings may need to change in the future
# - system.defaults.LaunchServices.LSQuarantine - Whether to enable quarantine
#   for downloaded applications. The default is true.
# - system.defaults.NSGlobalDomain."com.apple.sound.beep.volume"
# - system.defaults.NSGlobalDomain."com.apple.sound.beep.feedback"
# - programs.nix-index.enable - Whether to enable nix-index and its
#   command-not-found helper.
# - security.pam.enableSudoTouchIdAuth
# - system.defaults.NSGlobalDomain.NSAutomaticQuoteSubstitutionEnabled
# - system.defaultsNSGlobalDomain._HIHideMenuBar - whether to autohide menu bar

let
  user = builtins.readFile ./username;
  homeDir = "/Users/${user}";
  configDir = "${homeDir}/.config";
  cacheDir = "${homeDir}/.cache";
in
{
  # https://nix-community.github.io/home-manager/index.html#sec-install-nix-darwin-module
  imports = [
    <home-manager/nix-darwin>
  ];

  # * Nix
  # auto upgrade nix package and the daemon service
  services.nix-daemon.enable = true;

  nix = {
    package = pkgs.nixUnstable;

    # "A list of names of users that have additional rights when connecting to
    # the Nix daemon, such as the ability to specify additional binary caches,
    # or to import unsigned NARs."
    # @admin - anyone in wheel group
    settings.trusted-users = [ "root" "@admin" ];

    # already have in user's nix.conf, so I'm not sure this is actually useful
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # automatically garbage collect to reduce nix store size
    gc = {
      user = "root";
      automatic = true;
      # gc at beginning of each weak at 2am
      interval = { Weekday = 0; Hour = 2; Minute = 0; };
      options = "--delete-older-than 30d";
    };

    settings = {
      # you can use the nix-community cache to prevent having to build
      # emacs-overlay, but you need to use the exact same build inputs, so I
      # probably won't ever benefit from it
      # https://old.reddit.com/r/NixOS/comments/m2hmum/nixcommunity_cachix_frequently_not_used/
      substituters = [
        # for more binaries; can upload your own binaries to cachix as well
        "https://nix-community.cachix.org"
        # default is just this
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  # * Environment
  # installing both here and with home manager (should be shared in nix store)
  environment.systemPackages = with pkgs;
    (import ./packages.nix { pkgs = pkgs; });

  # paths to add to PATH
  # environment.systemPath

  # * Fonts
  # don't fully manage fontdir (will remove any manually installed fonts)
  # (default is false)
  fonts.fontDir.enable = false;

  # fonts to install
  fonts.fonts = [
    # https://github.com/NixOS/nixpkgs/blob/master/pkgs/data/fonts/nerdfonts/shas.nix
    (pkgs.nerdfonts.override {
      fonts = ["CascadiaCode" "FiraCode" "FiraMono"];
    })
    pkgs.cascadia-code
    pkgs.office-code-pro
    pkgs.font-awesome
  ];

  # * Homebrew
  # for anything not packaged or don't want through nix
  homebrew = {
    enable = true;
    # completely remove brew packages/casks and their files if they are removed
    # here
    # may want to install from other brewfiles
    # onActivation.cleanup = "zap";
    taps = [
      "homebrew/core"
      "homebrew/cask"
      "homebrew/cask-fonts"
      "koekeishiya/formulae"
    ];
    brews =  [
      "wallpaper"

      "pyenv"
      "nvm"

      # for gls for example
      # TODO fails
      # "ext4fuse"

      # trash-cli works; doesn't show up in trash can, but I don't use it
      # "trash"

      # not packaged in nix for darwin for some reason
      "progress"

    ];
    casks = [
      # not packaged for nix
      "font-delugia-complete"
      "sf-symbols"

      # keyboard layout editing
      "ukelele"

      # launchers
      "alfred"
      "raycast"

      "wezterm"

      # window switching; bound to command+. by default
      # "hotswitch"

      # if ever need and it works
      # "virtualbox"
      # "virtualbox-extension-pack"

      # keep mac awake; may not work
      # "caffeine"
      # "keepingyouawake"

      # TODO maybe if want 3 finger middle click
      # https://formulae.brew.sh/cask/middleclick#default

      # backup
      "visual-studio-code"

    ];
    # need to be signed into app store; won't automatically uninstall if remove
    # These app IDs are from using the mas CLI app
    # mas = mac app store
    # https://github.com/mas-cli/mas
    # homebrew.masApps = {
    #   "hidden-bar" = 1452453066;
    # };
  };

  # * Networking
  # networking.dns

  # * Program Configuration
  programs.zsh.enable = true;

  # * Services
  # would rather use user service and config; see home-manager config below
  # services.skhd.enable = true;

  # yabai needs root to set up scripting additions; this seems a lot saner than
  # having to configure for passwordless sudo; instead run as root and drop root
  # permissions to run the user config (which is possible since the config just
  # a script, unlike skhd)
  services.yabai.enable = true;
  services.yabai.enableScriptingAddition = true;
  services.yabai.extraConfig =
    "sudo -u ${user} ${configDir}/yabai/yabairc";

  # * System Settings
  # TODO what does this actually do?
  # system.keyboard.enableKeyMapping

  system.defaults = {
    # ** Appearance
    # dark mode
    NSGlobalDomain.AppleInterfaceStyle = "Dark";

    # ** Dock, Mission Control
    dock = {
        autohide = true;
        # make smaller (default 64)
        tilesize = 48;

        # whether to automatically rearrange spaces based on most recent use
        # don't want if using yabai
        mru-spaces = false;

        # make icons of hidden applications translucent
        # showhidden
        # autohide-delay
        # animation speed
        # autohide-time-modifier
        # mission control animation speed
        # expose-animation-duration

        # defaults
        # show-recents = true;
        # animate opening applications from dock
        # launchanim = true;
        # orientation = "bottom";
    };

    # ** Keyboard
    # step sliders in UI are:
    # InitialKeyRepeat: 120, 94, 68, 35, 25, 15
    # KeyRepeat: 120, 90, 60, 30, 12, 6, 2
    # default is 25 and 6
    # multiply each by 15 to get milliseconds
    # result: 300ms ti start and 66.6... repeats per second
    # second
    NSGlobalDomain.InitialKeyRepeat = 20;
    NSGlobalDomain.KeyRepeat = 1;

    # ** Mouse
    # enable tap to click
    trackpad.Clicking = true;
    # TODO what is the difference between these two?
    NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;

    # disable mouse acceleration
    # FIXME
    # ".GlobalPreferences"."com.apple.mouse.scaling" = (-1.0);

    # disable natural scroll direction
    NSGlobalDomain."com.apple.swipescrolldirection" = false;

    # tracking speed; 0.0-3.0 (default 1)
    # NSGlobalDomain."com.apple.trackpad.scaling"

    # ** Finder
    # don't show desktop icons
    finder.CreateDesktop = false;

    NSGlobalDomain.AppleShowAllExtensions = true;
    finder.AppleShowAllExtensions = true;
    # default to list view
    finder.FXPreferredViewStyle = "Nlsv";

    # full path in window title
    finder._FXShowPosixPathInTitle = true;

    # finder sidebar icon size
    # NSGlobalDomain.NSTableViewDefaultSizeMode
  };

  # * Users
  users.users.${user} = {
    name = user;
    home = homeDir;
  };

  # * Home Manager
  # use home manager as nix-darwin module, so that user profiles are built
  # together with the system when running darwin-rebuild
  home-manager = {
    useGlobalPkgs = true;
    users.${user} = {
      home.enableNixpkgsReleaseCheck = false;
      home.packages = pkgs.callPackage ./packages.nix {};

      home.stateVersion = "22.11";

      # extra directories to add to path
      home.sessionPath = [
        "${user}/bin"
      ];

      launchd.agents.skhd = {
        enable = true;
        config = {
          ProgramArguments = [
            "${pkgs.skhd}/bin/skhd"
            "-c"
            "${configDir}/skhd/skhdrc"
          ];
          KeepAlive = true;
          ProcessType = "Interactive";
          EnvironmentVariables = {
            # NOTE: not necessary; will get PATH from non-interactive shell
            # config (.zshenv, which loads my profile)

            # PATH = pkgs.lib.concatStringsSep ":" [
            #   "/run/current-system/sw/bin"

            #   "/nix/var/nix/profiles/default/"
            #   "${homeDir}/.nix-profile/bin"

            #   "/bin"
            #   "/sbin"
            #   "/usr/bin"
            #   "/usr/sbin"
            #   "/usr/local/bin"
            #   "${homeDir}/.local/bin"
            # ];
          };
          StandardOutPath = "${cacheDir}/skhd.log";
          StandardErrorPath = "${cacheDir}/skhd.log";
        };
      };

      # https://github.com/dustinlyons/nixos-config/blob/21b871ed43e821611dd00c9874112a0a2c4c0bcd/darwin/home-manager.nix#L85
      # https://github.com/nix-community/home-manager/issues/3344
      # "Marked broken Oct 20, 2022 check later to remove this
      # Confirmed still broken, Mar 5, 2023"
      manual.manpages.enable = false;
    };
  };
}
