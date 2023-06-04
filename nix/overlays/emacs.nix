# my custom Emacs
# use emacs-plus patches on osx
# (eventually) use lucid on linux

# relevant links:
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/emacs/generic.nix
# https://github.com/nix-community/emacs-overlay/blob/master/overlays/emacs.nix
# https://github.com/d12frosted/homebrew-emacs-plus/tree/master/patches/emacs-30

self: super: rec {
  # configuration shared for all systems
  emacsGitNoctuidGeneric = super.emacsGit.override {
    withSQLite3 = true;
    withWebP = true;
    withImageMagick = true;
    # have to force this; lib.version check wrong or because emacsGit?
    withTreeSitter = true;
  };
  emacsNoctuid =
    if super.stdenv.isDarwin
    then
      emacsGitNoctuidGeneric.overrideAttrs (old: {
        patches =
          (old.patches or [])
          ++ [
            # Don't raise another frame when closing a frame
            (super.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/no-frame-refocus-cocoa.patch";
              sha256 = "QLGplGoRpM4qgrIAJIbVJJsa4xj34axwT3LiWt++j/c=";
            })
            # Fix OS window role so that yabai can pick up Emacs
            (super.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
              sha256 = "+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
            })
            # Use poll instead of select to get file descriptors
            (super.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
              sha256 = "jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
            })
            # Add setting to enable rounded window with no decoration (still
            # have to alter default-frame-alist)
            (super.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/round-undecorated-frame.patch";
              sha256 = "uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
            })
            # Make Emacs aware of OS-level light/dark mode
            # https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
            (super.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
              sha256 = "oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
            })
          ];
      })
    else
      # TODO nix's lucid reports the wrong mm-size (breaks textsize package):
      # (frame-monitor-attribute 'mm-size (selected-frame))
      (emacsGitNoctuidGeneric.override {
        withX = true;
        # lucid
        # withGTK2 = false;
        withGTK3 = true;
        withXinput2 = true;
      }).overrideAttrs(_: {
        # for full control/testing (e.g. can't do lucid without cairo using
        # builtin withs)
        configureFlags = [
          # for a (more) reproducible build
          "--disable-build-details"
          "--with-modules"
          "--with-x-toolkit=gtk3"
          "--with-xft"
          "--with-cairo"
          "--with-xaw3d"
          "--with-native-compilation"
          "--with-imagemagick"
          "--with-xinput2"
        ];
      });
  emacsNoctuidWithPackages =
    ((super.emacsPackagesFor emacsNoctuid).emacsWithPackages (epkgs: [
      # necessary to install through nix to get libenchant integration working
      epkgs.jinx
      # not needed on linux but needed on mac
      epkgs.vterm
    ]));

  # for WSL with weston
  emacsPgtk =
    (emacsGitNoctuidGeneric.override {
      # pgtk since wslg uses weston (at least by default)
      withX = false;
      withPgtk = true;
    });
  emacsPgtkWithPackages =
    ((super.emacsPackagesFor emacsPgtk).emacsWithPackages (epkgs: [
      # necessary to install through nix to get libenchant integration working
      epkgs.jinx
      # not needed but prevents need to compile on first run
      epkgs.vterm
    ]));
}
