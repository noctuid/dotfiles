# my custom Emacs
# use emacs-plus patches on osx
# (eventually) use lucid on linux

# relevant links:
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/emacs/generic.nix
# https://github.com/nix-community/emacs-overlay/blob/master/overlays/emacs.nix
# https://github.com/d12frosted/homebrew-emacs-plus/tree/master/patches/emacs-30

final: prev: {
  emacsNoctuid =
    if prev.stdenv.isDarwin
    then
      prev.emacsGit.overrideAttrs (old: {
        patches =
          (old.patches or [])
          ++ [
            # Fix OS window role so that yabai can pick up emacs
            (prev.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30fix-window-role.patch";
              sha256 = "+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
            })
            # Use poll instead of select to get file descriptors
            (prev.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/poll.patch";
              sha256 = "jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
            })
            # Enable rounded window with no decoration
            (prev.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/round-undecorated-frame.patch";
              sha256 = "qPenMhtRGtL9a0BvGnPF4G1+2AJ1Qylgn/lUM8J2CVI=";
            })
            # Make emacs aware of OS-level light/dark mode
            (prev.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/system-appearance.patch";
              sha256 = "oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
            })
          ];
      })
    else
      # TODO nix's lucid reports the wrong mm-size (breaks textsize package):
      # (frame-monitor-attribute 'mm-size (selected-frame))
      (prev.emacsGit.override {
        withSQLite3 = true;
        withWebP = true;
        withX = true;
        # lucid
        # withGTK2 = false;
        withGTK3 = true;
        withImageMagick = true;
        # have to force these; lib.version check wrong or because emacsGit?
        withTreeSitter = true;
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
}
