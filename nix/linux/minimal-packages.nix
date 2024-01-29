{ pkgs }:

# the amount of trouble I've had with using nix packages, this exists as
# fallback that only includes packages that meet one of the following criteria:

# - any packages not in AUR
# - some packages in AUR if don't need latest version
# - emacs (for patches, exact control over which version, etc.)

# TODO common packages are probably safe enough to include here
# remove graphical packages from common? currently only zathura maybe pinentry

with pkgs; [
  # if don't include common
  emacsNoctuidWithPackages

# * Torrents
  nodePackages.webtorrent-cli

# * Mouse Tools
  # prefer warpd over keynav though don't really use
  # it's too old
  # warpd

# * Sound/Music
  jamesdsp

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
