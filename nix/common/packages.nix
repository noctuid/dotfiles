{ pkgs }:

# TODO specify python11 or version in just one place?

with pkgs; [
# * Filesystem Support
  ntfs3g
  dosfstools
  gvfs

# * Security
  # gnupg
  # TODO issues when different versions of gpg are installed  (mismatched
  # version compared to whatever gpg-agent started first)
  # pass

  pinentry

  passff-host

  keybase
  kbfs

  # for matrix
  # will need to verify all other devices through panctl
  # > list-devices - see available devices
  # > start-verification tab tab <device to verify>
  # confirm on other device
  # > confirm verification <same args>
  pantalaimon

# * VCS
  git
  # for github auth token
  gh

  # to grab specific emacs config files from github
  subversion

# * Terminals
  kitty
  # backup
  xterm
  rxvt-unicode

# * Shell/TUI/CLI
  # TODO prefixed like gls on darwin?
  coreutils

  stow

  zsh
  fish
  powershell

  direnv
  nix-direnv

  tmux

  curl
  wget

  ranger
  # ranger video preview
  ffmpegthumbnailer
  # ranger info (alternatively exiftool)
  mediainfo
  # won't work with kitty installed through system
  # alternate to w3m image preview
  ueberzug

  # for fzf_preview script with fzf-tab
  lesspipe

  # terminal graphics
  chafa

  # trashy and trash-cli conflict; installing trash-cli on the system side
  # - trashy for trashing since "trash-put -- -foo" works but complains that --
  #   does not exist
  # - trash-cli for restore since it is limited to the current directory and
  #   interactive by default; trashy can only do one of those two at a time
  # https://github.com/oberblastmeister/trashy/issues/77
  # trash-cli
  trashy

  # progress bars:
  # https://github.com/DeeNewcum/dotfiles/blob/master/bin/eta
  # http://www.theiling.de/projects/bar.html
  # https://github.com/Xfennec/progress
  # e.g. dd if=$source | pv | dd of=$target
  # supports darwin and linux
  pv

  fasd

  # random/fun
  cowsay
  fortune

  neofetch

  # ls alternatives
  exa
  lsd

  bat
  # TODO install what use
  # bat-extras.*

  delta
  difftastic

  # search/fuzzy finders
  ripgrep
  fzf
  fd

  # format parsing
  jq
  miller

# * Disk Usage
  baobab
  du-dust
  ncdu

# * Backup
  borgbackup
  restic

# * Editors
  # emacsGit with emacs-plus patches on OSX and packages that must be installed
  # through nix
  emacsNoctuidWithPackages
  jansson
  emacsPackages.cask

  neovim
  # opengl
  # neovide
  # TODO fails to build
  # nvimpager

  # backup
  # telemetry disabled by default; some extensions have licenses that
  # only allow use with official vscode, but I don't care since I will ideally
  # never use
  vscodium

# * Browsers
  # NOTE: firefox/chrome are not available for darwin and I wouldn't want to run
  # them through wsl

  # used, for example, by ranger for image preview and html preview
  w3m

# * Video/Images
  # better to use distro package
  # mpv

# * File Type Support and Conversion
  pandoc

  # for pdfunite, pdftotext (ranger preview), etc.
  poppler

  # markdown preview
  nodePackages.livedown
  # TODO vmd?

# * PDF Viewing
  zathura

# * Shared Disk Utilities
  # automatic mounting
  udiskie

# * System Status Monitoring
  htop
  btop

# * Cross-Platform Keyboard Remapping
  # currently need git version on linux; need on system side for wsl
  # kanata

# * Cron
  fcron

# * Language Tools (Spellchecking, Grammar, Dictionary)
  aspell
  aspellDicts.en
  hunspell
  hunspellDicts.en-us
  nuspell
  # no point since need library for emacs (using jinx package to handle):
  # https://nixos.wiki/wiki/FAQ#I_installed_a_library_but_my_compiler_is_not_finding_it._Why.3F
  # enchant
  sdcv

  # grammar
  languagetool
  vale

# * Downloading
  # audio doesn't work when used with mpv? (no audio tracks); may just be
  # out-of-date
  # yt-dlp
  gallery-dl
  aria
  megatools

# * Typesetting
  # every texlive package
  texlive.combined.scheme-full
  # sile

# * Redshift
  redshift

# * Archive Utilities
  unrar
  unzip
  zip
  p7zip
  # frontend for archive extraction, creation, etc.
  atool
  # look inside archives
  avfs

# * Programming
# ** General
  # for syntax highlignt in latex export and elsewhere
  python311Packages.pygments

  editorconfig-core-c

  tree-sitter

  cloc

  # run github actions locally
  act

# ** Docker/Podman
  # docker
  # docker-compose
  # couldn't get this working; nvidia-container-runtime in aur works fine
  # nvidia-docker
  # podman

# ** Licensing
  # TODO harvey

# ** Bash/Shell Scripting
  nodePackages.bash-language-server
  shellcheck

# ** C
  gdb

  # needed for libvterm, for example
  cmake

# ** Clojure
  clojure
  clojure-lsp
  leiningen
  babashka
  # TODO shadow-cljs?

# ** Common Lisp
  sbcl
  roswell
  # TODO phantomjs for cl-minispec

# ** Go
  # mainly have just used for go get
  go

# ** Lua/Fennel
  lua-language-server
  luajitPackages.fennel

# ** Nix
  rnix-lsp
  statix

# ** Python
  # TODO messes with system python/package installation? (e.g. zscroll)
  # python311
  # python311Packages.pip
  # python311Packages.pipx

# *** Project Management
  # TODO installing poetry this way is not officially supported and it seems
  # like poetry plugins will not work (though that seems to also be the case for
  # the distro package?)
  poetry
  hatch
  # required for hatch (check if included)
  # python311Packages.tomli-w
  # python311Packages.hatchling

# *** Python LSP
  pyright

# *** Jupyter
  jupyter
  # python311Packages.ipykernel

# *** Repl
  # python311Packages.ipython
  # python311Packages.ptpython

# *** Debugger
  # python311Packages.ipdb

# *** Linting
  # using project local installation of mypy, black, etc., so none of this is
  # really needed

  ruff
  # TODO ruff-lsp; not packaged

# ** Racket
  # racket

# ** Ruby
  # if need gem
  ruby

# ** SQL
  postgresql
  sqlite

# ** Web - JavaScript, TypeScript, etc.
  nodejs

  yarn
  nodePackages.npm
  nodePackages.pnpm

  nodePackages.typescript
  nodePackages.typescript-language-server

# * Screenshots
  # TODO test on macOS (packaged for darwin)
  maim

]
