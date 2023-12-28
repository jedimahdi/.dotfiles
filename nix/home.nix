{ config, pkgs, inputs, lib, browser, ... }:

{
  programs.home-manager.enable = true;
  home.username = "mahdi";
  home.homeDirectory = "/home/mahdi";

  imports = [
    inputs.nix-colors.homeManagerModules.default
    ./user/sh.nix
    ./user/git.nix
    ./user/gtk.nix
    ./user/mpv.nix
    ./user/lf.nix
    ./user/hyprland.nix
    ./user/${browser}.nix
    ./user/alacritty.nix
    ./user/imv.nix
    ./user/zathura.nix
    ./user/langs/c.nix
    ./user/langs/haskell.nix
    ./user/langs/js.nix
    ./user/langs/rust.nix
  ];

  home.stateVersion = "23.05";

  colorScheme = inputs.nix-colors.lib.schemeFromYAML "onedarker" (builtins.readFile ./onedarker.yaml);

  home.packages = with pkgs; [
    cachix
    patchelf
    nix-prefetch-git
    nix-index
    nil
    nixpkgs-fmt
    statix
    manix
    (writeShellScriptBin "fmanix" ''
      manix "" | grep '^# ' | sed 's/^# \(.*\) (.*/\1/;s/ (.*//;s/^# //' | fzf-tmux -p 80% --preview="manix '{}'" | xargs manix
    '')
    deadnix

    alacritty
    kitty
    dmenu
    v2raya

    xdg-utils
    zlib.dev
    curlFull
    diffutils
    fd
    file
    findutils
    gawk
    glib
    gnupg
    less
    mailutils
    watch
    wget
    htop
    ffmpeg
    rar
    unzip
    zip

    tldr
    git
    ripgrep
    entr
    pkg-config
    eza
    zoxide
    tmux
    fzf
    jq
    pamixer

    pcmanfm
    xarchiver
    font-manager
    atlauncher

    nodePackages.bash-language-server
    shfmt
    nodePackages.yaml-language-server
    stylua
    lua-language-server

    (callPackage ./ddper.nix { })
    brave
    osu-lazer-bin
  ];

  xdg.enable = true;
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    download = "${config.home.homeDirectory}/Downloads";
  };
  xdg.mime.enable = true;
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "inode/directory" = [ "pcmanfm.desktop" ];
    "application/epub+zip" = [ "xarchiver.desktop" ];
    "application/zip" = [ "xarchiver.desktop" ];
    "application/gzip" = [ "xarchiver.desktop" ];
    "application/x-rar" = [ "xarchiver.desktop" ];
  };

  programs.aria2.enable = true;

  home.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = browser;
  };
}
