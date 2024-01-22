{ config, pkgs, inputs, browser, ... }:

{
  imports = [
    inputs.nix-colors.homeManagerModules.default
    inputs.nix-index-database.hmModules.nix-index
    ./user/sh.nix
    ./user/git.nix
    ./user/gtk.nix
    ./user/mpv.nix
    ./user/lf.nix
    ./user/hyprland.nix
    ./user/${browser}.nix
    ./user/terminal/alacritty.nix
    ./user/terminal/foot.nix
    ./user/imv.nix
    ./user/zathura.nix
    ./user/langs/c.nix
    ./user/langs/haskell.nix
    ./user/langs/js.nix
    ./user/langs/rust.nix
  ];

  nixpkgs.config.allowUnfree = true;

  programs.home-manager.enable = true;
  home = {
    stateVersion = "23.11";
    username = "mahdi";
    homeDirectory = "/home/mahdi";

    packages = with pkgs; [
      cachix
      patchelf
      nix-prefetch-git
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
      qv2ray

      xdg-utils
      zlib.dev
      curlFull
      binutils
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
      ouch

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
      thefuck
      ytfzf

      pcmanfm
      xarchiver
      font-manager

      nodePackages.bash-language-server
      shellcheck
      shfmt
      nodePackages.yaml-language-server
      stylua
      lua-language-server

      (callPackage ./ddper.nix { })
      brave
      osu-lazer-bin
    ];

    sessionVariables = {
      EDITOR = "nvim";
      BROWSER = browser;
    };
  };

  colorScheme = inputs.nix-colors.lib.schemeFromYAML "onedarker" (builtins.readFile ./onedarker.yaml);

  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
      download = "${config.home.homeDirectory}/Downloads";
    };
    mime.enable = true;
    mimeApps.enable = true;
    mimeApps.defaultApplications = {
      "inode/directory" = [ "pcmanfm.desktop" ];
      "application/zip" = [ "xarchiver.desktop" ];
      "application/gzip" = [ "xarchiver.desktop" ];
      "application/x-rar" = [ "xarchiver.desktop" ];
    };
  };

  programs = {
    aria2.enable = true;
    thefuck.enable = true;
    nix-index.enable = true;
    nix-index-database.comma.enable = true;
  };

  manual = {
    html.enable = false;
    json.enable = false;
    manpages.enable = true;
  };
}
