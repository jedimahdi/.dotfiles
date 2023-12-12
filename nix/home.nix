{ config, pkgs, browser, ... }:

{
  programs.home-manager.enable = true;
  home.username = "mahdi";
  home.homeDirectory = "/home/mahdi";

  imports = [
    ./user/sh.nix
    ./user/git.nix
    ./user/gtk.nix
    ./user/mpv.nix
    ./user/lf.nix
    ./user/hyprland.nix
    ./user/${browser}.nix
  ];

  home.stateVersion = "23.05";

  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    (pkgs.nerdfonts.override { fonts = [ "VictorMono" ]; })

    cachix
    nix-prefetch-git
    nixpkgs-fmt

    alacritty
    kitty
    dmenu
    v2raya

    gcc
    automake
    cmake
    curlFull
    diffutils
    fd
    file
    findutils
    gawk
    glib
    gnumake
    gnupg
    less
    libtool
    mailutils
    watch
    wget
    htop

    tldr
    git
    ripgrep
    entr
    pkg-config
    eza
    zoxide
    tmux
    fzf

    pamixer

    pcmanfm
  ];

  xdg.enable = true;
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    download = "${config.home.homeDirectory}/Downloads";
  };
  xdg.mime.enable = true;
  xdg.mimeApps.enable = true;


  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/mahdi/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = browser;
  };

  # Let Home Manager install and manage itself.
}
