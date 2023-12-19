{ config, pkgs, inputs, browser, ... }:

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
    ./user/qutebrowser.nix
    ./user/alacritty.nix
    ./user/imv.nix
    ./user/zathura.nix
  ];

  home.stateVersion = "23.05";

  colorScheme = inputs.nix-colors.lib.schemeFromYAML "onedarker" (builtins.readFile ./onedarker.yaml);

  # fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
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
    jq

    pamixer
    patchelf
    nix-index

    pcmanfm
    font-manager

    nodejs-18_x
    typescript
    nodePackages.typescript-language-server
    nodePackages.prettier
    nodePackages.bash-language-server
    shfmt
    nodePackages.vscode-langservers-extracted
    nodePackages.yaml-language-server
    stylua
    lua-language-server
    clang-tools

    (callPackage ./ddper.nix { })
  ];

  xdg.enable = true;
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    download = "${config.home.homeDirectory}/Downloads";
  };
  xdg.mime.enable = true;
  xdg.mimeApps.enable = true;

  programs.aria2.enable = true;

  home.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = browser;
  };
}
