{ pkgs, ... }: {
  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    bat-extras.batman
    binutils
    bottom
    brightnessctl
    curlFull
    diffutils
    du-dust # fancy version of `du`
    duf
    entr
    fd
    ffmpeg
    file
    file
    findutils
    gawk
    glib
    gnugrep
    gnused
    graphviz
    jaq
    killall
    libnotify
    lolcat
    mdcat
    navi
    neofetch
    neovim
    nix-du
    nix-tree
    ouch
    pamixer
    pkg-config
    rar
    ripgrep
    tealdeer # rust implementation of `tldr`
    ueberzugpp # Terminal image viewer integration
    unzip
    watch
    wget
    xdg-utils
    zip
  ];

  programs = {
    eza.enable = true;
    aria2.enable = true;
    thefuck.enable = true;
    nix-index.enable = true;
    nix-index-database.comma.enable = true;
    jq.enable = true;
    htop.enable = true;
    less.enable = true;
    zk.enable = true;
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
    fzf = {
      enable = true;
      enableZshIntegration = true;
      colors = {
        "bg+" = "-1";
      };
    };
    bat = {
      enable = true;
      config = {
        theme = "ansi";
        style = "header";
      };
    };
  };
}
