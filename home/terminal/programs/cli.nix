{ pkgs, ... }: {
  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    binutils
    brightnessctl
    curlFull
    diffutils
    du-dust # fancy version of `du`
    duf
    entr
    fd
    ffmpeg
    figlet
    file
    file
    findutils
    gawk
    glib
    glow
    gnugrep
    gnused
    graphviz
    jaq
    killall
    libnotify
    lolcat
    mdcat
    navi
    neovim
    nh
    nix-du
    nix-tree
    ouch
    pamixer
    pkg-config
    rar
    ripgrep
    skim
    tealdeer # rust implementation of `tldr`
    ueberzugpp # Terminal image viewer integration
    unzip
    watch
    wget
    xdg-utils
    zip
  ];

  programs = {
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
  };
}
