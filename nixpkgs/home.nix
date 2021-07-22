{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "mahdi";
  home.homeDirectory = "/home/mahdi";

  # Set nixpkgs options (for home-manager installed packages only).
  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = false;
    allowUnsupportedSystem = false;
  };

  # Documentation!
  programs.man.enable = true;
  programs.info.enable = true;

  home.packages = with pkgs; [
    # Nix
    nixfmt
    niv

    # Basic GNU uitls
    coreutils
    findutils
    diffutils
    binutils
    mailutils
    gawk
    gnumake
    automake
    less
    watch
    wget
    curl
    fd
    file
    gnupg
    glib
    cmake
    libtool

    # CLI
    colordiff
    pkg-config
    bindfs
    direnv
    cloc
    entr
    ripgrep
    pass
    poppler
    youtube-dl
    jq
    tree
    tldr
    act

    # C
    llvm

    lazygit
    feh
    uget
    networkmanager
    networkmanager-openvpn
    openvpn
    appstream
    spotify
    systemd
    mesa
    protonvpn-gui
  ];

  services.lorri.enable = true;

  services.network-manager-applet.enable = true;

  services.dunst = {
    enable = true;
    settings = {
      global = {
        geometry = "300x5-30+20";
        transparency = 10;
        frame_color = "#eceff1";
        font = "Droid Sans 9";
      };

      urgency_normal = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout = 10;
      };
    };
  };

  programs.bat = {
    enable = true;
    config = {
      pager = "less -FR";
      theme = "TwoDark";
      style = "plain";
    };
  };

  programs.git = {
    enable = true;
    userName = "Mahdi Seyedan";
    userEmail = "seyedmahdiseydan78@yahoo.com";
    aliases = {
      ignore = "!gi() { curl -sL https://www.gitignore.io/api/$@ ;}; gi";
    };
    extraConfig = {
      github.user = "jedimahdi";
      credential = {
        helper = "store";
      };
    };
  };

  programs.mpv = {
    enable = true;
    config = {
      autofit-larger = "100%x95%";
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";
}
