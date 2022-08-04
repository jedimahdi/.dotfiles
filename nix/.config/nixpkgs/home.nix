{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "mahdi";
  home.homeDirectory = "/home/mahdi";

  # fixing local issues, settings XDG_DATA_DIRS, etc
  targets.genericLinux.enable = true;

  home.sessionVariables = {
    LANG = "en_US.UTF-8";
    LC_CTYPE = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    EDITOR = "nvim";
    PAGER = "less -FirSwX";
    MANPAGER = "less -FirSwX";
  };

  # Set nixpkgs options (for home-manager installed packages only).
  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = false;
    allowUnsupportedSystem = false;
  };

  # Documentation!
  programs.man.enable = false;
  programs.info.enable = false;

  home.packages = with pkgs; [
    # Nix
    nixfmt
    niv
    cachix

    # CLI
    # direnv
    tree
    act

    # lazygit
    # spotify
  ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  # home.file.".tmux2.conf" = {
  #  text = ''
  #  set-option -g default-shell /run/current-system/sw/bin/fish
  #  set-window-option -g mode-keys vi
  #  set -g default-terminal "screen-256color"
  #  set -ga terminal-overrides ',screen-256color:Tc'
  #  '';
  # };

  #  services.lorri.enable = true;

  # services.dunst = {
  #   enable = true;
  #   settings = {
  #     global = {
  #       geometry = "300x5-30+20";
  #       transparency = 10;
  #       frame_color = "#eceff1";
  #       font = "Droid Sans 9";
  #     };
  #
  #      urgency_normal = {
  #        background = "#37474f";
  #        foreground = "#eceff1";
  #        timeout = 10;
  #      };
  #    };
  #  };

  programs.bat = {
    enable = true;
    config = {
      pager = "less -FR";
      theme = "TwoDark";
      style = "plain";
    };
  };

  # programs.git = {
  # enable = true;
  # userName = "Mahdi Seyedan";
  # userEmail = "seyedmahdiseydan78@yahoo.com";
  # aliases = {
  # ignore = "!gi() { curl -sL https://www.gitignore.io/api/$@ ;}; gi";
  # };
  # extraConfig = {
  # github.user = "jedimahdi";
  # credential = {
  # helper = "store";
  # };
  # };
  # };

  programs.mpv = {
    enable = true;
    config = {
      autofit-larger = "100%x95%";
    };
  };

  home.stateVersion = "22.05";
}
