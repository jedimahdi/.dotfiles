{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "mahdi";
  home.homeDirectory = "/home/mahdi";

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

    # CLI
    # direnv
    tree
    act

    lazygit
    spotify
  ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  # optional for nix flakes support
  programs.direnv.nix-direnv.enableFlakes = true;

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
