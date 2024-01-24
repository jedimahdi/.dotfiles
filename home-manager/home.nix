{
  config,
  pkgs,
  inputs,
  browser,
  ...
}: {
  imports = [
    inputs.nix-colors.homeManagerModules.default
    inputs.nix-index-database.hmModules.nix-index
    ./sh.nix
    ./git.nix
    ./gtk.nix
    ./mpv.nix
    ./lf.nix
    ./hyprland.nix
    ./${browser}.nix
    ./terminal/alacritty.nix
    ./terminal/foot.nix
    ./imv.nix
    ./zathura.nix
    ./langs/c.nix
    ./langs/haskell.nix
    ./langs/js.nix
    ./langs/rust.nix
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
      alejandra
      statix
      deadnix

      alacritty
      kitty
      dmenu
      v2raya
      qv2ray

      xdg-utils
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

      nodePackages.bash-language-server
      shellcheck
      shfmt
      nodePackages.yaml-language-server
      stylua
      lua-language-server

      (callPackage ../pkgs/ddper {})
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
      "inode/directory" = ["pcmanfm.desktop"];
      "application/zip" = ["xarchiver.desktop"];
      "application/gzip" = ["xarchiver.desktop"];
      "application/x-rar" = ["xarchiver.desktop"];
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
