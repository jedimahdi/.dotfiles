{ config, pkgs, inputs, browser, wm, ... }: {
  imports = [
    inputs.stylix.homeManagerModules.stylix
    inputs.nix-index-database.hmModules.nix-index
    ./stylix.nix
    ./sh.nix
    ./tmux
    ./git.nix
    ./gtk.nix
    ./mpv.nix
    ./lf.nix
    ./${wm}.nix
    ./${browser}.nix
    ./terminal/alacritty.nix
    ./terminal/foot.nix
    ./terminal/kitty.nix
    ./imv.nix
    ./zathura.nix
    ./langs/c.nix
    ./langs/haskell.nix
    ./langs/js.nix
    ./langs/rust.nix
    ./langs/go.nix
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
      nix-tree
      manix

      v2raya

      xdg-utils
      curlFull
      binutils
      diffutils
      file
      findutils
      gawk
      glib
      gnupg
      watch
      wget
      ffmpeg
      rar
      unzip
      zip
      ouch

      tealdeer # rust implementation of `tldr`
      du-dust # fancy version of `du`
      ripgrep
      entr
      pkg-config
      eza
      fzf
      pamixer
      thefuck

      pcmanfm
      xarchiver

      nodePackages.bash-language-server
      shellcheck
      shfmt
      stylua
      lua-language-server

      aspell
      aspellDicts.en

      brave

      (callPackage ../pkgs/ddper { })
    ];

    sessionVariables = {
      EDITOR = "nvim";
      BROWSER = browser;
    };
  };

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
