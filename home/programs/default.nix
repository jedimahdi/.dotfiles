{ pkgs, self, ... }: {
  imports = [
    ./browsers/firefox
    ./gtk.nix
    ./zathura.nix
    ./media
    ./langs
  ];

  home.packages = with pkgs; [
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
    nix-du
    graphviz

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
    navi

    pcmanfm
    xarchiver

    nodePackages.bash-language-server
    shellcheck
    shfmt
    stylua
    lua-language-server

    aspell
    aspellDicts.en

    self.packages.${pkgs.system}.ddper
    self.packages.${pkgs.system}.repl
  ];
}
