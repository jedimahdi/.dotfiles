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

    v2raya

    pcmanfm
    xarchiver

    self.packages.${pkgs.system}.ddper
    self.packages.${pkgs.system}.repl
  ];
}
