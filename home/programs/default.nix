{ pkgs, self, ... }: {
  imports = [
    ./browsers/firefox
    ./gtk.nix
    ./zathura.nix
    ./media
    ./langs
  ];

  home.packages = with pkgs; [
    v2raya

    pcmanfm
    xarchiver
    telegram-desktop
    vscode
    peek
    just
    nekoray
    alacritty

    glib

    self.packages.${pkgs.system}.ddper
    self.packages.${pkgs.system}.repl
  ];
}
