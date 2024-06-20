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
    telegram-desktop
    foliate
    vscode
    zed-editor
    peek
    just
    zulip
    nekoray
    brave
    alacritty

    glib

    self.packages.${pkgs.system}.ddper
    self.packages.${pkgs.system}.repl
  ];
}
