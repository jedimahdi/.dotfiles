{ pkgs, wm, ... }:
{
  stylix = {
    autoEnable = false;
    base16Scheme = ./onedarker.yaml;
    image = ./wallpaper.png;
    cursor = {
      name = "phinger-cursors-light";
      package = pkgs.phinger-cursors;
      size = 32;
    };
    fonts = {
      sansSerif = {
        package = pkgs.noto-fonts;
        name = "Noto Sans";
      };
      serif = {
        package = pkgs.noto-fonts;
        name = "Noto Sans";
      };
      monospace = {
        package = pkgs.nerdfonts.override { fonts = [ "VictorMono" ]; };
        name = "VictorMono NF SemiBold";
      };
      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
      sizes =
        if wm == "hyprland" then {
          desktop = 14;
          terminal = 18;
          applications = 14;
          popups = 22;
        } else {
          desktop = 14;
          terminal = 13;
          applications = 14;
          popups = 16;
        };
    };

    targets = {
      fuzzel.enable = true;
      zathura.enable = true;
      hyprland.enable = true;
      gtk.enable = true;
      bat.enable = true;
      foot.enable = true;
      alacritty.enable = true;
      kitty.enable = true;
      mako.enable = true;
      fzf.enable = true;
      xresources.enable = true;
      firefox.enable = true;
      tmux.enable = true;
      yazi.enable = true;
      zellij.enable = true;
    };
  };
}
