{ config, pkgs, lib, ... }:
let
  startupScript = pkgs.writeShellScriptBin "start" ''
    ${pkgs.feh}/bin/feh --bg-scale ${./wallpaper.png}
    $HOME/.dotfiles/bin/remaps
  '';
in
{
  imports = [
    (import ./dmenu.nix {
      dmenu_command = "dmenu";
      inherit config lib pkgs;
    })
  ];

  home.packages = with pkgs; [
    xclip
    feh
    startupScript
  ];

  services.picom = {
    enable = true;
    backend = "glx";
    opacityRules = [
      "85:class_g = 'Alacritty'"
      "85:class_g = 'kitty'"
    ];
    settings = {
      blur = {
        method = "dual_kawase";
        kern = "3x3box";
        strength = 5;
        background = false;
        background-frame = false;
        background-fixed = false;
      };
    };
  };
}
