{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    imv # simple image viewer
  ];

  xdg.mimeApps.defaultApplications =
    {
      "image/gif" = [ "imv.desktop" ];
      "image/jpeg" = [ "imv.desktop" ];
      "image/jpg" = [ "imv.desktop" ];
      "image/png" = [ "imv.desktop" ];
      "image/webp" = [ "imv.desktop" ];
    };
}
