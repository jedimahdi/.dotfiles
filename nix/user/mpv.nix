{ config, pkgs, ... }:

{
  programs.mpv = {
    enable = true;
    config = {
      autofit-larger = "100%x95%";
    };
  };

  xdg.mimeApps.defaultApplications =
    {
      "audio/*" = [ "mpv.desktop" ];
      "video/*" = [ "mpv.dekstop" ];
    };
}
