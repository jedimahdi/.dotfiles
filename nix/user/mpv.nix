{ config, pkgs, ... }:

{
  programs.mpv = {
    enable = true;
    config = {
      autofit-larger = "100%x95%";
    };
  };
}
