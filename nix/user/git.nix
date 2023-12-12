{ config, pkgs, ... }:

{
  home.packages = [ pkgs.git ];
  programs.git.enable = true;
  programs.git.userName = "Mahdi Seyedan";
  programs.git.userEmail = "mahdi.se@yahoo.com";
  programs.git.extraConfig = {
    init.defaultBranch = "main";
  };
}
