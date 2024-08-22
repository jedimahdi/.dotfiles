{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs; [
      dmenu
      xcape
    ];
  };

  services = {
    xserver.displayManager = {
      lightdm.enable = true;
    };

    xserver = {
      windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs {
          src = ./dwm;
        };
      };
    };
  };
}
