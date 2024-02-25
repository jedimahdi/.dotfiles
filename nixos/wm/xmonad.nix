{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    xorg.xdpyinfo
    xorg.xrandr
    autorandr
    dmenu
    xcape
    acpi
  ];
  services = {
    xserver = {
      enable = true;
      autorun = true;
      xkb.layout = "us,ir";
      autoRepeatDelay = 300;
      autoRepeatInterval = 50;
      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+xmonad";
      };
    };

    xserver.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = builtins.readFile ./xmonad.hs;
    };
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };
    autorandr.enable = true;
    gnome.gnome-keyring.enable = true;
  };
}
