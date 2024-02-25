{ pkgs, inputs, ... }: {

  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
    portalPackage = inputs.hyprland.packages.${pkgs.system}.xdg-desktop-portal-hyprland;
    xwayland.enable = true;
  };

  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal
      # pkgs.xdg-desktop-portal-wlr
      # pkgs.xdg-desktop-portal-gtk
    ];
  };
  services = {
    xserver.displayManager = {
      defaultSession = "hyprland";
      lightdm.enable = true;
      # gdm = {
      #   enable = true;
      #   wayland = true;
      # };
    };

    gnome.gnome-keyring.enable = true;
    xserver = {
      enable = true;
      # videoDrivers = [ "amdgpu" ];
      autoRepeatDelay = 300;
      autoRepeatInterval = 50;
      xkb = {
        layout = "us";
        variant = "";
      };
    };
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };
  };

  environment.sessionVariables = {
    WLR_NO_HARDWARE_CURSORS = "1";
    NIXOS_OZONE_WL = "1";
  };
}
