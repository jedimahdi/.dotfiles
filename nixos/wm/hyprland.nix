{ pkgs, ... }: {

  programs.hyprland = {
    enable = true;
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
    displayManager.defaultSession = "hyprland";

    xserver.displayManager = {
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
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    GDK_BACKEND = "wayland,x11";
    ANKI_WAYLAND = "1";
    SDL_VIDEODRIVER = "wayland";
    CLUTTER_BACKEND = "wayland";
  };
}
