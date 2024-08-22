{ pkgs, ... }: {

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  # xdg.portal = {
  #   enable = true;
  #   extraPortals = [
  #     pkgs.xdg-desktop-portal
  #     # pkgs.xdg-desktop-portal-wlr
  #     # pkgs.xdg-desktop-portal-gtk
  #   ];
  # };

  environment.sessionVariables = {
    # NIXOS_OZONE_WL = "1";
    # MOZ_ENABLE_WAYLAND = "1";
    # GDK_BACKEND = "wayland,x11";
    # ANKI_WAYLAND = "1";
    # SDL_VIDEODRIVER = "wayland";
    CLUTTER_BACKEND = "wayland";
  };

  environment = {
    systemPackages = with pkgs; [
      qt5.qtwayland
    ];
  };
}
