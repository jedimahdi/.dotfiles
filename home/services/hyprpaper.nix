{ pkgs, lib, config, ... }: {
  # home.file.".config/hypr/hyprpaper.conf".text = ''
  #   preload = '' + config.stylix.image + ''
  #
  #   wallpaper = eDP-1,'' + config.stylix.image + ''
  # '';
  systemd.user.services.hyprpaper = {
    Unit = {
      Description = "Hyprland wallpaper daemon";
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${lib.getExe pkgs.hyprpaper}";
      Restart = "on-failure";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
