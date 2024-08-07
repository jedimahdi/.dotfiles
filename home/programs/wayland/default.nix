{ pkgs, ... }:
{
  imports = [
    ./hyprland.nix
    ./fuzzel.nix
  ];

  services.mako = {
    enable = true;
    defaultTimeout = 4 * 1000; # millis
    maxVisible = 3;
  };
  services.cliphist.enable = true;

  home.packages = with pkgs; [
    # screenshot
    grim
    slurp
    swappy
    imagemagick
    (writeShellScriptBin "screenshot" ''
      grim -g "$(slurp)" - | convert - -shave 1x1 PNG:- | wl-copy
    '')
    (writeShellScriptBin "screenshot-edit" ''
      wl-paste | swappy -f -
    '')
    (writeShellScriptBin "screenrec" ''
      wl-screenrec -g "$(slurp)" -f ~/recording_$(date +"%Y-%m-%d_%H:%M:%S.mp4")
    '')
    (writeShellScriptBin "screenrec-stop" ''
      killall -s SIGINT wl-screenrec
    '')

    # utils
    wl-clipboard
    wl-screenrec
    wlr-randr
  ];

  # make stuff work on wayland
  home.sessionVariables = {
    QT_QPA_PLATFORM = "wayland";
    SDL_VIDEODRIVER = "wayland";
    XDG_SESSION_TYPE = "wayland";
  };
}
