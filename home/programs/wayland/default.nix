{ pkgs, ... }:
{
  imports = [
    ./hyprland.nix
  ];

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
