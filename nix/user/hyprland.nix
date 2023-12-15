{ config, pkgs, lib, ... }:

{
  imports = [
    (import ./dmenu.nix {
      dmenu_command = "fuzzel -d";
      inherit config lib pkgs;
    })
  ];

  home.packages = with pkgs; [
    wl-clipboard
    slurp
    grim
    pavucontrol
    pamixer
    fuzzel
    swww

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
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    plugins = [ ];
    settings = { };
    extraConfig = ''
      exec-once = hyprctl setcursor ${config.gtk.cursorTheme.name} ${builtins.toString config.gtk.cursorTheme.size}
      exec-once = swww init && swww img "~/Pictures/wallpaper.png"

      monitor=,preferred,auto,1
      input {
          kb_layout = us
          kb_options = caps:escape
          repeat_delay = 300
          repeat_rate = 50
          follow_mouse = 2
      }
      general {
          layout = master
          gaps_in = 5
          gaps_out = 5
          border_size = 2
          col.active_border = rgba(33ccffee) rgba(00ff99ee) 45deg
          col.inactive_border = rgba(595959aa)
      }
      decoration {
          rounding = 0
          blur {
              enabled = true
              size = 5
              passes = 2
              ignore_opacity = true
              contrast = 1.17
              brightness = 0.8
          }
      }
      master {
          new_is_master = true
          no_gaps_when_only = 1
      }
      binds {
          allow_workspace_cycles = yes
      }
      misc {
          disable_hyprland_logo = true
          disable_splash_rendering = true
      }
      animations {
          enabled = no
      }
      $mainMod = SUPER
      bind = $mainMod, Return, exec, alacritty
      bind = $mainMod, Q, killactive,
      bind = $mainMod, M, exit,
      bind = $mainMod, E, exec, pcmanfm
      bind = $mainMod, T, togglefloating,
      bind = $mainMod, D, exec, rofi -show drun
      bind = $mainMod, I, exec, networkmanager_dmenu

      bind = $mainMod, J, layoutmsg, cyclenext
      bind = $mainMod, K, layoutmsg, cycleprev
      binde = $mainMod, L, resizeactive, 10 0
      binde = $mainMod, H, resizeactive, -10 0

      bind = $mainMod,Tab,workspace,previous

      bind = $mainMod, 1, workspace, 1
      bind = $mainMod, 2, workspace, 2
      bind = $mainMod, 3, workspace, 3
      bind = $mainMod, 4, workspace, 4
      bind = $mainMod, 5, workspace, 5
      bind = $mainMod, 6, workspace, 6
      bind = $mainMod, 7, workspace, 7
      bind = $mainMod, 8, workspace, 8
      bind = $mainMod, 9, workspace, 9
      bind = $mainMod, 0, workspace, 10

      bind = $mainMod SHIFT, 1, movetoworkspace, 1
      bind = $mainMod SHIFT, 2, movetoworkspace, 2
      bind = $mainMod SHIFT, 3, movetoworkspace, 3
      bind = $mainMod SHIFT, 4, movetoworkspace, 4
      bind = $mainMod SHIFT, 5, movetoworkspace, 5
      bind = $mainMod SHIFT, 6, movetoworkspace, 6
      bind = $mainMod SHIFT, 7, movetoworkspace, 7
      bind = $mainMod SHIFT, 8, movetoworkspace, 8
      bind = $mainMod SHIFT, 9, movetoworkspace, 9
      bind = $mainMod SHIFT, 0, movetoworkspace, 10

      bind = $mainMod, S, togglespecialworkspace, magic
      bind = $mainMod SHIFT, S, movetoworkspace, special:magic

      bindm = $mainMod, mouse:272, movewindow
      bindm = $mainMod, mouse:273, resizewindow

      binde=,code:122,exec,pamixer -d 10
      binde=,code:123,exec,pamixer -i 10
      bind=,code:121,exec,pamixer -t
      bind=,code:232,exec,brightnessctl set 15-
      bind=,code:233,exec,brightnessctl set +15

      windowrulev2 = opacity 0.90 0.90,class:^(Alacritty)$
    '';
    xwayland = { enable = true; };
  };
}
