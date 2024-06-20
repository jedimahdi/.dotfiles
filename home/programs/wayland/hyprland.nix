{ config, pkgs, ... }:
let
  dmenu-command = "fuzzel -d";
  dmenu-run = "fuzzel";
in
{
  imports = [
    (import ../dmenu {
      inherit pkgs dmenu-command;
    })
  ];

  wayland.windowManager.hyprland =
    let
      startupScript = pkgs.writeShellScriptBin "start" ''
        hyprctl setcursor ${config.gtk.cursorTheme.name} ${builtins.toString config.gtk.cursorTheme.size}
      '';
    in
    {
      enable = true;
      # plugins = [ ];
      settings = {
        # exec-once = ''${startupScript}/bin/start'';
        monitor = ",preferred,auto,1";
        env = [
          "NIXOS_OZONE_WL,1" # for any ozone-based browser & electron apps to run on wayland
          "MOZ_ENABLE_WAYLAND,1" # for firefox to run on wayland
          "MOZ_WEBRENDER,1"
          # misc
          "_JAVA_AWT_WM_NONREPARENTING,1"
          "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
          "QT_QPA_PLATFORM,wayland"
          "SDL_VIDEODRIVER,wayland"
          "GDK_BACKEND,wayland"
        ];
        input = {
          kb_layout = "us,ir";
          kb_options = "caps:escape,grp:alt_shift_toggle";
          repeat_delay = 300;
          repeat_rate = 50;
          follow_mouse = 1;
        };
        general = {
          layout = "master";
          gaps_in = 2;
          gaps_out = 0;
          border_size = 0;
          # "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
          # "col.inactive_border" = "rgba(595959aa)";
        };
        decoration = {
          rounding = 0;
          drop_shadow = false;
          blur = {
            enabled = true;
            size = 5;
            passes = 2;
            ignore_opacity = true;
            contrast = 1.17;
            brightness = 0.8;
          };
        };
        master = {
          new_is_master = true;
          no_gaps_when_only = 1;
        };
        binds = {
          allow_workspace_cycles = "yes";
        };
        misc = {
          disable_hyprland_logo = true;
          disable_splash_rendering = true;
          force_default_wallpaper = false;
        };
        animations = {
          enabled = "no";
        };
        "$mod" = "SUPER";
        bind = [
          "$mod, Return, exec, run-as-service alacritty"
          "$mod, E, exec, pcmanfm"
          "$mod, Z, exec, firefox"
          "$mod, P, exec, dmenu-pass"
          "$mod, Q, killactive,"
          "$mod, M, exit,"
          "$mod, D, exec, ${dmenu-run}"
          "$mod, I, exec, networkmanager_dmenu"
          "$mod, C, exec, dmenu-clip"
          "$mod, S, exec, screenshot"
          "$mod SHIFT, S, exec, screenshot-edit"
          "$mod, R, exec, screenrec"
          "$mod SHIFT, R, exec, screenrec-stop"
          ", code:121, exec, pamixer -t"
          "$mod, Tab, workspace, previous"
          "$mod, F, fullscreen"
          "$mod, T, togglefloating,"
          "$mod, J, layoutmsg, cyclenext"
          "$mod, K, layoutmsg, cycleprev"
          "$mod SHIFT, J, layoutmsg, swapnext"
          "$mod SHIFT, K, layoutmsg, swapprev"
          "$mod, 1, workspace, 1"
          "$mod, 2, workspace, 2"
          "$mod, 3, workspace, 3"
          "$mod, 4, workspace, 4"
          "$mod, 5, workspace, 5"
          "$mod, 6, workspace, 6"
          "$mod, 7, workspace, 7"
          "$mod, 8, workspace, 8"
          "$mod, 9, workspace, 9"
          "$mod, 0, workspace, 10"
          "$mod SHIFT, 1, movetoworkspace, 1"
          "$mod SHIFT, 2, movetoworkspace, 2"
          "$mod SHIFT, 3, movetoworkspace, 3"
          "$mod SHIFT, 4, movetoworkspace, 4"
          "$mod SHIFT, 5, movetoworkspace, 5"
          "$mod SHIFT, 6, movetoworkspace, 6"
          "$mod SHIFT, 7, movetoworkspace, 7"
          "$mod SHIFT, 8, movetoworkspace, 8"
          "$mod SHIFT, 9, movetoworkspace, 9"
          "$mod SHIFT, 0, movetoworkspace, 10"

          ", XF86AudioNext , exec , ${pkgs.playerctl}/bin/playerctl next"
          ", XF86AudioPrev , exec , ${pkgs.playerctl}/bin/playerctl previous"
          ", XF86AudioPlay , exec , ${pkgs.playerctl}/bin/playerctl play-pause"
          ", XF86AudioPause , exec , ${pkgs.playerctl}/bin/playerctl pause"
          ", XF86AudioStop , exec , ${pkgs.playerctl}/bin/playerctl stop"
        ];
        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];
        binde = [
          "$mod, L, resizeactive, 10 0"
          "$mod, H, resizeactive, -10 0"

          ", XF86AudioRaiseVolume , exec , ${pkgs.alsa-utils}/bin/amixer -q set Master 5%+"
          ", XF86AudioLowerVolume , exec , ${pkgs.alsa-utils}/bin/amixer -q set Master 5%-"
          ", XF86MonBrightnessUp , exec , ${pkgs.brightnessctl}/bin/brightnessctl set 5%+"
          ", XF86MonBrightnessDown , exec , ${pkgs.brightnessctl}/bin/brightnessctl set 5%-"
        ];
        windowrule = [
          "tile,^(firefox)$"
        ];
        windowrulev2 = [
          "opacity 0.80 0.80,class:^(Alacritty)$"
          "opacity 0.80 0.80,class:^(foot)$"
          "opacity 0.80 0.80,class:^(footclient)$"
          "opacity 0.80 0.80,class:^(kitty)$"

          # telegram media viewer
          "float, title:^(Media viewer)$"

          "idleinhibit focus, class:^(mpv)$"

          "noblur, class:^(pcmanfm)$"
          "noshadow, class:^(pcmanfm)$"

          # firefox
          "idleinhibit fullscreen, class:^(firefox)$"
          "float,title:^(Firefox — Sharing Indicator)$"
          "move 0 0,title:^(Firefox — Sharing Indicator)$"
          "float, title:^(Picture-in-Picture)$"
          "pin, title:^(Picture-in-Picture)$"
          "workspace 3, class:^(firefox)$"
          "noblur, class:^(firefox)$"
          "noshadow, class:^(firefox)$"

          # pavucontrol
          "float,class:^(pavucontrol)$"
          "size 70% 70%,class:^(pavucontrol)$"
          "center,,class:^(pavucontrol)$"
          "opacity 0.80,class:^(pavucontrol)$"
          "float,title:^(Volume Control)$"
          "size 70% 70%,title:^(Volume Control)$"
          "center,title:^(Volume Control)$"
          "float, class:^(imv)$"
        ];
      };
    };
}
