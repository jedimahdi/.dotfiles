{ config, pkgs, lib, ... }: {
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

  wayland.windowManager.hyprland =
    let
      startupScript = pkgs.writeShellScriptBin "start" ''
        hyprctl setcursor ${config.gtk.cursorTheme.name} ${builtins.toString config.gtk.cursorTheme.size}

        ${pkgs.swww}/bin/swww init &
        sleep 1
        ${pkgs.swww}/bin/swww img ${./wallpaper.png} &
      '';
    in
    {
      enable = true;
      plugins = [ ];
      settings = {
        exec-once = ''${startupScript}/bin/start'';
        monitor = ",preferred,auto,1";
        input = {
          kb_layout = "us,ir";
          kb_options = "caps:escape";
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
        };
        animations = {
          enabled = "no";
        };
        "$mod" = "SUPER";
        bind = [
          "$mod, Return, exec, alacritty"
          "$mod, E, exec, pcmanfm"
          "$mod, Z, exec, firefox"
          "$mod, Q, killactive,"
          "$mod, M, exit,"
          "$mod, D, exec, fuzzel"
          "$mod, I, exec, networkmanager_dmenu"
          "$mod, C, exec, cliphist list | fuzzel -d | cliphist decode | wl-copy"
          "$mod, S, exec, screenshot"
          "$mod SHIFT, S, exec, screenshot-edit"
          ", code:121, exec, pamixer -t"
          "$mod, Tab, workspace, previous"
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
        ];
        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];
        binde = [
          "$mod, L, resizeactive, 10 0"
          "$mod, H, resizeactive, -10 0"
          "$mod, code:122, exec, brightnessctl set 15-"
          "$mod, code:123, exec, brightnessctl set +15"
          ", code:122, exec, pamixer -d 10"
          ", code:123, exec, pamixer -i 10"
        ];
        windowrule = [
          "tile,^(firefox)$"

        ];
        windowrulev2 = [
          "opacity 0.80 0.80,class:^(Alacritty)$"
          "opacity 0.80 0.80,class:^(foot)$"
          "opacity 0.80 0.80,class:^(footclient)$"
          "opacity 0.80 0.80,class:^(kitty)$"
        ];
      };
      xwayland = { enable = true; };
    };

  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        width = 60;
      };
      border = {
        width = 2;
        radius = 7;
      };
    };
  };
  services.mako = {
    enable = true;
    defaultTimeout = 4 * 1000; # millis
    maxVisible = 3;
  };
  services.cliphist.enable = true;
}
