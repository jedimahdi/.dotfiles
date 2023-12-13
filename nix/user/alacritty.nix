{ config, pkgs, ... }:

{
  home.packages = [ pkgs.alacritty ];
  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        TERM = "xterm-256color";
      };

      font = {
        normal = {
          family = "Victor Mono Nerd Font";
          style = "SemiBold";
        };
        bold = {
          family = "Victor Mono Nerd Font";
          style = "SemiBold";
        };
        italic = {
          family = "Victor Mono Nerd Font";
          style = "SemiBold";
        };
        size = 15;
        offset.x = 0;
        offset.y = 5;
      };

      window = {
        padding.x = 4;
        padding.y = 4;
        dynamic_padding = true;
      };

      mouse = {
        hide_when_typing = true;
      };

      colors = {
        primary = {
          background = "0x1f2329";
          foreground = "0xabb2bf";
        };
        bright_foreground = "0xe6efff";
        normal = {
          black = "0x1e2127";
          red = "0xe06c75";
          green = "0x98c379";
          yellow = "0xd19a66";
          blue = "0x61afef";
          magenta = "0xc678dd";
          cyan = "0x56b6c2";
          white = "0x828791";
        };
        bright = {
          black = "0x5c6370";
          red = "0xe06c75";
          green = "0x98c379";
          yellow = "0xd19a66";
          blue = "0x61afef";
          magenta = "0xc678dd";
          cyan = "0x56b6c2";
          white = "0xe6efff";
        };
        dim = {
          black = "0x1e2127";
          red = "0xe06c75";
          green = "0x98c379";
          yellow = "0xd19a66";
          blue = "0x61afef";
          magenta = "0xc678dd";
          cyan = "0x56b6c2";
          white = "0x828791";
        };
      };
    };
  };
}
