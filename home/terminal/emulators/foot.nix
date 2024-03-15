_:
# let
#   colors = {
#     foreground = "abb2bf";
#     background = "1f2329";
#     selection-foreground = "c8c093";
#     selection-background = "2d4f67";
#     regular0 = "1e2127";
#     regular1 = "e06c75";
#     regular2 = "98c379";
#     regular3 = "d19a66";
#     regular4 = "61afef";
#     regular5 = "c678dd";
#     regular6 = "56b6c2";
#     regular7 = "828791";
#     bright0 = "5c6370";
#     bright1 = "e06c75";
#     bright2 = "98c379";
#     bright3 = "d19a66";
#     bright4 = "61afef";
#     bright5 = "c678dd";
#     bright6 = "56b6c2";
#     bright7 = "e6efff";
#     dim0 = "0x1e2127";
#     dim1 = "0xe06c75";
#     dim2 = "0x98c379";
#     dim3 = "0xd19a66";
#     dim4 = "0x61afef";
#     dim5 = "0xc678dd";
#     dim6 = "0x56b6c2";
#     dim7 = "0x828791";
#     "16" = "ffa066";
#     "17" = "ff5d62";
#   };
# in
{
  programs.foot = {
    enable = true;
    # server.enable = true;

    settings = {
      main = {
        term = "xterm-256color";
        # font = "VictorMono NF SemiBold:size=15";
        line-height = "38px";
        box-drawings-uses-font-glyphs = "yes";
        pad = "4x4 center";
        notify = "notify-send -a \${app-id} -i \${app-id} \${title} \${body}";
        selection-target = "clipboard";
      };

      scrollback = {
        lines = 10000;
        multiplier = 3;
      };

      text-bindings = {
        "\\x0d" = "Control+Return Shift+Return Control+Shift+Return";
      };

      url = {
        launch = "xdg-open \${url}";
        label-letters = "sadfjklewcmpgh";
        osc8-underline = "url-mode";
        protocols = "http, https, ftp, ftps, file";
        uri-characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.,~:;/?#@!$&%*+=\"'()[]";
      };

      cursor = {
        # style = "beam";
        # beam-thickness = 1;
      };
      # inherit colors;
    };
  };
}
