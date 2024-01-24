{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    lf
  ];
  programs.lf = {
    enable = true;
    settings = {
      preview = false;
      ratios = [1];
      info = ["size"];
    };
    commands = {
      dragon-out = ''%${pkgs.xdragon}/bin/xdragon -a -x "$fx"'';
      editor-open = ''$$EDITOR $f'';
      mkdir = ''
        ''${{
          printf "Directory Name: "
          read DIR
          mkdir $DIR
        }}
      '';
      download = ''
        ''${{
          printf "URL: "
          read URL
          aria2c $URL
        }}
      '';
    };
    keybindings = {
      "\\\"" = "";
      c = "mkdir";
      "." = "set hidden!";
      "`" = "mark-load";
      "\\'" = "mark-load";
      "<enter>" = "open";
      do = "dragon-out";
      "g~" = "cd";
      gh = "cd";
      "g/" = "/";
      gd = "cd ~/Downloads";
      gs = "cd ~/Downloads/series";
      d = "";
      dw = "download";
      dd = "cut";
      D = "delete";
      C = "clear";
      a = "rename";
      p = "paste";
      r = "reload";
      y = "copy";
      U = "unselect";
      ee = "editor-open";
    };
  };
}
