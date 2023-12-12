{ config, pkgs, ... }:

{
  programs.lf = {
    enable = true;
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
      d = "";
      dd = "cut";
      D = "delete";
      C = "clear";
      a = "rename";
      p = "paste";
      r = "reload";
      y = "copy";
      U = "unselect";
    };
  };
}
