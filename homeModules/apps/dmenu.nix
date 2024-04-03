{ pkgs, lib, config, ... }:
let
  cfg = config.apps.dmenu;
  inherit (lib) mkEnableOption mkOption types mkIf;
in
{
  options.apps.dmenu = {
    enable = mkEnableOption "Dmenu";

    command = mkOption {
      type = types.str;
      default = "fuzzle -d";
    };
  };

  config = mkIf cfg.enable {
    home.packages =
      let
        dmenu-clip = pkgs.writeShellScriptBin "dmenu-clip" ''
          copy=$(cliphist list | ${cfg.command} | cliphist decode)
          [[ -n $copy ]] || exit
          wl-copy "$copy"
        '';
        dmenu-pass = pkgs.writeShellScriptBin "dmenu-pass" ''
          shopt -s nullglob globstar

          prefix=''${PASSWORD_STORE_DIR-~/.password-store}
          password_files=( "$prefix"/**/*.gpg )
          password_files=( "''${password_files[@]#"$prefix"/}" )
          password_files=( "''${password_files[@]%.gpg}" )
          password=$(printf '%s\n' "''${password_files[@]}" | ${cfg.command} "$@")
          [[ -n $password ]] || exit

          pass show -c "$password" 2>/dev/null
        '';
      in
      [ dmenu-clip dmenu-pass pkgs.networkmanager_dmenu ];

    home.file.".config/networkmanager-dmenu/config.ini".text =
      ''
        [dmenu]
        dmenu_command = ''
      + cfg.command
      + ''

      compact = True
      wifi_chars = ▂▄▆█
      list_saved = True

      [editor]
      terminal = alacritty
      # gui_if_available = <True or False> (Default: True)
    '';
  };
}
