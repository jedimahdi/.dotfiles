{ pkgs, dmenu-command, ... }:
let
  dmenu-pass = pkgs.writeShellScriptBin "dmenu-pass" ''
    shopt -s nullglob globstar

    prefix=''${PASSWORD_STORE_DIR-~/.password-store}
    password_files=( "$prefix"/**/*.gpg )
    password_files=( "''${password_files[@]#"$prefix"/}" )
    password_files=( "''${password_files[@]%.gpg}" )
    password=$(printf '%s\n' "''${password_files[@]}" | ${dmenu-command} "$@")
    [[ -n $password ]] || exit

    pass show -c "$password" 2>/dev/null
  '';
in
{
  home.packages = [ dmenu-pass ];
}
