{ pkgs, dmenu-command, ... }:
let
  dmenu-clip = pkgs.writeShellScriptBin "dmenu-clip" ''
    copy=$(cliphist list | ${dmenu-command} | cliphist decode)
    [[ -n $copy ]] || exit
    wl-copy "$copy"
  '';
in
{
  home.packages = [ dmenu-clip ];
}
