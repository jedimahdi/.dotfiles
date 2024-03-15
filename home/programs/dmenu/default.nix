{ pkgs, dmenu-command ? "fuzzel -d", ... }:
{
  imports = [
    (import ./dmenu-networkmanager.nix {
      inherit pkgs dmenu-command;
    })
    (import ./dmenu-pass.nix {
      inherit pkgs dmenu-command;
    })
    (import ./dmenu-clip.nix {
      inherit pkgs dmenu-command;
    })
  ];
}
