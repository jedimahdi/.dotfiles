{ pkgs, ... }:
# media - control and enjoy audio/video
{
  imports = [
    ./mpv.nix
    ./imv.nix
  ];

  home.packages = with pkgs; [
    # audio control
    pavucontrol
    pulsemixer
  ];
}
