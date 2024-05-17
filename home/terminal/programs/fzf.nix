{ config, lib, ... }:
let
  mkFzfKeyValue = lib.generators.mkKeyValueDefault { } ":";

  colorConfig = with config.lib.stylix.colors.withHashtag;
    lib.concatStringsSep "," (lib.mapAttrsToList mkFzfKeyValue {
      "bg" = base00;
      "bg+" = "-1";
      "fg" = base04;
      "fg+" = base06;
      "header" = base0D;
      "hl" = base0D;
      "hl+" = base0D;
      "info" = base0A;
      "marker" = base0C;
      "pointer" = base0C;
      "prompt" = base0A;
      "spinner" = base0C;
    });
in
{
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultOptions = lib.mkAfter [ "--color=${colorConfig}" ];
  };
}
