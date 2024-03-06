{ config, pkgs, ... }:
{
  programs.password-store = {
    enable = true;
    package = pkgs.pass-wayland.withExtensions (exts: [
      exts.pass-update
      exts.pass-import
      exts.pass-otp
    ]);
    settings = {
      PASSWORD_STORE_DIR = "${config.xdg.configHome}/password-store";
    };
  };
}
