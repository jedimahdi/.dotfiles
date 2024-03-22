{ config, pkgs, ... }:
{
  programs.gpg = {
    enable = true;
    homedir = "${config.xdg.configHome}/gnupg";
  };

  # Fix pass
  services.gpg-agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
      allow-preset-passphrase
    '';
  };
}
