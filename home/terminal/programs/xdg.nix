{ config, ... }:
{
  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
      download = "${config.home.homeDirectory}/Downloads";
    };
    mime.enable = true;
    mimeApps.enable = true;
    mimeApps.defaultApplications = {
      "inode/directory" = [ "pcmanfm.desktop" ];
      "application/zip" = [ "xarchiver.desktop" ];
      "application/gzip" = [ "xarchiver.desktop" ];
      "application/x-rar" = [ "xarchiver.desktop" ];
    };
  };
}
