{ lib, config, ... }:
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
    mimeApps.defaultApplications =
      let
        code = [
          "text/english"
          "text/plain"
          "text/x-makefile"
          "text/x-c++hdr"
          "text/x-c++src"
          "text/x-chdr"
          "text/x-csrc"
          "text/x-java"
          "text/x-moc"
          "text/x-pascal"
          "text/x-tcl"
          "text/x-tex"
          "application/x-shellscript"
          "text/x-c"
          "text/x-c++"
        ];
      in
      {
        "inode/directory" = [ "pcmanfm.desktop" ];
        "application/zip" = [ "xarchiver.desktop" ];
        "application/gzip" = [ "xarchiver.desktop" ];
        "application/x-rar" = [ "xarchiver.desktop" ];
      } //
      lib.genAttrs code (_: [ "nvim.desktop" ]);
  };
}
