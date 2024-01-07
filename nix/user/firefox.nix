{ lib, pkgs, ... }:

{
  home.packages = [ pkgs.firefox ];

  xdg.mimeApps.defaultApplications =
    let
      urls = [
        "text/html"
        "text/xml"
        "application/xml"
        "application/xhtml+xml"
        "application/xhtml_xml"
        "application/rdf+xml"
        "application/rss+xml"
        "application/x-extension-htm"
        "application/x-extension-html"
        "application/x-extension-shtml"
        "application/x-extension-xht"
        "application/x-extension-xhtml"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
        "x-scheme-handler/about"
        "x-scheme-handler/ftp"
        "x-scheme-handler/unknown"
      ];
    in
    lib.genAttrs urls (_: [ "firefox.desktop" ]);

  home.sessionVariables = {
    DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
  };
}
