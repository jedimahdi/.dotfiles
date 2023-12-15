{ config, lib, pkgs, ... }:

{
  home.packages = [ pkgs.firefox ];

  xdg.mimeApps.defaultApplications =
    let
      browser = [ "firefox.desktop" ];
    in
    {
      "text/html" = browser;
      "text/xml" = browser;
      "application/xml" = browser;
      "application/xhtml+xml" = browser;
      "application/xhtml_xml" = browser;
      "application/rdf+xml" = browser;
      "application/rss+xml" = browser;
      "application/x-extension-htm" = browser;
      "application/x-extension-html" = browser;
      "application/x-extension-shtml" = browser;
      "application/x-extension-xht" = browser;
      "application/x-extension-xhtml" = browser;
      "x-scheme-handler/about" = browser;
      "x-scheme-handler/ftp" = browser;
      "x-scheme-handler/http" = browser;
      "x-scheme-handler/https" = browser;
      "x-scheme-handler/unknown" = browser;
    };

  home.sessionVariables = {
    DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
  };
}
