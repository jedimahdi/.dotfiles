{ lib, pkgs, inputs, ... }:
let
  extensions = with inputs.rycee-nurpkgs.packages.${pkgs.system}; [
    darkreader
    ublock-origin
    vimium
    passff
  ];
in
{
  home.sessionVariables.BROWSER = "firefox";

  programs.firefox = {
    enable = true;

    policies = {
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DisableFeedbackCommands = true;
      DisableSetDesktopBackground = true;
      DontCheckDefaultBrowser = true;
      # DisableFirefoxAccounts = true;
      FirefoxHome = { Pocket = false; Snippets = false; };
      UserMessaging = { SkipOnboarding = true; ExtensionRecommendations = false; };
    };

    profiles = {
      default = {
        id = 0;
        isDefault = true;
        settings = import ./settings.nix;
        inherit extensions;
        extraConfig = ''
          ${builtins.readFile ./user.js}
        '';
      };
    };
  };

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
