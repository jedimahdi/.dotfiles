{ lib, pkgs, inputs, ... }:
let
  extensions = with inputs.rycee-nurpkgs.packages.${pkgs.system}; [
    darkreader
    ublock-origin
    vimium
  ];

  settings = {
    "app.normandy.first_run" = false;
    "app.update.channel" = "default";

    # disable all the annoying quick actions
    "browser.urlbar.quickactions.enabled" = false;
    "browser.urlbar.quickactions.showPrefs" = false;
    "browser.urlbar.shortcuts.quickactions" = false;
    "browser.urlbar.suggest.quickactions" = false;

    "general.autoScroll" = true;
    "general.useragent.locale" = "en-US";

    "extensions.update.enabled" = false;

    "accessibility.force_disabled" = 1;

    "browser.aboutConfig.showWarning" = false;

    # HIDDEN PREF: disable recommendation pane in about:addons (uses Google Analytics)
    "extensions.getAddons.showPane" = false;
    # recommendations in about:addons' Extensions and Themes panes [FF68+]
    "extensions.htmlaboutaddons.recommendations.enabled" = false;

    # disable Network Connectivity checks
    # [1] https://bugzilla.mozilla.org/1460537
    "network.connectivity-service.enabled" = false;

    # disable new data submission
    "datareporting.policy.dataSubmissionEnabled" = false;
    # disable Health Reports
    "datareporting.healthreport.uploadEnabled" = false;

    # 0332: disable telemetry
    "toolkit.telemetry.unified" = false;
    "toolkit.telemetry.enabled" = false;
    "toolkit.telemetry.server" = "data:,";
    "toolkit.telemetry.archive.enabled" = false;
    "toolkit.telemetry.newProfilePing.enabled" = false;
    "toolkit.telemetry.shutdownPingSender.enabled" = false;
    "toolkit.telemetry.updatePing.enabled" = false;
    "toolkit.telemetry.bhrPing.enabled" = false;
    "toolkit.telemetry.firstShutdownPing.enabled" = false;

    # disable PingCentre telemetry (used in several System Add-ons) [FF57+]
    "browser.ping-centre.telemetry" = false;
    # disable Firefox Home (Activity Stream) telemetry
    "browser.newtabpage.activity-stream.feeds.telemetry" = false;
    "browser.newtabpage.activity-stream.telemetry" = false;
    "toolkit.telemetry.reportingpolicy.firstRun" = false;
    "toolkit.telemetry.shutdownPingSender.enabledFirstsession" = false;
    "browser.vpn_promo.enabled" = false;
  };
in
{

  programs.firefox = {
    enable = true;

    policies = {
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      # DisableFirefoxAccounts = true;
      FirefoxHome = { Pocket = false; Snippets = false; };
      UserMessaging = { SkipOnboarding = true; ExtensionRecommendations = false; };
    };

    profiles = {
      default = {
        id = 0;
        isDefault = true;
        inherit settings extensions;
      };

      mahdi = {
        id = 1;
        isDefault = false;
        inherit settings extensions;
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
