{
  "browser.tabs.firefox-view" = false; # Disable Firefox View
  "browser.tabs.firefox-view-next" = false;
  "extensions.update.enabled" = false;

  # Disable telemetry
  "browser.newtabpage.activity-stream.feeds.telemetry" = false;
  "browser.ping-centre.telemetry" = false;
  "browser.tabs.crashReporting.sendReport" = false;
  "devtools.onboarding.telemetry.logged" = false;
  "toolkit.telemetry.enabled" = false;
  "toolkit.telemetry.server" = "data:,";
  "toolkit.telemetry.unified" = false;
  "toolkit.telemetry.archive.enabled" = false;
  "toolkit.telemetry.newProfilePing.enabled" = false;
  "toolkit.telemetry.shutdownPingSender.enabled" = false;
  "toolkit.telemetry.updatePing.enabled" = false;
  "toolkit.telemetry.bhrPing.enabled" = false;
  "toolkit.telemetry.firstShutdownPing.enabled" = false;

  # # Disable Pocket
  "browser.newtabpage.activity-stream.feeds.discoverystreamfeed" = false;
  "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
  "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
  "browser.newtabpage.activity-stream.showSponsored" = false;
  "extensions.pocket.enabled" = false;

  # Disable prefetching
  "network.dns.disablePrefetch" = true;
  "network.prefetch-next" = false;

  # Disable JS in PDFs
  "pdfjs.enableScripting" = false;

  # Harden SSL
  "security.ssl.require_safe_negotiation" = true;

  # Tweaks from archwiki
  "browser.cache.disk.enable" = false;
  "browser.cache.memory.enable" = true;
  "browser.cache.memory.capacity" = -1;
  "browser.aboutConfig.showWarning" = false;
  "browser.preferences.defaultPerformanceSettings.enabled" = false;
  "middlemouse.paste" = false;

  # # Extra
  "identity.fxaccounts.enabled" = false;
  "browser.download.useDownloadDir" = false;
  "browser.search.suggest.enabled" = false;
  "browser.urlbar.shortcuts.bookmarks" = false;
  "browser.urlbar.shortcuts.history" = false;
  "browser.urlbar.shortcuts.tabs" = false;
  "browser.urlbar.suggest.bookmark" = false;
  "browser.urlbar.suggest.searches" = false;
  "browser.urlbar.suggest.engines" = false;
  "browser.urlbar.suggest.history" = true;
  "browser.urlbar.suggest.openpage" = false;
  "browser.urlbar.suggest.topsites" = false;
  "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
  "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
  "signon.rememberSignons" = false;
  "signon.autofillForms" = false;
  "network.dns.disableIPv6" = true;
  "network.proxy.socks_remote_dns" = true;
  "dom.security.https_first" = true;

  # Disable permission
  # 0=always ask (default), 1=allow, 2=block
  "permissions.default.geo" = 2;
  "permissions.default.camera" = 2;
  "permissions.default.microphone" = 0;
  "permissions.default.desktop-notification" = 2;
  "permissions.default.xr" = 2; # Virtual Reality
  "browser.discovery.enabled" = false;
  "datareporting.healthreport.uploadEnabled" = false;
  "datareporting.policy.dataSubmissionEnabled" = false;
  "app.shield.optoutstudies.enabled" = false;
  "app.normandy.enabled" = false;
  "app.normandy.api_url" = "";
}
