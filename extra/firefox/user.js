user_pref("browser.aboutConfig.showWarning", false);
user_pref("browser.startup.page", 3);
user_pref("browser.startup.homepage", "chrome://browser/content/blanktab.html");
user_pref("browser.newtabpage.enabled", false);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredCheckboxes", false);
user_pref("browser.newtabpage.activity-stream.default.sites", "");
user_pref("browser.newtabpage.activity-stream.feeds.telemetry", false);
user_pref("browser.newtabpage.activity-stream.telemetry", false);
user_pref( "browser.newtabpage.activity-stream.telemetry.privatePing.enabled", false);
user_pref( "browser.newtabpage.activity-stream.telemetry.privatePing.inferredInterests.enabled", false);
user_pref( "browser.newtabpage.activity-stream.telemetry.privatePing.redactNewtabPing.enabled", false);
user_pref("browser.shell.checkDefaultBrowser", false);
user_pref("browser.aboutwelcome.enabled", false);
user_pref("browser.startup.homepage_override.mstone", "ignore");
user_pref("startup.homepage_welcome_url", "");
user_pref("startup.homepage_welcome_url.additional", "");

user_pref("browser.search.separatePrivateDefault", true);
user_pref("browser.search.separatePrivateDefault.ui.enabled", true);

// Cache
user_pref("browser.cache.disk.enable", false);
user_pref("browser.cache.memory.enable", true);
user_pref("browser.privatebrowsing.forceMediaMemoryCache", true);
user_pref("browser.sessionstore.privacy_level", 2);
// user_pref("browser.cache.memory.capacity", 262144); // 256 MB memory cache
user_pref("browser.cache.memory.max_entry_size", 51200); // 50 MB max item
user_pref("media.memory_cache_max_size", 524288); // 512 MB media memory cache
user_pref("media.memory_caches_combined_limit_kb", 1048576); // 1 GB combined media cache
user_pref("image.cache.size", 20971520); // 20 MB image cache
user_pref("image.mem.decode_bytes_at_a_time", 32768);
user_pref("gfx.canvas.accelerated.cache-items", 32768);
user_pref("gfx.canvas.accelerated.cache-size", 4096);
user_pref("gfx.content.skia-font-cache-size", 32);
user_pref("network.ssl_tokens_cache_capacity", 10240); // Boost SSL token cache for quicker secure reconnects.
user_pref("browser.sessionstore.interval", 60000); // save session every 60s instead of 15s
// user_pref("media.cache_readahead_limit", 300); // 300s
// user_pref("media.cache_resume_threshold", 150); // 150s

user_pref("dom.security.https_only_mode", true); // Force HTTPS everywhere (reduces insecure requests; FF147 optimizes this).
user_pref("dom.security.https_only_mode_send_http_background_request", false); // Block background HTTP requests in HTTPS mode.

user_pref("network.proxy.socks_remote_dns", true);
user_pref("network.file.disable_unc_paths", true);
user_pref("network.gio.supported-protocols", "");

user_pref("extensions.pocket.enabled", false);

user_pref("browser.theme.content-theme", 0);
user_pref("browser.theme.toolbar-theme", 0);
user_pref("layout.css.prefers-color-scheme.content-override", 0);
user_pref("browser.compactmode.show", true);
user_pref("browser.uidensity", 1);

// Disable Picture-in-Picture feature
user_pref("media.videocontrols.picture-in-picture.enabled", false);
user_pref("media.videocontrols.picture-in-picture.video-toggle.enabled", false);

user_pref("middlemouse.paste", false);
user_pref("general.autoScroll", true);

// Disable fullscreen warning/prompt
user_pref("full-screen-api.warning.timeout", 0);
user_pref("full-screen-api.warning.delay", -1);

// Disable tab hover previews (thumbnails on hover)
user_pref("browser.tabs.hoverPreview.enabled", false);
user_pref("browser.tabs.cardPreview.enabled", false);

user_pref("dom.battery.enabled", false); // disable battery API
user_pref("dom.gamepad.enabled", false); // disable gamepad API
user_pref("geo.enabled", false); // disable geolocation API
user_pref("media.autoplay.default", 1); //  1 = block autoplay with sound, 5 = block all autoplay
user_pref("media.autoplay.blocking_policy", 0);

user_pref("gfx.webrender.all", true); // force GPU rendering
user_pref("layers.acceleration.force-enabled", true); // Force hardware acceleration if not auto-detected (check about:support > Graphics for "Compositing: WebRender").
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("gfx.webrender.compositor", true);
user_pref("gfx.webrender.layer-compositor", true); // Enable advanced WebRender compositing for snappier UI (builds on your existing WebRender prefs).

user_pref("browser.safebrowsing.malware.enabled", true);
user_pref("browser.safebrowsing.phishing.enabled", true);

user_pref("browser.tabs.animate", false);
user_pref("browser.download.animateNotifications", false);
user_pref("toolkit.cosmeticAnimations.enabled", false);

user_pref("reader.parse-on-load.enabled", false);
user_pref("extensions.screenshots.disabled", true);
user_pref("extensions.formautofill.creditCards.enabled", false);
user_pref("extensions.formautofill.addresses.enabled", false);

user_pref("browser.search.serpEventTelemetryCategorization.enabled", false);
user_pref("browser.search.serpEventTelemetryCategorization.regionEnabled", false);
user_pref("identity.fxaccounts.telemetry.clientAssociationPing.enabled", false);
user_pref("nimbus.telemetry.targetingContextEnabled", false);
// user_pref("permissions.desktop-notification.telemetry.siteCategories", false);
user_pref("browser.urlbar.eventTelemetry.enabled", false); // [FF93+]
user_pref("browser.tabs.firefox-view-next", false); // Firefox View telemetry
user_pref("browser.tabs.firefox-view", false);
user_pref("browser.vpn_promo.enabled", false); // VPN promo telemetry
user_pref("browser.promo.focus.enabled", false); // Focus app promo
user_pref("browser.promo.pin.enabled", false); // Pin promo
user_pref("datareporting.usage.uploadEnabled", false);
user_pref("browser.topsites.contile.enabled", false);
user_pref("browser.preferences.moreFromMozilla", false);

// Sensors off
user_pref("device.sensors.ambientLight.enabled", false);
user_pref("device.sensors.enabled", false);
user_pref("device.sensors.motion.enabled", false);
user_pref("device.sensors.orientation.enabled", false);

// Notification/push annoyances
user_pref("dom.webnotifications.enabled", false);
user_pref("dom.push.enabled", false);

user_pref("permissions.default.geo", 2);
user_pref("permissions.default.desktop-notification", 2);

/* 0202: disable using the OS's geolocation service ***/
user_pref("geo.provider.ms-windows-location", false); // [WINDOWS]
user_pref("geo.provider.use_corelocation", false); // [MAC]
user_pref("geo.provider.use_geoclue", false); // [FF102+] [LINUX]

/** RECOMMENDATIONS ***/
/* 0320: disable recommendation pane in about:addons (uses Google Analytics) ***/
user_pref("extensions.getAddons.showPane", false); // [HIDDEN PREF]
/* 0321: disable recommendations in about:addons' Extensions and Themes panes [FF68+] ***/
user_pref("extensions.htmlaboutaddons.recommendations.enabled", false);
/* 0322: disable personalized Extension Recommendations in about:addons and AMO [FF65+]
 * [NOTE] This pref has no effect when Health Reports (8501) are disabled
 * [SETTING] Privacy & Security>Firefox Data Collection and Use>Allow personalized extension recommendations
 * [1] https://support.mozilla.org/kb/personalized-extension-recommendations ***/
user_pref("browser.discovery.enabled", false);

/** STUDIES ***/
/* 0340: disable Studies
 * [SETTING] Privacy & Security>Firefox Data Collection and Use>Install and run studies ***/
user_pref("app.shield.optoutstudies.enabled", false);
/* 0341: disable Normandy/Shield [FF60+]
 * Shield is a telemetry system that can push and test "recipes"
 * [1] https://mozilla.github.io/normandy/ ***/
user_pref("app.normandy.enabled", false);
user_pref("app.normandy.api_url", "");

/** CRASH REPORTS ***/
/* 0350: disable Crash Reports ***/
user_pref("breakpad.reportURL", "");
user_pref("browser.tabs.crashReporting.sendReport", false); // [FF44+]
// user_pref("browser.crashReports.unsubmittedCheck.enabled", false); // [FF51+] [DEFAULT: false]
/* 0351: enforce no submission of backlogged Crash Reports [FF58+]
 * [SETTING] Privacy & Security>Firefox Data Collection and Use>Send backlogged crash reports  ***/
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit2", false); // [DEFAULT: false]

/** OTHER ***/
/* 0360: disable Captive Portal detection
 * [1] https://www.eff.org/deeplinks/2017/08/how-captive-portals-interfere-wireless-security-and-privacy ***/
user_pref("captivedetect.canonicalURL", "");
user_pref("network.captive-portal-service.enabled", false); // [FF52+]
/* 0361: disable Network Connectivity checks [FF65+]
 * [1] https://bugzilla.mozilla.org/1460537 ***/
user_pref("network.connectivity-service.enabled", false);

/*** [SECTION 0600]: BLOCK IMPLICIT OUTBOUND [not explicitly asked for - e.g. clicked on] ***/
user_pref("_user.js.parrot", "0600 syntax error: the parrot's no more!");
/* 0601: disable link prefetching
 * [1] https://developer.mozilla.org/docs/Web/HTTP/Link_prefetching_FAQ ***/
user_pref("network.prefetch-next", false);
/* 0602: disable DNS prefetching
 * [1] https://developer.mozilla.org/docs/Web/HTTP/Headers/X-DNS-Prefetch-Control ***/
user_pref("network.dns.disablePrefetch", true);
user_pref("network.dns.disablePrefetchFromHTTPS", true);
/* 0603: disable predictor / prefetching ***/
user_pref("network.predictor.enabled", false);
// user_pref("network.predictor.enable-prefetch", false); // [FF48+] [DEFAULT: false]
/* 0604: disable link-mouseover opening connection to linked server
 * [1] https://news.slashdot.org/story/15/08/14/2321202/how-to-quash-firefoxs-silent-requests ***/
user_pref("network.http.speculative-parallel-limit", 0);
/* 0605: disable mousedown speculative connections on bookmarks and history [FF98+] ***/
user_pref("browser.places.speculativeConnect.enabled", false);
/* 0610: enforce no "Hyperlink Auditing" (click tracking)
 * [1] https://www.bleepingcomputer.com/news/software/major-browsers-to-prevent-disabling-of-click-tracking-privacy-risk/ ***/
// user_pref("browser.send_pings", false); // [DEFAULT: false]

/* 0801: disable location bar making speculative connections [FF56+]
 * [1] https://bugzilla.mozilla.org/1348275 ***/
user_pref("browser.urlbar.speculativeConnect.enabled", false);

/* 0802: disable location bar contextual suggestions
 * [NOTE] The UI is controlled by the .enabled pref
 * [SETTING] Search>Address Bar>Suggestions from...
 * [1] https://blog.mozilla.org/data/2021/09/15/data-and-firefox-suggest/ ***/
user_pref("browser.urlbar.quicksuggest.enabled", false); // [FF92+]
user_pref("browser.urlbar.suggest.quicksuggest.nonsponsored", false); // [FF95+]
user_pref("browser.urlbar.suggest.quicksuggest.sponsored", false); // [FF92+]
/* 0803: disable live search suggestions
 * [NOTE] Both must be true for live search to work in the location bar
 * [SETUP-CHROME] Override these if you trust and use a privacy respecting search engine
 * [SETTING] Search>Show search suggestions | Show search suggestions in address bar results ***/
user_pref("browser.search.suggest.enabled", false);
user_pref("browser.urlbar.suggest.searches", false);
/* 0805: disable urlbar trending search suggestions [FF118+]
 * [SETTING] Search>Search Suggestions>Show trending search suggestions (FF119) ***/
user_pref("browser.urlbar.trending.featureGate", false);
/* 0806: disable urlbar suggestions ***/
user_pref("browser.urlbar.addons.featureGate", false); // [FF115+]
user_pref("browser.urlbar.amp.featureGate", false); // [FF141+] adMarketplace
user_pref("browser.urlbar.importantDates.featureGate", false); // [FF143+]
user_pref("browser.urlbar.market.featureGate", false); // [FF143+] stock market
user_pref("browser.urlbar.mdn.featureGate", false); // [FF117+]
user_pref("browser.urlbar.weather.featureGate", false); // [FF108+]
user_pref("browser.urlbar.wikipedia.featureGate", false); // [FF141+]
user_pref("browser.urlbar.yelp.featureGate", false); // [FF124+]
user_pref("browser.urlbar.yelpRealtime.featureGate", false); // [FF144+]
/* 0807: disable urlbar clipboard suggestions [FF118+] ***/
// user_pref("browser.urlbar.clipboard.featureGate", false);
/* 0808: disable recent searches [FF120+]
 * [NOTE] Recent searches are cleared with history (2811+)
 * [1] https://support.mozilla.org/kb/search-suggestions-firefox ***/
// user_pref("browser.urlbar.recentsearches.featureGate", false);
/* 0810: disable search and form history
 * [NOTE] We also clear formdata on exit (2811+)
 * [SETUP-WEB] Be aware that autocomplete form data can be read by third parties [1][2]
 * [SETTING] Privacy & Security>History>Custom Settings>Remember search and form history
 * [1] https://blog.mindedsecurity.com/2011/10/autocompleteagain.html
 * [2] https://bugzilla.mozilla.org/381681 ***/
user_pref("browser.formfill.enable", false);

user_pref("network.auth.subresource-http-auth-allow", 1);

user_pref("_user.js.parrot", "8500 syntax error: the parrot's off the twig!");
/* 8500: disable new data submission [FF41+]
 * If disabled, no policy is shown or upload takes place, ever
 * [1] https://bugzilla.mozilla.org/1195552 ***/
user_pref("datareporting.policy.dataSubmissionEnabled", false);
/* 8501: disable Health Reports
 * [SETTING] Privacy & Security>Firefox Data Collection and Use>Send technical... data ***/
user_pref("datareporting.healthreport.uploadEnabled", false);
/* 0802: disable telemetry
 * The "unified" pref affects the behavior of the "enabled" pref
 * - If "unified" is false then "enabled" controls the telemetry module
 * - If "unified" is true then "enabled" only controls whether to record extended data
 * [NOTE] "toolkit.telemetry.enabled" is now LOCKED to reflect prerelease (true) or release builds (false) [2]
 * [1] https://firefox-source-docs.mozilla.org/toolkit/components/telemetry/telemetry/internals/preferences.html
 * [2] https://medium.com/georg-fritzsche/data-preference-changes-in-firefox-58-2d5df9c428b5 ***/
user_pref("toolkit.telemetry.unified", false);
user_pref("toolkit.telemetry.enabled", false); // see [NOTE]
user_pref("toolkit.telemetry.server", "data:,");
user_pref("toolkit.telemetry.archive.enabled", false);
user_pref("toolkit.telemetry.newProfilePing.enabled", false); // [FF55+]
user_pref("toolkit.telemetry.shutdownPingSender.enabled", false); // [FF55+]
user_pref("toolkit.telemetry.updatePing.enabled", false); // [FF56+]
user_pref("toolkit.telemetry.bhrPing.enabled", false); // [FF57+] Background Hang Reporter
user_pref("toolkit.telemetry.firstShutdownPing.enabled", false); // [FF57+]
/* 8503: disable Telemetry Coverage
 * [1] https://blog.mozilla.org/data/2018/08/20/effectively-measuring-search-in-firefox/ ***/
user_pref("toolkit.telemetry.coverage.opt-out", true); // [HIDDEN PREF]
user_pref("toolkit.coverage.opt-out", true); // [FF64+] [HIDDEN PREF]
user_pref("toolkit.coverage.endpoint.base", "");

user_pref( "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false);
user_pref( "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false);
user_pref("extensions.webcompat-reporter.enabled", false); // [DEFAULT: false]
user_pref("browser.messaging-system.whatsNewPanel.enabled", false);

// Disable Containers
user_pref("privacy.userContext.enabled", false);
user_pref("privacy.userContext.ui.enabled", false);
user_pref("browser.discovery.containers.enabled", false);

user_pref("browser.tabs.groups.enabled", false);

user_pref("browser.tabs.splitView.enabled", false);
user_pref("sidebar.revamp", false);
user_pref("sidebar.verticalTabs", false);

user_pref("pdfjs.disabled", false); // [DEFAULT: false]
user_pref("pdfjs.enableScripting", false);

user_pref("browser.tabs.searchclipboardfor.middleclick", false);

user_pref("browser.download.manager.addToRecentDocs", false);

user_pref("network.http.referer.XOriginTrimmingPolicy", 2); // Trim cross-origin referers
user_pref("privacy.partition.network_state", true); // Network state partitioning
user_pref("privacy.partition.serviceWorkers", true); // Service worker partitioning
user_pref("network.IDN_show_punycode", true); // Show punycode (anti-phishing)

user_pref("network.trr.mode", 5);
user_pref("network.trr.uri", "https://127.0.0.1:3000/dns-query");
user_pref("network.trr.custom_uri", "https://127.0.0.1:3000/dns-query");
user_pref("network.dns.echconfig.enabled", true);
user_pref("network.dns.use_https_rr_as_altsvc", true);

user_pref("browser.contentblocking.category", "strict");
user_pref("browser.uitour.enabled", false);
user_pref("privacy.globalprivacycontrol.enabled", true);

user_pref("security.OCSP.enabled", 0);
user_pref("security.OCSP.require", false);

user_pref("browser.contentanalysis.enabled", false);
user_pref("browser.contentanalysis.default_result", 0);
user_pref("privacy.antitracking.isolateContentScriptResources", true);
user_pref("security.csp.reporting.enabled", false);

user_pref("security.ssl.treat_unsafe_negotiation_as_broken", true);
user_pref("browser.xul.error_pages.expert_bad_cert", true);
user_pref("security.tls.enable_0rtt_data", true);
user_pref("security.remote_settings.crlite_filters.enabled", true);
user_pref("security.pki.crlite_mode", 2);
user_pref("security.tls.version.min", 3);
user_pref("security.tls.version.enable-deprecated", false); // [DEFAULT: false]

// user_pref("security.ssl.require_safe_negotiation", true);
// user_pref("security.cert_pinning.enforcement_level", 2);

// fullscreen notice
user_pref("full-screen-api.transition-duration.enter", "0 0"); // default=200 200
user_pref("full-screen-api.transition-duration.leave", "0 0"); // default=200 200
user_pref("full-screen-api.warning.timeout", 0); // default=3000; alt=1250
user_pref("full-screen-api.warning.delay", -1); // default=500

// tracking
user_pref("privacy.trackingprotection.enabled", true);
user_pref("privacy.trackingprotection.pbmode.enabled", true);
user_pref("privacy.trackingprotection.socialtracking.enabled", true);
user_pref("privacy.trackingprotection.cryptomining.enabled", true);
user_pref("privacy.trackingprotection.fingerprinting.enabled", true);

user_pref("privacy.query_stripping.enabled", true);
user_pref("privacy.query_stripping.enabled.pbmode", true);

user_pref("browser.send_pings", false);

// ai
user_pref("browser.ai.control.default", "blocked");
user_pref("browser.ml.enable", false);
user_pref("browser.tabs.groups.smart.enabled", false);
user_pref("browser.ml.linkPreview.enabled", false);
user_pref("browser.ai.control.linkPreviewKeyPoints", "blocked");
user_pref("browser.ai.control.sidebarChatbot", "blocked");
user_pref("browser.ai.control.pdfjsAltText", "blocked");
user_pref("browser.ai.control.smartTabGroups", "blocked");
user_pref("browser.ml.chat.enabled", false);
user_pref("browser.ml.chat.shortcuts", false);
user_pref("browser.ml.chat.menu", false);
user_pref("extensions.ml.enabled", false);

// user_pref("intl.accept_languages", "en-us, en");
user_pref("privacy.spoof_english", 2);
user_pref("widget.non-native-theme.use-theme-accent", false);
user_pref("browser.link.open_newwindow", 3);
user_pref("browser.link.open_newwindow.restriction", 0);
user_pref("browser.urlbar.showSearchTerms.enabled", false);

user_pref("extensions.enabledScopes", 5);
// user_pref("privacy.resistFingerprinting.block_mozAddonManager", true);
// user_pref("extensions.webextensions.restrictedDomains", "");

user_pref("media.peerconnection.enabled", false);
//user_pref("media.peerconnection.ice.proxy_only_if_behind_proxy", true);
//user_pref("media.peerconnection.ice.default_address_only", true);
//user_pref("media.peerconnection.ice.no_host", true);

user_pref("browser.translations.enable", true); // Translation feature (has telemetry)
user_pref("browser.translations.automaticallyPopup", false);
