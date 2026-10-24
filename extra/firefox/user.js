// CACHE
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

user_pref("middlemouse.paste", false);
user_pref("browser.tabs.searchclipboardfor.middleclick", false);
user_pref("general.autoScroll", true);
user_pref("browser.ctrlTab.sortByRecentlyUsed", true);
user_pref("ui.key.menuAccessKeyFocuses", false);
user_pref("browser.quitShortcut.disabled", true);
user_pref("ui.caretBlinkTime", 0);

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

user_pref("reader.parse-on-load.enabled", false);
user_pref("extensions.screenshots.disabled", true);
user_pref("extensions.formautofill.creditCards.enabled", false);
user_pref("extensions.formautofill.addresses.enabled", false);

user_pref("browser.search.serpEventTelemetryCategorization.enabled", false);
user_pref("browser.search.serpEventTelemetryCategorization.regionEnabled", false);
user_pref("identity.fxaccounts.telemetry.clientAssociationPing.enabled", false);
user_pref("nimbus.telemetry.targetingContextEnabled", false);
// user_pref("permissions.desktop-notification.telemetry.siteCategories", false);
user_pref("browser.tabs.firefox-view-next", false); // Firefox View telemetry
user_pref("browser.tabs.firefox-view", false);
user_pref("browser.vpn_promo.enabled", false); // VPN promo telemetry
user_pref("browser.promo.focus.enabled", false); // Focus app promo
user_pref("browser.promo.pin.enabled", false); // Pin promo
user_pref("browser.topsites.contile.enabled", false);

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

user_pref("network.auth.subresource-http-auth-allow", 1);

// UI
user_pref("browser.theme.content-theme", 0);
user_pref("browser.theme.toolbar-theme", 0);
user_pref("layout.css.prefers-color-scheme.content-override", 0);
user_pref("ui.systemUsesDarkTheme", 1);
user_pref("browser.compactmode.show", true);
user_pref("browser.uidensity", 1);

user_pref("extensions.getAddons.showPane", false);
user_pref("extensions.htmlaboutaddons.recommendations.enabled", false);
user_pref("browser.shell.checkDefaultBrowser", false);
user_pref("browser.preferences.moreFromMozilla", false);
user_pref("browser.aboutConfig.showWarning", false);
user_pref("browser.startup.homepage_override.mstone", "ignore");
user_pref("browser.aboutwelcome.enabled", false);
user_pref("browser.profiles.enabled", false);
user_pref("browser.startup.page", 3);
user_pref("browser.startup.homepage", "chrome://browser/content/blanktab.html");
user_pref("startup.homepage_welcome_url", "");
user_pref("startup.homepage_welcome_url.additional", "");

user_pref("identity.fxaccounts.toolbar.pxiToolbarEnabled", false);
user_pref("sidebar.main.tools", "history");

user_pref("full-screen-api.transition-duration.enter", "0 0");
user_pref("full-screen-api.transition-duration.leave", "0 0");
user_pref("full-screen-api.warning.timeout", 0);

user_pref("media.videocontrols.picture-in-picture.enabled", false);
user_pref("media.videocontrols.picture-in-picture.video-toggle.enabled", false);

user_pref("browser.tabs.hoverPreview.enabled", false);
user_pref("browser.tabs.cardPreview.enabled", false);

user_pref("browser.tabs.animate", false);
user_pref("browser.download.animateNotifications", false);
user_pref("toolkit.cosmeticAnimations.enabled", false);

user_pref("browser.tabs.groups.enabled", false);
user_pref("browser.tabs.splitView.enabled", false);
user_pref("sidebar.revamp", false);
user_pref("sidebar.verticalTabs", false);

user_pref("browser.newtabpage.enabled", false);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredCheckboxes", false);
user_pref("browser.newtabpage.activity-stream.showWebNotifications", false);
user_pref("browser.newtabpage.activity-stream.default.sites", "");
user_pref("browser.newtabpage.activity-stream.feeds.telemetry", false);
user_pref("browser.newtabpage.activity-stream.feeds.weatherfeed", false);
user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
user_pref("browser.newtabpage.activity-stream.telemetry", false);
user_pref("browser.newtabpage.activity-stream.telemetry.privatePing.enabled", false);
user_pref("browser.newtabpage.activity-stream.telemetry.privatePing.inferredInterests.enabled", false);
user_pref("browser.newtabpage.activity-stream.telemetry.privatePing.redactNewtabPing.enabled", false);
user_pref("browser.newtabpage.activity-stream.widgets.enabled", false);
user_pref("browser.newtabpage.activity-stream.widgets.feedback.enabled", false);
user_pref("browser.newtabpage.activity-stream.widgets.hideAllToast.enabled", true);
user_pref("browser.newtabpage.activity-stream.widgets.privacy.showVpnMessages", false);
user_pref("browser.newtabpage.activity-stream.discoverystream.carousel.enabled", false);

// SPECULATIVE LOADING
user_pref("network.http.speculative-parallel-limit", 0);
user_pref("network.dns.disablePrefetch", true);
user_pref("network.dns.disablePrefetchFromHTTPS", true);
user_pref("browser.urlbar.speculativeConnect.enabled", false);
user_pref("browser.places.speculativeConnect.enabled", false);
user_pref("network.prefetch-next", false);
user_pref("network.early-hints.preconnect.max_connections", 0);

// URLBAR
user_pref("browser.urlbar.trimHttps", true);
user_pref("browser.urlbar.untrimOnUserInteraction.featureGate", true);
user_pref("browser.urlbar.groupLabels.enabled", false);
user_pref("browser.urlbar.addons.featureGate", false);
user_pref("browser.urlbar.amp.featureGate", false);
user_pref("browser.urlbar.importantDates.featureGate", false);
user_pref("browser.urlbar.market.featureGate", false);
user_pref("browser.urlbar.mdn.featureGate", false);
user_pref("browser.urlbar.weather.featureGate", false);
user_pref("browser.urlbar.wikipedia.featureGate", false);
user_pref("browser.urlbar.yelp.featureGate", false);
user_pref("browser.urlbar.yelpRealtime.featureGate", false);
user_pref("browser.urlbar.suggest.engines", false);
user_pref("browser.urlbar.trending.featureGate", false);
user_pref("browser.search.suggest.enabled", false);
user_pref("browser.urlbar.suggest.searches", false);
user_pref("browser.urlbar.quicksuggest.enabled", false);
user_pref("browser.urlbar.quicksuggest.online.enabled", false);
user_pref("browser.urlbar.suggest.quicksuggest.nonsponsored", false);
user_pref("browser.urlbar.suggest.quicksuggest.sponsored", false);
user_pref("browser.urlbar.showSearchTerms.enabled", false);
user_pref("browser.urlbar.eventTelemetry.enabled", false);
user_pref("browser.urlbar.clipboard.featureGate", false);
user_pref("browser.urlbar.recentsearches.featureGate", false);
user_pref("network.IDN_show_punycode", true);
user_pref("browser.formfill.enable", false);
user_pref("browser.search.separatePrivateDefault", true);
user_pref("browser.search.separatePrivateDefault.ui.enabled", true);

// TELEMETRY
user_pref("toolkit.telemetry.unified", false);
user_pref("toolkit.telemetry.enabled", false);
user_pref("toolkit.telemetry.server", "data:,");
user_pref("toolkit.telemetry.archive.enabled", false);
user_pref("toolkit.telemetry.newProfilePing.enabled", false);
user_pref("toolkit.telemetry.firstShutdownPing.enabled", false);
user_pref("toolkit.telemetry.shutdownPingSender.enabled", false);
user_pref("toolkit.telemetry.bhrPing.enabled", false);
user_pref("toolkit.telemetry.updatePing.enabled", false);
user_pref("toolkit.coverage.enabled", false);
user_pref("toolkit.coverage.opt-out", true);
user_pref("toolkit.telemetry.coverage.opt-out", true);
user_pref("toolkit.coverage.endpoint.base", "");
user_pref("datareporting.policy.dataSubmissionEnabled", false);
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("app.normandy.enabled", false);
user_pref("app.normandy.api_url", "");
user_pref("app.shield.optoutstudies.enabled", false);
user_pref("nimbus.rollouts.enabled", false);
user_pref("browser.discovery.enabled", false);
user_pref("browser.tabs.crashReporting.sendReport", false);
user_pref("browser.crashReports.unsubmittedCheck.enabled", false);
user_pref("breakpad.reportURL", "");
user_pref("network.connectivity-service.enabled", false);
user_pref("network.captive-portal-service.enabled", false);
user_pref("captivedetect.canonicalURL", "");
user_pref("datareporting.usage.uploadEnabled", false);
user_pref("identity.fxaccounts.telemetry.clientInfoPing.enabled", false);
user_pref("browser.send_pings", false);
user_pref("browser.ping-centre.telemetry", false);

user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false);
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false);
user_pref("extensions.webcompat-reporter.enabled", false); // [DEFAULT: false]
user_pref("browser.messaging-system.whatsNewPanel.enabled", false);

// Disable Containers
user_pref("privacy.userContext.enabled", false);
user_pref("privacy.userContext.ui.enabled", false);
user_pref("browser.discovery.containers.enabled", false);

user_pref("pdfjs.disabled", false);
user_pref("pdfjs.enableScripting", false);

user_pref("browser.download.manager.addToRecentDocs", false);

user_pref("network.http.referer.XOriginTrimmingPolicy", 2); // Trim cross-origin referers
user_pref("privacy.partition.network_state", true); // Network state partitioning
user_pref("privacy.partition.serviceWorkers", true); // Service worker partitioning

user_pref("doh-rollout.provider-list", '[{"UIName":"Quad9 (No Filtering)","uri":"https://dns10.quad9.net/dns-query"}, {"UIName":"Quad9 (Malware blocking)","uri":"https://dns.quad9.net/dns-query"}, {"UIName":"LibreDNS (No Filtering)","uri":"https://doh.libredns.gr/dns-query"}, {"UIName":"LibreDNS (Adblocking)","uri":"https://doh.libredns.gr/noads"}, {"UIName":"Wikimedia DNS (No Filtering)","uri":"https://wikimedia-dns.org/dns-query"}, {"UIName":"DNS4All (No Filtering)","uri":"https://doh.dns4all.eu/dns-query"}, {"UIName":"Cloudflare DNS","uri":"https://cloudflare-dns.com/dns-query"}, {"UIName":"Mozilla Cloudflare","uri":"https://mozilla.cloudflare-dns.com/dns-query"}, {"UIName":"Google DNS","uri":"https://dns.google/dns-query"}, {"UIName":"AdGuard DNS (Default filtering)","uri":"https://dns.adguard-dns.com/dns-query"}]');
user_pref("doh-rollout.enabled", false);
user_pref("network.trr.useGET", false);
user_pref("network.trr.mode", 5);
user_pref("network.trr.default_provider_uri", "https://dns.quad9.net/dns-query");
user_pref("network.dns.echconfig.enabled", true);
user_pref("network.dns.use_https_rr_as_altsvc", true);

user_pref("browser.uitour.enabled", false);
user_pref("browser.uitour.url", "");
user_pref("privacy.globalprivacycontrol.enabled", true);

user_pref("security.OCSP.enabled", 0);
user_pref("security.OCSP.require", false);

user_pref("browser.contentanalysis.enabled", false);
user_pref("browser.contentanalysis.default_result", 0);
user_pref("privacy.antitracking.isolateContentScriptResources", true);
user_pref("security.csp.reporting.enabled", false);

user_pref("security.cert_pinning.enforcement_level", 2);
user_pref("security.ssl.treat_unsafe_negotiation_as_broken", true);
user_pref("browser.xul.error_pages.expert_bad_cert", true);
user_pref("security.tls.enable_0rtt_data", false);
user_pref("security.remote_settings.crlite_filters.enabled", true);
user_pref("security.pki.crlite_mode", 2);
user_pref("security.tls.version.min", 3);
user_pref("security.tls.version.enable-deprecated", false); // [DEFAULT: false]

user_pref("security.certerrors.mitm.auto_enable_enterprise_roots", false);
user_pref("security.enterprise_roots.enabled", false);
user_pref("security.qwacs.enabled", false);

user_pref("browser.contentblocking.category", "strict");
user_pref("privacy.trackingprotection.enabled", true);
user_pref("privacy.trackingprotection.pbmode.enabled", true);
user_pref("privacy.trackingprotection.socialtracking.enabled", true);
user_pref("privacy.trackingprotection.cryptomining.enabled", true);
user_pref("privacy.trackingprotection.fingerprinting.enabled", true);

user_pref("browser.safebrowsing.malware.enabled", true);
user_pref("browser.safebrowsing.phishing.enabled", true);

user_pref("privacy.query_stripping.enabled", true);
user_pref("privacy.query_stripping.enabled.pbmode", true);

// ai
user_pref("browser.ai.control.default", "blocked");
user_pref("browser.ml.enable", false);
user_pref("browser.tabs.groups.smart.enabled", false);
user_pref("browser.smartwindow.searchTheWebFast", false);
user_pref("browser.ml.linkPreview.enabled", false);
user_pref("browser.ml.linkPreview.blockListEnabled", false);
user_pref("browser.ai.control.linkPreviewKeyPoints", "blocked");
user_pref("browser.ai.control.sidebarChatbot", "blocked");
user_pref("browser.ai.control.pdfjsAltText", "blocked");
user_pref("browser.ai.control.smartTabGroups", "blocked");
user_pref("browser.ai.control.speechRecognition", "blocked");
user_pref("browser.ml.chat.enabled", false);
user_pref("browser.ml.chat.shortcuts", false);
user_pref("browser.ml.chat.menu", false);
user_pref("browser.preferences.aiControls", false);
user_pref("extensions.ml.enabled", false);
user_pref("extensions.formautofill.useml", false);

user_pref("privacy.spoof_english", 2);
user_pref("intl.accept_languages", "en-US, en");

user_pref("widget.non-native-theme.use-theme-accent", false);
user_pref("browser.link.open_newwindow", 3);
user_pref("browser.link.open_newwindow.restriction", 0);

user_pref("extensions.enabledScopes", 5);
user_pref("devtools.aboutdebugging.showHiddenAddons", true);
user_pref("extensions.webextensions.restrictedDomains", "");

user_pref("media.peerconnection.enabled", false);
user_pref("media.peerconnection.ice.default_address_only", true);
user_pref("media.peerconnection.ice.proxy_only_if_behind_proxy", true);
//user_pref("media.peerconnection.ice.no_host", true);

user_pref("browser.translations.enable", true);
user_pref("browser.translations.automaticallyPopup", false);

user_pref("browser.region.network.url", "");
user_pref("browser.region.update.enabled", false);

user_pref("identity.fxaccounts.enabled", false);
user_pref("browser.profiles.enabled", false);
