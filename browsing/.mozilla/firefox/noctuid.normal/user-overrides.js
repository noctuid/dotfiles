// outsourcing config to https://github.com/arkenfox/user.js

// security plugins using with
// - uBlock Origin with AdGuard URL Tracking Protection filter enabled
// - Skip Redirect

// privacy badger, https everwhere, decentraleyes, etc. are unnecesary:
// https://github.com/arkenfox/user.js/wiki/4.1-Extensions
// canvasblocker is mainly useful if not using RFP
// https://github.com/arkenfox/user.js/issues/767
// https://github.com/arkenfox/user.js/issues/767#issuecomment-831229286

// Goal is not really protection against advanced fingerprinting (since I am
// using some extensions and other changes); can use a separate browser for that
// if needed

// some test websites:
// https://github.com/arkenfox/user.js/wiki/Appendix-A-Test-Sites
// https://github.com/arkenfox/user.js/wiki/Appendix-B-Test-Sites-%5BFingerprinting%5D
// https://arkenfox.github.io/TZP/
// in particular
// https://amiunique.org/fp
// https://coveryourtracks.eff.org
// https://browserleaks.com/canvas
// https://browserleaks.com/fonts

// * Tridactyl
// :fixamo equivalent
// allow tridactyl on more domains
user_pref("extensions.webextensions.restrictedDomains", "");
// arkenfox already does this
// user_pref("privacy.resistFingerprinting.block_mozAddonManager", true);

// stay in normal mode when loading new page
user_pref("browser.autofocus", false);

// tridactyl will override this from arkenfox:
// user_pref("browser.newtabpage.enabled", false);

// * Arkenfox Overrides
// overrides to make casual browsing experience more pleasant

// ** Session Data
// keep history/sessions/etc. (can use private windows for anything transient)
// https://kb.mozillazine.org/Browser.sessionstore.enabled
// session store seems to work even with this false?
user_pref("browser.sessionstore.enabled", true);
// restore session on startup
user_pref("browser.startup.page", 3);

user_pref("places.history.enabled", true);
// arkenfox does not currently set privatebrowsing autostart to true
user_pref("browser.privatebrowsing.autostart", false);
user_pref("privacy.sanitize.sanitizeOnShutdown", true);
// NOTE: these only have an effect when privacy.sanitize.santizeOnShutdown is
// true
user_pref("privacy.clearOnShutdown.history", false);
user_pref("privacy.clearOnShutdown.sessions", false);
// consider clearing cookies
user_pref("privacy.clearOnShutdown.cookies", false);
// this is fine; I already had it at 2
// user_pref("browser.sessionstore.privacy_level", 2);

// don't care about clearing these
// user_pref("privacy.clearOnShutdown.downloads", true);
// user_pref("privacy.clearOnShutdown.cache", true);
// user_pref("privacy.clearOnShutdown.formdata", true);
// basic authentication login (not browser session; that's
// privacy.clearOnShutdown.openWindows, which arkenfox does not touch)
// user_pref("privacy.clearOnShutdown.sessions", true);

// TODO what is the practical effect of this?
// "offlineApps": Offline Website Data: localStorage, service worker cache,
// QuotaManager (IndexedDB, asm-cache)
// user_pref("privacy.clearOnShutdown.offlineApps", true);

// ** Other
// don't ask; use XDG_DOWNLOAD_DIR
user_pref("browser.download.useDownloadDir", true);

// * Not Overriding
// have already been using RFP and it does not seem very problematic; using
// chrome as fallback in rare case a website does not work
// user_pref("privacy.resistFingerprinting", true);

// I can verify the safety of executable files myself; see arkenfox user.js for
// more info
// user_pref("browser.safebrowsing.downloads.remote.enabled", ...);

// generally don't use streaming sites and don't use them in firefox if I do
// user_pref("media.eme.enabled", ...);

// says it can break some input methods (e.g. ibus), but fcitx still works
// user_pref("javascript.use_us_english_locale", true);

// * Maybe Change in the Future
// user_pref("browser.startup.homepage", "about:blank");
// user_pref("captivedetect.canonicalURL", "");

// how significant is the performance benefit?
// if enable, enable memory cache:
// https://wiki.archlinux.org/title/Firefox/Tweaks#Turn_off_the_disk_cache
// user_pref("browser.cache.disk.enable", false);

// don't think I currently use anything that uses webgl
// user_pref("webgl.disabled", true);

// if it causes issues
// user_pref("network.dns.disableIPv6", true);

// * General
// disable pocket; this shows as true by default, but pocket doesn't show up in
// extensions (maybe you can manually delete it)
user_pref("extensions.pocket.enabled", false);

// https://kb.mozillazine.org/Layout.spellcheckDefault
// spell check even single line text boxes
user_pref("layout.spellcheckDefault", 2);

// https://kb.mozillazine.org/Clipboard.autocopy
// defaults to true but doesn't seem to have an effect
// user_pref("clipboard.autocopy", false);

// default is 25
user_pref("browser.sessionstore.max_tabs_undo", 100);
