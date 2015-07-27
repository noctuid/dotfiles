// setting things in ./.pentadactylrc also
// overrides (just put at bottom) for:
// https://github.com/pyllyukko/user.js/tree/master
// primary actually ("*p)
user_pref("clipboard.autocopy",                  true);
// probably change once have more passwords setup with pass
user_pref("privacy.clearOnShutdown.cookies",     false);
// http://kb.mozillazine.org/Network.cookie.lifetimePolicy
// 0- cookie's lifetime supplied by server
// 3- to use network.cookie.lifetime.days
user_pref("network.cookie.lifetimePolicy",       0);
// I like the sessions/history (pentadactyl completion)
user_pref("places.history.enabled",              true);
user_pref("browser.privatebrowsing.autostart",   false);
user_pref("privacy.sanitize.sanitizeOnShutdown", false);
user_pref("privacy.clearOnShutdown.history",     false);
user_pref("privacy.clearOnShutdown.sessions",    false);
// days until history entry expires
user_pref("browser.history_expire_days",         14);
// max sites (default 40,000)
// http://kb.mozillazine.org/Browser.history_expire_sites
user_pref("browser.history_expire_sites",        2000);
// max history entries (default 20,000)
// http://kb.mozillazine.org/Browser.history_expire_visits
user_pref("browser.history_expire_visits",       2000);
// required for pentadactyl autocompletion
user_pref("browser.urlbar.autocomplete.enabled", false);
// don't ask me where to download every time
user_pref("browser.download.useDownloadDir",     true);
