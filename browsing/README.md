# Pentadactyl Cool Things
I'm using [this command](https://github.com/noctuid/dotfiles/blob/3f722f0a087dccd752e1dd766c0027c8082e46be/browsing/.pentadactylrc#L544) for navigating tabs in the current 0-9 range with the space key.

Since you can create bookmarks in the config file, keyword searches are nicer to set up (e.g. being able to type `:open d <search terms>` to search something with duckduckgo). You can also use yubnub as the default search engine for a similar effect.

I use the DownloadPost autocmd to use ranger for choosing a saved file's location. I may switch to using dired, since it's probably faster (with emacsclient) than waiting for ranger to load.

To go beyond just pentadactyl bindings, I've started using site specific bindings for frequently used sites to cut down on keystrokes.

This involves a combination of site specific passthroughs either for sites that have existing bindings (i.e. twitter, gmail, tumblr extended with xkit, etc.; see `groups.penta`) or possibly in combination with userscripts that add bindings like 4chanX, RES, Webcomic Reader (bindings for pretty much any gallery/comic/image site), YT Center (allows for youtube bindings without clicking on the video and the video grabbing focus), etc. See `userscript_settings`.

I also, for example, re-use the same key sequence to download different things depending on the url (comics if in an image gallery or comic, the video if on youtube, etc.).

I might be doing something similar with tabgroups if they worked consistently with TabGroupie.

# Pentadactyl vs. Vimperator
Right now, pentadactyl is the main reason I'm using firefox. There are no equivalent chromium plugins I'm close to satisfied with (including cvim). Ideally, I'd prefer to use a "lightweight" browser designed with pentadactyl like functionality built in. For me, [qutebrowser](https://github.com/The-Compiler/qutebrowser) is the only alternative browser that looks promising in this regard, and I hope to switch to it in the future since it is being actively developed.

Pentadactyl is a vimperator fork. Both are now on github (pentadactyl has moved since google code is shutting down). The differences between the two are not very clear, and the most comprehensive [comparison](https://www.wikivs.com/wiki/Pentadactyl_vs_Vimperator) I've found isn't particularly accurate. For example, it says that pentadactyl is more focused on "100% vim compatibility." I don't actually understand what this is supposed to mean, but in some regards this is just blatantly false. For example, vimperator uses "noremap" which is deprecated (in favour of "-builtin") in pentadactyl. The looks with both are quite customizable (e.g. I use hints that are styled like in vimium as opposed to the ugly default), so I don't really see any fundamental difference there. As for development, they are both being maintained at least somewhat.

Here are some specific differences I've actually found. Please correct me if anything is wrong or has changed; there are a lot of annoying naming differences between the two (e.g. ignorekeys v passkeys), so I may have missed something.

## Pentadactyl Over Vimperator
Pentadactyl has groups (I'm pretty sure vimperator doesn't) which allow you to have different settings, key mappings, etc. for different urls. I use this, for example, to have more convenient mappings for zooming when the url corresponds to an image. I'm pretty sure vimperator doesn't have this, and that's enough reason for me not to use it (unless I'm wrong).

Pentadactyl has `nmap -ex`. I don't know if vimperator has an equivalent feature (I couldn't find it) or if it is just faster at executing key-sequences (which seems unlikely). Basically processing mappings as keys (using nnoremap or nmap -builtin) can be very noticeably slow. If you want to do `nmap <keys> :some command<cr>`, it's much faster to use `nmap -ex <keys> some command`.

The `addons` command in pentadactyl will popup your list of addons with link hintable "off|on", "del", "upd", and "opt" sections. In vimperator, it just takes you to about:addons.

Pentadactyl has "if.. else" conditional support in the config file which is very nice for me (I couldn't find anything about such a feature in the vimperator documentation). I use this in my `relative-tab-move` command.

Pentadactyl supports vim's bar "|" for multiple commands, though it doesn't work with a lot of things and I haven't found it to be that useful.

Pentadactyl doesn't require a restart to be enabled. It also still has passthrough mode which vimperator sadly removed. At the time of writing, vimperator also doesn't allow you to remap certain keys (like \<space\>) which is another reason I wouldn't use it.

## Vimperator Over Pentadactyl:
Vimperator has tabgroup management commands built in (e.g. for switching tabgroups, moving tabs between tabgroups, etc.). Pentadactyl has a plugin called TabGroupie, but it's pretty broken and only works half the time. I don't know if the vimperator implementation is better, but it doesn't support renaming tabgroups I think.

Vimperator allows saving your bookmarks to different folders. The issue for this in pentadactyl has been open for years and has a high priority, but I have to doubt at this point that this functionality will ever be added.

Vimperator currently works with firefox nightly with no hassle. This has been a problem with pentadactyl lately (no nightlies; having to update the max version yourself; small fixes coming late for new firefoxes), but right now there are current nightlies for pentadactyl that supposedly work fine with the newest firefox as well. Vimperator seems to be better/faster at keeping up though.

Vimperator has coloured mode indicators, and I don't think pentadactyl does.

## No Difference
The differences seem to be pretty small for the most part. They both have autocmds, quickmarks, support for executing shell and js commands, completion (especially nice with `:b` and `:open`), caret mode, and support for setting firefox about:config settings with `set!`.

The startup time and completion performance is supposedly better in pentadactyl according to the aforementioned link. I don't really notice a difference in speed between the two; neither cause any significant slowdown for me.

According to the link, the incremental find, sanitize command, and private mode support are supposedly better in pentadactyl, but I can't really comment on this. I did see some recent commits in vimperator that mentioned improving the sanitize command though.

## Keysnail
There's also [keysnail](https://github.com/mooz/keysnail), which I fail to see the point of. It says it's tailored to emacs users (without much explanation) but provides pretty much the same functionality as pentadactyl but with a much worse (imho) configuration syntax. It does have a lot of plugins though; any keysnail users are welcome to enlighten if I am missing anything.
