**Dotfiles**
These are my dotfiles. Some are pretty heavily commented. If you have interest in keyboard ergonomics and remapping or Colemak, they may be of some use. This repo is pretty incomplete at the moment. The documentation is mainly to allow me to keep track of things, so it may be somewhat incomprehensible if you are not well versed in the ways of the Himalayan sand frog. Be wary of my scripts. I'm getting better at bash, but a lot of them are still poorly written.

See `aesthetics/` for pretty pictures.

## Pictures:
![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/keyboard_ftw.gif "Words of wisdom from Tatsuya")

See `aesthetics/` for more info on theming, panels, etc.
![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/clean.png "SCREENSHOT")

Visualizations in ncmpcpp and album art of current song in ranger (outdated) both opened from vimus (`scripts/bin/music`, outdated)
![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/dirty.png "SCREENSHOT")

# Goals for Configuration & Workflow:
- Increase efficiency and speed; cut wasted time and movement
- Reduce/eliminate hand, pinky, and wrist pain

## Achieving This (General)
- One time configuration
  - Software should be configured in human editable plain text files stored in a central location; this prevents the need for slow gui menu interaction or problems finding and backing up one's configuration
  - More complicated/customizable software should be configured in an actual programming language (e.g. elisp)
- Arch Linux is the choice of distro
  - AUR: I use a lot of packages not in the official repos or often want the latest git version of something; this means less time spent dealing with program installation and helps tremendously with scripting the post-install setup
  - No default GUI programs or WM/DE that I would uninstall
  - Text based installation (easily scriptable)
  - Also looking into NixOS for its nice central, declarative config file and for a more easily reproducible setup with less manual interaction
- 99% mouse-free setup; 1% is activity specific, such as for gaming or when feeling lazy
  - Use of keyboard friendly programs
    - Text editor: vim or emacs/evil
    - Browser: firefox w/ pentadactyl/vimperator or something like qutebrowser
    - File manager: ranger or dired
    - Tiling WM: bspwm, i3, herbstluftwm, stumpwm, etc. (note that all these can be interacted with via shell commands)
  - When there is not an existing keyboard friendly solution, hack one together with macros or faking cursor movement and clicks (or actually write software.. eventually)
- Navigation should be seemless, out of the way, and fast
  - Text: vim motions; imenu and unite/helm sources such as unite's line, fold, and outline or helm swoop and occur or swiper; plugins like sneak, easymotion, etc.; case specific plugins like lispy and functions like worf-goto or helm-org-in-buffer-headings; buffer local marks
  - Files: ag or pt; again, helm or unite (or fzf) with mru, current project, open buffers, and locate sources; 'quickmarks' for most used files in a workgroup and quickmarks for most used directories in the file manager; use of tools like fasd and blscd on the command line (or if using ansi-term or eshell, syncing the cwd)
  - Tabs, workspaces, windows: context bindings (see below)
- Automation
  - Text manipulation: text objects; [smart-parens](https://github.com/Fuco1/smartparens); use of tools such as snippets and abbreviations; use of other auto-generation plugins such as [emmet-vim](https://github.com/mattn/emmet-vim), [tiny](https://github.com/abo-abo/tiny), and [vim-transform](https://github.com/t9md/vim-transform); keyboard macros, iedit, multiple cursors, etc.
  - Startup: window manager, programs, tmux sessions, previous vim/emacs session, previous browsing session, etc.
  - Semi-automatic backups: text files locally with text editor (and git); to external harddrives; offsite with cron (manual authentication)
- Use the fewest number of comfortable keystrokes to perform all actions
  - Use modes and letter bindings in all situations where multiple consecutive actions are frequent (e.g. browsing)
  - Use prefix keys, simple letter chords (e.g. vim-arpeggio and emacs key-chord), or well placed thumb modifiers where usually only one or two actions take place as at a time (e.g. window management)
  - Frequently used programs should be opened automatically (e.g. on a specific desktop/workspace) and bound to a key, not opened with a launcher
  - Better text input; write language/sentences using chording (steno); a long term goal
  - Most comfortable keys to reach should be personalized for most frequently used actions
- Eliminate usage of hard to reach pinky modifiers and straighten wrists
  - With modal interfaces, control and many modifier bindings are mostly obsoleted; they can still be remapped and used for other purposes; note that I don't have some irrational hate for control as an arbitrary modifier; I hate its position on the standard keyboard; it can, of course, be remapped, but then again, I find most of the default functionality of control to be useless
  - With home thumb-key modifiers and maybe home-row modifiers (with smartly implemented dual-role keys) or letter chording, pinky modifiers are obsoleted entirely
  - Use a wide layout mod to keep wrists straighter and allow for more easy access to ralt/altgr or whatever thumb keys exist on standard (shitty) keyboards
  - Dual-role keys when possible; use caps as modifier if needed (preferable in comparison to other pinky modifiers but still undesirable)
  - Use better hardware if possible: thumb cluster or split up spacebar (Japanese keyboards) and split keyboard (preferably vertically staggered, mechanical, tented, etc.); support for tmk firmware
    - This allows for remapping all modifiers to the thumb as well along with other frequently used keys (backspace, enter, shift, and alt/win (misc or wm modifiers))
    - Even if not split, the ISO layout allows for a wider wide mod
- Eliminate motion away from the home row (particularly horizontal movement that requires hand repositioning)
  - Again, don't waste time switching to and using the general inferiority that is the rodent
  - Remap symbols, backspace, enter, tab, etc. based on frequency of use
  - Rely on layer locks (both temporary and automatically escaped (basically a prefix key; think sticky keys and dead keys)) and macros (one key executes multiple keypresses) when beneficial (see [tmk firmware](https://github.com/tmk/tmk_keyboard) for hardware solution)
    - e.g. you type in multiple languages; lock an unmodified layer for another language instead of using modifiers for special characters (this again with the modal spirit in mind); set up a key to change software keyboard layouts instead if hardware remapping is not available/viable
    - e.g. you don't use qwerty and a game doesn't allow for remapping; lock layer/switch to "mode" for wasd/esdf and such; basically I mean an alternate layout with key bindings to revert it
  - Use of arrow keys, home, end, pageup, etc. is obsoleted by modality (or letter chording) in many cases; otherwise remap navigation keys to home row with caps or thumbkey
- Use emacs for as much as possible: the more I use emacs, the more convinced I'm it provides a an unmatched interface for a lot of software (you get vim text selection/motions with evil, helm-occur, multi-key and key-chord bindings, a consitent way of doing settings and keybindings, scriptability with elisp, etc.); with evil, many programs are far more "vimmy" than vim inspired standalone programs

# For More Information:
I'll add documentation of the specifics once I've cleaned things up.

See [my blog](http://angelic-sedition.github.io/).

# Working On
## Right Now
- Cleaning up all config files and adding them here
- Cleaning emacs init file and using emacs for other things (e.g. file management and mail) when it makes sense
- Making my bash scripts suck less
- Testing post-install scripts; ideally, installing the OS, setting up configuration, folder structure, programs installed, etc. to 99% similarity of past install should take under 30 minutes of the user's direct attention if not even less

## Hopefully
- Buy one of the many new ergo keyboards with a thumbcluster (e.g. keyboardio) and set up tmk
- Nix configs
- Make or get someone else to make a hotkey program that supports letter chords and/or smartly implements letter keys as modifiers
  - Hoping a good chording or dual-role implementation can be beneficial, even with good hardware
  - Especially with the standard keyboard, this is important for eliminating pinky modifiers (shift hurts my pinky; I often have to use other fingers to press it when typing for extended periods of time, and my hand still ends up hurting)

# Cool Thing I Stole
## Use Shell Functions in Ranger
I use this mainly for things like image rotation, image uploading, git (not so much), and file extraction; ranger serves as a nice interface in many cases. I've only found this to work for functions and not aliases.

The downside of this is that the shell command is much slower if you have a lot of functions. Because of this, I have switched to sourcing a separate file instead of my whole `~/.zshrc`. You can of course, make bindings to specific 'shell something somethings' instead, but this is nice for more complex functions that don't quite warrant their own script.

[source](https://bbs.archlinux.org/viewtopic.php?pid=895749#p895749)

In `commands.py`, replace with the following under `class shell(Command):`

	self.fm.execute_command('bash -c "source /path/to/file;' + command + '"', flags=flags)

Note that you shouldn't use the line in the thread because the quoting order will kill ranger's handling of escaping.
See `media/.config/ranger/ranger_functions`.

# More Interesting Parts of My Workflow:
## Make Any Terminal Emulator Dropdown Regardless of Window Manager
I've tried to create a much more generalized version of my dropdown script that works well with more window managers and has extra functionality that other dropdowns don't have.

See [tdrop](https://github.com/angelic-sedition/tdrop) and make an issue if there isn't already floating support for your wm.

## Use Ranger Instead of Default GUI Popup for File Saving
Pentadactyl already has :w and ;s which allow for typing out file paths with tab completion. This is cumbersome especially if you have as large a folder structure as I do. I used to just use an alias to open ranger in my downloads folder and save there automatically. I found even then that I didn't always get around to moving stuff, so now I have an autocommand to send the file name on download to a script which will open a floating term with ranger running and pass the file location to ranger's --selectfile and cut it (see `scripts/bin/ranger/ranger_browser_fm.sh`).

See [this post](http://angelic-sedition.github.io/blog/2014/04/30/using-ranger-for-downloads/) for more detailed information and other possibilities.

I've found that pentadactyl's "upload file: " that appears when you hint an upload link to be nice but not a universal solution (if there is no such link to hint). Right now, I do image uploads and mail attachment from the commandline/ranger (see `media/.config/ranger/ranger_functions`). When I have to use the upload gui I often paste in file locations after copying them in ranger.

## Block Layout in Vim and Example of Context Bindings
Using tabs is only useless if you're trying to use one buffer per tab (in an editor that supports buffers/a bufferline). I use tabs (or workgroups in emacs) as workspaces. I setup them up with names so I can see which number tab corresponds to what subject and can easily jump to them with <space><home row>. In vim, I use a script that also sets up custom key bindings for different tab names. For example, I use `,` as a prefix key to jump to specific files depending on tab name. I have a general set of these "quickmarks" and specific ones that either correspond to files by frequency of use (,f ,s ,t for first second third) or by name. This allows for key re-use. <space>p will do different things with Unite depending on the tab name/context. For example, in my "bin" tab, it will open the file unite source in ~/bin. I also have different bindings to execute certain shell commands depending on the tab.

This drastically reduces the time it takes to get to a specific file as well as the complexity of key bindings to execute certain actions. This is the order of preference for me when it comes to navigation of files:

1. navigate to open buffer or quickmarks (2-4 keys; 2 for right tab/workgroup, maybe 2 for getting correct pane or using a quickmark)
2. interactive search of open buffers and mru files and current dir (unite, helm)
3. locate or find search (or maybe ag if searching for by contents) (unite, helm)
4. file manager w/ quickmarked dirs as last resort (ranger if in terminal vim; otherwise dired or vimfiler)

While I find things like fasd and vimfiler to be cool, I never really find myself using them because they actually end up being slower. On the commandline, fasd is probably the best option for non-quickmarked dirs that aren't close. I've kind of made it a bad habit to use ranger or blscd with quickmarks and f<chars> for directory navigation when fasd would be better. However, for opening files and the other actions it has, something interactive like fzf or ranger is a much better idea in my opinion. Using a fasd command in ranger, I think ranger might always be preferable to the commandline for navigation because you still have access to shell commands and get folder visualization as a bonus. Still, I don't deal with a huge number of folder, and manually added quickmarks/bookmarks for directories take care of most of my problems. Slow speed when loading certain folders in ranger may be an issue though (my only annoyance with it).

I don't currently have directory specific quickmarks for zsh, though I may add them in the future (if I've not mostly switched to emacs/(w)dired). Memorization is the trade-off though, and the less you use a quickmark mapping, the more forgettable it is. That's also why I find open buffers/tab layouts (saved in a vim/emacs session or setup automatically) and f s t key bindings for most frequently used files to be a nice way to not have to memorize things while keeping keystrokes to a minimum.

In vim/emacs, I am using 'm' as a prefix key for whatever major mode/filetype I'm currently in. In org mode, m<keys> performs org mode navigation, clocking, todo, etc. actions. In code files, I use m<keys> to compile and run the current file in the shell split on the right (vimshell or eshell) as well as for repl interaction, error navigation, etc. I plan on using m<home row> to switch between channel buffers if I ever start using erc or circe.

As for other, non-vim/emacs examples, I have context bindings for empty vs. non-empty workspaces/desktops. On empty desktops, I have sxhkd automatically restarted with a custom config for single key mappings for opening programs and switching desktops. Why use more keys than necessary? The transition time to get used to the difference was insignificant, so I don't think consistency is of any benefit here.

In firefox, I also use the space bar as a prefix key for tab navigation. I bind <space><home row> to a command that will go to tab 1-10 in the curent tab range (e.g. <space>a on tab 24 will go to tab 21). I also have key bindings to switch to specific tab groups and setup a few custom key bindings depending on tab group. I haven't done much with this though since TabGroupie works very inconsistently. Pentadactyl's groups (not related to tab groups) are probably the best example for taking advantage of different contexts. I use them to set up site-specific key bindings. For example, on reddit, I pass through keys for use with RES and also add custom goto bindings with the prefix g for going to specific subreddits. I use this to setup more convenient zoom bindings on image urls. This also allows setting up custom key bindings for sites that have non-configurable key bindings.

The best example I've seen of key re-use lately is [lispy](https://github.com/abo-abo/lispy). Org speed keys and [worf](https://github.com/abo-abo/worf) are also good examples.

See 
`vim/.navigation.vim` and `emacs/.emacs.d/navigation.el` 
`common/.config/bspwm/bspwmrc` and `remap/.config/sxhkd/empty_sxhkdrc` 
`browsing/.pentadactylrc` (search `relative-move`) 
`browsing/.pentadactyl/groups.penta`

## Stream Any Video in MPV
Existing solutions for playing videos in the player of your choice (e.g. mplayer or vlc) are limited in what they work with. There are quite a few programs that allow this for a few sites such as youtube and daily motion (consider youtube-viewer and using quvi), but I'd rather use mpv with all its features (keybindings for screenshots, seeking, etc.) than send fake clicks to pause and play videos where things like [noflash](http://www-users.cs.umn.edu/~oztekin/software/noflash/) don't work. I'm not particularly fond of the mozplugger/viewtube approach either where your player is basically embedded in the browser instead of the default flash player (even if this worked with all sites.)

This is a relatively simple thing to do in actuality. The reason existing solutions are site specific is because they operate based on the site link. Mpv will already play youtube videos from the url (and those on other sites supported by youtube-dl). It will have no problem playing pretty much any video if you pass it the direct link, so all you have to do is set up a script to get that link.

There's certainly a much better way to do this, but I only know how to get this link manually: you open up firebug (or control+shift+j in chrome) and go to the net/media tab. When you play the video, the direct link will show up. What I've done is scripted the opening of the firebug window, the clicking on the video to start it, the clicking on the firebug window to copy that link, and then the opening of the link in mpv. This is mouse location dependent; the areas that need to be clicked will depend on screen size, window size, and firebug window size (all of these are constant for me but may be a problem if you use a floating wm; then again, if you're using a floating wm, you might not care about having to manually copy the link in the first place). I use this extensively, and it works quickly and consistently for me for almost every site/video player I've tried. A nice thing about firebug is that there is a wide range where you can click beside and below the media popup where it will allow you to copy the link.

Requirements: Pentadactyl with firebug plugin, MPV, and Firefox
Some problems: Very rarely the video will quit in the middle or the buffering will be slow (it's the site's fault). For some sites, the played video won't be detected (I've only encountered this on two websites out of the 25+ I've tried).

The firebug plugin for pentadactyl will not be able to open firebug if it has not been opened yet in the firefox window. I originally got around this by faking the key combo to open the console with xdotool, but realized that pentadactyl's emenu can be used instead. This fixes the problem where the binding had to be used twice to open the firebug console and get it to the right tab. This also makes it easier to setup autocommands to open firebug for specific sites. Another nice thing about firebug is that once you've opened it for a site in a window, it will open whenever you navigate to that site in that firefox window. However, I think this is a really ugly, hacky way of doing it, and hope to fix this in the future to somehow query firebug or something else instead for the direct link.

See 
`scripts/bin/firebug_fake_mouse.sh` 
the corresponding section in my `.pentadactylrc` (search undescore MPV)

## Make Gifs in MPV
I thought it would be efficient to set up bindings within mpv to create gifs. Now that mpv has an a-b loop (issue #1241), I've gone back to using a script (`ffcut`) that first cuts part of a video out and then optionally makes a gif from that part. I have three keys set up in mpv for this: two to adjust the start and end positions and one to execute the script. To deal with videos being streamed, I've added a `-d` flag that will download the video using aria2 before cutting out a section (this assumes mpv has been passed the direct link of the video; see above). Note that a-b looping still works when streaming, so while it will take longer to create the cut video, it won't take more key presses (the time points can be marked prior to the download). When downloading the video is a hassle, one can always use a start and stop hotkey (for example, bound with sxhkd or in mpv) to screen record what's playing in mpv instead.

The `makegif` script is just a wrapper for ffmpeg, imagemagick, and optionally gifsicle that takes a video, makes frames from it, and then creates an optimized 600 width 10 fps gif. It has much improved. For example, if the output gif is not satisfactory, one can simply use the frames already created and try different options:
```
makegif <path/to/video>
# notice that there are some extra frames at the end; go into ~/Move/gif/frames and delete a few at the end
makegif -u
# use max optimization with gifsicle and increase fuzz percent
makegif -u -O 3 -z 1.8
# changing fps or width values requires remaking the frames (unless you want something sped up/slowed down):
makegif -w 800 -O 3 -f 15 -o mygiff.gif <path/to/video>
```

I still need to alter it to make it easier to deal with. I couldn't find a good way to be able to seek/use the exact time instead of from a key frame, so often one has to mark a larger section then delete the pictures at the beginning and end in the output directory (which is stupidly hardcoded currently). It works though. I'm thinking GNEVE may be an alternate way to do this better.

An example gif with default settings (made within mpv):
![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/example.gif "Tigre-sama Catches an Arrow")

See 
`scripts/bin/mpv/` 
`media/.mpv/input.conf`

## Tabs Outliner Replacement
One reason I stuck with Chromium for so long is because of this extension. I used to use things like pocket, evernote webclipper, other read later extensions, and even bookmarks for anything I wanted to look at later. I found it better to just save links in an organized structure with TO (if I didn't plan on looking at them for a while) and save anything I wanted to keep as html and incorporate it into my folder structure. I thought of several possibilities for replacing this functionality with pentadactyl (tab groups and session saving; stuff with bookmarks) and decided to use vimwiki as in interface for saved links. Basically, I created a script that saves the current link to a .wiki file by the title of the argument you give it. Saving all tabs can be done with pentadactyl's :tabdo command. Another binding opens an instance of vim for the index.wiki file which I automatically populate with the created wikis. Enter opens links. I may add a more "tree style" like structure in the future or do something with org mode instead.

Like TO, when the window is closed (with a custom D binding), the link will be deleted from any of the .wiki files it is in.

See 
`scripts/bin/pentadactyl/to.sh` 
`browsing/.pentadactylrc`

## Vim/Emacs and the Clipboard
I've tried quite a few clipboard managers without liking any of them. What I really wanted was one with vim bindings, so I ended up deciding just to use vim with Unite's history/yank source. This doesn't actually work if vim doesn't become focused before changing the clipboard contents. I don't usually need more than this, but I guess you could use something like CaptureClipboard in it's own vim instance if needed. There's also [clipmon](https://github.com/bburns/clipmon) for emacs which I'll probably start using instead. There's also easyclip, which I find nicer than yank stack or yank ring even though I don't use it.

'y' and 'p' are my "universal" copy paste bindings. I don't use <c-v> or  <c-c>, and I have everything go to the system clipboard (+ register). I have these bindings set up for pentadactyl, zsh, weechat, vim, tmux, etc. I have also set up bindings for pasting into command mode and insert mode in pentadactyl and vim ('.yp' expands to clipboard contents) and am messing with letter chording.

Search underscore clipboard in my `.vimrc`.

## Termite Link Hinting and Remapping:
Currently, termite does not support rebindings from the config file. I use colemak and prefer tmux copy mode, so this isn't that much of a problem for me. However, I really prefer termite's link hinting to things like urlview and urlscan. I usually only use link hinting in weechat, so I've bound f in normal mode (see my vimode.py fork) to `scripts/bin/link_hint.sh` to fake the key combo necessary to open the url hint window. I have a zsh binding, and for everything else I've bound it to tmux prefix-f (which I'll probably never use).

This still isn't really optimal. Having to use numbers to select urls is annoying, and having to deal with multi-line urls in mutt and weechat is even more of a pain. By comparison, text selection and url opening is far better in a mail or irc client for emacs.

## Home Row Window Mangement (Eliminate the Window Management Binding Layer)
This started as something I did for fun, but I've actually found it pretty useful. See `.vimrc` and `.lesskey` for examples. I might abandon this in favour of thumbkey modifiers if I get a better keyboard.

For me, window management is pretty much split between tmux and bspwm. Bspwm takes care of all my gui windows (and occasionally a terminal window), and tmux takes care of all my terminal sessions, windows, splits, etc.

The idea of modal window management has interested me, but modal window management isn't really efficient when most of the time you only execute one wm command (it just requires an extra key for escaping as opposed to using a prefix key). It introduces other problems as well. Escape can't be used to enter this "window management mode" (with sxhkd this would make escape lose functionality everywhere else). Unlike in vim, "normal mode" would be infrequently entered and immediately exited. Although I am a fan of modality, I do not think having modes within modes does anything other than overcomplicate things. Instead of trying to mirror this functionality, I've found it most efficient to eliminate window management as a separate entity and build it in to all my programs just as I would set up the same (or similar) bindings for split navigation for different programs.

My most used gui programs (gvim, firefox, mpv, apvlv/zathura, and sxiv) all allow for bindings to terminal commands as well as multikey bindings (thanks to wm4 for implementing this in mpv!). This probably won't be as useful for anyone who uses a lot of gui programs, without doing something particularly convulted like using sxhkd as a wrapper for modal keybindings (see below). The difference between pressing "super+5" and rd (qwerty "sg") may not seem to be a big deal, but it's been quite noticeable to me. As for delay, I've only experienced slight lag when gvim was being slow. It should be noted that "-ex" and not "-builtin" should be used for pentadactyl bindings (builtin interpretting as keys is much slower and will cause a noticeable delay).

I've also built tmux bindings into all of tui my programs (vim, zsh, less, weechat, ranger, emacs, mutt, tig, w3m, and vimus). The only downside of this is that zsh bindings obviously won't work if you have something running (not a problem if you're running zsh in emacs though!). On the other hand, this isn't that big of a deal because tmux allows use of a prefix key on a layer (e.g. mine is grave, which is mode_switch f for me). For things like interactive python and ghci, I like to run them in vimshell or eshell, so I can still use modal keybindings (and things like slime). At some point in the distant future I may stop using terminals/tui software much at all in favour of emacs.

Previously I was repurposing 'r' and 's' as these prefix keys. I've for the most part switched to just using r. I've started using the `wm_action` script as a wrapper for my window management keybindings. This is kind of ugly, but it has already allowed me to get rid of some old scripts I was using. For example, I've set it up to determine whether mpv is being run in a terminal or not so that bspc or tmux key bindings are used accordingly. I've started binding keys in sxhkd to it instead of directly to bspc (e.g. to wm_action wmsel left or something) with the intent of using this to use the same sxhkd config when experimenting with other window managers. I'm also using it in the programs I am remapping 'r' for so that I can change the actions for certain keys in one place instead of 10 and have them work in whatever wm I'm working with.

I'd also like to try window management with chording or dual-roled keys at some point (pressing qwerty s + {h,j,k,l} simultaneously will do window switching). This gets pretty messy without a universal way of doing chording, and it may just be better/cleaner to do window management with thumbkeys instead.

See the README in the remap folder for more info.

## Flashcard Script
I've made a simple cli flashcard script with zsh completion. I'm probably going to trash this in favour with something like org-drill or something else like anki.

See 
`scripts/bin/flashcards` 
`terminal/.zsh/completion/_flashcards`

## Create a Modal Interface For Programs That Don't Support Rebinding
I've pretty much abandoned software that doesn't support modality and prefix bindings, but this may be useful for users of such software.

There are many programs that have extensive keyboard shortcuts that could potentially be useful if their default bindings weren't oriented towards masochists. For some programs, the few available shortcuts can still be massively useful when implemented in vim-like modes (e.g. Libre Office).

Solution: Rebind keys to fake the existing keyboard shortcuts
[Video Demonstration With Libre Writer](http://youtu.be/iB1fCASlpY8)

[Explanation](http://forum.colemak.com/viewtopic.php?id=1817)

This solution is restricted to X currently (though something similar could probably done with AHK). It makes use of xchainkeys for the modal bindings and xdotool and xsendkey to fake the necessary keyboard input. A potentially "software independent" solution would be to use tmk firmware to make layers with macros and keys for "mode" (layer) switching. I have not been able to test this.

See `remap/xchainkeys.examplevimlayer.conf` for an example configuration for Libre Writer. Since I've started using LaTeX instead for the most part, I haven't done anything else with this, but I think that it would be more desirable to have the modal interface automatically started (setting up and deconstructing bindings on window change) for the program it is being used for (using bspc --subscribe and awk to run a bash script on window change that checks if the current window is, for example, Libre Office).

# Credit
Anything I've swiped for my config files has a url.

Some general stuff: 
Credit to vaskozl for [his thread](http://forum.colemak.com/viewtopic.php?id=1731&p=1) on not using the mouse, which is one of the main reasons I ever took interest in any of this. Credit to DreymaR and lalop for inspiration on layout stuff after I switched to Colemak and to bunnfly for the colemak vim config (all from the Colemak forum).

Thanks to baskerville/bloom for bspwm and sxhkd. Thanks to kana, Shougo, tpope, junegunn, etc. for all their awesome vim plugins. Thanks to abo-abo for his awesome emacs packages like lispy and hydra. Thanks to tuhdo for his great guide on helm. Thanks to codestation for qcma. Thanks to sol, haasn, etc. for vimus. Thanks to ttzhou for setroot.
