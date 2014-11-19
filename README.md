**Dotfiles**

These are my dotfiles. Some are pretty heavily commented. If you have interest in keyboard ergonomics and remapping, they may be of some use. This repo is pretty incomplete at the moment. The documentation is mainly to allow me to keep track of things, so it may be somewhat incomprehensible if you are not well versed in the ways of the himalayan sand frog. Also note that I am not a programmer and some of my scripts are probably done or written badly.

See `aesthetics/` for pretty pictures. Everything else is boring and tailored for my bizarre workflow.

## Pictures:
![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/keyboard_ftw.gif "Words of wisdom from Tatsuya")

See aesthetics for more info on theming, panels, etc.
![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/clean.png "SCREENSHOT")

Visualizations in ncmpcpp and album art of current song in ranger (outdated **see aesthetics/ for pretty things!**) both opened from vimus (see below and `bin/music`)
![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/dirty.png "SCREENSHOT")

# Goals for Configuration & Workflow:
- Increase efficiency and speed; cut wasted time and movement; get the computer to work with instead of against
- Reduce/eliminate hand, pinky, and wrist pain

## Acheiving This (General Principles)
- Arch Linux is the choice of distro
  - AUR (I use a lot of packages not in the official repos; bleeding edge); less time spent dealing with program installation; helps with scripting post-install setup
  - No default gui programs or WM/DE (don't have to remove DE, since I don't use one)
  - etc.
- 99% mouseless setup (1% is activity specific (gaming, photo editing) or when feeling lazy)
  - Use of keyboard friendly programs (vim (or emacs/evil), pentadactyl, ranger, decent wm, and terminal programs)
  - When there is not an existing keyboard friendly solution, hack one together with macros or faking cursor movement and clicks (or actually write software.. eventually)
- Navigation should be seemless, out of the way, and essentially instantaneous
  - No extra time should be spent getting to the text you want to edit (vim and plugins like sneak, seek, easymotion, or ace jump, etc.)
  - No extra time should be spent getting to the the tabs, workspace, window, etc you want to (proper bindings for window manager, tmux, ranger, vim, firefox, etc.)
  - Use of context based mappings; no extra keys should be used; since navigation is a common action it should require only the home row in most circumstances
- Use the fewest number of comfortable keystrokes to perform all actions
  - Use modes and letter bindings in all situations where multiple consequtive actions are frequent (e.g. browsing)
  - Use prefix keys, simple chords (e.g. vim-arpeggio), or well placed thumb modifiers where usually only one or two actions take place as at a time (e.g. window management)
  - Use two and three letter aliases for most programs (or have run automatically in proper context); balance between brevity and clarity/memorizability
  - Better text input; write language/sentences in shorthand with text expansion (e.g. autokey or iabbr) or use steno/plover
  - Most comfortable keys to reach should be personalized for most frequently used actions (e.g. \<leader\> should be space or a letter)
  - Automate as much as possible; use smart completion and expansion as much as possible (snippets, autocompletion and autogeneration)
- Eliminate usage of hard to reach pinky modifiers and straighten wrists
  - With modal interfaces, control and many modifier bindings are obsoleted; they can still be remapped and used for other purposes
  - Wide mod to keep wrists straighter and more easy access to ralt/altgr or whatever thumb keys exist
  - Dual role keys when possible; use caps as modifier if needed
  - Better Hardware: thumb cluster or split up spacebar (japanese keyboards) and split keyboard (preferably vertically staggered and mechanical)
    - This allows for remapping all modifiers to thumb as well as other keys (backspace, enter, shift, and better alt location (misc modifier) in addition to a backup wm key)
    - Even if not split, the ISO layout allows for a wider wide mod
- Eliminate motion away from the home row (particularly horizontal movement that requires hand repositioning)
  - The more frequent the action, the better position it should have; Window management and tab/split/file navigation can be done with no modifiers and using mostly just the home row
  - Again, don't waste time switching to and using the general inferiority that is the rodent
  - Remap symbols, backspace, enter, tab, etc. based on frequency of use (for symbols, can, for example, use letter chords or 1+ letters with iabbr if thumb keys not available for symbol layer)
  - Rely on layer locks (both temporary and automatically escaped (types of prefixes)) and macros when beneficial (see tmk firmware for hardware solution)
    - e.g. you type in multiple languages; lock an unmodified layer for another language instead of using modifiers for special characters (this again with the modal spirit); set up a key to change software keyboard layouts instead if hardware remapping is not available/viable
    - e.g. you don't use qwerty and games don't allow for remapping; lock layer/switch to "mode" for wasd/esdf and such
  - Use of arrow keys, home, end, pageup, etc. is obsoleted by modality (or letter chording) in many cases; otherwise remap navigation keys to home row with caps or thumbkey (less desirable)

# For More Information:
- Will add documentation of the specifics once this has been cleaned up

See [my blog](http://angelic-sedition.github.io/).

# Working On
## Right Now
- Cleaning up all my config files and adding them here (specifically mutt and many related config files)
- Making long xkb configs..
- Perfect post-install script (ideally, installing os, setting up configuration, folder structure, programs installed, etc. to 99% similarity of past install should take under 30 minutes of the user's direct attention)
- messing around with a reversed (front is back) keycool 84 for thumbkeys; testing viability
- Lose virginity to a half foreign mixed race walrus of high status and social respectability
- Messing around with insert mode commands and chording (vim-arpeggio); still considering the potential viability of plover

Now completed:
- Script to display album art in terminal split (see below and `bin/albumart_display.sh`)
- Got Pterosaur working

## Of Less Importance
- Design a better compact keyboard (see [will add]; 60% (no numpad and such); cherry brown switches (quiet; nkro for plover); non-horizontally-staggered?; possibly split or with irregular shape (vertical stagger); thumb keys; rows not tilted upwards (flat); tmk firmware compatable; etc.)
- Mess around more with Emacs (meh)
- Make or coerce someone into making an actually decent dual role program/script for GNU/Linux

# Cool Things I've Stolen
## Use Functions in Ranger
I use this mainly for things like image rotation, git (not so much), and file extraction; ranger serves as a nice interface in many cases. I've only found this to work for functions and not aliases.
The downside of this is that the shell command is much slower if you have a lot of functions. Because of this, I have switched to sourcing a separate file instead of my whole fucking `~/.zshrc`. You can of course, make bindings to specific 'shell something somethings' instead.

[source](https://bbs.archlinux.org/viewtopic.php?id=93025&p=34)
In `commands.py`, replace with the following under `class shell(Command):`

	self.fm.execute_command('bash -c "source /path/to/file;' + command + '"', flags=flags)

# More Interesting Parts of My Workflow:
## Create a Modal Interface For Programs That Don't Support Rebinding
There already exist programs that are modal by default or only need one set of letter bindings (tui programs such as ncmpcpp, vim, evil for emacs (and other less impressive things like vintageous for ST), pentadactyl for firefox, scripts for irssi and weechat, bash readline and zsh vi mode, tmux, and window management with sxhkd or xchainkeys for bindings (though seperately modal bindings aren't as important or even really desirable in the last two cases)).

However, there are many examples of programs that do have extensive keyboard shortcuts that could potentially be useful if their default bindings weren't oriented towards masochists (e.g. photoshop). For some programs, the few available shortcuts can still be massively useful when implemented in vim-like modes (e.g. Libre Office).

Solution: Rebind keys to fake the existing keyboard shortcuts
[Video Demonstration With Libre Writer](http://youtu.be/iB1fCASlpY8)

[Explanation](http://forum.colemak.com/viewtopic.php?id=1817)

This solution is restricted to X currently (though something similar could probably done with AHK). It makes use of xchainkeys for the modal bindings and xdotool and xsendkey to fake the necessary keyboard input. A potentially "software independent" solution would be to use tmk firmware to make layers with macros and keys for "mode" (layer) switching. I have not been able to test this.

See `remap/.xchainkeys` for an example configuration for Libre Writer. Since I've started using LaTeX instead for the most part, I haven't done anything else with this, but I think that it would be more desirable to have the modal interface automatically started (setting up and deconstructing bindings on window change) for the program it is being used for (using bspc --subscribe and awk to run a bash script on window change that checks if the current window is, for example, Libre Office).

## Make Any Terminal Emulator Dropdown Regardless of Window Manager
Sure there's guake, tilde, xfce4-term, yakuake, finalterm (*throws up*) and solutions for urxvt (-pe quake), xterm (yeahconsole), and specific window managers (using wmctrl, scratchpad solutions, etc.). The benefit of this is that the idea can be adapted to different window managers and terminal emulators.

When I switched from primarily guake to termite, I wanted to find a way to use termite as a dropdown term. Originally, I decided to use tmux sesison and just close the window or open a new one and reattach.

[Video Demonstration](http://youtu.be/3yhX_y1VdWE)

See `not_in_use/my_dropdown.sh` for the script.

I recently came across a simpler, faster solution that uses xdotool's windowunmap. Anyone interested in adapting the script will still have to deal with resizing/window placement, but it eliminates the need to actually kill the terminal and reattach with tmux (which can get slower with high RAM usage). It's also a true toggle unlike the previous script which requires an extra hotkey press to close in the case that it loses focus without special focus rules in place or some other workaround. It also fixes the problem from the previous script where ranger would crash upon closing the terminal with image preview enabled.

See `bin/hide_show.sh` for my modified version of [quiv's mapunmap script](https://bbs.archlinux.org/viewtopic.php?pid=1436909#p1436909) and [this blog post](http://angelic-sedition.github.io/blog/2014/07/19/make-any-terminal-emulator-a-quake-style-dropdown/) where go into more detail.

## Stream Any Video in MPV
Existing solutions for playing videos in the player of your choice (e.g. mplayer or vlc) are limited in what they work with. There are quite a few programs that allow this for a few sites such as youtube and daily motion (consider as youtube-viewer and using quvi), but I'd rather use mpv with all its features (keybindings for screenshots, seeking, etc.) than send just fake clicks to pause and play videos where things like [noflash](http://www-users.cs.umn.edu/~oztekin/software/noflash/) don't work.

This is a relatively simple thing to do in actuality. The reason existing solutions are site specific is because they operate based on the site link. Mpv will have no problem playing pretty much any video if you pass it the direct link, so all you have to do is set up a script to get that link.

There's almost certainly a much better way to do this, but I only know how to get this link manually: you open up firebug (or control+shift+j in chrome) and go to the net/media tab. When you play the video, the direct link will show up. What I've done is scripted the opening of the firebug window, the clicking on the video to start it, the clicking on the firebug window to copy that link, and then the opening of the link in mpv. This is mouse location dependent; the areas that need to be clicked will depend on screen size, window size, and firebug window size (all of these are constant for me but may be a problem if you use a floating wm; then again, if you're using a floating wm, you might not care about having to manually copy the link in the first place). I use this extensively, and it works quickly and consistently for me for almost every site/video player I've tried. A nice thing about firebug is that there is a wide range where you can click beside and below the media popup where it will allow you to copy the link.

Requirements: Pentadactyl with firebug plugin, MPV, and Firefox
Some problems: Occasionally the video will quit in the middle or the buffering will be slow (it's the site's fault). For some sites, the played video won't be detected (I've only encountered this on two websites out of the 25+ I've tried).

The firebug plugin for pentadactyl will not be able to open firebug if it has not been opened yet in the firefox window. I originally got around this by faking the key combo to open the console with xdotool, but realized that pentadactyl's emenu can be used instead. This fixes the problem where the binding had to be used twice to open the firebug console and get it to the right tab. This also makes it easier to setup autocommands to open firebug for specific sites. Another nice thing about firebug is that once you've opened it for a site in a window, it will open whenever you navigate to that site in that firefox window.

See 
`bin/firebug_fake_mouse.sh`
the corresponding section in my `.pentadactylrc` (search undescore MPV)

## Use Ranger Instead of Default GUI Popup for File Saving
Pentadactyl already has :w and ;s which allow for typing out file path with tab completion. This is cumbersome especially if you have as large a folder structure as I do. I used to just use an alias to open ranger in my downloads folder and save there automatically. I find even then I don't always get around to moving stuff, so now I have an autocommand to send the file name on download to a script which will open ranger floating and pass the file location to ranger's --selectfile and cut it (see `common/bin/ranger/ranger_browser_fm.sh`).

See [this post](http://angelic-sedition.github.io/blog/2014/04/30/using-ranger-for-downloads/) for more detailed information and other possibilities.

I've found that pentadactyl's "upload file: " when you hint an upload link only works in many cases. Right now, I do image uploads from the commandline and paste in the file location (after yanking it in ranger) to the upload gui when I have to use it (see the above blog post for more information).

## Tabs Outliner Replacement
One reason I stuck with Chromium for so long is because of this extension. I used to use things like pocket, evernote webclipper, other read later extensions, and even bookmarks for anything I wanted to look at later. I found it better to just save links in an organized structure with TO (if I didn't plan on looking at them for a while) and save anything I wanted to keep as html and incorporate it into my folder structure. I thought of several possibilities for replacing this functionality with pentadactyl (tab groups and session saving; stuff with bookmarks) and decided to use vimwiki as in interface for saved links. Basically, I created a script that saves the current link to a .wiki file by the title of the argument you give it. Saving all tabs can be done with pentadactyl's :tabdo command. Another binding opens an instance of vim for the index.wiki file. Enter opens links. I still need to add automatic addition of new .wiki files to the index.wiki file. I may work on automatic naming based on tab groups similar to what I do with unite sometime in the future. I may add a more "tree style" like structure in the future.

Like TO, when the window is closed (with a special d binding), the link will be deleted from any of the .wiki files it is in.

See `bin/to.sh` and `bin/to_win.sh` as well as `browsing/.pentadactylrc.`

## Block Layout in Vim and Example of Context Bindings
Will add more explanation.. see `navigation.vim`, `navigation.penta`, `groups.penta` (site specific bindings), etc.

## Vim as a Clipboard Manager
I've tried quite a few clipboard managers without liking any of them. What I really wanted was one with vim bindings, so I ended up deciding just to use vim. This doesn't actually work if vim doesn't become focused before changing the clipboard contents (which is what I want, I generally don't see the purpose of a clipboard manager).

Also, y and p are my universal copy paste bindings. I don't use c-v and c-c, and I have everything go to the system clipboard (+ register). I have these bindings set up for pentadactyl, zsh, weechat, vim, tmux, etc. I have also set up bindings for pasting into command mode and insert mode in pentadactyl and vim (.yp expands) and am messing with letter chording. Unite can essentially act as a clipboard manager while simultaneously replacing yank stack/ yank ring. I don't really need the complex functionality given by either; I just want a list I can scroll through and copy/paste from with vim bindings, and Unite does this well.

There's also easyclip, which I find nicer than yank stack or yank ring even though I don't use it

Search underscore clipboard in my `.vimrc`.

## Termite Link Hinting and Remapping:
Currently, termite does not support rebindings from the config file. I use colemak and prefer tmux copy mode, so this isn't that much of a problem for me. However, I really prefer termite's link hinting to things like urlview and urlscan. I usually only use link hinting in weechat, so I've bound f in normal mode (see my vimode.py fork) to `bin/link_hint.sh` to fake the key combo necessary to open the url hint window. I have a zsh binding, and for everything else I've bound it to tmux prefix-f (which I'll probably never use).

## Display Lyrics and Album Art With MPD (glitchy)
Before I used ncmpcpp, I used cmus. I like the idea of having vi-like key bindings to begin with, but I could never get scrobbling working and like mpd a lot. However, to be frank, ncmpcpp has the worst binding capabilities of any tui program I use extensively (it's really pitiful), so I've recently switched to [vimus](https://github.com/vimus/vimus). I had some trouble installing it at first, and though it may not have as complex a ui as ncmpcpp, it has all the functionality I need as well as some interesting default bindings (like visual yanking and pasting of songs and appending/insertion of songs to the playlist) and support for multikey bindings.

Vimus also supports bindings to shell commands (it doesn't have things like a tag editor, so you can set up bindings to use external programs). I've set up a binding to open a tmux split and download the album art for the currently playing song if it doesn't exist using [artget](https://github.com/berkoz/artget) and then display it in a tmux split using w3m. This will work in terminal that support w3m's image display (xterm, urxvt, termite, terminology, gnome-terminal, etc.). To use this with a mpd client other than vimus that does not support bindings to shell commands, a tmux binding will be necessary, or the script will have to be run manually in window 1 split 1 (/path/to/script run_script_in_split).
I now have it working so that it will update the album art without manually hitting a keybinding again. The displaying of the image may be more glitchy depending on the terminal emulator. Some glitches with tmux when viewing images include the image being displayed in other splits if left displayed when changing (goes away when you press a key) and the image not being fully visible (a black bar).

I've also bound a key to display the lyrics of the currently playing song in ncmpcpp in a tmux split.

See `bin/music`

## Home Row Window Mangement (Eliminate the Window Management Binding Layer)
See `.vimrc` and `.lesskey` for examples.

For me, window management is pretty much split between tmux and bspwm. Bspwm takes care of all my gui windows (and occasionally a terminal window), and tmux takes care of all my terminal sessions, windows, splits, etc.

The idea of modal window management has interested me, but modal window management isn't really efficient when most of the time you only execute one wm command (it just requires an extra key for escaping as opposed to using a prefix key). It introduces other problems as well. Escape can't be used to enter this "window management mode" (with sxhkd this would make escape lose functionality everywhere else). Unlike in vim, "normal mode" would be infrequently entered and immediately exited. Although I am a fan of modality, I do not think having modes within modes does anything other than overcomplicate things.

Instead of trying to mirror this functionality, I've found it most efficient to eliminate window management as a separate entity and build it in to all my programs just as I would set up the same (or similar) bindings for split navigation for different programs.

For optimally comfortable and distinct window management bindings, I would need 3-4 dedicated thumb keys (2 for bspc; 2 for tmux; in each case 1 where selection is two home row key presses and another where desktop/window(tmux) navigation and movement is 2 home row key presses). I don't have that many to spare and would rather use letter bindings.

I decided to test out setting up letter bindings for fun, but because it actually works fairly well, I've been using this regularly lately.. it makes room for other important keys instead of having as many specifically for window management, though I have not gotten rid of my wm key (super).

Building bindings into every program instead of using sxhkd and a regular tmux prefix key has the following advantages:
- The keys used can be on the home row (better position; 90% navigation is now home row)
- The same physical keys can be used in each context (reducing the total number of needed keys) whereas if super is being used for bspc with a hotkey program, it cannot also be used for tmux
Disadvantages:
- Initial setup time (temporary)
- You lose two keys for program specific bindings (not a big problem for me)
- Not as viable if programs that don't allow this sort of rebinding are being used (I'm not using any)
- Possible slowdown (maybe if you have hundreds of buffers and tabs in gvim)

My four most used gui programs (gvim, firefox, mpv, and apvlv) all allow for bindings to terminal commands as well as multikey bindings (thanks to wm4 for recently implementing this in mpv!). I decided to try to eliminate the "wm layer" and build parallel wm bindings into the normal mode of each program. Colemak "r" becomes "redraw," "resize," etc. and colemak "s" becomes select (two keys I don't use much anyway and can easily get a lot more out of with a prefix key for window management than a single operation key). This means window management is now "builtin" to each program with the illusion of universality. If this doesn't make sense, basically instead of having to use a harder to reach prefix or modifier key for window management like super, alt, grave, etc. a letter can be used instead.

This probably won't be as useful for anyone who uses a lot of gui programs (unless you are making extensive use of hooks to setup and get rid of modal hotkeys on window switching). For me, window management is something that should be out of the way and take essentially no time, and this worked a lot better than I thought it would (it replaces 95% use of a dedicated wm key for me). The difference between pressing "super+5" and rd (qwerty "sg") may not seem to be a big deal, but it's been quite noticeable to me. As for delay, I've only experienced slight lag when gvim was being slow. It should be noted that "-ex" and not "-builtin" should be used for pentadactyl bindings (builtin interpretting as keys is much slower and will cause a noticeable delay).

I'd also like to try this with chording at some point (pressing qwerty s + {h,j,k,l} simultaneously will do window switching). However, very few programs support this kind of thing (vim and emacs). This gets pretty messy without a universal way of doing things, and it may just be better/cleaner to do window management with thumbkeys

I initially thought this would be impossible to replicate in the terminal but spent a day testing it, and I have now built tmux bindings into all of tui my programs (vim, zsh, less, weechat, ranger, emacs, mutt, tig (which used to have just gotten multikey bindings), w3m, and vimus). The only downside of this is that zsh bindings obviously won't work if you have something running. On the other hand, this isn't that big of a deal because tmux allows use of a prefix key on a layer (e.g. mine is grave, which is mode_switch f for me). For things like interactive python and ghci, I sometimes like to run them in VimShell, so I can still use modal keybindings. Eshell how I envy you.

See the README in the remap folder for more info.

## Make Gifs in MPV
I thought it would be efficient to set up bindings within mpv to create gifs. Now that mpv has an a-b loop (issue #1241), I've gone back to using a script (`ffcut`) that first cuts part of a video out and then optionally makes a gif from that part. I have three keys set up in mpv for this: two to adjust the start and end positions and one to execute the script. To deal with videos being streamed, I've added a `-d` flag that will download the video using aria2 before cutting out a section (this assumes mpv has been passed the direct link of the video; see "Stream Any Video in MPV" above). Note that a-b looping still works when streaming, so while it will take longer to create the cut video, it won't take more key presses (the time points can be marked prior to the download). When downloading the video is a hassle, one can always use a start and stop hotkey (for example, bound with sxhkd or in mpv) to screen record what's playing in mpv instead.

The `makegif` script is just a wrapper for ffmpeg, imagemagick, and optionally gifsicle that takes a video, makes frames from it, and then creates an optimized 600 width 10 fps gif. It has much improved. For example, if the output gif is not satisfactory, one can simply use the frames already created and try different options:
```bash
makegif <path/to/video>
# notice that there are some extra frames at the end; go into ~/Move/gif/frames and delete a few at the end
makegif -u
# use max optimization with gifsicle and increase fuzz percent
makegif -u -O 3 -z 1.8
# changing fps or width values requires remaking the frames (unless you want something sped up/slowed down):
makegif -w 800 -O 3 -f 15 -o mygiff.gif <path/to/video>
```

An example gif with default settings (made within mpv):
![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/example.gif "Tigre-sama Catches an Arrow")

See 
`scripts/bin/mpv/`
`media/.mpv/input.conf`


# Credit
If I've taken anything from anyone else's config file, I've almost always put the url in the relevant config file.

Some general stuff:
Credit to vaskozl for his thread on not using the mouse, which is one of the main reasons I ever took interest in any of this shit.
Credit to DreymaR and lalop for inspiration on layout stuff after I switched to Colemak and to bunnfly for colemak vim config (all from Colemak forum).
Credit to [this site](https://osrc.dfm.io/angelic-sedition/) for making me feel amazing about myself, even though the statistics are misleading as hell.

For now I'll just list a few devs whose work I admire:
- baskerville/bloom
- Shougo
- tpope
- kana
- junegunn
- codestation
- Coldbird (from wololo)
- hasu (on geekhack) for tmk
- sol and haasn
