These are my dotfiles. Some are pretty heavily commented. If you have interest in keyboard ergonomics and remapping, they may be of some use. This is pretty incomplete at the moment. The documentation is mainly to allow me to keep track of things, so it may be somewhat incomprehensible if you are not well versed in the ways of the himalayan sand frog.

## Pictures:
- Conky
- Bar-aint-recursive (panel and panel_bar)
- Feh to set background
- tmux powerline (will probably switch to powerline or tmuxline)

![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/clean.png "SCREENSHOT")

Visualizations in ncmpcpp and album art of current song in ranger both opened from vimus (see bin/music)
![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/dirty.png "SCREENSHOT")

# Goals for Configuration & Workflow:
- Increase efficiency and speed; cut wasted time and movement; get the computer to work with instead of against
- Reduce/eliminate hand, pinky, and wrist pain

## Acheiving This (General Principles)
- Arch Linux is the choice of distro (however I am working on duplicating my setup on Windows and OSX as well)
  - AUR (I use a lot of packages not in the official repos; bleeding edge); less time spent dealing with program installation; helps with scripting post-arch-install setup
  - No default programs or WM/DE (don't have to remove DE, since I don't use one)
- 99% mouseless setup (1% is activity specific (gaming) or when feeling lazy)
  - Use of keyboard friendly programs (vim (or emacs/evil), pentadactyl, ranger, decent wm, and terminal programs)
  - When there is not an existing keyboard friendly solution, hack one together with macros or faking cursor movement and clicks
- Navigation should be seemless, out of the way, and essentially instantaneous
  - No extra time should be spent getting to the text you want to edit (vim and plugins like sneak, seek, easymotion, or ace jump, etc.)
  - No extra time should be spent getting to the the tabs, workspace, window, etc you want to (proper bindings for window manager, tmux, ranger, vim, firefox, etc.)
  - Use of context based mappings
- Use the fewest number of comfortable keystrokes to perform all actions
  - Use modes and letter bindings in all situations where multiple consequtive actions are frequent (i.e browsing)
  - Use prefix key or well placed thumb modifiers where usually only one or two actions take place as at a time (i.e window management)
  - Use two and three letter aliases for most programs; use aliases for frequent long terminal commands (balance between brevity and clarity/memorizability)
  - Better text input; write language/sentences in shorthand with text expansion (i.e. autokey or iabbr) or use steno/plover
  - Most comfortable keys to reach should be personalized for most frequently used actions (i.e leader should be space or a letter)
  - Automate as much as possible; use smart completion and expansion as much as possible
- Eliminate usage of hard to reach pinky modifiers and straighten wrists
  - With modal interfaces, control and many modifier bindings are obsoleted; they can still be remapped and used for other purposes
  - Wide mod to keep wrists straighter and more easy access to ralt/altgr or whatever thumb keys exist
  - Dual role keys when possible; use caps as modifier if needed
  - Better Hardware: thumb cluster or split up spacebar (japanese keyboards) and split keyboard (preferably vertically staggered and mechanical)
    - This allows for remapping all modifiers to thumb as well as other keys (backspace, enter, shift, and better alt location (misc modifier) in addition to wm key)
    - Even if not split, the ISO layout allows for a wider wide mod

  - Maybe dual role home row letters as modifiers; never tested successfully (implement some sort of hybrid chording)
- Eliminate motion away from the home row (particularly horizontal movement that requires hand repositioning)
  - Again, don't waste time switching to and using the general inferiority that is the rodent
  - Remap symbols, backspace, enter, tab, etc. based on frequency of use
  - Rely on layer locks (both temporary and automatically escaped (types of prefixes)) and macros when beneficial (see tmk firmware for hardware solution)
    - i.e. you type in multiple languages; lock an unmodified layer for another language instead of using modifiers for special characters (this again with the modal spirit)
    - i.e. you don't use qwerty and games don't allow for remapping; lock layer/switch to "mode" for wasd/esdf and such
  - Use of arrow keys, home, end, pageup, etc. is obsoleted by modality; otherwise remap navigation keys to home row with caps or thumbkey

# For More Information:
- Will add documentation

# Working On
## Right Now
- Cleaning up all my config files and adding them here
- Perfect post-install script (ideally, installing os, setting up configuration, folder structure, programs installed, etc. to 99% similarity of past install should take under 30 minutes of the user's direct attention)
- Get a compact keyboard with enough thumb keys for portable use with laptop (will affect bindings)
- Lose virginity to a half foreign mixed race walrus of high status and social respectability
- Implement more shorthand and completion; still considering the potential viability of plover
- Get pterosaur working..

Now completed:
- Script to display album art in terminal split (see below and bin/albumart_display.sh)

## Of Less Importance
- Design a better compact keyboard (see [will add])
- Windows and Other OS setups
- Mess around more with emacs and ST
- Make or coerce someone into making an actually decent dual role program/script for linux

# Cool Things I've Stolen
## Use Functions in Ranger
I use this mainly for things like image rotation, git, and file extraction; ranger serves as a nice interface in many cases. I've only found this to work for functions and not aliases.
The downside of this is that the shell command is much slower. Because of this, I have duplicated the original and renamed it "quickshell" for bindings (see commands.py).

[source](https://bbs.archlinux.org/viewtopic.php?id=93025&p=34)
I've modified it for zsh.
In commands.py, replace with the following under class shell(Command)
            self.fm.execute_command("zsh -c 'source ~/.zshrc;" + command + "'", flags=flags)

# Some (Possibly) Interesting Things I've Done:
## Create a Modal Interface For Programs That Don't Support Rebinding
There already exist programs that are modal by default or only need one set of letter bindings (tui programs such as ncmpcpp, vim, evil for emacs (and other less impressive things like vintageous for ST), pentadactyl for firefox, scripts for irssi and weechat, bash and zsh vim mode, tmux, and window management with sxhkd or xchainkeys for bindings (though modal bindings aren't as important in the last two cases)).

However, there are many examples of programs that do have extensive keyboard shortcuts that could potentially be useful if their default bindings weren't oriented towards masochists (i.e. photoshop). For some programs, the few available shortcuts can still be massively useful when implemented in vim-like modes (i.e Libre Office).

Solution: Rebind keys to fake the existing keyboard shortcuts
[Video Demonstration With Libre Writer](http://youtu.be/iB1fCASlpY8)

[Explanation](http://forum.colemak.com/viewtopic.php?id=1817)

This solution is restricted to X currently. It makes use of xchainkeys for the modal bindings and xdotool and xsendkey to fake the necessary keyboard input. A potentially "software independent" solution would be using tmk firmware to make layers with macros and keys for "mode" (layer) switching. I have not been able to test this.

## Stream Any Video in MPV
Existing solutions for playing videos in the player of your choice (i.e. mplayer or vlc) are limited in what they work with. There are quite a few programs that allow this for a few sites such as youtube and daily motion (consider as youtube-viewer and using quvi), but I'd rather use mpv with all its features (keybindings for screenshots, seeking, etc.) than send just fake clicks to pause and play videos where things like noflash don't work.

This is a relatively simple thing to do in actuality. The reason existing solutions are site specific is because they operate based on the site link. Mpv will have no problem playing pretty much any video if you pass it the direct link, so all you have to do is set up a script to get that link.

There's probably a better way to do this, but I only know how to get this link manually: you open up firebug (or control+shift+j in chrome) and go to the net/media tab. When you play the video, the direct link will show up. What I've done is scripted the opening of the firebug window, the clicking on the video to start it, the clicking on the firebug window to copy that link, and then the opening of the link in mpv. This is mouse location dependent; the areas that need to be clicked will depend on screen size, window size, and firebug window size (all of these are constant for me but will be a problem if you use a floating wm; then again, if you're using a floating wm, you might not care about having to manually copy the link in the first place). I use this extensively, and it works quickly and consistently for me. The nice thing about firebug as well is that there is a wide range where you can click beside and below the media popup where it will allow you to copy the link.

See bin/firebug_fake_mouse
bin/firebug_fake_key
and the corresponding section in my .pentadactylrc (search undescore MPV)
Requirements: Pentadactyl, MPV, Firefox, and xdotool

firebug_fake_key is necessary because the firebug pentadactyl plugin is not able to open the firebug window if it has not already been accessed in that ff window (at least for me). I have tried adding sleep time, but no changes have fixed the glitch where you may have to press the keybinding twice to open the firebug window on the media tab. This does consist of two bindings (one to open the window.. the next to play the video), but this is not problematic for me, as I usually open the firebug window once and then go into otaku mode for 14 hours straight without having to use the first binding ever again.
## Make Any Terminal Emulator Dropdown
See bin/my_dropdown.sh
This is really just an idea that will require a lot of changes for someone else using it with different programs. The basic idea is to use a tmux session to save the terminal state and then actually kill or open a terminal and reattach. 

Sure there is already guake, tilde, xfce4-term, and yakuake and solutions solutions for urxvt (-pe quake), xterm (yeahconsole), and specific window managers. I use guake sometimes, but I primarily like termite and use bspwm. The benefit of this is that the idea can be adapted to different window managers and terminal emulators.

[Video Demonstration](http://youtu.be/3yhX_y1VdWE)

## Use Ranger Instead of Default GUI Popup for File Saving (WIP)
Pentadactyl already has :w and ;s which allow for typing out file path with tab completion. This is cumbersome especially if you have as large a folder structure as I do. I used to just use an alias to open ranger in my downloads folder and save there automatically. I find even then I don't always get around to moving stuff, so now I have an autocommand to send the file name on download to a script which will open ranger floating and pass the file location to ranger's --selectfile and cut it (see bin/ranger_browser_fm.sh). I still have to fix a few things with this (I've fixed it now so it works with space in the name, but certain symbols may mess it up; also I want to find a way to get this to not execute for private windows).

I really wish I could do this for uploads as well. If anyone knows a good way to universally obsolete the upload gui, please let me know. I don't really want to make a bunch of mouse clicking hotkeys for the upload gui. Right now, I do image uploads from the commandline and mail attachments with mutt.

## Tabs Outliner Replacement
One reason I stuck with chromium for so long is because of this extension. I used to use things like pocket, evernote webclipper, other read later extensions, and even bookmarks for anything I wanted to look at later. I found it better to just save links in an organized structure with TO and save anything I wanted to keep as html and incorporate it into my by folder structure. I thought of severally possibilities for replacing this functionality with pentadactyl (tab groups and session.. stuff with bookmarks) and decided to use vimwiki as in interface for saved links. Basically, I created a script that saves the current link to a .wiki file by the title of the argument you give it. Saving all tabs can be done with pentadactyl's :tabdo command. Another binding opens an instance of vim for the index.wiki file. Enter opens links. I still need to add automatic addition of new .wiki files to the index.wiki file. I may work on automatic naming based on tab groups similar to what I do with unite sometime in the future. I may add a more "tree style" like structure in the future.

Like TO, when the window is closed (with a special d binding), the link will be deleted from any of the .wiki files it is in.

See bin/to.sh and bin/to_win.sh as well as .pentadactylrc.

## Block Layout in Vim and Example of Context Bindings
Will add explanation.. see quickvimrc

## Vim as a Clipboard Manager
I've tried quite a few clipboard managers without liking any of them. What I really wanted was one with vim bindings, so I ended up deciding just to use vim.

Also, y and p are my universal copy paste bindings. I don't ever use c-v and c-c, and everything goes to the system clipboard (+ register). I have these bindings set up for pentadactyl, zsh, weechat, vim, tmux, etc. The one thing I do need to decide on is what to iabbr c-v in command mode to. Unite can essentially act as a clipboard manager while simultaneously replacing yank stack/ yank ring. I don't really need the complex functionality given by either; I just want a list I can scroll through and copy/paste from with vim bindings, and Unite does this well.

There's also easyclip, which I find nicer than yank stack or yank ring even though I don't use it

Search underscore clipboard in my vimrc.

## Termite Link Hinting and Remapping:
Currently, termite does not support rebindings from the config file. I use colemak and prefer tmux copy mode, so this isn't that much of a problem for me. I really prefer termite's link hinting to things like urlview and urlscan. I usually only use link hinting in weechat, so I've bound f in normal mode (see my vimode.py fork) to bin/link_hint.sh to fake the key combo necessary to open the url hint window. I have a zsh binding, and for everything else I've bound it to tmux prefix-f.

## Display Lyrics and Album Art With Vimus
Before I used ncmpcpp, I used cmus. I like the premise of having vi-like key bindings to begin with, but I could never get scrobbling working and like mpd a lot. However, to be frank, ncmpcpp has the worst binding capabilities of any tui program I use extensively, so I've recently switched to [vimus](https://github.com/vimus/vimus). I had some trouble installing it at first, and though it may not have as complex a ui as ncmpcpp, it has all the functionality I need as well as some interesting default bindings (like visual yanking and pasting of songs and appending/insertion of songs to the playlist) and support for multikey bindings.

Since vimus doesn't support things like tag editing (which I never used in ncmpcpp anyway) it has the "%" and ":!" options. Because it allows binding keys to terminal commands and allows for the possibility of sending the file location of the currently playing song with "%" I've been able to do the following:

.bind a key to a script to get (if doesn't exist) album art for currently playing song using [artget](https://github.com/berkoz/artget) and then display the picture in a tmux split using ranger (an image preview compatible terminal like xterm, urxvt, termite, terminology, gnome-terminal, etc. is required)
.bind a key to display lyrics in ncmpcpp in a tmux split (vimus doesn't support lyric fetching; it matters not)

Using conky, for example, to constantly display album art on my desktop never really intersted me. I'd much rather just have the art displayed in the program (in this case, in the same "window" next to it).  I still need to add some features like maybe using mpd-on-change to execute the script every time the song changes.

## Eliminate Window Management Binding Layer
See .vimrc and .lesskey for examples

For me, window management is pretty much split between tmux and bspwm. Bspwm takes care of all my gui windows (and occasionally a terminal window), and tmux takes care of all my terminal sessions, windows, splits, etc.

The idea of modal window management has interested me, but modal window management isn't really efficient when most of the time you only execute one wm command (it just requires an extra key for escaping as opposed to using a prefix key). It introduces other problems as well. Escape can't be used to enter this "window management mode" (with sxhkd this would make escape lose functionality everywhere else). Unlike in vim, "normal mode" would be infrequently entered and immediately exited. Although I am a fan of modality, I do not think modes within modes does anything other than overcomplicate things.

Instead of trying to mirror this functionality, I've found it most efficient to eliminate window management as a separate entity and just build it in to all my programs just as I would set up bindings for split navigation.

For optimally comfortable and distinct window management bindings, I would need 3-4 thumb keys (2 for bspc; 2 for tmux; in each case 1 where selection is two home row key presses and another where desktop/window(tmux) navigation and movement is 2 home row key presses). I don't have that many to spare and would rather use letter bindings. 

I quickly find myself running out of room for good places for important keys, especially on a normal keyboard. I decided to test out setting up letter bindings for fun, but because it actually works fairly well, I've been using this regularly lately.. it makes room for other important keys instead of having as many specifically for window management.

Building bindings into every program instead of using sxhkd and a regular tmux prefix key has the following advantages:
- The keys used can be on the home row (better position)
- The same keys can be used in each context (reducing the total number of needed keys) whereas if super is being used for bspc, it cannot also be used for tmux
Disadvantages:
- Initial setup time (temporary)
- You lose two keys for program specific bindings (not a big problem for me)
- Not as viable if programs that don't allow this sort of rebinding are being used (I'm not using any)
- Some noticeable delay for some programs (for me just pentadactyl/firefox and I have fixed it somewhat)

My three most used gui programs (gvim, firefox, and apvlv (infrequently)) all allow for bindings to terminal commands. I decided to try to eliminate the "wm layer" and build parallel wm bindings into the normal mode of each program. "r" becomes "redraw," "resize," etc. and "s" becomes select (two keys I don't use much anyway and can easily get a lot more out of with a prefix key for window management than a single operation key). This means window management is now "builtin" to each program with the illusion of universality. If this doesn't make sense, basically instead of having to use a harder to reach prefix or modifier key for window management like super, alt, grave, etc. you can now just use a letter. Again, trying to do this with sxhkd would result in that letter losing all functionality.

Does it work? Probably not if you use a lot of gui programs. For me, window management is something that should be out of the way and take essentially no time, and this worked a lot better than I thought it would (it replaces 99% use of a dedicated wm key for me). The difference between pressing "super+5" and rd may not seem to be a big deal, but it's been quite noticeable to me. As for speed, I've noticed no speed decrease from gvim and apvlv or in any terminal program, though there is sometimes a slight delay with pentadactyl as ":silent" (which is necessary) seems to execute slower. This can be somewhat dealt with by making commands.

I initially thought this would be impossible to replicate in the terminal but spent a day testing it, and I have now built tmux bindings into all of my programs (tmux, zsh/readline, less, weechat, ranger, and vimus). The only downside of this is that obviously readline bindings won't work if you have something running. On the other hand, this isn't that big of a deal because tmux allows use of a prefix key on a layer (i.e. mine is grave, which is mode_switch f for me).

See the README in the remap folder for more info.

## Emacs With Vim Bindings
I messed around with emacs for about a day to try to replicate as much of the functionality I have in vim with the same bindings (because I would never use emacs' defaults). Emacs is pretty awesome. The configuration is infinitely more complicated, but I see some interesting possibilities for doing certain things that I will try when I have the time and motivation to configure emacs further. My ~/.emacs may be of use.

# Credit
will add
