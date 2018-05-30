"""Qutebrowser configuration"""
# * Pylint
# prevent pylint warnings and fix completion for c/config
# line too long
# pylint: disable = C0301
# E501 - line too long
# F401 - module imported but unused
# F821 - undefined name
# E0602 - undefined variable
# C0103 - invalid constant name
import re
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401,E501 pylint: disable=unused-import
from qutebrowser.config.config import ConfigContainer  # noqa: F401,E501 pylint: disable=unused-import
config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103

# * Notes
# ** Probably Will Be Useful
# - click-element
# - ;; for command chaining
# - content.user_stylesheets

# ** TODO Add/Look Into
# - difference between url and url:pretty/pretty-url
# - make sanitize command ('history-clear ;; download-clear'; TODO
#   clear-cookies)
# - password management
#   https://github.com/qutebrowser/qutebrowser/issues/180
#   https://github.com/qutebrowser/qutebrowser/issues/3350
# - require permissions per page to use flash
# - spell checking (https://github.com/qutebrowser/qutebrowser/issues/700)
# - change background/active tab colors
# - loading bar under tabs
# - maybe equivalent of readability bookmarklet

# ** Nice Things About Qutebrowser
# - more sophisticated completion than pentadactyl (e.g. " " like ".*")
# - very fast/responsive
# - navigation wizardry with :navigate (prev|next|up)

# ** Missing Functionality/ Wishlist
# *** Necessary for Everyday Usage
# - custom command instead of file browser or post download auto-command
#   (currently have to manually run :download-open; TODO make issue)
# - insert editing keys (e.g. rl-backward-kill-word doesn't work in insert;
#   TODO make issue)
# - consistent tab width like firefox (TODO make issue)
# - single key quickmarks (real quickmarks;
#   https://github.com/qutebrowser/qutebrowser/issues/711; also
#   https://github.com/qutebrowser/qutebrowser/issues/882)
# - per-domain keybindings
#   (https://github.com/qutebrowser/qutebrowser/issues/3636)
# - better greasemonkey support
#   (https://github.com/qutebrowser/qutebrowser/issues/3238)

# *** Major
# - plugin support (https://github.com/qutebrowser/qutebrowser/issues/30)
#   - security: decentraleyes, cookie auto delete (dedicated issue:
#     https://github.com/qutebrowser/qutebrowser/issues/1660), privacy badger
#     (a ghosterey/disconnect/privacy badger equivalent), agent
#     spoofing/browser fingerprint minimization, etc.
#   - hover zoom/imagus equivalent (if userscript doesn't work)
#   - DownThemAll equivalent (particularly in conjunction with comic/gallery
#     userscript)
#   - RES equivalent (at least some of the features would be nice)
#   - "automatically spawn mpv on video pages"
# - HTTPS everywhere (https://github.com/qutebrowser/qutebrowser/issues/335)
# - tabgroups (https://github.com/qutebrowser/qutebrowser/issues/49)
# - autocmds (https://github.com/qutebrowser/qutebrowser/issues/35)
# - undo completion/history
#   (https://github.com/qutebrowser/qutebrowser/issues/32; also see
#   https://github.com/qutebrowser/qutebrowser/issues/1031)
# - same-window, tab-unique inspector instead of separate windows
#   (https://github.com/qutebrowser/qutebrowser/issues/1400)
# - don't autoplay videos
#   (https://github.com/qutebrowser/qutebrowser/issues/1643)

# *** Minor
# - better ad-blocking (e.g. youtube not supported;
#   https://github.com/qutebrowser/qutebrowser/issues/29)
# - way to reference ~ / $HOME in filename (TODO make issue)
# - simpler key syntax (c instead of Ctrl,  esc instead of Escape, etc.;
#   TODO make issue)
# - tree style tab view (https://github.com/qutebrowser/qutebrowser/issues/927)
# - rikaikun/rikaichan (seems unlikely)
# - unbind should work for default keybindings (TODO make issue)
# - better caret mode or way to open current page in editor (TODO make issue)
# - full jumplist (https://github.com/qutebrowser/qutebrowser/issues/2642)
# - more hinting modes (https://github.com/qutebrowser/qutebrowser/issues/521)
# - save editor buffer (https://github.com/qutebrowser/qutebrowser/issues/1596)
# - cookies-clear command (TODO make issue)
# - tab-focus jump within current 10 tabs range(or this possible with plugin
#   API; TODO make issue)
# - configurable completion
#   (https://github.com/qutebrowser/qutebrowser/issues/836)
# - support recursive command aliasing (alias an alias)

# * Helper Functions
def bind(key, command, mode):  # noqa: E302
    """Bind key to command in mode."""
    # TODO set force; doesn't exist yet
    config.bind(key, command, mode=mode)


def nmap(key, command):
    """Bind key to command in normal mode."""
    bind(key, command, 'normal')


def imap(key, command):
    """Bind key to command in insert mode."""
    bind(key, command, 'insert')


def pmap(key, command):
    """Bind key to command in passthrough mode."""
    bind(key, command, 'passthrough')


def cmap(key, command):
    """Bind key to command in caret mode."""
    bind(key, command, 'caret')


def unmap(key, mode):
    """Unbind key in mode."""
    config.unbind(key, mode=mode)


def nunmap(key):
    """Unbind key in normal mode."""
    unmap(key, mode='normal')


# * Settings
# ** Session
# always restore opened sites when opening qutebrowser
c.auto_save.session = True

# ** Tabs
# open new tabs (middleclick/ctrl+click) in the background
c.tabs.background = True
# select previous tab instead of next tab when deleting current tab
c.tabs.select_on_remove = 'prev'
# open unrelated tabs after the current tab not last
c.tabs.new_position.unrelated = 'next'

c.tabs.title.format = '{index}{private}{title_sep}{title}'

# ** Command Aliases
c.aliases['xa'] = 'quit --save'
c.aliases['h'] = 'help'

# ** Appearance
c.fonts.monospace = 'Fira mono'

# default but not transparent
c.colors.hints.bg = \
    'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 rgba(255, 247, 133, 1), ' \
    + 'stop:1 rgba(255, 197, 66, 1))'

c.scrolling.bar = True
# keep smooth scrolling off

# lower delay for keyhint dialog (comparable to which-key)
c.keyhint.delay = 250

# ** Editor
c.editor.command = ['emacsclient', '-c', '-a', '""', '+{line}:{column}', '{}']

# ** Hints
# don't require enter after hint keys
c.hints.auto_follow = 'always'
c.hints.chars = 'arstdhneiowfpluy'

# ** Downloads
c.downloads.location.directory = '~/move'
c.downloads.location.prompt = False
c.downloads.open_dispatcher = 'dl_move {}'

# ** Security
# defaults
# c.content.cookies.accept = 'no-3rdparty'
# c.content.geolocation = 'ask'
# c.content.headers.do_not_track = True
# c.content.headers.referer = 'same-domain'
# c.content.host_blocking.enabled = True
# c.content.media_capture = 'ask'
# c.content.notifications = 'ask'
# c.content.ssl_strict = 'ask'

# content.javascript.can_access_clipboard
# content.headers.user_agent
# content.javascript.enabled
# content.pdfjs
# content.plugins
# content.proxy
# content.xss_auditing

# ** Input
# don't timeout for during partially entered command
c.input.partial_timeout = 0
# arrow key link element navigation; works okay on some pages
c.input.spatial_navigation = True

# ** Home/Start Page
# TODO generic user home directory reference instead
c.url.default_page = '/home/noctuid/src/homepage/home.html'
c.url.start_pages = ['/home/noctuid/src/homepage/home.html']

# ** Search Keywords
c.url.searchengines = \
    {'DEFAULT': 'https://www.startpage.com/do/search?query={}&?prf=7de10a290cc3cee4fa552d4b43dc3f48',  # noqa: E501
     'aff': 'http://www.anime4fun.com/search?keyword={}',
     'bt': 'http://bato.to/search?name_cond=c&name={}',
     'am': 'http://www.amazon.com/s/url=search-alias%3Daps&field-keywords={}',
     'aw': 'http://wiki.archlinux.org/index.php?search={}',
     'az': 'http://search.azlyrics.com/search.php?q={}',
     'd': 'http://duckduckgo.com/?q={}',
     'dv': 'http://www.deviantart.com/?q={}',
     'fl': 'http://fluffy.is/search.php?folders=&name={}',
     'gh': 'http://github.com/search?q={}',
     'hg': 'http://www.haskell.org/hoogle/?hoogle={}',
     'j': 'http://jisho.org/search/{}',
     'mal': 'https://myanimelist.net/search/all?q={}',
     'mus': 'http://www.allmusic.com/search/all/{}',
     'r': 'http://www.reddit.com/r/{}/',
     'rt': 'http://www.rottentomatoes.com/search/?search={}',
     's': 'https://searx.me/?q={}',
     't': 'http://www.tumblr.com/search/{}',
     # other useful yub nub commands: cnv, gim, tiny, ddg, torf, etc.
     # useful mostly for stuff that combines things like mash, split, weird
     # piping stuff, etc.
     'yub': 'http://yubnub.org/parser/parse?command={}',
     'y': 'http://www.youtube.com/results?search_query={}',
     'wayback':  'http://web.archive.org/web/*/{}',
     'zerochan':  'http://www.zerochan.net/{}',
    }

# ** Bookmarklets/Custom Commands
c.aliases['archive'] = 'open -t http://web.archive.org/save/{url}'
c.aliases['view-archive'] = 'open -t http://web.archive.org/web/*/{url}'
c.aliases['va'] = 'open -t http://web.archive.org/web/*/{url}'
c.aliases['view-google-cache'] = \
    'open http://www.google.com/search?q=cache:{url}'
c.aliases['vgc'] = 'open http://www.google.com/search?q=cache:{url}'

# * Key Bindings
# ** Miscellaneous Swaps
# swap ; and :
nmap(';', 'set-cmd-text :')
nunmap(':')
hints_dict = dict()  # pylint: disable = C0103
for k, v in c.bindings.default['normal'].items():
    if k.startswith(';'):
        hints_dict[re.sub(r'^;(.*)', r':\1', k)] = v
c.bindings.commands['normal'].update(hints_dict)

# lose scroll left
nmap('h', 'back')
nmap('H', 'forward')

# lose scroll right
nmap('l', 'tab-focus last')

nmap('b', 'set-cmd-text -s :buffer')

# ** Colemak Swaps
nmap('n', 'run-with-count 5 scroll down')
nmap('e', 'run-with-count 5 scroll up')
nmap('N', 'tab-prev')
# no default binding
nmap('E', 'tab-next')

# add back search
nmap('k', 'search-next')
nmap('K', 'search-prev')

cmap('n', 'move-to-next-line')
cmap('e', 'move-to-prev-line')
cmap('i', 'move-to-next-char')
# add back e functionality
cmap('j', 'move-to-end-of-word')

cmap('N', 'scroll down')
cmap('E', 'scroll up')
cmap('I', 'scroll right')

# ** Hinting
# I think I like this better than going to first input
nmap('gi', 'hint inputs')

# ** Tabs and Windows
nmap('o', 'set-cmd-text -s :open -t')
nmap('O', 'set-cmd-text -s :open')

# lose tab-only and download-clear
nmap('c', 'set-cmd-text :open -t -r {url:pretty}')

nmap('gn', 'navigate previous')
nmap('ge', 'navigate next')

# open new private window
nmap('tp', 'open -p')

# tn and te for tab moving
nmap('tn', 'tab-move -')
nmap('te', 'tab-move +')

# ** Yanking and Pasting
# don't need primary or extra yanks
nmap('p', 'open -t -- {clipboard}')
nmap('P', 'open -- {clipboard}')
nmap('y', 'yank')
nmap('Y', 'yank selection')

# ** Editor
imap('<Ctrl-i>', 'open-editor')
# open source in editor
nmap('gF', 'view-source --edit')

# ** Insert/RL
# TODO not supported in insert mode
# imap('<Ctrl-w>', 'rl-backward-kill-word')
# nunmap('<Ctrl-w>')
# prevent c-w from closing tab
del c.bindings.default['normal']['<Ctrl-W>']

# ** Passthrough
nmap(',', 'enter-mode passthrough')
pmap('<Escape>', 'leave-mode')

# ** Undo
# no :undo completion currently
# nmap('U', 'set-cmd-text -s :undo')

# ** Quickmarks and Marks
nmap("'", 'set-cmd-text -s :quickmark-load -t')
# nmap('B', 'set-cmd-text -s :bookmark-load -t')
nmap("t'", 'set-cmd-text -s :bookmark-load -t')

# add back mark jumping
nmap('"', 'enter-mode jump_mark')
nmap('tl', 'jump-mark "\'"')

# ** Spawn/Shell
# "y" for youtube-dl
nmap('ty', 'spawn -d mpv {url}')

# ** Downloads
nmap('tg', 'spawn --detach dlg "{url}"')
nmap('td', 'download-open')

# ** Zooming
nmap('zi', 'zoom-in')
nmap('zo', 'zoom-out')

# ** Inspector
nmap('ti', 'inspector')
