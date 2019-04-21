# TODO maybe just switch to https://gitlab.com/jjzmajic/qutewal if this becomes
# an effort to maintain; didn't realize it existed before writing this...

from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401,E501 pylint: disable=unused-import
from qutebrowser.config.config import ConfigContainer  # noqa: F401,E501 pylint: disable=unused-import
config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103

# * Color Definition
background='{background}'
foreground='{foreground}'
cursor='{cursor}'

color0='{color0}'
color1='{color1}'
color2='{color2}'
color3='{color3}'
color4='{color4}'
color5='{color5}'
color6='{color6}'
color7='{color7}'
color8='{color8}'
color9='{color9}'
color10='{color10}'
color11='{color11}'
color12='{color12}'
color13='{color13}'
color14='{color14}'
color15='{color15}'

black = color0
red = color1
green = color2
yellow = color3
blue = color4
magenta = color5
cyan = color6
white = color7

# so can easily invert
fg = foreground
bg = background

# * Completions
# header color
c.colors.completion.category.bg = red
c.colors.completion.category.fg = bg
c.colors.completion.category.border.bottom = black
c.colors.completion.category.border.top = black

c.colors.completion.even.bg = fg
c.colors.completion.odd.bg = fg
c.colors.completion.fg = bg

c.colors.completion.item.selected.bg = green
c.colors.completion.item.selected.border.bottom = 'transparent'
c.colors.completion.item.selected.border.top = 'transparent'

c.colors.completion.match.fg = red

c.colors.completion.scrollbar.bg = bg
c.colors.completion.scrollbar.fg = fg

# * Downloads
c.colors.downloads.bar.bg = black

c.colors.downloads.start.bg = blue
c.colors.downloads.stop.bg = green
c.colors.downloads.error.bg = black

c.colors.downloads.start.fg = fg
c.colors.downloads.stop.fg = fg
c.colors.downloads.error.fg = fg

# * Hints
c.colors.hints.match.fg = green

# * Key Hints
c.colors.keyhint.bg = bg
c.colors.keyhint.fg = fg
c.colors.keyhint.suffix.fg = yellow

# * Messages
c.colors.messages.error.bg = red
c.colors.messages.info.bg = black
c.colors.messages.warning.bg = yellow

c.colors.messages.error.border = 'transparent'
c.colors.messages.info.border = 'transparent'
c.colors.messages.warning.border = 'transparent'

c.colors.messages.error.fg = fg
c.colors.messages.info.fg = fg
c.colors.messages.warning.fg = fg

# * Prompts
c.colors.prompts.bg = bg
c.colors.prompts.fg = fg

# * Statusbar
# c.colors.statusbar.caret.bg = magenta
# c.colors.statusbar.caret.selection.bg = magenta
c.colors.statusbar.command.bg = bg
c.colors.statusbar.command.private.bg = magenta
c.colors.statusbar.insert.bg = green
c.colors.statusbar.normal.bg = bg
c.colors.statusbar.passthrough.bg = blue
c.colors.statusbar.private.bg = magenta

c.colors.statusbar.caret.fg = fg
c.colors.statusbar.caret.selection.fg = fg
c.colors.statusbar.command.fg = fg
c.colors.statusbar.command.private.fg = fg
c.colors.statusbar.insert.fg = fg
c.colors.statusbar.normal.fg = fg
c.colors.statusbar.passthrough.fg = fg
c.colors.statusbar.private.fg = fg

c.colors.statusbar.progress.bg = fg
c.colors.statusbar.url.fg = fg
c.colors.statusbar.url.error.fg = red
c.colors.statusbar.url.hover.fg = cyan
c.colors.statusbar.url.success.http.fg = fg
c.colors.statusbar.url.success.https.fg = green
c.colors.statusbar.url.warn.fg = yellow

# * Commandline
# * Tabs
c.colors.tabs.bar.bg = bg

c.colors.tabs.even.bg = bg
c.colors.tabs.odd.bg = bg
c.colors.tabs.even.fg = fg
c.colors.tabs.odd.fg = fg

# swap colors for selected
c.colors.tabs.selected.even.bg = fg
c.colors.tabs.selected.odd.bg = fg
c.colors.tabs.selected.even.fg = bg
c.colors.tabs.selected.odd.fg = bg

c.colors.tabs.pinned.even.bg = green
c.colors.tabs.pinned.odd.bg = green
c.colors.tabs.pinned.even.fg = fg
c.colors.tabs.pinned.odd.fg = fg

c.colors.tabs.pinned.selected.even.bg = cyan
c.colors.tabs.pinned.selected.odd.bg = cyan
c.colors.tabs.pinned.selected.even.fg = fg
c.colors.tabs.pinned.selected.odd.fg = fg

c.colors.tabs.indicator.error = red
c.colors.tabs.indicator.start = blue
c.colors.tabs.indicator.stop = green

# **** Wepage
c.colors.webpage.bg = bg
