-- Pull in the wezterm API
local wezterm = require 'wezterm'
local io = require 'io'
local os = require 'os'
local act = wezterm.action

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
   config = wezterm.config_builder()
end

local home = wezterm.home_dir;

config.default_prog = { '/usr/bin/env', 'zsh' }

-- * Appearance
-- fine if this doesn't exist as wezterm as default fallback
config.font = wezterm.font 'Delugia'

local file_exists
function file_exists(name)
   local f = io.open(name, "r")
   return f ~= nil and io.close(f)
end

local wezterm_wal = home.."/.cache/wal/wezterm-wal.toml"
if file_exists(wezterm_wal) then
   wezterm.add_to_config_reload_watch_list()
   config.color_scheme_dirs = {home.."/.cache/wal"}
   config.color_scheme = 'wezterm-wal'
else
   config.color_scheme = 'GruvboxDark'
end

config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.show_new_tab_button_in_tab_bar = false

config.inactive_pane_hsb = {
   saturation = 0.9,
   brightness = 0.7,
}

-- slow blink rate
config.cursor_blink_rate = 1200

-- transparency (doesn't affect text)
config.window_background_opacity = 0.8
-- this affects tab bar, for example; as far as I can tell, it doesn't affect
-- the text itself
config.text_background_opacity = 0.8

-- disable titlebar (needed on macOS)
config.window_decorations = "RESIZE"

-- * Scrollback
-- increase scrollback
config.scrollback_lines = 100000

-- backwards from normal string due to how wezterm uses it
config.quick_select_alphabet = "yulpfwienhdtsr"

-- * Custom Events
-- https://wezfurlong.org/wezterm/config/lua/wezterm/on.html?h=custom+events#custom-events
wezterm.on('trigger-emacs-with-scrollback', function(window, pane)
-- no arg will just give visible text
  local viewport_text = pane:get_lines_as_text(config.scrollback_lines)

  local name = os.tmpname()
  local f = io.open(name, 'w+')
  f:write(viewport_text)
  f:flush()
  f:close()

  -- window:perform_action(
  --   act.SpawnCommandInNewWindow {
  --     args = { 'emacsclient', '-c', name },
  --   },
  --   pane
  -- )

  wezterm.background_child_process({ 'emacsclient', '-c', name })

  -- Wait "enough" time for Emacs to read the file before we remove it.
  -- The window creation and process spawn are asynchronous wrt. running
  -- this script and are not awaitable, so we just pick a number.
  --
  -- Note: We don't strictly need to remove this file, but it is nice
  -- to avoid cluttering up the temporary directory.
  wezterm.sleep_ms(1000)
  os.remove(name)
end)

-- * Keybindings
config.disable_default_key_bindings = true
config.keys = {
   -- ** Panes
   {
      key = '\'',
      mods = 'CTRL',
      action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
   },
   {
      key = 'w',
      mods = 'CTRL',
      action = act.CloseCurrentPane { confirm = false },
   },

   {
      key = 'n',
      mods = 'CTRL',
      action = act.ActivatePaneDirection 'Left',
   },
   {
      key = 'e',
      mods = 'CTRL',
      action = act.ActivatePaneDirection 'Right',
   },


   -- ** Tabs
   {
      key = 't',
      mods = 'CTRL',
      action = act.SpawnTab 'DefaultDomain',
   },

   {
      key = 'Tab',
      mods = 'CTRL|SHIFT',
      action = act.ActivateTabRelative(-1)
   },
   {
      key = 'Tab',
      mods = 'CTRL',
      action = act.ActivateTabRelative(1)
   },

   -- ** Copy/Paste
   -- hooray global paste (as it's broken in ranger)
   {
      key = 'y',
      mods = 'CTRL',
      action = act.PasteFrom 'Clipboard',
   },

   -- ** Scrollback
   {
      key = '/',
      mods = 'CTRL',
      action = act.Search { CaseInSensitiveString = '' },
   },

   {
      key = 'i',
      mods = 'CTRL',
      action = act.EmitEvent 'trigger-emacs-with-scrollback',
   },

   {
      key = 'q',
      mods = 'CTRL',
      action = act.QuickSelect,
   },

   -- https://wezfurlong.org/wezterm/config/lua/keyassignment/QuickSelectArgs.html
   {
      key = 'f',
      mods = 'CTRL',
      action = wezterm.action.QuickSelectArgs {
         label = 'open url',
         patterns = {
            'https?://\\S+',
            -- doesn't work (open_with doesn't detect as should be opened in
            -- browser or something else?)
            -- 'www\\.\\S+',
         },
         action = wezterm.action_callback(function(window, pane)
               local url = window:get_selection_text_for_pane(pane)
               wezterm.log_info('opening: ' .. url)
               wezterm.open_with(url)
         end),
      },
   },

   {
      key = 'Escape',
      mods = 'ALT',
      action = act.ActivateCopyMode,
   },
   {
      key = 'v',
      mods = 'ALT',
      action = act.ActivateCopyMode,
   },

   -- ** Font Size
   {
      key = '-',
      mods = 'CTRL',
      action = act.DecreaseFontSize,
   },
   {
      key = '=',
      mods = 'CTRL',
      action = act.IncreaseFontSize,
   },

}

-- ** Copy Mode
local copy_mode = nil
if wezterm.gui then
  copy_mode = wezterm.gui.default_key_tables().copy_mode
  local my_copy_mode = {
      { key = 'n', mods = 'NONE', action = act.CopyMode 'MoveDown' },
      { key = 'e', mods = 'NONE', action = act.CopyMode 'MoveUp' },
      { key = 'i', mods = 'NONE', action = act.CopyMode 'MoveRight' },
      -- TODO would be nice to be able to search without clearing selection
      -- {
      --    key = '/',
      --    mods = 'NONE',
      --    action = act.Search { CaseInSensitiveString = '' },
      -- },
   }
  for _, val in ipairs(my_copy_mode) do
     table.insert(copy_mode, val)
  end
end

config.key_tables = {
   copy_mode = copy_mode
}

-- * Local Configuration
if pcall(require, 'local') then
   local local_config = require 'local'
   local_config.apply_to_config(config)
end

return config
