-- functions that are too specific for a plugin or need more testing/development
-- before becoming part of one
local utils = require "mp.utils";

-- * General Helpers
function set_script_opt(option, value)
   local full = option .. "=" .. value
   mp.commandv("no-osd", "change-list", "script-opts", "append", full)
end

function read_file(file)
   local fh = assert(io.open(file, "rb"))
   local contents = fh:read("*all")
   fh:close()
   return contents
end

function message(text, duration)
    local ass = mp.get_property_osd("osd-ass-cc/0")
    ass = ass .. text
    return mp.osd_message(ass, duration or 1)
end

function set_values(set)
   local ret={}
   for k, v in pairs(set) do
      if v then
         ret[#ret+1] = k
      end
   end
   return ret
end

-- * Remember and Restore Zoom and Pan on Navigate
-- TODO flicker
-- store and restore
local store_zoom_and_pan = false
local zoom = {}
local pan_x = {}
local pan_y = {}
local last_path = nil

function store_and_restore_zoom_and_pan()
   if store_zoom_and_pan then
      local path = mp.get_property("path")
      if last_path then
         zoom[last_path] = mp.get_property("video-zoom")
         pan_x[last_path] = mp.get_property("video-pan-x")
         pan_y[last_path] = mp.get_property("video-pan-y")
      end
      if zoom[path] then
         mp.set_property("video-zoom", zoom[path])
      end
      if pan_x[path] then
         mp.set_property("video-pan-x", pan_x[path])
      end
      if pan_y[path] then
         mp.set_property("video-pan-y", pan_y[path])
      end
      last_path = path
   end
end

function toggle_restore_zoom_and_pan()
   store_zoom_and_pan = not store_zoom_and_pan
   if store_zoom_and_pan then
      last_path = mp.get_property("path")
   else
      last_path = nil
   end
end

mp.add_hook("on_load", 50, store_and_restore_zoom_and_pan)
mp.add_key_binding(nil, "toggle-restore-zoom-and-pan",
                   toggle_restore_zoom_and_pan)

-- * Reset Zoom and Pan on Navigate
local should_reset_zoom_and_pan = false

function reset_zoom_and_pan()
   if should_reset_zoom_and_pan then
      local path = mp.get_property("path")
      if not store_zoom_and_pan
         or not (zoom[path] or pan_x[path] or pan_y[path]) then
         mp.set_property("video-zoom", 0)
         mp.commandv("script-message", "reset-pan-if-visible")
      end
   end
end

function toggle_reset_zoom_and_pan()
   reset_zoom = not reset_zoom
end

mp.add_hook("on_load", 50, reset_zoom_and_pan)

mp.add_key_binding(nil, "toggle-reset-zoom-and-pan",
                   toggle_reset_zoom_and_pan)

-- * Reset or Restore Zoom and Pan on Navigate
function toggle_restore_or_reset_zoom_and_pan()
   toggle_restore_zoom_and_pan()
   toggle_reset_zoom_and_pan()
end

mp.add_key_binding(nil, "toggle-restore-or-reset-zoom-and-pan",
                   toggle_restore_or_reset_zoom_and_pan)

-- * Marks
-- TODO toggling different actions (ln, mv, rm)
local marks = {}

function toggle_mark(tag)
   tag = tag or "mark"
   if not marks[tag] then
      marks[tag] = {}
   end
   local path = mp.get_property("path")
   if marks[tag][path] then
      message("Untagged " .. path .. " " .. tag)
      marks[tag][path] = false
   else
      message("Tagged " .. path .. " " .. tag)
      marks[tag][path] = true
   end
end

function save_marks_table(tag, mark_table)
   local marked_files = set_values(mark_table)
   if #marked_files >= 1 then
      mp.commandv("run", "mkdir", "-p", tag)
      table.insert(marked_files, 1, "ln")
      table.insert(marked_files, tag)
      local webtorrent_result = mp.command_native({
            name = "subprocess",
            playback_only = false,
            args = marked_files
      })
   end
end

function save_all_marks()
   for tag, mark_table in pairs(marks) do
      save_marks_table(tag, mark_table)
   end
end

mp.add_key_binding(nil, "toggle-image-mark", toggle_mark)
mp.register_event("shutdown", save_all_marks)

-- * Basic Comic
-- TODO flicker: https://github.com/mpv-player/mpv/issues/7293

local comic_mode = false
local comic_pan = false
local at_bottom = false
local at_top = false
local align_top = true

function align_top()
   mp.command("script-message align-border 0 1")
end

function align_bottom()
   mp.command("script-message align-border 0 -1")
end

function maybe_switch_page(_name, value)
   if comic_mode then
      if not comic_pan then
         at_bottom = false
         at_top = false
      end
      comic_pan = false
   end
end

mp.observe_property("video-pan-y", "string", maybe_switch_page)


function comic_backward()
   comic_pan = true
   if at_top then
      mp.command("playlist-prev")
      align_bottom()
      at_top = false
      at_bottom = true
   else
      align_top()
      at_bottom = false
      at_top = true
   end
end

function comic_forward()
   comic_pan = true
   if at_bottom then
      mp.command("playlist-next")
      align_top()
      at_bottom = false
      at_top = true
   else
      align_bottom()
      at_top = false
      at_bottom = true
   end
end

mp.add_key_binding(nil, "comic-forward", comic_forward)
mp.add_key_binding(nil, "comic-backward", comic_backward)

function comic_toggle(backward_key, forward_key)
   comic_mode = not comic_mode
   if comic_mode then
      mp.set_property("video-zoom", 0.8)
      align_top()
      mp.add_forced_key_binding(backward_key, "comic-forward-temp",
                                comic_backward)
      mp.add_forced_key_binding(forward_key, "comic-backward-temp",
                                comic_forward)
   else
      mp.set_property("video-zoom", 0)
      mp.command("script-message reset-pan-if-visible")
      mp.remove_key_binding("comic-forward-temp")
      mp.remove_key_binding("comic-backward-temp")
   end
end

mp.add_key_binding(nil, "comic-toggle", comic_toggle)

-- * Pywal
-- remove #
function plain_color(color)
   return string.sub(color, 2)
end

function set_pywal_colors()
   local pywal_colors_path = mp.command_native({
         "expand-path", "~/.cache/wal/colors.json"
   })
   local colors = utils.parse_json(read_file(pywal_colors_path))
   local background = colors["special"]["background"]
   local foreground = colors["special"]["foreground"]
   local cyan = colors["colors"]["color14"]

   -- general
   mp.set_property("background", background)

   -- playlist view and contact sheet
   -- set_script_opt("playlist_view-background_color", "00FF00")
   set_script_opt("playlist_view-background_opacity", "AA")
   set_script_opt("contact_sheet-background_opacity", "AA")

   set_script_opt("playlist_view-normal_border_color", plain_color(background))
   set_script_opt("contact_sheet-normal_border_color", plain_color(background))
   -- currently displayed image
   set_script_opt("playlist_view-active_border_color", plain_color(cyan))
   set_script_opt("contact_sheet-active_border_color", plain_color(cyan))
   -- gallery selection
   set_script_opt("playlist_view-selected_border_color", plain_color(foreground))
   set_script_opt("contact_sheet-selected_border_color", plain_color(foreground))
   -- flagged_border_color
end

set_pywal_colors()

-- * Playlist View Thumbnail Size Cycling
local playlist_view_open = false
local contact_sheet_open = false

-- adding these in order to keep track of whether gallery is open
function toggle_playlist_view()
   if contact_sheet_open then
      mp.command("script-message contact-sheet-close")
      contact_sheet_open = false
   end
   mp.command("script-message playlist-view-toggle")
   playlist_view_open = not playlist_view_open
end

function toggle_contact_sheet()
   if playlist_view_open then
      mp.command("script-message playlist-view-close")
      playlist_view_open = false
   end
   mp.command("script-message contact-sheet-toggle")
   contact_sheet_open = not contact_sheet_open
end

function reset_gallery_open_status()
   playlist_view_open = false
   contact_sheet_open = false
end

mp.add_hook("on_load", 50, reset_gallery_open_status)

mp.add_key_binding(nil, "noct-toggle-playlist-view", toggle_playlist_view)
mp.add_key_binding(nil, "noct-toggle-contact-sheet", toggle_contact_sheet)


local sizes = {384, 536}
local size_index = 1

function cycle_thumbnail_size(script, increment)
   size_index = size_index + (increment or 1)
   if size_index > #sizes then
      size_index = 1
   elseif size_index < 1 then
      size_index = #sizes
   end
   local option = script .. "-thumbnail_size"
   local value = "{" .. sizes[size_index] .. "," .. sizes[size_index] .. "}"
   set_script_opt(option, value)
end

function zoom_in_or_cycle_thumbnail_size()
   if playlist_view_open then
      cycle_thumbnail_size("playlist_view", 1)
   elseif contact_sheet_open then
      cycle_thumbnail_size("contact_sheet", 1)
   else
      mp.command("no-osd add video-zoom 0.2")
   end
end

function zoom_out_or_cycle_thumbnail_size()
   if playlist_view_open then
      cycle_thumbnail_size("playlist_view", -1)
   elseif contact_sheet_open then
      cycle_thumbnail_size("contact_sheet", -1)
   else
      mp.command("no-osd add video-zoom -0.2")
   end
end

mp.add_key_binding(nil, "zoom-in-or-cycle-thumbnail-size",
                   zoom_in_or_cycle_thumbnail_size)
mp.add_key_binding(nil, "zoom-out-or-cycle-thumbnail-size",
                   zoom_out_or_cycle_thumbnail_size)
