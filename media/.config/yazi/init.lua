require("starship"):setup()

-- file status
require("git"):setup()

-- acts like ranger (enter directories, open marked files, open file at point;
-- priority in that order)
require("smart-enter"):setup {
    open_multi = true,
}

-- https://yazi-rs.github.io/docs/tips/#symlink-in-status
Status:children_add(function(self)
	local h = self._current.hovered
	if h and h.link_to then
		return " -> " .. tostring(h.link_to)
	else
		return ""
	end
end, 3300, Status.LEFT)

-- modification time
-- https://github.com/sxyazi/yazi/discussions/1658
Status:children_add(function()
	local h = cx.active.current.hovered
	return ui.Line({
		ui.Span(os.date("%Y-%m-%d %I:%M:%S %p", tostring(h.cha.mtime):sub(1, 10))):fg("blue"),
		ui.Span(" "),
	})
end, 500, Status.RIGHT)

-- user:group
-- https://yazi-rs.github.io/docs/tips#user-group-in-status
Status:children_add(function()
	local h = cx.active.current.hovered
	if h == nil or ya.target_family() ~= "unix" then
		return ""
	end

	return ui.Line {
		ui.Span(ya.user_name(h.cha.uid) or tostring(h.cha.uid)):fg("magenta"),
		":",
		ui.Span(ya.group_name(h.cha.gid) or tostring(h.cha.gid)):fg("magenta"),
		" ",
	}
end, 500, Status.RIGHT)
