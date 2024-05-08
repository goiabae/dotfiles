local awful = require("awful")
local gears = require("gears")

---@param f function|table
---@return function
local function fix(f, x)
	return function(...)
		return f(x, ...)
	end
end

local terminal = os.getenv("TERMINAL") or "xterm"

local Super = "Mod4"

local global_keys = gears.table.join(
	-- stylua: ignore start
	awful.key({ Super, "Shift" }, "Return", fix(awful.spawn, terminal), { description = "open a terminal", group = "launcher" }),
	awful.key({ Super, "Shift" }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),
	awful.key({ Super, "Shift" }, "e", awesome.quit, { description = "quit awesome", group = "awesome" })
	--stylua: ignore end
)

awful.layout.layouts = { awful.layout.suit.floating }

root.keys(global_keys)

local tag_count = 6

awful.screen.connect_for_each_screen(function (s)
	local tag_names = {}
	for i = 1, tag_count do
		table.insert(tag_names, tostring(i))
	end
	awful.tag(tag_names, s, awful.layout.layouts[1])
end)

awful.rules.rules = {
	{
		rule = {},
		properties = {
			focus = awful.client.focus.filter,
			raise = true,
			screen = awful.screen.preferred,
			placement = awful.placement.no_overlap + awful.placement.no_offscreen
		}
	}
}
