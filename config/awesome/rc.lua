local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")

---@param f function|table
---@return function
local function fix(f, x)
	return function(...)
		return f(x, ...)
	end
end

local terminal = os.getenv("TERMINAL") or "xterm"
local editor = os.getenv("EDITOR") or (terminal .. " -e " .. "nano")

beautiful.init(gears.filesystem.get_themes_dir() .. "gtk" .. "/theme.lua")

local Super = "Mod4"

local mouse_click = {
	left = 1,
	middle = 2,
	right = 3,
	scroll_up = 4,
	scroll_down = 5,
}

local global_keys = gears.table.join(
	-- stylua: ignore start

	-- change client focus
	awful.key({ Super }, "j", fix(awful.client.focus.byidx, 1), { description = "focus next", group = "client" }),
	awful.key({ Super }, "k", fix(awful.client.focus.byidx, -1), { description = "focus previous", group = "client" }),

	-- change tag focus
	awful.key({ Super }, "h", awful.tag.viewprev, { description = "view previous", group = "tag" }),
	awful.key({ Super }, "l", awful.tag.viewnext, { description = "view next", group = "tag" }),

	awful.key({ Super }, "Return", fix(awful.spawn, terminal .. " --drop-down"), { description = "open a drop-down terminal", group = "launcher" }),
	awful.key({ Super, "Shift" }, "Return", fix(awful.spawn, terminal), { description = "open a terminal", group = "launcher" }),
	awful.key({ Super }, "e", fix(awful.spawn, editor), { description = "open a text editor", group = "launcher" }),
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

do
	local buttons = gears.table.join(
		awful.button({ Super }, mouse_click.left, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
			awful.mouse.client.move(c)
		end),
		awful.button({ Super }, mouse_click.right, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
			awful.mouse.client.resize(c)
		end)
	)

	awful.rules.rules = {
		{
			rule = {},
			properties = {
				focus = awful.client.focus.filter,
				raise = true,
				buttons = buttons,
				screen = awful.screen.preferred,
				placement = awful.placement.no_overlap + awful.placement.no_offscreen
			}
		}
	}
end

-- focus follows mouse.
client.connect_signal("mouse::enter", function(c)
	c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)
