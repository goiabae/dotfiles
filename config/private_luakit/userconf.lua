require "adblock"

-- require "downloads"
-- downloads.default_dir = os.getenv("HOME") .. "/dl"

local modes = require "modes"
modes.add_binds("normal", {
    { "<Control-C>", "Copy selected text.", function ()
        luakit.selection.clipboard = luakit.selection.primary
    end},
})

local settings = require "settings"
settings.webview.zoom_level = 80

local engines = settings.window.search_engines
-- engines.aur = "https://aur.archlinux.org/packages.php?O=0&K=%s&do_Search=Go"
-- engines.aw  = "https://wiki.archlinux.org/index.php/Special:Search?fulltext=Search&search=%s"
engines.void = "https://voidlinux.org/packages/?arch=x86_64&q=%s"
engines.reddit = "https://teddit.net/search?q=%s&nsfw=on"

