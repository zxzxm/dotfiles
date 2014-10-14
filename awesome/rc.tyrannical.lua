-- Standard awesome library
local gears      = require("gears")
local awful      = require("awful")
awful.rules      = require("awful.rules")
require("awful.autofocus")
-- Widgets and layout libraries
local wibox      = require("wibox")
local vicious    = require("vicious")
local blingbling = require("blingbling")
-- Theme handling library
local beautiful  = require("beautiful")
-- Notification library
local naughty    = require("naughty")
local menubar    = require("menubar")
-- Utils
require("utils.run_once")
-- Other libraries
-- local keydoc  = require("keydoc")
local tyrannical = require("tyrannical")
local scratch    = require("scratch")
-- local powerline  = require("powerline")

-- Autoruns   ..................................... &autoruns ...
run_once("unagi")
run_once("synapse -s")
run_once("clipit")
run_once("pa-applet")
run_once("xscreensaver -no-splash")
run_once("xflux -z 20910")

-- Notification Theme ............................. &notifications ...
naughty.config.defaults.width                  = 450
naughty.config.presets.critical.width          = 300
naughty.config.presets.critical.bg             = '#91231c'
-- naughty.config.presets.critical.timeout        = 5
-- naughty.config.presets.screen               = 1
-- naughty.config.presets.position             = "top_right"
-- naughty.config.presets.margin               = 4
-- naughty.config.presets.height               = 16
-- naughty.config.presets.normal.width         = 300
-- naughty.config.presets.gap                  = 1
naughty.config.presets.ontop                   = true
naughty.config.defaults.font                   = "Arial 11"
-- naughty.config.presets.icon                 = nil
-- naughty.config.presets.icon_size            = 16
naughty.config.presets.normal.fg               = beautiful.fg_focus or '#ffffff'
naughty.config.presets.normal.bg               = '#3F3F3f'
naughty.config.presets.normal.border_color    = '#535d6c'
-- naughty.config.presets.border_width         = 1
naughty.config.presets.normal.timeout          = 3

--  Error handling   ..................................... &errors ...
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "Oops, there were errors during startup!",
                    text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
   local in_error = false
   awesome.connect_signal("debug::error", function (err)
                             -- Make sure we don't go into an endless error loop
                             if in_error then return end
                             in_error = true

                             naughty.notify({ preset = naughty.config.presets.critical,
                                              title = "Oops, an error happened!",
                                              text = err })
                             in_error = false
   end)
end

-- Theme ................................................ &themes ...
beautiful.init("/home/thermans/.config/awesome/themes/zenburn/theme.lua")
-- beautiful.init("/usr/share/awesome/themes/zenburn/theme.lua")

--  Variable definitions
terminal   = "urxvtc"
browser    = "firefox"
im         = "pidgin"
mail       = "thunderbird"
editor     = "emacsclient -n -c"
editor_cmd = terminal .. " -e " .. editor
-- Default modkey.
modkey = "Mod4"     -- Start key

-- Layouts .............................................. &layouts ...
local layouts =
   {
      awful.layout.suit.floating,       -- 1
      awful.layout.suit.tile,           -- 2
      awful.layout.suit.tile.right,     -- 3
      awful.layout.suit.tile.left,      -- 4
      awful.layout.suit.tile.bottom,    -- 5
      awful.layout.suit.fair,           -- 6
      awful.layout.suit.max,            -- 7
      awful.layout.suit.max.fullscreen, -- 8
      awful.layout.suit.magnifier       -- 9
   }


--  Wallpaper -- ......................................... &wallpaper ...
if beautiful.wallpaper then
   for s = 1, screen.count() do
      gears.wallpaper.maximized(beautiful.wallpaper, s, true)
   end
end


--  Tags ................................................... &tags ...
-- Define a tag table which hold all screen tags.
tyrannical.tags = {
   {
      name        = "term",                 -- Call the tag "Term"
      init        = true,                   -- Load the tag on startup
      exclusive   = true,                   -- Refuse any other type of clients (by classes)
      screen      = {1,2},                  -- Create this tag on screen 1 and screen 2
      position    = 1,
      layout      = awful.layout.suit.tile, -- Use the tile layout
      selected    = true,
      exec_once   = terminal,
      class       = { --Accept the following classes, refuse everything else (because of "exclusive=true")
         "urxvt" , "aterm","URxvt","konsole","terminator","gnome-terminal"
      }
   },
   {
      name        = "emacs",
      init        = true,
      exclusive   = true,
      screen      = 1,
      position    = 2,
      exec_once   = editor,
      clone_on    = 2, -- Create a single instance of this tag on screen 1, but also show it on screen 2
                       -- The tag can be used on both screen, but only one at once
      layout      = awful.layout.suit.max                          ,
      class = {
         "Emacs"
      }
   },
   {
      name        = "www",
      init        = true,
      exclusive   = true,
      position    = 3,
      screen      = {1, 2},
      exec_once   = browser,
      layout      = awful.layout.suit.max,      -- Use the max layout
      class = {
         "Firefox", "Chrome", "luakit", "Dwb"
      }
   },
   {
      name        = "im",
      init        = true,
      exclusive   = true,
      screen      = 1,
      position    = 4,
      ncol        = 2,
      mwfact      = 0.80,
      layout      = awful.layout.suit.tile.right,
      exec_once   = im,
      class = {
         "Pidgin"
      }
   },
   {
      name        = "doc",
      init        = false, -- This tag wont be created at startup, but will be when one of the
                           -- client in the "class" section will start. It will be created on
                           -- the client startup screen
      exclusive   = true,
      layout      = awful.layout.suit.max,
      class       = {
         "MuPDF"         , "Zathura"         , "Evince"     , "libreoffice-writer"
      },
   },
   {
      name        = "ldap",
      init        = false,
      exclusive   = true,
      layout      = awful.layout.suit.floating,
      class       = {
         "lbe-ui-BrowserAp",
      },
   },
   {
      name        = "music",
      init        = false,
      exclusive   = false,
      layout      = awful.layout.suit.floating,
      class       = {
         "Vlc", "Ario", "Clementine",
      },
   },
   {
      name        = "multixterm",
      init        = false,
      exclusive   = false,
      layout      = awful.layout.suit.floating,
      class       = {
         "Multixterm", "Xterm", "xterm",
      },
   },
   {
      name        = "gimp",
      init        = false,
      exclusive   = true,
      layout      = awful.layout.suit.max,
      class       = {
         "Gimp-2.8"
      },
   },
   {
      name        = "video",
      init        = false,
      exclusive   = true,
      layout      = awful.layout.suit.floating,
      class       = {
         "Mplayer", "Smplayer", "Vlc"
      },
   },

}

-- Ignore the tag "exclusive" property for the following clients (matched by classes)
tyrannical.properties.intrusive = {
   "feh",  "Synapse", "Spacefm", "Keepassx", "KeePass2", "Easystroke", "Lxappearance", "Clipit", "Seahorse-ssh-askpass"
}

-- Ignore the tiled layout for the matching clients
tyrannical.properties.floating = {
   "MPlayer"      , "feh"             , "Meld"       , "Keepassx"     , "Cheese"         ,
   "Prefs.py"     , "Nvidia-settings" , "Easystroke" , "KeePass2", "Lxappearance", "Seahorse-ssh-askpass"
}

-- Make the matching clients (by classes) on top of the default layout
tyrannical.properties.ontop = {
   "Xephyr"       , "Synapse"
}

-- Force the matching clients (by classes) to be centered on the screen on init
tyrannical.properties.centered = {
   "Spacefm"
}

tyrannical.settings.block_children_focus_stealing = true --Block popups ()
tyrannical.settings.group_children = true --Force popups/dialogs to have the same tags as the parent client

--  Menu ................................................... &menu ...
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
}
                       })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it


-- Widgets .................................................. &widgets ...


-- Date
-- dateicon    = wibox.widget.imagebox()
-- dateicon:set_image(beautiful.widget_date)
datewidget  = wibox.widget.textbox()
vicious.register(datewidget, vicious.widgets.date, " %I:%M %p %a %b %d", 61)


-- Separator
separator   = wibox.widget.imagebox()
separator:set_image(beautiful.widget_separator)

-- Memory
mem_graph = blingbling.line_graph({ height = 18,
                                      width = 100,
                                      show_text = true,
                                      label = "Mem: $percent %",
                                     })

vicious.register(mem_graph, vicious.widgets.mem, '$1', 2)

-- CPU
cpu_graph = blingbling.line_graph({ height = 18,
                                    width = 100,
                                    show_text = true,
                                    label = "Load: $percent %",
                                    rounded_size = 0.3,
                                    graph_background_color = "#00000033"
})
vicious.register(cpu_graph, vicious.widgets.cpu,'$1',2)

blingbling.popups.htop(cpu_graph, { title_color = beautiful.notify_font_color_1 , user_color = beautiful.notify_font_color_2 , root_color = beautiful.notify_font_color_3 , terminal =  terminal })



-- Weather
-- weathericon = wibox.widget.imagebox()
-- weathericon:set_image(beautiful.widget_weather)
weatherwidget  = wibox.widget.textbox()
vicious.register(weatherwidget, vicious.widgets.weather, "${sky} ${tempf}° | ", 300, "KIAD")

--  Wibox ................................................... &wibox ...

-- Create a wibox for each screen and add it
mywibox = {}
mybottombox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
   awful.button({        }, 1, awful.tag.viewonly),
   awful.button({ modkey }, 1, awful.client.movetotag),
   awful.button({        }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, awful.client.toggletag),
   awful.button({        }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
   awful.button({        }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
)
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
                   if c == client.focus then
                      c.minimized = true
                   else
                      -- Without this, the following
                      -- :isvisible() makes no sense
                      c.minimized = false
                      if not c:isvisible() then
                         awful.tag.viewonly(c:tags()[1])
                      end
                      -- This will also un-minimize
                      -- the client, if needed
                      client.focus = c
                      c:raise()
                   end
   end),
   awful.button({ }, 3, function ()
                   if instance then
                      instance:hide()
                      instance = nil
                   else
                      instance = awful.menu.clients({ width=250 })
                   end
   end),
   awful.button({ }, 4, function ()
                   awful.client.focus.byidx(1)
                   if client.focus then client.focus:raise() end
   end),
   awful.button({ }, 5, function ()
                   awful.client.focus.byidx(-1)
                   if client.focus then client.focus:raise() end
end))

for s = 1, screen.count() do
   -- Create a promptbox for each screen
   mypromptbox[s] = awful.widget.prompt()
   -- Create an imagebox widget which will contains an icon indicating which layout we're using.
   -- We need one layoutbox per screen.
   mylayoutbox[s] = awful.widget.layoutbox(s)
   mylayoutbox[s]:buttons(awful.util.table.join(
                             awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                             awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                             awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                             awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
   -- Create a taglist widget
   mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)
   -- mytaglist[s] = blingbling.tagslist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

   -- Create a tasklist widget
   mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

   -- Create the top wibox
   mywibox[s] = awful.wibox({ position = "top", screen = s, ontop = false })

   -- Widgets that are aligned to the left
   local top_left_layout = wibox.layout.fixed.horizontal()
   top_left_layout:add(mytaglist[s])
   top_left_layout:add(mylauncher)
   top_left_layout:add(mypromptbox[s])

   -- Widgets that are aligned to the right
   local top_right_layout = wibox.layout.fixed.horizontal()
   top_right_layout:add(mylayoutbox[s])

   -- Now bring it all together (with the tasklist in the middle)
   local layout = wibox.layout.align.horizontal()
   layout:set_left(top_left_layout)
   layout:set_middle(mytasklist[s])
   layout:set_right(top_right_layout)

   mywibox[s]:set_widget(layout)

   -- Create the bottom wibox
   mybottombox[s] = awful.wibox({ position = "bottom", screen = s, ontop = false })
   local bottom_right_layout = wibox.layout.fixed.horizontal()
   if s == 1 then
      -- bottom_right_layout:add(weathericon)
      bottom_right_layout:add(weatherwidget)
      bottom_right_layout:add(datewidget)
      -- bottom_right_layout:add(powerline_widget)
      bottom_right_layout:add(separator)
      bottom_right_layout:add(wibox.widget.systray())
   end
   if s == 2 then
      bottom_right_layout:add(mem_graph)
      bottom_right_layout:add(cpu_graph)
   end

   local bottom_layout = wibox.layout.align.horizontal()
   bottom_layout:set_right(bottom_right_layout)
   mybottombox[s]:set_widget(bottom_layout)

end


--  Mouse bindings
root.buttons(awful.util.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end)
                                  )
)


-- Keys ............................................................ &keys ...
globalkeys = awful.util.table.join(

   -- Layout manipulation
   -- keydoc.group("Layout Manipulation"),
   awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
   awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
   awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
   awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
   awful.key({ modkey,           }, "Tab",
             function ()
                awful.client.focus.history.previous()
                if client.focus then
                   client.focus:raise()
                end
   end),

   awful.key({ modkey,           }, "j",
             function ()
                awful.client.focus.byidx( 1)
                if client.focus then client.focus:raise() end
   end),
   awful.key({ modkey,           }, "k",
             function ()
                awful.client.focus.byidx(-1)
                if client.focus then client.focus:raise() end
   end),

   -- Tags
   -- keydoc.group("Tags"),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext), -- Next tag
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev), -- Previous tag
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore), -- Last tag

   awful.key({ modkey, }, "d", function () awful.tag.delete() end), -- Delete tag

   -- Add tag
   awful.key({ modkey,           }, "a",
             function ()
                awful.prompt.run({ prompt = "New tag name: " },
                                 mypromptbox[mouse.screen].widget,
                                 function(new_name)
                                    if not new_name or #new_name == 0 then
                                       return
                                    else
                                       props = {selected = true}
                                       if tyrannical.tags_by_name[new_name] then
                                          props = tyrannical.tags_by_name[new_name]
                                       end
                                       t = awful.tag.add(new_name, props)
                                       awful.tag.viewonly(t)
                                    end
                                 end
                )
   end),

   -- Rename tag
   awful.key({ modkey, "Shift"   }, "r",
             function ()
                awful.prompt.run({ prompt = "New tag name: " },
                                 mypromptbox[mouse.screen].widget,
                                 function(new_name)
                                    if not new_name or #new_name == 0 then
                                       return
                                    else
                                       local screen = mouse.screen
                                       local tag = awful.tag.selected(screen)
                                       if tag then
                                          tag.name = new_name
                                       end
                                    end
                end)
   end),



   -- Launchers
   -- keydoc.group("Launchers"),
   awful.key({ modkey,           }, 'w', function ()
                local matcher = function (c)
                   return awful.rules.match(c, {class = 'Firefox'})
                end
                awful.client.run_or_raise(browser, matcher)  end),
   awful.key({ modkey,           }, "e",      function ()
                local matcher = function (c)
                   return awful.rules.match(c, {class = 'Emacs'})
                end
                awful.client.run_or_raise(editor, matcher)  end),
   awful.key({ modkey,           }, "i",      function ()
                local matcher = function (c)
                   return awful.rules.match(c, {class = 'Pidgin'})
                end
                awful.client.run_or_raise(im, matcher)  end),

   -- Special keystrokes
   awful.key({ modkey,           }, "x",      function () awful.util.spawn("/home/thermans/bin/putpw") end),
   awful.key({ modkey, "Shift"   }, "i",      function () scratch.pad.toggle() end),
   -- awful.key({ modkey,           }, "s",      function (c) scratch.pad.set(c, 0.60, 0.60, true) end),
   awful.key({ modkey            }, "t",      function () scratch.drop("urxvtc", "top", "center", 0.40) end),

   -- Multi-media (from https://github.com/sseemayer/pavolume)
   --awful.key({                   }, "XF86AudioRaiseVolume", function() awful.util.spawn("pavolume volup") end),
   --awful.key({                   }, "XF86AudioLowerVolume", function() awful.util.spawn("pavolume voldown") end),
   --awful.key({                   }, "XF86AudioMute",        function() awful.util.spawn("pavolume mutetoggle") end),

   -- Standard program
   -- keydoc.group("Standard"),
   awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
   awful.key({ modkey, "Control" }, "r",      awesome.restart),
   awful.key({ modkey, "Shift"   }, "q",      awesome.quit),

   awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
   awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
   awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),

   awful.key({ modkey, "Control" }, "n", awful.client.restore),

   -- Prompt
   awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

   -- awful.key({ modkey }, "x",
   --           function ()
   --               awful.prompt.run({ prompt = "Run Lua code: " },
   --               mypromptbox[mouse.screen].widget,
   --               awful.util.eval, nil,
   --               awful.util.getdir("cache") .. "/history_eval")
   --           end),
   -- Menubar
   awful.key({ modkey }, "p", function() menubar.show() end)

   -- Show key bindings
   -- awful.key({ modkey, }, "F1", keydoc.display, "Display this help")




)

clientkeys = awful.util.table.join(

   -- keydoc.group("Client Manipulation"),
   awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
   awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
   -- awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),

   -- move focus to other screen
   awful.key({ modkey,           }, "n",     function () screenfocus(1)                     end),

   -- awful.key({ modkey,           }, "n",
   --     function (c)
   --         -- The client currently has the input focus, so it cannot be
   --         -- minimized, since minimized clients can't have the focus.
   --         c.minimized = true
   --     end),
   awful.key({ modkey,           }, "m",
             function (c)
                c.maximized_horizontal = not c.maximized_horizontal
                c.maximized_vertical   = not c.maximized_vertical
   end)
)

-- Bind all key numbers to tags
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      local tag = awful.tag.gettags(client.focus.screen)[i]
                      if client.focus and tag then
                          awful.client.movetotag(tag)
                     end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      local tag = awful.tag.gettags(client.focus.screen)[i]
                      if client.focus and tag then
                          awful.client.toggletag(tag)
                      end
                  end))
end

clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)


--  Rule --------------------------------------------------- &rules ---
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = awful.client.focus.filter,
                    size_hints_honor = false,
                    keys = clientkeys,
                    buttons = clientbuttons } },
   { rule = { class = "MPlayer" },
     properties = { floating = true } },
   { rule = { class = "pinentry" },
     properties = { floating = true } },

   -- Pidgin ---------------------------------------------------
   { rule       = { class = "Pidgin", role = "conversation" },
     properties = { callback = awful.client.setslave } },

   -- Firefox ---------------------------------------------------
   { rule = { class = "Firefox", name = "Firefox Preferences"},
     properties = { floating = true }},

   { rule = { class = "Firefox", instance = "Places"},
     properties = { floating = true }},

   { rule = { class = "Firefox", instance = "Browser"},
     properties = { floating = true }},

   { rule = { class = "Firefox", name = "Saved Passwords"},
     properties = { floating = true }},

   { rule = { class = "Firefox", role = "view-source"},
     properties = { floating = true }},

   { rule = { class = "Firefox", name = "Compose Mail - thermans@gmail.com - Gmail - Firefox"},
     properties = { floating = true }},

   -- Launchers ------------------------------------------------
   { rule = { class = "Synapse"},
     properties = { border_width = 0 }},

   { rule = { class = "Kupfer.py"},
     properties = { border_width = 0 }},

   { rule = { class = "Kupfer.py", name = "Kupfer Preferences"},
     properties = { floating = true }},

   { rule = { class = "Launchy"},
     properties = { border_width = 0 }},

   -- Emacs ---------------------------------------------------
   { rule = { class = "Emacs", name = "Ediff"},
     properties = { floating = true }},

   -- Thunderbird ---------------------------------------------------
   { rule = { class = "Thunderbird", name = "Address Book" },
     properties = { floating = true } },
   -- -- Sublime 2
   -- { rule = { class = "sublime_text" },
   --   properties = { tag = tags[2][2], switchtotag = true }},

   -- -- Sublime 3
   -- { rule = { class = "Subl3" },
   --   properties = { tag = tags[2][2], switchtotag = true }},

   -- -- Eclipse
   -- { rule = { class = "Eclipse" },
   --   properties = { tag = tags[2][5], switchtotag = true } },
   -- -- LightTable
   -- { rule = { class = "Ltbin" },
   --   properties = { tag = tags[1][7], switchtotag = true } },


   -- Floaters ----------------------------------------------- &floaters ---
   { rule = { class = "VirtualBox" },
     properties = { floating = true } },
   { rule = { class = "Keepassx" },
     properties = { floating = true } },
   { rule = { name = "MusicBrainz Picard" },
     properties = { floating = true } },
   { rule = { class = "Mozilla", name = "SQLite Manager" },
     properties = { floating = true } },
   { rule = { class = "Meld" },
     properties = { floating = true } },
   { rule = { class = "Skype" },
     properties = { floating = true } },
   { rule = { class = "Cheese" },
     properties = { floating = true } },
   { rule = { class = "Guake" },
     properties = { floating = true } },
   { rule = { class = "Prefs.py" },
     properties = { floating = true } },
   { rule = { class = "Evince" },
     properties = { floating = true } },
   { rule = { class = "Nvidia-settings" },
     properties = { floating = true } },
   { rule = { class = "Qtconfig" },
     properties = { floating = true } },
   { rule = { class = "Gconf-editor" },
     properties = { floating = true } },
   { rule = { class = "Pcmanfm" },
     properties = { floating = true } },
   { rule = { class = "Pavucontrol" },
     properties = { floating = true } },
   { rule = { class = "Nautilus" },
     properties = { floating = true } },
   { rule = { class = "Thunar" },
     properties = { floating = true } },
   { rule = { class = "Zathura" },
     properties = { floating = true } },
   { rule = { class = "Zenmap" },
     properties = { floating = true } },
   { rule = { class = "Multixterm" },
     properties = { floating = true } },
   { rule = { class = "Wine" },
     properties = { floating = true } },
   { rule = { class = "Vinagre" },
     properties = { floating = true } },
   { rule = { class = "Qpaeq" },
     properties = { floating = true } },
   { rule = { class = "Proximity.py" },
     properties = { floating = true } },
   { rule = { class = "Nitrogen" },
     properties = { floating = true } },
   { rule = { class = "Vlc" },
     properties = { floating = true } },
   { rule = { class = "mpv" },
     properties = { floating = true } },
   { rule = { class = "feh" },
     properties = { floating = true } },
   { rule = { title = "Event Tester" },
     properties = { floating = true } },
   { rule = { class = "Rapidsvn" },
     properties = { floating = true } },
   { rule = { class = "Qsvn" },
     properties = { floating = true } },
   { rule = { class = "Spacefm" },
     properties = { floating = true } },
   { rule = { class = "Unison-x11" },
     properties = { floating = true } },
   { rule = { class = "Unison-gtk2" },
     properties = { floating = true } },
   { rule = { class = "Adl" }, -- Adobe Air
      properties = { floating = true } },
   { rule = { class = "Mysql-workbench-bin" },
     properties = { floating = true } },
   { rule = { class = "Calibre" },
     properties = { floating = true } },
   { rule = { class = "Sound-juicer" },
     properties = { floating = true } },
   { rule = { class = "Ario" },
     properties = { floating = true } },
   { rule = { class = "Hgview" },
     properties = { floating = true } },
   { rule = { class = "Gnome-font-viewer" },
     properties = { floating = true } },
   { rule = { class = "Tkremind" },
     properties = { floating = true } },
   { rule = { class = "java-lang-Thread", name = "sun-awt-X11-XFramePeer" },
     properties = { floating = true } },
   { rule = { class = "sun-applet-PluginMain", name = "sun-awt-X11-XFramePeer" },
     properties = { floating = true } },
   { rule = { name="Exaile" },
     properties = { floating = true } },
   { rule = { class = "Pulseaudio-equalizer.py" },
     properties = { floating = true } },
   { rule = { class = "sun-tools-jconsole-JConsole" },
     properties = { floating = true } },
   { rule = { class = "lbe-ui-BrowserApp", name = "sun-awt-X11-XFramePeer" },
     properties = { floating = true } }

}


--  Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
                         -- Enable sloppy focus
                         c:connect_signal("mouse::enter", function(c)
                                             if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
                                             and awful.client.focus.filter(c) then
                                                client.focus = c
                                             end
                         end)

                         if not startup then
                            -- Set the windows at the slave,
                            -- i.e. put it at the end of others instead of setting it master.
                            -- awful.client.setslave(c)

                            -- Put windows in a smart way, only if they does not set an initial position.
                            if not c.size_hints.user_position and not c.size_hints.program_position then
                               awful.placement.no_overlap(c)
                               awful.placement.no_offscreen(c)
                            end
                         end

                         if c.class == "Pidgin" and c.role:find("conversation") then
                            awful.client.setslave(c)
                         end

                         local titlebars_enabled = false
                         if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
                            -- Widgets that are aligned to the left
                            local left_layout = wibox.layout.fixed.horizontal()
                            left_layout:add(awful.titlebar.widget.iconwidget(c))

                            -- Widgets that are aligned to the right
                            local right_layout = wibox.layout.fixed.horizontal()
                            right_layout:add(awful.titlebar.widget.floatingbutton(c))
                            right_layout:add(awful.titlebar.widget.maximizedbutton(c))
                            right_layout:add(awful.titlebar.widget.stickybutton(c))
                            right_layout:add(awful.titlebar.widget.ontopbutton(c))
                            right_layout:add(awful.titlebar.widget.closebutton(c))

                            -- The title goes in the middle
                            local title = awful.titlebar.widget.titlewidget(c)
                            title:buttons(awful.util.table.join(
                                             awful.button({ }, 1, function()
                                                             client.focus = c
                                                             c:raise()
                                                             awful.mouse.client.move(c)
                                             end),
                                             awful.button({ }, 3, function()
                                                             client.focus = c
                                                             c:raise()
                                                             awful.mouse.client.resize(c)
                                             end)
                            ))

                            -- Now bring it all together
                            local layout = wibox.layout.align.horizontal()
                            layout:set_left(left_layout)
                            layout:set_right(right_layout)
                            layout:set_middle(title)

                            awful.titlebar(c):set_widget(layout)
                         end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
