-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Special libraries
require("shifty")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
-- Utilities
require('myutils')
require('scratch')
-- Widgets
require("vicious")
require("vain")
-- require("timgimp")
vain.widgets.terminal = 'urxvtc'

print("Entered rc.lua: " .. os.time())

-- {{{ Error handling  ..................................... &errors ...
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
    awesome.add_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- Themes ................................................ &themes ...
--beautiful.init("/usr/share/awesome/themes/zenburn/theme.lua")
beautiful.init("/home/thermans/.config/awesome/themes/default/theme.lua")
 -- }}}

-- {{{ Widgets .......................................... &widgets ...

vicious.cache(vicious.widgets.fs)

-- Separator
separator = widget({ type = "imagebox" })
separator.image = image(beautiful.widget_separator)

-- Date
dateicon       = widget({ type = "imagebox" })
dateicon.image = image(beautiful.widget_date)
datewidget     = widget({ type = "textbox" })
vicious.register(datewidget, vicious.widgets.date, "%a %b %d %l:%M %P", 61)

-- Weather
weatherwidget  = widget({ type = "textbox"})
vicious.register(weatherwidget, vicious.widgets.weather, "${tempf}°", 300, "KIAD")

-- Memory
memorywidget = vain.widgets.memusage()

-- }}}

-- {{{ Variables ...................................... &variables ...

-- Default apps
terminal   = "urxvtc"
browser    = "firefox"
im         = "pidgin"
mail       = "thunderbird"
editor     = "emacsclient -n -c"
editor_cmd = terminal .. " -e " .. editor

-- Default modkeys.
altkey = "Mod1"                 -- Alt
modkey = "Mod4"                 -- Start

local home   = os.getenv("HOME")
local exec   = awful.util.spawn
local scount = screen.count()

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
   awful.layout.suit.floating,    -- 1
   awful.layout.suit.tile,        -- 2
   awful.layout.suit.tile.right,  -- 3
   awful.layout.suit.tile.left,   -- 4
   awful.layout.suit.tile.bottom, -- 5
   awful.layout.suit.fair,        -- 6
   awful.layout.suit.max,         -- 7
   -- tim.layout.gimp               -- 8
}
-- }}}

-- {{{ Shifty ................................................. &shifty ...
shifty.config.tags = { 
    term = {
        layout      = awful.layout.suit.left,
        mwfact      = 0.60,
        exclusive   = false,
        position    = 1,
        init        = true,
        screen      = 1,
        slave       = true,
    },
    emacs = {
        layout      = awful.layout.suit.tile.fair,
        mwfact      = 0.65,
        exclusive   = true,
        max_clients = 1,
        position    = 2,
        spawn       = editor,
    },
    www = {
        layout      = awful.layout.suit.tile.fair,
        mwfact      = 0.65,
        exclusive   = true,
        max_clients = 1,
        position    = 3,
        spawn       = browser,
    },
    im = {
       layout       = awful.layout.suit.tile.right,
       mwfact       = 0.80,
       ncol         = 2,
       exclusive    = true,
       position     = 4,
       spawn        = im,
    },
    mail = {
        layout      = awful.layout.suit.float,
        mwfact      = 0.55,
        exclusive   = false,
        position    = 5,
        spawn       = mail,
        screen      = 2,
        slave       = true
    },
    media = {
        layout      = awful.layout.suit.float,
        exclusive   = false,
        position    = 8,
    },
    office = {
        layout      = awful.layout.suit.tile,
        position    = 9,
    },
}

-- SHIFTY: application matching rules
-- order here matters, early rules will be applied first
shifty.config.apps = {
   {
      match = {
         "Synapse",
      },
      intrusive = true,
      slave = true,
   },
   {
      match = {
         "Emacs",
      },
      tag = "emacs",
   },
   {
      match = {
         "Navigator",
         "Vimperator",
      },
      tag = "www",
   },
   {
      match = {
         "Thunderbird",
      },
      tag = "mail",
   },
      {
      match = {
         "Pidgin",
      },
      tag = "im",
   },
   {
      match = {
         class = {
         "pcmanfm",
         },
      },
      intrusive = true,
      slave = true
   },
   {
      match = {
         "libreoffice-writer",
      },
      tag = "office",
   },
   {
      match = {
         "Mplayer.*",
         "Mirage",
         "gimp",
         "gtkpod",
         "Ufraw",
         "easytag",
      },
      tag = "media",
      nopopup = true,
   },
   {
      match = {
         "MPlayer",
         "Gnuplot",
         "galculator",
      },
      float = true,
   },
   {
      match = {
         terminal,
      },
      honorsizehints = false,
      slave = true,
   },
   {
      match = {""},
      buttons = awful.util.table.join(
         awful.button({}, 1, function (c) client.focus = c; c:raise() end),
         awful.button({modkey}, 1, function(c)
                         client.focus = c
                         c:raise()
                         awful.mouse.client.move(c)
                                   end),
         awful.button({modkey}, 3, awful.mouse.client.resize)
                                     )
   },
}

-- SHIFTY: default tag creation rules
-- parameter description
--  * floatBars : if floating clients should always have a titlebar
--  * guess_name : should shifty try and guess tag names when creating
--                 new (unconfigured) tags?
--  * guess_position: as above, but for position parameter
--  * run : function to exec when shifty creates a new tag
--  * all other parameters (e.g. layout, mwfact) follow awesome's tag API
shifty.config.defaults = {
    layout = awful.layout.suit.tile.bottom,
    ncol = 1,
    mwfact = 0.60,
    floatBars = true,
    guess_name = true,
    guess_position = true,
}

-- }}}

-- {{{ Menu ................................................ &menu ...
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },

   { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" })

-- Create a systray
mysystray = widget({ type = "systray" })

-- Create a wibox for each screen and add it
mywibox = {}
mybottombox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
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
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s, ontop = false})
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mytaglist[s],
            mylauncher,
            separator,
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s],
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }

    -- Bottom box (only on screen one)
    mybottombox = awful.wibox({ position = "bottom", screen = 1, ontop = false })
    mybottombox.widgets = {
       mysystray or nil,
       separator or nil,
       datewidget or nil,
       dateicon or nil,
       separator or nil,
       weatherwidget or nil,
       -- memorywidget,
       layout = awful.widget.layout.horizontal.rightleft
    }
    
    -- mybottombox[s] = awful.wibox({ position = "bottom", screen = s, ontop = false })
    -- mybottombox[s].widgets = {
    --    s == 1 and mysystray or nil,
    --    s == 1 and separator or nil,
    --    s == 1 and datewidget or nil,
    --    s == 1 and dateicon or nil,
    --    s == 1 and separator or nil,
    --    s == 1 and weatherwidget,
    --    s == 2 and memorywidget,
    --    layout = awful.widget.layout.horizontal.rightleft
    -- }
end
-- }}}

-- SHIFTY: initialize shifty
-- the assignment of shifty.taglist must always be after its actually
-- initialized with awful.widget.taglist.new()
shifty.taglist = mytaglist
shifty.init()

-- -------------------------------------------------------- &bindings ---
-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end)
))
-- }}}

-- {{{ Key bindings ............................... &global_bindings ...
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    -- Shifty: keybindings specific to shifty
    awful.key({modkey, "Shift"}, "d", shifty.del), -- delete a tag
    awful.key({modkey, "Shift"}, "n", shifty.send_prev), -- client to prev tag
    awful.key({modkey}, "n", shifty.send_next), -- client to next tag
    awful.key({modkey, "Control"},
              "n",
              function()
                  local t = awful.tag.selected()
                  local s = awful.util.cycle(screen.count(), t.screen + 1)
                  awful.tag.history.restore()
                  t = shifty.tagtoscr(s, t)
                  awful.tag.viewonly(t)
              end),
    awful.key({modkey}, "a", shifty.add), -- creat a new tag
    awful.key({modkey,}, "r", shifty.rename), -- rename a tag
    awful.key({modkey, "Shift"}, "a", -- nopopup new tag
    function()
        shifty.add({nopopup = true})
    end),

    --
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

    -- Layout manipulation
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

    -- Launchers
    awful.key({ modkey,           }, "Return", function () exec(terminal) end),
    awful.key({ modkey,           }, "e",      function () run_or_raise(editor,  { instance = "Emacs" })       end),
    awful.key({ modkey,           }, "w",      function () run_or_raise(browser, { instance = "Navigator" })   end),
    awful.key({ modkey,           }, "i",      function () run_or_raise(im,      { instance = "Pidgin" })      end),
    awful.key({ modkey,           }, "t",      function () run_or_raise(mail,    { instance = "Mail" })        end),

    -- Special keystrokes
    awful.key({ modkey,           }, "x",      function () exec("xdotool getactivewindow type utBjCxb7") end),

    -- Standard program
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    -- awful.key({ modkey, "Control" }, "n", awful.client.restore),
    awful.key({ modkey,           }, "n",     function () screenfocus(1) end),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey, "Shift"   }, "t",     function () scratch.drop("urxvt", "top") end)

    -- awful.key({ modkey }, "x",
    --           function ()
    --               awful.prompt.run({ prompt = "Run Lua code: " },
    --               mypromptbox[mouse.screen].widget,
    --               awful.util.eval, nil,
    --               awful.util.getdir("cache") .. "/history_eval")
    --           end)
)

-- ................................................. &client_bindings ...
clientkeys = awful.util.table.join(

    awful.key({ modkey            }, 'Home',   function() awful.client.moveresize(  0,  20,   0,   0) end),
    awful.key({ modkey            }, 'Insert', function() awful.client.moveresize(  0, -20,   0,   0) end),
    awful.key({ modkey            }, 'Delete', function() awful.client.moveresize(-20,   0,   0,   0) end),
    awful.key({ modkey            }, 'End',    function() awful.client.moveresize( 20,   0,   0,   0) end),

    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey, "Control" }, "t",      function (c) c.ontop = not c.ontop            end),
    -- awful.key({ modkey,           }, "n",
    --      function (c)
    --          -- The client currently has the input focus, so it cannot be
    --         -- minimized, since minimized clients can't have the focus.
    --          c.minimized = not c.minimized
    --      end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- SHIFTY: assign client keys to shifty for use in
-- match() function(manage hook)
shifty.config.clientkeys = clientkeys
shifty.config.modkey = modkey

-- Compute the maximum number of digit we need, limited to 9
for i = 1, (shifty.config.maxtags or 9) do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({modkey}, i, function()
            local t =  awful.tag.viewonly(shifty.getpos(i))
            end),
        awful.key({modkey, "Control"}, i, function()
            local t = shifty.getpos(i)
            t.selected = not t.selected
            end),
        awful.key({modkey, "Control", "Shift"}, i, function()
            if client.focus then
                awful.client.toggletag(shifty.getpos(i))
            end
            end),
        -- move clients to other tags
        awful.key({modkey, "Shift"}, i, function()
            if client.focus then
                t = shifty.getpos(i)
                awful.client.movetotag(t)
                awful.tag.viewonly(t)
            end
        end))
    end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules ----------------------------------------------- &rules ---
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     size_hints_honor = false,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    -- { rule = { class = "gimp" },
      -- properties = { floating = true } },

    { rule       = { class = "Pidgin", role = "conversation" },
      properties = { size_hints_honor = false, callback = awful.client.setslave } },

    -- { rule       = { class = "Pidgin", role = "conversation" },
    --   properties = { size_hints_honor = false, mwfact = 0.2 } },


    -- Firefox ---------------------------------------------------
    -- { rule = { class = "Firefox" },
    --  properties = { tag = tags[1][3], switchtotag = true } },

    { rule = { class = "Firefox", name = "Firefox Preferences"},
      properties = { floating = true }},

    { rule = { class = "Firefox", instance = "Places"},
      properties = { floating = true }},

    { rule = { class = "Firefox", instance = "Browser"},
      properties = { floating = true }},

    { rule = { class = "Firefox", role = "view-source"},
      properties = { floating = true }},


    { rule = { class = "Emacs", name = "Ediff"},
      properties = { floating = true }},

    { rule = { class = "Thunderbird", name = "Msgcompose" },
      properties = { floating = true } },

    { rule = { class = "Thunderbird", name = "Address Book" },
      properties = { floating = true } },

    -- Floaters ----------------------------------------------- &floaters ---
    { rule = { class = "VirtualBox" },
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
    -- { rule = { class = "Epiphany" },
      -- properties = { floating = true } },
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
    { rule = { class = "Nautilus" },
      properties = { floating = true } },
    { rule = { class = "Zathura" },
      properties = { floating = true } },
    { rule = { class = "Zenmap" },
      properties = { floating = true } },
    { rule = { class = "Multixterm" },
      properties = { floating = true } },
    { rule = { class = "Wine" },
      properties = { floating = true } },
    { rule = { class = "Mysql-workbench-bin" },
      properties = { floating = true } },
    { rule = { class = "Calibre" },
      properties = { floating = true } },
    { rule = { class = "sun-awt-X11-XFramePeer" },
      properties = { floating = true } },
    { rule = { class = "sun-tools-jconsole-JConsole" },
      properties = { floating = true } },
    { rule = { class = "lbe-ui-BrowserApp" },
      properties = { floating = true } }
}

awful.rules.rules = awful.util.table.join(
   awful.rules.rules
   -- vain.layout.gimp.rules
                                         )
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)

   -- Enable sloppy focus
   c:add_signal("mouse::enter", function(c)
   if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
                client.focus = c
            end
   end)

    if not startup then
        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
