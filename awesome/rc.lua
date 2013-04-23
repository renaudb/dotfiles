--------------------------------------------------------------------------------
-- INCLUDES
--------------------------------------------------------------------------------
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")

-- Widget library
local vicious = require("vicious")

--------------------------------------------------------------------------------
-- ERROR HANDLING
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- WM
--------------------------------------------------------------------------------
-- Start composite manager
--awful.util.spawn_with_shell("compton -cCGb -o 0.38 -r 3.2 -m 0.88 -t 0.02 -l 0.02")

--------------------------------------------------------------------------------
-- VARIABLES
--------------------------------------------------------------------------------
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/themes/magulodon/theme2.lua")

-- This is used later as defaults
terminal = "gnome-terminal"

-- Default modkey
modkey = "Mod4"

--------------------------------------------------------------------------------
-- LAYOUTS
--------------------------------------------------------------------------------
local layouts =
   {
   awful.layout.suit.tile,
   awful.layout.suit.fair,
   awful.layout.suit.floating,
   awful.layout.suit.max.fullscreen,
   }

--------------------------------------------------------------------------------
-- TAGS
--------------------------------------------------------------------------------
tags = {}
for s = 1, screen.count() do
    tags[s] = awful.tag({ "⠐", "⠡", "⠲", "⠵", "⠻", "⠿" }, s, layouts[1])
end

--------------------------------------------------------------------------------
-- MENU
--------------------------------------------------------------------------------
mylauncher = awful.widget.launcher({
   image = beautiful.awesome_icon,
   menu  = awful.menu({
      items = {
         { "Restart",  awesome.restart },
	 { "Quit",     awesome.quit },
	 { "Reboot",   "sudo shutdown -r now" },
	 { "Shutdown", "sudo shutdown -h now" }
      }
   })
})

--------------------------------------------------------------------------------
-- MOUSE BINDINGS
--------------------------------------------------------------------------------
root.buttons(awful.util.table.join(
    awful.button({ }, 3, nil),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))

--------------------------------------------------------------------------------
-- GLOBAL KEY BINDINGS
--------------------------------------------------------------------------------
globalkeys = awful.util.table.join(

   -- Tag switching
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

   -- Application switching
   awful.key({ modkey,           }, "Tab",
	     function ()
		awful.client.focus.history.previous()
		if client.focus then
		   client.focus:raise()
		end
	     end),
   awful.key({ modkey, "Shift"   }, "Tab", function () awful.client.swap.byidx(1) end),

   -- Layout changes
   awful.key({ modkey,           }, "]",     function () awful.tag.incmwfact( 0.05)    end),
   awful.key({ modkey,           }, "[",     function () awful.tag.incmwfact(-0.05)    end),
   awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
   awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
   awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
   awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
   awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

   awful.key({ modkey, "Control" }, "n", awful.client.restore),

   -- Run prompt
   awful.key({ modkey            }, "r", function () mypromptbox[mouse.screen]:run() end),

   -- Programs shortcuts
   awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),

   --Volume manipulation
   --awful.key({}, "XF86AudioRaiseVolume", function() awful.util.spawn("amixer -q set Master 1+")     end),
   --awful.key({}, "XF86AudioLowerVolume", function() awful.util.spawn("amixer -q set Master 1-")     end),
   --awful.key({}, "XF86AudioMute",        function() awful.util.spawn("amixer -q set Master toggle") end),

   --Lock/suspend/quit
   awful.key({ modkey, "Shift"   }, "q", awesome.quit),
   awful.key({ modkey, "Shift"   }, "l", function() awful.util.spawn("gnome-screensaver-command --lock") end),
   awful.key({ modkey, "Shift"   }, "s", function() awful.util.spawn("sudo pm-suspend") end)
)

--------------------------------------------------------------------------------
-- CLIENT KEY BINDINGS
--------------------------------------------------------------------------------
clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",      function (c) c.minimized = true               end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

--------------------------------------------------------------------------------
-- TAG KEY BINDINGS
--------------------------------------------------------------------------------
-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber))
end

-- Bind all key numbers to tags.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
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

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     keys = clientkeys,
                     buttons = clientbuttons,
		     size_hints_honor = false } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    { rule = { class = "Steam" },
      properties = { floating = true } },
    { rule = { class = "Chromium" },
      properties = { tag = tags[1][2] } },
    { rule = { class = "Emacs", instance = "emacs" },
      properties = {tag = tags[1][3]} },
}
-- }}}

-- {{{ Signals
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
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- Spacing widget
spacewidget = wibox.widget.textbox()
spacewidget:set_text("")
spacerwidget = wibox.widget.textbox()
spacerwidget:set_text(" ")

-- CPU usage widget
cpuicon = wibox.widget.imagebox()
cpuicon:set_image(beautiful.widget_cpu)
cpuwidget = wibox.widget.textbox()
vicious.register(cpuwidget, vicious.widgets.cpu, "$1%", 2)

-- Memory usage widget
memicon = wibox.widget.imagebox()
memicon:set_image(beautiful.widget_mem)
memwidget = wibox.widget.textbox()
vicious.register(memwidget, vicious.widgets.mem, "$1%", 2)

-- HD usage
hdicon = wibox.widget.imagebox()
hdicon:set_image(beautiful.widget_fs)
hdwidget = wibox.widget.textbox()
vicious.register(hdwidget, vicious.widgets.fs, "${/home used_p}%")

-- Battery widget
baticon = wibox.widget.imagebox()
baticon:set_image(beautiful.widget_batfull)
batwidget = wibox.widget.textbox()
vicious.register(batwidget, vicious.widgets.bat,
		 function(widget, args)
		    if args[1] == "-" then
		       baticon:set_image(beautiful.widget_batfull)
		    else
		       baticon:set_image(beautiful.widget_ac)
		    end
		    return string.format("%d%%", args[2])
		 end, 2, "BAT0")

-- Volume widget
volicon = wibox.widget.imagebox()
volicon:set_image(beautiful.widget_vol)
volwidget = wibox.widget.textbox()
vicious.register(volwidget, vicious.widgets.volume,
		 function(widget, args)
		    if args[2] == "♫" then
		       volicon:set_image(beautiful.widget_vol)
		    else
		       volicon:set_image(beautiful.widget_mute)
		    end
		    return string.format("%d%%", args[1])
		 end, 1, "Master")

-- Create a textclock widget
clockicon = wibox.widget.imagebox()
clockicon:set_image(beautiful.widget_clock)
clockwidget = wibox.widget.textbox()
vicious.register(clockwidget, vicious.widgets.date, "%a %b %d, %R")

-- Create a systray
mysystray = wibox.widget.systray()

-- Create a wibox for each screen and add it
mywibox = {}
mybwibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
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

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })

    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mylayoutbox[s])
    left_layout:add(mypromptbox[s])

    local right_layout = wibox.layout.fixed.horizontal()
    -- Insert systray.
    if s == 1 then right_layout:add(mysystray) end
    -- Insert cpu widget.
    right_layout:add(spacerwidget)
    right_layout:add(cpuicon)
    right_layout:add(spacewidget)
    right_layout:add(cpuwidget)
    -- Insert memory widget.
    right_layout:add(spacerwidget)
    right_layout:add(memicon)
    right_layout:add(spacewidget)
    right_layout:add(memwidget)
    -- Insert hard drive widget.
    right_layout:add(spacerwidget)
    right_layout:add(hdicon)
    right_layout:add(spacewidget)
    right_layout:add(hdwidget)
    -- Insert volume widget.
    right_layout:add(spacerwidget)
    right_layout:add(volicon)
    right_layout:add(spacewidget)
    right_layout:add(volwidget)
    -- Insert battery widget.
    right_layout:add(spacerwidget)
    right_layout:add(baticon)
    right_layout:add(spacewidget)
    right_layout:add(batwidget)
    -- Insert clock widget.
    right_layout:add(spacerwidget)
    right_layout:add(clockicon)
    right_layout:add(spacewidget)
    right_layout:add(clockwidget)

    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end

function run_once(prg, arg_string, pname, screen)
    if not prg then
        do return nil end
    end

    if not pname then
       pname = prg
    end

    if not arg_string then
        awful.util.spawn_with_shell("pgrep -f -u $USER -x '" .. pname .. "' || (" .. prg .. ")",screen)
    else
        awful.util.spawn_with_shell("pgrep -f -u $USER -x '" .. pname .. "' || (" .. prg .. " " .. arg_string .. ")",screen)
    end
end

-- Other startup programs
awful.util.spawn_with_shell("chromium")
