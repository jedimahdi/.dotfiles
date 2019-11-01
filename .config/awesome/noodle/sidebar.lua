local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")

local helpers = require("helpers")
local pad = helpers.pad

-- Some commonly used variables
local playerctl_button_size = dpi(48)
local icon_size = dpi(36)
local progress_bar_width = dpi(215)
-- local progress_bar_margins = dpi(9)

-- Helper function that changes the appearance of progress bars and their icons
-- Create horizontal rounded bars
local function format_progress_bar(bar, icon)
    icon.forced_height = icon_size
    icon.forced_width = icon_size
    icon.resize = true
    bar.forced_width = progress_bar_width
    bar.shape = gears.shape.rounded_bar
    bar.bar_shape = gears.shape.rounded_bar

    local w = wibox.widget{
        nil,
        {
            icon,
            bar,
            spacing = dpi(10),
            layout = wibox.layout.fixed.horizontal
        },
        expand = "none",
        layout = wibox.layout.align.horizontal
    }
    return w
end

-- Item configuration
local exit_icon = wibox.widget.imagebox(icons.poweroff)
exit_icon.resize = true
exit_icon.forced_width = icon_size
exit_icon.forced_height = icon_size
local exit_text = wibox.widget.textbox("Exit")
exit_text.font = "sans 14"

local exit = wibox.widget{
    exit_icon,
    exit_text,
    layout = wibox.layout.fixed.horizontal
}
exit:buttons(gears.table.join(
    awful.button({ }, 1, function ()
        exit_screen_show()
        sidebar.visible = false
    end)
))

-- Weather widget with image icons
-- local weather_widget = require("noodle.weather")
-- local weather_widget_icon = weather_widget:get_all_children()[1]
-- weather_widget_icon.forced_width = icon_size
-- weather_widget_icon.forced_height = icon_size
-- local weather_widget_text = weather_widget:get_all_children()[2]
-- weather_widget_text.font = "sans 14"

-- Weather widget with text icons
local weather_widget = require("noodle.text_weather")
local weather_widget_icon = weather_widget:get_all_children()[1]
weather_widget_icon.font = "Typicons 25"
local weather_widget_text = weather_widget:get_all_children()[2]
weather_widget_text.font = "sans 14"

local weather = wibox.widget{
    nil,
    weather_widget,
    nil,
    layout = wibox.layout.align.horizontal,
    expand = "none"
}


local temperature_icon = wibox.widget.imagebox(icons.temperature)
local temperature_bar = require("noodle.temperature_bar")
local temperature = format_progress_bar(temperature_bar, temperature_icon)
temperature:buttons(
    gears.table.join(
        awful.button({ }, 1, function ()
            helpers.run_or_raise({class = 'sensors'}, false, user.terminal.." --class sensors -e watch sensors")
        end)
))

local battery_icon = wibox.widget.imagebox(icons.battery)
awesome.connect_signal("evil::charger", function(plugged)
    if plugged then
        battery_icon.image = icons.battery_charging
    else
        battery_icon.image = icons.battery
    end
end)
local battery_bar = require("noodle.battery_bar")
local battery = format_progress_bar(battery_bar, battery_icon)

local cpu_icon = wibox.widget.imagebox(icons.cpu)
local cpu_bar = require("noodle.cpu_bar")
local cpu = format_progress_bar(cpu_bar, cpu_icon)

cpu:buttons(
    gears.table.join(
        awful.button({ }, 1, function ()
            helpers.run_or_raise({class = 'htop'}, false, user.terminal.." --class htop -e htop")
        end),
        awful.button({ }, 3, function ()
            helpers.run_or_raise({class = 'Lxtask'}, false, "lxtask")
        end)
))

local ram_icon = wibox.widget.imagebox(icons.ram)
local ram_bar = require("noodle.ram_bar")
local ram = format_progress_bar(ram_bar, ram_icon)

ram:buttons(
    gears.table.join(
        awful.button({ }, 1, function ()
            helpers.run_or_raise({class = 'htop'}, false, user.terminal.." --class htop -e htop")
        end),
        awful.button({ }, 3, function ()
            helpers.run_or_raise({class = 'Lxtask'}, false, "lxtask")
        end)
))


local playerctl_toggle_icon = wibox.widget.imagebox(icons.playerctl_toggle)
playerctl_toggle_icon.resize = true
playerctl_toggle_icon.forced_width = playerctl_button_size
playerctl_toggle_icon.forced_height = playerctl_button_size
playerctl_toggle_icon:buttons(gears.table.join(
    awful.button({ }, 1, function ()
        awful.spawn.with_shell("mpc -q toggle")
    end),
    awful.button({ }, 3, function ()
        awful.spawn.with_shell("mpvc toggle")
    end),
    awful.button({ }, 8, function ()
        sidebar.visible = false
        awful.spawn.with_shell("~/scr/Rofi/rofi_mpvtube")
    end),
    awful.button({ }, 9, function ()
        awful.spawn.with_shell("~/scr/info/mpv-query.sh")
    end)
))

local playerctl_prev_icon = wibox.widget.imagebox(icons.playerctl_prev)
playerctl_prev_icon.resize = true
playerctl_prev_icon.forced_width = playerctl_button_size
playerctl_prev_icon.forced_height = playerctl_button_size
playerctl_prev_icon:buttons(gears.table.join(
    awful.button({ }, 1, function ()
        awful.spawn.with_shell("mpc -q prev")
    end),
    awful.button({ }, 3, function ()
        awful.spawn.with_shell("mpvc prev")
    end)
))

local playerctl_next_icon = wibox.widget.imagebox(icons.playerctl_next)
playerctl_next_icon.resize = true
playerctl_next_icon.forced_width = playerctl_button_size
playerctl_next_icon.forced_height = playerctl_button_size
playerctl_next_icon:buttons(gears.table.join(
    awful.button({ }, 1, function ()
        awful.spawn.with_shell("mpc -q next")
    end),
    awful.button({ }, 3, function ()
        awful.spawn.with_shell("mpvc next")
    end)
))

local playerctl_buttons = wibox.widget {
    nil,
    {
        playerctl_prev_icon,
        playerctl_toggle_icon,
        playerctl_next_icon,
        spacing = dpi(10),
        layout  = wibox.layout.fixed.horizontal
    },
    expand = "none",
    layout = wibox.layout.align.horizontal,
}

local time = wibox.widget.textclock("%H %M")
time.align = "center"
time.valign = "center"
time.font = "sans 55"

local date = wibox.widget.textclock("%B %d")
-- local date = wibox.widget.textclock("%A, %B %d")
-- local date = wibox.widget.textclock("%A, %B %d, %Y")
date.align = "center"
date.valign = "center"
date.font = "sans medium 16"

-- local fancy_date = wibox.widget.textclock("%-j days around the sun")
local fancy_date = wibox.widget.textclock("Knowing that today is %A fills you with determination.")
fancy_date.align = "center"
fancy_date.valign = "center"
fancy_date.font = "sans italic 11"

local fancy_time_widget = wibox.widget.textclock("%H%M")
fancy_time_widget.markup = fancy_time_widget.text:sub(1,2) .. "<span foreground='" .. beautiful.xcolor12 .."'>" .. fancy_time_widget.text:sub(3,4) .. "</span>"
fancy_time_widget:connect_signal("widget::redraw_needed", function () 
    fancy_time_widget.markup = fancy_time_widget.text:sub(1,2) .. "<span foreground='" .. beautiful.xcolor12 .."'>" .. fancy_time_widget.text:sub(3,4) .. "</span>"
end)
fancy_time_widget.align = "center"
fancy_time_widget.valign = "center"
fancy_time_widget.font = "sans 55"
local fancy_time_decoration = wibox.widget.textbox()
-- local decoration_string = "------------------------"
local decoration_string = "──────  ──────"
fancy_time_decoration.markup = "<span foreground='" .. beautiful.xcolor12 .."'>"..decoration_string.."</span>"
fancy_time_decoration.font = "sans 18"
fancy_time_decoration.align = "center"
fancy_time_decoration.valign = "top"

local fancy_time = {
    fancy_time_widget,
    fancy_time_decoration,
    layout = wibox.layout.fixed.vertical,
}

local mpd_song = require("noodle.mpd_song")
local mpd_widget_children = mpd_song:get_all_children()
local mpd_title = mpd_widget_children[1]
local mpd_artist = mpd_widget_children[2]
mpd_title.font = "sans medium 14"
mpd_artist.font = "sans 11"

-- Set forced height in order to limit the widgets to one line.
-- Might need to be adjusted depending on the font.
mpd_title.forced_height = dpi(24)
mpd_artist.forced_height = dpi(18)

mpd_song:buttons(gears.table.join(
    awful.button({ }, 1, function ()
        awful.spawn.with_shell("mpc -q toggle")
    end),
    awful.button({ }, 3, function ()
        -- Spawn music terminal
        awful.spawn(user.music_client)
    end),
    awful.button({ }, 4, function ()
        awful.spawn.with_shell("mpc -q prev")
    end),
    awful.button({ }, 5, function ()
        awful.spawn.with_shell("mpc -q next")
    end)
))

local disk_space = require("noodle.disk")
disk_space.font = "sans 14"
local disk_icon = wibox.widget.imagebox(icons.files)
disk_icon.resize = true
disk_icon.forced_width = icon_size
disk_icon.forced_height = icon_size
local disk = wibox.widget{
    nil,
    {
        disk_icon,
        disk_space,
        layout = wibox.layout.fixed.horizontal
    },
    nil,
    expand = "none",
    layout = wibox.layout.align.horizontal
}

disk:buttons(gears.table.join(
    awful.button({ }, 1, function ()
        awful.spawn(user.file_manager, {floating = true})
    end),
    awful.button({ }, 3, function ()
        awful.spawn(user.file_manager .. " /data", {floating = true})
    end)
))

local search_icon = wibox.widget.imagebox(icons.search)
search_icon.resize = true
search_icon.forced_width = icon_size
search_icon.forced_height = icon_size
local search_text = wibox.widget.textbox("Search")
search_text.font = "sans 14"

local search = wibox.widget{
    search_icon,
    search_text,
    layout = wibox.layout.fixed.horizontal
}
search:buttons(gears.table.join(
    awful.button({ }, 1, function ()
        awful.spawn.with_shell("rofi -matching fuzzy -show combi")
        sidebar.visible = false
    end),
    awful.button({ }, 3, function ()
        awful.spawn.with_shell("rofi -matching fuzzy -show run")
        sidebar.visible = false
    end)
))

local volume_icon = wibox.widget.imagebox(icons.volume)
local volume_bar = require("noodle.volume_bar")
local volume = format_progress_bar(volume_bar, volume_icon)

volume:buttons(gears.table.join(
    -- Left click - Mute / Unmute
    awful.button({ }, 1, function ()
        helpers.volume_control(0)
    end),
    -- Right click - Run or raise pavucontrol
    awful.button({ }, 3, function ()
        helpers.run_or_raise({class = 'Pavucontrol'}, true, "pavucontrol")
    end),
    -- Scroll - Increase / Decrease volume
    awful.button({ }, 4, function ()
        helpers.volume_control(5)
    end),
    awful.button({ }, 5, function ()
        helpers.volume_control(-5)
    end)
))

-- Add clickable effects on some widgets
helpers.add_hover_cursor(cpu, "hand1")
helpers.add_hover_cursor(ram, "hand1")
helpers.add_hover_cursor(temperature, "hand1")
helpers.add_hover_cursor(volume, "hand1")

-- Create the sidebar
sidebar = wibox({visible = false, ontop = true, type = "dock"})
sidebar.bg = beautiful.sidebar_bg or beautiful.wibar_bg or "#111111"
sidebar.fg = beautiful.sidebar_fg or beautiful.wibar_fg or "#FFFFFF"
sidebar.opacity = beautiful.sidebar_opacity or 1
sidebar.height = beautiful.sidebar_height or awful.screen.focused().geometry.height
sidebar.width = beautiful.sidebar_width or dpi(300)
sidebar.y = beautiful.sidebar_y or 0
local radius = beautiful.sidebar_border_radius or 0
if beautiful.sidebar_position == "right" then
    awful.placement.right(sidebar)
    sidebar.shape = helpers.prrect(radius, true, false, false, true)
else
    awful.placement.left(sidebar)
    sidebar.shape = helpers.prrect(radius, false, true, true, false)
end
-- sidebar.shape = helpers.rrect(radius)

sidebar:buttons(gears.table.join(
    -- Middle click - Hide sidebar
    awful.button({ }, 2, function ()
        sidebar.visible = false
    end)
    -- Right click - Hide sidebar
    -- awful.button({ }, 3, function ()
    --     sidebar.visible = false
    --     -- mymainmenu:show()
    -- end)
))

-- Hide sidebar when mouse leaves
if user.sidebar_hide_on_mouse_leave then
    sidebar:connect_signal("mouse::leave", function ()
        sidebar.visible = false
    end)
end
-- Activate sidebar by moving the mouse at the edge of the screen
if user.sidebar_show_on_mouse_screen_edge then
    local sidebar_activator = wibox({y = sidebar.y, width = 1, visible = true, ontop = false, opacity = 0, below = true})
    sidebar_activator.height = sidebar.height
    sidebar_activator:connect_signal("mouse::enter", function ()
        sidebar.visible = true
    end)

    if beautiful.sidebar_position == "right" then
        awful.placement.right(sidebar_activator)
    else
        awful.placement.left(sidebar_activator)
    end

    sidebar_activator:buttons(
        gears.table.join(
            awful.button({ }, 4, function ()
                awful.tag.viewprev()
            end),
            awful.button({ }, 5, function ()
                awful.tag.viewnext()
            end)
    ))
end

-- Item placement
sidebar:setup {
    { ----------- TOP GROUP -----------
        pad(1),
        pad(1),
        -- fancy_time,
        time,
        date,
        pad(1),
        weather,
        pad(1),
        pad(1),
        layout = wibox.layout.fixed.vertical
    },
    { ----------- MIDDLE GROUP -----------
        playerctl_buttons,
        {
            -- Put some margins at the left and right edge so that
            -- it looks better with extremely long titles/artists
            mpd_song,
            left = dpi(10),
            right = dpi(10),
            widget = wibox.container.margin
        },
        pad(1),
        pad(1),
        volume,
        cpu,
        temperature,
        ram,
        battery,
        pad(1),
        disk,
        pad(1),
        pad(1),
        layout = wibox.layout.fixed.vertical
        -- layout = wibox.layout.fixed.vertical
    },
    { ----------- BOTTOM GROUP -----------
        nil,
        {
            {
                search,
                exit,
                spacing = dpi(50),
                layout = wibox.layout.fixed.horizontal
            },
            left = dpi(20),
            right = dpi(20),
            bottom = dpi(20),
            widget = wibox.container.margin
        },
        nil,
        layout = wibox.layout.align.horizontal,
        expand = "none"
    },
    layout = wibox.layout.align.vertical,
    -- expand = "none"
}
