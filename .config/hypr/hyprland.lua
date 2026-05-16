hl.monitor({
	output = "",
	mode = "preferred",
	position = "auto",
	scale = "1",
})

hl.config({
	input = {
		kb_layout = "us,ir",
		kb_variant = "",
		kb_model = "",
		kb_options = "caps:escape,grp:alt_shift_toggle",
		kb_rules = "",
		repeat_rate = 35,
		repeat_delay = 200,
		follow_mouse = 1,
	},
	general = {
		gaps_in = 2,
		gaps_out = 0,
		border_size = 0,
		resize_on_border = false,
		allow_tearing = false,
		layout = "master",
	},

	decoration = {
		rounding = 0,
		active_opacity = 1.0,
		inactive_opacity = 1.0,
		shadow = {
			enabled = false,
		},

		blur = {
			enabled = false,
			size = 5,
			passes = 1,
			vibrancy = 0.1696,
		},
	},

	animations = {
		enabled = false,
	},

	misc = {
		force_default_wallpaper = 0,
		disable_hyprland_logo = true,
		disable_splash_rendering = true,
	},

	master = {
		new_status = "master",
	},

	ecosystem = {
		no_update_news = true,
		no_donation_nag = true,
	},
})

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")

local terminal = "footclient"
local fileManager = "pcmanfm"
local menu = "fuzzel"

local mainMod = "SUPER"

hl.bind(mainMod .. " + Return", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + Q", hl.dsp.window.close())
hl.bind(
	mainMod .. " + M",
	hl.dsp.exec_cmd("command -v hyprshutdown >/dev/null 2>&1 && hyprshutdown || hyprctl dispatch 'hl.dsp.exit()'")
)
hl.bind(mainMod .. " + E", hl.dsp.exec_cmd(fileManager))
hl.bind(mainMod .. " + T", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + D", hl.dsp.exec_cmd(menu))

hl.bind(mainMod .. " + S", hl.dsp.exec_cmd("wayland-screenshot"))
hl.bind(mainMod .. " + SHIFT + S", hl.dsp.exec_cmd("wayland-screenshot-edit"))
hl.bind(mainMod .. " + R", hl.dsp.exec_cmd("wayland-screenrec"))
hl.bind(mainMod .. " + SHIFT + R", hl.dsp.exec_cmd("wayland-screenrec-stop"))

hl.bind(
	mainMod .. " + C",
	hl.dsp.exec_cmd(
		'fzfmenu sh -c \'selected=$(cliphist list | fzf --height 100% --prompt="Clipboard> ") ; [ -n "$selected" ] && echo "$selected" | cliphist decode | wl-copy\''
	)
)
hl.bind(mainMod .. " + P", hl.dsp.exec_cmd("hyprpicker -n -a"))

hl.bind(mainMod .. " + Z", hl.dsp.exec_cmd("hyprctl keyword cursor:zoom_factor 3"))
hl.bind(mainMod .. " + SHIFT + Z", hl.dsp.exec_cmd("hyprctl keyword cursor:zoom_factor 1"))

hl.bind(mainMod .. " + J", hl.dsp.focus({ direction = "right" }))
hl.bind(mainMod .. " + K", hl.dsp.focus({ direction = "down" }))

hl.bind(mainMod .. " + TAB", hl.dsp.focus({ workspace = "previous" }))
for i = 1, 10 do
	local key = i % 10
	hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ workspace = i }))
	hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

hl.bind(
	"XF86AudioRaiseVolume",
	hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"),
	{ locked = true, repeating = true }
)
hl.bind(
	"XF86AudioLowerVolume",
	hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),
	{ locked = true, repeating = true }
)
hl.bind(
	"XF86AudioMute",
	hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),
	{ locked = true, repeating = true }
)
hl.bind(
	"XF86AudioMicMute",
	hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"),
	{ locked = true, repeating = true }
)
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl -e4 -n2 set 1%+"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl -e4 -n2 set 1%-"), { locked = true, repeating = true })

hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"), { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"), { locked = true })

hl.workspace_rule({ workspace = "3", gaps_out = -3 })

hl.window_rule({ match = { class = "footclient" }, tag = "+term" })
hl.window_rule({ match = { class = "foot" }, tag = "+term" })
hl.window_rule({ match = { class = "Alacritty" }, tag = "+term" })
hl.window_rule({ match = { class = "kitty" }, tag = "+term" })

hl.window_rule({ match = { tag = "term" }, opacity = "0.9" })

hl.window_rule({
	name = "fzfmenu",
	match = { class = "fzfmenu" },
	float = true,
	stay_focused = true,
	center = true,
	pin = true,
	opacity = "0.9",
	dim_around = true,
	size = "(monitor_w*0.7) (monitor_h*0.7)",
})
