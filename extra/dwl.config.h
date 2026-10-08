/* Taken from https://github.com/djpohly/dwl/issues/466 */
#define COLOR(hex)    { ((hex >> 24) & 0xFF) / 255.0f, \
                        ((hex >> 16) & 0xFF) / 255.0f, \
                        ((hex >> 8) & 0xFF) / 255.0f, \
                        (hex & 0xFF) / 255.0f }

// static const int hide_cursor_when_typing = 1;
static const int sloppyfocus               = 0;  /* focus follows mouse */
static const int bypass_surface_visibility = 0;  /* 1 means idle inhibitors will disable idle tracking even if it's surface isn't visible  */
static const unsigned int borderpx         = 0;  /* border pixel of windows */
static const unsigned int snap             = 32; /* snap pixel */
static const float rootcolor[]             = COLOR(0x222222ff);
static const float bordercolor[]           = COLOR(0x444444ff);
static const float focuscolor[]            = COLOR(0x005577ff);
static const float urgentcolor[]           = COLOR(0xff0000ff);
/* This conforms to the xdg-protocol. Set the alpha to zero to restore the old behavior */
static const float fullscreen_bg[]         = {0.0f, 0.0f, 0.0f, 1.0f}; /* You can also use glsl colors */

/* tagging - TAGCOUNT must be no greater than 31 */
#define TAGCOUNT (9)

/* logging */
static int log_level = WLR_ERROR;

static const Rule rules[] = {
  /* app_id             title       tags mask     isfloating   monitor */
  { "Gimp_EXAMPLE",     NULL,       0,            1,           -1 }, /* Start on currently visible tags floating, not tiled */
  { "firefox_EXAMPLE",  NULL,       1 << 8,       0,           -1 }, /* Start on ONLY tag "9" */
    /* default/example rule: can be changed but cannot be eliminated; at least one rule must exist */
};

/* layout(s) */
static const Layout layouts[] = {
  /* symbol     arrange function */
  { "[]=",      tile },
};

/* monitors */
/* (x=-1, y=-1) is reserved as an "autoconfigure" monitor position indicator
 * WARNING: negative values other than (-1, -1) cause problems with Xwayland clients due to
 * https://gitlab.freedesktop.org/xorg/xserver/-/issues/899 */
static const MonitorRule monrules[] = {
   /* name        mfact  nmaster scale layout       rotate/reflect                x    y
    * example of a HiDPI laptop monitor:
    { "eDP-1",    0.5f,  1,      2,    &layouts[0], WL_OUTPUT_TRANSFORM_NORMAL,   -1,  -1 }, */
  { NULL,       0.55f, 1,      1,    &layouts[0], WL_OUTPUT_TRANSFORM_NORMAL,   -1,  -1 },
  /* default monitor rule: can be changed but cannot be eliminated; at least one monitor rule must exist */
};

static const struct xkb_rule_names xkb_rules = {
  /* can specify fields: rules, model, layout, variant, options */
  .layout = "us,ir",
  .options = "caps:escape,grp:alt_shift_toggle",
};

static const int repeat_rate = 35;
static const int repeat_delay = 200;

/* Trackpad */
static const int tap_to_click = 1;
static const int tap_and_drag = 1;
static const int drag_lock = 1;
static const int natural_scrolling = 0;
static const int disable_while_typing = 1;
static const int left_handed = 0;
static const int middle_button_emulation = 0;
/* You can choose between:
LIBINPUT_CONFIG_SCROLL_NO_SCROLL
LIBINPUT_CONFIG_SCROLL_2FG
LIBINPUT_CONFIG_SCROLL_EDGE
LIBINPUT_CONFIG_SCROLL_ON_BUTTON_DOWN
*/
static const enum libinput_config_scroll_method scroll_method = LIBINPUT_CONFIG_SCROLL_2FG;

/* You can choose between:
LIBINPUT_CONFIG_CLICK_METHOD_NONE
LIBINPUT_CONFIG_CLICK_METHOD_BUTTON_AREAS
LIBINPUT_CONFIG_CLICK_METHOD_CLICKFINGER
*/
static const enum libinput_config_click_method click_method = LIBINPUT_CONFIG_CLICK_METHOD_BUTTON_AREAS;

/* You can choose between:
LIBINPUT_CONFIG_SEND_EVENTS_ENABLED
LIBINPUT_CONFIG_SEND_EVENTS_DISABLED
LIBINPUT_CONFIG_SEND_EVENTS_DISABLED_ON_EXTERNAL_MOUSE
*/
static const uint32_t send_events_mode = LIBINPUT_CONFIG_SEND_EVENTS_DISABLED_ON_EXTERNAL_MOUSE;

/* You can choose between:
LIBINPUT_CONFIG_ACCEL_PROFILE_FLAT
LIBINPUT_CONFIG_ACCEL_PROFILE_ADAPTIVE
*/
static const enum libinput_config_accel_profile accel_profile = LIBINPUT_CONFIG_ACCEL_PROFILE_ADAPTIVE;
static const double accel_speed = 0.0;

/* You can choose between:
LIBINPUT_CONFIG_TAP_MAP_LRM -- 1/2/3 finger tap maps to left/right/middle
LIBINPUT_CONFIG_TAP_MAP_LMR -- 1/2/3 finger tap maps to left/middle/right
*/
static const enum libinput_config_tap_button_map button_map = LIBINPUT_CONFIG_TAP_MAP_LRM;

/* If you want to use the windows key for MODKEY, use WLR_MODIFIER_LOGO */
#define MODKEY WLR_MODIFIER_LOGO

#define TAGKEYS(KEY,TAG) \
  { MODKEY,                    KEY,            view,            {.ui = 1 << TAG} }, \
  { MODKEY|WLR_MODIFIER_CTRL,  KEY,            toggleview,      {.ui = 1 << TAG} }, \
  { MODKEY|WLR_MODIFIER_SHIFT, KEY,            tag,             {.ui = 1 << TAG} }, \
  { MODKEY|WLR_MODIFIER_CTRL|WLR_MODIFIER_SHIFT,KEY,toggletag,  {.ui = 1 << TAG} }

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

static const char *termcmd[] = { "footclient", NULL };
static const char *menucmd[] = { "wmenu-run", "-f", "monospace 16", NULL };

static const char *volup[]   = { "wpctl", "set-volume", "-l", "1", "@DEFAULT_AUDIO_SINK@", "5%+", NULL };
static const char *voldown[] = { "wpctl", "set-volume", "@DEFAULT_AUDIO_SINK@", "5%-", NULL };
static const char *mute[]    = { "wpctl", "set-mute", "@DEFAULT_AUDIO_SINK@", "toggle", NULL };
static const char *mutemic[] = { "wpctl", "set-mute", "@DEFAULT_AUDIO_SOURCE@", "toggle", NULL };

static const char *musicnext[]  = { "playerctl", "next", NULL };
static const char *musicprev[]  = { "playerctl", "previous", NULL };
static const char *musicpause[] = { "playerctl", "play-pause", NULL };
static const char *musicstop[]  = { "playerctl", "play-pause", NULL };

static const char *brightnessup[]   = { "brightnessctl", "-e4" "-n2", "set", "1%+", NULL };
static const char *brightnessdown[] = { "brightnessctl", "-e4" "-n2", "set", "1%-", NULL };

static const Key keys[] = {
  { MODKEY, XKB_KEY_d,           spawn,            {.v = menucmd} },
  { MODKEY, XKB_KEY_Return,      spawn,            {.v = termcmd} },
  { MODKEY, XKB_KEY_c,           spawn,            SHCMD("cliphist list | wmenu -f 'monospace 16' -l 6 | cliphist decode | wl-copy") },
  { MODKEY, XKB_KEY_j,           focusstack,       {.i = +1} },
  { MODKEY, XKB_KEY_k,           focusstack,       {.i = -1} },
  { MODKEY, XKB_KEY_h,           setmfact,         {.f = -0.05f} },
  { MODKEY, XKB_KEY_l,           setmfact,         {.f = +0.05f} },
  { MODKEY, XKB_KEY_z,           zoom,             {0} },
  { MODKEY, XKB_KEY_Tab,         view,             {0} },
  { MODKEY, XKB_KEY_q,           killclient,       {0} },
  { MODKEY, XKB_KEY_t,           togglefloating,   {0} },
  { MODKEY, XKB_KEY_f,           togglefullscreen, {0} },
  TAGKEYS(  XKB_KEY_1,           0),
  TAGKEYS(  XKB_KEY_2,           1),
  TAGKEYS(  XKB_KEY_3,           2),
  TAGKEYS(  XKB_KEY_4,           3),
  TAGKEYS(  XKB_KEY_5,           4),
  TAGKEYS(  XKB_KEY_6,           5),
  TAGKEYS(  XKB_KEY_7,           6),
  TAGKEYS(  XKB_KEY_8,           7),
  TAGKEYS(  XKB_KEY_9,           8),
  { MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_q,   quit,             {0} },
  { 0,XKB_KEY_XF86AudioRaiseVolume,  spawn, {.v = volup} },
  { 0,XKB_KEY_XF86AudioLowerVolume,  spawn, {.v = voldown} },
  { 0,XKB_KEY_XF86AudioMute,         spawn, {.v = mute} },
  { 0,XKB_KEY_XF86AudioMicMute,      spawn, {.v = mutemic} },
  { 0,XKB_KEY_XF86AudioNext,         spawn, {.v = musicnext} },
  { 0,XKB_KEY_XF86AudioPrev,         spawn, {.v = musicprev} },
  { 0,XKB_KEY_XF86AudioPlay,         spawn, {.v = musicpause} },
  { 0,XKB_KEY_XF86AudioStop,         spawn, {.v = musicstop} },
  { 0,XKB_KEY_XF86MonBrightnessUp,   spawn, {.v = brightnessup} },
  { 0,XKB_KEY_XF86MonBrightnessDown, spawn, {.v = brightnessdown} },

  /* Ctrl-Alt-Backspace and Ctrl-Alt-Fx used to be handled by X server */
  { WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT,XKB_KEY_BackSpace, quit, {0} },
  /* Ctrl-Alt-Fx is used to switch to another VT, if you don't know what a VT is
   * do not remove them.
   */
#define CHVT(n) { WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT,XKB_KEY_F##n, chvt, {.ui = (n)} }
  CHVT(1), CHVT(2), CHVT(3), CHVT(4), CHVT(5), CHVT(6),
  CHVT(7), CHVT(8), CHVT(9), CHVT(10), CHVT(11), CHVT(12),
};

static const Button buttons[] = {
  { MODKEY, BTN_LEFT,   moveresize,     {.ui = CurMove} },
  { MODKEY, BTN_MIDDLE, togglefloating, {0} },
  { MODKEY, BTN_RIGHT,  moveresize,     {.ui = CurResize} },
};

static const Axis axes[] = {
  /* example of volume control:
  { MODKEY, AxisUp,   spawn, SHCMD("volume-up_EXAMPLE") },
  { MODKEY, AxisDown, spawn, SHCMD("volume-down_EXAMPLE") }, */
  { 0, 0, NULL, {0} },
  /* does nothing, but the array cannot be empty */
};
