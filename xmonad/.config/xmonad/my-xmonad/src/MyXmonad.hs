module MyXmonad
    ( main
    ) where

import           Control.Monad                   ( join, when )
import           Data.Map                        ( Map )
import qualified Data.Map                        as Map
import           Data.Maybe                      ( maybeToList )
import           Graphics.X11.ExtraTypes.XF86
import qualified MyXmonad.Dmenu.Configs          as Dmenu
import qualified MyXmonad.Prompt.Calculator      as Prompt
import qualified MyXmonad.Prompt.Hoogle          as Prompt
import           System.Exit                     ( exitSuccess )
import           XMonad
import           XMonad.Actions.CycleWS          ( toggleWS )
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.Fullscreen
    ( fullscreenEventHook, fullscreenFull, fullscreenManageHook, fullscreenSupport )
import           XMonad.Layout.Gaps
    ( Direction2D (D, L, R, U), GapMessage (DecGap, IncGap, ToggleGaps), gaps, setGaps )
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet                 as W
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.Loggers
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.SpawnOnce
import MyXmonad.Layout.CenterMainFluid
import XMonad.Layout.ThreeColumns

myTerminal :: String
myTerminal = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth   = 2

myWorkspaces :: [WorkspaceId]
myWorkspaces =  ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myNormalBorderColor :: String
-- myNormalBorderColor  = "#3b4252"
myNormalBorderColor = "#2d3436"

myFocusedBorderColor :: String
-- myFocusedBorderColor = "#bc96da"
myFocusedBorderColor  = "#3b4252"

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

-- safeSpawn' :: MonadIO m => FilePath -> String -> m ()
-- safeSpawn' p = safeSpawn p . words
--
-- runOrRaise :: String -> [String] -> Query Bool -> X ()
-- runOrRaise = (raiseMaybe .) . safeSpawn

clipboardy, centerlaunch, sidebarlaunch, ewwclose, maimcopy, maimsave, rofiLauncher :: MonadIO m => m ()
clipboardy = spawn "rofi -modi \"\63053 :greenclip print\" -show \"\63053 \" -run-command '{cmd}' -theme ~/.config/rofi/launcher/style.rasi"
centerlaunch = spawn "exec ~/.dotfiles/bin/eww open-many blur_full weather profile quote search_full incognito-icon vpn-icon home_dir screenshot power_full reboot_full lock_full logout_full suspend_full"
sidebarlaunch = spawn "exec ~/.dotfiles/bin/eww open-many weather_side time_side smol_calendar player_side sys_side sliders_side"
ewwclose = spawn "exec ~/.dotfiles/bin/eww close-all"
maimcopy = spawn "maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"
maimsave = spawn "maim -s --hidecursor ~/Desktop/$(date +%Y-%m-%d_%H-%M-%S).png && notify-send \"Screenshot\" \"Saved to Desktop\" -i flameshot"
rofiLauncher = spawn "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\" "

myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.modMask = myModMask } = Map.fromList $

     -- launch a terminal
     [ ((myModMask, xK_Return)              , spawn $ XMonad.terminal conf)
     , ((myModMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf ++ " --working-directory  \"`xcwd`\"")

     -- lock screen
     , ((myModMask,               xK_F1    ), spawn "betterlockscreen -l")

     -- launch rofi and dashboard
     , ((myModMask,               xK_o     ), rofiLauncher)
     , ((myModMask,               xK_p     ), centerlaunch)
     , ((myModMask .|. shiftMask, xK_p     ), ewwclose)

    -- launch eww sidebar
    , ((myModMask,               xK_s     ), sidebarlaunch)
    , ((myModMask .|. shiftMask, xK_s     ), ewwclose)

     -- Audio keys
     , ((0, xF86XK_AudioPlay)               , spawn "playerctl play-pause")
     , ((0, xF86XK_AudioPrev)               , spawn "playerctl previous")
     , ((0, xF86XK_AudioNext)               , spawn "playerctl next")
     , ((0, xF86XK_AudioStop)               , spawn "playerctl stop")
     , ((0, xF86XK_AudioRaiseVolume)        , spawn "pactl set-sink-volume 0 +5%")
     , ((0, xF86XK_AudioLowerVolume)        , spawn "pactl set-sink-volume 0 -5%")
     , ((0, xF86XK_AudioMute)               , spawn "pactl set-sink-mute 0 toggle")

     -- Brightness keys
     , ((0, xF86XK_MonBrightnessUp)         , spawn "brightnessctl s +10%")
     , ((0, xF86XK_MonBrightnessDown)       , spawn "brightnessctl s 10-%")

     -- Screenshot
     , ((0,         xK_Print)              , maimsave)
     , ((myModMask, xK_Print)              , maimcopy)

     -- My Stuff
     , ((myModMask,               xK_b     ), spawn "exec ~/.dotfiles/bin/bartoggle")
     -- , ((myModMask,               xK_z     ), spawn "exec ~/.dotfiles/bin/inhibit_activate")
     -- , ((myModMask .|. shiftMask, xK_z     ), spawn "exec ~/.dotfiles/bin/inhibit_deactivate")
     , ((myModMask .|. shiftMask, xK_a     ), clipboardy)

     -- close focused window
     , ((myModMask, xK_q)                   , kill)

     -- GAPS!!!
     -- , ((myModMask, xK_f), sendMessage ToggleGaps)               -- toggle all gaps
     , ((myModMask .|. shiftMask, xK_g), sendMessage $ setGaps [(L,15), (R,15), (U,60), (D,15)]) -- reset the GapSpec

     , ((myModMask .|. controlMask, xK_t), sendMessage $ IncGap 10 L)              -- increment the left-hand gap
     , ((myModMask .|. shiftMask, xK_t     ), sendMessage $ DecGap 10 L)           -- decrement the left-hand gap

     , ((myModMask .|. controlMask, xK_y), sendMessage $ IncGap 10 U)              -- increment the top gap
     , ((myModMask .|. shiftMask, xK_y     ), sendMessage $ DecGap 10 U)           -- decrement the top gap

     , ((myModMask .|. controlMask, xK_u), sendMessage $ IncGap 10 D)              -- increment the bottom gap
     , ((myModMask .|. shiftMask, xK_u     ), sendMessage $ DecGap 10 D)           -- decrement the bottom gap

     , ((myModMask .|. controlMask, xK_i), sendMessage $ IncGap 10 R)              -- increment the right-hand gap
     , ((myModMask .|. shiftMask, xK_i     ), sendMessage $ DecGap 10 R)           -- decrement the right-hand gap

     -- Rotate through the available layout algorithms
     , ((myModMask, xK_space)               , sendMessage NextLayout)

     --  Reset the layouts on the current workspace to default
     , ((myModMask .|. shiftMask, xK_space) , setLayout $ XMonad.layoutHook conf)

     -- Resize viewed windows to the correct size
     , ((myModMask,               xK_n     ), refresh)

     -- Move focus to the next window
     , ((myModMask, xK_j)                   , windows W.focusDown)

     -- Move focus to the previous window
     , ((myModMask, xK_k)                   , windows W.focusUp)

     -- Move focus to the master window
     , ((myModMask, xK_m)                   , windows W.focusMaster)

     -- Swap the focused window and the master window
     , ((myModMask .|. shiftMask, xK_m)     , windows W.swapMaster)

     -- Swap the focused window with the next window
     , ((myModMask .|. shiftMask, xK_j)     , windows W.swapDown)

     -- Swap the focused window with the previous window
     , ((myModMask .|. shiftMask, xK_k)     , windows W.swapUp)

     -- Shrink the master area
     , ((myModMask, xK_h)                   , sendMessage Shrink)

     -- Expand the master area
     , ((myModMask, xK_l)                   , sendMessage Expand)

     -- Push window back into tiling
     , ((myModMask, xK_t)                   , withFocused $ windows . W.sink)

     -- Increment the number of windows in the master area
     , ((myModMask, xK_comma)               , sendMessage (IncMasterN 1))

     -- Deincrement the number of windows in the master area
     , ((myModMask, xK_period)              , sendMessage (IncMasterN (-1)))

     -- Quit xmonad
     -- , ((myModMask .|. shiftMask, xK_q)     , spawn "~/.dotfiles/bin/powermenu.sh")

     -- Restart xmonad
     , ((myModMask .|. shiftMask , xK_r)    , spawn "xmonad --recompile && xmonad --restart")

     , ((myModMask, xK_d)                   , spawn "dmenu_run")
     -- , ((myModMask, xK_b)                   , spawn "feh --bg-fill --randomize ~/Picture/Wallpaper/*")

     -- , ((myModMask, xK_z)                   , spawn "pcmanfm")
     -- , ((myModMask, xK_c)                   , Dmenu.configs)
     -- , ((myModMask, xK_x)                   , spawn "~/.dotfiles/bin/dmkill")
     , ((myModMask, xK_g)                   , Prompt.hoogle)
     , ((myModMask .|. shiftMask, xK_x)     , Prompt.calculator)
     , ((myModMask .|. shiftMask, xK_q)     , liftIO exitSuccess)
     , ((myModMask, xK_Tab)                 , toggleWS)
     , ((myModMask, xK_f)                   , sendMessage ToggleStruts)
     ]

     -- mod-[1..9], Switch to workspace N

  ++ [ ((myModMask, k), windows $ W.greedyView i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ]

     -- mod-shift-[1..9], Move client to workspace N

  ++ [ ((myModMask .|. shiftMask, k), windows $ W.shift i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ]

myMouseBindings XConfig {XMonad.modMask = modm} = Map.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| CenterMainFluid 1 (3/100) (80/100) ||| ThreeColMid 1 (3/100) (1/2) ||| gaps [(U,40), (R,20), (L, 20), (D, 20)] (Tall 1 (3/100) (1/2))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myManageHook :: ManageHook
myManageHook = fullscreenManageHook <+> manageDocks <+> mconcat
  [ className =? "Gimp" --> doFloat
  , className =? "mpv" --> doFloat
  , className =? "firefox" --> doShift (myWorkspaces !! 2)
  , className =? "Uget-gtk" --> doShift (myWorkspaces !! 5)
  , className =? "Pcmanfm" --> doShift (myWorkspaces !! 3)
  , className =? "TelegramDesktop" --> doShift (myWorkspaces !! 4)
  , (title =? "Spotify Premium" <||> title =? "Spotify") --> doShift (myWorkspaces !! 7)
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  ]

myEventHook = mempty

myLogHook = return ()

myStartupHook = do
  -- spawnOnce "exec ~/.dotfiles/bin/bartoggle"
  -- spawnOnce "exec ~/.dotfiles/bin/eww daemon"
  spawn "xsetroot -cursor_name left_ptr"
  -- spawn "exec ~/.dotfiles/bin/lock.sh"
  -- spawnOnce "feh --bg-fill ~/.dotfiles/utils/wallpapers/mountains.jpg"
  spawnOnce "feh --bg-fill ~/Picture/Wallpaper/astronaut-in-the-jungle-using-ai-1920×1080.jpg"
  -- spawnOnce "picom --experimental-backends"
  spawnOnce "greenclip daemon"
  spawnOnce "dunst"
  -- spawnOnce "~/.dotfiles/bin/systray.sh"

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = mod4Mask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        manageHook = myManageHook,
        -- layoutHook = smartGaps . spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True . smartBorders $ myLayout,
        layoutHook = smartGaps . smartBorders $ myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook >> addEWMHFullscreen
    }

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh  $ defaults

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myXmobarPP :: X PP
myXmobarPP = clickablePP $ def
      { ppCurrent         = xmobarColor "#71abeb" ""
      , ppVisible         = xmobarColor "#5AB1BB" ""
      , ppHidden          = xmobarColor "#e5c07b" ""
      , ppHiddenNoWindows = xmobarColor "#d6d5d5" ""
      , ppUrgent          = xmobarColor "#e06c75" "" . wrap "!" "!"
      , ppTitle           = xmobarColor "#9ec07c" "" . shorten 90
      , ppWsSep           = "  "
      , ppLayout          = xmobarColor "#c678dd" ""
      , ppSep             = "<fc=#4b5363> <fn=1>|</fn> </fc>"
      , ppOrder           = \(ws : l : t : extras) -> [ws,l]++extras++[t]
      , ppExtras          = [ xmobarColorL "#5AB1BB" "#1A1B26" windowCount]
      }
--
-- ----------------------
-- -- XMOBAR INSTANCES --
-- ----------------------
xmobar0 :: StatusBarConfig
xmobar0 = statusBarProp "xmobar" myXmobarPP

--
-- _config = def
--     { terminal           = _terminal
--     , modMask            = mod4Mask
--     , keys               = _keys
--     , layoutHook         = _layout
--     , startupHook        = myStartupHook
--     , workspaces         = _workspaces
--     , normalBorderColor  = _normalBorderColor
--     , focusedBorderColor = _focusedBorderColor
--     , borderWidth        = 2
--     , manageHook         = _manageHook <+> manageHook def
--     }
--
-- main :: IO ()
-- main = xmonad . docks . withSB xmobar0 . ewmh $ _config

uniformBorder :: Integer -> Border
uniformBorder i = Border i i i i

smartGaps = spacingRaw True (uniformBorder 0) False (uniformBorder 5) True

--
-- _layout = avoidStruts $ (smartGaps . smartBorders $ tiled) ||| noBorders Full ||| Grid
--  where
--   tiled   = Tall nmaster delta ratio
--   nmaster = 1
--   ratio   = 1 / 2
--   delta   = 3 / 100
