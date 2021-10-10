module MyXmonad (main) where

import           XMonad
import           Data.Map                        ( Map )
import qualified Data.Map                        as Map
import qualified XMonad.StackSet                 as W
import           XMonad.Actions.CycleWS          ( toggleWS )
import           System.Exit                     ( exitSuccess )
import           XMonad.Hooks.ManageDocks
import           Graphics.X11.ExtraTypes.XF86
import           XMonad.Layout.Spacing
import           XMonad.Layout.NoBorders
import           XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Loggers
import qualified MyXmonad.Prompt.Hoogle as Prompt
import qualified MyXmonad.Prompt.Calculator as Prompt
import qualified MyXmonad.Dmenu.Configs as Dmenu
import           XMonad.Util.SpawnOnce


_terminal :: String
_terminal = "alacritty"

_workspaces :: [WorkspaceId]
_workspaces =  ["1", "2", "3", "4", "5", "6", "7", "8", "9"]


_keys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
_keys conf@XConfig { XMonad.modMask = myModMask } =
  Map.fromList
    $  [ ((myModMask, xK_Return)              , spawn $ XMonad.terminal conf)
       , ((myModMask, xK_d)                   , spawn "dmenu_run")
       , ((myModMask, xK_b)                   , spawn "feh --bg-fill --randomize ~/Pictures/wallpapers/*")
       , ((myModMask .|. shiftMask, xK_Return), spawn $ _terminal ++ " --working-directory `xcwd`")
       -- , ((myModMask .|. shiftMask, xK_Return), spawn $ Settings.terminal ++ " " ++ Settings.terminalDirectoryFlag ++ " `xcwd`")
       , ((myModMask, xK_z)                   , spawn "pcmanfm")
       -- , ((myModMask, xK_x)                   , debugStuff)
       -- , ((myModMask, xK_x)                   , spawn "dmkill")
       , ((myModMask, xK_c)                   , Dmenu.configs)
       -- , ((myModMask, xK_p)                   , Dmenu.prompts)
       , ((myModMask, xK_g)                   , Prompt.hoogle)
       , ((myModMask .|. shiftMask, xK_x)     , Prompt.calculator)
       , ((myModMask .|. shiftMask , xK_r)    , spawn "xmonad --recompile && xmonad --restart")
       , ((myModMask .|. shiftMask, xK_q)     , io exitSuccess)
       , ((myModMask, xK_space)               , sendMessage NextLayout)
       , ((myModMask .|. shiftMask, xK_space) , setLayout $ XMonad.layoutHook conf)
       , ((myModMask, xK_j)                   , windows W.focusDown)
       , ((myModMask, xK_k)                   , windows W.focusUp)
       , ((myModMask, xK_m)                   , windows W.focusMaster)
       , ((myModMask .|. shiftMask, xK_m)     , windows W.swapMaster)
       , ((myModMask .|. shiftMask, xK_j)     , windows W.swapDown)
       , ((myModMask .|. shiftMask, xK_k)     , windows W.swapUp)
       , ((myModMask, xK_h)                   , sendMessage Shrink)
       , ((myModMask, xK_l)                   , sendMessage Expand)
       , ((myModMask, xK_t)                   , withFocused $ windows . W.sink)
       , ((myModMask, xK_comma)               , sendMessage (IncMasterN 1))
       , ((myModMask, xK_period)              , sendMessage (IncMasterN (-1)))
       , ((myModMask .|. shiftMask, xK_q)     , liftIO exitSuccess)
       , ((myModMask, xK_Tab)                 , toggleWS)
       , ((myModMask, xK_q)                   , kill)
       , ((myModMask, xK_f)                   , sendMessage ToggleStruts)
       , ((0, xF86XK_AudioPlay)               , spawn "playerctl play-pause")
       , ((0, xF86XK_AudioStop)               , spawn "playerctl stop")
       , ((0, xF86XK_AudioNext)               , spawn "playerctl next")
       , ((0, xF86XK_AudioPrev)               , spawn "playerctl previous")
       , ((0, xF86XK_AudioMute)               , spawn "amixer -q set Master toggle")
       , ((0, xF86XK_AudioLowerVolume)        , spawn "amixer -q set Master 5%-")
       , ((0, xF86XK_AudioRaiseVolume)        , spawn "amixer -q set Master 5%+")
       , ((0, xF86XK_MonBrightnessUp)         , spawn "xbacklight -inc 5")
       , ((0, xF86XK_MonBrightnessDown)       , spawn "xbacklight -dec 5")
       ]
    ++ [ ((myModMask, k)                      , windows $ W.greedyView i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ]
    ++ [ ((myModMask .|. shiftMask, k)        , windows $ W.shift i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ]

_normalBorderColor :: String
_normalBorderColor = "#282c34"

_focusedBorderColor :: String
_focusedBorderColor = "#7f8c8d"

uniformBorder :: Integer -> Border
uniformBorder i = Border i i i i

smartGaps = spacingRaw True (uniformBorder 0) False (uniformBorder 5) True

_layout = avoidStruts $ (smartGaps . smartBorders $ tiled) ||| noBorders Full
 where
  tiled   = Tall nmaster delta ratio
  nmaster = 1
  ratio   = 1 / 2
  delta   = 3 / 100

_manageHook :: ManageHook
_manageHook = mconcat
  [ className =? "Gimp" --> doFloat
  , className =? "mpv" --> doFloat
  , className =? "firefox" --> doShift (_workspaces !! 2)
  , className =? "Uget-gtk" --> doShift (_workspaces !! 5)
  , className =? "Pcmanfm" --> doShift (_workspaces !! 3)
  , className =? "TelegramDesktop" --> doShift (_workspaces !! 4)
  , (title =? "Spotify Premium" <||> title =? "Spotify") --> doShift (_workspaces !! 7)
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  ]

trayer :: String
trayer =
  "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 22 "

myStartupHook :: X ()
myStartupHook = do
  spawnOnce trayer

--------------------------
-- XMOBAR CONFIGURATION --
--------------------------

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myXmobarPP :: X PP
myXmobarPP = clickablePP . filterOutWsPP [scratchpadWorkspaceTag] $ def
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

----------------------
-- XMOBAR INSTANCES --
----------------------
xmobar0 :: StatusBarConfig
xmobar0 = statusBarProp "xmobar" myXmobarPP


_config = def
    { terminal           = _terminal
    , modMask            = mod4Mask
    , keys               = _keys
    , layoutHook         = _layout
    , startupHook        = myStartupHook
    , workspaces         = _workspaces
    , normalBorderColor  = _normalBorderColor
    , focusedBorderColor = _focusedBorderColor
    , borderWidth        = 2
    , manageHook         = _manageHook <+> manageHook def
    }

main :: IO ()
main = xmonad . docks . withSB xmobar0 . ewmhFullscreen . ewmh $ _config
