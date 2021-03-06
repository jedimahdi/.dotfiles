module MyXmonad.Keybindings
    ( myKeys
    ) where

import           Data.Map                        ( Map )
import qualified Data.Map                        as Map
import           Graphics.X11.ExtraTypes.XF86
import qualified MyXmonad.Settings               as Settings
import qualified MyXmonad.Util.Dmenu.Configs     as Dmenu
import qualified MyXmonad.Util.Dmenu.Prompts     as Dmenu
import qualified MyXmonad.Util.Prompt.Calculator as Prompt
import qualified MyXmonad.Util.Prompt.Hoogle     as Prompt
import           System.Exit                     ( exitSuccess )
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS          ( toggleWS )
import           XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet                 as W

debugStuff :: X ()
debugStuff = withWindowSet (\ws -> do
    liftIO $ logToTmpFile $ show ws
  )

myAppendFile :: FilePath -> String -> IO ()
myAppendFile f s = do
  withFile f AppendMode $ \h -> do
    hPutStrLn h s

logToTmpFile :: String -> IO ()
logToTmpFile = myAppendFile "~/tmp/xmonad.log" . (++ "\n")

myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.modMask = myModMask } =
  Map.fromList
    $  [ ((myModMask, xK_Return)              , spawn $ XMonad.terminal conf)
       , ((myModMask, xK_d)                   , spawn "dmenu_run")
       , ((myModMask, xK_b)                   , spawn "feh --bg-fill --randomize ~/Pictures/wallpapers/*")
       , ((myModMask .|. shiftMask, xK_Return), spawn $ Settings.terminal ++ " " ++ Settings.terminalDirectoryFlag ++ " `xcwd`")
       , ((myModMask, xK_z)                   , spawn "pcmanfm")
       , ((myModMask, xK_x)                   , debugStuff)
       -- , ((myModMask, xK_x)                   , spawn "dmkill")
       , ((myModMask, xK_c)                   , Dmenu.configs)
       , ((myModMask, xK_p)                   , Dmenu.prompts)
       , ((myModMask, xK_g)                   , Prompt.hoogle)
       , ((myModMask .|. shiftMask, xK_x)     , Prompt.calculator)
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
       ]
    ++ [ ((myModMask, k)                      , windows $ W.greedyView i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ]
    ++ [ ((myModMask .|. shiftMask, k)        , windows $ W.shift i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] ]
