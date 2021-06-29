module MyXmonad
    ( main
    ) where

import qualified Data.Map                  as M
import           Data.Maybe                ( fromJust )
import           Graphics.X11.Xlib
import           MyXmonad.Keybindings      ( myKeys )
import           MyXmonad.Layout           ( myLayout )
import           System.IO                 ( hPutStrLn )
import           XMonad
import           XMonad.Actions.ShowText
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet           as W
import           XMonad.Util.Run           ( spawnPipe )
import           XMonad.Util.SpawnOnce

myTerminal :: String
myTerminal = "kitty"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myNormalBorderColor :: String
myNormalBorderColor = "#282c34"

myFocusedBorderColor :: String
myFocusedBorderColor = "#7f8c8d"

myBorderWidth :: Dimension
myBorderWidth = 2

myWorkspaces :: [String]
myWorkspaces = [" dev ", " term ", " www ", " file ", " vid ", " uget ", " gfx ", " vbox ", " mus "]

myWorkspaceIndices :: M.Map String Int
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

clickable :: String -> String
clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>" where i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: ManageHook
myManageHook = mconcat
  [ className =? "Gimp" --> doFloat
  , className =? "mpv" --> doFloat
  , className =? "firefox" --> doShift (myWorkspaces !! 2)
  , className =? "Uget-gtk" --> doShift (myWorkspaces !! 5)
  , className =? "Pcmanfm" --> doShift (myWorkspaces !! 3)
  , className =? "TelegramDesktop" --> doShift (myWorkspaces !! 4)
  , className =? "Spotify" --> doShift (myWorkspaces !! 7)
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  ]

trayer :: String
trayer =
  "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 22 "

myStartupHook :: X ()
myStartupHook = mapM_
  spawnOnce
  [ trayer
  , "nm-applet"
  , "/home/mahdi/tmp/.dotfiles/.i3/keyboard.sh"
  , "xfce4-power-manager"
  , "/usr/lib/xfce4/notifyd/xfce4-notifyd"
  , "volumeicon"
  , "feh --bg-fill ~/Pictures/wallpapers/sensei-1920×1080.jpg"
  , "picom"
  ]

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad . ewmh . docks $ def
    { modMask            = mod4Mask
    , terminal           = myTerminal
    , keys               = myKeys
    , workspaces         = myWorkspaces
    , startupHook        = myStartupHook
    , layoutHook         = myLayout
    , handleEventHook    = handleTimerEvent
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , borderWidth        = myBorderWidth
    , logHook            = dynamicLogWithPP xmobarPP { ppOutput          = hPutStrLn xmproc
                                                     , ppTitle           = xmobarColor "#b3afc2" "" . shorten 60
                                                     , ppCurrent         = xmobarColor "#98be65" "" . wrap "[" "]"
                                                     , ppVisible         = xmobarColor "#98be65" "" . clickable
                                                     , ppHiddenNoWindows = xmobarColor "#c792ea" "" . clickable
                                                     , ppHidden          = xmobarColor "#82AAFF" "" . wrap "*" "" . clickable
                                                     , ppSep             = "<fc=#666666> <fn=1>|</fn> </fc>"
                                                     , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"
                                                     , ppExtras          = [windowCount]
                                                     , ppOrder           = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                                                     }
    , manageHook         = myManageHook <+> manageHook def
    }
