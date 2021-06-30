module MyXmonad.Util.Dmenu.Configs
    ( configs
    ) where

import           Data.Map          ( Map )
import qualified Data.Map          as Map
import qualified MyXmonad.Settings as Settings
import           XMonad            ( X, spawn )
import           XMonad.Util.Dmenu ( dmenuMap )

data Config = Config { directory :: String
                     , firstFile :: String
                     }

myConfigs :: Map String Config
myConfigs = Map.fromList
  [ ("neovim"   , Config "~/.config/nvim" "init.lua")
  , ("xmonad"   , Config "~/.xmonad" "src/MyXmonad.hs")
  , ("alacritty", Config "~/.config/alacritty" "alacritty.yml")
  , ("kitty"    , Config "~/.config/kitty" "kitty.conf")
  , ("picom"    , Config "~/.config/picom" "picom.conf")
  , ("xmobar"   , Config "~/.config/xmobar" "xmobarrc")
  ]

spawnEditor :: Config -> X ()
spawnEditor config = spawn
  $ Settings.terminal ++ " --title nvim " ++ Settings.terminalDirectoryFlag ++ " " ++ directory config ++ " " ++ Settings.terminalExecuteFlag ++ " nvim " ++ firstFile config

configs :: X ()
configs = dmenuMap myConfigs >>= maybe (pure ()) spawnEditor
