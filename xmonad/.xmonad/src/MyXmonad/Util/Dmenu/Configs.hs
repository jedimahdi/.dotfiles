module MyXmonad.Util.Dmenu.Configs (configs) where

import qualified Data.Map                           as Map
import           Data.Map                            ( Map )
import           XMonad                              ( X
                                                     , spawn
                                                     )
import           XMonad.Util.Dmenu                   ( dmenuMap )

data Config = Config
  { directory :: String
  , firstFile :: String
  }

myConfigs :: Map String Config
myConfigs = Map.fromList
  [ ("neovim"   , Config "~/.config/nvim" "init.lua")
  , ("xmonad"   , Config "~/.xmonad" "src/MyXmonad.hs")
  , ("alacritty", Config "~/.config/alacritty" "alacritty.yml")
  , ("picom"    , Config "~/.config/picom" "picom.conf")
  , ("xmobar"   , Config "~/.config/xmobar" "xmobarrc")
  ]

spawnEditor :: Config -> X ()
spawnEditor config = spawn $ "alacritty --title nvim --working-directory " ++ directory config ++ " -e nvim " ++ firstFile config

configs :: X ()
configs = dmenuMap myConfigs >>= maybe (pure ()) spawnEditor
