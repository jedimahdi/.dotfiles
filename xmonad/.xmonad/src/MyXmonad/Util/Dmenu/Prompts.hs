module MyXmonad.Util.Dmenu.Prompts
    ( prompts
    ) where

import           Data.Map                        ( Map )
import qualified Data.Map                        as Map
import qualified MyXmonad.Util.Prompt.Calculator as Prompt
import qualified MyXmonad.Util.Prompt.Hoogle     as Prompt
import           XMonad                          ( X )
import           XMonad.Util.Dmenu               ( dmenuMap )

newtype Prompt
  = Prompt { prompt :: X () }

myPrompts :: Map String Prompt
myPrompts = Map.fromList
  [ ("calculator" , Prompt Prompt.calculator)
  , ("hoogle"     , Prompt Prompt.hoogle)
  ]

spawnEditor :: Prompt -> X ()
spawnEditor = prompt

prompts :: X ()
prompts = dmenuMap myPrompts >>= maybe (pure ()) spawnEditor
