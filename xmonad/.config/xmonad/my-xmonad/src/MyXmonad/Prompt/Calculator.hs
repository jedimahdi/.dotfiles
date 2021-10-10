module MyXmonad.Prompt.Calculator
    ( calculator
    ) where

import           Data.Char                    ( isSpace )
import qualified MyXmonad.Prompt.Prelude as Prompt
import           XMonad                       ( MonadIO (liftIO), X )
import           XMonad.Prompt                ( XPConfig )
import           XMonad.Prompt.Input          ( inputPrompt, (?+) )
import           XMonad.Util.Run              ( runProcessWithInput )

calculator :: X ()
calculator = calcPrompt Prompt.config "qalc"

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = inputPrompt c (trim ans) ?+ \input -> liftIO (runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
    trim = f . f where f = reverse . dropWhile isSpace
