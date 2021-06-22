module MyXmonad.Layout (myLayout) where

import           XMonad
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing

uniformBorder :: Integer -> Border
uniformBorder i = Border i i i i

smartGaps = spacingRaw True (uniformBorder 0) False (uniformBorder 5) True

myLayout = avoidStruts $ (smartGaps . smartBorders $ tiled) ||| noBorders Full
 where
  tiled   = Tall nmaster delta ratio
  nmaster = 1
  ratio   = 1 / 2
  delta   = 3 / 100
