module MyXmonad.Prompt.Prelude
    ( completionFunctionWith
    , config
    ) where

import           Control.Arrow            ( first )
import qualified Data.Map                 as M
import           XMonad                   hiding ( config )
import           XMonad.Config.Dmwit
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import qualified XMonad.StackSet          as W
import           XMonad.Util.Run          ( runProcessWithInput )

completionFunctionWith :: String -> [String] -> IO [String]
completionFunctionWith cmd args = lines <$> runProcessWithInput cmd args ""

myFont :: String
myFont = "xft:Victor Mono:medium:size=16:antialias=true:hinting=true"

config :: XPConfig
config = def { font                    = myFont
                 , bgColor             = "#1a1b26"
                 , fgColor             = "#d6d5d5"
                 , bgHLight            = "#1a1b26"
                 , fgHLight            = "#71abeb"
                 , borderColor         = "#1a1b26"
                 , promptBorderWidth   = 0
                 , promptKeymap        = dtXPKeymap
                 , position            = Top
                 , height              = 30
                 , historySize         = 10
                 , historyFilter       = id
                 , defaultText         = []
                 , autoComplete        = Nothing
                 , showCompletionOnTab = False
                 , searchPredicate     = fuzzyMatch
                 , alwaysHighlight     = True
                 , maxComplRows        = Nothing
                 }


dtXPKeymap :: M.Map (KeyMask, KeySym) (XP ())
dtXPKeymap =
  M.fromList
    $  map
         (first $ (,) controlMask)      -- control + <key>
         [ (xK_z          , killBefore)               -- kill line backwards
         , (xK_w, killWord Prev)
         , (xK_a, startOfLine)
         , (xK_e, endOfLine)
         , (xK_d, deleteString Next)
         , (xK_v, pasteString)              -- paste a string
         , (xK_b, moveCursor Prev) -- move cursor forward
         , (xK_f, moveCursor Next)
         , (xK_g, quit)                     -- quit out of prompt
         ]
    ++ map
         (first $ (,) altMask)          -- meta key + <key>
         [ (xK_BackSpace, killWord Prev)    -- kill the prev word
         , (xK_f        , moveWord Next)            -- move a word forward
         , (xK_b        , moveWord Prev)            -- move a word backward
         , (xK_d        , killWord Next)            -- kill the next word
         , (xK_n        , moveHistory W.focusUp')   -- move up thru history
         , (xK_p        , moveHistory W.focusDown') -- move down thru history
         ]
    ++ map
         (first $ (,) 0) -- <key>
         [ (xK_Return   , setSuccess True >> setDone True)
         , (xK_KP_Enter , setSuccess True >> setDone True)
         , (xK_BackSpace, deleteString Prev)
         , (xK_Delete   , deleteString Next)
         , (xK_Left     , moveCursor Prev)
         , (xK_Right    , moveCursor Next)
         , (xK_Home     , startOfLine)
         , (xK_End      , endOfLine)
         , (xK_Down     , moveHistory W.focusUp')
         , (xK_Up       , moveHistory W.focusDown')
         , (xK_Escape   , quit)
         ]

