module MyXmonad.Util.Prompt.Prelude
    ( config
    ) where

import           Control.Arrow            ( first )
import qualified Data.Map                 as M
import           XMonad                   hiding ( config )
import           XMonad.Config.Dmwit
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import qualified XMonad.StackSet          as W

myFont :: String
myFont = "xft:Ubuntu:regular:size=13:antialias=true:hinting=true"

config :: XPConfig
config = def { font                = myFont
                 , bgColor             = "#282c34"
                 , fgColor             = "#bbc2cf"
                 , bgHLight            = "#c792ea"
                 , fgHLight            = "#000000"
                 , borderColor         = "#535974"
                 , promptBorderWidth   = 0
                 , promptKeymap        = dtXPKeymap
                 , position            = Top
                 , height              = 25
                 , historySize         = 256
                 , historyFilter       = id
                 , defaultText         = []
                 , autoComplete        = Just 100000
                 , showCompletionOnTab = False
                 , searchPredicate     = fuzzyMatch
                 , defaultPrompter     = id
                 , alwaysHighlight     = True
                 , maxComplRows        = Nothing
                 }


dtXPKeymap :: M.Map (KeyMask, KeySym) (XP ())
dtXPKeymap =
  M.fromList
    $  map
         (first $ (,) controlMask)      -- control + <key>
         [ (xK_z          , killBefore)               -- kill line backwards
         , (xK_k          , killAfter)                -- kill line forwards
         , (xK_a          , startOfLine)              -- move to the beginning of the line
         , (xK_e          , endOfLine)                -- move to the end of the line
         , (xK_m          , deleteString Next)        -- delete a character foward
         , (xK_b          , moveCursor Prev)          -- move cursor forward
         , (xK_f          , moveCursor Next)          -- move cursor backward
         , (xK_BackSpace  , killWord Prev)    -- kill the previous word
         , (xK_y          , pasteString)              -- paste a string
         , (xK_g          , quit)                     -- quit out of prompt
         , (xK_bracketleft, quit)
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

