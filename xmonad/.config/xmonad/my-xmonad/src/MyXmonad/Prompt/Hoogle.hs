module MyXmonad.Prompt.Hoogle
    ( hoogle
    ) where

import           Data.List                    ( findIndex, isPrefixOf, tails )
import           MyXmonad.Prompt.Prelude ( completionFunctionWith )
import qualified MyXmonad.Prompt.Prelude as Prompt
import           XMonad                       hiding ( config )
import           XMonad.Prompt

data Hoogle = Hoogle

instance XPrompt Hoogle where
  showXPrompt Hoogle = "Hoogle: "

hoogle :: X ()
hoogle = mkXPrompt Hoogle Prompt.config hoogleCompletion hoogleRun

hoogleCompletion :: String -> IO [String]
hoogleCompletion s = completionFunctionWith pathToHoogleBin ["--count", "10", "--link", s]

hoogleRun :: String -> X ()
hoogleRun s = do
  let link = do
        i <- findSeqIndex s "https://"
        pure $ drop i s
  case link of
    Just l -> spawn $ "brave " ++ l
    _      -> pure ()
  where
    findSeqIndex :: (Eq a) => [a] -> [a] -> Maybe Int
    findSeqIndex xs xss = findIndex (isPrefixOf xss) $ tails xs

pathToHoogleBin :: String
pathToHoogleBin = "/home/mahdi/.cabal/bin/hoogle"

