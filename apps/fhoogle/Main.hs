{-# LANGUAGE BlockArguments #-}
module Main where

import           Hoogle
import           System.Process

main :: IO ()
main = do
  dbLocation <- defaultDatabaseLocation
  withDatabase dbLocation \db -> do
    let targets = init $ unlines $ map (targetResultDisplay False) $ take 1 $ searchDatabase db "id"
    _ <- runCommand $ "echo '" ++ targets ++ "' | fzf-tmux -p"
    print targets
