module Main (main) where

import Data.Maybe (fromMaybe)
import Midgard.Node.App qualified as App
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let configPath = fromMaybe "node/config/midgard-node.example.yaml" $
        case args of
          [] -> Nothing
          (path : _) -> Just path
  App.run configPath
