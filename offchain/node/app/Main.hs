module Main (main) where

import Midgard.Node.App qualified as App
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let defaultConfigPath = "node/config/midgard-node.example.yaml"
  case args of
    ["serve"] -> App.runServe defaultConfigPath
    _ -> fail "Usage: midgard-node [serve]"
