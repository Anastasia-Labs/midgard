module Main (main) where

import Midgard.Node.App qualified as App
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let defaultConfigPath = "node/config/midgard-node.example.yaml"
  case args of
    [] -> App.runServe defaultConfigPath
    ["serve"] -> App.runServe defaultConfigPath
    ["migrate"] -> App.runMigrate defaultConfigPath
    ["verify"] -> App.runVerify defaultConfigPath
    ["serve", configPath] -> App.runServe configPath
    ["migrate", configPath] -> App.runMigrate configPath
    ["verify", configPath] -> App.runVerify configPath
    [configPath] -> App.runServe configPath
    _ -> fail "Usage: midgard-node [serve|migrate|verify] [config-path]"
