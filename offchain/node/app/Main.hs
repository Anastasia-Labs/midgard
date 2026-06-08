module Main (main) where

import Midgard.Node.App qualified as App
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let defaultConfigPath = "node/config/midgard-node.example.yaml"
  -- Keep the CLI intentionally small for now:
  --   serve   - run the HTTP server
  --   migrate - apply pending SQL migrations
  --   verify  - assert that the DB matches the schema this binary expects
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
