module Spec.Node.DB (tests) where

import Data.ByteString.Char8 qualified as ByteString
import System.Environment (lookupEnv)

import Database.Persist.Postgresql
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Midgard.Node.DB.Schema (migrateAll)
import Midgard.Node.DB.Utils (runWithConnStr)

tests :: TestTree
tests =
  testGroup
    "node-db"
    [ testCase "Persistent schema matches the externally migrated database" $ do
        connectionString <- requireDatabaseUrl
        pendingMigrations <- runWithConnStr connectionString (getMigration migrateAll)
        pendingMigrations @?= []
    ]

-- The test intentionally targets a database initialized by the canonical raw
-- SQL migrations. Haskell must only verify that Persistent sees no schema drift.
requireDatabaseUrl :: IO ConnectionString
requireDatabaseUrl = do
  value <- lookupEnv "DB_CONN_STR"
  case value of
    Just connectionString
      | not (null connectionString) ->
          pure (ByteString.pack connectionString)
    _ -> do
      assertFailure "DB_CONN_STR must provide a postgresql compatible connection string"
      pure ByteString.empty
