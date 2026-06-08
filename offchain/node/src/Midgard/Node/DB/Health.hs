module Midgard.Node.DB.Health (
  ping,
) where

import Data.Pool (Pool)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)

ping :: Pool SqlBackend -> IO Bool
ping pool = do
  rows <- runDB pool (rawSql "SELECT 1" [])
  pure (not (null (rows :: [Single Int])))
