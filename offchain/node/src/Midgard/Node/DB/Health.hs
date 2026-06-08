module Midgard.Node.DB.Health (
  ping,
) where

import Data.Pool (Pool)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)

-- Minimal DB liveness probe used by `readyz`.
-- Keep this deliberately small: the purpose is to detect broken connections,
-- not to duplicate the deeper schema checks in `DB.Migration.verifyDatabase`.
ping :: Pool SqlBackend -> IO Bool
ping pool = do
  rows <- runDB pool (rawSql "SELECT 1" [])
  pure (not (null (rows :: [Single Int])))
