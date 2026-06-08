module Midgard.Node.Migrations (
  findSqlMigrationsDirectory,
  listSqlMigrations,
) where

import Control.Monad (filterM)
import Data.List (sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

findSqlMigrationsDirectory :: IO FilePath
findSqlMigrationsDirectory = do
  let candidate = "../demo/midgard-node/src/database/migrations/sql"
  exists <- doesDirectoryExist candidate
  if exists
    then pure candidate
    else ioError (userError ("Midgard SQL migrations directory not found: " <> candidate))

listSqlMigrations :: IO [FilePath]
listSqlMigrations = do
  directory <- findSqlMigrationsDirectory
  names <- listDirectory directory
  let sortedNames = sort names
  sqlNames <-
    filterM
      (\name -> doesFileExist (directory </> name))
      [name | name <- sortedNames, takeExtension name == ".sql"]
  pure [directory </> name | name <- sqlNames]
