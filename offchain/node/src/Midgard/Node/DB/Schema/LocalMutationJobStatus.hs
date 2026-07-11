{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.LocalMutationJobStatus (
  LocalMutationJobStatus (..),
) where

import Database.Persist.TH (derivePersistField)

data LocalMutationJobStatus
  = Running
  | Completed
  | Failed
  deriving stock (Eq, Read, Show)

$(derivePersistField "LocalMutationJobStatus")
