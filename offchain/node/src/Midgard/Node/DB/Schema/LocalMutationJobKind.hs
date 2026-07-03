{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.LocalMutationJobKind (
  LocalMutationJobKind (..),
) where

import Database.Persist.TH (derivePersistField)

data LocalMutationJobKind
  = LocalBlockFinalization
  | ConfirmedMergeFinalization
  deriving stock (Eq, Read, Show)

$(derivePersistField "LocalMutationJobKind")
