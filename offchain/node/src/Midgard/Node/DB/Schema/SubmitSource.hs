{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.SubmitSource (
  SubmitSource (..),
) where

import Database.Persist.TH (derivePersistField)

data SubmitSource
  = Native
  | Backfill
  deriving stock (Eq, Read, Show)

$(derivePersistField "SubmitSource")
