{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.DepositsUtxosStatus (
  DepositsUtxosStatus (..),
) where

import Database.Persist.TH (derivePersistField)

data DepositsUtxosStatus
  = Awaiting
  | Projected
  | Consumed
  deriving stock (Eq, Read, Show)

$(derivePersistField "DepositsUtxosStatus")
