{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.WithdrawalValidity (
  WithdrawalValidity (..),
) where

import Database.Persist.TH (derivePersistField)

data WithdrawalValidity
  = WithdrawalIsValid
  | NonExistentWithdrawalUtxo
  | SpentWithdrawalUtxo
  | IncorrectWithdrawalOwner
  | IncorrectWithdrawalValue
  | IncorrectWithdrawalSignature
  | TooManyTokensInWithdrawal
  | UnpayableWithdrawalValue
  deriving stock (Eq, Read, Show)

$(derivePersistField "WithdrawalValidity")
