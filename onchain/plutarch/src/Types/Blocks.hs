{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Blocks (
  StateCommitmentSetNode (..),
  PStateCommitmentSetNode (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude
import PlutusLedgerApi.V2 (BuiltinByteString)
import PlutusTx qualified
import Types.StateCommitment

data StateCommitmentSetNode = MkStateCommitmentSetNode
  { key :: BuiltinByteString
  , next :: BuiltinByteString
  , blockInfo :: StateCommitment
  }
  deriving stock (Show, Generic)

PlutusTx.unstableMakeIsData ''StateCommitmentSetNode

data PStateCommitmentSetNode (s :: S)
  = PStateCommitmentSetNode
  { pscsn'key :: Term s (PAsData PByteString)
  , pscsn'next :: Term s (PAsData PByteString)
  , pscsn'commitmentInfo :: Term s PStateCommitment
  }
  deriving stock (Generic)
  deriving anyclass
    ( SOP.Generic
    , PIsData
    , PEq
    , PShow
    )
  deriving (PlutusType) via (DeriveAsDataStruct PStateCommitmentSetNode)

deriving via
  DeriveDataPLiftable PStateCommitmentSetNode StateCommitmentSetNode
  instance
    PLiftable PStateCommitmentSetNode
