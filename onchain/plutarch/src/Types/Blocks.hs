{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Types.Blocks (
  StateCommitmentSetNode(..),
  PStateCommitmentSetNode(..),
) where

import Plutarch.Prelude
import Plutarch.DataRepr (DerivePConstantViaData(DerivePConstantViaData), PDataFields)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusLedgerApi.V2 (BuiltinByteString)
import qualified PlutusTx
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
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PByteString
               , "next" ':= PByteString
               , "commitmentInfo" ':= PStateCommitment  
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PStateCommitmentSetNode where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PStateCommitmentSetNode where
  type PLifted PStateCommitmentSetNode = StateCommitmentSetNode

deriving via
  (DerivePConstantViaData StateCommitmentSetNode PStateCommitmentSetNode)
  instance
    PConstantDecl StateCommitmentSetNode