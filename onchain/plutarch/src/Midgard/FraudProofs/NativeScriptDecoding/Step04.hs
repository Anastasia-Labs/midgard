module Midgard.FraudProofs.NativeScriptDecoding.Step04 (PArgs (..)) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

data PArgs s = PArgs
  { pargs'inputIndex :: Term s (PAsData PInteger)
  , pargs'outputIndex :: Term s (PAsData PInteger)
  , pargs'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PArgs)
