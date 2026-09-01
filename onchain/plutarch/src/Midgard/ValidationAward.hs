module Midgard.ValidationAward (
  PValidationAwardArgs (..),
  PValidationAwardDatum,
  PValidationAwardSpendRedeemer,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.ComputationThread (PStepDatum, PStepRedeemer)

type PValidationAwardDatum = PStepDatum
type PValidationAwardSpendRedeemer = PStepRedeemer

data PValidationAwardArgs (s :: S) = PValidationAwardArgs
  { pvalidationAward'inputIndex :: Term s (PAsData PInteger)
  , pvalidationAward'outputIndex :: Term s (PAsData PInteger)
  , pvalidationAward'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationAwardArgs)
