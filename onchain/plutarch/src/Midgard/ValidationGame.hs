module Midgard.ValidationGame (
  PPendingValidationClaimV1 (..),
  PValidationGameStateV1 (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PPubKeyHash)
import Plutarch.Prelude

import Midgard.LedgerState (PHeaderV1)
import Midgard.ValidationClaim (PValidationClaimWitnessV1)
import Midgard.ValidationDispute (PValidationDisputeV1)
import Midgard.ValidationTrace (PValidationTraceDescriptorV1)

data PPendingValidationClaimV1 (s :: S) = PPendingValidationClaimV1
  { ppendingClaim'challengedHeaderHash :: Term s (PAsData PByteString)
  , ppendingClaim'challengedHeader :: Term s (PAsData PHeaderV1)
  , ppendingClaim'claim :: Term s (PAsData PValidationClaimWitnessV1)
  , ppendingClaim'challengerDescriptor :: Term s (PAsData PValidationTraceDescriptorV1)
  , ppendingClaim'openTimeUpper :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPendingValidationClaimV1)

data PValidationGameStateV1 (s :: S) = PValidationGameStateV1
  { pvalidationGame'challengedHeaderHash :: Term s (PAsData PByteString)
  , pvalidationGame'operatorVkey :: Term s (PAsData PPubKeyHash)
  , pvalidationGame'dispute :: Term s (PAsData PValidationDisputeV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationGameStateV1)
