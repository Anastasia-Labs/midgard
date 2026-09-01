{- |
Module      : Midgard.FraudProofs.InvalidRange
Description : Plutarch port of @lib/midgard/fraud-proofs/invalid-range/step-0{1,2}.ak@.

The thread state and redeemer payloads of the invalid-range family (spec
§5.1.1): a committed transaction whose validity interval is not covered by the
block's.

Two steps. Step-01 binds the transaction, normalises its interval and pairs it
with the block's; step-02 adjudicates the pair.

=== The normalised range is thread state, and therefore wire format

@bad_tx_normalized_validity_range@ is a
'DesignPatterns.ValidityRangeNormalization.PNormalizedTimeRange' sitting
in a datum, so its @Constr@ tags are the SDK-facing interface. It is normalised
in step-01 rather than carried raw because the raw form is two sentinel-bearing
integers and the adjudication is a case analysis over four genuinely different
shapes — doing that analysis twice, once to normalise and once to decide, is how
the two halves would drift apart.
-}
module Midgard.FraudProofs.InvalidRange (
  PStep02State (..),
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import DesignPatterns.ValidityRangeNormalization (PNormalizedTimeRange)

{- | Aiken @invalid_range/step_02.State@.

The block's two bounds and the transaction's normalised range. The block's bounds
are read off the header in step-01, which is the only step that authenticated a
header — the same reason the counted commitment travels in
'Midgard.FraudProofs.WithdrawnReferenceInput'.
-}
data PStep02State (s :: S) = PStep02State
  { pstep02State'blockValidFrom :: Term s (PAsData PInteger)
  , pstep02State'blockValidTo :: Term s (PAsData PInteger)
  , pstep02State'badTxNormalizedValidityRange ::
      Term s (PAsData PNormalizedTimeRange)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @invalid_range/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)
