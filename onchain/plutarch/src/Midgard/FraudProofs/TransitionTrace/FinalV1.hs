{- |
Module      : Midgard.FraudProofs.TransitionTrace.FinalV1
Description : Plutarch port of @lib/midgard/fraud-proofs/transition-trace/final-v1.ak@.

The datum and redeemer shape shared by all eight final transition-trace
validators. Aiken keeps it in a library file rather than in each validator so
that the eight agree on their ABI by construction, and the port does the same.

The datum is @ct.StepDatum<TransitionFaultProof>@, which the port already has as
'Midgard.ComputationThread.PStepDatum' carrying its state as raw @Data@ — so the
only thing this module has to name is the @Continue@ payload.

=== Why four fields when six of the eight read three

@hub_ref_input_index@ is read by @deposit-v1@ and @l1-event-v1@ alone; the other
six skip it with @..@. It stays in the shared type because a router that has to
choose which of eight scripts a thread continues to would otherwise have to
choose between two redeemer shapes as well, and the field is one integer.
-}
module Midgard.FraudProofs.TransitionTrace.FinalV1 (
  PTransitionTraceFinalArgs (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

{- | Aiken @final_v1.Args@.

The two output indices a conviction needs — where the thread is spent and where
the conviction is parked — the hub oracle reference input for the two validators
that consult it, and the index of the fraud-proof policy's mint redeemer.
-}
data PTransitionTraceFinalArgs (s :: S) = PTransitionTraceFinalArgs
  { pfinalArgs'inputIndex :: Term s (PAsData PInteger)
  , pfinalArgs'outputIndex :: Term s (PAsData PInteger)
  , pfinalArgs'hubRefInputIndex :: Term s (PAsData PInteger)
  , pfinalArgs'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTransitionTraceFinalArgs)
