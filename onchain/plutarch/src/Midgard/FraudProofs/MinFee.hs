{- |
Module      : Midgard.FraudProofs.MinFee
Description : Plutarch port of @lib/midgard/fraud-proofs/min-fee/step-0{1,2}.ak@.

The thread state and redeemer payload of the min-fee family (spec §5.1.1): a
committed transaction whose fee is below the disputed header's fee schedule.

=== Why @bad_tx@ is @PData@ here

Aiken's state field is a @NativeTxCompact@. The port's 'PNativeTxCompact' is
Scott-encoded, deliberately: it is produced by the codec and consumed by the
accessors inside a single script in the rest of the port.

So the field is typed 'PData' and step-01 writes the bytes through
'Midgard.Validators.FraudProofs.MinFee.pnativeTxCompactToData', which produces
exactly the @Constr 0@ layout an SDK builds. The datum bytes are identical to
Aiken's; step-02 decodes the few scalar fields it needs back into the existing
Scott representation before applying the canonical-size calculation.
-}
module Midgard.FraudProofs.MinFee (
  PStep02State (..),
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Types (PNativeTxWitnessSetCompact)
import Midgard.NativeTxFieldAccess (PFieldCarriageV1)

{- | Aiken @min_fee/step_02.State@.

@bad_tx@ is the verified compact structure as @Data@; see the module header for
why it is not decoded.
-}
data PStep02State (s :: S) = PStep02State
  { pstep02State'badTx :: Term s PData
  , pstep02State'badTxBodyFee :: Term s (PAsData PInteger)
  , pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'minFeeA :: Term s (PAsData PInteger)
  , pstep02State'minFeeB :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @min_fee/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , pstep02Args'nativeTxCompactCbor :: Term s (PAsData PByteString)
  , pstep02Args'witnessSet :: Term s (PAsData PNativeTxWitnessSetCompact)
  , pstep02Args'fieldCarriages :: Term s (PAsData (PBuiltinList (PAsData PFieldCarriageV1)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)
