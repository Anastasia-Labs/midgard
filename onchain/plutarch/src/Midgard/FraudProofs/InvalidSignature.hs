{- |
Module      : Midgard.FraudProofs.InvalidSignature
Description : Plutarch port of @lib/midgard/fraud-proofs/invalid-signature/step-0{1,2}.ak@.

The thread state and redeemer payload of the invalid-signature family (spec
§5.1.1): a committed transaction carrying an address witness whose signature does
not verify over the transaction id.

Two steps only. Step-01 binds the transaction and writes the §2.5 anchor; step-02
opens field 7 and checks one witness.

=== Both halves of the anchor travel, for the same reason as @missing-signature@

@bad_tx_id@ alone would anchor fields 0–5 and nothing else: §3's id preimage is
the body, so the compact structure's trailing @witness_set_hash@ is outside it
and a step-02 redeemer could supply any value there. Step-01 read the real one
off the structure the block's counted @transactions_root@ committed, so it is
carried — the same 32 bytes the retired @bad_addr_tx_wits_hash@ cost, covering
all three witness-set fields rather than one.
-}
module Midgard.FraudProofs.InvalidSignature (
  PStep02State (..),
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)

-- | Aiken @invalid_signature/step_02.State@ — the bad transaction's §2.5 anchor.
data PStep02State (s :: S) = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @invalid_signature/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for field 7's preimage, together with
    -- the witness set the door checks against the anchored hash.
    pstep02Args'addrTxWitsOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep02Args'badAddrTxWitIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)
