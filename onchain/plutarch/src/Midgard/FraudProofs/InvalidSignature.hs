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
  PStep01Source (..),
  PStep01Args (..),
  pterminalContradiction,
  PStep02State (..),
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PAddressWitnessSignatureInvalid))
import Midgard.TransitionTrace (PRootMembershipProof)

data PStep01Source (s :: S)
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PHeaderV1))
      (Term s (PAsData PRootMembershipProof))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Source)

newtype PStep01Args (s :: S) = PStep01Args (Term s (PAsData PStep01Source))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

-- | Aiken @invalid_signature/step_02.State@ — the bad transaction's §2.5 anchor.
data PStep02State (s :: S) = PStep02State
  { pstep02State'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pstep02State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @invalid_signature/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , pstep02Args'addrTxWitsOpening :: Term s (PAsData PFieldOpeningV1)
  {- ^ The prover's chosen §8 carriage for field 7's preimage, together with
  the witness set the door checks against the anchored hash.
  -}
  , pstep02Args'badAddrTxWitIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

{- | The rejection names an exact witness coordinate. An absent witness cannot
substantiate a signature-invalid rejection, even when its index is negative.
-}
pterminalContradiction :: forall s. Term s (Subject.PVerdictSubject :--> PInteger :--> PBool :--> PBool :--> PBool)
pterminalContradiction = phoistAcyclic $ plam $ \subject index inRange signatureValid ->
  pmatch subject $ \Subject.PVerdictSubject {Subject.psubject'direction} ->
    let result = Subject.pterminalContradiction # subject # (inRange #&& pnot # signatureValid)
     in pif
          (pfromData psubject'direction #== 1)
          (plet (Subject.pbindExactRejectionReason # subject # pcon (PAddressWitnessSignatureInvalid $ pdata index)) $ \_ -> result)
          result
