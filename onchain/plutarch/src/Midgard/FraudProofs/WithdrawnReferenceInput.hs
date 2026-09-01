{- |
Module      : Midgard.FraudProofs.WithdrawnReferenceInput
Description : Plutarch port of @lib/midgard/fraud-proofs/withdrawn-reference-input/step-0{1,2,3}.ak@.

The thread state and redeemer payloads of the withdrawn-reference-input family
(spec §5.1.16): a committed transaction referencing an output that a withdrawal
event had already taken off L2.

Three steps. Step-01 binds the transaction and picks up the header's counted
withdrawals commitment; step-02 opens field 1 and names the reference input;
step-03 exhibits the withdrawal event that spent it.

=== Why the counted commitment travels rather than being re-read

The withdrawals root and its count are read off the block header in step-01 and
carried to step-03 as thread state, even though step-03 could in principle look
them up again. It could not do so /soundly/: step-03 has no inclusion argument
and therefore no way to establish which block's header it is reading. The pair
travels for the same reason the §2.5 anchor does — the step that authenticated it
is the only step that can.

=== The count rides along with the root

Both halves are needed, because a Midgard root is a commitment to
@(domain, phas_root, count)@ and step-03 has to unwrap it before it can walk the
raw MPF tree underneath. A root without its count cannot be unwrapped at all, and
a count a redeemer chose would let a prover present a tree of the wrong size —
which is precisely what the counted scheme exists to prevent.
-}
module Midgard.FraudProofs.WithdrawnReferenceInput (
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep03Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput)
import Midgard.TransitionTrace (PRootMembershipProof)

{- | Aiken @withdrawn_reference_input/step_02.State@.

The disputed transaction's id, and the block's counted withdrawals commitment.
Only the id is an anchor here: this family opens field 1, which lives in the body,
so §3's id preimage covers it and no witness-set hash is needed.
-}
data PStep02State (s :: S) = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'blocksWithdrawalsRoot :: Term s (PAsData PByteString)
  , pstep02State'blocksWithdrawalCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @withdrawn_reference_input/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for field 1's preimage, together with
    -- the compact structures @bad_tx_id@ authenticates.
    pstep02Args'referenceInputsOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep02Args'badReferenceInputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

-- | Aiken @withdrawn_reference_input/step_03.State@.
data PStep03State (s :: S) = PStep03State
  { pstep03State'missingReferenceInput :: Term s (PAsData PMidgardTxInput)
  , pstep03State'blocksWithdrawalsRoot :: Term s (PAsData PByteString)
  , pstep03State'blocksWithdrawalCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

{- | Aiken @withdrawn_reference_input/step_03.Args@.

Aiken's @withdrawal_membership@ is a @RootMembershipProof<WithdrawalId,
WithdrawalInfo>@; the port fixes both parameters to 'PByteString' and the value is
decoded from its bytes at the call site. See 'Midgard.TransitionTrace'.
-}
data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , -- | Counted-root membership witness for the withdrawal event under the
    -- header's @withdrawals_root@ (@WithdrawalsRootDomain@).
    pstep03Args'withdrawalMembership :: Term s (PAsData PRootMembershipProof)
  , pstep03Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)
