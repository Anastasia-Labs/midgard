{- |
Module      : Midgard.FraudProofs.NoInput
Description : Plutarch port of @lib/midgard/fraud-proofs/no-input/step-0{1,2,3,4}.ak@.

The thread state and redeemer payloads of the non-existent-input family (spec
§5.1.1): a committed transaction spending an output that never existed.

Non-existence is two absences, not one, and the family's shape is that pair.
An output either was in the block's initial ledger or was produced by some
transaction of the same block; the proof has to close both doors, so step-03
proves absence from @prev_utxos_root@ and step-04 proves absence from
@transactions_root@. Either alone proves nothing — an output produced mid-block
is legitimately absent from the initial ledger.

=== The two roots travel from step-01

Both are read off evidence step-01 already authenticated: @prev_utxos_root@ from
the challenged header, and the raw transactions root from the carriage /after/
'Midgard.FraudProofs.Common.ppassNativeTxToNextStep' has checked it against the
header's counted @transactions_root@. A later step cannot re-derive either, so
they are thread state — and thread state is what makes them the block's roots
rather than the prover's.

=== The keys are different, and neither is obvious

The ledger MPF is keyed by the node's CBOR encoding of a transaction input — a
definite two-element array, which is
'Midgard.FraudProofs.NativeTx.Components.pencodeMidgardTxInput' and /not/ a
serialised Plutus constructor. The transactions MPF is keyed by the raw 32-byte
native transaction id, directly. Getting either wrong produces a proof that
verifies against a key nothing ever stored.
-}
module Midgard.FraudProofs.NoInput (
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep03Args (..),
  PStep04State (..),
  PStep04Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNonMembershipCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput)

{- | Aiken @no_input/step_02.State@.

The disputed transaction's __id__, not its spend-inputs commitment: step-02
re-opens field 0 through the §8.8 door from that id, which is what lets it read
one input arithmetically instead of reproducing the whole collection to re-hash
it (the Q1X-F6 shape, issue #551).
-}
data PStep02State (s :: S) = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'blocksPrevUtxosRoot :: Term s (PAsData PByteString)
  , pstep02State'blocksTransactionsRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @no_input/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for field 0's preimage, together with
    -- the compact structures the id authenticates.
    pstep02Args'spendInputsOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep02Args'badInputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

-- | Aiken @no_input/step_03.State@ — the decoded disputed input, and both roots.
data PStep03State (s :: S) = PStep03State
  { pstep03State'missingInput :: Term s (PAsData PMidgardTxInput)
  , pstep03State'blocksPrevUtxosRoot :: Term s (PAsData PByteString)
  , pstep03State'blocksTransactionsRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

-- | Aiken @no_input/step_03.Args@.
data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen carriage for the initial-ledger absence proof.
    pstep03Args'nonMembershipInLedger :: Term s (PAsData PNonMembershipCarriage)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

{- | Aiken @no_input/step_04.State@.

Only the /producing transaction's/ id survives step-03, because that is the
transactions-root key. The output index is not part of it: a transaction that
does not exist produced no output at any index.
-}
data PStep04State (s :: S) = PStep04State
  { pstep04State'missingInputTxId :: Term s (PAsData PByteString)
  , pstep04State'blocksTransactionsRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

-- | Aiken @no_input/step_04.Args@.
data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen carriage for the transactions-root absence proof.
    pstep04Args'nonMembershipInTxs :: Term s (PAsData PNonMembershipCarriage)
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)
