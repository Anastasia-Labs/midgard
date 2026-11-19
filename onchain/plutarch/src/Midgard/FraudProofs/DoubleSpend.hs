{- |
Module      : Midgard.FraudProofs.DoubleSpend
Description : Plutarch port of @lib/midgard/fraud-proofs/double-spend/step-0{1,2,3,4}.ak@.

The thread state and redeemer payloads of the double-spend family (spec §5.1.1):
two distinct transactions of one block spending the same output.

Each Aiken step module declares a @State@ (what the step's own @StepDatum@
carries), an @Args@ (what its @Continue@ redeemer carries) and two aliases over
"Midgard.ComputationThread"'s generic pair. Only the first two are types of their
own, so only those are here; the aliases are spelled at the validator's call
sites, where the payload type is known.

=== Everything here is wire format

A @State@ is the @data@ field of a step's datum and an @Args@ is the payload of
its @Continue@ redeemer, so both are @Constr 0@ with fields in declaration order.
The validators compare produced state /as @Data@, byte for byte/ against a value
they rebuild — see 'Midgard.Validators.FraudProofs.DoubleSpend' — so a shifted
field would not merely mis-decode, it would make every honest continuation fail.

=== Why the state is this small

Step-01 forwards only the verified transaction id. Carrying its spend-inputs
commitment alongside would be carrying a second, weaker name for the same thing:
step-03 re-opens field 0 through the §8.8 door /from the id/, and under §4's
plain hashing a field commitment names no slot at all — a field-0 and a field-1
preimage over the same items hash identically. The id is what pins the slot,
positionally, through the compact structures it authenticates.
-}
module Midgard.FraudProofs.DoubleSpend (
  -- * Step 01
  PStep01State (..),

  -- * Step 02
  PStep02State (..),

  -- * Step 03
  PStep03State (..),
  PStep03Args (..),

  -- * Step 04
  PStep04State (..),
  PStep04Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput)

{- | Aiken @double_spend/step_01.State@.

Declared by the Aiken module and never read: step-01 spends the initialised
fraud-proof UTxO, whose @data@ is @None@. It is ported because the step's
@Datum@ alias is @StepDatum<State>@ and dropping the parameter would make the
two files stop corresponding.
-}
newtype PStep01State (s :: S) = PStep01State
  {pstep01State'verifiedTx1Id :: Term s (PAsData PByteString)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01State)

{- | Aiken @double_spend/step_02.State@ — the first conflicting transaction's
verified id, which step-01 produced and step-02 reads.
-}
newtype PStep02State (s :: S) = PStep02State
  {pstep02State'verifiedTx1Id :: Term s (PAsData PByteString)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

{- | Aiken @double_spend/step_03.State@ — both verified ids.

Steps 03 and 04 each re-open one transaction's field 0 through the door from its
own id, so both travel together until the first of them has been used.
-}
data PStep03State (s :: S) = PStep03State
  { pstep03State'verifiedTx1Id :: Term s (PAsData PByteString)
  , pstep03State'verifiedTx2Id :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

-- | Aiken @double_spend/step_03.Args@.
data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for tx1's field-0 preimage.
    pstep03Args'tx1SpendInputsOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep03Args'doubleSpentInputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

{- | Aiken @double_spend/step_04.State@.

The second transaction's id, still unused, and the /decoded/ output reference
step-03 read out of the first transaction's authenticated field 0. Carrying the
decoded input rather than its bytes is what lets step-04 compare with @==@ on a
value both sides derived through the same decoder.
-}
data PStep04State (s :: S) = PStep04State
  { pstep04State'verifiedTx2Id :: Term s (PAsData PByteString)
  , pstep04State'doubleSpentInput :: Term s (PAsData PMidgardTxInput)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

-- | Aiken @double_spend/step_04.Args@.
data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for tx2's field-0 preimage.
    pstep04Args'tx2SpendInputsOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep04Args'doubleSpentInputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)
