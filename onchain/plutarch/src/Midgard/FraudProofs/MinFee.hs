{- |
Module      : Midgard.FraudProofs.MinFee
Description : Plutarch port of @lib/midgard/fraud-proofs/min-fee/step-0{1,2}.ak@.

The thread state and redeemer payload of the min-fee family (spec §5.1.1): a
committed transaction whose fee is below the protocol minimum.

=== This family cannot finalize, and that is Aiken's state too

Aiken's step-02 concludes @bad_tx_body_fee < get_min_transaction_fee(bad_tx)@,
and @get_min_transaction_fee@ is a stub marked @TODO: This will need execution
traces to calculate it@ that returns @0@ for every transaction. The conclusion is
therefore @fee < 0@, which no honest fee satisfies. The port reproduces the stub
rather than inventing a fee model: the two trees agree on which transactions pass,
and a Plutarch validator that convicted where Aiken's does not would be the
divergence, not the fix.

The step is still ported in full, because its /binding/ half is real — step-01
authenticates the transaction against the block's counted root exactly as every
other family's does — and because the redeemer and datum shapes are the SDK-facing
interface whether or not the last conjunct can hold.

=== Why @bad_tx@ is @PData@ here

Aiken's state field is a @NativeTxCompact@. The port's 'PNativeTxCompact' is
Scott-encoded, deliberately: it is produced by the codec and consumed by the
accessors inside a single script and never crosses a @Data@ boundary anywhere
else, and Data-encoding a twelve-field body would cost execution units in every
native family to serve this one.

So the field is typed 'PData' and step-01 writes the bytes through
'Midgard.Validators.FraudProofs.MinFee.pnativeTxCompactToData', which produces
exactly the @Constr 0@ layout an SDK builds. The datum bytes are identical to
Aiken's; what is given up is a decode step-02 never performs. __If
@get_min_transaction_fee@ ever becomes real and step-02 has to read the compact
structure, this is where the type must move to @DeriveAsDataStruct@__ — and that
change has to carry 'PNativeTxBodyCompact' with it.
-}
module Midgard.FraudProofs.MinFee (
  PStep02State (..),
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

{- | Aiken @min_fee/step_02.State@.

@bad_tx@ is the verified compact structure as @Data@; see the module header for
why it is not decoded.
-}
data PStep02State (s :: S) = PStep02State
  { pstep02State'badTx :: Term s PData
  , pstep02State'badTxBodyFee :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @min_fee/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)
