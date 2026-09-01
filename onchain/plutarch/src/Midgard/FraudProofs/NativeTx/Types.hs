{- |
Module      : Midgard.FraudProofs.NativeTx.Types
Description : Partial Plutarch port of
              @lib/midgard/fraud-proofs/native-tx/types.ak@.

The value types the native-transaction carriage layer decodes into.

Ported on demand, in two halves.

The *component* types — inputs, addresses, values, outputs and witnesses — are
what "Midgard.FraudProofs.NativeTx.Components" encodes and decodes. They are
@Data@-encoded, and that is not a choice: Aiken represents every user type as
@Constr@ data, and these appear inside @List<..>@ fields of
@MidgardTransactionBody@, which at the UPLC level is a @list(data)@. Their wire
form is therefore part of the format, not an implementation detail.

The *compact* types are the records "Midgard.FraudProofs.NativeTx.Compact"
builds and reads, and they are Scott-encoded instead. They are decoded from
bytes and consumed in-script, never crossing a @Data@ boundary, so paying for a
@Data@ representation would buy nothing. Three of them are exceptions, because
they do cross one: 'PMidgardTxValidity', 'PNativeTxWitnessSetCompact' (a field of
the Q1x step validators' @FieldOpeningV1@ redeemer) and
'PNativeTxProofSourceCborV1' (a field of the field-receipt policy's
@PublishField@ redeemer). For those the @Constr 0@ shape is wire format.

The whole-transaction types sit with the component ones and are @Data@-encoded
for the same reason. The partial views and partial preimages are @Data@ too:
they are what a dispute hands across a redeemer boundary.
-}
module Midgard.FraudProofs.NativeTx.Types (
  pnativeTxVersionV1,
  PMidgardTxValidity (..),

  -- * Transaction components
  PMidgardTxInput (..),
  PMidgardCredential (..),
  PMidgardAddress (..),
  PMidgardValue (..),
  PMidgardAssets,
  PMidgardScriptLanguage (..),
  PMidgardVersionedScript (..),
  PMidgardScriptWitness,
  PMidgardTxOutput (..),
  PMidgardAddressWitness (..),
  PMidgardRedeemerPurpose (..),
  PMidgardExecutionUnits (..),
  PMidgardRedeemerWitness (..),

  -- * The whole transaction
  PMidgardTransactionBody (..),
  PMidgardTransactionWitnessSet (..),
  PMidgardTransaction (..),

  -- * Partial views
  PMidgardTransactionBodyPartialView (..),
  PMidgardTransactionWitnessSetPartialView (..),
  PMidgardTransactionPartialView (..),
  PMidgardTransactionBodyPartialPreimages (..),
  PMidgardTransactionWitnessSetPartialPreimages (..),

  -- * The compact form
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxWitnessSetCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PVerifiedMidgardNativeTxCompact (..),
  PNativeTxProofSourceCborV1 (..),
  PVerifiedMidgardTransaction (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap (PAssocMap)
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))

{- | Aiken @native_tx/types.native_tx_version_v1@ — @1@.

The only version the carriage layer accepts. Every encoder and decoder in
"Midgard.FraudProofs.NativeTx.Compact" pins it, and the version byte is inside
the transaction-id preimage, so a transaction cannot be reinterpreted under a
future version.
-}
pnativeTxVersionV1 :: forall (s :: S). Term s PInteger
pnativeTxVersionV1 = 1

{- | Aiken @native_tx/types.MidgardTxValidity@.

An operator's verdict on an L2 transaction. Tags: @TxIsValid@ 0,
@NonExistentInputUtxo@ 1, @InvalidSignature@ 2, @FailedScript@ 3, @FeeTooLow@ 4,
@UnbalancedTx@ 5.

/This is a second declaration of the same type./ Aiken declares it here and again
in @lib/midgard/ledger-state.ak@, with the same six constructors in the same
order, and the port keeps both rather than collapsing them: they have identical
@Data@ encodings and are interchangeable across that boundary, but the
duplication is the Aiken tree's and hiding it here would make the two files stop
corresponding. See 'Midgard.LedgerState.PMidgardTxValidity'.

Collapsing them is also not currently possible — @Midgard.LedgerState@ sits
above this module in the import graph, so depending on it from here would cycle.
-}
data PMidgardTxValidity (s :: S)
  = PTxIsValid
  | PNonExistentInputUtxo
  | PInvalidSignature
  | PFailedScript
  | PFeeTooLow
  | PUnbalancedTx
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTxValidity)

--------------------------------------------------------------------------------
-- Transaction components
--------------------------------------------------------------------------------

{- | Aiken @native_tx/types.MidgardTxInput@.

Note the output index is /not/ bounded by the type — the bound lives in the
encoder, which pins every input to the fixed three-byte form. See
'Midgard.FraudProofs.NativeTx.Components.pencodeFixedOutputIndex'.
-}
data PMidgardTxInput (s :: S) = PMidgardTxInput
  { ptxInput'txId :: Term s (PAsData PByteString)
  , ptxInput'outputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTxInput)

{- | Aiken @native_tx/types.MidgardCredential@ — @PubKeyCredential@ 0,
@ScriptCredential@ 1.

The constructors carry a @Midgard@ prefix that the Aiken names do not, because
the unprefixed ones are already taken by @Plutarch.LedgerApi.V3@'s
@PCredential@ and the two are not interchangeable: this one holds a raw hash and
is encoded into a Midgard address header, not into an L1 address.
-}
data PMidgardCredential (s :: S)
  = PMidgardPubKeyCredential (Term s (PAsData PByteString))
  | PMidgardScriptCredential (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardCredential)

{- | Aiken @native_tx/types.MidgardAddress@.

@protected@ is Midgard's own bit, carried in the high nibble of the address
header where L1 has nothing — a protected address cannot be spent by an ordinary
L2 transaction. The header byte is @address_type * 16 + network_id +
protected_offset@, so the bit rides alongside the network id rather than
displacing it.
-}
data PMidgardAddress (s :: S) = PMidgardAddress
  { paddress'protected :: Term s (PAsData PBool)
  , paddress'networkId :: Term s (PAsData PInteger)
  , paddress'paymentCredential :: Term s (PAsData PMidgardCredential)
  , paddress'stakeCredential :: Term s (PAsData (PMaybeData PMidgardCredential))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardAddress)

{- | Aiken's @List<Pair<ByteArray, Int>>@ of assets, keyed by /asset unit/.

A unit is the 28-byte policy id with the asset name concatenated onto it, not a
nested policy-to-tokens map. The flat keying is what makes the encoder's
grouping pass necessary: it has to rediscover the policy boundaries by scanning
for a change of prefix.

@PAssocMap@ rather than a sorted map because nothing here promises an order —
only that entries sharing a policy are adjacent.
-}
type PMidgardAssets = PAssocMap PByteString PInteger

-- | Aiken @native_tx/types.MidgardValue@.
data PMidgardValue (s :: S) = PMidgardValue
  { pvalue'lovelace :: Term s (PAsData PInteger)
  , pvalue'assets :: Term s (PAsData PMidgardAssets)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardValue)

{- | Aiken @native_tx/types.MidgardScriptLanguage@.

Constructor indices are 0, 1, 2 as declared — but the /CBOR tags/ they encode to
are 0, 3 and 128, which is a different mapping entirely. See
'Midgard.FraudProofs.NativeTx.Components.pmidgardScriptLanguageToTag'.
-}
data PMidgardScriptLanguage (s :: S)
  = PNativeCardanoScript
  | PPlutusV3Script
  | PMidgardV1Script
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardScriptLanguage)

-- | Aiken @native_tx/types.MidgardVersionedScript@.
data PMidgardVersionedScript (s :: S) = PMidgardVersionedScript
  { pversionedScript'language :: Term s (PAsData PMidgardScriptLanguage)
  , pversionedScript'scriptBytes :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardVersionedScript)

-- | Aiken @native_tx/types.MidgardScriptWitness@ — an alias, as in Aiken.
type PMidgardScriptWitness = PMidgardVersionedScript

{- | Aiken @native_tx/types.MidgardTxOutput@.

The datum is carried as raw CBOR rather than as @Data@, so an output's bytes are
fixed by the producer and are not re-derived from a decoded value.
-}
data PMidgardTxOutput (s :: S) = PMidgardTxOutput
  { ptxOutput'address :: Term s (PAsData PMidgardAddress)
  , ptxOutput'value :: Term s (PAsData PMidgardValue)
  , ptxOutput'datumCbor :: Term s (PAsData (PMaybeData PByteString))
  , ptxOutput'scriptRef :: Term s (PAsData (PMaybeData PMidgardVersionedScript))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTxOutput)

-- | Aiken @native_tx/types.MidgardAddressWitness@ — a key and its signature.
data PMidgardAddressWitness (s :: S) = PMidgardAddressWitness
  { paddressWitness'verificationKey :: Term s (PAsData PByteString)
  , paddressWitness'signature :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardAddressWitness)

{- | Aiken @native_tx/types.MidgardRedeemerPurpose@ — tags 0 through 6.

@ReceiveRedeemer@ has no L1 counterpart; it is Midgard's own purpose.
-}
data PMidgardRedeemerPurpose (s :: S)
  = PSpendRedeemer
  | PMintRedeemer
  | PCertRedeemer
  | PRewardRedeemer
  | PVoteRedeemer
  | PProposeRedeemer
  | PReceiveRedeemer
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardRedeemerPurpose)

-- | Aiken @native_tx/types.MidgardExecutionUnits@.
data PMidgardExecutionUnits (s :: S) = PMidgardExecutionUnits
  { pexecutionUnits'memory :: Term s (PAsData PInteger)
  , pexecutionUnits'steps :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardExecutionUnits)

-- | Aiken @native_tx/types.MidgardRedeemerWitness@.
data PMidgardRedeemerWitness (s :: S) = PMidgardRedeemerWitness
  { predeemerWitness'purpose :: Term s (PAsData PMidgardRedeemerPurpose)
  , predeemerWitness'index :: Term s (PAsData PInteger)
  , predeemerWitness'redeemerCbor :: Term s (PAsData PByteString)
  , predeemerWitness'executionUnits :: Term s (PAsData PMidgardExecutionUnits)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardRedeemerWitness)

--------------------------------------------------------------------------------
-- The whole transaction
--------------------------------------------------------------------------------

{- | Aiken @native_tx/types.MidgardTransactionBody@.

The twelve body fields, six of them collections. Note @mint@ is raw @Data@ and
not a typed map: it is the one field whose preimage is walked in place rather
than materialised, so a type here would force exactly the materialisation
@verify_canonical_mint_preimage_cbor@ exists to avoid. See
"Midgard.FraudProofs.NativeTx.Preimages".
-}
data PMidgardTransactionBody (s :: S) = PMidgardTransactionBody
  { pbody'inputs :: Term s (PAsData (PBuiltinList (PAsData PMidgardTxInput)))
  , pbody'referenceInputs :: Term s (PAsData (PBuiltinList (PAsData PMidgardTxInput)))
  , pbody'outputs :: Term s (PAsData (PBuiltinList (PAsData PMidgardTxOutput)))
  , pbody'fee :: Term s (PAsData PInteger)
  , pbody'validityIntervalStart :: Term s (PAsData PInteger)
  , pbody'validityIntervalEnd :: Term s (PAsData PInteger)
  , pbody'requiredObservers :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pbody'requiredSigners :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pbody'mint :: Term s PData
  , pbody'scriptIntegrityHash :: Term s (PAsData PByteString)
  , pbody'auxiliaryDataHash :: Term s (PAsData PByteString)
  , pbody'networkId :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTransactionBody)

-- | Aiken @native_tx/types.MidgardTransactionWitnessSet@.
data PMidgardTransactionWitnessSet (s :: S) = PMidgardTransactionWitnessSet
  { pwitnessSet'addrTxWits :: Term s (PAsData (PBuiltinList (PAsData PMidgardAddressWitness)))
  , pwitnessSet'scriptTxWits :: Term s (PAsData (PBuiltinList (PAsData PMidgardVersionedScript)))
  , pwitnessSet'redeemerTxWits :: Term s (PAsData (PBuiltinList (PAsData PMidgardRedeemerWitness)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTransactionWitnessSet)

{- | Aiken @native_tx/types.MidgardTransaction@.

The version is carried /inside/ the value as well as being pinned by every
encoder, because it is part of the transaction-id preimage — a transaction
cannot be reinterpreted under a later version.
-}
data PMidgardTransaction (s :: S) = PMidgardTransaction
  { ptransaction'version :: Term s (PAsData PInteger)
  , ptransaction'validity :: Term s (PAsData PMidgardTxValidity)
  , ptransaction'body :: Term s (PAsData PMidgardTransactionBody)
  , ptransaction'witnessSet :: Term s (PAsData PMidgardTransactionWitnessSet)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTransaction)

--------------------------------------------------------------------------------
-- Partial views
--------------------------------------------------------------------------------

{- | Aiken @native_tx/types.MidgardTransactionBodyPartialView@.

What a dispute can see of a body when only some fields have been revealed. The
three scalars and the two fixed hashes are always present, because the compact
body carries them in full; the six collections are @None@ until their preimage
is opened.
-}
data PMidgardTransactionBodyPartialView (s :: S) = PMidgardTransactionBodyPartialView
  { pbodyView'inputs :: Term s (PAsData (PMaybeData (PBuiltinList (PAsData PMidgardTxInput))))
  , pbodyView'referenceInputs :: Term s (PAsData (PMaybeData (PBuiltinList (PAsData PMidgardTxInput))))
  , pbodyView'outputs :: Term s (PAsData (PMaybeData (PBuiltinList (PAsData PMidgardTxOutput))))
  , pbodyView'fee :: Term s (PAsData (PMaybeData PInteger))
  , pbodyView'validityIntervalStart :: Term s (PAsData (PMaybeData PInteger))
  , pbodyView'validityIntervalEnd :: Term s (PAsData (PMaybeData PInteger))
  , pbodyView'requiredObservers :: Term s (PAsData (PMaybeData (PBuiltinList (PAsData PByteString))))
  , pbodyView'requiredSigners :: Term s (PAsData (PMaybeData (PBuiltinList (PAsData PByteString))))
  , pbodyView'mint :: Term s (PAsData (PMaybeData PData))
  , pbodyView'scriptIntegrityHash :: Term s (PAsData (PMaybeData PByteString))
  , pbodyView'auxiliaryDataHash :: Term s (PAsData (PMaybeData PByteString))
  , pbodyView'networkId :: Term s (PAsData (PMaybeData PInteger))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTransactionBodyPartialView)

-- | Aiken @native_tx/types.MidgardTransactionWitnessSetPartialView@.
data PMidgardTransactionWitnessSetPartialView (s :: S) = PMidgardTransactionWitnessSetPartialView
  { pwitnessSetView'addrTxWits :: Term s (PAsData (PMaybeData (PBuiltinList (PAsData PMidgardAddressWitness))))
  , pwitnessSetView'scriptTxWits :: Term s (PAsData (PMaybeData (PBuiltinList (PAsData PMidgardVersionedScript))))
  , pwitnessSetView'redeemerTxWits :: Term s (PAsData (PMaybeData (PBuiltinList (PAsData PMidgardRedeemerWitness))))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTransactionWitnessSetPartialView)

-- | Aiken @native_tx/types.MidgardTransactionPartialView@.
data PMidgardTransactionPartialView (s :: S) = PMidgardTransactionPartialView
  { pview'version :: Term s (PAsData (PMaybeData PInteger))
  , pview'validity :: Term s (PAsData (PMaybeData PMidgardTxValidity))
  , pview'body :: Term s (PAsData (PMaybeData PMidgardTransactionBodyPartialView))
  , pview'witnessSet :: Term s (PAsData (PMaybeData PMidgardTransactionWitnessSetPartialView))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTransactionPartialView)

{- | Aiken @native_tx/types.MidgardTransactionBodyPartialPreimages@.

The raw bytes a caller has to hand, one slot per revealable field. Only the six
collections appear — the scalars need no preimage, since the compact body
already carries them.
-}
data PMidgardTransactionBodyPartialPreimages (s :: S) = PMidgardTransactionBodyPartialPreimages
  { pbodyPreimages'inputs :: Term s (PAsData (PMaybeData PByteString))
  , pbodyPreimages'referenceInputs :: Term s (PAsData (PMaybeData PByteString))
  , pbodyPreimages'outputs :: Term s (PAsData (PMaybeData PByteString))
  , pbodyPreimages'requiredObservers :: Term s (PAsData (PMaybeData PByteString))
  , pbodyPreimages'requiredSigners :: Term s (PAsData (PMaybeData PByteString))
  , pbodyPreimages'mint :: Term s (PAsData (PMaybeData PByteString))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTransactionBodyPartialPreimages)

-- | Aiken @native_tx/types.MidgardTransactionWitnessSetPartialPreimages@.
data PMidgardTransactionWitnessSetPartialPreimages (s :: S) = PMidgardTransactionWitnessSetPartialPreimages
  { pwitnessSetPreimages'addrTxWits :: Term s (PAsData (PMaybeData PByteString))
  , pwitnessSetPreimages'scriptTxWits :: Term s (PAsData (PMaybeData PByteString))
  , pwitnessSetPreimages'redeemerTxWits :: Term s (PAsData (PMaybeData PByteString))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTransactionWitnessSetPartialPreimages)

--------------------------------------------------------------------------------
-- The compact form
--------------------------------------------------------------------------------

{- | Aiken @native_tx/types.NativeTxBodyCompact@.

A Midgard transaction body with its six variable-length fields replaced by their
hashes. This is what a fault proof carries: the fixed scalars in full, and one
32-byte commitment per collection, so the body is a bounded size no matter how
large the transaction was.

Scott-encoded, like the rest of this module: these are decoded from bytes and
consumed in-script, and never cross a @Data@ boundary.
-}
data PNativeTxBodyCompact (s :: S) = PNativeTxBodyCompact
  { pbodyCompact'spendInputsHash :: Term s PByteString
  , pbodyCompact'referenceInputsHash :: Term s PByteString
  , pbodyCompact'outputsHash :: Term s PByteString
  , pbodyCompact'fee :: Term s PInteger
  , pbodyCompact'validityIntervalStart :: Term s PInteger
  , pbodyCompact'validityIntervalEnd :: Term s PInteger
  , pbodyCompact'requiredObserversHash :: Term s PByteString
  , pbodyCompact'requiredSignersHash :: Term s PByteString
  , pbodyCompact'mintHash :: Term s PByteString
  , pbodyCompact'scriptIntegrityHash :: Term s PByteString
  , pbodyCompact'auxiliaryDataHash :: Term s PByteString
  , pbodyCompact'networkId :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PNativeTxBodyCompact)

{- | Aiken @native_tx/types.NativeTxCompact@.

The compact body, the witness set's hash, and the operator's verdict as a scalar
code. Note the verdict travels as a code here rather than as a constructor — see
'Midgard.FraudProofs.NativeTx.Codec.pvalidityToPlutusData' for the bridge.
-}
data PNativeTxCompact (s :: S) = PNativeTxCompact
  { pcompact'body :: Term s PNativeTxBodyCompact
  , pcompact'witnessSetHash :: Term s PByteString
  , pcompact'validityCode :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PNativeTxCompact)

{- | Aiken @native_tx/types.NativeTxWitnessSetCompact@ — three collection hashes.

Data-encoded, unlike its sibling compact structures. Those are produced by the
codec and consumed by the accessors inside one script and never cross a data
boundary, so Scott costs less. This one does cross: it is a field of
'Midgard.FraudProofs.FieldOpening.PFieldOpeningV1', which is a redeemer field on
the Q1x step validators, so its @Constr 0@ shape is wire format.
-}
data PNativeTxWitnessSetCompact (s :: S) = PNativeTxWitnessSetCompact
  { pwitnessSetCompact'addrTxWitsHash :: Term s (PAsData PByteString)
  , pwitnessSetCompact'scriptTxWitsHash :: Term s (PAsData PByteString)
  , pwitnessSetCompact'redeemerTxWitsHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeTxWitnessSetCompact)

{- | Aiken @native_tx/types.NativeTxFieldPreimageLengthsV1@.

The encoded size of each of the nine dynamic fields. Committing to the lengths
separately is what lets the canonical size of the full transaction be computed
from the compact form alone, without having the preimages to hand.

/The record order is not the wire order./ The declaration puts
@address_witnesses@ before @script_witnesses@; the CBOR encoder and decoder both
write and read /script before address/. They agree with each other, so the format
round-trips — but a positional port that follows the record would silently swap
the two. See the note on
'Midgard.FraudProofs.NativeTx.Compact.pencodeNativeTxFieldPreimageLengthsV1'.
-}
data PNativeTxFieldPreimageLengthsV1 (s :: S) = PNativeTxFieldPreimageLengthsV1
  { plengths'spendInputs :: Term s PInteger
  , plengths'referenceInputs :: Term s PInteger
  , plengths'outputs :: Term s PInteger
  , plengths'requiredObservers :: Term s PInteger
  , plengths'requiredSigners :: Term s PInteger
  , plengths'mint :: Term s PInteger
  , plengths'addressWitnesses :: Term s PInteger
  , plengths'scriptWitnesses :: Term s PInteger
  , plengths'redeemers :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PNativeTxFieldPreimageLengthsV1)

{- | Aiken @native_tx/types.VerifiedMidgardNativeTxCompact@.

A compact transaction that has been checked against its id. The type is the
evidence: nothing produces one except the verifiers in
"Midgard.FraudProofs.NativeTx.Compact", so a function taking this argument can
rely on the binding having been made.
-}
data PVerifiedMidgardNativeTxCompact (s :: S) = PVerifiedMidgardNativeTxCompact
  { pverified'txId :: Term s PByteString
  , pverified'version :: Term s PInteger
  , pverified'txCompact :: Term s PNativeTxCompact
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PVerifiedMidgardNativeTxCompact)

{- | Aiken @native_tx/types.NativeTxProofSourceCborV1@.

The three byte strings a V1 proof source is: the compact transaction, the
compact witness set, and the field preimage lengths.

Data-encoded, unlike the rest of the compact group, and for the same reason
'PNativeTxWitnessSetCompact' is: it is a field of the field-receipt policy's
@PublishField@ redeemer, so its @Constr 0@ shape is wire format.
-}
data PNativeTxProofSourceCborV1 (s :: S) = PNativeTxProofSourceCborV1
  { pproofSource'compactCbor :: Term s (PAsData PByteString)
  , pproofSource'witnessSetCompactCbor :: Term s (PAsData PByteString)
  , pproofSource'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeTxProofSourceCborV1)

{- | Aiken @native_tx/types.VerifiedMidgardTransaction@.

The whole-transaction counterpart of 'PVerifiedMidgardNativeTxCompact', and
evidence in the same way: only
'Midgard.FraudProofs.NativeTx.Transaction.pverifyMidgardTransactionV1' produces
one, so holding the type means the id was checked against the bytes.
-}
data PVerifiedMidgardTransaction (s :: S) = PVerifiedMidgardTransaction
  { pverifiedTx'txId :: Term s PByteString
  , pverifiedTx'transaction :: Term s PMidgardTransaction
  , pverifiedTx'compact :: Term s PNativeTxCompact
  , pverifiedTx'compactCbor :: Term s PByteString
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PVerifiedMidgardTransaction)
