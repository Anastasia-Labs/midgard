{- |
Module      : Midgard.FraudProofs.NativeTx.Transaction
Description : Plutarch port of
              @lib/midgard/fraud-proofs/native-tx/transaction.ak@.

The top of the native-transaction carriage layer: a whole Midgard transaction,
the compact form derived from it, the commitments that bind the two, and the
verifiers a dispute uses to open one field at a time.

=== The shape of every field check

@decode, then re-encode, then compare bytes@. A hash match on its own would
authenticate any preimage that happens to collide with the committed shape — but
more usefully, it would authenticate a /non-canonical spelling/ of the right
value, and the whole format rests on one value having one spelling. Re-encoding
is what closes that.

Field 5 is the single exception and it is deliberate: materialising a whole mint
map as @Data@ purely to compare bytes does not fit inside the L1 execution-memory
envelope, so 'Midgard.FraudProofs.NativeTx.Preimages.pverifyCanonicalMintPreimageCbor'
walks the bytes in place and enforces the canonical rules as it goes.

=== Field identity is positional

§4 hashes a field's preimage with no domain tag, no version prefix and no field
index mixed in. A field is the field it is because of the /slot its hash lands
in/, not because of anything inside the hash. That is why nothing here accepts a
free-standing field hash: every expected hash comes out of the tx-id-verified
compact structures.

=== Retired counted opening

@verify_midgard_transaction_field_chunk_v1@ is retained exactly because the
field-receipt policy still calls it. It authenticates the compact proof source,
then tries to open a counted-collection root from one of §4's flat field hashes.
No honestly constructed transaction can satisfy that last binding, but keeping
the literal verifier preserves the Aiken rejection boundary and its standalone
proof-source tests until the receipt path is removed or rebound.
-}
module Midgard.FraudProofs.NativeTx.Transaction (
  -- * Whole-transaction encoding
  pencodeMidgardTransactionBodyFull,
  pencodeMidgardTransactionWitnessSetFull,
  pencodeMidgardTransactionV1,
  pdecodeMidgardTransactionV1,
  pdecodeMidgardTransactionBodyData,
  pdecodeMidgardTransactionWitnessSetData,
  pverifyMidgardTransactionV1,

  -- * Compaction
  pmidgardTransactionBodyToCompact,
  pmidgardTransactionWitnessSetToCompact,
  pmidgardTransactionToCompact,
  pmidgardTransactionProofSourceV1,
  pmidgardTransactionProofCommitmentV1,

  -- * Field verification
  pverifyMidgardTransactionFieldPreimageV1,
  pverifyMidgardTransactionFieldPreimagesV1,
  pverifyMidgardTransactionFieldChunkV1,
  pverifyMidgardTransactionMintPreimageCommitmentV1,

  -- * Partial views
  ppartialViewFromCompact,
  ppartialBodyViewFromCompactAndPreimages,
  ppartialWitnessSetViewFromCompactAndPreimages,
  ppartialViewFromCompactAndPreimages,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.BoundedCollection (pverifyBoundedCollectionItem)
import Midgard.BoundedItem (PChunkProofV1 (..), pverifyChunk)
import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pdecodeCanonicalIntAt,
  pdecodeCanonicalUintAt,
  pdecodeDefiniteArrayHeaderAt,
  pdecodeDefiniteBytesAt,
  pencodeDefiniteBytes,
  pexpectH32,
  pexpectNetworkId,
  pexpectNonNegative,
  pvalidityFromCode,
  pvalidityToCode,
 )
import Midgard.FraudProofs.NativeTx.Compact (
  pdecodeNativeTxFieldPreimageLengthsV1,
  pencodeNativeTxBodyCompact,
  pencodeNativeTxCompactV1,
  pencodeNativeTxFieldPreimageLengthsV1,
  pencodeNativeTxWitnessSetCompact,
  pnativeTxIdForVersion,
  pnativeTxProofCommitmentV1,
  pverifyNativeTxProofSourceV1,
 )
import Midgard.FraudProofs.NativeTx.Preimages (
  pdecodeMidgardTxAddressWitnessesPreimageCbor,
  pdecodeMidgardTxHash28ListPreimageCbor,
  pdecodeMidgardTxInputsPreimageCbor,
  pdecodeMidgardTxMintPreimageCbor,
  pdecodeMidgardTxOutputsPreimageCbor,
  pdecodeMidgardTxRedeemerWitnessesPreimageCbor,
  pdecodeMidgardTxScriptWitnessesPreimageCbor,
  pencodeAddressWitnessPreimage,
  pencodeHash28ListPreimage,
  pencodeInputPreimage,
  pencodeMintPreimage,
  pencodeOutputPreimage,
  pencodeRedeemerWitnessPreimage,
  pencodeScriptWitnessPreimage,
  pverifyCanonicalMintPreimageCbor,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardTransaction (..),
  PMidgardTransactionBody (..),
  PMidgardTransactionBodyPartialPreimages (..),
  PMidgardTransactionBodyPartialView (..),
  PMidgardTransactionPartialView (..),
  PMidgardTransactionWitnessSet (..),
  PMidgardTransactionWitnessSetPartialPreimages (..),
  PMidgardTransactionWitnessSetPartialView (..),
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxProofSourceCborV1 (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
  PVerifiedMidgardTransaction (..),
  pnativeTxVersionV1,
 )
import Midgard.LedgerState (PItemProofV1 (..))
import Midgard.NativeTxFieldAccess (pfieldCommitment)

--------------------------------------------------------------------------------
-- Shared shapes
--------------------------------------------------------------------------------

-- | Inequality; Plutarch has @#==@ but no negated form.
(#!=) :: forall (s :: S) (a :: S -> Type). PEq a => Term s a -> Term s a -> Term s PBool
x #!= y = pnot # (x #== y)

infix 4 #!=

{- | Aiken @transaction.expect_field_preimage_hash@.

§4 plain hashing: the commitment is @blake2b_256@ over the preimage bytes
themselves, so authenticating a revealed field is one hash — no per-item leaves,
no frontier, no Merkle machinery. The decode that follows is what keeps a hash
match from authenticating a non-canonical shape.
-}
pexpectFieldPreimageHash ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
pexpectFieldPreimageHash = phoistAcyclic $
  plam $ \preimageCbor expectedHash ->
    pif
      (pfieldCommitment # preimageCbor #== expectedHash)
      preimageCbor
      perror

{- | Authenticate the bytes, then decode them.

The seven @decode_midgard_tx_*_preimage@ functions in the Aiken file are this
combinator at seven different decoders.
-}
pauthenticatedDecode ::
  forall (s :: S) (a :: S -> Type).
  Term s (PByteString :--> a) ->
  Term s (PByteString :--> PByteString :--> a)
pauthenticatedDecode decode =
  plam $ \preimageCbor expectedHash ->
    decode #$ pexpectFieldPreimageHash # preimageCbor # expectedHash

--------------------------------------------------------------------------------
-- Whole-transaction encoding
--------------------------------------------------------------------------------

{- | Aiken @transaction.encode_midgard_transaction_body_full@.

Twelve entries, with each of the six collections written as its whole §5.1
preimage wrapped in a definite byte string — so the /full/ transaction encoding
carries the field bytes, where the compact one carries only their hashes.
-}
pencodeMidgardTransactionBodyFull ::
  forall (s :: S). Term s (PMidgardTransactionBody :--> PByteString)
pencodeMidgardTransactionBodyFull = phoistAcyclic $
  plam $ \body -> P.do
    PMidgardTransactionBody
      { pbody'inputs
      , pbody'referenceInputs
      , pbody'outputs
      , pbody'fee
      , pbody'validityIntervalStart
      , pbody'validityIntervalEnd
      , pbody'requiredObservers
      , pbody'requiredSigners
      , pbody'mint
      , pbody'scriptIntegrityHash
      , pbody'auxiliaryDataHash
      , pbody'networkId
      } <-
      pmatch body
    scriptIntegrityHash <- plet (pfromData pbody'scriptIntegrityHash)
    auxiliaryDataHash <- plet (pfromData pbody'auxiliaryDataHash)
    pconstant "\x8c"
      <> (pencodeDefiniteBytes #$ pencodeInputPreimage # pfromData pbody'inputs)
      <> (pencodeDefiniteBytes #$ pencodeInputPreimage # pfromData pbody'referenceInputs)
      <> (pencodeDefiniteBytes #$ pencodeOutputPreimage # pfromData pbody'outputs)
      <> pcborInt (pexpectNonNegative # pfromData pbody'fee)
      <> pcborInt (pfromData pbody'validityIntervalStart)
      <> pcborInt (pfromData pbody'validityIntervalEnd)
      <> (pencodeDefiniteBytes #$ pencodeHash28ListPreimage #$ punwrapBytes # pfromData pbody'requiredObservers)
      <> (pencodeDefiniteBytes #$ pencodeHash28ListPreimage #$ punwrapBytes # pfromData pbody'requiredSigners)
      <> (pencodeDefiniteBytes #$ pencodeMintPreimage # pbody'mint)
      <> (pencodeDefiniteBytes #$ pexpectH32 # scriptIntegrityHash)
      <> (pencodeDefiniteBytes #$ pexpectH32 # auxiliaryDataHash)
      <> pcborInt (pexpectNetworkId # pfromData pbody'networkId)

{- | Unwraps a @Data@-encoded byte-string list.

The observer and signer fields are @List<ByteArray>@, which Aiken carries as a
@list(data)@; the preimage producer takes the unwrapped list. See the note on
@PItems@ in "Midgard.FraudProofs.NativeTx.Preimages".
-}
punwrapBytes ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PByteString) :--> PBuiltinList PByteString)
punwrapBytes = phoistAcyclic $
  pfix $ \self -> plam $ \items ->
    pelimList
      (\item rest -> pcons # pfromData item # (self # rest))
      pnil
      items

-- | The inverse of 'punwrapBytes'.
pwrapBytes ::
  forall (s :: S).
  Term s (PBuiltinList PByteString :--> PBuiltinList (PAsData PByteString))
pwrapBytes = phoistAcyclic $
  pfix $ \self -> plam $ \items ->
    pelimList
      (\item rest -> pcons # pdata item # (self # rest))
      pnil
      items

-- | Aiken @transaction.encode_midgard_transaction_witness_set_full@ — three preimages.
pencodeMidgardTransactionWitnessSetFull ::
  forall (s :: S). Term s (PMidgardTransactionWitnessSet :--> PByteString)
pencodeMidgardTransactionWitnessSetFull = phoistAcyclic $
  plam $ \witnessSet -> P.do
    PMidgardTransactionWitnessSet
      {pwitnessSet'addrTxWits, pwitnessSet'scriptTxWits, pwitnessSet'redeemerTxWits} <-
      pmatch witnessSet
    pconstant "\x83"
      <> (pencodeDefiniteBytes #$ pencodeAddressWitnessPreimage # pfromData pwitnessSet'addrTxWits)
      <> (pencodeDefiniteBytes #$ pencodeScriptWitnessPreimage # pfromData pwitnessSet'scriptTxWits)
      <> (pencodeDefiniteBytes #$ pencodeRedeemerWitnessPreimage # pfromData pwitnessSet'redeemerTxWits)

-- | Aiken @transaction.encode_midgard_transaction_for_version@.
pencodeMidgardTransactionForVersion ::
  forall (s :: S). Term s (PInteger :--> PMidgardTransaction :--> PByteString)
pencodeMidgardTransactionForVersion = phoistAcyclic $
  plam $ \expectedVersion tx -> P.do
    PMidgardTransaction
      {ptransaction'version, ptransaction'validity, ptransaction'body, ptransaction'witnessSet} <-
      pmatch tx
    version <- plet (pfromData ptransaction'version)
    pif (expectedVersion #!= pnativeTxVersionV1 #|| version #!= expectedVersion) perror $
      pconstant "\x84"
        <> pcborInt version
        <> (pencodeMidgardTransactionBodyFull # pfromData ptransaction'body)
        <> (pencodeMidgardTransactionWitnessSetFull # pfromData ptransaction'witnessSet)
        <> pcborInt (pvalidityToCode # pfromData ptransaction'validity)

-- | Aiken @transaction.encode_midgard_transaction_v1@.
pencodeMidgardTransactionV1 ::
  forall (s :: S). Term s (PMidgardTransaction :--> PByteString)
pencodeMidgardTransactionV1 = phoistAcyclic $
  plam $ \tx -> pencodeMidgardTransactionForVersion # pnativeTxVersionV1 # tx

--------------------------------------------------------------------------------
-- Compaction
--------------------------------------------------------------------------------

{- | Aiken @transaction.midgard_transaction_body_to_compact@.

§4: each of the six body commitments is a flat @blake2b_256@ over that field's
§5.1 enveloped preimage. Field identity is positional — it comes from the slot
the hash lands in, not from anything mixed into the hash.
-}
pmidgardTransactionBodyToCompact ::
  forall (s :: S). Term s (PMidgardTransactionBody :--> PNativeTxBodyCompact)
pmidgardTransactionBodyToCompact = phoistAcyclic $
  plam $ \body -> P.do
    PMidgardTransactionBody
      { pbody'inputs
      , pbody'referenceInputs
      , pbody'outputs
      , pbody'fee
      , pbody'validityIntervalStart
      , pbody'validityIntervalEnd
      , pbody'requiredObservers
      , pbody'requiredSigners
      , pbody'mint
      , pbody'scriptIntegrityHash
      , pbody'auxiliaryDataHash
      , pbody'networkId
      } <-
      pmatch body
    pcon
      ( PNativeTxBodyCompact
          { pbodyCompact'spendInputsHash =
              pfieldCommitment #$ pencodeInputPreimage # pfromData pbody'inputs
          , pbodyCompact'referenceInputsHash =
              pfieldCommitment #$ pencodeInputPreimage # pfromData pbody'referenceInputs
          , pbodyCompact'outputsHash =
              pfieldCommitment #$ pencodeOutputPreimage # pfromData pbody'outputs
          , pbodyCompact'fee = pfromData pbody'fee
          , pbodyCompact'validityIntervalStart = pfromData pbody'validityIntervalStart
          , pbodyCompact'validityIntervalEnd = pfromData pbody'validityIntervalEnd
          , pbodyCompact'requiredObserversHash =
              pfieldCommitment #$ pencodeHash28ListPreimage #$ punwrapBytes # pfromData pbody'requiredObservers
          , pbodyCompact'requiredSignersHash =
              pfieldCommitment #$ pencodeHash28ListPreimage #$ punwrapBytes # pfromData pbody'requiredSigners
          , pbodyCompact'mintHash = pfieldCommitment #$ pencodeMintPreimage # pbody'mint
          , pbodyCompact'scriptIntegrityHash = pfromData pbody'scriptIntegrityHash
          , pbodyCompact'auxiliaryDataHash = pfromData pbody'auxiliaryDataHash
          , pbodyCompact'networkId = pfromData pbody'networkId
          }
      )

-- | Aiken @transaction.midgard_transaction_witness_set_to_compact@.
pmidgardTransactionWitnessSetToCompact ::
  forall (s :: S).
  Term s (PMidgardTransactionWitnessSet :--> PNativeTxWitnessSetCompact)
pmidgardTransactionWitnessSetToCompact = phoistAcyclic $
  plam $ \witnessSet -> P.do
    PMidgardTransactionWitnessSet
      {pwitnessSet'addrTxWits, pwitnessSet'scriptTxWits, pwitnessSet'redeemerTxWits} <-
      pmatch witnessSet
    pcon
      ( PNativeTxWitnessSetCompact
          { pwitnessSetCompact'addrTxWitsHash =
              pdata (pfieldCommitment #$ pencodeAddressWitnessPreimage # pfromData pwitnessSet'addrTxWits)
          , pwitnessSetCompact'scriptTxWitsHash =
              pdata (pfieldCommitment #$ pencodeScriptWitnessPreimage # pfromData pwitnessSet'scriptTxWits)
          , pwitnessSetCompact'redeemerTxWitsHash =
              pdata (pfieldCommitment #$ pencodeRedeemerWitnessPreimage # pfromData pwitnessSet'redeemerTxWits)
          }
      )

-- | Aiken @transaction.midgard_transaction_to_compact@.
pmidgardTransactionToCompact ::
  forall (s :: S). Term s (PMidgardTransaction :--> PNativeTxCompact)
pmidgardTransactionToCompact = phoistAcyclic $
  plam $ \tx -> P.do
    PMidgardTransaction {ptransaction'validity, ptransaction'body, ptransaction'witnessSet} <-
      pmatch tx
    pcon
      ( PNativeTxCompact
          { pcompact'body = pmidgardTransactionBodyToCompact # pfromData ptransaction'body
          , pcompact'witnessSetHash =
              pblake2b_256
                #$ pencodeNativeTxWitnessSetCompact
                #$ pmidgardTransactionWitnessSetToCompact
                # pfromData ptransaction'witnessSet
          , pcompact'validityCode = pvalidityToCode # pfromData ptransaction'validity
          }
      )

{- | Aiken @transaction.midgard_transaction_proof_source_v1@.

The three byte strings a proof carries. The lengths are computed by encoding
each field and measuring it, which is why this is the producer side — a verifier
gets them from the wire.
-}
pmidgardTransactionProofSourceV1 ::
  forall (s :: S). Term s (PMidgardTransaction :--> PNativeTxProofSourceCborV1)
pmidgardTransactionProofSourceV1 = phoistAcyclic $
  plam $ \tx -> P.do
    PMidgardTransaction {ptransaction'version, ptransaction'body, ptransaction'witnessSet} <-
      pmatch tx
    pif (pfromData ptransaction'version #!= pnativeTxVersionV1) perror $ P.do
      body <- plet (pfromData ptransaction'body)
      witnessSet <- plet (pfromData ptransaction'witnessSet)
      PMidgardTransactionBody
        { pbody'inputs
        , pbody'referenceInputs
        , pbody'outputs
        , pbody'requiredObservers
        , pbody'requiredSigners
        , pbody'mint
        } <-
        pmatch body
      PMidgardTransactionWitnessSet
        {pwitnessSet'addrTxWits, pwitnessSet'scriptTxWits, pwitnessSet'redeemerTxWits} <-
        pmatch witnessSet
      witnessSetCompactCbor <-
        plet (pencodeNativeTxWitnessSetCompact #$ pmidgardTransactionWitnessSetToCompact # witnessSet)
      pcon
        ( PNativeTxProofSourceCborV1
            { pproofSource'compactCbor =
                pdata (pencodeNativeTxCompactV1 #$ pmidgardTransactionToCompact # tx)
            , pproofSource'witnessSetCompactCbor = pdata witnessSetCompactCbor
            , pproofSource'fieldPreimageLengthsCbor =
                pdata $
                  pencodeNativeTxFieldPreimageLengthsV1
                  #$ pcon
                  ( PNativeTxFieldPreimageLengthsV1
                      { plengths'spendInputs =
                          plengthBS #$ pencodeInputPreimage # pfromData pbody'inputs
                      , plengths'referenceInputs =
                          plengthBS #$ pencodeInputPreimage # pfromData pbody'referenceInputs
                      , plengths'outputs =
                          plengthBS #$ pencodeOutputPreimage # pfromData pbody'outputs
                      , plengths'requiredObservers =
                          plengthBS #$ pencodeHash28ListPreimage #$ punwrapBytes # pfromData pbody'requiredObservers
                      , plengths'requiredSigners =
                          plengthBS #$ pencodeHash28ListPreimage #$ punwrapBytes # pfromData pbody'requiredSigners
                      , plengths'mint = plengthBS #$ pencodeMintPreimage # pbody'mint
                      , plengths'addressWitnesses =
                          plengthBS #$ pencodeAddressWitnessPreimage # pfromData pwitnessSet'addrTxWits
                      , plengths'scriptWitnesses =
                          plengthBS #$ pencodeScriptWitnessPreimage # pfromData pwitnessSet'scriptTxWits
                      , plengths'redeemers =
                          plengthBS #$ pencodeRedeemerWitnessPreimage # pfromData pwitnessSet'redeemerTxWits
                      }
                  )
            }
        )

-- | Aiken @transaction.midgard_transaction_proof_commitment_v1@.
pmidgardTransactionProofCommitmentV1 ::
  forall (s :: S). Term s (PMidgardTransaction :--> PByteString)
pmidgardTransactionProofCommitmentV1 = phoistAcyclic $
  plam $ \tx -> P.do
    PNativeTxProofSourceCborV1
      { pproofSource'compactCbor
      , pproofSource'witnessSetCompactCbor
      , pproofSource'fieldPreimageLengthsCbor
      } <-
      pmatch (pmidgardTransactionProofSourceV1 # tx)
    pnativeTxProofCommitmentV1
      # pfromData pproofSource'compactCbor
      # pfromData pproofSource'witnessSetCompactCbor
      # pfromData pproofSource'fieldPreimageLengthsCbor

--------------------------------------------------------------------------------
-- Field verification
--------------------------------------------------------------------------------

{- | Aiken @transaction.verify_midgard_transaction_field_preimage_against_source_v1@.

Field indices are consensus ordered: 0 spend inputs, 1 reference inputs,
2 outputs, 3 observers, 4 signers, 5 mint, 6 script witnesses, 7 address
witnesses, 8 redeemers. Note 6 and 7 are /script before address/, the same
transposition the lengths record carries.

Every branch checks the declared length first, then decodes against the
positional hash, then re-encodes and compares bytes. An out-of-range index
returns @False@ rather than failing — this is a predicate, and the caller
decides.
-}
pverifyMidgardTransactionFieldPreimageAgainstSourceV1 ::
  forall (s :: S).
  Term
    s
    ( PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact
        :--> PNativeTxFieldPreimageLengthsV1
        :--> PInteger
        :--> PByteString
        :--> PBool
    )
pverifyMidgardTransactionFieldPreimageAgainstSourceV1 = phoistAcyclic $
  plam $ \verified witnessSet fieldLengths fieldIndex preimageCbor -> P.do
    PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch verified
    PNativeTxCompact {pcompact'body} <- pmatch pverified'txCompact
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash
      , pbodyCompact'referenceInputsHash
      , pbodyCompact'outputsHash
      , pbodyCompact'requiredObserversHash
      , pbodyCompact'requiredSignersHash
      , pbodyCompact'mintHash
      } <-
      pmatch pcompact'body
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash
      , pwitnessSetCompact'scriptTxWitsHash
      , pwitnessSetCompact'redeemerTxWitsHash
      } <-
      pmatch witnessSet
    PNativeTxFieldPreimageLengthsV1
      { plengths'spendInputs
      , plengths'referenceInputs
      , plengths'outputs
      , plengths'requiredObservers
      , plengths'requiredSigners
      , plengths'mint
      , plengths'addressWitnesses
      , plengths'scriptWitnesses
      , plengths'redeemers
      } <-
      pmatch fieldLengths
    len <- plet (plengthBS # preimageCbor)
    let sized declared body = pif (len #!= declared) perror (body ())
        inputsField declared hash =
          sized declared $ \() ->
            (pencodeInputPreimage #$ pauthenticatedDecode pdecodeMidgardTxInputsPreimageCbor # preimageCbor # hash)
              #== preimageCbor
    pif (fieldIndex #== 0) (inputsField plengths'spendInputs pbodyCompact'spendInputsHash) $
      pif (fieldIndex #== 1) (inputsField plengths'referenceInputs pbodyCompact'referenceInputsHash) $
        pif
          (fieldIndex #== 2)
          ( sized plengths'outputs $ \() ->
              (pencodeOutputPreimage #$ pauthenticatedDecode pdecodeMidgardTxOutputsPreimageCbor # preimageCbor # pbodyCompact'outputsHash)
                #== preimageCbor
          )
          $ pif
            (fieldIndex #== 3)
            (hash28Field sized plengths'requiredObservers pbodyCompact'requiredObserversHash preimageCbor)
            $ pif
              (fieldIndex #== 4)
              (hash28Field sized plengths'requiredSigners pbodyCompact'requiredSignersHash preimageCbor)
              $ pif
                (fieldIndex #== 5)
                ( sized plengths'mint $ \() ->
                    pverifyCanonicalMintPreimageCbor # preimageCbor # pbodyCompact'mintHash
                )
                $ pif
                  (fieldIndex #== 6)
                  ( sized plengths'scriptWitnesses $ \() ->
                      (pencodeScriptWitnessPreimage #$ pauthenticatedDecode pdecodeMidgardTxScriptWitnessesPreimageCbor # preimageCbor # pfromData pwitnessSetCompact'scriptTxWitsHash)
                        #== preimageCbor
                  )
                  $ pif
                    (fieldIndex #== 7)
                    ( sized plengths'addressWitnesses $ \() ->
                        (pencodeAddressWitnessPreimage #$ pauthenticatedDecode pdecodeMidgardTxAddressWitnessesPreimageCbor # preimageCbor # pfromData pwitnessSetCompact'addrTxWitsHash)
                          #== preimageCbor
                    )
                    $ pif
                      (fieldIndex #== 8)
                      ( sized plengths'redeemers $ \() ->
                          (pencodeRedeemerWitnessPreimage #$ pauthenticatedDecode pdecodeMidgardTxRedeemerWitnessesPreimageCbor # preimageCbor # pfromData pwitnessSetCompact'redeemerTxWitsHash)
                            #== preimageCbor
                      )
                      (pconstant False)
  where
    hash28Field sized declared hash preimageCbor =
      sized declared $ \() ->
        ( pencodeHash28ListPreimage
            #$ pauthenticatedDecode pdecodeMidgardTxHash28ListPreimageCbor
            # preimageCbor
            # hash
        )
          #== preimageCbor

{- | Aiken @transaction.verify_midgard_transaction_field_preimage_v1@.

Verifies one independently revealed V1 field against the compact transaction
source.

/Not a field-access idiom./ It takes a whole revealed preimage and answers
yes or no about it; it hands out no view and no item, and it never accepts a
free-standing field hash — the expected hashes come from the tx-id-verified
compact structures. The Aiken tree records that it has no production caller:
what binds it is the §5/§9.1 conformance suite, which needs exactly this
decode-and-re-encode round trip to pin the canonical spelling of all nine
fields.
-}
pverifyMidgardTransactionFieldPreimageV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PBool
    )
pverifyMidgardTransactionFieldPreimageV1 = phoistAcyclic $
  plam $ \transactionId transactionCommitment compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor fieldIndex preimageCbor -> P.do
    PPair verified witnessSet <-
      pmatch
        ( pverifyNativeTxProofSourceV1
            # transactionId
            # compactCbor
            # witnessSetCompactCbor
            # fieldPreimageLengthsCbor
        )
    pif
      ( pnativeTxProofCommitmentV1
          # compactCbor
          # witnessSetCompactCbor
          # fieldPreimageLengthsCbor
          #!= transactionCommitment
      )
      perror
      ( pverifyMidgardTransactionFieldPreimageAgainstSourceV1
          # verified
          # witnessSet
          # (pdecodeNativeTxFieldPreimageLengthsV1 # fieldPreimageLengthsCbor)
          # fieldIndex
          # preimageCbor
      )

{- | Aiken @transaction.verify_midgard_transaction_field_chunk_v1@.

This is the retired counted opening used by @PublishField@. The proof source and
transaction commitment are authenticated first, the collection and chunk
coordinates must agree, then the collection proof opens the positional compact
field hash and the chunk proof opens the item commitment carried by that proof.
-}
pverifyMidgardTransactionFieldChunkV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PItemProofV1
        :--> PChunkProofV1
        :--> PBool
    )
pverifyMidgardTransactionFieldChunkV1 = phoistAcyclic $
  plam $ \transactionId transactionCommitment compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor collectionProof proof -> P.do
    PPair verified witnessSet <-
      pmatch
        ( pverifyNativeTxProofSourceV1
            # transactionId
            # compactCbor
            # witnessSetCompactCbor
            # fieldPreimageLengthsCbor
        )
    PItemProofV1
      { pitemProof'fieldIndex
      , pitemProof'itemIndex
      , pitemProof'itemLength
      , pitemProof'itemCommitment
      } <-
      pmatch collectionProof
    PChunkProofV1
      { pchunkProof'fieldIndex
      , pchunkProof'itemIndex
      , pchunkProof'totalLength
      } <-
      pmatch proof
    ( pnativeTxProofCommitmentV1
        # compactCbor
        # witnessSetCompactCbor
        # fieldPreimageLengthsCbor
        #== transactionCommitment
      )
      #&& (pitemProof'fieldIndex #== pchunkProof'fieldIndex)
      #&& (pitemProof'itemIndex #== pchunkProof'itemIndex)
      #&& (pitemProof'itemLength #== pchunkProof'totalLength)
      #&& ( pverifyBoundedCollectionItem
              # ( pretiredCountedFieldCommitmentV1
                    # verified
                    # witnessSet
                    # pfromData pchunkProof'fieldIndex
                )
              # collectionProof
          )
      #&& (pverifyChunk # pfromData pitemProof'itemCommitment # proof)

-- | Aiken @transaction.retired_counted_field_commitment_v1@.
pretiredCountedFieldCommitmentV1 ::
  forall (s :: S).
  Term
    s
    ( PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact
        :--> PInteger
        :--> PByteString
    )
pretiredCountedFieldCommitmentV1 = phoistAcyclic $
  plam $ \verified witnessSet fieldIndex -> P.do
    PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch verified
    PNativeTxCompact {pcompact'body} <- pmatch pverified'txCompact
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash
      , pbodyCompact'referenceInputsHash
      , pbodyCompact'outputsHash
      , pbodyCompact'requiredObserversHash
      , pbodyCompact'requiredSignersHash
      , pbodyCompact'mintHash
      } <-
      pmatch pcompact'body
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash
      , pwitnessSetCompact'scriptTxWitsHash
      , pwitnessSetCompact'redeemerTxWitsHash
      } <-
      pmatch witnessSet
    pif (fieldIndex #== 0) pbodyCompact'spendInputsHash $
      pif (fieldIndex #== 1) pbodyCompact'referenceInputsHash $
        pif (fieldIndex #== 2) pbodyCompact'outputsHash $
          pif (fieldIndex #== 3) pbodyCompact'requiredObserversHash $
            pif (fieldIndex #== 4) pbodyCompact'requiredSignersHash $
              pif (fieldIndex #== 5) pbodyCompact'mintHash $
                pif (fieldIndex #== 6) (pfromData pwitnessSetCompact'scriptTxWitsHash) $
                  pif (fieldIndex #== 7) (pfromData pwitnessSetCompact'addrTxWitsHash) $
                    pif (fieldIndex #== 8) (pfromData pwitnessSetCompact'redeemerTxWitsHash) perror

{- | Aiken @transaction.verify_midgard_transaction_mint_preimage_commitment_v1@.

Authenticates the bounded mint field against the V1 compact source without
recursively validating the entire map in one L1 step. Callers must still stream
the canonical CBOR through a bounded state machine before treating any mint
semantics as valid.
-}
pverifyMidgardTransactionMintPreimageCommitmentV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
pverifyMidgardTransactionMintPreimageCommitmentV1 = phoistAcyclic $
  plam $ \transactionId transactionCommitment compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor preimageCbor -> P.do
    PPair verified _ <-
      pmatch
        ( pverifyNativeTxProofSourceV1
            # transactionId
            # compactCbor
            # witnessSetCompactCbor
            # fieldPreimageLengthsCbor
        )
    PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch verified
    PNativeTxCompact {pcompact'body} <- pmatch pverified'txCompact
    PNativeTxBodyCompact {pbodyCompact'mintHash} <- pmatch pcompact'body
    PNativeTxFieldPreimageLengthsV1 {plengths'mint} <-
      pmatch (pdecodeNativeTxFieldPreimageLengthsV1 # fieldPreimageLengthsCbor)
    ( pnativeTxProofCommitmentV1
        # compactCbor
        # witnessSetCompactCbor
        # fieldPreimageLengthsCbor
        #== transactionCommitment
      )
      #&& plengthBS
      # preimageCbor
      #== plengths'mint
      #&& pverifyCanonicalMintPreimageCbor
      # preimageCbor
      # pbodyCompact'mintHash

{- | Aiken @transaction.verify_midgard_transaction_field_preimages_v1@.

The whole forced-order bundle, decoding the compact proof source only once.
Exactly nine preimages, one per consensus-ordered field, and the list must be
exactly nine long — a short or long list fails rather than verifying a prefix.
-}
pverifyMidgardTransactionFieldPreimagesV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBuiltinList PByteString
        :--> PBool
    )
pverifyMidgardTransactionFieldPreimagesV1 = phoistAcyclic $
  plam $ \transactionId transactionCommitment compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor preimages -> P.do
    PPair verified witnessSet <-
      pmatch
        ( pverifyNativeTxProofSourceV1
            # transactionId
            # compactCbor
            # witnessSetCompactCbor
            # fieldPreimageLengthsCbor
        )
    fieldLengths <- plet (pdecodeNativeTxFieldPreimageLengthsV1 # fieldPreimageLengthsCbor)
    pif
      ( pnativeTxProofCommitmentV1
          # compactCbor
          # witnessSetCompactCbor
          # fieldPreimageLengthsCbor
          #!= transactionCommitment
      )
      perror
      $ P.do
        -- `expect [a, .., i] = preimages` — exactly nine, no more and no fewer.
        items <- plet (pninePreimages # preimages)
        pverifyEachField # verified # witnessSet # fieldLengths # items # 0

{- | Checks the bundle is exactly nine items and hands it back.

Written as a length check rather than a nine-way destructure because the caller
then walks it positionally anyway.
-}
pninePreimages ::
  forall (s :: S).
  Term s (PBuiltinList PByteString :--> PBuiltinList PByteString)
pninePreimages = phoistAcyclic $
  plam $ \preimages ->
    pif (plength # preimages #== 9) preimages perror

-- | Walks the nine-item bundle, checking each against its own field index.
pverifyEachField ::
  forall (s :: S).
  Term
    s
    ( PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact
        :--> PNativeTxFieldPreimageLengthsV1
        :--> PBuiltinList PByteString
        :--> PInteger
        :--> PBool
    )
pverifyEachField = phoistAcyclic $
  pfix $ \self -> plam $ \verified witnessSet fieldLengths items fieldIndex ->
    pelimList
      ( \item rest ->
          pverifyMidgardTransactionFieldPreimageAgainstSourceV1
            # verified
            # witnessSet
            # fieldLengths
            # fieldIndex
            # item
            #&& self
            # verified
            # witnessSet
            # fieldLengths
            # rest
            # (fieldIndex + 1)
      )
      (pconstant True)
      items

--------------------------------------------------------------------------------
-- Whole-transaction decoding
--------------------------------------------------------------------------------

-- | Aiken @transaction.decode_midgard_transaction_body_data@.
pdecodeMidgardTransactionBodyData ::
  forall (s :: S). Term s (PData :--> PMidgardTransactionBody)
pdecodeMidgardTransactionBodyData = phoistAcyclic $
  plam $ \dat ->
    ptake12 (pasList # dat) $
      \d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 ->
        pmkBody
          (pasByteStr # d0)
          (pasByteStr # d1)
          (pasByteStr # d2)
          (pasInt # d3)
          (pasInt # d4)
          (pasInt # d5)
          (pasByteStr # d6)
          (pasByteStr # d7)
          (pasByteStr # d8)
          (pasByteStr # d9)
          (pasByteStr # d10)
          (pasInt # d11)
          False

{- | Builds a body from the twelve preimage byte strings and scalars.

@checked@ is the difference between the two decoders: the @Data@ one takes the
scalars as they come, while the byte one runs them through the same @expect@s
the encoder does.
-}
pmkBody ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Bool ->
  Term s PMidgardTransactionBody
pmkBody inputs referenceInputs outputs fee vstart vend observers signers mint sih adh networkId checked =
  pcon
    ( PMidgardTransactionBody
        { pbody'inputs = pdata (pdecodeMidgardTxInputsPreimageCbor # inputs)
        , pbody'referenceInputs = pdata (pdecodeMidgardTxInputsPreimageCbor # referenceInputs)
        , pbody'outputs = pdata (pdecodeMidgardTxOutputsPreimageCbor # outputs)
        , pbody'fee = pdata (guardNonNegative fee)
        , pbody'validityIntervalStart = pdata vstart
        , pbody'validityIntervalEnd = pdata vend
        , pbody'requiredObservers =
            pdata (pwrapBytes #$ pdecodeMidgardTxHash28ListPreimageCbor # observers)
        , pbody'requiredSigners =
            pdata (pwrapBytes #$ pdecodeMidgardTxHash28ListPreimageCbor # signers)
        , pbody'mint = pdecodeMidgardTxMintPreimageCbor # mint
        , pbody'scriptIntegrityHash = pdata (guardH32 sih)
        , pbody'auxiliaryDataHash = pdata (guardH32 adh)
        , pbody'networkId = pdata (guardNetworkId networkId)
        }
    )
  where
    guardNonNegative x = if checked then pexpectNonNegative # x else x
    guardH32 x = if checked then pexpectH32 # x else x
    guardNetworkId x = if checked then pexpectNetworkId # x else x

-- | Aiken @transaction.decode_midgard_transaction_witness_set_data@.
pdecodeMidgardTransactionWitnessSetData ::
  forall (s :: S). Term s (PData :--> PMidgardTransactionWitnessSet)
pdecodeMidgardTransactionWitnessSetData = phoistAcyclic $
  plam $ \dat ->
    ptake3 (pasList # dat) $ \d0 d1 d2 ->
      pmkWitnessSet (pasByteStr # d0) (pasByteStr # d1) (pasByteStr # d2)

-- | Builds a witness set from its three preimage byte strings.
pmkWitnessSet ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PMidgardTransactionWitnessSet
pmkWitnessSet addrTxWits scriptTxWits redeemerTxWits =
  pcon
    ( PMidgardTransactionWitnessSet
        { pwitnessSet'addrTxWits =
            pdata (pdecodeMidgardTxAddressWitnessesPreimageCbor # addrTxWits)
        , pwitnessSet'scriptTxWits =
            pdata (pdecodeMidgardTxScriptWitnessesPreimageCbor # scriptTxWits)
        , pwitnessSet'redeemerTxWits =
            pdata (pdecodeMidgardTxRedeemerWitnessesPreimageCbor # redeemerTxWits)
        }
    )

-- | Aiken @transaction.decode_midgard_transaction_body_at@.
pdecodeMidgardTransactionBodyAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PMidgardTransactionBody)
pdecodeMidgardTransactionBodyAt = phoistAcyclic $
  plam $ \txCbor offset -> P.do
    PPair o0 fieldCount <- pmatch (pdecodeDefiniteArrayHeaderAt # txCbor # offset)
    pif (fieldCount #!= 12) perror $ P.do
      PPair o1 inputs <- pmatch (pdecodeDefiniteBytesAt # txCbor # o0)
      PPair o2 referenceInputs <- pmatch (pdecodeDefiniteBytesAt # txCbor # o1)
      PPair o3 outputs <- pmatch (pdecodeDefiniteBytesAt # txCbor # o2)
      PPair o4 fee <- pmatch (pdecodeCanonicalUintAt # txCbor # o3)
      PPair o5 vstart <- pmatch (pdecodeCanonicalIntAt # txCbor # o4)
      PPair o6 vend <- pmatch (pdecodeCanonicalIntAt # txCbor # o5)
      PPair o7 observers <- pmatch (pdecodeDefiniteBytesAt # txCbor # o6)
      PPair o8 signers <- pmatch (pdecodeDefiniteBytesAt # txCbor # o7)
      PPair o9 mint <- pmatch (pdecodeDefiniteBytesAt # txCbor # o8)
      PPair o10 sih <- pmatch (pdecodeDefiniteBytesAt # txCbor # o9)
      PPair o11 adh <- pmatch (pdecodeDefiniteBytesAt # txCbor # o10)
      PPair o12 networkId <- pmatch (pdecodeCanonicalUintAt # txCbor # o11)
      pcon
        ( PPair
            o12
            (pmkBody inputs referenceInputs outputs fee vstart vend observers signers mint sih adh networkId True)
        )

-- | Aiken @transaction.decode_midgard_transaction_witness_set_at@.
pdecodeMidgardTransactionWitnessSetAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PMidgardTransactionWitnessSet)
pdecodeMidgardTransactionWitnessSetAt = phoistAcyclic $
  plam $ \txCbor offset -> P.do
    PPair o0 fieldCount <- pmatch (pdecodeDefiniteArrayHeaderAt # txCbor # offset)
    pif (fieldCount #!= 3) perror $ P.do
      PPair o1 addrTxWits <- pmatch (pdecodeDefiniteBytesAt # txCbor # o0)
      PPair o2 scriptTxWits <- pmatch (pdecodeDefiniteBytesAt # txCbor # o1)
      PPair o3 redeemerTxWits <- pmatch (pdecodeDefiniteBytesAt # txCbor # o2)
      pcon (PPair o3 (pmkWitnessSet addrTxWits scriptTxWits redeemerTxWits))

{- | Aiken @transaction.decode_midgard_transaction_for_version@.

Note the closing @expect encode(decoded) == tx_cbor@: the whole-transaction
decoder re-encodes too, so the canonical spelling is pinned at this level and
not only field by field.
-}
pdecodeMidgardTransactionForVersion ::
  forall (s :: S). Term s (PInteger :--> PByteString :--> PMidgardTransaction)
pdecodeMidgardTransactionForVersion = phoistAcyclic $
  plam $ \expectedVersion txCbor -> P.do
    PPair o0 fieldCount <- pmatch (pdecodeDefiniteArrayHeaderAt # txCbor # 0)
    pif (fieldCount #!= 4 #|| expectedVersion #!= pnativeTxVersionV1) perror $ P.do
      PPair o1 version <- pmatch (pdecodeCanonicalUintAt # txCbor # o0)
      pif (version #!= expectedVersion) perror $ P.do
        PPair o2 body <- pmatch (pdecodeMidgardTransactionBodyAt # txCbor # o1)
        PPair o3 witnessSet <- pmatch (pdecodeMidgardTransactionWitnessSetAt # txCbor # o2)
        PPair o4 validityCode <- pmatch (pdecodeCanonicalUintAt # txCbor # o3)
        pif (o4 #!= plengthBS # txCbor) perror $ P.do
          transaction <-
            plet
              ( pcon
                  ( PMidgardTransaction
                      { ptransaction'version = pdata version
                      , ptransaction'validity = pdata (pvalidityFromCode # validityCode)
                      , ptransaction'body = pdata body
                      , ptransaction'witnessSet = pdata witnessSet
                      }
                  )
              )
          pif
            ((pencodeMidgardTransactionForVersion # expectedVersion # transaction) #== txCbor)
            transaction
            perror

-- | Aiken @transaction.decode_midgard_transaction_v1@.
pdecodeMidgardTransactionV1 ::
  forall (s :: S). Term s (PByteString :--> PMidgardTransaction)
pdecodeMidgardTransactionV1 = phoistAcyclic $
  plam $ \txCbor -> pdecodeMidgardTransactionForVersion # pnativeTxVersionV1 # txCbor

{- | Aiken @transaction.verify_midgard_transaction_v1@.

Decode, compact, and check the transaction id is the one this body hashes to.
The returned evidence carries the compact form and its bytes, so the caller does
not recompute them.
-}
pverifyMidgardTransactionV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PVerifiedMidgardTransaction)
pverifyMidgardTransactionV1 = phoistAcyclic $
  plam $ \txId txCbor -> P.do
    tx <- plet (pdecodeMidgardTransactionForVersion # pnativeTxVersionV1 # txCbor)
    compact <- plet (pmidgardTransactionToCompact # tx)
    PNativeTxCompact {pcompact'body} <- pmatch compact
    pif
      ( (pnativeTxIdForVersion # pnativeTxVersionV1 #$ pencodeNativeTxBodyCompact # pcompact'body)
          #!= txId
      )
      perror
      ( pcon
          ( PVerifiedMidgardTransaction
              { pverifiedTx'txId = txId
              , pverifiedTx'transaction = tx
              , pverifiedTx'compact = compact
              , pverifiedTx'compactCbor = pencodeNativeTxCompactV1 # compact
              }
          )
      )

--------------------------------------------------------------------------------
-- Partial views
--------------------------------------------------------------------------------

{- | Aiken @transaction.partial_view_from_compact@.

What is visible with nothing revealed: the version, the verdict, and the five
scalar body fields the compact form carries in full. All six collections are
@None@.
-}
ppartialViewFromCompact ::
  forall (s :: S).
  Term s (PVerifiedMidgardNativeTxCompact :--> PMidgardTransactionPartialView)
ppartialViewFromCompact = phoistAcyclic $
  plam $ \compact -> P.do
    PVerifiedMidgardNativeTxCompact {pverified'version, pverified'txCompact} <- pmatch compact
    PNativeTxCompact {pcompact'body, pcompact'validityCode} <- pmatch pverified'txCompact
    PNativeTxBodyCompact
      { pbodyCompact'fee
      , pbodyCompact'validityIntervalStart
      , pbodyCompact'validityIntervalEnd
      , pbodyCompact'scriptIntegrityHash
      , pbodyCompact'auxiliaryDataHash
      , pbodyCompact'networkId
      } <-
      pmatch pcompact'body
    pmkView
      pverified'version
      pcompact'validityCode
      ( pcon
          ( PMidgardTransactionBodyPartialView
              { pbodyView'inputs = pdata (pcon PDNothing)
              , pbodyView'referenceInputs = pdata (pcon PDNothing)
              , pbodyView'outputs = pdata (pcon PDNothing)
              , pbodyView'fee = pdata (pjust pbodyCompact'fee)
              , pbodyView'validityIntervalStart = pdata (pjust pbodyCompact'validityIntervalStart)
              , pbodyView'validityIntervalEnd = pdata (pjust pbodyCompact'validityIntervalEnd)
              , pbodyView'requiredObservers = pdata (pcon PDNothing)
              , pbodyView'requiredSigners = pdata (pcon PDNothing)
              , pbodyView'mint = pdata (pcon PDNothing)
              , pbodyView'scriptIntegrityHash = pdata (pjust pbodyCompact'scriptIntegrityHash)
              , pbodyView'auxiliaryDataHash = pdata (pjust pbodyCompact'auxiliaryDataHash)
              , pbodyView'networkId = pdata (pjust pbodyCompact'networkId)
              }
          )
      )
      (pcon PDNothing)

-- | @Some x@, as a 'PMaybeData'.
pjust :: forall (s :: S) (a :: S -> Type). PIsData a => Term s a -> Term s (PMaybeData a)
pjust = pcon . PDJust . pdata

-- | The three-slot view constructor, shared by the two view producers.
pmkView ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PMidgardTransactionBodyPartialView ->
  Term s (PMaybeData PMidgardTransactionWitnessSetPartialView) ->
  Term s PMidgardTransactionPartialView
pmkView version validityCode body witnessSetView =
  pcon
    ( PMidgardTransactionPartialView
        { pview'version = pdata (pjust version)
        , pview'validity = pdata (pjust (pvalidityFromCode # validityCode))
        , pview'body = pdata (pjust body)
        , pview'witnessSet = pdata witnessSetView
        }
    )

{- | Authenticates an optional preimage and decodes it, or stays @None@.

The seven @decode_optional_*_preimage@ functions in the Aiken file. Note the
hash is only consulted when a preimage is actually present — an unrevealed field
carries no obligation.
-}
pdecodeOptional ::
  forall (s :: S) (a :: S -> Type).
  PIsData a =>
  Term s (PByteString :--> a) ->
  Term s (PMaybeData PByteString) ->
  Term s PByteString ->
  Term s (PMaybeData a)
pdecodeOptional decode preimageCbor expectedHash =
  pmatch preimageCbor $ \case
    PDJust bytes ->
      pjust (pauthenticatedDecode decode # pfromData bytes # expectedHash)
    PDNothing -> pcon PDNothing

-- | Aiken @transaction.partial_body_view_from_compact_and_preimages@.
ppartialBodyViewFromCompactAndPreimages ::
  forall (s :: S).
  Term
    s
    ( PNativeTxBodyCompact
        :--> PMidgardTransactionBodyPartialPreimages
        :--> PMidgardTransactionBodyPartialView
    )
ppartialBodyViewFromCompactAndPreimages = phoistAcyclic $
  plam $ \body preimages -> P.do
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash
      , pbodyCompact'referenceInputsHash
      , pbodyCompact'outputsHash
      , pbodyCompact'fee
      , pbodyCompact'validityIntervalStart
      , pbodyCompact'validityIntervalEnd
      , pbodyCompact'requiredObserversHash
      , pbodyCompact'requiredSignersHash
      , pbodyCompact'mintHash
      , pbodyCompact'scriptIntegrityHash
      , pbodyCompact'auxiliaryDataHash
      , pbodyCompact'networkId
      } <-
      pmatch body
    PMidgardTransactionBodyPartialPreimages
      { pbodyPreimages'inputs
      , pbodyPreimages'referenceInputs
      , pbodyPreimages'outputs
      , pbodyPreimages'requiredObservers
      , pbodyPreimages'requiredSigners
      , pbodyPreimages'mint
      } <-
      pmatch preimages
    pcon
      ( PMidgardTransactionBodyPartialView
          { pbodyView'inputs =
              pdata (pdecodeOptional pdecodeMidgardTxInputsPreimageCbor (pfromData pbodyPreimages'inputs) pbodyCompact'spendInputsHash)
          , pbodyView'referenceInputs =
              pdata (pdecodeOptional pdecodeMidgardTxInputsPreimageCbor (pfromData pbodyPreimages'referenceInputs) pbodyCompact'referenceInputsHash)
          , pbodyView'outputs =
              pdata (pdecodeOptional pdecodeMidgardTxOutputsPreimageCbor (pfromData pbodyPreimages'outputs) pbodyCompact'outputsHash)
          , pbodyView'fee = pdata (pjust pbodyCompact'fee)
          , pbodyView'validityIntervalStart = pdata (pjust pbodyCompact'validityIntervalStart)
          , pbodyView'validityIntervalEnd = pdata (pjust pbodyCompact'validityIntervalEnd)
          , pbodyView'requiredObservers =
              pdata (pdecodeOptional (plam (\b -> pwrapBytes #$ pdecodeMidgardTxHash28ListPreimageCbor # b)) (pfromData pbodyPreimages'requiredObservers) pbodyCompact'requiredObserversHash)
          , pbodyView'requiredSigners =
              pdata (pdecodeOptional (plam (\b -> pwrapBytes #$ pdecodeMidgardTxHash28ListPreimageCbor # b)) (pfromData pbodyPreimages'requiredSigners) pbodyCompact'requiredSignersHash)
          , pbodyView'mint =
              pdata (pdecodeOptional pdecodeMidgardTxMintPreimageCbor (pfromData pbodyPreimages'mint) pbodyCompact'mintHash)
          , pbodyView'scriptIntegrityHash = pdata (pjust pbodyCompact'scriptIntegrityHash)
          , pbodyView'auxiliaryDataHash = pdata (pjust pbodyCompact'auxiliaryDataHash)
          , pbodyView'networkId = pdata (pjust pbodyCompact'networkId)
          }
      )

-- | Aiken @transaction.partial_witness_set_view_from_compact_and_preimages@.
ppartialWitnessSetViewFromCompactAndPreimages ::
  forall (s :: S).
  Term
    s
    ( PNativeTxWitnessSetCompact
        :--> PMidgardTransactionWitnessSetPartialPreimages
        :--> PMidgardTransactionWitnessSetPartialView
    )
ppartialWitnessSetViewFromCompactAndPreimages = phoistAcyclic $
  plam $ \witnessSet preimages -> P.do
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash
      , pwitnessSetCompact'scriptTxWitsHash
      , pwitnessSetCompact'redeemerTxWitsHash
      } <-
      pmatch witnessSet
    PMidgardTransactionWitnessSetPartialPreimages
      { pwitnessSetPreimages'addrTxWits
      , pwitnessSetPreimages'scriptTxWits
      , pwitnessSetPreimages'redeemerTxWits
      } <-
      pmatch preimages
    pcon
      ( PMidgardTransactionWitnessSetPartialView
          { pwitnessSetView'addrTxWits =
              pdata (pdecodeOptional pdecodeMidgardTxAddressWitnessesPreimageCbor (pfromData pwitnessSetPreimages'addrTxWits) (pfromData pwitnessSetCompact'addrTxWitsHash))
          , pwitnessSetView'scriptTxWits =
              pdata (pdecodeOptional pdecodeMidgardTxScriptWitnessesPreimageCbor (pfromData pwitnessSetPreimages'scriptTxWits) (pfromData pwitnessSetCompact'scriptTxWitsHash))
          , pwitnessSetView'redeemerTxWits =
              pdata (pdecodeOptional pdecodeMidgardTxRedeemerWitnessesPreimageCbor (pfromData pwitnessSetPreimages'redeemerTxWits) (pfromData pwitnessSetCompact'redeemerTxWitsHash))
          }
      )

{- | Aiken @transaction.partial_view_from_compact_and_preimages@.

Note the witness set is /optional/ and, when supplied, is checked against the
compact transaction's @witness_set_hash@ before anything under it is read. The
compact form commits to the witness set by hash only, so without that check a
caller could substitute a different witness set and open its fields instead.
-}
ppartialViewFromCompactAndPreimages ::
  forall (s :: S).
  Term
    s
    ( PVerifiedMidgardNativeTxCompact
        :--> PMidgardTransactionBodyPartialPreimages
        :--> PMaybe PNativeTxWitnessSetCompact
        :--> PMidgardTransactionWitnessSetPartialPreimages
        :--> PMidgardTransactionPartialView
    )
ppartialViewFromCompactAndPreimages = phoistAcyclic $
  plam $ \compact bodyPreimages witnessSet witnessSetPreimages -> P.do
    PVerifiedMidgardNativeTxCompact {pverified'version, pverified'txCompact} <- pmatch compact
    PNativeTxCompact {pcompact'body, pcompact'witnessSetHash, pcompact'validityCode} <-
      pmatch pverified'txCompact
    witnessSetView <-
      plet
        ( pmatch witnessSet $ \case
            PJust witnessSetCompact ->
                pif
                  ( (pblake2b_256 #$ pencodeNativeTxWitnessSetCompact # witnessSetCompact)
                      #!= pcompact'witnessSetHash
                  )
                  perror
                  ( pjust
                      ( ppartialWitnessSetViewFromCompactAndPreimages
                          # witnessSetCompact
                          # witnessSetPreimages
                      )
                  )
            PNothing -> pcon PDNothing
        )
    pmkView
      pverified'version
      pcompact'validityCode
      (ppartialBodyViewFromCompactAndPreimages # pcompact'body # bodyPreimages)
      witnessSetView

--------------------------------------------------------------------------------
-- Fixed-arity list destructuring
--------------------------------------------------------------------------------

{- | Aiken's @expect [a, b, c] = list@ over a three-element @Data@ list.

Written out rather than folded, because the arity is the check: a list of any
other length errors.
-}
ptake3 ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList PData) ->
  (Term s PData -> Term s PData -> Term s PData -> Term s r) ->
  Term s r
ptake3 items k =
  pelimList
    ( \d0 r0 ->
        pelimList
          ( \d1 r1 ->
              pelimList
                (\d2 r2 -> pif (pnull # r2) (k d0 d1 d2) perror)
                perror
                r1
          )
          perror
          r0
    )
    perror
    items

-- | The twelve-element counterpart of 'ptake3'.
ptake12 ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList PData) ->
  ( Term s PData ->
    Term s PData ->
    Term s PData ->
    Term s PData ->
    Term s PData ->
    Term s PData ->
    Term s PData ->
    Term s PData ->
    Term s PData ->
    Term s PData ->
    Term s PData ->
    Term s PData ->
    Term s r
  ) ->
  Term s r
ptake12 items k =
  go items [] 12
  where
    go rest acc (0 :: Int) =
      pif (pnull # rest) (apply (reverse acc)) perror
    go rest acc n =
      pelimList (\x xs -> go xs (x : acc) (n - 1)) perror rest
    apply [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11] =
      k d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11
    apply _ = error "ptake12: unreachable"
