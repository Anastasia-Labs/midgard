{- |
Module      : Midgard.FraudProofs.NativeTx.Compact
Description : Plutarch port of
              @lib/midgard/fraud-proofs/native-tx/compact.ak@.

The *compact* form of a Midgard transaction, and the commitments over it.

A fault proof cannot carry a whole L2 transaction on L1, so it carries this
instead: every fixed scalar in full, and one 32-byte hash per variable-length
collection. That is a bounded size regardless of how large the transaction was,
and it is enough to bind the transaction's id — because the id is defined as a
hash of exactly these compact body bytes.

=== Why the encoders and decoders come in pairs

Each direction exists separately, and the verifiers use *both*:
'pverifyNativeTxCompactForVersion' decodes nothing — it takes a value, re-encodes
it, and demands the bytes come back identical to what the prover supplied. That
is the check that makes the compact form canonical in practice rather than only
in principle: a prover cannot supply bytes that decode to the right value but are
not the bytes the encoder would have produced, because the comparison is on the
bytes, not on the value.

'pverifyNativeTxCompactCborForVersion' goes the other way — it decodes, and
slices the body's own bytes back out of the input to hash them, rather than
re-encoding. Both end at the same place, and both are exported because different
callers hold different things.

=== One trap

'pencodeNativeTxFieldPreimageLengthsV1' writes @script_witnesses@ *before*
@address_witnesses@, which is the opposite of the order the record declares them
in. The decoder reads them in the same swapped order, so the format round-trips
and 'pverifyNativeTxProofSourceV1' would catch any disagreement — but a
positional port that followed the record declaration would silently transpose the
two lengths.
-}
module Midgard.FraudProofs.NativeTx.Compact (
  -- * Transaction identity
  pnativeTxIdForVersion,
  pnativeTxFullHashV1,

  -- * The compact body and witness set
  pencodeNativeTxBodyCompact,
  pencodeNativeTxWitnessSetCompact,
  pdecodeNativeTxWitnessSetCompact,
  pencodeNativeTxCompactV1,
  pdecodeNativeTxCompactV1,

  -- * Proof sources
  pencodeNativeTxProofSourceV1,
  pnativeTxProofCommitmentV1,
  pencodeNativeTxFieldPreimageLengthsV1,
  pdecodeNativeTxFieldPreimageLengthsV1,
  pnativeTxCanonicalSizeV1,

  -- * Verification
  pverifyNativeTxCompactV1,
  pverifyNativeTxCompactCborV1,
  pverifyNativeTxProofSourceV1,
) where

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pdecodeCanonicalIntAt,
  pdecodeCanonicalUintAt,
  pencodeDefiniteBytes,
  pexpectByte,
  pexpectH32,
  pexpectNetworkId,
  pexpectNonNegative,
  pexpectValidityCode,
  psliceLen,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
  pnativeTxVersionV1,
 )

--------------------------------------------------------------------------------
-- Domains
--------------------------------------------------------------------------------

-- | Aiken @compact.native_tx_body_v1_domain@ — @"MidgardNativeTxBodyV1"@.
pnativeTxBodyV1Domain :: forall (s :: S). Term s PByteString
pnativeTxBodyV1Domain = pconstant "MidgardNativeTxBodyV1"

-- | Aiken @compact.native_tx_full_v1_domain@ — @"MidgardNativeTxFullV1"@.
pnativeTxFullV1Domain :: forall (s :: S). Term s PByteString
pnativeTxFullV1Domain = pconstant "MidgardNativeTxFullV1"

-- | Aiken @compact.native_tx_proof_source_v1_domain@ — @"MidgardNativeTxProofSourceV1"@.
pnativeTxProofSourceV1Domain :: forall (s :: S). Term s PByteString
pnativeTxProofSourceV1Domain = pconstant "MidgardNativeTxProofSourceV1"

-- | Aiken @compact.expect_supported_native_tx_version@.
pexpectSupportedVersion :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectSupportedVersion = phoistAcyclic $
  plam $ \version ->
    pif (version #== pnativeTxVersionV1) version perror

--------------------------------------------------------------------------------
-- Transaction identity
--------------------------------------------------------------------------------

{- | Aiken @compact.native_tx_id_for_version@.

A Midgard transaction's id is the domain-separated hash of its version and its
compact *body* bytes — not the whole transaction. The witness set is therefore
outside the id, which is what lets witnesses be supplied and checked separately
without changing what the transaction is.
-}
pnativeTxIdForVersion ::
  forall (s :: S). Term s (PInteger :--> PByteString :--> PByteString)
pnativeTxIdForVersion = phoistAcyclic $
  plam $ \version bodyCbor ->
    plet (pexpectSupportedVersion # version) $ \v ->
      pblake2b_256 #$ pnativeTxBodyV1Domain <> pcborInt v <> bodyCbor

{- | Aiken @compact.native_tx_full_hash_v1@.

The consensus commitment over the *exact* canonical bytes of a full transaction,
witness preimages included. This only defines the domain separation — callers
must already have decoded the bytes with the exact-V1 decoder, or they are
hashing something they have not checked.
-}
pnativeTxFullHashV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
pnativeTxFullHashV1 = phoistAcyclic $
  plam $ \nativeTxCbor ->
    pblake2b_256
      #$ pnativeTxFullV1Domain
      <> pcborInt pnativeTxVersionV1
      <> nativeTxCbor

--------------------------------------------------------------------------------
-- The compact body and witness set
--------------------------------------------------------------------------------

{- | Aiken @compact.encode_native_tx_body_compact@.

Twelve fields as a definite CBOR array (@0x8c@). Every hash is checked to be 32
bytes and every scalar to be in range before anything is written, so the encoder
cannot produce bytes that its own decoder would reject.

Note @validity_interval_start@ and @validity_interval_end@ are *not* range
checked — they are signed and read back with the canonical /int/ decoder, unlike
@fee@ and @network_id@.
-}
pencodeNativeTxBodyCompact ::
  forall (s :: S). Term s (PNativeTxBodyCompact :--> PByteString)
pencodeNativeTxBodyCompact = phoistAcyclic $
  plam $ \body -> P.do
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
    pconstant "\x8c"
      <> (pencodeDefiniteBytes #$ pexpectH32 # pbodyCompact'spendInputsHash)
      <> (pencodeDefiniteBytes #$ pexpectH32 # pbodyCompact'referenceInputsHash)
      <> (pencodeDefiniteBytes #$ pexpectH32 # pbodyCompact'outputsHash)
      <> pcborInt (pexpectNonNegative # pbodyCompact'fee)
      <> pcborInt pbodyCompact'validityIntervalStart
      <> pcborInt pbodyCompact'validityIntervalEnd
      <> (pencodeDefiniteBytes #$ pexpectH32 # pbodyCompact'requiredObserversHash)
      <> (pencodeDefiniteBytes #$ pexpectH32 # pbodyCompact'requiredSignersHash)
      <> (pencodeDefiniteBytes #$ pexpectH32 # pbodyCompact'mintHash)
      <> (pencodeDefiniteBytes #$ pexpectH32 # pbodyCompact'scriptIntegrityHash)
      <> (pencodeDefiniteBytes #$ pexpectH32 # pbodyCompact'auxiliaryDataHash)
      <> pcborInt (pexpectNetworkId # pbodyCompact'networkId)

-- | Aiken @compact.encode_native_tx_witness_set_compact@ — three hashes, @0x83@.
pencodeNativeTxWitnessSetCompact ::
  forall (s :: S). Term s (PNativeTxWitnessSetCompact :--> PByteString)
pencodeNativeTxWitnessSetCompact = phoistAcyclic $
  plam $ \witnessSet -> P.do
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash
      , pwitnessSetCompact'scriptTxWitsHash
      , pwitnessSetCompact'redeemerTxWitsHash
      } <-
      pmatch witnessSet
    pconstant "\x83"
      <> (pencodeDefiniteBytes #$ pexpectH32 # pfromData pwitnessSetCompact'addrTxWitsHash)
      <> (pencodeDefiniteBytes #$ pexpectH32 # pfromData pwitnessSetCompact'scriptTxWitsHash)
      <> (pencodeDefiniteBytes #$ pexpectH32 # pfromData pwitnessSetCompact'redeemerTxWitsHash)

{- | Aiken @compact.decode_native_tx_witness_set_compact@.

Note the trailing @expect offset == length@: the input must be exactly the
witness set and nothing more, so trailing bytes cannot ride along inside
something whose hash is checked elsewhere.
-}
pdecodeNativeTxWitnessSetCompact ::
  forall (s :: S). Term s (PByteString :--> PNativeTxWitnessSetCompact)
pdecodeNativeTxWitnessSetCompact = phoistAcyclic $
  plam $ \cbor ->
    pmatch (pdecodeHash32At # cbor # (pexpectByte # cbor # 0 # 131)) $
      \(PPair o1 addrTxWitsHash) ->
        pmatch (pdecodeHash32At # cbor # o1) $ \(PPair o2 scriptTxWitsHash) ->
          pmatch (pdecodeHash32At # cbor # o2) $ \(PPair o3 redeemerTxWitsHash) ->
            pif
              (o3 #== plengthBS # cbor)
              ( pcon
                  ( PNativeTxWitnessSetCompact
                      { pwitnessSetCompact'addrTxWitsHash = pdata addrTxWitsHash
                      , pwitnessSetCompact'scriptTxWitsHash = pdata scriptTxWitsHash
                      , pwitnessSetCompact'redeemerTxWitsHash = pdata redeemerTxWitsHash
                      }
                  )
              )
              perror

{- | Aiken @compact.decode_hash32_at@.

Reads exactly the two-byte header @0x58 0x20@ and the 32 bytes after it. Pinning
the header rather than accepting any definite byte-string form is what keeps a
32-byte hash from also being writable in the packed or wider forms.
-}
pdecodeHash32At ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PByteString)
pdecodeHash32At = phoistAcyclic $
  plam $ \bytes offset ->
    plet (pexpectByte # bytes # (pexpectByte # bytes # offset # 88) # 32) $ \start ->
      pcon (PPair (start + 32) (psliceLen # bytes # start # 32))

-- | Aiken @compact.encode_native_tx_compact_v1@.
pencodeNativeTxCompactV1 ::
  forall (s :: S). Term s (PNativeTxCompact :--> PByteString)
pencodeNativeTxCompactV1 = phoistAcyclic $
  plam $ \tx -> pencodeNativeTxCompactForVersion # pnativeTxVersionV1 # tx

-- | Aiken @compact.encode_native_tx_compact_for_version@.
pencodeNativeTxCompactForVersion ::
  forall (s :: S).
  Term s (PInteger :--> PNativeTxCompact :--> PByteString)
pencodeNativeTxCompactForVersion = phoistAcyclic $
  plam $ \version tx -> P.do
    PNativeTxCompact {pcompact'body, pcompact'witnessSetHash, pcompact'validityCode} <-
      pmatch tx
    pconstant "\x84"
      <> pcborInt (pexpectSupportedVersion # version)
      <> (pencodeNativeTxBodyCompact # pcompact'body)
      <> (pencodeDefiniteBytes #$ pexpectH32 # pcompact'witnessSetHash)
      <> pcborInt (pexpectValidityCode # pcompact'validityCode)

-- | Aiken @compact.decode_native_tx_compact_v1@.
pdecodeNativeTxCompactV1 ::
  forall (s :: S). Term s (PByteString :--> PNativeTxCompact)
pdecodeNativeTxCompactV1 = phoistAcyclic $
  plam $ \cbor -> P.do
    o0 <- plet $ pexpectByte # cbor # (pexpectByte # cbor # 0 # 132) # pnativeTxVersionV1
    PPair o1 body <- pmatch (pdecodeNativeTxBodyCompactAt # cbor # o0)
    PPair o2 witnessSetHash <- pmatch (pdecodeHash32At # cbor # o1)
    PPair o3 validityCode <- pmatch (pdecodeCanonicalUintAt # cbor # o2)
    pif
      (o3 #== plengthBS # cbor #&& validityCode #<= 5)
      ( pcon
          ( PNativeTxCompact
              { pcompact'body = body
              , pcompact'witnessSetHash = witnessSetHash
              , pcompact'validityCode = validityCode
              }
          )
      )
      perror

{- | Aiken @compact.decode_native_tx_body_compact_cbor_at@.

Returns the offset past the body as well as the body, so a caller can slice the
body's own bytes back out — which is what
'pverifyNativeTxCompactCborV1' does instead of re-encoding.
-}
pdecodeNativeTxBodyCompactAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PNativeTxBodyCompact)
pdecodeNativeTxBodyCompactAt = phoistAcyclic $
  plam $ \cbor offset -> P.do
    o0 <- plet $ pexpectByte # cbor # offset # 140
    PPair o1 spendInputsHash <- pmatch (pdecodeHash32At # cbor # o0)
    PPair o2 referenceInputsHash <- pmatch (pdecodeHash32At # cbor # o1)
    PPair o3 outputsHash <- pmatch (pdecodeHash32At # cbor # o2)
    PPair o4 fee <- pmatch (pdecodeCanonicalUintAt # cbor # o3)
    PPair o5 validityStart <- pmatch (pdecodeCanonicalIntAt # cbor # o4)
    PPair o6 validityEnd <- pmatch (pdecodeCanonicalIntAt # cbor # o5)
    PPair o7 requiredObserversHash <- pmatch (pdecodeHash32At # cbor # o6)
    PPair o8 requiredSignersHash <- pmatch (pdecodeHash32At # cbor # o7)
    PPair o9 mintHash <- pmatch (pdecodeHash32At # cbor # o8)
    PPair o10 scriptIntegrityHash <- pmatch (pdecodeHash32At # cbor # o9)
    PPair o11 auxiliaryDataHash <- pmatch (pdecodeHash32At # cbor # o10)
    PPair o12 networkId <- pmatch (pdecodeCanonicalUintAt # cbor # o11)
    pcon $
      PPair
        o12
        ( pcon
            ( PNativeTxBodyCompact
                { pbodyCompact'spendInputsHash = spendInputsHash
                , pbodyCompact'referenceInputsHash = referenceInputsHash
                , pbodyCompact'outputsHash = outputsHash
                , pbodyCompact'fee = fee
                , pbodyCompact'validityIntervalStart = validityStart
                , pbodyCompact'validityIntervalEnd = validityEnd
                , pbodyCompact'requiredObserversHash = requiredObserversHash
                , pbodyCompact'requiredSignersHash = requiredSignersHash
                , pbodyCompact'mintHash = mintHash
                , pbodyCompact'scriptIntegrityHash = scriptIntegrityHash
                , pbodyCompact'auxiliaryDataHash = auxiliaryDataHash
                , pbodyCompact'networkId = pexpectNetworkId # networkId
                }
            )
        )

--------------------------------------------------------------------------------
-- Proof sources
--------------------------------------------------------------------------------

-- | Aiken @compact.encode_native_tx_proof_source_v1@.
pencodeNativeTxProofSourceV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PByteString)
pencodeNativeTxProofSourceV1 = phoistAcyclic $
  plam $ \compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor ->
    pconstant "\x83"
      <> (pencodeDefiniteBytes # compactCbor)
      <> (pencodeDefiniteBytes # witnessSetCompactCbor)
      <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)

{- | Aiken @compact.native_tx_proof_commitment_v1@.

The single hash a datum carries in place of a whole proof source. All three
components are inside it, so a prover cannot pair one transaction's compact bytes
with another's witness set or field lengths.
-}
pnativeTxProofCommitmentV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PByteString)
pnativeTxProofCommitmentV1 = phoistAcyclic $
  plam $ \compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor ->
    pblake2b_256
      #$ pnativeTxProofSourceV1Domain
      <> pcborInt 1
      <> ( pencodeNativeTxProofSourceV1
             # compactCbor
             # witnessSetCompactCbor
             # fieldPreimageLengthsCbor
         )

{- | Aiken @compact.encode_native_tx_field_preimage_lengths_v1@.

Nine lengths as a definite array (@0x89@).

/Read the order carefully./ The wire order is spend inputs, reference inputs,
outputs, required observers, required signers, mint, **script witnesses, address
witnesses**, redeemers — script before address, which is the opposite of the
record's field order. 'pdecodeNativeTxFieldPreimageLengthsV1' reads them the same
way round, so the pair is consistent and
'pverifyNativeTxProofSourceV1' would catch any drift; the hazard is only for
someone porting positionally from the record.
-}
pencodeNativeTxFieldPreimageLengthsV1 ::
  forall (s :: S). Term s (PNativeTxFieldPreimageLengthsV1 :--> PByteString)
pencodeNativeTxFieldPreimageLengthsV1 = phoistAcyclic $
  plam $ \lengths -> P.do
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
      pmatch lengths
    pconstant "\x89"
      <> pcborInt (pexpectNonNegative # plengths'spendInputs)
      <> pcborInt (pexpectNonNegative # plengths'referenceInputs)
      <> pcborInt (pexpectNonNegative # plengths'outputs)
      <> pcborInt (pexpectNonNegative # plengths'requiredObservers)
      <> pcborInt (pexpectNonNegative # plengths'requiredSigners)
      <> pcborInt (pexpectNonNegative # plengths'mint)
      -- Script before address: the wire order, not the record order.
      <> pcborInt (pexpectNonNegative # plengths'scriptWitnesses)
      <> pcborInt (pexpectNonNegative # plengths'addressWitnesses)
      <> pcborInt (pexpectNonNegative # plengths'redeemers)

-- | Aiken @compact.decode_native_tx_field_preimage_lengths_v1@.
pdecodeNativeTxFieldPreimageLengthsV1 ::
  forall (s :: S). Term s (PByteString :--> PNativeTxFieldPreimageLengthsV1)
pdecodeNativeTxFieldPreimageLengthsV1 = phoistAcyclic $
  plam $ \cbor -> P.do
    o0 <- plet $ pexpectByte # cbor # 0 # 137
    PPair o1 spendInputs <- pmatch (pdecodeCanonicalUintAt # cbor # o0)
    PPair o2 referenceInputs <- pmatch (pdecodeCanonicalUintAt # cbor # o1)
    PPair o3 outputs <- pmatch (pdecodeCanonicalUintAt # cbor # o2)
    PPair o4 requiredObservers <- pmatch (pdecodeCanonicalUintAt # cbor # o3)
    PPair o5 requiredSigners <- pmatch (pdecodeCanonicalUintAt # cbor # o4)
    PPair o6 mint <- pmatch (pdecodeCanonicalUintAt # cbor # o5)
    -- Script before address, matching the encoder.
    PPair o7 scriptWitnesses <- pmatch (pdecodeCanonicalUintAt # cbor # o6)
    PPair o8 addressWitnesses <- pmatch (pdecodeCanonicalUintAt # cbor # o7)
    PPair o9 redeemers <- pmatch (pdecodeCanonicalUintAt # cbor # o8)
    pif
      (o9 #== plengthBS # cbor)
      ( pcon
          ( PNativeTxFieldPreimageLengthsV1
              { plengths'spendInputs = spendInputs
              , plengths'referenceInputs = referenceInputs
              , plengths'outputs = outputs
              , plengths'requiredObservers = requiredObservers
              , plengths'requiredSigners = requiredSigners
              , plengths'mint = mint
              , plengths'addressWitnesses = addressWitnesses
              , plengths'scriptWitnesses = scriptWitnesses
              , plengths'redeemers = redeemers
              }
          )
      )
      perror

{- | Aiken @compact.definite_bytes_encoded_size@.

How many bytes a definite byte string of this payload size occupies, header
included — the size ladder of 'pencodeDefiniteBytes' read as arithmetic.
-}
pdefiniteBytesEncodedSize :: forall (s :: S). Term s (PInteger :--> PInteger)
pdefiniteBytesEncodedSize = phoistAcyclic $
  plam $ \payloadSize ->
    pif (payloadSize #< 0) perror $
      pif (payloadSize #< 24) (1 + payloadSize) $
        pif (payloadSize #<= 255) (2 + payloadSize) $
          pif (payloadSize #<= 65535) (3 + payloadSize) (5 + payloadSize)

{- | Aiken @compact.native_tx_canonical_size_v1@.

The exact size the full canonical transaction would occupy, computed from the
compact form and the nine committed lengths — without holding the preimages.
That is the point: a size limit can be enforced against a transaction nobody has
in full.

The two @34@s are the script-integrity and auxiliary-data hashes: two header
bytes plus 32.
-}
pnativeTxCanonicalSizeV1 ::
  forall (s :: S).
  Term s (PNativeTxCompact :--> PNativeTxFieldPreimageLengthsV1 :--> PInteger)
pnativeTxCanonicalSizeV1 = phoistAcyclic $
  plam $ \compact lengths -> P.do
    PNativeTxCompact {pcompact'body, pcompact'validityCode} <- pmatch compact
    PNativeTxBodyCompact
      { pbodyCompact'fee
      , pbodyCompact'validityIntervalStart
      , pbodyCompact'validityIntervalEnd
      , pbodyCompact'networkId
      } <-
      pmatch pcompact'body
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
      pmatch lengths
    let size = plam $ \n -> pdefiniteBytesEncodedSize # n
        scalar = plam $ \n -> plengthBS # pcborInt n
        bodySize =
          1
            + (size # plengths'spendInputs)
            + (size # plengths'referenceInputs)
            + (size # plengths'outputs)
            + (scalar # pbodyCompact'fee)
            + (scalar # pbodyCompact'validityIntervalStart)
            + (scalar # pbodyCompact'validityIntervalEnd)
            + (size # plengths'requiredObservers)
            + (size # plengths'requiredSigners)
            + (size # plengths'mint)
            + 34
            + 34
            + (scalar # pbodyCompact'networkId)
        witnessSetSize =
          1
            + (size # plengths'addressWitnesses)
            + (size # plengths'scriptWitnesses)
            + (size # plengths'redeemers)
    1
      + (scalar # pnativeTxVersionV1)
      + bodySize
      + witnessSetSize
      + (scalar # pcompact'validityCode)

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

{- | Aiken @compact.verify_native_tx_compact_v1@.

Takes a compact transaction the caller already holds, re-encodes it, and demands
both that the body hashes to the claimed id /and/ that the re-encoded bytes are
byte-for-byte the ones supplied. The second check is what makes the encoding
canonical in practice: a prover cannot hand over bytes that merely decode to the
right value.
-}
pverifyNativeTxCompactV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PNativeTxCompact
        :--> PByteString
        :--> PVerifiedMidgardNativeTxCompact
    )
pverifyNativeTxCompactV1 = phoistAcyclic $
  plam $ \nativeTxId nativeTx nativeTxCompactCbor -> P.do
    PNativeTxCompact {pcompact'body, pcompact'witnessSetHash, pcompact'validityCode} <-
      pmatch nativeTx
    bodyCbor <- plet $ pencodeNativeTxBodyCompact # pcompact'body
    reencoded <-
      plet $
        pconstant "\x84"
          <> pcborInt pnativeTxVersionV1
          <> bodyCbor
          <> (pencodeDefiniteBytes #$ pexpectH32 # pcompact'witnessSetHash)
          <> pcborInt (pexpectValidityCode # pcompact'validityCode)
    pif
      ( (pnativeTxIdForVersion # pnativeTxVersionV1 # bodyCbor)
          #== nativeTxId
          #&& reencoded
          #== nativeTxCompactCbor
      )
      ( pcon
          ( PVerifiedMidgardNativeTxCompact
              { pverified'txId = nativeTxId
              , pverified'version = pnativeTxVersionV1
              , pverified'txCompact = nativeTx
              }
          )
      )
      perror

{- | Aiken @compact.verify_native_tx_compact_cbor_v1@.

The other direction: decode the supplied bytes, and hash the body's own slice of
them rather than re-encoding. Reading the body back out of the input is what
makes this equivalent to the re-encoding check — the bytes that were hashed are
the bytes that were supplied.
-}
pverifyNativeTxCompactCborV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PVerifiedMidgardNativeTxCompact)
pverifyNativeTxCompactCborV1 = phoistAcyclic $
  plam $ \nativeTxId cbor -> P.do
    bodyStart <-
      plet $ pexpectByte # cbor # (pexpectByte # cbor # 0 # 132) # pnativeTxVersionV1
    PPair afterBody body <- pmatch (pdecodeNativeTxBodyCompactAt # cbor # bodyStart)
    bodyCbor <- plet $ psliceLen # cbor # bodyStart # (afterBody - bodyStart)
    PPair afterHash witnessSetHash <- pmatch (pdecodeHash32At # cbor # afterBody)
    PPair afterCode validityCode <- pmatch (pdecodeCanonicalUintAt # cbor # afterHash)
    pif
      ( (pnativeTxIdForVersion # pnativeTxVersionV1 # bodyCbor)
          #== nativeTxId
          #&& afterCode
          #== plengthBS # cbor
          #&& validityCode
          #<= 5
      )
      ( pcon
          ( PVerifiedMidgardNativeTxCompact
              { pverified'txId = nativeTxId
              , pverified'version = pnativeTxVersionV1
              , pverified'txCompact =
                  pcon
                    ( PNativeTxCompact
                        { pcompact'body = body
                        , pcompact'witnessSetHash = witnessSetHash
                        , pcompact'validityCode = validityCode
                        }
                    )
              }
          )
      )
      perror

{- | Aiken @compact.verify_native_tx_proof_source_v1@.

The entry point everything above serves: given a transaction id and the three
byte strings a proof source carries, authenticate all of them together.

Three bindings are made, and each closes a different substitution. The compact
bytes must hash to the claimed id. The witness-set bytes must hash to the
@witness_set_hash@ *inside* those compact bytes, so a witness set cannot be
swapped for another transaction's. And the field lengths must re-encode to
exactly the bytes supplied, which is where the encoder/decoder pair is checked
against itself — including the script/address transposition noted above.
-}
pverifyNativeTxProofSourceV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PPair PVerifiedMidgardNativeTxCompact PNativeTxWitnessSetCompact
    )
pverifyNativeTxProofSourceV1 = phoistAcyclic $
  plam $ \nativeTxId compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor -> P.do
    compact <- plet $ pverifyNativeTxCompactCborV1 # nativeTxId # compactCbor
    witnessSet <- plet $ pdecodeNativeTxWitnessSetCompact # witnessSetCompactCbor
    PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch compact
    PNativeTxCompact {pcompact'witnessSetHash} <- pmatch pverified'txCompact
    fieldLengths <-
      plet $ pdecodeNativeTxFieldPreimageLengthsV1 # fieldPreimageLengthsCbor
    pif
      ( (pblake2b_256 # witnessSetCompactCbor)
          #== pcompact'witnessSetHash
          #&& (pencodeNativeTxFieldPreimageLengthsV1 # fieldLengths)
          #== fieldPreimageLengthsCbor
      )
      (pcon (PPair compact witnessSet))
      perror
