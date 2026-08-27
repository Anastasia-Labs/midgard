{- |
Module      : Midgard.FraudProofs.NativeTx.Codec
Description : Plutarch port of
              @lib/midgard/fraud-proofs/native-tx/codec.ak@.

Hand-written CBOR, in both directions, plus the small "expect this shape" helpers
the native-transaction carriage layer is built from.

Why hand-written rather than delegated to a generic serialiser: Midgard commits
to *canonical* CBOR, and canonical CBOR admits exactly one encoding of each
value. A generic encoder is free to pick any admissible form, and a generic
decoder is free to accept any of them — either would break the one-encoding-per-
value property that every hash commitment here depends on.

=== Two decoders for each integer, and why

@decode_uint_at@ and @decode_canonical_uint_at@ read the same grammar and differ
only in that the canonical one rejects a value encoded in a wider form than it
needs. Both exist because the two are used in different places: the compact
transaction array and the machine's work-witness array pin their own header
widths at the call site and read through the permissive decoder, while anything
whose bytes are covered by a hash must go through the canonical one — otherwise
the same logical transaction would have several valid byte encodings and several
different commitments.

The array-header reader here is deliberately the *permissive* one, and is not
the §5.1 field-preimage decoder; that one lives behind
@native_tx_field_access_v1.decode_field_array_header_at@, which is the single
place that grammar is interpreted.

=== Not ported

@expect_deserialise@, which wraps Aiken's @cbor.deserialise@ — a complete CBOR
reader written in Aiken, since Plutus has no deserialising builtin. It has **no
callers** anywhere in the live tree, so porting it would mean carrying a large
decoder for nothing. If something later needs it, that is its own slice.
-}
module Midgard.FraudProofs.NativeTx.Codec (
  -- * Shape assertions
  pexpectH32,
  pexpectH28,
  pexpectNonNegative,
  pexpectValidityCode,
  pexpectNetworkId,
  pexpectByte,
  pexpectPreimageHash,

  -- * Verdict codes
  pvalidityFromCode,
  pvalidityToCode,
  pvalidityToPlutusData,
  pvalidityFromPlutusData,

  -- * Reading
  pbyteAt,
  psliceLen,
  pdecodeUintAt,
  pdecodeCanonicalUintAt,
  pdecodeIntAt,
  pdecodeCanonicalIntAt,
  pdecodeDefiniteBytesAt,
  pdecodeDefiniteArrayHeaderAt,
  pdecodeDefiniteMapHeaderAt,

  -- * Writing
  pcborInt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteMapHeader,
  pencodeDefiniteBytes,
) where

import Plutarch.Builtin.ByteString (
  pbyteStringToInteger,
  pintegerToByteString,
  pmostSignificantFirst,
 )
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Builtin.Data (pasConstr, pconstrBuiltin, pserialiseData)
import Plutarch.Core.Internal.Builtins (pconsBS', pindexBS')
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Types (PMidgardTxValidity (..))

--------------------------------------------------------------------------------
-- Shape assertions
--------------------------------------------------------------------------------

{- | Aiken @codec.expect_h32@ — the identity on 32-byte strings, an error
otherwise.

Written as a checked identity rather than a predicate so it can sit inline in a
pipeline; the caller gets the value back only if it was the right width.
-}
pexpectH32 :: forall (s :: S). Term s (PByteString :--> PByteString)
pexpectH32 = phoistAcyclic $
  plam $ \bytes -> pif (plengthBS # bytes #== 32) bytes perror

-- | Aiken @codec.expect_h28@.
pexpectH28 :: forall (s :: S). Term s (PByteString :--> PByteString)
pexpectH28 = phoistAcyclic $
  plam $ \bytes -> pif (plengthBS # bytes #== 28) bytes perror

-- | Aiken @codec.expect_non_negative@.
pexpectNonNegative :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectNonNegative = phoistAcyclic $
  plam $ \value -> pif (0 #<= value) value perror

-- | Aiken @codec.expect_validity_code@ — a verdict code is @0..5@.
pexpectValidityCode :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectValidityCode = phoistAcyclic $
  plam $ \value ->
    pif (0 #<= value #&& value #<= 5) value perror

{- | Aiken @codec.expect_network_id@.

Zero, one, or @255@. The last is the ledger's "unspecified" marker, not a real
network, and admitting it is deliberate.
-}
pexpectNetworkId :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectNetworkId = phoistAcyclic $
  plam $ \value ->
    pif
      (0 #<= value #&& (value #== 0 #|| value #== 1 #|| value #== 255))
      value
      perror

{- | Aiken @codec.expect_byte@.

Asserts the byte at @offset@ and returns the offset /after/ it, so a decoder can
chain literal bytes without naming a cursor at each step.
-}
pexpectByte ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PInteger :--> PInteger)
pexpectByte = phoistAcyclic $
  plam $ \bytes offset expected ->
    pif ((pbyteAt # bytes # offset) #== expected) (offset + 1) perror

{- | Aiken @codec.expect_preimage_hash@.

Returns the preimage only if it hashes to the expected 32-byte value — the
content-addressing check the whole carriage layer rests on.
-}
pexpectPreimageHash ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
pexpectPreimageHash = phoistAcyclic $
  plam $ \preimageCbor expectedHash ->
    pif
      ( (plengthBS # expectedHash #== 32)
          #&& (pblake2b_256 # preimageCbor) #== expectedHash
      )
      preimageCbor
      perror

--------------------------------------------------------------------------------
-- Verdict codes
--------------------------------------------------------------------------------

{- | Aiken @codec.validity_from_code@.

The scalar the compact encoding carries, back into the constructor language. Out
of range is an @expect@, so it errors.
-}
pvalidityFromCode :: forall (s :: S). Term s (PInteger :--> PMidgardTxValidity)
pvalidityFromCode = phoistAcyclic $
  plam $ \value ->
    plet (pexpectValidityCode # value) $ \code ->
      pif (code #== 0) (pcon PTxIsValid) $
        pif (code #== 1) (pcon PNonExistentInputUtxo) $
          pif (code #== 2) (pcon PInvalidSignature) $
            pif (code #== 3) (pcon PFailedScript) $
              pif (code #== 4) (pcon PFeeTooLow) (pcon PUnbalancedTx)

-- | Aiken @codec.validity_to_code@.
pvalidityToCode :: forall (s :: S). Term s (PMidgardTxValidity :--> PInteger)
pvalidityToCode = phoistAcyclic $
  plam $ \validity ->
    pmatch validity $ \case
      PTxIsValid -> 0
      PNonExistentInputUtxo -> 1
      PInvalidSignature -> 2
      PFailedScript -> 3
      PFeeTooLow -> 4
      PUnbalancedTx -> 5

{- | Aiken @codec.validity_to_plutus_data@.

The explicit bridge between the scalar language the compact encoding uses and
the nullary-constructor language the datums use. Both directions are spelled out
rather than relying on the two happening to agree, because they are two separate
encodings of the same six verdicts and nothing but this pair keeps them aligned.
-}
pvalidityToPlutusData :: forall (s :: S). Term s (PMidgardTxValidity :--> PData)
pvalidityToPlutusData = phoistAcyclic $
  plam $ \validity ->
    pforgetData (pconstrBuiltin # (pvalidityToCode # validity) # pcon PNil)

{- | Aiken @codec.validity_from_plutus_data@.

Note it insists the constructor carries /no/ fields, so a verdict cannot be
smuggled in alongside a payload.
-}
pvalidityFromPlutusData :: forall (s :: S). Term s (PData :--> PMidgardTxValidity)
pvalidityFromPlutusData = phoistAcyclic $
  plam $ \dat ->
    plet (pasConstr # dat) $ \constr ->
      pif
        (pnull # (psndBuiltin # constr))
        (pvalidityFromCode # (pfstBuiltin # constr))
        perror

--------------------------------------------------------------------------------
-- Reading
--------------------------------------------------------------------------------

-- | Aiken @codec.byte_at@ — @bytearray.at@, which errors past the end.
pbyteAt :: forall (s :: S). Term s (PByteString :--> PInteger :--> PInteger)
pbyteAt = phoistAcyclic $
  plam $ \bytes offset -> pindexBS' # bytes # offset

{- | Aiken @codec.slice_len@.

A length-based slice over Aiken's inclusive-end @bytearray.slice@, with the
zero-length case pulled out — @slice(b, o, o - 1)@ is not the empty string.
-}
psliceLen ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PByteString)
psliceLen = phoistAcyclic $
  plam $ \bytes offset len ->
    pif
      (len #< 0)
      perror
      (pif (len #== 0) (pconstant "") (psliceBS # offset # len # bytes))

{- | Aiken @codec.decode_uint_at@.

A CBOR unsigned integer, returning @(next offset, value)@. Accepts non-minimal
widths — see the module note; use 'pdecodeCanonicalUintAt' anywhere the bytes are
under a hash.
-}
pdecodeUintAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeUintAt = phoistAcyclic $
  plam $ \bytes offset ->
    plet (pbyteAt # bytes # offset) $ \tag ->
      pif (tag #<= 23) (pcon (PPair (offset + 1) tag)) $
        pif (tag #== 24) (pcon (PPair (offset + 2) (pbyteAt # bytes # (offset + 1)))) $
          pif (tag #== 25) (pcon (PPair (offset + 3) (pbeUint bytes (offset + 1) 2))) $
            pif (tag #== 26) (pcon (PPair (offset + 5) (pbeUint bytes (offset + 1) 4))) $
              pif
                (tag #== 27)
                (pcon (PPair (offset + 9) (pbeUint bytes (offset + 1) 8)))
                perror

{- | Aiken @codec.decode_canonical_uint_at@.

As 'pdecodeUintAt', but each wider form must carry a value that would not have
fit in the narrower one. That is what makes the encoding injective: without it a
single value has up to five byte forms, and a commitment over those bytes stops
identifying the value.
-}
pdecodeCanonicalUintAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeCanonicalUintAt = phoistAcyclic $
  plam $ \bytes offset ->
    plet (pbyteAt # bytes # offset) $ \tag ->
      pif (tag #<= 23) (pcon (PPair (offset + 1) tag)) $
        pif
          (tag #== 24)
          (pminimal (pbyteAt # bytes # (offset + 1)) 23 (offset + 2))
          $ pif
            (tag #== 25)
            (pminimal (pbeUint bytes (offset + 1) 2) 0xff (offset + 3))
            $ pif
              (tag #== 26)
              (pminimal (pbeUint bytes (offset + 1) 4) 0xffff (offset + 5))
              $ pif
                (tag #== 27)
                (pminimal (pbeUint bytes (offset + 1) 8) 0xffffffff (offset + 9))
                perror
  where
    -- The value must exceed what the next narrower form could have held.
    pminimal value floor' next =
      pif (floor' #< value) (pcon (PPair next value)) perror

{- | Aiken @codec.decode_int_at@.

A CBOR integer of either sign. Negatives use major type 1, where the payload @n@
denotes @-1 - n@ — so the representable negative range is one wider than the
positive one, and the encoding has no negative zero.
-}
pdecodeIntAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeIntAt = phoistAcyclic $
  plam $ \bytes offset ->
    plet (pbyteAt # bytes # offset) $ \tag ->
      pif (tag #<= 27) (pdecodeUintAt # bytes # offset) $
        pif
          (32 #<= tag #&& tag #<= 55)
          (pcon (PPair (offset + 1) (-1 - (tag - 32))))
          $ pif
            (tag #== 56)
            (pcon (PPair (offset + 2) (-1 - (pbyteAt # bytes # (offset + 1)))))
            $ pif
              (tag #== 57)
              (pcon (PPair (offset + 3) (-1 - pbeUint bytes (offset + 1) 2)))
              $ pif
                (tag #== 58)
                (pcon (PPair (offset + 5) (-1 - pbeUint bytes (offset + 1) 4)))
                $ pif
                  (tag #== 59)
                  (pcon (PPair (offset + 9) (-1 - pbeUint bytes (offset + 1) 8)))
                  perror

-- | Aiken @codec.decode_canonical_int_at@ — 'pdecodeIntAt' with the minimality rule.
pdecodeCanonicalIntAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeCanonicalIntAt = phoistAcyclic $
  plam $ \bytes offset ->
    plet (pbyteAt # bytes # offset) $ \tag ->
      pif (tag #<= 27) (pdecodeCanonicalUintAt # bytes # offset) $
        pif
          (32 #<= tag #&& tag #<= 55)
          (pcon (PPair (offset + 1) (-1 - (tag - 32))))
          $ pif
            (tag #== 56)
            (pminimalNeg (pbyteAt # bytes # (offset + 1)) 23 (offset + 2))
            $ pif
              (tag #== 57)
              (pminimalNeg (pbeUint bytes (offset + 1) 2) 0xff (offset + 3))
              $ pif
                (tag #== 58)
                (pminimalNeg (pbeUint bytes (offset + 1) 4) 0xffff (offset + 5))
                $ pif
                  (tag #== 59)
                  (pminimalNeg (pbeUint bytes (offset + 1) 8) 0xffffffff (offset + 9))
                  perror
  where
    pminimalNeg value floor' next =
      pif (floor' #< value) (pcon (PPair next (-1 - value))) perror

{- | Aiken @codec.decode_definite_bytes_at@.

A definite-length byte string, returning @(offset past the payload, payload)@.
Indefinite-length strings are not in the grammar and their tag falls through to
the error.
-}
pdecodeDefiniteBytesAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PByteString)
pdecodeDefiniteBytesAt = phoistAcyclic $
  plam $ \bytes offset ->
    plet (pbyteAt # bytes # offset) $ \tag ->
      pif
        (64 #<= tag #&& tag #<= 87)
        (ptake bytes (offset + 1) (tag - 64))
        $ pif
          (tag #== 88)
          (ptake bytes (offset + 2) (pbyteAt # bytes # (offset + 1)))
          $ pif
            (tag #== 89)
            (ptake bytes (offset + 3) (pbeUint bytes (offset + 1) 2))
            $ pif
              (tag #== 90)
              (ptake bytes (offset + 5) (pbeUint bytes (offset + 1) 4))
              perror
  where
    ptake bytes start len =
      pcon (PPair (start + len) (psliceLen # bytes # start # len))

{- | Aiken @codec.decode_definite_array_header_at@.

The general definite-array head reader, for structures outside the §5.1
field-preimage grammar — the compact-transaction array and the machine's
work-witness array, both of which pin their own arity and header width at the
call site.

**Not the §5.1 decoder.** It accepts non-minimal widths and the four-byte form,
neither of which §5.1 admits; that grammar is interpreted only by
@native_tx_field_access_v1.decode_field_array_header_at@.
-}
pdecodeDefiniteArrayHeaderAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeDefiniteArrayHeaderAt = phoistAcyclic $ pheaderReader 128 152

-- | Aiken @codec.decode_definite_map_header_at@ — the map counterpart.
pdecodeDefiniteMapHeaderAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeDefiniteMapHeaderAt = phoistAcyclic $ pheaderReader 160 184

{- | The shared shape of the two header readers.

Arrays and maps differ only in where their major type starts, so the ladder is
written once and applied at both bases. @small@ is the base of the packed form
and @wide@ the base of the one-, two- and four-byte forms.
-}
pheaderReader ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pheaderReader small wide =
  plam $ \bytes offset ->
    plet (pbyteAt # bytes # offset) $ \tag ->
      pif
        (small #<= tag #&& tag #<= small + 23)
        (pcon (PPair (offset + 1) (tag - small)))
        $ pif
          (tag #== wide)
          (pcon (PPair (offset + 2) (pbyteAt # bytes # (offset + 1))))
          $ pif
            (tag #== wide + 1)
            (pcon (PPair (offset + 3) (pbeUint bytes (offset + 1) 2)))
            $ pif
              (tag #== wide + 2)
              (pcon (PPair (offset + 5) (pbeUint bytes (offset + 1) 4)))
              perror

{- | A big-endian unsigned integer of @width@ bytes read at @offset@.

Aiken spells each width out as nested @* 256 +@ arithmetic; this reads the slice
and converts it, which is the same value for every width used here.
-}
pbeUint ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger
pbeUint bytes offset width =
  pbyteStringToInteger # pmostSignificantFirst # (psliceBS # offset # width # bytes)

--------------------------------------------------------------------------------
-- Writing
--------------------------------------------------------------------------------

{- | Aiken @codec.encode_definite_array_header@.

The CBOR header for a definite-length array of @length@ items, in the shortest
form that fits: packed into the initial byte up to 23, then one, two or four
following bytes.

A negative length is an @expect@, so it errors. Note the ceiling: the largest
form emitted is the four-byte one, so a length above @2^32 - 1@ would be encoded
wrong — @from_int_big_endian@ errors first, which is what saves it.
-}
pencodeDefiniteArrayHeader ::
  forall (s :: S). Term s (PInteger :--> PByteString)
pencodeDefiniteArrayHeader = phoistAcyclic $ pheaderWriter 128 0x98

-- | Aiken @codec.encode_definite_map_header@ — the map counterpart.
pencodeDefiniteMapHeader ::
  forall (s :: S). Term s (PInteger :--> PByteString)
pencodeDefiniteMapHeader = phoistAcyclic $ pheaderWriter 160 0xb8

-- | The shared ladder behind both header writers.
pheaderWriter ::
  forall (s :: S).
  Term s PInteger ->
  Integer ->
  Term s (PInteger :--> PByteString)
pheaderWriter small wide =
  plam $ \len ->
    pif
      (len #< 0)
      perror
      ( pif
          (len #<= 23)
          (pconsBS' # (small + len) # pconstant "")
          ( pif
              (len #<= 255)
              (pbyte wide <> pbigEndian 1 len)
              ( pif
                  (len #<= 65535)
                  (pbyte (wide + 1) <> pbigEndian 2 len)
                  (pbyte (wide + 2) <> pbigEndian 4 len)
              )
          )
      )

{- | Aiken @codec.encode_definite_bytes@.

The same ladder for a byte string, prefixed to the bytes themselves. Unlike the
array header there is no @expect@ here, because the length comes from the string
rather than from a caller.
-}
pencodeDefiniteBytes ::
  forall (s :: S). Term s (PByteString :--> PByteString)
pencodeDefiniteBytes = phoistAcyclic $
  plam $ \bytes ->
    plet (plengthBS # bytes) $ \len ->
      pif
        (len #<= 23)
        (pconsBS' # (64 + len) # bytes)
        ( pif
            (len #<= 255)
            (pbyte 0x58 <> pbigEndian 1 len <> bytes)
            ( pif
                (len #<= 65535)
                (pbyte 0x59 <> pbigEndian 2 len <> bytes)
                (pbyte 0x5a <> pbigEndian 4 len <> bytes)
            )
        )

-- | A one-byte string holding the given literal.
pbyte :: forall (s :: S). Integer -> Term s PByteString
pbyte n = pconsBS' # pconstant n # pconstant ""

-- | Aiken @bytearray.from_int_big_endian@, with the width fixed at the call site.
pbigEndian :: forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PByteString
pbigEndian width n = pintegerToByteString # pmostSignificantFirst # width # n

{- | Aiken's @cbor.serialise@ applied to an integer.

@cbor.serialise@ encodes the value's @Data@ representation, and an @Int@'s is
@I n@, so this is the plain CBOR integer encoding — major type 0 for
non-negatives and 1 for negatives, and a bignum tag beyond 64 bits, which the
decoders above do not read. That last part matches Aiken: its decoders stop at
the eight-byte forms too.
-}
pcborInt :: forall (s :: S). Term s PInteger -> Term s PByteString
pcborInt n = pserialiseData # pforgetData (pdata n)
