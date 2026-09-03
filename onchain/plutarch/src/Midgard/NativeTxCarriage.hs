{- |
Module      : Midgard.NativeTxCarriage
Description : Plutarch port of @lib/midgard/native-tx-carriage-v1.ak@.

The publication half of the §8 field-preimage carriage ladder — §8.4 to §8.7.

"Midgard.NativeTxFieldAccess" is the __consumer__ half: it takes carriage that
already exists and turns it into an authenticated view. This module is the
__producer__ half, and it owns two things.

__The deterministic split rule (§8.4).__ Chunk @j@ is bytes @[j·K, (j+1)·K)@ with
a ragged last chunk and minimum-necessary chunks, a pure function of the preimage
bytes. That is what makes independent publishers byte-compatible — identical
chunks, identical digest vectors, interchangeable certificates — and it is why
§8.7's content addressing works at all: two publishers who never met produce the
same content address for the same bytes, so anyone's republication heals anyone's
certificate.

__The certification predicate (§8.6).__ Everything the permissionless minting
policy must prove about a certificate before a token may exist for it. The
transaction-shape wrapper lives in the validator; the content rules live here so
they are reachable by a focused test without building a transaction.

=== Positional identity stays internal

Certification needs the field commitment the tx-id committed, and §4 says an
expected hash may only come from a committed structure in view. So this module
takes the committed structures as CBOR, re-derives the tx-id from them through
the unchanged §3 derivation, and reads the field hash out of the result itself.
No caller hands in a free-standing field hash and none is handed back —
'pfieldCommitmentAt' is not exported, exactly as the Aiken table is private, and
for the same reason: keeping the table behind the predicate is how §4 is enforced
rather than merely documented.

=== The pieces written twice, and why

'Midgard.NativeTxFieldAccess' owns everything with a wire consequence and those
are imported rather than restated. Four things are deliberately duplicated in the
Aiken tree and stay duplicated here: the §2.5 positional table, the raw-chunk
datum read, the chunk-shape check and the slicing helper. The Aiken module
records the reasoning at length; the one worth repeating is that
'pchunkShapeMatches' is written to be __total__ — false for every input that is
not the §8.4 shape, including inputs its current caller cannot produce — because
this is the copy a minting policy runs, where a certificate is brought into
existence rather than merely read.

=== §2.4's transposition is live

Wire position 6 is __script__ witnesses and 7 is __address__ witnesses, the
opposite of the record's declaration order. A table that transposed only one of
the pair, or both, fails a different test — both directions are driven.
-}
module Midgard.NativeTxCarriage (
  -- * The mint redeemer
  PFieldPreimageCertificateMintRedeemerV1 (..),

  -- * §8.4 the deterministic split
  psplitFieldPreimage,
  pfieldPreimageChunkDigests,
  pfieldPreimageCertificateV1,

  -- * §8.5 raw carriage
  prawChunkBytes,

  -- * §8.6 certification
  pverifyFieldPreimageCertificateV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3 (
  POutputDatum (..),
  PPubKeyHash,
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Compact (
  pdecodeNativeTxWitnessSetCompact,
  pverifyNativeTxCompactCborV1,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.NativeTxFieldAccess (
  PFieldPreimageCertificateV1 (..),
  pchunkBytesK,
  pexpectedChunkCount,
  pfieldCommitment,
  pfieldPreimageCertificateAssetName,
  pmaxTier3ChunkCount,
  pmaxTransactionAggregateFieldBytes,
 )

--------------------------------------------------------------------------------
-- Wire types
--------------------------------------------------------------------------------

{- | Aiken @native_tx_carriage_v1.FieldPreimageCertificateMintRedeemerV1@.

Constructor order is frozen consensus wire format — @Certify@ is @Constr 0@,
@Retire@ @Constr 1@ — because an off-chain minter emits these tags and the
compiled policy branches on them.

@Certify@ carries no identity: publication and certification are permissionless
(§8.7), so the policy checks content and never who supplied it. What it carries
is the committed structures the field hash is extracted from, and the positional
order of the raw chunks.
-}
data PFieldPreimageCertificateMintRedeemerV1 (s :: S)
  = PCertify
      { pcertify'compactCbor :: Term s (PAsData PByteString)
      , pcertify'witnessSetCompactCbor :: Term s (PAsData PByteString)
      , pcertify'chunkRefInputIndices :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
      , pcertify'outputIndex :: Term s (PAsData PInteger)
      }
  | PRetire
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFieldPreimageCertificateMintRedeemerV1)

--------------------------------------------------------------------------------
-- §8.4 the deterministic split rule
--------------------------------------------------------------------------------

{- | Aiken @chunk_slice@ — the only slice in this module.

'psliceBS' clamps out-of-range arguments — the §7.3 abort-never-clamp hazard — so
the bound is checked first and the read fails closed.
-}
pchunkSlice ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PByteString)
pchunkSlice = phoistAcyclic $
  plam $ \bytes offset len ->
    pif
      (0 #<= offset #&& 0 #< len #&& offset + len #<= plengthBS # bytes)
      (psliceBS # offset # len # bytes)
      perror

{- | Aiken @split_field_preimage@.

§8.4's deterministic split, reproducible from the bytes alone — no length table,
no publisher state, no transaction context. That is the whole basis of §8.7's
content addressing and of healing: an unrelated party who obtains the same
preimage produces the same chunks, so re-publishing after a yank produces
carriage the existing certificate still describes.

Defined for any preimage from one byte up to the §5.4 aggregate cap. The tier-3
lower bound is a /certificate/ rule, not a splitting rule, and is enforced where
certificates are made and consumed.
-}
psplitFieldPreimage ::
  forall (s :: S).
  Term s (PByteString :--> PBuiltinList (PAsData PByteString))
psplitFieldPreimage = phoistAcyclic $
  plam $ \preimage ->
    plet (plengthBS # preimage) $ \totalLength ->
      pif
        (0 #< totalLength #&& totalLength #<= pmaxTransactionAggregateFieldBytes)
        (psplitFrom # preimage # 0 # totalLength)
        perror

psplitFrom ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData PByteString)
    )
psplitFrom = phoistAcyclic $
  pfix $ \self -> plam $ \preimage offset totalLength ->
    plet (totalLength - offset) $ \remaining ->
      pif
        (remaining #<= pchunkBytesK)
        (psingleton #$ pdata (pchunkSlice # preimage # offset # remaining))
        ( pcons
            # pdata (pchunkSlice # preimage # offset # pchunkBytesK)
            # (self # preimage # (offset + pchunkBytesK) # totalLength)
        )

-- | Aiken @field_preimage_chunk_digests@ — @blake2b_256@ of each §8.4 chunk, in order.
pfieldPreimageChunkDigests ::
  forall (s :: S).
  Term s (PByteString :--> PBuiltinList (PAsData PByteString))
pfieldPreimageChunkDigests = phoistAcyclic $
  plam $ \preimage ->
    pmap
      # plam (\chunk -> pdata (pblake2b_256 # pfromData chunk))
      # (psplitFieldPreimage # preimage)

{- | Aiken @field_preimage_certificate_v1@.

The certificate a publisher of @preimage@ should build — the producer twin of
'pverifyFieldPreimageCertificateV1', derived from the bytes and the
@(tx_id, field_index)@ binding alone.

@total_length > K@ is the §8.4 tier boundary, enforced here as well as at
consumption so that a certificate for a preimage that fits tier 1 or tier 2
cannot be brought into existence at all. The tiering is a partition, not a
preference.

The asset-name call is bound rather than discarded because it is what enforces
@field_index@ in @0..8@ and a 32-byte @tx_id@ — bound-forcing, not decoration,
exactly as in the original.
-}
pfieldPreimageCertificateV1 ::
  forall (s :: S).
  Term
    s
    ( PPubKeyHash
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PFieldPreimageCertificateV1
    )
pfieldPreimageCertificateV1 = phoistAcyclic $
  plam $ \owner txId fieldIndex preimage ->
    plet (plengthBS # preimage) $ \totalLength ->
      pif
        ( pchunkBytesK
            #< totalLength
            #&& (plengthBS # pto owner #== 28)
            #&& ( (plengthBS #$ pfieldPreimageCertificateAssetName # txId # fieldIndex)
                    #== 32
                )
        )
        `flip` perror
        $ pcon
          PFieldPreimageCertificateV1
            { pcert'owner = pdata owner
            , pcert'txId = pdata txId
            , pcert'fieldIndex = pdata fieldIndex
            , pcert'totalLength = pdata totalLength
            , pcert'chunkDigests = pdata (pfieldPreimageChunkDigests # preimage)
            }

--------------------------------------------------------------------------------
-- §2.5 positional hash extraction (internal by design)
--------------------------------------------------------------------------------

{- | Aiken @field_commitment_at@ — §2.5's field-index table.

Private for the same reason the door's copy is: an expected field hash may only
come from a committed structure in view (§4), and keeping the table behind the
predicate is how that is enforced rather than merely documented.

Note positions 6 and 7: §2.4 transposes them relative to the record's declaration
order, so wire 6 is /script/ witnesses and wire 7 is /address/ witnesses.
-}
pfieldCommitmentAt ::
  forall (s :: S).
  Term s PNativeTxBodyCompact ->
  Term s PNativeTxWitnessSetCompact ->
  Term s PInteger ->
  Term s PByteString
pfieldCommitmentAt body witnessSet fieldIndex = P.do
  PNativeTxBodyCompact
    { pbodyCompact'spendInputsHash
    , pbodyCompact'referenceInputsHash
    , pbodyCompact'outputsHash
    , pbodyCompact'requiredObserversHash
    , pbodyCompact'requiredSignersHash
    , pbodyCompact'mintHash
    } <-
    pmatch body
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
                  pif
                    (fieldIndex #== 8)
                    (pfromData pwitnessSetCompact'redeemerTxWitsHash)
                    perror

--------------------------------------------------------------------------------
-- §8.5 raw carriage
--------------------------------------------------------------------------------

{- | Aiken @raw_chunk_bytes@.

§8.5: raw carriage is unauthenticated data published as a nothing-but-bytes
inline datum. Nothing here trusts provenance — the bytes are verified by hash
afterwards, so wrong bytes simply fail and a chunk UTxO may sit at any address,
under any key, published by anyone.
-}
prawChunkBytes ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo) :--> PInteger :--> PByteString)
prawChunkBytes = phoistAcyclic $
  plam $ \referenceInputs index ->
    pif (0 #<= index) `flip` perror $ P.do
      PTxInInfo {ptxInInfo'resolved} <- pmatch (pfromData (pelemAt # index # referenceInputs))
      PTxOut {ptxOut'datum} <- pmatch ptxInInfo'resolved
      pmatch ptxOut'datum $ \case
        POutputDatum {poutputDatum'outputDatum} -> pasByteStr # pto poutputDatum'outputDatum
        _ -> perror

--------------------------------------------------------------------------------
-- §8.6 certification
--------------------------------------------------------------------------------

{- | Aiken @verify_field_preimage_certificate_v1@.

Everything §8.6 requires of a certificate before a token may exist for it.

__The order matters.__ 'pverifyNativeTxCompactCborV1' re-derives the tx-id from
the supplied body through the unchanged §3 derivation and compares it to the
certificate's @tx_id@, so a certificate naming another transaction is refused
before any byte of carriage is looked at. A certificate is a statement about one
transaction's one field, and it is that binding — not the bytes — that is checked
first.

The witness set is required and checked for /every/ field, not only 6–8. The door
skips it below field 6 because it re-hashes on every dispute step; certification
happens once per field per transaction, so one code path with no conditional
authentication is worth more than one hash.

__The chunks are checked three ways and all three are needed.__ Their /shape/ is
what makes them the §8.4 split rather than an arbitrary partition, so offset math
over the digest vector is arithmetic rather than a search; their /digests/ are
what a later single-chunk read authenticates against; and their /concatenation/
is what ties the whole thing to the field commitment the tx-id committed.
Dropping the shape check would let a publisher certify a lopsided split whose
boundaries no consumer could compute; dropping the per-chunk digests would leave
§8.6's one-chunk-hash access with nothing to compare against.
-}
pverifyFieldPreimageCertificateV1 ::
  forall (s :: S).
  Term
    s
    ( PFieldPreimageCertificateV1
        :--> PByteString
        :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PBool
    )
pverifyFieldPreimageCertificateV1 = phoistAcyclic $
  plam $ \certificate compactCbor witnessSetCompactCbor chunks -> P.do
    PFieldPreimageCertificateV1
      { pcert'owner
      , pcert'txId
      , pcert'fieldIndex
      , pcert'totalLength
      , pcert'chunkDigests
      } <-
      pmatch certificate
    totalLength <- plet $ pfromData pcert'totalLength
    chunkDigests <- plet $ pfromData pcert'chunkDigests
    -- The minter sets the min-Ada reclaim authority and nothing checks who that
    -- is, but it has to be a spendable key hash or the output is dead.
    pif (plengthBS # pto (pfromData pcert'owner) #== 28) `flip` perror $ P.do
      -- The binding: the tx-id is re-derived, never accepted.
      PVerifiedMidgardNativeTxCompact {pverified'txCompact} <-
        pmatch (pverifyNativeTxCompactCborV1 # pfromData pcert'txId # compactCbor)
      PNativeTxCompact {pcompact'body, pcompact'witnessSetHash} <- pmatch pverified'txCompact
      pif (pblake2b_256 # witnessSetCompactCbor #== pcompact'witnessSetHash) `flip` perror $
        plet (pdecodeNativeTxWitnessSetCompact # witnessSetCompactCbor) $ \witnessSet ->
          plet (pfieldCommitmentAt pcompact'body witnessSet (pfromData pcert'fieldIndex)) $
            \expectedHash ->
              plet (plength # chunkDigests) $ \chunkCount ->
                pif
                  ( pchunkBytesK
                      #< totalLength
                      #&& (totalLength #<= pmaxTransactionAggregateFieldBytes)
                      #&& (chunkCount #== pexpectedChunkCount # totalLength)
                      #&& (chunkCount #<= pmaxTier3ChunkCount)
                      #&& (plength # chunks #== chunkCount)
                      #&& (pchunkShapeMatches # chunks # totalLength)
                      #&& ( (pmap # plam (\c -> pdata (pblake2b_256 # pfromData c)) # chunks)
                              #== chunkDigests
                          )
                  )
                  `flip` perror
                  $ (pfieldCommitment #$ pconcatChunks # chunks)
                    #== expectedHash

{- | Aiken @chunk_shape_matches@.

Every chunk but the last is exactly @K@, and the last is ragged but never empty.
Together with 'pexpectedChunkCount' this pins the partition to the one the split
rule produces, so chunk @j@ really is bytes @[j·K, (j+1)·K)@ and a consumer can
locate an offset arithmetically.

Written to be __total__ — the empty list is @False@ here, and the last chunk's
length is bounded on both sides — even though the caller's guards make both
unreachable. This is the copy a minting policy runs, where a certificate is
brought into existence rather than merely read, so it is written to stay correct
if the guard order above is ever rearranged. The door's copy leans on its
caller's guards instead; the divergence is documented at both sites.
-}
pchunkShapeMatches ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PByteString) :--> PInteger :--> PBool)
pchunkShapeMatches = phoistAcyclic $
  pfix $ \self -> plam $ \chunks remaining ->
    pelimList
      ( \chunk rest ->
          pelimList
            -- A non-empty tail: this chunk must be exactly K.
            (\_ _ -> (plengthBS # pfromData chunk #== pchunkBytesK) #&& (self # rest # (remaining - pchunkBytesK)))
            -- The last chunk: ragged, but never empty and never over K.
            ( 0
                #< remaining
                #&& (remaining #<= pchunkBytesK)
                #&& (plengthBS # pfromData chunk #== remaining)
            )
            rest
      )
      (pconstant False)
      chunks

-- | Aiken @concat_chunks@ — a left fold, so the chunks join in order.
pconcatChunks ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PByteString) :--> PByteString)
pconcatChunks = phoistAcyclic $
  plam $ \chunks ->
    pfoldl # plam (\joined chunk -> joined <> pfromData chunk) # pconstant "" # chunks
