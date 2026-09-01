{- |
Module      : Midgard.NativeTxFieldAccess
Description : Plutarch port of @lib/midgard/native-tx-field-access-v1.ak@.

Positional access to a Midgard transaction's nine committed fields — the spec's
§4, §5, §7 and §8. This module is the single consumer-visible access idiom for
those fields: a caller obtains a 'PFieldViewV1' from the one
'pauthenticatedFieldView' door and then reads items through the slice-only
accessors. Nothing else at this layer opens a field.

Four properties the door owns, so that no caller has to.

__Positional hash extraction is internal__ (§2.5, §4). Callers hand over the
committed compact structures, never a free-standing field hash, so the
positional-identity invariant cannot be side-stepped at a dispute entry point.
'pfieldCommitmentAt' is private for exactly that reason.

For the three witness-set fields the door re-derives the witness-set hash and
checks it against the @witness_set_hash@ the supplied compact structure carries.
__That is half of the binding, not all of it.__ §3's transaction-id preimage is
the /body alone/, so a compact structure's trailing @witness_set_hash@ is not
covered by the id: a caller that re-derived the structure from redeemer bytes
has authenticated nothing about it. The other half is the caller's, and for a
downstream family step it comes from thread state — the value its own step-01
read off the block-committed structure. A caller holding structures straight
from @pass_native_tx_to_next_step@ already has them authenticated and owes
nothing further.

__Abort, never clamp__ (§7.3). @sliceByteString@ clamps, and two clamped
out-of-range reads are byte-equal — which would fabricate equality evidence out
of a perfectly valid block. Every read goes through 'psliceExact', which fails
closed.

__Straddle awareness__ (§8.4, §8.8). Under tier-3 carriage an item's byte range
can cross a chunk boundary; reads stitch across chunks and verify each chunk
they touch against the certificate's digest vector. A chunk nobody reads is
never hashed.

__One grammar, one verdict__ (§5.1, §6.1). 'pdecodeFieldArrayHeaderAt' and
'pitemHeaderAt' are the only readers of the §5.1 heads, and both enforce the
narrow acceptance set: minimal width, capped at @99 NNNN@ / @59 LLLL@.
Fixed-stride access reads the item wrapper too rather than inferring it from the
stride, so one logical field has exactly one admissible commitment.

=== Why the header decoder lives here and not in @codec@

§5.1's acceptance set is /narrower/ than CBOR's: minimal width only, capped at
the @99 NNNN@ form. The four-byte @9a@ head is well-formed CBOR and rejects here.
'Midgard.FraudProofs.NativeTx.Codec.pdecodeDefiniteArrayHeaderAt' is the general
reader and deliberately admits both — it is for structures /outside/ this
grammar, the compact-transaction array and the machine's work-witness array,
which pin their own widths at the call site.

Every §5.1 envelope in the tree decodes through 'pdecodeFieldArrayHeaderAt', so
the grammar has one verdict rather than two. The Aiken tree calls out the
consequence of getting this wrong: the mint field never re-encodes, so a second
lenient reader there would go uncaught.

=== Two counts, and why only one is exported

'pfieldItemCount' is the /authenticated/ count and it __aborts__ for a
variable-width field under tier-3 carriage: there the count exists only in the
§5.1 header and nothing affordable authenticates it. Tiers 1–2 authenticate
theirs by walking the whole preimage at view construction; tier 3 cannot run
that walk, because a chunked read re-verifies the chunk it lands in every time,
so an N-item walk costs N hashes over a whole chunk. Rather than hand back a
number nobody checked — which a count-consuming rule would take for a fact — the
door declines to answer.

'pdeclaredItemCount' is the header's own number with no authentication claim
attached. It is private and used only as the range guard on a read. What makes a
read sound is that the walk and the chunk digests behind it fail closed the
moment the read leaves the committed bytes, so an inflated count buys extra
indices that all abort and a deflated one only refuses reads.

=== Two known limits, both loud

'pchunkBytesK' reads @15900@ and __that is not the value of K__: the Phase-4
measurement refuted it, and §8.3 erratum E1 re-pins K to @15148@. The literal
stands because it is compiled into an acceptance predicate and into every chunk
boundary in the system. While the two disagree, preimages in @(15148, 15900]@
have no admissible carriage at all, and — because the chunker cuts at this
literal — no tier-3 preimage of any length can be published, since every tier-3
plan's first chunk is 264 bytes over @maxTxSize@.

Tier 3 is also refused outright for the witness-set fields, but that refusal
lives one layer up in @fraud-proofs/field-opening-v1.ak@ rather than here.
-}
module Midgard.NativeTxFieldAccess (
  -- * Shape constants
  pfieldCount,
  pmaxTransactionAggregateFieldBytes,
  pmaxSpendInputsPreimageBytes,
  pmaximumCardanoSpendRedeemerCount,
  pmaxFieldItemCount,
  pchunkBytesK,
  pmaxTier1RedeemerPreimageBytes,
  pmaxTier3ChunkCount,
  pfixedItemWrapperBytes,
  pspendInputItemBytes,
  pspendInputStride,
  phash28ItemBytes,
  phash28Stride,
  paddressWitnessItemBytes,
  paddressWitnessStride,
  pwalkDerivedStride,

  -- * Wire types (§8.8)
  PFieldCarriageV1 (..),
  PFieldViewV1 (..),
  PFieldPreimageCertificateV1 (..),
  pfieldPreimageCertificateAssetName,

  -- * The §5.1 envelope
  pencodeFieldArrayHeader,
  pencodeFieldPreimage,
  pdecodeFieldArrayHeader,
  pdecodeFieldArrayHeaderAt,
  pfieldStride,

  -- * Commitments
  pfieldCommitment,
  pfieldCommitmentFromItems,
  pemptyFieldCommitment,

  -- * The door
  pauthenticatedFieldView,
  pexpectedChunkCount,

  -- * Accessors
  pfieldItemCount,
  pfieldTotalLength,
  pfieldItemAt,
  pfieldItemExtent,
  pfieldViewStride,
  pfieldHeaderLen,
  pfieldReadRange,
  pfieldItemHeaderAt,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.ByteString (
  pintegerToByteString,
  pmostSignificantFirst,
 )
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Internal.Builtins (pconsBS')
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  POutputDatum (..),
  PPubKeyHash,
  PTokenName (..),
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottStruct (..))
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.NativeTx.Codec (pbyteAt, pencodeDefiniteBytes, psliceLen)
import Midgard.FraudProofs.NativeTx.Compact (pencodeNativeTxWitnessSetCompact)
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )

--------------------------------------------------------------------------------
-- Shape constants
--------------------------------------------------------------------------------

{- | Aiken @native_tx_field_access_v1.field_count@ — @9@.

A Midgard transaction has exactly nine fields: spend inputs, reference inputs,
outputs, required observers, required signers, mint, script witnesses, address
witnesses, and redeemers. Field identity is /positional/, and this is the tree's
single definition of the arity — the retired counted modules each used to
restate it.
-}
pfieldCount :: forall (s :: S). Term s PInteger
pfieldCount = 9

{- | Aiken @native_tx_field_access_v1.max_transaction_aggregate_field_bytes@ — @32768@.

§5.4. Retained from the counted era by owner ruling: tightening it would be a
capability change outside the reversion's scope.
-}
pmaxTransactionAggregateFieldBytes :: forall (s :: S). Term s PInteger
pmaxTransactionAggregateFieldBytes = 32768

-- | Aiken @native_tx_field_access_v1.max_spend_inputs_preimage_bytes@ — field 0's own bound equals the aggregate bound.
pmaxSpendInputsPreimageBytes :: forall (s :: S). Term s PInteger
pmaxSpendInputsPreimageBytes = 32768

-- | Aiken @native_tx_field_access_v1.maximum_cardano_spend_redeemer_count@ — §5.4, a Cardano shape bound.
pmaximumCardanoSpendRedeemerCount :: forall (s :: S). Term s PInteger
pmaximumCardanoSpendRedeemerCount = 296

{- | Aiken @native_tx_field_access_v1.max_field_item_count@ — @65535@.

§5.1 caps the item-count header at the @99 NNNN@ form, so this is not an
independent policy bound but the largest count the grammar can spell.
-}
pmaxFieldItemCount :: forall (s :: S). Term s PInteger
pmaxFieldItemCount = 65535

{- | Aiken @native_tx_field_access_v1.chunk_bytes_k@ — @15900@.

The §8.3 tier-2 bound and tier-3 chunk size. __This literal is not the value of
K__: the Phase-4 measurement refuted it (a real signed publication of a
15,900-byte chunk measures 16,648 bytes against a 16,384-byte @maxTxSize@) and
§8.3 erratum E1 re-pins K to 15,148. The literal stands here because it is
compiled into an acceptance predicate and into every chunk boundary in the
system; see the module header for what the divergence costs while it lasts.
-}
pchunkBytesK :: forall (s :: S). Term s PInteger
pchunkBytesK = 15900

{- | Aiken @native_tx_field_access_v1.max_tier1_redeemer_preimage_bytes@ — @14336@.

The §8.3 tier-1 bound. Provisional on the same footing as 'pchunkBytesK':
@maxTxSize@ less a round 2,048-byte allowance for step machinery.
-}
pmaxTier1RedeemerPreimageBytes :: forall (s :: S). Term s PInteger
pmaxTier1RedeemerPreimageBytes = 14336

-- | Aiken @native_tx_field_access_v1.max_tier3_chunk_count@ — @ceil(32768 / 15900)@.
pmaxTier3ChunkCount :: forall (s :: S). Term s PInteger
pmaxTier3ChunkCount = 3

{- | Aiken @native_tx_field_access_v1.fixed_item_wrapper_bytes@ — @2@.

§5.3: each fixed-width item carries a two-byte @58 LL@ wrapper, so
@stride = 2 + item bytes@.
-}
pfixedItemWrapperBytes :: forall (s :: S). Term s PInteger
pfixedItemWrapperBytes = 2

{- | Aiken @native_tx_field_access_v1.spend_input_item_bytes@ — @38@.

§5.3 fields 0 and 1: @82 ‖ 58 20 tx_id ‖ 19 index_be16@. The fixed three-byte
output index is what makes the item width constant.
-}
pspendInputItemBytes :: forall (s :: S). Term s PInteger
pspendInputItemBytes = 38

-- | Aiken @native_tx_field_access_v1.spend_input_stride@ — @40@.
pspendInputStride :: forall (s :: S). Term s PInteger
pspendInputStride = 40

{- | Aiken @native_tx_field_access_v1.hash28_item_bytes@ — @28@.

§5.3 fields 3 and 4 carry a raw 28-byte hash per item, which is what fixes their
stride at 30 and makes item access arithmetic rather than a walk.
-}
phash28ItemBytes :: forall (s :: S). Term s PInteger
phash28ItemBytes = 28

-- | Aiken @native_tx_field_access_v1.hash28_stride@ — @30@.
phash28Stride :: forall (s :: S). Term s PInteger
phash28Stride = 30

-- | Aiken @native_tx_field_access_v1.address_witness_item_bytes@ — §5.3 field 7, @82 ‖ 58 20 vkey ‖ 58 40 signature@.
paddressWitnessItemBytes :: forall (s :: S). Term s PInteger
paddressWitnessItemBytes = 101

-- | Aiken @native_tx_field_access_v1.address_witness_stride@ — @103@.
paddressWitnessStride :: forall (s :: S). Term s PInteger
paddressWitnessStride = 103

{- | Aiken @native_tx_field_access_v1.walk_derived_stride@ — @0@.

Fields 2, 5, 6 and 8 are variable-width: top-level access walks the envelope
instead of multiplying a stride, and this sentinel is how a view says so.
-}
pwalkDerivedStride :: forall (s :: S). Term s PInteger
pwalkDerivedStride = 0

--------------------------------------------------------------------------------
-- Wire types (§8.8)
--------------------------------------------------------------------------------

{- | Aiken @native_tx_field_access_v1.FieldCarriageV1@.

How a field's preimage bytes reach the consuming transaction (§8.1–§8.4).
Constructor order is frozen consensus wire format: @Inline@ is @Constr 0@,
@RawUtxo@ 1, @Certified@ 2, and off-chain builders emit exactly these tags.
-}
data PFieldCarriageV1 (s :: S)
  = -- | Tier 1 — the step's own redeemer carries the preimage.
    PInline {pinline'preimage :: Term s (PAsData PByteString)}
  | -- | Tier 2 — one nothing-but-bytes inline datum at the prover's key
    -- address, named by its positional reference-input index.
    PRawUtxo {prawUtxo'refInputIndex :: Term s (PAsData PInteger)}
  | -- | Tier 3 — deterministic fixed-'pchunkBytesK' chunks plus one certified
    -- digest manifest. @chunkRefInputIndices@ is all-chunks-positional:
    -- element @k@ is the reference-input index of chunk @k@.
    PCertified
      { pcertified'certRefInputIndex :: Term s (PAsData PInteger)
      , pcertified'chunkRefInputIndices :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFieldCarriageV1)

{- | Aiken @native_tx_field_access_v1.FieldViewV1@.

An authenticated field, ready to slice. The carriage tier is an encoding detail
that validator logic never branches on — the accessors read both variants.

Scott-encoded rather than data-encoded: a view is produced by the door and
consumed by the accessors inside one script, and never crosses a data boundary.
-}
data PFieldViewV1 (s :: S)
  = -- | Tiers 1–2: the whole preimage is present and hash-checked.
    PWholeView
      { pwhole'bytes :: Term s PByteString
      , pwhole'count :: Term s PInteger
      , pwhole'stride :: Term s PInteger
      }
  | -- | Tier 3: chunks are present but unhashed until touched; the digests and
    -- the item count come from the mint-verified certificate.
    PChunkedView
      { pchunked'chunks :: Term s (PBuiltinList PByteString)
      , pchunked'chunkDigests :: Term s (PBuiltinList PByteString)
      , pchunked'count :: Term s PInteger
      , pchunked'stride :: Term s PInteger
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottStruct PFieldViewV1)

{- | Aiken @native_tx_field_access_v1.FieldPreimageCertificateV1@.

§8.6's tier-3 digest manifest, minted by the permissionless certificate
validator and consumed here. It arrives as an inline datum, so it is
data-encoded — a record, hence @Constr 0@.
-}
data PFieldPreimageCertificateV1 (s :: S) = PFieldPreimageCertificateV1
  { pcert'owner :: Term s (PAsData PPubKeyHash)
  , pcert'txId :: Term s (PAsData PByteString)
  , pcert'fieldIndex :: Term s (PAsData PInteger)
  , pcert'totalLength :: Term s (PAsData PInteger)
  , pcert'chunkDigests :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFieldPreimageCertificateV1)

{- | Aiken @native_tx_field_access_v1.field_preimage_certificate_asset_name@.

@blake2b_256(field_index_byte ‖ tx_id)@ — a 33-byte preimage whose leading byte
is the 0..8 field index and whose remaining 32 are the transaction id. The
single-byte prefix is domain separation, not a length header: with both bounds
enforced the preimage is unambiguous, which is why both are checked here rather
than assumed of the caller.

Fixed because both sides of the tier-3 handshake — the minting policy that
certifies and the door that consumes — must agree on one derivation.
-}
pfieldPreimageCertificateAssetName ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PByteString)
pfieldPreimageCertificateAssetName = phoistAcyclic $
  plam $ \txId fieldIndex ->
    plet (pexpectFieldIndex # fieldIndex) $ \index ->
      pif
        (plengthBS # txId #== 32)
        (pblake2b_256 #$ pconsBS' # index # txId)
        perror

--------------------------------------------------------------------------------
-- The §5.1 envelope
--------------------------------------------------------------------------------

-- | Aiken @native_tx_field_access_v1.expect_field_index@ — @0 <= i < 9@, aborting otherwise.
pexpectFieldIndex :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectFieldIndex = phoistAcyclic $
  plam $ \fieldIndex ->
    pif (0 #<= fieldIndex #&& fieldIndex #< pfieldCount) fieldIndex perror

{- | Aiken @native_tx_field_access_v1.encode_field_array_header@.

Minimal width, capped at @99 NNNN@. Note the ladder stops two forms short of
CBOR's: there is no four-byte head in this grammar at all.
-}
pencodeFieldArrayHeader :: forall (s :: S). Term s (PInteger :--> PByteString)
pencodeFieldArrayHeader = phoistAcyclic $
  plam $ \count ->
    pif (count #< 0) perror $
      pif
        (count #<= 23)
        (pconsBS' # (128 + count) # pconstant "")
        ( pif
            (count #<= 255)
            (pconstant "\x98" <> pbigEndian 1 count)
            ( pif
                (count #<= pmaxFieldItemCount)
                (pconstant "\x99" <> pbigEndian 2 count)
                perror
            )
        )

{- | Aiken @native_tx_field_access_v1.encode_field_preimage@.

The header followed by one definite byte string per item — so an empty field is
exactly @80@, and every field, mint included, has the same shape. The uniform
envelope is what buys an O(1) top-level skip: one head decode plus a byte jump
per item, with a single walk implementation serving every field dispute.
-}
pencodeFieldPreimage ::
  forall (s :: S). Term s (PBuiltinList PByteString :--> PByteString)
pencodeFieldPreimage = phoistAcyclic $
  plam $ \items ->
    (pencodeFieldArrayHeader # (plength # items)) <> (pencodeItems # items)

-- | The item run behind 'pencodeFieldPreimage'.
pencodeItems ::
  forall (s :: S). Term s (PBuiltinList PByteString :--> PByteString)
pencodeItems = phoistAcyclic $
  pfix $ \self -> plam $ \items ->
    pelimList
      (\item rest -> (pencodeDefiniteBytes # item) <> (self # rest))
      (pconstant "")
      items

-- | Aiken @native_tx_field_access_v1.decode_field_array_header@ — at offset zero.
pdecodeFieldArrayHeader ::
  forall (s :: S). Term s (PByteString :--> PPair PInteger PInteger)
pdecodeFieldArrayHeader = phoistAcyclic $
  plam $ \preimage -> pdecodeFieldArrayHeaderAt # preimage # 0

{- | Aiken @native_tx_field_access_v1.decode_field_array_header_at@.

The one §5.1 header decoder. Returns @(offset just past the header, N)@.

The two @expect@s inside are the minimality rule: a count of 24 or more must not
be spelt in the packed form, and a count above 255 must not be spelt in the
one-byte form. Without them the same field would have several admissible
preimages and therefore several commitments.
-}
pdecodeFieldArrayHeaderAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeFieldArrayHeaderAt = phoistAcyclic $
  plam $ \bytes offset ->
    plet (pbyteAt # bytes # offset) $ \tag ->
      pif
        (128 #<= tag #&& tag #<= 151)
        (pcon (PPair (offset + 1) (tag - 128)))
        ( pif
            (tag #== 152)
            ( plet (pbyteAt # bytes # (offset + 1)) $ \count ->
                pif (count #< 24) perror (pcon (PPair (offset + 2) count))
            )
            ( pif
                (tag #== 153)
                ( plet
                    ( (pbyteAt # bytes # (offset + 1)) * 256
                        + (pbyteAt # bytes # (offset + 2))
                    )
                    $ \count ->
                      pif (count #<= 0xff) perror (pcon (PPair (offset + 3) count))
                )
                perror
            )
        )

{- | Aiken @native_tx_field_access_v1.field_stride@.

§5.3's stride table. Fields 0 and 1 are spend-input shaped, 3 and 4 are raw
28-byte hashes, 7 is the address witness; everything else is variable-width and
answers 'pwalkDerivedStride'.
-}
pfieldStride :: forall (s :: S). Term s (PInteger :--> PInteger)
pfieldStride = phoistAcyclic $
  plam $ \fieldIndex ->
    plet (pexpectFieldIndex # fieldIndex) $ \index ->
      pif (index #== 0 #|| index #== 1) pspendInputStride $
        pif (index #== 3 #|| index #== 4) phash28Stride $
          pif (index #== 7) paddressWitnessStride pwalkDerivedStride

--------------------------------------------------------------------------------
-- Commitments
--------------------------------------------------------------------------------

{- | Aiken @native_tx_field_access_v1.field_commitment@.

§4 — plain hashing. No domain tag, no version prefix, no field index in the hash
input, so a watcher needs the raw bytes and @blake2b_256@ and nothing else.

The absence of a field index is deliberate and has a visible consequence: all
nine empty fields share one commitment, 'pemptyFieldCommitment'.
-}
pfieldCommitment :: forall (s :: S). Term s (PByteString :--> PByteString)
pfieldCommitment = phoistAcyclic $ plam $ \preimage -> pblake2b_256 # preimage

-- | Aiken @native_tx_field_access_v1.field_commitment_from_items@.
pfieldCommitmentFromItems ::
  forall (s :: S). Term s (PBuiltinList PByteString :--> PByteString)
pfieldCommitmentFromItems = phoistAcyclic $
  plam $ \items -> pfieldCommitment #$ pencodeFieldPreimage # items

{- | Aiken @native_tx_field_access_v1.empty_field_commitment@.

@field_commitment(#"80")@, pinned as a literal because emptiness comparisons are
dense in the validation machine and each recomputation re-ran @blake2b_256@ over
the same one byte. The Plutarch test suite proves the literal against the
producer, as the Aiken test file does, so the pin cannot drift.
-}
pemptyFieldCommitment :: forall (s :: S). Term s PByteString
pemptyFieldCommitment =
  pconstant
    "\x45\xb0\xcf\xc2\x20\xce\xec\x5b\x7c\x1c\x62\xc4\xd4\x19\x3d\x38\
    \\xe4\xeb\xa4\x8e\x88\x15\x72\x9c\xe7\x5f\x9c\x0a\xb0\xe4\xc1\xc0"

--------------------------------------------------------------------------------
-- §7.3 abort-never-clamp
--------------------------------------------------------------------------------

{- | Aiken @native_tx_field_access_v1.slice_exact@.

The only slice in this module. The builtin clamps out-of-range arguments, so the
bound is checked first and the read fails closed. Two clamped reads past the end
of a preimage are byte-equal, which is how a clamping read fabricates equality
evidence out of a perfectly valid block.
-}
psliceExact ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PInteger :--> PByteString)
psliceExact = phoistAcyclic $
  plam $ \bytes offset len ->
    pif
      (0 #<= offset #&& 0 #<= len #&& offset + len #<= plengthBS # bytes)
      (psliceLen # bytes # offset # len)
      perror

--------------------------------------------------------------------------------
-- §2.5 positional hash extraction (internal by design)
--------------------------------------------------------------------------------

{- | Aiken @native_tx_field_access_v1.field_commitment_at@.

§2.5's field-index table. Deliberately private: the positional-identity
invariant (§4) says an expected hash may only come from a committed structure in
view, and keeping this behind the door is how that is enforced rather than
merely documented.
-}
pfieldCommitmentAt ::
  forall (s :: S).
  Term
    s
    ( PNativeTxBodyCompact
        :--> PNativeTxWitnessSetCompact
        :--> PInteger
        :--> PByteString
    )
pfieldCommitmentAt = phoistAcyclic $
  plam $ \body witnessSet fieldIndex -> P.do
    index <- plet $ pexpectFieldIndex # fieldIndex
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
    pif (index #== 0) pbodyCompact'spendInputsHash $
      pif (index #== 1) pbodyCompact'referenceInputsHash $
        pif (index #== 2) pbodyCompact'outputsHash $
          pif (index #== 3) pbodyCompact'requiredObserversHash $
            pif (index #== 4) pbodyCompact'requiredSignersHash $
              pif (index #== 5) pbodyCompact'mintHash $
                pif (index #== 6) (pfromData pwitnessSetCompact'scriptTxWitsHash) $
                  pif
                    (index #== 7)
                    (pfromData pwitnessSetCompact'addrTxWitsHash)
                    (pfromData pwitnessSetCompact'redeemerTxWitsHash)

--------------------------------------------------------------------------------
-- The door
--------------------------------------------------------------------------------

{- | Aiken @native_tx_field_access_v1.authenticated_field_view@.

The single field-access door (§8.8). Authenticates field @fieldIndex@ of the
transaction committed by @verified@ against whichever carriage tier @carriage@
names, and returns a view the slice-only accessors can read.

Fields 6–8 additionally require @witnessSet@ to re-derive to the compact
structure's committed @witness_set_hash@, so a caller cannot substitute a
witness set the transaction id never committed — but see the module header for
why that is only half the binding.

§7.1 authenticate-once is the caller's to exploit: build the view once per field
and read it many times; an untouched field is never authenticated.

@certificatePolicyId@ is consulted only on tier 3, where a flat hash cannot
authenticate a chunk on its own and the certificate is therefore the binding:
the door requires the named reference input to actually hold that policy's
@(tx_id, field_index)@ token, so a look-alike UTxO at any address cannot pass
its datum off as a manifest.
-}
pauthenticatedFieldView ::
  forall (s :: S).
  Term
    s
    ( PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact
        :--> PInteger
        :--> PFieldCarriageV1
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PFieldViewV1
    )
pauthenticatedFieldView = phoistAcyclic $
  plam $ \verified witnessSet fieldIndex carriage referenceInputs certificatePolicyId -> P.do
    PVerifiedMidgardNativeTxCompact {pverified'txId, pverified'txCompact} <- pmatch verified
    PNativeTxCompact {pcompact'body, pcompact'witnessSetHash} <- pmatch pverified'txCompact
    pif
      ( 0
          #<= fieldIndex
          #&& fieldIndex
          #< pfieldCount
          -- Positional identity (§4): the witness-set fields are only readable
          -- once the supplied witness set re-derives to the `witness_set_hash`
          -- these compact structures carry. Lazy `#||` on purpose — Aiken's
          -- `or {}` short-circuits, so a body field never pays the hash.
          #&& ( fieldIndex
                  #< 6
                  #|| (pblake2b_256 #$ pencodeNativeTxWitnessSetCompact # witnessSet)
                  #== pcompact'witnessSetHash
              )
      )
      ( P.do
          expectedHash <- plet $ pfieldCommitmentAt # pcompact'body # witnessSet # fieldIndex
          stride <- plet $ pfieldStride # fieldIndex
          pmatch carriage $ \case
            PInline {pinline'preimage} ->
              pwholeView # pfromData pinline'preimage # expectedHash # stride
            PRawUtxo {prawUtxo'refInputIndex} ->
              pwholeView
                # (prawCarriageBytes # referenceInputs # pfromData prawUtxo'refInputIndex)
                # expectedHash
                # stride
            PCertified {pcertified'certRefInputIndex, pcertified'chunkRefInputIndices} ->
              pcertifiedView
                # pverified'txId
                # fieldIndex
                # stride
                # referenceInputs
                # pfromData pcertified'certRefInputIndex
                # pfromData pcertified'chunkRefInputIndices
                # certificatePolicyId
      )
      perror

{- | Aiken @native_tx_field_access_v1.whole_view@.

Tiers 1–2 (§8.1, §8.2): the whole preimage is in hand, so it is hashed once
against the positionally-extracted commitment and then structurally validated
before any accessor may touch it.

The split of structural work between here and the accessors is deliberate.
Construction proves what is O(1) or already O(N)-unavoidable: the header is
minimal, and the declared count agrees with the byte length — by arithmetic for
the fixed-stride fields (§7.4), by a full walk for the variable-width ones,
which is the only way to know where their items end. Per-item wrapper
canonicality for the fixed-stride fields is proved at each read instead
('pfieldItemExtent'), so a field with 296 items does not pay 296 wrapper decodes
to answer one dispute.
-}
pwholeView ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PFieldViewV1)
pwholeView = phoistAcyclic $
  plam $ \preimage expectedHash stride -> P.do
    totalLength <- plet $ plengthBS # preimage
    PPair headerLen count <- pmatch (pdecodeFieldArrayHeader # preimage)
    pif
      ( (pfieldCommitment # preimage)
          #== expectedHash
          #&& totalLength
          #<= pmaxTransactionAggregateFieldBytes
          #&& pif
            (stride #> pwalkDerivedStride)
            -- §7.4 count consistency, arithmetic form.
            (headerLen + stride * count #== totalLength)
            -- §5.1 fail-closed: the walked content must account for exactly the
            -- declared item count and leave no trailing bytes.
            ((pwalkToEnd # preimage # headerLen # count) #== totalLength)
      )
      ( pcon
          ( PWholeView
              {pwhole'bytes = preimage, pwhole'count = count, pwhole'stride = stride}
          )
      )
      perror

{- | Aiken @native_tx_field_access_v1.certified_view@.

Tier 3 (§8.4): chunks stay unhashed until an accessor touches them. The
certificate is mint-verified, so @total_length@ and the digest vector are
authenticated data; @(tx_id, field_index)@ is matched against the
already-authenticated disputed transaction, never against redeemer-supplied
identity (§8.6).

__Why no walk to the end here.__ Tier 3 exists precisely because the field is
larger than one chunk, and §8.4's guarantee is that reaching one item costs one
chunk hash (two when the item straddles). Walking a variable-width field to its
end at construction would cost far more than hashing the preimage once, because
a chunked read re-verifies the chunk it lands in on /every/ read. So the §5.1
count-consistency check 'pwholeView' runs is not available, and this does not
pretend otherwise: a variable-width field's header count is bounded here but not
authenticated, which is why 'pfieldItemCount' refuses to answer for one.

The @total_length > K@ lower bound is what makes the ladder a partition rather
than a preference: a field that fits tier 1 or tier 2 has exactly one admissible
carriage. Without it a single-chunk "certificate" authenticates a preimage of
any size, and every structural check tiers 1–2 run at view construction can be
side-stepped by re-carrying the same bytes here.
-}
pcertifiedView ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PInteger
        :--> PBuiltinList (PAsData PInteger)
        :--> PAsData PCurrencySymbol
        :--> PFieldViewV1
    )
pcertifiedView = phoistAcyclic $
  plam $
    \txId fieldIndex stride referenceInputs certRefInputIndex chunkRefInputIndices certificatePolicyId -> P.do
      certInput <-
        plet $
          pif
            (certRefInputIndex #< 0)
            perror
            (pfromData (pelemAt # certRefInputIndex # referenceInputs))
      PTxInInfo {ptxInInfo'resolved} <- pmatch certInput
      PTxOut {ptxOut'value, ptxOut'datum} <- pmatch ptxInInfo'resolved
      certDatumData <-
        plet $ pmatch ptxOut'datum $ \case
          POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
          _ -> perror
      PFieldPreimageCertificateV1
        {pcert'txId, pcert'fieldIndex, pcert'totalLength, pcert'chunkDigests} <-
        pmatch (punsafeCoerceData @PFieldPreimageCertificateV1 certDatumData)
      totalLength <- plet $ pfromData pcert'totalLength
      chunkDigests <-
        plet $ pmap # plam pfromData # pfromData pcert'chunkDigests
      chunkCount <- plet $ plength # chunkDigests
      chunks <-
        plet $
          pmap
            # plam (\index -> prawCarriageBytes # referenceInputs # pfromData index)
            # chunkRefInputIndices

      pif
        ( pallLazy
            [ ( Value.pvalueOf
                  # pto (pfromData ptxOut'value)
                  # pfromData certificatePolicyId
                  # pcon (PTokenName (pfieldPreimageCertificateAssetName # txId # fieldIndex))
              )
                #== 1
            , pfromData pcert'txId #== txId
            , pfromData pcert'fieldIndex #== fieldIndex
            , totalLength #> pchunkBytesK
            , totalLength #<= pmaxTransactionAggregateFieldBytes
            , chunkCount #== pexpectedChunkCount # totalLength
            , chunkCount #<= pmaxTier3ChunkCount
            , plength # chunkRefInputIndices #== chunkCount
            , pchunkLengthsMatch # chunks # totalLength
            ]
        )
        ( plet
            ( pif
                (stride #> pwalkDerivedStride)
                -- §7.4 count consistency against the mint-verified
                -- `total_length`; no chunk hash is spent to learn the count.
                (pcountFromTotalLength # stride # totalLength)
                -- A variable-width field has no arithmetic count, so the header
                -- is read out of chunk 0 — and chunk 0 is verified at that
                -- moment, so the number is at least the one the committed bytes
                -- carry. Above the boundary `total_length` always leaves three
                -- bytes to read.
                ( P.do
                    header <- plet $ preadChunkedRange # chunks # chunkDigests # 0 # 3
                    PPair headerLen count <- pmatch (pdecodeFieldArrayHeader # header)
                    -- The one O(1) count check available here: an enveloped item
                    -- is at least one byte (`40`), so `count` items cannot fit in
                    -- fewer than `count` bytes. This bounds the read guard in
                    -- 'pfieldItemExtent'; it does not authenticate the count.
                    pif (headerLen + count #<= totalLength) count perror
                )
            )
            $ \count ->
              pcon
                ( PChunkedView
                    { pchunked'chunks = chunks
                    , pchunked'chunkDigests = chunkDigests
                    , pchunked'count = count
                    , pchunked'stride = stride
                    }
                )
        )
        perror

{- | Aiken @native_tx_field_access_v1.expected_chunk_count@.

§8.4's deterministic split rule: chunk @j@ is bytes @[j·K, (j+1)·K)@ with a
ragged last chunk, minimum-necessary chunks by construction.
-}
pexpectedChunkCount :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectedChunkCount = phoistAcyclic $
  plam $ \totalLength ->
    pif
      (totalLength #> 0)
      (pdiv # (totalLength + pchunkBytesK - 1) # pchunkBytesK)
      perror

{- | Aiken @native_tx_field_access_v1.chunk_lengths_match@.

The §8.4 split's shape on the consumer side: every chunk but the last is exactly
K, the last accounts for whatever remains.

__This is a partial check that presumes its caller's guards__, and its twin in
the minting policy is not. On the empty list this answers @total_length == 0@
where the producer's answers @False@, and on the last chunk it checks only
@length == total_length@ where the producer's also requires @0 < total_length <= K@.
Neither difference is reachable: 'pcertifiedView' has already established
@total_length > K@, the chunk count and the list length before it calls this, so
@ceil(total_length / K) >= 2@ makes the empty case unreachable and the last
element's remainder lies in @(0, K]@ by arithmetic.

The consequence for anyone editing 'pcertifiedView': those three guards are
load-bearing for this function, not merely for the ones they appear to guard.
-}
pchunkLengthsMatch ::
  forall (s :: S). Term s (PBuiltinList PByteString :--> PInteger :--> PBool)
pchunkLengthsMatch = phoistAcyclic $
  pfix $ \self -> plam $ \chunks totalLength ->
    pelimList
      ( \chunk rest ->
          pelimList
            ( \_ _ ->
                (plengthBS # chunk)
                  #== pchunkBytesK
                  #&& (self # rest # (totalLength - pchunkBytesK))
            )
            ((plengthBS # chunk) #== totalLength)
            rest
      )
      (totalLength #== 0)
      chunks

{- | Aiken @native_tx_field_access_v1.count_from_total_length@.

§7.4's arithmetic count for a fixed-stride field, recovered from the
mint-verified total length alone. The three branches are the three admissible
header widths, and each is checked for minimality — a count of 24 recovered from
the one-byte ladder would have been spelt in the packed form.
-}
pcountFromTotalLength ::
  forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pcountFromTotalLength = phoistAcyclic $
  plam $ \stride totalLength ->
    pexpecting (stride #> pwalkDerivedStride) $
      plet (pdiv # (totalLength - 1) # stride) $ \oneByteCount ->
        pif
          (oneByteCount #<= 23 #&& 1 + stride * oneByteCount #== totalLength)
          oneByteCount
          ( plet (pdiv # (totalLength - 2) # stride) $ \twoByteCount ->
              pif
                ( twoByteCount
                    #>= 24
                    #&& twoByteCount
                    #<= 255
                    #&& 2 + stride * twoByteCount
                    #== totalLength
                )
                twoByteCount
                ( plet (pdiv # (totalLength - 3) # stride) $ \threeByteCount ->
                    pif
                      ( threeByteCount
                          #> 255
                          #&& threeByteCount
                          #<= pmaxFieldItemCount
                          #&& 3 + stride * threeByteCount
                          #== totalLength
                      )
                      threeByteCount
                      perror
                )
          )

--------------------------------------------------------------------------------
-- Carriage resolution
--------------------------------------------------------------------------------

{- | Aiken @native_tx_field_access_v1.raw_carriage_bytes@.

§8.5: raw carriage is unauthenticated data published as a nothing-but-bytes
inline datum. Nothing here trusts provenance — content is verified by hash at
consumption, so wrong bytes simply fail.
-}
prawCarriageBytes ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo) :--> PInteger :--> PByteString)
prawCarriageBytes = phoistAcyclic $
  plam $ \referenceInputs index ->
    pif (index #< 0) perror $
      pmatch (pfromData (pelemAt # index # referenceInputs)) $
        \PTxInInfo {ptxInInfo'resolved} ->
          pmatch ptxInInfo'resolved $ \PTxOut {ptxOut'datum} ->
            pmatch ptxOut'datum $ \case
              POutputDatum {poutputDatum'outputDatum} ->
                pasByteStr # pto poutputDatum'outputDatum
              _ -> perror

--------------------------------------------------------------------------------
-- Accessors — slice-only, straddle-aware
--------------------------------------------------------------------------------

{- | Aiken @native_tx_field_access_v1.field_item_count@.

The reveal-derived item count (§5.2), and every answer it returns is
authenticated — see the module header for why it __aborts__ for a
variable-width field under tier-3 carriage rather than hand back the §5.1
header's self-assertion.

Reads are unaffected: 'pfieldItemAt' still serves such a view.
-}
pfieldItemCount :: forall (s :: S). Term s (PFieldViewV1 :--> PInteger)
pfieldItemCount = phoistAcyclic $
  plam $ \view ->
    pmatch view $ \case
      PWholeView {pwhole'count} -> pwhole'count
      PChunkedView {pchunked'count, pchunked'stride} ->
        pif (pchunked'stride #> pwalkDerivedStride) pchunked'count perror

{- | Aiken @native_tx_field_access_v1.declared_item_count@.

The count as the view holds it, with no authentication claim attached. Private,
and used only as the range guard on a read — never handed to a caller as the
item count.
-}
pdeclaredItemCount :: forall (s :: S). Term s (PFieldViewV1 :--> PInteger)
pdeclaredItemCount = phoistAcyclic $
  plam $ \view ->
    pmatch view $ \case
      PWholeView {pwhole'count} -> pwhole'count
      PChunkedView {pchunked'count} -> pchunked'count

-- | Aiken @native_tx_field_access_v1.field_total_length@.
pfieldTotalLength :: forall (s :: S). Term s (PFieldViewV1 :--> PInteger)
pfieldTotalLength = phoistAcyclic $
  plam $ \view ->
    pmatch view $ \case
      PWholeView {pwhole'bytes} -> plengthBS # pwhole'bytes
      PChunkedView {pchunked'chunks} ->
        pfoldl # plam (\total chunk -> total + (plengthBS # chunk)) # 0 # pchunked'chunks

{- | Aiken @native_tx_field_access_v1.field_item_at@.

The canonical item encoding at @index@, with its byte-string wrapper stripped.

§7.3: fails unless @0 <= index < count@ /and/ the item's full byte range lies
inside the preimage. Fixed-stride fields resolve arithmetically (§5.3);
variable-width fields walk the envelope, one head decode plus a byte jump per
skipped item, with no offset table to trust (§7.2).
-}
pfieldItemAt ::
  forall (s :: S). Term s (PFieldViewV1 :--> PInteger :--> PByteString)
pfieldItemAt = phoistAcyclic $
  plam $ \view index -> P.do
    count <- plet $ pdeclaredItemCount # view
    pexpecting (0 #<= index #&& index #< count) $ P.do
      PPair offset len <- pmatch (pfieldItemExtent # view # index)
      pfieldReadRange # view # offset # len

{- | Aiken @native_tx_field_access_v1.field_item_extent@.

The @(offset, length)@ of item @index@'s payload within the preimage. Exposed
because a resumable walk records positions, never bytes (§7.6).

__Both branches read the item's own wrapper.__ The fixed-stride arithmetic says
/where/ item @index@ begins; only the two header bytes there say that the item
is spelled the one way §5.1 admits. Skipping them would let @81 ‖ 00 00 ‖ …@ and
@81 ‖ ff ff ‖ …@ open beside the canonical @81 ‖ 58 1c ‖ …@ and hand back the
same payload — three admissible byte forms for one logical field, which §6.1
forbids and which would leave a non-canonically committed preimage unfaultable.
The wrapper read is O(1) per access, so §7.1's cost model and tier 3's lazy
chunk verify are untouched: the check rides along on the chunk the item is about
to be read from anyway.
-}
pfieldItemExtent ::
  forall (s :: S).
  Term s (PFieldViewV1 :--> PInteger :--> PPair PInteger PInteger)
pfieldItemExtent = phoistAcyclic $
  plam $ \view index -> P.do
    count <- plet $ pdeclaredItemCount # view
    pexpecting (0 #<= index #&& index #< count) $ P.do
      stride <- plet $ pfieldViewStride # view
      pif
        (stride #> pwalkDerivedStride)
        ( P.do
            itemOffset <- plet $ (pheaderLenForCount # count) + stride * index
            PPair payloadOffset len <- pmatch (pitemHeaderAt # view # itemOffset)
            -- Every fixed stride in §5.3 wraps 24..255 payload bytes, so the
            -- canonical wrapper is exactly the two-byte `58 LL` form and the
            -- stride pins `LL`.
            pif
              ( payloadOffset
                  #== itemOffset + pfixedItemWrapperBytes
                  #&& len
                  #== stride - pfixedItemWrapperBytes
              )
              (pcon (PPair payloadOffset len))
              perror
        )
        (pwalkItemExtent # view # (pheaderLenForCount # count) # index)

{- | Aiken @native_tx_field_access_v1.field_view_stride@.

§5.3's stride for the field this view was opened on. Public for the same reason
'pfieldItemExtent' is (§7.6): a resumable walk records positions, and which
/kind/ of position a field has — arithmetic or walk-derived — is what decides
whether a carried offset is self-authenticating.
-}
pfieldViewStride :: forall (s :: S). Term s (PFieldViewV1 :--> PInteger)
pfieldViewStride = phoistAcyclic $
  plam $ \view ->
    pmatch view $ \case
      PWholeView {pwhole'stride} -> pwhole'stride
      PChunkedView {pchunked'stride} -> pchunked'stride

{- | Aiken @native_tx_field_access_v1.field_header_len@.

The byte width of this view's §5.1 array header — equivalently, the offset at
which item 0's wrapper begins. Public so a walk's /opening/ position is derived
from the authenticated view rather than supplied by a redeemer (§10.2). The
count it reads is the view's own, fixed at construction, so this cannot be
steered from outside.
-}
pfieldHeaderLen :: forall (s :: S). Term s (PFieldViewV1 :--> PInteger)
pfieldHeaderLen = phoistAcyclic $
  plam $ \view -> pheaderLenForCount #$ pdeclaredItemCount # view

-- | Aiken @native_tx_field_access_v1.header_len_for_count@.
pheaderLenForCount :: forall (s :: S). Term s (PInteger :--> PInteger)
pheaderLenForCount = phoistAcyclic $
  plam $ \count ->
    pif (count #< 0) perror $
      pif (count #<= 23) 1 $
        pif (count #<= 255) 2 $
          pif (count #<= pmaxFieldItemCount) 3 perror

{- | Aiken @native_tx_field_access_v1.field_read_range@.

Reads @length@ bytes at @offset@, stitching across chunk boundaries and
verifying every chunk it touches (§8.8 straddle awareness, lazy verify).
-}
pfieldReadRange ::
  forall (s :: S).
  Term s (PFieldViewV1 :--> PInteger :--> PInteger :--> PByteString)
pfieldReadRange = phoistAcyclic $
  plam $ \view offset len ->
    pmatch view $ \case
      PWholeView {pwhole'bytes} -> psliceExact # pwhole'bytes # offset # len
      PChunkedView {pchunked'chunks, pchunked'chunkDigests} ->
        preadChunkedRange # pchunked'chunks # pchunked'chunkDigests # offset # len

-- | Aiken @native_tx_field_access_v1.read_chunked_range@.
preadChunkedRange ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList PByteString
        :--> PBuiltinList PByteString
        :--> PInteger
        :--> PInteger
        :--> PByteString
    )
preadChunkedRange = phoistAcyclic $
  pfix $ \self -> plam $ \chunks chunkDigests offset len ->
    pexpecting (0 #<= offset #&& 0 #<= len) $
      pif (len #== 0) (pconstant "") $ P.do
        chunkIndex <- plet $ pdiv # offset # pchunkBytesK
        within <- plet $ offset - chunkIndex * pchunkBytesK
        chunk <- plet $ pelemAt # chunkIndex # chunks
        digest <- plet $ pelemAt # chunkIndex # chunkDigests
        available <- plet $ (plengthBS # chunk) - within
        -- Lazy chunk verify: a chunk is hashed the first time a read reaches
        -- it, and a chunk nobody reads is never hashed at all.
        pexpecting ((pblake2b_256 # chunk) #== digest #&& available #> 0) $
          pif
            (len #<= available)
            (psliceExact # chunk # within # len)
            ( (psliceExact # chunk # within # available)
                <> (self # chunks # chunkDigests # (offset + available) # (len - available))
            )

--------------------------------------------------------------------------------
-- Envelope walking
--------------------------------------------------------------------------------

{- | Aiken @native_tx_field_access_v1.field_item_header_at@.

Decodes the §5.1 definite-byte-string header at @offset@, returning
@(payload offset, payload length)@. Minimal width; fails closed otherwise.

Public as the resumable walk's single step primitive (§10.2). It stays the
__only__ reader of a §5.1 item head in the tree — the walk core steps through
this function rather than re-deriving the grammar, so one logical field keeps
one verdict (§6.1). Advancing from an arbitrary @offset@ is what makes a walk
resumable without re-reading completed items; the offset is a position, never a
claim about content, and every byte it reaches is read through
'pfieldReadRange' and so stays inside the authenticated bytes.
-}
pfieldItemHeaderAt ::
  forall (s :: S).
  Term s (PFieldViewV1 :--> PInteger :--> PPair PInteger PInteger)
pfieldItemHeaderAt = phoistAcyclic $
  plam $ \view offset -> pitemHeaderAt # view # offset

-- | Aiken @native_tx_field_access_v1.item_header_at@.
pitemHeaderAt ::
  forall (s :: S).
  Term s (PFieldViewV1 :--> PInteger :--> PPair PInteger PInteger)
pitemHeaderAt = phoistAcyclic $
  plam $ \view offset ->
    plet (pbyteAt # (pfieldReadRange # view # offset # 1) # 0) $ \tag ->
      pif
        (64 #<= tag #&& tag #<= 87)
        (pcon (PPair (offset + 1) (tag - 64)))
        ( pif
            (tag #== 88)
            ( plet (pbyteAt # (pfieldReadRange # view # (offset + 1) # 1) # 0) $ \len ->
                pif (len #>= 24) (pcon (PPair (offset + 2) len)) perror
            )
            ( pexpecting (tag #== 89) $
                plet (pfieldReadRange # view # (offset + 1) # 2) $ \head' ->
                  plet ((pbyteAt # head' # 0) * 256 + (pbyteAt # head' # 1)) $ \len ->
                    pif (len #> 0xff) (pcon (PPair (offset + 3) len)) perror
            )
        )

-- | Aiken @native_tx_field_access_v1.walk_item_extent@.
pwalkItemExtent ::
  forall (s :: S).
  Term s (PFieldViewV1 :--> PInteger :--> PInteger :--> PPair PInteger PInteger)
pwalkItemExtent = phoistAcyclic $
  pfix $ \self -> plam $ \view offset remaining -> P.do
    PPair payloadOffset len <- pmatch (pitemHeaderAt # view # offset)
    pif
      (remaining #<= 0)
      (pcon (PPair payloadOffset len))
      (self # view # (payloadOffset + len) # (remaining - 1))

{- | Aiken @native_tx_field_access_v1.walk_to_end@.

Walks @count@ enveloped items from @offset@ and returns the offset one past the
last item. Callers compare it against the preimage length so that a miscounted
or truncated preimage fails closed (§5.1).

It wraps the bare preimage in a throwaway 'PWholeView' rather than reading the
grammar itself, which is what keeps 'pitemHeaderAt' the tree's single §5.1 item
head reader.
-}
pwalkToEnd ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PInteger)
pwalkToEnd = phoistAcyclic $
  pfix $ \self -> plam $ \preimage offset remaining ->
    pif (remaining #<= 0) offset $ P.do
      view <-
        plet $
          pcon (PWholeView {pwhole'bytes = preimage, pwhole'count = 0, pwhole'stride = 0})
      PPair payloadOffset len <- pmatch (pitemHeaderAt # view # offset)
      self # preimage # (payloadOffset + len) # (remaining - 1)

--------------------------------------------------------------------------------
-- Local helpers
--------------------------------------------------------------------------------

{- | Aiken's @expect cond@ — evaluate to @value@ when @cond@ holds, abort
otherwise. Named because the shape appears at every guard in this module and
@pif cond value perror@ reads as a three-way choice when it is not one.
-}
pexpecting :: forall (a :: S -> Type) (s :: S). Term s PBool -> Term s a -> Term s a
pexpecting cond value = pif cond value perror

{- | A chain of @expect@s, as a lazy conjunction.

Aiken's sequential @expect@s short-circuit, so an earlier failure means a later
check is never evaluated. 'Midgard.Utils.pand'List' is strict and would evaluate
all of them, which matters here because several later checks are only
well-defined once the earlier ones hold.
-}
pallLazy :: forall (s :: S). [Term s PBool] -> Term s PBool
pallLazy = foldr1 (#&&)

-- | @punsafeCoerce@ into a data-encoded Plutarch type, as the rest of the port does.
punsafeCoerceData ::
  forall (a :: S -> Type) (s :: S). (PIsData a) => Term s PData -> Term s a
punsafeCoerceData d = pfromData (punsafeCoerce @(PAsData a) d)

-- | Aiken @bytearray.from_int_big_endian@, with the width fixed at the call site.
pbigEndian ::
  forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PByteString
pbigEndian width n = pintegerToByteString # pmostSignificantFirst # width # n
