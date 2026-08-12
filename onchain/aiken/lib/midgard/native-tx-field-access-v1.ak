//// The lib-level field-access module for the nine compact-transaction field
//// commitments — `docs/spec/midgard-tx.md` §4, §5, §7 and §8.
////
//// This module is the **single consumer-visible access idiom** for those nine
//// fields at the lib layer. A consumer obtains a `FieldViewV1` from the one
//// `authenticated_field_view` door and then reads items through the slice-only
//// accessors below; nothing else at this layer opens a field.
////
//// Three properties the door owns, so that no caller has to:
////
////   * **Positional hash extraction is internal** (§2.5/§4). Callers hand over
////     the committed compact structures, never a free-standing field hash, so
////     the positional-identity invariant cannot be side-stepped at a dispute
////     entry point. For the three witness-set fields the door re-derives the
////     witness-set hash and checks it against the `witness_set_hash` the
////     supplied compact structure carries, before it will read anything.
////     **That check is half of the binding, not all of it**: §3's transaction
////     id preimage is the body alone, so a compact structure's trailing
////     `witness_set_hash` is *not* covered by the id and a caller that
////     re-derived the structure from redeemer bytes has authenticated nothing
////     about it. The other half is the caller's, and
////     `midgard/fraud_proofs/field_opening_v1`'s `WitnessAnchor` is where a
////     downstream family step gets it: the value its own step-01 read off the
////     block-committed structure. A caller holding structures straight from
////     `pass_native_tx_to_next_step` already has them authenticated and owes
////     nothing further.
////   * **Abort, never clamp** (§7.3). `bytearray.slice` clamps, and two
////     clamped out-of-range reads are byte-equal — which would fabricate
////     equality evidence out of a perfectly valid block. Every read here goes
////     through `slice_exact`, which fails closed instead.
////   * **Straddle awareness** (§8.4/§8.8). Under tier-3 carriage an item's
////     byte range can cross a chunk boundary; reads stitch across chunks and
////     verify each chunk they touch against the certificate's digest vector
////     (lazy chunk verify — a chunk nobody reads is never hashed).
////   * **One grammar, one verdict** (§5.1/§6.1). `decode_field_array_header_at`
////     and `item_header_at` are the only readers of the §5.1 heads, and both
////     enforce the narrow acceptance set: minimal width, capped at
////     `99 NNNN` / `59 LLLL`. Fixed-stride access reads the item wrapper too
////     rather than inferring it from the stride, so one logical field has
////     exactly one admissible commitment.
////
//// The retired counted bounded-collection scheme committed each field with a
//// per-item Merkle construction. Under this module a field commitment is a
//// plain `blake2b_256` over the field's enveloped preimage bytes (§4), so a
//// watcher recomputes any commitment from raw canonical bytes alone.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input}
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_witness_set_compact,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxCompact, NativeTxWitnessSetCompact,
  VerifiedMidgardNativeTxCompact,
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// The nine committed fields of §2.5. Field identity is positional.
///
/// This is the tree's single definition. The retired counted modules
/// (`bounded_collection_v1`, `bounded_item_v1`, `bounded_blob_v1`) each used
/// to restate it; they import it from here instead, so §2.5's field count has
/// one place to change and the retired surface cannot drift away from the
/// live one while it waits to be deleted.
pub const field_count: Int = 9

/// §5.4. Retained from the counted era by owner ruling: tightening it would be
/// a capability change outside the reversion's scope.
pub const max_transaction_aggregate_field_bytes: Int = 32768

/// §5.4. Field 0's own bound equals the aggregate bound.
pub const max_spend_inputs_preimage_bytes: Int = 32768

/// §5.4. The operative spend maximum, a Cardano shape bound.
pub const maximum_cardano_spend_redeemer_count: Int = 296

/// §5.1 caps the item-count header at the `99 NNNN` form.
pub const max_field_item_count: Int = 65535

/// §8.3 tier-2 bound and tier-3 chunk size, at the value erratum E1 re-pinned
/// it to.
///
/// **E1 falsified the provisional 15,900 and this constant carries the repair.**
/// The Phase-4 measurement it was declared provisional pending was taken and
/// refuted 15,900: a real signed key-address publication of a 15,900-byte chunk
/// measures 16,648 bytes against a 16,384-byte `maxTxSize`. Because the chunker
/// cuts *at* `K`, that made every tier-3 plan's first chunk unpublishable at
/// every preimage length, so the whole window `(15,148, 32,768]` had no
/// admissible carriage anywhere and tier 3 did not function on L1.
///
/// 15,148 is the reserve-clearing publication frontier: a 15,148-byte chunk
/// publishes as exactly 15,872 bytes, `maxTxSize` less the 512-byte reliability
/// reserve. `K` is therefore now the largest chunk that can be published, which
/// is what makes §8.4's split a partition whose every piece is carriable.
/// `ceil(32,768 / 15,148) = 3`, so `max_tier3_chunk_count` below does not move.
///
/// **`K` is the split, not merely a bound**, which is why this re-pin is not a
/// one-line change: every chunk boundary in the system moves with it — the §8.10
/// three-chunk corner, the #569 cross-language straddle vector, the #568
/// chunk-count goldens, the §8.10 execution ledger and the §11.4/§12.5 rows
/// whose fixtures sit at `chunk_bytes_k`. Those all move in the same commit as
/// this literal, and the spec's §8.3/§8.10/§12.5 rows with them.
///
/// Declared once here; callers name this constant rather than repeating the
/// literal. Its TypeScript twin is `MIDGARD_CHUNK_BYTES_K_V1` in
/// `demo/midgard-core/src/codec/native-tx-field-access-v1.ts`, which is asserted
/// equal to the cost model's derived frontier so neither half can drift.
pub const chunk_bytes_k: Int = 15148

/// §8.3 tier-1 bound. **PROVISIONAL** on the same footing as `chunk_bytes_k`:
/// `maxTxSize` (16,384) minus a round 2,048-byte allowance for step machinery,
/// an engineering choice that Phase 4 re-pins against the real step
/// transaction.
pub const max_tier1_redeemer_preimage_bytes: Int = 14336

/// §8.3, derived: `ceil(max_transaction_aggregate_field_bytes / chunk_bytes_k)`.
pub const max_tier3_chunk_count: Int = 3

/// §5.3 fixed strides. Each fixed-width item carries a two-byte `58 LL`
/// wrapper, so `stride = 2 + item bytes`.
pub const fixed_item_wrapper_bytes: Int = 2

/// §5.3 fields 0/1: `82 ‖ 58 20 tx_id ‖ 19 index_be16` — the fixed 3-byte
/// output index is what makes the item width constant.
pub const spend_input_item_bytes: Int = 38

pub const spend_input_stride: Int = 40

/// §5.3 fields 3/4: a raw 28-byte hash per item.
pub const hash28_item_bytes: Int = 28

pub const hash28_stride: Int = 30

/// §5.3 field 7: `82 ‖ 58 20 vkey(32) ‖ 58 40 signature(64)`.
pub const address_witness_item_bytes: Int = 101

pub const address_witness_stride: Int = 103

/// Fields 2/5/6/8 are variable-width: top-level access walks the envelope
/// instead of multiplying a stride.
pub const walk_derived_stride: Int = 0

// ---------------------------------------------------------------------------
// Wire types (§8.8) — constructor order is frozen consensus wire format
// ---------------------------------------------------------------------------

/// How a field's preimage bytes reach the consuming transaction (§8.1–§8.4).
/// Constructor order is frozen: `Inline` is Constr 0, `RawUtxo` 1,
/// `Certified` 2. Off-chain builders emit exactly these tags.
pub type FieldCarriageV1 {
  /// Tier 1 — the step's own redeemer carries the preimage.
  Inline { preimage: ByteArray }
  /// Tier 2 — one nothing-but-bytes inline datum at the prover's key address,
  /// named by its positional reference-input index.
  RawUtxo { ref_input_index: Int }
  /// Tier 3 — deterministic fixed-`chunk_bytes_k` chunks plus one certified
  /// digest-manifest. `chunk_ref_input_indices` is all-chunks-positional:
  /// element `k` is the reference-input index of chunk `k`.
  Certified { cert_ref_input_index: Int, chunk_ref_input_indices: List<Int> }
}

/// An authenticated field, ready to slice. Carriage tier is an encoding
/// detail that validator logic never branches on — the accessors below read
/// both variants.
pub type FieldViewV1 {
  /// Tiers 1–2: the whole preimage is present and hash-checked.
  Whole { bytes: ByteArray, count: Int, stride: Int }
  /// Tier 3: chunks are present but unhashed until touched; `chunk_digests`
  /// and the item count come from the mint-verified certificate.
  Chunked {
    chunks: List<ByteArray>,
    chunk_digests: List<ByteArray>,
    count: Int,
    stride: Int,
  }
}

/// §8.6. The tier-3 digest manifest, minted by the permissionless
/// multi-handler certificate validator and consumed here.
pub type FieldPreimageCertificateV1 {
  owner: VerificationKeyHash,
  tx_id: ByteArray,
  field_index: Int,
  total_length: Int,
  chunk_digests: List<ByteArray>,
}

/// §8.6's deterministic certificate asset name, derived from
/// `(tx_id, field_index)` so indexers can find a certificate and so a consumer
/// can name the exact token it requires without trusting the redeemer. Fixed
/// here because both sides of the tier-3 handshake — the minting policy that
/// certifies and the door that consumes — must agree on one derivation.
///
/// The derivation is exactly
/// `blake2b_256(field_index_byte ‖ tx_id)` — a 33-byte preimage whose leading
/// byte is the 0..8 field index and whose remaining 32 are the tx-id, giving a
/// 32-byte name. The single-byte prefix is domain separation, not a length
/// header: with `field_index` bounded to 0..8 and `tx_id` fixed at 32 bytes,
/// the preimage is unambiguous. §8.6 of `docs/spec/midgard-tx.md` carries this
/// as a normative erratum so the off-chain minter has one derivation to match
/// rather than a description to guess at.
pub fn field_preimage_certificate_asset_name(
  tx_id: ByteArray,
  field_index: Int,
) -> ByteArray {
  expect _field_index = expect_field_index(field_index)
  // Both bounds are what make the 33-byte preimage unambiguous, so both are
  // enforced rather than assumed of the caller.
  expect bytearray.length(tx_id) == 32
  blake2b_256(bytearray.push(tx_id, field_index))
}

// ---------------------------------------------------------------------------
// §5.1 envelope grammar
// ---------------------------------------------------------------------------

fn expect_field_index(field_index: Int) -> Int {
  expect field_index >= 0
  expect field_index < field_count
  field_index
}

/// The §5.1 preimage: a definite array header followed by one definite
/// byte-string-wrapped item per element. All nine fields share it; an empty
/// field is exactly `80`.
pub fn encode_field_preimage(items: List<ByteArray>) -> ByteArray {
  let count = list.length(items)
  expect count <= max_field_item_count
  bytearray.concat(
    encode_field_array_header(count),
    list.foldr(
      items,
      #"",
      fn(item, encoded_tail) {
        bytearray.concat(encode_definite_bytes(item), encoded_tail)
      },
    ),
  )
}

/// §5.1 `definite_array_header(N)`, minimal width, capped at `99 NNNN`.
pub fn encode_field_array_header(count: Int) -> ByteArray {
  expect count >= 0
  if count <= 23 {
    bytearray.push(#"", 128 + count)
  } else if count <= 255 {
    bytearray.concat(#"98", bytearray.from_int_big_endian(count, 1))
  } else {
    expect count <= max_field_item_count
    bytearray.concat(#"99", bytearray.from_int_big_endian(count, 2))
  }
}

/// §4 — plain hashing. No domain tag, no version prefix, no field index in the
/// hash input; a watcher needs the raw bytes and `blake2b_256`, nothing else.
pub fn field_commitment(preimage: ByteArray) -> ByteArray {
  blake2b_256(preimage)
}

/// The producer twin of `field_commitment`: envelope the items, then hash.
pub fn field_commitment_from_items(items: List<ByteArray>) -> ByteArray {
  field_commitment(encode_field_preimage(items))
}

/// `field_commitment(#"80")` — what every empty field commits to.
///
/// Under §4's plain hashing this value carries no field index, so all nine
/// empty fields share it and it is a compile-time constant rather than
/// something to recompute. Pinned as a literal because emptiness comparisons
/// are dense in the validation machine and each
/// `field_commitment_from_items([])` re-ran `blake2b_256` for the same 32
/// bytes. `empty_field_commitment_matches_the_producer` in this module's test
/// file proves the literal against the producer, so the pin cannot drift.
pub const empty_field_commitment: ByteArray =
  #"45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

/// Reads the §5.1 array header at offset 0. Returns `(header_len, count)`.
/// Non-minimal widths and any non-array head fail closed.
pub fn decode_field_array_header(preimage: ByteArray) -> Pair<Int, Int> {
  decode_field_array_header_at(preimage, 0)
}

/// The **one** §5.1 `definite_array_header(N)` decoder, at any offset.
/// Returns `(offset just past the header, N)`.
///
/// §5.1's acceptance set is narrower than CBOR's: minimal width only, capped
/// at the `99 NNNN` form — the four-byte `9a` head is outside the grammar and
/// rejects here even though it is well-formed CBOR. Every §5.1 envelope in the
/// tree decodes through this function, so the grammar has one verdict rather
/// than two; `codec.decode_definite_array_header_at` is the general CBOR head
/// reader for structures outside this grammar (the compact-tx array, the
/// work-witness array) and deliberately does not enforce §5.1.
pub fn decode_field_array_header_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, Int> {
  let tag = bytearray.at(bytes, offset)
  if and {
    tag >= 128,
    tag <= 151,
  } {
    Pair(offset + 1, tag - 128)
  } else if tag == 152 {
    let count = bytearray.at(bytes, offset + 1)
    expect count >= 24
    Pair(offset + 2, count)
  } else {
    expect tag == 153
    let count = bytearray.at(bytes, offset + 1) * 256 + bytearray.at(
      bytes,
      offset + 2,
    )
    expect count > 0xff
    Pair(offset + 3, count)
  }
}

/// §5.3 stride table. `walk_derived_stride` marks the variable-width fields.
pub fn field_stride(field_index: Int) -> Int {
  expect _field_index = expect_field_index(field_index)
  if or {
    field_index == 0,
    field_index == 1,
  } {
    spend_input_stride
  } else if or {
    field_index == 3,
    field_index == 4,
  } {
    hash28_stride
  } else if field_index == 7 {
    address_witness_stride
  } else {
    walk_derived_stride
  }
}

// ---------------------------------------------------------------------------
// §7.3 abort-never-clamp primitives
// ---------------------------------------------------------------------------

/// The only slice in this module. `bytearray.slice` clamps out-of-range
/// arguments, so the bound is checked first and the read fails closed.
fn slice_exact(bytes: ByteArray, offset: Int, length: Int) -> ByteArray {
  expect offset >= 0
  expect length >= 0
  expect offset + length <= bytearray.length(bytes)
  if length == 0 {
    #""
  } else {
    bytearray.slice(bytes, offset, offset + length - 1)
  }
}

// ---------------------------------------------------------------------------
// §2.5 positional hash extraction (internal by design)
// ---------------------------------------------------------------------------

/// §2.5's field-index table. Deliberately private: the positional-identity
/// invariant (§4) says an expected hash may only come from a committed
/// structure in view, and keeping this behind the door is how that is
/// enforced rather than merely documented.
fn field_commitment_at(
  body: NativeTxBodyCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
) -> ByteArray {
  expect _field_index = expect_field_index(field_index)
  if field_index == 0 {
    body.spend_inputs_hash
  } else if field_index == 1 {
    body.reference_inputs_hash
  } else if field_index == 2 {
    body.outputs_hash
  } else if field_index == 3 {
    body.required_observers_hash
  } else if field_index == 4 {
    body.required_signers_hash
  } else if field_index == 5 {
    body.mint_hash
  } else if field_index == 6 {
    witness_set.script_tx_wits_hash
  } else if field_index == 7 {
    witness_set.addr_tx_wits_hash
  } else {
    witness_set.redeemer_tx_wits_hash
  }
}

// ---------------------------------------------------------------------------
// The door
// ---------------------------------------------------------------------------

/// The single field-access door (§8.8).
///
/// Authenticates field `field_index` of the transaction committed by
/// `verified` against whichever carriage tier `carriage` names, and returns a
/// view the slice-only accessors below can read. Fields 6–8 additionally
/// require `witness_set` to re-derive to the compact structure's committed
/// `witness_set_hash`, so a caller cannot substitute a witness set the tx-id
/// never committed.
///
/// §7.1 authenticate-once is the caller's to exploit: build the view once per
/// field and read it many times; an untouched field is never authenticated.
///
/// `certificate_policy_id` is the field-preimage certificate minting policy
/// (§8.6). It is consulted only on tier 3, where a flat hash cannot
/// authenticate a chunk on its own and the certificate is therefore the
/// binding: the door requires the named reference input to actually hold that
/// policy's `(tx_id, field_index)` token, so a look-alike UTxO at any address
/// cannot pass its datum off as a manifest.
pub fn authenticated_field_view(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> FieldViewV1 {
  let Pair(expected_hash, stride) = field_door_prologue(
    verified,
    witness_set,
    field_index,
  )
  when carriage is {
    Inline { preimage } -> whole_view(preimage, expected_hash, stride)
    RawUtxo { ref_input_index } ->
      whole_view(
        raw_carriage_bytes(reference_inputs, ref_input_index),
        expected_hash,
        stride,
      )
    Certified { cert_ref_input_index, chunk_ref_input_indices } ->
      certified_view(
        verified.tx_id,
        field_index,
        stride,
        reference_inputs,
        cert_ref_input_index,
        chunk_ref_input_indices,
        certificate_policy_id,
      )
  }
}

/// Everything both public doors do before they look at the carriage: check the
/// field index, establish positional identity for the witness-set fields, and
/// derive the `(expected commitment, §5.3 stride)` pair the tier branches all
/// need.
///
/// **Factored because it is the binding, and a binding that has to be edited in
/// two places is a binding that will one day only be half-edited.**
/// `authenticated_field_view` and `authenticated_whole_field_view` differ only in
/// what they do with a tier-3 carriage; the checks below are the same checks in
/// both, so they live here once.
///
/// Positional identity (§4): the witness-set fields are only readable once the
/// supplied witness set re-derives to the `witness_set_hash` these compact
/// structures carry. Whether *that* hash is itself authenticated is the caller's
/// to establish — see the module header — because §3's id preimage is the body
/// and does not reach it. §2.5 pairs the two structures; nothing here assumes the
/// pairing, it proves it.
fn field_door_prologue(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
) -> Pair<ByteArray, Int> {
  expect _field_index = expect_field_index(field_index)
  let NativeTxCompact { body, witness_set_hash, .. } = verified.tx_compact
  expect or {
      field_index < 6,
      blake2b_256(encode_native_tx_witness_set_compact(witness_set)) == witness_set_hash,
    }
  Pair(
    field_commitment_at(body, witness_set, field_index),
    field_stride(field_index),
  )
}

/// Tiers 1–2 (§8.1/§8.2): the whole preimage is in hand, so it is hashed once
/// against the positionally-extracted commitment and then structurally
/// validated before any accessor may touch it.
///
/// The split of structural work between here and the accessors is deliberate.
/// Construction proves what is O(1) or already O(N)-unavoidable: the header is
/// minimal, and the declared count agrees with the byte length — by arithmetic
/// for the fixed-stride fields (§7.4), by a full walk for the variable-width
/// ones, which is the only way to know where their items end. Per-item wrapper
/// canonicality for the fixed-stride fields is proved at each read instead
/// (`field_item_extent`), so a field with 296 items does not pay 296 wrapper
/// decodes to answer one dispute.
fn whole_view(
  preimage: ByteArray,
  expected_hash: ByteArray,
  stride: Int,
) -> FieldViewV1 {
  expect field_commitment(preimage) == expected_hash
  let total_length = bytearray.length(preimage)
  expect total_length <= max_transaction_aggregate_field_bytes
  let Pair(header_len, count) = decode_field_array_header(preimage)
  if stride > walk_derived_stride {
    // §7.4 count consistency, arithmetic form.
    expect header_len + stride * count == total_length
  } else {
    // §5.1 fail-closed: the walked content must account for exactly the
    // declared item count and leave no trailing bytes.
    expect walk_to_end(preimage, header_len, count) == total_length
  }
  Whole { bytes: preimage, count, stride }
}

/// Tier 3 (§8.4): chunks stay unhashed until an accessor touches them. The
/// certificate is mint-verified, so `total_length` and the digest vector are
/// authenticated data; `(tx_id, field_index)` is matched against the
/// already-authenticated disputed transaction, never against redeemer-supplied
/// identity (§8.6).
///
/// **Why no `walk_to_end` here.** Tier 3 exists precisely because the field is
/// larger than one chunk, and §8.4's guarantee is that reaching one item costs
/// one chunk hash (two when the item straddles). Walking a variable-width
/// field to its end at construction would cost far more than hashing the
/// preimage once: `read_chunked_range` re-verifies the chunk a read lands in
/// on *every* read, so an N-item walk spends N `blake2b_256` hashes over a
/// whole chunk. That is not a cost tier 3 can absorb at construction for
/// fields nobody goes on to read past item 0, and there is no cheaper walk to
/// substitute.
///
/// So the §5.1 count-consistency check that `whole_view` runs is not available
/// here, and this function does not pretend otherwise. What still holds:
/// `total_length` and every chunk digest are mint-verified against the field
/// hash at certification (§8.6), so the bytes are the committed bytes; the
/// fixed-stride fields keep the full §7.4 arithmetic check against
/// `total_length`; and a variable-width field's header count is bounded here
/// but not authenticated, which is why `field_item_count` refuses to answer
/// for one rather than hand back a number nobody checked.
fn certified_view(
  tx_id: ByteArray,
  field_index: Int,
  stride: Int,
  reference_inputs: List<Input>,
  cert_ref_input_index: Int,
  chunk_ref_input_indices: List<Int>,
  certificate_policy_id: PolicyId,
) -> FieldViewV1 {
  let CertifiedChunksV1 { chunks, chunk_digests, total_length } = certified_chunks(
    tx_id,
    field_index,
    reference_inputs,
    cert_ref_input_index,
    chunk_ref_input_indices,
    certificate_policy_id,
  )
  let count = if stride > walk_derived_stride {
    // §7.4 count consistency against the mint-verified `total_length`; no
    // chunk hash is spent to learn the count (§8.6).
    count_from_total_length(stride, total_length)
  } else {
    // A variable-width field has no arithmetic count, so the header is read
    // out of chunk 0 — and chunk 0 is verified at that moment, so the number
    // is at least the one the committed bytes carry. Above the boundary
    // `total_length` always leaves three bytes to read.
    let header = read_chunked_range(chunks, chunk_digests, 0, 3)
    let Pair(header_len, count) = decode_field_array_header(header)
    // The one count check that is O(1) here: an enveloped item is at least
    // one byte (`40`), so `count` items cannot fit in fewer than `count`
    // bytes. This bounds the read guard in `field_item_extent`; it does not
    // authenticate the count, which is why `field_item_count` refuses it.
    expect header_len + count <= total_length
    count
  }
  Chunked { chunks, chunk_digests, count, stride }
}

/// The digest manifest and the chunk bytes it names, with every §8.4/§8.6 check
/// that costs no chunk hash already applied.
///
/// Split out of `certified_view` because tier 3 is read two ways and both ways
/// need exactly these checks: lazily, chunk by chunk, which is
/// `certified_view`; and materialised whole, which is
/// `authenticated_whole_field_view` below. Neither way trusts the manifest for
/// content — the digests bound lazy reads, and the materialised path is checked
/// against §4's flat commitment directly.
type CertifiedChunksV1 {
  chunks: List<ByteArray>,
  chunk_digests: List<ByteArray>,
  total_length: Int,
}

fn certified_chunks(
  tx_id: ByteArray,
  field_index: Int,
  reference_inputs: List<Input>,
  cert_ref_input_index: Int,
  chunk_ref_input_indices: List<Int>,
  certificate_policy_id: PolicyId,
) -> CertifiedChunksV1 {
  expect cert_ref_input_index >= 0
  expect Some(cert_input) = list.at(reference_inputs, cert_ref_input_index)
  expect
    assets.quantity_of(
      cert_input.output.value,
      certificate_policy_id,
      field_preimage_certificate_asset_name(tx_id, field_index),
    ) == 1
  expect InlineDatum(cert_datum) = cert_input.output.datum
  expect certificate: FieldPreimageCertificateV1 = cert_datum
  expect certificate.tx_id == tx_id
  expect certificate.field_index == field_index
  let total_length = certificate.total_length
  // §8.4 defines tier 3 as the `preimage_len > K` case. Enforcing that lower
  // bound is what makes the ladder a partition rather than a preference: a
  // field that fits tier 1 or tier 2 has exactly one admissible carriage, so
  // it cannot be carried two ways, and simplest-fitting-first (§8) is a
  // property of the format instead of a convention builders are trusted to
  // follow. Without it a single-chunk "certificate" authenticates a preimage
  // of any size, and every structural check tiers 1–2 run at view
  // construction can be side-stepped by re-carrying the same bytes here.
  expect total_length > chunk_bytes_k
  expect total_length <= max_transaction_aggregate_field_bytes
  let chunk_digests = certificate.chunk_digests
  let chunk_count = list.length(chunk_digests)
  expect chunk_count == expected_chunk_count(total_length)
  expect chunk_count <= max_tier3_chunk_count
  expect list.length(chunk_ref_input_indices) == chunk_count
  let chunks = list.map(
    chunk_ref_input_indices,
    fn(index) { raw_carriage_bytes(reference_inputs, index) },
  )
  expect chunk_lengths_match(chunks, total_length)
  CertifiedChunksV1 { chunks, chunk_digests, total_length }
}

/// The door, opened so that the view it returns is **always** a `Whole` — §5.1
/// count consistency included, whatever tier the carriage names.
///
/// Why this exists. `field_item_count` refuses to answer for a variable-width
/// field under tier 3, because tier 3's lazy reads never establish the §5.1
/// header count and nothing else in the format mirrors it. That refusal is
/// right for a reader that wants one item, and wrong for a *rule that consumes
/// the count* — of which the validation machine has several: the phases that
/// scan a whole field take its item count from the field itself and then
/// require the scan to reach it, so an unauthenticated count would let a
/// prover declare a short field and skip the items past its claim.
///
/// Under tiers 1–2 there is nothing to add: the whole preimage is already in
/// hand and `whole_view` walks it. Under tier 3 this materialises the preimage
/// by concatenating the manifest's chunks in order and then hands it to
/// `whole_view`, which hashes it once against §4's flat commitment. That single
/// hash is the whole authentication — the digests are not re-checked here and
/// do not need to be, because a chunk substituted for another changes the
/// concatenation and so changes the hash. What the manifest still buys is the
/// §8.4 partition (`total_length > K`, the derived chunk count, the exact chunk
/// shape) and the `(tx_id, field_index)` binding its token carries, all of
/// which cost no chunk hash.
///
/// **The cost is stated, not hidden.** This is tier 2's cost profile applied to
/// a tier-3 field: one `blake2b_256` over up to
/// `max_transaction_aggregate_field_bytes` per view construction, in place of
/// tier 3's promise of one chunk hash per read. A caller that only wants item
/// bytes must keep using `authenticated_field_view` and pay §8.4's price
/// instead. §12.5 already prices the tier-2 per-step full-preimage re-hash;
/// this is the same row, reached from the other tier.
pub fn authenticated_whole_field_view(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> FieldViewV1 {
  let Pair(expected_hash, stride) = field_door_prologue(
    verified,
    witness_set,
    field_index,
  )
  when carriage is {
    Inline { preimage } -> whole_view(preimage, expected_hash, stride)
    RawUtxo { ref_input_index } ->
      whole_view(
        raw_carriage_bytes(reference_inputs, ref_input_index),
        expected_hash,
        stride,
      )
    Certified { cert_ref_input_index, chunk_ref_input_indices } -> {
      let CertifiedChunksV1 { chunks, .. } = certified_chunks(
        verified.tx_id,
        field_index,
        reference_inputs,
        cert_ref_input_index,
        chunk_ref_input_indices,
        certificate_policy_id,
      )
      whole_view(
        list.foldl(
          chunks,
          #"",
          fn(chunk, joined) { bytearray.concat(joined, chunk) },
        ),
        expected_hash,
        stride,
      )
    }
  }
}

/// §8.4's deterministic split rule: chunk `j` is bytes `[j·K, (j+1)·K)` with a
/// ragged last chunk, minimum-necessary chunks by construction.
pub fn expected_chunk_count(total_length: Int) -> Int {
  expect total_length > 0
  ( total_length + chunk_bytes_k - 1 ) / chunk_bytes_k
}

/// The §8.4 split's shape on the consumer side: every chunk but the last is
/// exactly `K`, the last accounts for whatever remains.
///
/// **This is a partial check that presumes its caller's guards, and its twin in
/// `native_tx_carriage_v1` (`chunk_shape_matches`) is not.** The two differ in
/// exactly two places: on the empty list this answers `total_length == 0` where
/// the producer's answers `False`, and on the last chunk this checks only
/// `length == total_length` where the producer's also requires
/// `0 < total_length <= K`. Neither difference is reachable. `certified_view`
/// has already established `total_length > K`, `chunk_count ==
/// expected_chunk_count(total_length)` and a chunk list of exactly
/// `chunk_count` elements before it calls this, so `ceil(total_length / K) >= 2`
/// makes the empty case unreachable, and by the last element the remainder is
/// `total_length - (n-1)·K`, which lies in `(0, K]` by arithmetic. The producer
/// side is written total because it is what a *minting* policy runs; this side
/// is the per-read hot path and does not re-derive what its caller just proved.
///
/// The consequence for anyone editing `certified_view`: those three guards are
/// load-bearing for this function, not merely for the ones they appear to
/// guard. Do not reorder them below this call or relax them without making this
/// function total first. Consolidating the two into one shared form is a
/// Phase-1-surface change tracked in the #565 lane.
fn chunk_lengths_match(chunks: List<ByteArray>, total_length: Int) -> Bool {
  when chunks is {
    [] -> total_length == 0
    [chunk] -> bytearray.length(chunk) == total_length
    [chunk, ..rest] -> and {
        bytearray.length(chunk) == chunk_bytes_k,
        chunk_lengths_match(rest, total_length - chunk_bytes_k),
      }
  }
}

fn count_from_total_length(stride: Int, total_length: Int) -> Int {
  expect stride > walk_derived_stride
  let one_byte_count = ( total_length - 1 ) / stride
  if and {
    one_byte_count <= 23,
    1 + stride * one_byte_count == total_length,
  } {
    one_byte_count
  } else {
    let two_byte_count = ( total_length - 2 ) / stride
    if and {
      two_byte_count >= 24,
      two_byte_count <= 255,
      2 + stride * two_byte_count == total_length,
    } {
      two_byte_count
    } else {
      let three_byte_count = ( total_length - 3 ) / stride
      expect three_byte_count > 255
      expect three_byte_count <= max_field_item_count
      expect 3 + stride * three_byte_count == total_length
      three_byte_count
    }
  }
}

// ---------------------------------------------------------------------------
// Carriage resolution
// ---------------------------------------------------------------------------

/// §8.5: raw carriage is unauthenticated data published as a nothing-but-bytes
/// inline datum. Nothing here trusts provenance — content is verified by hash
/// at consumption, so wrong bytes simply fail.
fn raw_carriage_bytes(reference_inputs: List<Input>, index: Int) -> ByteArray {
  expect index >= 0
  expect Some(input) = list.at(reference_inputs, index)
  expect InlineDatum(datum) = input.output.datum
  builtin.un_b_data(datum)
}

// ---------------------------------------------------------------------------
// Accessors — slice-only, straddle-aware
// ---------------------------------------------------------------------------

/// Reveal-derived item count (§5.2). It is mirrored nowhere else in the
/// format, so every answer this returns is authenticated: under tiers 1–2 the
/// header is read out of a hash-checked preimage that also passed §5.1's
/// full-content check, and under tier 3 a fixed-stride field's count is §7.4
/// arithmetic over the certificate's mint-verified `total_length`.
///
/// **It aborts for a variable-width field under tier 3.** There the count
/// exists only in the §5.1 header, and nothing affordable authenticates it.
/// Tiers 1–2 authenticate theirs by walking the whole preimage at view
/// construction; tier 3 cannot run that walk, because `read_chunked_range`
/// re-verifies the chunk a read lands in every time and an N-item walk
/// therefore costs N `blake2b_256` hashes over a whole chunk. Rather than
/// return a number nobody checked — which a count-consuming rule would take
/// for a fact, so that a preimage miscounted on purpose would satisfy a rule
/// the real items do not — the door declines to answer. A function that
/// aborts is a smaller hazard than one that answers wrongly.
///
/// Reads are unaffected: `field_item_at` still serves such a view. Its guard
/// is the header's declared count, and the envelope walk behind it fails
/// closed the moment it leaves the committed bytes, so a read either returns
/// bytes the producer committed at the position it claims or aborts.
pub fn field_item_count(view: FieldViewV1) -> Int {
  when view is {
    Whole { count, .. } -> count
    Chunked { count, stride, .. } -> {
      expect stride > walk_derived_stride
      count
    }
  }
}

/// The count as the view holds it, with no authentication claim attached: the
/// self-asserted §5.1 header number for a variable-width field under tier 3,
/// and the authenticated count everywhere else. Private, and used only as the
/// range guard on a read — never handed to a caller as the item count.
fn declared_item_count(view: FieldViewV1) -> Int {
  when view is {
    Whole { count, .. } -> count
    Chunked { count, .. } -> count
  }
}

pub fn field_total_length(view: FieldViewV1) -> Int {
  when view is {
    Whole { bytes, .. } -> bytearray.length(bytes)
    Chunked { chunks, .. } ->
      list.foldl(
        chunks,
        0,
        fn(chunk, total) { total + bytearray.length(chunk) },
      )
  }
}

/// The canonical item encoding `enc_i` at `index`, with its byte-string
/// wrapper stripped.
///
/// §7.3: fails unless `0 ≤ index < count` **and** the item's full byte range
/// lies inside the preimage. Fixed-stride fields resolve arithmetically
/// (§5.3); variable-width fields walk the envelope, one head decode plus a
/// byte jump per skipped item, with no offset table to trust (§7.2).
///
/// The `count` in that guard is the view's declared count, which for a
/// variable-width field under tier 3 is the §5.1 header's own number and is
/// therefore not authenticated (see `field_item_count`). It is a range guard,
/// not the source of the answer: what makes a read sound is that the walk and
/// the chunk digests behind it fail closed the moment the read leaves the
/// committed bytes, so an inflated count buys extra indices that all abort and
/// a deflated one only refuses reads.
pub fn field_item_at(view: FieldViewV1, index: Int) -> ByteArray {
  let count = declared_item_count(view)
  expect index >= 0
  expect index < count
  let Pair(offset, length) = field_item_extent(view, index)
  field_read_range(view, offset, length)
}

/// The `(offset, length)` of item `index`'s payload within the preimage.
/// Exposed because a resumable walk records positions, never bytes (§7.6).
///
/// **Both branches read the item's own wrapper.** The fixed-stride arithmetic
/// says *where* item `index` begins; only the two `definite_bytes_header`
/// bytes there say that the item is spelled the one way §5.1 admits. Skipping
/// them would let `81 ‖ 00 00 ‖ <28 B>` and `81 ‖ ff ff ‖ <28 B>` open beside
/// the canonical `81 ‖ 58 1c ‖ <28 B>` and hand back the same payload — three
/// admissible byte forms for one logical field, which §6.1 forbids and which
/// would leave a non-canonically committed preimage unfaultable. The wrapper
/// read is O(1) per access, so §7.1's authenticate-once cost model and tier
/// 3's lazy chunk verify are untouched: the check rides along on the chunk the
/// item is about to be read from anyway.
pub fn field_item_extent(view: FieldViewV1, index: Int) -> Pair<Int, Int> {
  let count = declared_item_count(view)
  expect index >= 0
  expect index < count
  let stride = field_view_stride(view)
  if stride > walk_derived_stride {
    let item_offset = header_len_for_count(count) + stride * index
    let Pair(payload_offset, length) = item_header_at(view, item_offset)
    // Every fixed stride in §5.3 wraps 24..255 payload bytes, so the canonical
    // wrapper is exactly the two-byte `58 LL` form and the stride pins `LL`.
    expect payload_offset == item_offset + fixed_item_wrapper_bytes
    expect length == stride - fixed_item_wrapper_bytes
    Pair(payload_offset, length)
  } else {
    walk_item_extent(view, header_len_for_count(count), index)
  }
}

/// §5.3's stride for the field this view was opened on:
/// `walk_derived_stride` for the variable-width fields, the item stride for
/// the fixed-width ones.
///
/// Public for the same reason `field_item_extent` is (§7.6): a resumable walk
/// records positions, and which *kind* of position a field has — arithmetic or
/// walk-derived — is what decides whether a carried offset is self-authenticating
/// (`midgard/native_tx_machine_walk_v1`, spec §10.2).
pub fn field_view_stride(view: FieldViewV1) -> Int {
  when view is {
    Whole { stride, .. } -> stride
    Chunked { stride, .. } -> stride
  }
}

/// The byte width of this view's §5.1 array header — equivalently, the offset
/// at which item 0's wrapper begins.
///
/// Public so a walk's *opening* position is derived from the authenticated view
/// rather than supplied by a redeemer (§10.2). The count it reads is the view's
/// own, fixed at construction, so this cannot be steered from outside.
pub fn field_header_len(view: FieldViewV1) -> Int {
  header_len_for_count(declared_item_count(view))
}

fn header_len_for_count(count: Int) -> Int {
  expect count >= 0
  if count <= 23 {
    1
  } else if count <= 255 {
    2
  } else {
    expect count <= max_field_item_count
    3
  }
}

/// Reads `length` bytes at `offset`, stitching across chunk boundaries and
/// verifying every chunk it touches (§8.8 straddle awareness, lazy verify).
pub fn field_read_range(
  view: FieldViewV1,
  offset: Int,
  length: Int,
) -> ByteArray {
  when view is {
    Whole { bytes, .. } -> slice_exact(bytes, offset, length)
    Chunked { chunks, chunk_digests, .. } ->
      read_chunked_range(chunks, chunk_digests, offset, length)
  }
}

fn read_chunked_range(
  chunks: List<ByteArray>,
  chunk_digests: List<ByteArray>,
  offset: Int,
  length: Int,
) -> ByteArray {
  expect offset >= 0
  expect length >= 0
  if length == 0 {
    #""
  } else {
    let chunk_index = offset / chunk_bytes_k
    let within = offset - chunk_index * chunk_bytes_k
    expect Some(chunk) = list.at(chunks, chunk_index)
    expect Some(digest) = list.at(chunk_digests, chunk_index)
    // Lazy chunk verify: a chunk is hashed the first time a read reaches it,
    // and a chunk nobody reads is never hashed at all.
    expect blake2b_256(chunk) == digest
    let available = bytearray.length(chunk) - within
    expect available > 0
    if length <= available {
      slice_exact(chunk, within, length)
    } else {
      bytearray.concat(
        slice_exact(chunk, within, available),
        read_chunked_range(
          chunks,
          chunk_digests,
          offset + available,
          length - available,
        ),
      )
    }
  }
}

// ---------------------------------------------------------------------------
// Envelope walking
// ---------------------------------------------------------------------------

/// Decodes the §5.1 `definite_bytes_header(L)` at `offset`, returning
/// `(payload offset, payload length)`. Minimal width; fails closed otherwise.
///
/// Public as the resumable walk's single step primitive (§10.2). It stays the
/// **only** reader of a §5.1 item head in the tree — the walk core steps
/// through this function rather than re-deriving the grammar, so one logical
/// field keeps one verdict (§6.1). Advancing from an arbitrary `offset` is what
/// makes a walk resumable without re-reading completed items; the offset is a
/// position, never a claim about content, and every byte it reaches is read
/// through `field_read_range` and so stays inside the authenticated bytes.
pub fn field_item_header_at(view: FieldViewV1, offset: Int) -> Pair<Int, Int> {
  item_header_at(view, offset)
}

fn item_header_at(view: FieldViewV1, offset: Int) -> Pair<Int, Int> {
  let tag = bytearray.at(field_read_range(view, offset, 1), 0)
  if and {
    tag >= 64,
    tag <= 87,
  } {
    Pair(offset + 1, tag - 64)
  } else if tag == 88 {
    let length = bytearray.at(field_read_range(view, offset + 1, 1), 0)
    expect length >= 24
    Pair(offset + 2, length)
  } else {
    expect tag == 89
    let head = field_read_range(view, offset + 1, 2)
    let length = bytearray.at(head, 0) * 256 + bytearray.at(head, 1)
    expect length > 0xff
    Pair(offset + 3, length)
  }
}

fn walk_item_extent(
  view: FieldViewV1,
  offset: Int,
  remaining: Int,
) -> Pair<Int, Int> {
  let Pair(payload_offset, length) = item_header_at(view, offset)
  if remaining <= 0 {
    Pair(payload_offset, length)
  } else {
    walk_item_extent(view, payload_offset + length, remaining - 1)
  }
}

/// Walks `count` enveloped items from `offset` and returns the offset one past
/// the last item. Callers compare it against the preimage length so that a
/// miscounted or truncated preimage fails closed (§5.1).
fn walk_to_end(preimage: ByteArray, offset: Int, remaining: Int) -> Int {
  if remaining <= 0 {
    offset
  } else {
    let Pair(payload_offset, length) = item_header_at(
      Whole { bytes: preimage, count: 0, stride: 0 },
      offset,
    )
    walk_to_end(preimage, payload_offset + length, remaining - 1)
  }
}
