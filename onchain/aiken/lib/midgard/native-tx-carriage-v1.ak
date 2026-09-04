//// The publication half of the §8 field-preimage carriage ladder —
//// `docs/spec/midgard-tx.md` §8.4 through §8.7.
////
//// `midgard/native_tx_field_access_v1` is the **consumer** half: it takes
//// carriage that already exists and turns it into an authenticated view. This
//// module is the **producer** half, and it owns exactly two things:
////
////   * **The deterministic split rule (§8.4).** Chunk `j` is bytes
////     `[j·K, (j+1)·K)` with a ragged last chunk and minimum-necessary chunks
////     by construction. It is a pure function of the preimage bytes, which is
////     what makes independent publishers byte-compatible: identical chunks,
////     identical digest vectors, interchangeable certificates. Content
////     addressing (§8.7) is only meaningful because of that — two publishers
////     who never met produce the same content address for the same bytes, so
////     anyone's republication heals anyone's certificate.
////   * **The certification predicate (§8.6).** Everything the permissionless
////     minting policy has to prove about a `FieldPreimageCertificateV1` before
////     a token may exist for it. `validators/field-preimage-certificate.ak` is
////     the thin transaction-shape wrapper around this; the content rules live
////     here so they are reachable by a focused test without building a
////     transaction.
////
//// **Positional identity stays internal (§4).** Certification needs the field
//// commitment the tx-id committed, and §4 says an expected hash may only come
//// from a committed structure in view. So this module takes the committed
//// structures as CBOR, re-derives the tx-id from them through the unchanged §3
//// derivation, and reads the field hash out of the result itself. No caller
//// ever hands in a free-standing field hash and none is ever handed back —
//// the same discipline `authenticated_field_view` keeps behind its door, kept
//// here for the one consumer that cannot go through that door: the certifier,
//// which exists precisely because no view is authenticable yet.
////
//// **What is shared with the door, and what is not.** Everything that has a
//// wire consequence is `native_tx_field_access_v1`'s and is imported rather
//// than restated: `FieldPreimageCertificateV1`, `chunk_bytes_k`,
//// `max_tier3_chunk_count`, `max_transaction_aggregate_field_bytes`,
//// `expected_chunk_count`, `field_count` and `field_commitment`. A producer
//// and a consumer who disagreed about any of those would disagree about what
//// a certificate *is*, and that class of drift is closed by construction.
//// (The §8.6 asset name is a shared constant of the same module since #606
//// retired its per-certificate derivation; only the transaction-shape wrapper
//// reads it.)
////
//// Four pieces of logic are nonetheless written twice, once here and once
//// behind the door, and each is a deliberate copy rather than an oversight:
////
////   * **The §2.5 positional table** (`field_commitment_at` here,
////     `field_commitment_at` there). Private on both sides on purpose. §4 says
////     an expected field hash may only come from a committed structure in
////     view, and the way that is enforced rather than merely documented is
////     that neither module can hand the extraction to anyone: on both sides
////     the table is reachable only *after* the caller has proved the witness
////     set is the one the tx-id committed. An exported copy would have to
////     carry that coupling in its own signature — a wrapper taking the proof
////     as an argument could enforce it — so this is a deferral rather than an
////     impossibility: `native_tx_field_access_v1` is a frozen shared surface
////     for the duration of #573, and shaping such a wrapper belongs to the
////     #565 serialized Phase-1-surface patch rather than to this lane. The
////     two are pinned to each other by test rather than by import: here
////     `certification_binds_each_field_to_its_positional_commitment` drives
////     all nine slots against a fixture where every slot holds a different
////     commitment, and the door's suite pins the same nine through
////     `door_serves_field_0_spend_inputs` .. `door_serves_field_8_redeemer_witnesses`
////     plus `a_field_cannot_be_opened_in_another_slot`. Either table drifting
////     alone reddens its own suite.
////   * **`raw_chunk_bytes`** (`raw_carriage_bytes` behind the door). Four
////     lines of §8.5 datum reading with no shared state and no wire
////     consequence: a divergence would make one side reject carriage the other
////     accepts, never make either accept wrong bytes, because the bytes are
////     hash-checked afterwards on both sides.
////   * **`chunk_shape_matches`** (`chunk_lengths_match` behind the door). The
////     two differ, and the difference is documented at both sites: this copy
////     is the standalone one, the door's leans on the guards its caller has
////     already run. See `chunk_shape_matches` below.
////   * **`chunk_slice`** (the door keeps its own three-line slicing helper).
////     Three lines of §7.3 clamp-hazard discipline with no shared state and no
////     wire consequence; admitted at its own site below for the same reason
////     `raw_chunk_bytes` is, and counted here so the inventory is complete.
////
//// Consolidating the §2.5 table and `chunk_shape_matches` into one shared
//// surface is a Phase-1 change to `native_tx_field_access_v1` and therefore
//// not a local edit: it belongs to the serialized Phase-1-surface patch in the
//// #565 lane, which every overlap lane rebases onto. Recorded here so that
//// patch inherits it.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/transaction.{InlineDatum, Input}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_witness_set_compact, verify_native_tx_compact_cbor_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxCompact, NativeTxWitnessSetCompact,
}
use midgard/native_tx_field_access_v1.{
  FieldPreimageCertificateV1, chunk_bytes_k, expected_chunk_count,
  field_commitment, field_count, max_tier3_chunk_count,
  max_transaction_aggregate_field_bytes,
}

// ---------------------------------------------------------------------------
// Wire types
// ---------------------------------------------------------------------------

/// The §8.6 certification redeemer. Constructor order is frozen consensus wire
/// format — `Certify` is Constr 0, `Retire` 1 — because an off-chain minter
/// emits these tags and the compiled policy branches on them.
///
/// `Certify` carries no identity: publication and certification are
/// permissionless (§8.7), so the policy checks content and never who supplied
/// it. What it carries is the committed structures the field hash is extracted
/// from, and the positional order of the raw chunks (§8.6: "order is supplied
/// by the redeemer's reference-input indices and verified in one shot").
pub type FieldPreimageCertificateMintRedeemerV1 {
  Certify {
    /// The disputed L2 transaction's compact CBOR. The tx-id is re-derived
    /// from this, never taken from the redeemer.
    compact_cbor: ByteArray,
    /// Its witness-set compact CBOR, checked against the compact structure's
    /// committed `witness_set_hash`.
    witness_set_compact_cbor: ByteArray,
    /// All-chunks-positional: element `k` is the reference-input index of
    /// chunk `k` (at most `max_tier3_chunk_count` under §8.3).
    chunk_ref_input_indices: List<Int>,
    /// Index of the transaction output carrying the new certificate.
    output_index: Int,
  }
  /// Burn. Cleanup is owner-discretionary (§8.7); the spend handler is what
  /// checks the owner's signature, so this branch only has to refuse to let a
  /// burn redeemer mint anything.
  Retire
}

// ---------------------------------------------------------------------------
// §8.4 the deterministic split rule
// ---------------------------------------------------------------------------

/// The only slice in this module. `bytearray.slice` clamps out-of-range
/// arguments — the §7.3 abort-never-clamp hazard — so the bound is checked
/// first and the read fails closed. (`native_tx_field_access_v1` keeps its own
/// copy of this for its accessors; both are three lines and neither is worth a
/// shared dependency that would let one module's slicing discipline be
/// weakened for the other's convenience.)
fn chunk_slice(bytes: ByteArray, offset: Int, length: Int) -> ByteArray {
  expect offset >= 0
  expect length > 0
  expect offset + length <= bytearray.length(bytes)
  bytearray.slice(bytes, offset, offset + length - 1)
}

/// §8.4's deterministic split: chunk `j` = bytes `[j·K, (j+1)·K)`, ragged last
/// chunk, minimum-necessary chunks.
///
/// Reproducible from the bytes alone — no length table, no publisher state, no
/// transaction context. That is the whole basis of §8.7's content addressing
/// and of healing: an unrelated party who obtains the same preimage produces
/// the same chunks, so re-publishing after a yank produces carriage that the
/// existing certificate still describes.
///
/// Defined for any preimage from one byte up to the §5.4 aggregate cap; the
/// tier-3 lower bound (`total_length > K`) is a *certificate* rule, not a
/// splitting rule, and is enforced where certificates are made and consumed.
pub fn split_field_preimage(preimage: ByteArray) -> List<ByteArray> {
  let total_length = bytearray.length(preimage)
  expect total_length > 0
  expect total_length <= max_transaction_aggregate_field_bytes
  split_from(preimage, 0, total_length)
}

fn split_from(
  preimage: ByteArray,
  offset: Int,
  total_length: Int,
) -> List<ByteArray> {
  let remaining = total_length - offset
  if remaining <= chunk_bytes_k {
    [chunk_slice(preimage, offset, remaining)]
  } else {
    [
      chunk_slice(preimage, offset, chunk_bytes_k),
      ..split_from(preimage, offset + chunk_bytes_k, total_length)
    ]
  }
}

/// The digest vector a certificate carries for `preimage`: `blake2b_256` of
/// each §8.4 chunk, in order.
pub fn field_preimage_chunk_digests(preimage: ByteArray) -> List<ByteArray> {
  list.map(split_field_preimage(preimage), blake2b_256)
}

/// The certificate a publisher of `preimage` should build — the producer twin
/// of `verify_field_preimage_certificate_v1`, derived from the bytes and the
/// `(tx_id, field_index)` binding alone.
///
/// `total_length > K` is the §8.4 tier boundary, enforced here as well as at
/// consumption so that a certificate for a preimage that fits tier 1 or tier 2
/// cannot be brought into existence at all. The tiering is a partition, not a
/// preference.
pub fn field_preimage_certificate_v1(
  owner: VerificationKeyHash,
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
) -> FieldPreimageCertificateV1 {
  let total_length = bytearray.length(preimage)
  expect total_length > chunk_bytes_k
  expect bytearray.length(owner) == 28
  // The retired asset-name derivation used to force these two bounds as a
  // side effect (#606 made the name a constant); they are what §8.6 requires
  // of the datum, so they are enforced here directly. The upper bound reads
  // §2.5's field count from its single definition
  // (`native_tx_field_access_v1.field_count`) rather than restating the
  // literal, so a producer and the door cannot drift apart about how many
  // fields exist — the same discipline the module header records for every
  // other bound shared with the door.
  expect field_index >= 0
  expect field_index < field_count
  expect bytearray.length(tx_id) == 32
  FieldPreimageCertificateV1 {
    owner,
    tx_id,
    field_index,
    field_hash: field_commitment(preimage),
    total_length,
    chunk_digests: field_preimage_chunk_digests(preimage),
  }
}

// ---------------------------------------------------------------------------
// §2.5 positional hash extraction (internal by design — see the module note)
// ---------------------------------------------------------------------------

/// §2.5's field-index table, over structures the tx-id has just been re-derived
/// from. Private for the same reason the door's copy is: an expected hash may
/// only come from a committed structure in view (§4), and keeping the table
/// behind the predicate is how that is enforced rather than merely documented.
///
/// §2.4's transposition is live here — wire position 6 is *script* witnesses
/// and 7 is *address* witnesses, the opposite of the record declaration order —
/// so this module's test file drives 6 and 7 against each other in both
/// directions rather than trusting the reading:
/// `certification_rejects_the_address_preimage_under_field_six` and
/// `certification_rejects_the_script_preimage_under_field_seven`. A table that
/// transposed only one of the pair fails the first; one that transposed both
/// fails the second.
fn field_commitment_at(
  body: NativeTxBodyCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
) -> ByteArray {
  when field_index is {
    0 -> body.spend_inputs_hash
    1 -> body.reference_inputs_hash
    2 -> body.outputs_hash
    3 -> body.required_observers_hash
    4 -> body.required_signers_hash
    5 -> body.mint_hash
    6 -> witness_set.script_tx_wits_hash
    7 -> witness_set.addr_tx_wits_hash
    8 -> witness_set.redeemer_tx_wits_hash
    _ -> fail @"field index outside 0..8"
  }
}

// ---------------------------------------------------------------------------
// §8.6 certification
// ---------------------------------------------------------------------------

/// §8.5: raw carriage is unauthenticated data published as a nothing-but-bytes
/// inline datum. Nothing here trusts provenance — the bytes are verified by
/// hash below, so wrong bytes simply fail and a chunk UTxO may sit at any
/// address, under any key, published by anyone.
///
/// Byte-identical to the door's private `raw_carriage_bytes`; see the module
/// note for why that copy is tolerated where the §2.5 table's is only tolerated
/// under protest.
pub fn raw_chunk_bytes(reference_inputs: List<Input>, index: Int) -> ByteArray {
  expect index >= 0
  expect Some(input) = list.at(reference_inputs, index)
  expect InlineDatum(datum) = input.output.datum
  builtin.un_b_data(datum)
}

/// Everything §8.6 requires of a certificate before a token may exist for it.
///
/// The order matters. `verify_native_tx_compact_cbor_v1` re-derives the tx-id
/// from the supplied body through the unchanged §3 derivation and compares it
/// to the certificate's `tx_id`, so a certificate naming another transaction is
/// refused before any byte of carriage is looked at — a certificate is a
/// statement about one transaction's one field, and it is that binding, not the
/// bytes, that is checked first.
///
/// `witness_set_compact_cbor` is required and checked for every field, not only
/// for 6–8. The door skips it below field 6 because it re-hashes on every
/// dispute step and the saving is real there; certification happens once per
/// field per transaction, so one code path with no conditional authentication
/// is worth more than one hash.
///
/// The chunks are checked three ways, and all three are needed: their *shape*
/// (all but the last exactly `K`, the last ragged and non-empty) is what makes
/// them the §8.4 split rather than an arbitrary partition, so offset math over
/// the digest vector is arithmetic; their *digests* are what a later single-
/// chunk read authenticates against; and their *concatenation* is what ties the
/// whole thing to the field commitment the tx-id committed. Dropping the shape
/// check would let a publisher certify a lopsided split whose chunk boundaries
/// no consumer could compute, and dropping the per-chunk digests would leave
/// §8.6's "O(one chunk hash)" access with nothing to compare against.
pub fn verify_field_preimage_certificate_v1(
  certificate: FieldPreimageCertificateV1,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  chunks: List<ByteArray>,
) -> Bool {
  let FieldPreimageCertificateV1 { owner, tx_id, field_index, .. } = certificate
  let total_length = certificate.total_length
  let chunk_digests = certificate.chunk_digests
  let certified_field_hash = certificate.field_hash
  // The minter sets the min-Ada reclaim authority (§8.6) and nothing checks who
  // that is, but it has to be a spendable key hash or the output is dead.
  expect bytearray.length(owner) == 28

  // The binding (§8.6): the tx-id is re-derived, never accepted.
  //
  // The re-derive-then-bind-the-witness-set pair below is the same pair
  // `compact.verify_native_tx_proof_source_v1` opens with, and it is not reused
  // here on purpose: that function additionally requires a
  // `field_preimage_lengths_cbor` and re-encodes it for canonicality. A
  // certifier has no such vector and no business acquiring one — it certifies
  // one field of one transaction from the chunks in front of it — so reusing it
  // would put a fourth, unused ByteArray in the frozen §8.6 redeemer whose only
  // job would be to satisfy a check about something else. Two lines of overlap
  // is the cheaper price than a wire field that means nothing.
  let verified = verify_native_tx_compact_cbor_v1(tx_id, compact_cbor)
  let NativeTxCompact { body, witness_set_hash, .. } = verified.tx_compact
  expect blake2b_256(witness_set_compact_cbor) == witness_set_hash
  let witness_set = decode_native_tx_witness_set_compact(
    witness_set_compact_cbor,
  )
  let expected_hash = field_commitment_at(body, witness_set, field_index)
  // The E2 weld (#606, owner ruling 2026-08-16): the datum's `field_hash` is
  // the same commitment the chunk concatenation is checked against below, so
  // `field_hash ↔ chunk_digests` is welded inside the mint-verified datum.
  // Where the hash "came from" is irrelevant; that the chunks were verified
  // against it is everything — a door can then compare `field_hash` to a
  // commitment it authenticated itself and be talking about these bytes.
  expect certified_field_hash == expected_hash

  // §8.4's tier boundary, enforced at both ends of the ladder.
  expect total_length > chunk_bytes_k
  expect total_length <= max_transaction_aggregate_field_bytes
  let chunk_count = list.length(chunk_digests)
  expect chunk_count == expected_chunk_count(total_length)
  expect chunk_count <= max_tier3_chunk_count
  expect list.length(chunks) == chunk_count
  expect chunk_shape_matches(chunks, total_length)
  expect list.map(chunks, blake2b_256) == chunk_digests

  field_commitment(concat_chunks(chunks)) == expected_hash
}

/// The §8.4 split's shape: every chunk but the last is exactly `K`, and the
/// last is ragged but never empty. Together with `expected_chunk_count` this
/// pins the partition to the one the split rule produces, so `chunk j` really
/// is bytes `[j·K, (j+1)·K)` and a consumer can locate an offset arithmetically.
///
/// **How this differs from `chunk_lengths_match` behind the door, and why.**
/// The two express the same shape and agree on every input either can reach,
/// but they are not the same function:
///
///   * on the empty list this answers `False`; the door's answers
///     `total_length == 0`;
///   * on the last chunk this additionally requires `0 < remaining <= K`; the
///     door's checks only `length(chunk) == remaining`.
///
/// Neither reading is a gap in the other, because both are called only behind
/// the same three guards — `total_length > K`, `chunk_count ==
/// expected_chunk_count(total_length)`, and the supplied chunk list having
/// exactly `chunk_count` elements. Under those, `ceil(total_length / K) >= 2`,
/// so the empty case is unreachable on both sides, and by the time the
/// recursion reaches the last element `remaining = total_length - (n-1)·K`
/// lies in `(0, K]` by arithmetic rather than by check. The extra conjuncts
/// here are therefore redundant *given the caller*, and that is exactly the
/// point of writing them: this copy is the one a minting policy runs, where a
/// certificate is brought into existence rather than merely read, so it is
/// written to be total — false for every input that is not the §8.4 shape,
/// including inputs its current caller cannot produce. The door's copy is the
/// hot path and leans on the guards its caller has just run.
///
/// If the two are ever consolidated (see the module note), the total form is
/// the one to keep: it is the one that stays correct if the guard order above
/// is rearranged.
fn chunk_shape_matches(chunks: List<ByteArray>, remaining: Int) -> Bool {
  when chunks is {
    [] -> False
    [last] -> and {
        remaining > 0,
        remaining <= chunk_bytes_k,
        bytearray.length(last) == remaining,
      }
    [chunk, ..rest] -> and {
        bytearray.length(chunk) == chunk_bytes_k,
        chunk_shape_matches(rest, remaining - chunk_bytes_k),
      }
  }
}

fn concat_chunks(chunks: List<ByteArray>) -> ByteArray {
  list.foldl(chunks, #"", fn(chunk, joined) { bytearray.concat(joined, chunk) })
}
