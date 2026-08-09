//// Seam vectors for the §8 carriage producer — the deterministic split rule
//// (§8.4) and the certification predicate (§8.6) of
//// `docs/spec/midgard-tx.md`.
////
//// Three properties carry this module, and each is established rather than
//// restated:
////
////   * **The split is reproducible from the bytes alone.** Not asserted by
////     calling the function twice, but pinned two ways: against an independent
////     take/drop derivation of the same partition, and against the
////     cross-language straddle vector in
////     `demo/midgard-core/tests/fixtures/native-tx-field-items-v1.generated.json`
////     — the same 400-byte block, the same 16,003-byte preimage, the same two
////     chunk digests the TypeScript twin produces. If the on-chain split and
////     the off-chain one ever disagreed about where chunk 0 ends, carriage
////     published by one side's tooling would not be certifiable through the
////     other's and healing would quietly stop working; this is the test that
////     would notice.
////   * **A certificate is a statement about one transaction's one field.** The
////     positional table is driven for all nine fields at once against a
////     fixture where every slot holds a different commitment, so a transposed
////     table cannot pass. §2.4's live transposition — wire position 6 is
////     *script* witnesses and 7 is *address* witnesses — is driven in both
////     directions on its own.
////   * **Healing is a property of content addressing, not of a role.** The
////     healing test re-derives carriage from the preimage a second time, the
////     way an unrelated party would, and certifies the original certificate
////     against those bytes: same content address, different publisher.
////
//// Fixture economy: every preimage here is exactly one byte over `K` — the
//// smallest length that is legally tier 3 — except where a test is about a
//// larger size on purpose (the aggregate cap, the straddle vector). Tier-3
//// fixtures are 16 KB each and the compiler pays for every one of them in
//// every test that touches it.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, encode_native_tx_compact_v1,
  encode_native_tx_witness_set_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxCompact, NativeTxWitnessSetCompact,
  VerifiedMidgardNativeTxCompact, native_tx_version_v1,
}
use midgard/native_tx_carriage_v1.{
  field_preimage_certificate_v1, field_preimage_chunk_digests,
  split_field_preimage, verify_field_preimage_certificate_v1,
}
use midgard/native_tx_field_access_v1.{
  Certified, FieldPreimageCertificateV1, FieldViewV1, authenticated_field_view,
  chunk_bytes_k, expected_chunk_count, field_commitment, field_item_at,
  field_item_count, field_item_extent, field_preimage_certificate_asset_name,
  max_tier3_chunk_count, max_transaction_aggregate_field_bytes,
  spend_input_stride,
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const zero_hash32: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const owner_hash: ByteArray =
  #"66666666666666666666666666666666666666666666666666666666"

/// A second publisher, so the healing test is about a different party rather
/// than about the same party running twice.
const healer_hash: ByteArray =
  #"77777777777777777777777777777777777777777777777777777777"

const certificate_policy_id: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

/// The smallest legally tier-3 preimage: one byte over `K`, so the split is
/// `[K, 1]` and the ragged tail is as ragged as it gets.
const smallest_tier3_bytes: Int = chunk_bytes_k + 1

/// Grows `seed` by doubling until it covers `length`. Doubling rather than
/// appending a block at a time because a 16 KB fixture built four bytes at a
/// time is four thousand concatenations where this is twelve.
fn grow(acc: ByteArray, length: Int) -> ByteArray {
  if bytearray.length(acc) >= length {
    acc
  } else {
    grow(bytearray.concat(acc, acc), length)
  }
}

/// `length` bytes that begin with `marker`. Two fixtures built from different
/// markers differ in every byte, and therefore in every chunk digest and every
/// commitment derived from them.
fn marked_bytes(marker: ByteArray, length: Int) -> ByteArray {
  let marker_length = bytearray.length(marker)
  expect length >= marker_length
  bytearray.take(grow(marker, length), length)
}

fn field_marker(field_index: Int) -> ByteArray {
  bytearray.push(#"a5c30f", 16 + field_index)
}

/// Field `field_index`'s own tier-3 preimage. Distinct per field by
/// construction, which is what lets the nine-field positional test fail.
fn field_preimage(field_index: Int) -> ByteArray {
  marked_bytes(field_marker(field_index), smallest_tier3_bytes)
}

/// The committed body holding **every** field's own commitment in its own §2.5
/// slot at the same time. A certification that reaches the wrong slot finds
/// another field's commitment there, never a placeholder, so it fails instead
/// of passing by coincidence.
fn nine_field_body() -> NativeTxBodyCompact {
  let slot = fn(index: Int) -> ByteArray {
    field_commitment(field_preimage(index))
  }
  NativeTxBodyCompact {
    spend_inputs_hash: slot(0),
    reference_inputs_hash: slot(1),
    outputs_hash: slot(2),
    fee: 0,
    validity_interval_start: 0,
    validity_interval_end: 0,
    required_observers_hash: slot(3),
    required_signers_hash: slot(4),
    mint_hash: slot(5),
    script_integrity_hash: zero_hash32,
    auxiliary_data_hash: zero_hash32,
    network_id: 255,
  }
}

fn nine_field_witness_set() -> NativeTxWitnessSetCompact {
  let slot = fn(index: Int) -> ByteArray {
    field_commitment(field_preimage(index))
  }
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: slot(7),
    script_tx_wits_hash: slot(6),
    redeemer_tx_wits_hash: slot(8),
  }
}

fn compact_cbor_of(
  body: NativeTxBodyCompact,
  witness_set_compact_cbor: ByteArray,
) -> ByteArray {
  encode_native_tx_compact_v1(
    NativeTxCompact {
      body,
      witness_set_hash: blake2b_256(witness_set_compact_cbor),
      validity_code: 0,
    },
  )
}

fn tx_id_of(body: NativeTxBodyCompact) -> ByteArray {
  native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(body),
  )
}

/// `(tx_id, compact_cbor, witness_set_compact_cbor)` — exactly what a
/// certification redeemer carries, plus the id it must derive to. The tx-id is
/// never in the redeemer: the predicate re-derives it from these bytes, which
/// is §8.6's binding.
fn nine_field_fixture() -> (ByteArray, ByteArray, ByteArray) {
  let body = nine_field_body()
  let witness_set_compact_cbor = encode_native_tx_witness_set_compact(
    nine_field_witness_set(),
  )
  (
    tx_id_of(body),
    compact_cbor_of(body, witness_set_compact_cbor),
    witness_set_compact_cbor,
  )
}

/// Certify field `field_index` from the bytes a publisher of `preimage` would
/// have produced — the ordinary honest path, folded into one call so a negative
/// test differs from the positive one in exactly the thing it is about.
fn certify_against(
  fixture: (ByteArray, ByteArray, ByteArray),
  field_index: Int,
  preimage: ByteArray,
) -> Bool {
  let (tx_id, compact_cbor, witness_set_compact_cbor) = fixture
  verify_field_preimage_certificate_v1(
    field_preimage_certificate_v1(owner_hash, tx_id, field_index, preimage),
    compact_cbor,
    witness_set_compact_cbor,
    split_field_preimage(preimage),
  )
}

fn certify(field_index: Int, preimage: ByteArray) -> Bool {
  certify_against(nine_field_fixture(), field_index, preimage)
}

fn joined(chunks: List<ByteArray>) -> ByteArray {
  list.foldl(chunks, #"", fn(chunk, acc) { bytearray.concat(acc, chunk) })
}

// ---------------------------------------------------------------------------
// §8.4 the deterministic split rule
// ---------------------------------------------------------------------------

/// The split's defining properties at the smallest tier-3 length: chunk 0 is
/// exactly `K`, the tail is ragged, the count is minimum-necessary, and the
/// pieces reassemble into the bytes they came from.
test split_is_fixed_k_with_a_ragged_tail() {
  let preimage = field_preimage(0)
  let chunks = split_field_preimage(preimage)
  and {
    bytearray.length(preimage) == smallest_tier3_bytes,
    list.map(chunks, bytearray.length) == [chunk_bytes_k, 1],
    list.length(chunks) == expected_chunk_count(smallest_tier3_bytes),
    joined(chunks) == preimage,
  }
}

/// The §8.3 boundary the tiering turns on: `K` bytes is one chunk, `K + 1` is
/// two. The split rule is defined either side of it — it is the *certificate*
/// that is confined to the tier-3 side, which
/// `certification_rejects_a_preimage_that_fits_tier_two` covers.
test split_boundary_at_k_is_one_chunk() {
  and {
    list.map(
      split_field_preimage(marked_bytes(#"01", chunk_bytes_k)),
      bytearray.length,
    ) == [chunk_bytes_k],
    list.map(
      split_field_preimage(marked_bytes(#"01", chunk_bytes_k + 1)),
      bytearray.length,
    ) == [chunk_bytes_k, 1],
  }
}

/// The §5.4 aggregate cap: the largest preimage the format admits, and the case
/// that fixes `max_tier3_chunk_count` at three.
test split_at_the_aggregate_cap_uses_three_chunks() {
  let preimage = marked_bytes(#"02", max_transaction_aggregate_field_bytes)
  let chunks = split_field_preimage(preimage)
  and {
    list.map(chunks, bytearray.length) == [chunk_bytes_k, chunk_bytes_k, 968],
    list.length(chunks) == max_tier3_chunk_count,
    joined(chunks) == preimage,
  }
}

/// One byte past the cap. A splitter that quietly produced a fourth chunk would
/// hand the certifier a vector no consumer can hold — §8.3 derives the
/// three-chunk maximum from this cap — so it fails closed instead.
test split_refuses_a_preimage_above_the_aggregate_cap() fail {
  split_field_preimage(
    marked_bytes(#"03", max_transaction_aggregate_field_bytes + 1),
  ) != []
}

/// Independent derivation. The partition is restated here as plain take/drop
/// arithmetic — the way a second implementation writes it, with no reference to
/// the recursion under test — and the two must agree byte for byte. That is
/// what "reproducible from the bytes alone" means operationally: anyone holding
/// the preimage reconstructs the same chunks without asking the publisher
/// anything.
test split_agrees_with_an_independent_derivation() {
  let preimage = bytearray.concat(
    marked_bytes(#"04", 2 * chunk_bytes_k),
    marked_bytes(
      #"40",
      max_transaction_aggregate_field_bytes - 2 * chunk_bytes_k,
    ),
  )
  let independent = [
    preimage |> bytearray.take(chunk_bytes_k),
    preimage |> bytearray.drop(chunk_bytes_k) |> bytearray.take(chunk_bytes_k),
    preimage |> bytearray.drop(2 * chunk_bytes_k),
  ]
  and {
    split_field_preimage(preimage) == independent,
    field_preimage_chunk_digests(preimage) == list.map(independent, blake2b_256),
  }
}

// ---------------------------------------------------------------------------
// The cross-language straddle vector
// ---------------------------------------------------------------------------

/// The §8.4 straddle vector, copied from
/// `demo/midgard-core/tests/fixtures/native-tx-field-items-v1.generated.json`
/// (`straddle.blockHex`) and pinned in
/// `native-tx-field-items-v1-golden.test.ak` alongside it: ten field-1 items of
/// 40 bytes each. The copy cannot rot silently — the commitment assertion below
/// re-derives `straddle.commitmentHex` from these bytes, so a mistyped digit
/// fails the test rather than weakening it.
const straddle_block: ByteArray =
  #"58268258201f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f819000058268258203e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb0209101719000158268258205d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f3619000258268258207c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e5519000358268258209ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d741900045826825820bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c931900055826825820d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb21900065826825820f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad11900075826825820171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f01900085826825820363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f190009"

const straddle_commitment: ByteArray =
  #"c33cac158cd252aeb86e3fafb6776fd03e7afacffa7923c05878afca870b4ef1"

fn repeat_block(block: ByteArray, times: Int, acc: ByteArray) -> ByteArray {
  if times <= 0 {
    acc
  } else {
    repeat_block(block, times - 1, bytearray.concat(acc, block))
  }
}

/// 400 field-1 items behind the §5.1 `99 0190` header: 16,003 bytes, the
/// vector's `straddle.totalLength`.
fn straddle_preimage() -> ByteArray {
  bytearray.concat(#"990190", repeat_block(straddle_block, 40, #""))
}

/// The cross-language pin. `straddle.chunkLengths` and
/// `straddle.chunkDigestsHex` are what the TypeScript producer emits for these
/// bytes; this asserts the Aiken split rule lands on the same partition and the
/// same digest vector.
test split_matches_the_cross_language_straddle_vector() {
  let preimage = straddle_preimage()
  and {
    bytearray.length(preimage) == 16003,
    field_commitment(preimage) == straddle_commitment,
    list.map(split_field_preimage(preimage), bytearray.length) == [15900, 103],
    field_preimage_chunk_digests(preimage) == [
      #"3d472a8b6608fd6572a3de26a9f95a37c86c9ba739c39b5b8314f860bc908806",
      #"7243f0ea84ad415d83212fc24569d54be7e81f50198a257f93de50cfcb3d7cb2",
    ],
  }
}

// ---------------------------------------------------------------------------
// §8.6 certification — the tx-id binding
// ---------------------------------------------------------------------------

/// The honest path: a certificate built by the producer twin from the preimage
/// and the `(tx_id, field_index)` binding alone verifies against the carriage
/// the split rule produces.
test certification_accepts_the_producer_certificate() {
  certify(0, field_preimage(0))
}

/// The §2.5 positional table, driven end to end. Every slot in the fixture
/// holds a different field's commitment, so this passes only if each field
/// index reaches its own slot; a transposed table fails on the transposed pair
/// rather than degrading quietly.
test certification_binds_each_field_to_its_positional_commitment() {
  let fixture = nine_field_fixture()
  list.all(
    list.range(0, 8),
    fn(index) { certify_against(fixture, index, field_preimage(index)) },
  )
}

/// §2.4's live transposition, driven in the direction that would pass if the
/// table were written in record-declaration order: wire position 6 is *script*
/// witnesses, and field 6's commitment is not field 7's.
test certification_rejects_the_address_preimage_under_field_six() fail {
  certify(6, field_preimage(7))
}

/// And the other direction, so a table that transposed both would not survive
/// by symmetry.
test certification_rejects_the_script_preimage_under_field_seven() fail {
  certify(7, field_preimage(6))
}

/// The binding itself. Same content, same field, same chunks — a transaction id
/// the supplied body does not derive to. §8.6 re-derives the id through the
/// unchanged §3 derivation, so the claim is refused before the carriage is
/// examined.
test certification_rejects_a_certificate_for_another_transaction() fail {
  let (_tx_id, compact_cbor, witness_set_compact_cbor) = nine_field_fixture()
  let preimage = field_preimage(0)
  verify_field_preimage_certificate_v1(
    field_preimage_certificate_v1(
      owner_hash,
      blake2b_256(#"6e6f742074686973207472616e73616374696f6e"),
      0,
      preimage,
    ),
    compact_cbor,
    witness_set_compact_cbor,
    split_field_preimage(preimage),
  )
}

/// A certificate over content the transaction never committed. It is internally
/// consistent — its digests are its chunks', its length is its chunks' — and
/// still fails, because the reconstruction does not hash to the field
/// commitment.
test certification_rejects_other_content() fail {
  let (tx_id, compact_cbor, witness_set_compact_cbor) = nine_field_fixture()
  let other = marked_bytes(#"ff", smallest_tier3_bytes)
  verify_field_preimage_certificate_v1(
    field_preimage_certificate_v1(owner_hash, tx_id, 0, other),
    compact_cbor,
    witness_set_compact_cbor,
    split_field_preimage(other),
  )
}

/// Right content, wrong order. The redeemer supplies chunk order (§8.6), so a
/// swapped vector is something a prover can actually build; the reconstruction
/// hash is what refuses it.
///
/// The transaction here commits *these* bytes in slot 0 rather than the shared
/// fixture's, so the correctly-ordered vector certifies and the swap is the
/// only thing left for the predicate to refuse. Against the shared fixture this
/// would have failed for the uninteresting reason that the content is foreign,
/// and putting the chunks back in the right order would not have changed the
/// answer.
test certification_rejects_a_reordered_chunk_vector() fail {
  let preimage = bytearray.concat(
    marked_bytes(#"05", chunk_bytes_k),
    marked_bytes(#"50", chunk_bytes_k),
  )
  let body = NativeTxBodyCompact {
    ..nine_field_body(),
    spend_inputs_hash: field_commitment(preimage),
  }
  let witness_set_compact_cbor = encode_native_tx_witness_set_compact(
    nine_field_witness_set(),
  )
  expect [chunk_0, chunk_1] = split_field_preimage(preimage)
  verify_field_preimage_certificate_v1(
    FieldPreimageCertificateV1 {
      owner: owner_hash,
      tx_id: tx_id_of(body),
      field_index: 0,
      total_length: 2 * chunk_bytes_k,
      chunk_digests: [blake2b_256(chunk_1), blake2b_256(chunk_0)],
    },
    compact_cbor_of(body, witness_set_compact_cbor),
    witness_set_compact_cbor,
    [chunk_1, chunk_0],
  )
}

/// A digest vector that does not describe the chunks it ships with. The
/// reconstruction still hashes correctly, so nothing but the per-chunk check
/// catches this — and without it §8.6's single-chunk access would authenticate
/// against a digest nobody verified.
test certification_rejects_a_tampered_chunk_digest() fail {
  let (tx_id, compact_cbor, witness_set_compact_cbor) = nine_field_fixture()
  let preimage = field_preimage(0)
  let certificate = field_preimage_certificate_v1(
    owner_hash,
    tx_id,
    0,
    preimage,
  )
  expect [_first, second] = certificate.chunk_digests
  verify_field_preimage_certificate_v1(
    FieldPreimageCertificateV1 {
      ..certificate,
      chunk_digests: [zero_hash32, second],
    },
    compact_cbor,
    witness_set_compact_cbor,
    split_field_preimage(preimage),
  )
}

/// `total_length` is mint-verified data that a consumer derives item counts and
/// offsets from (§8.6), so it is checked against the bytes rather than taken.
test certification_rejects_a_wrong_total_length() fail {
  let (tx_id, compact_cbor, witness_set_compact_cbor) = nine_field_fixture()
  let preimage = field_preimage(0)
  let certificate = field_preimage_certificate_v1(
    owner_hash,
    tx_id,
    0,
    preimage,
  )
  verify_field_preimage_certificate_v1(
    FieldPreimageCertificateV1 {
      ..certificate,
      total_length: smallest_tier3_bytes + 1,
    },
    compact_cbor,
    witness_set_compact_cbor,
    split_field_preimage(preimage),
  )
}

/// The §8.4 partition. A preimage that fits tier 2 has exactly one admissible
/// carriage, so no certificate may exist for it — otherwise every structural
/// check tiers 1–2 run at view construction is side-steppable by re-carrying
/// the same bytes under tier 3.
///
/// Isolating, and that is what makes it a test of the tier boundary rather than
/// of anything else: the transaction genuinely commits these `K` bytes in slot
/// 0, the digest vector is theirs, the one-chunk shape is the split rule's at
/// this length, and `expected_chunk_count(K)` is 1. Every conjunct in
/// `verify_field_preimage_certificate_v1` holds except `total_length > K`, so
/// removing that guard is what turns this vector green.
test certification_rejects_a_preimage_that_fits_tier_two() fail {
  let preimage = marked_bytes(#"06", chunk_bytes_k)
  let body = NativeTxBodyCompact {
    ..nine_field_body(),
    spend_inputs_hash: field_commitment(preimage),
  }
  let witness_set_compact_cbor = encode_native_tx_witness_set_compact(
    nine_field_witness_set(),
  )
  verify_field_preimage_certificate_v1(
    FieldPreimageCertificateV1 {
      owner: owner_hash,
      tx_id: tx_id_of(body),
      field_index: 0,
      total_length: chunk_bytes_k,
      chunk_digests: [blake2b_256(preimage)],
    },
    compact_cbor_of(body, witness_set_compact_cbor),
    witness_set_compact_cbor,
    [preimage],
  )
}

/// And the producer twin refuses to build such a certificate, so the tier
/// boundary is held at both ends of §8.6 rather than only where certificates
/// are consumed. Its own `total_length > K` guard is the only thing that can
/// refuse this call: the owner is a well-formed key hash and the
/// `(tx_id, field_index)` pair is in range.
test producer_refuses_a_preimage_that_fits_tier_two() fail {
  let (tx_id, _compact_cbor, _witness_set_compact_cbor) = nine_field_fixture()
  field_preimage_certificate_v1(
    owner_hash,
    tx_id,
    0,
    marked_bytes(#"06", chunk_bytes_k),
  ).total_length == chunk_bytes_k
}

/// A partition that is not the split rule's. Everything else about this
/// certificate is honest — the chunks reconstruct the committed preimage, the
/// digests are theirs, the length is right — but the boundary is in the wrong
/// place, so a consumer's offset arithmetic over the digest vector would land
/// in the wrong chunk.
test certification_rejects_a_lopsided_split() fail {
  let (tx_id, compact_cbor, witness_set_compact_cbor) = nine_field_fixture()
  let preimage = field_preimage(0)
  let head = bytearray.take(preimage, chunk_bytes_k - 100)
  let tail = bytearray.drop(preimage, chunk_bytes_k - 100)
  verify_field_preimage_certificate_v1(
    FieldPreimageCertificateV1 {
      owner: owner_hash,
      tx_id,
      field_index: 0,
      total_length: smallest_tier3_bytes,
      chunk_digests: [blake2b_256(head), blake2b_256(tail)],
    },
    compact_cbor,
    witness_set_compact_cbor,
    [head, tail],
  )
}

/// A certificate built at the top of the ladder, from the honest producer path:
/// the §5.4 aggregate cap exactly, three chunks, `max_tier3_chunk_count`
/// reached. This is the positive control for the rejection below — the two
/// differ in one byte of preimage length and nothing else — and it is also the
/// only place certification is driven at the three-chunk end, where
/// `chunk_shape_matches` recurses twice before it reaches the ragged tail.
test certification_accepts_carriage_at_the_aggregate_cap() {
  let preimage = marked_bytes(#"07", max_transaction_aggregate_field_bytes)
  let body = NativeTxBodyCompact {
    ..nine_field_body(),
    spend_inputs_hash: field_commitment(preimage),
  }
  let witness_set_compact_cbor = encode_native_tx_witness_set_compact(
    nine_field_witness_set(),
  )
  and {
    list.length(split_field_preimage(preimage)) == max_tier3_chunk_count,
    certify_against(
      (
        tx_id_of(body),
        compact_cbor_of(body, witness_set_compact_cbor),
        witness_set_compact_cbor,
      ),
      0,
      preimage,
    ),
  }
}

/// The §5.4 aggregate cap at the certification end of the ladder. The splitter
/// refuses to produce carriage above the cap
/// (`split_refuses_a_preimage_above_the_aggregate_cap`), so this drives the
/// independent bound the certifier keeps: carriage hand-built one byte over the
/// cap, in the correct §8.4 shape, with the correct digests, against a
/// transaction that genuinely committed those bytes. Everything the certificate
/// says about its content is true and it is still refused — which is what makes
/// the cap a property of the format rather than of the splitter's own
/// arithmetic. Without it §8.3's three-chunk maximum would be the only thing
/// keeping a fourth chunk out, and that is a count bound, not a length one.
test certification_rejects_a_total_length_above_the_aggregate_cap() fail {
  let oversized = marked_bytes(#"07", max_transaction_aggregate_field_bytes + 1)
  let chunks = [
    oversized |> bytearray.take(chunk_bytes_k),
    oversized |> bytearray.drop(chunk_bytes_k) |> bytearray.take(chunk_bytes_k),
    oversized |> bytearray.drop(2 * chunk_bytes_k),
  ]
  // A body that actually commits the oversized preimage in slot 0, so the
  // reconstruction check would pass if the cap check let it get that far.
  let body = NativeTxBodyCompact {
    ..nine_field_body(),
    spend_inputs_hash: field_commitment(oversized),
  }
  let witness_set_compact_cbor = encode_native_tx_witness_set_compact(
    nine_field_witness_set(),
  )
  verify_field_preimage_certificate_v1(
    FieldPreimageCertificateV1 {
      owner: owner_hash,
      tx_id: tx_id_of(body),
      field_index: 0,
      total_length: max_transaction_aggregate_field_bytes + 1,
      chunk_digests: list.map(chunks, blake2b_256),
    },
    compact_cbor_of(body, witness_set_compact_cbor),
    witness_set_compact_cbor,
    chunks,
  )
}

/// The `owner` is the one certificate field nothing proves — it is the minter's
/// own choice of min-Ada reclaim authority (§8.6). What is checked is that it
/// could be one: a payment key hash is 28 bytes, and anything else names an
/// authority no signature can ever satisfy, so the deposit would be locked
/// forever behind a certificate that otherwise verifies.
test certification_rejects_an_owner_that_is_not_a_key_hash() fail {
  let (tx_id, compact_cbor, witness_set_compact_cbor) = nine_field_fixture()
  let preimage = field_preimage(0)
  let certificate = field_preimage_certificate_v1(
    owner_hash,
    tx_id,
    0,
    preimage,
  )
  verify_field_preimage_certificate_v1(
    FieldPreimageCertificateV1 {
      ..certificate,
      owner: bytearray.take(owner_hash, 27),
    },
    compact_cbor,
    witness_set_compact_cbor,
    split_field_preimage(preimage),
  )
}

/// And the producer twin refuses to build one, so the two ends of §8.6 agree on
/// what a certificate may say rather than only the consuming end holding the
/// line.
test producer_refuses_an_owner_that_is_not_a_key_hash() fail {
  let (tx_id, _compact_cbor, _witness_set_compact_cbor) = nine_field_fixture()
  field_preimage_certificate_v1(
    bytearray.take(owner_hash, 27),
    tx_id,
    0,
    field_preimage(0),
  ).total_length == smallest_tier3_bytes
}

/// The witness-set fields are only readable once the supplied witness set is
/// shown to be the one the tx-id committed (§4). A substituted set fails even
/// though it is a well-formed witness-set encoding carrying the right field
/// hash in the right slot.
test certification_rejects_a_substituted_witness_set() fail {
  let (tx_id, compact_cbor, _witness_set_compact_cbor) = nine_field_fixture()
  let preimage = field_preimage(6)
  verify_field_preimage_certificate_v1(
    field_preimage_certificate_v1(owner_hash, tx_id, 6, preimage),
    compact_cbor,
    encode_native_tx_witness_set_compact(
      NativeTxWitnessSetCompact {
        addr_tx_wits_hash: zero_hash32,
        script_tx_wits_hash: field_commitment(preimage),
        redeemer_tx_wits_hash: zero_hash32,
      },
    ),
    split_field_preimage(preimage),
  )
}

// ---------------------------------------------------------------------------
// §8.7 healing
// ---------------------------------------------------------------------------

/// Republication as an unrelated party performs it: from the preimage bytes,
/// with no access to the original publisher's chunks, certificate, or UTxOs.
/// Written as take/drop arithmetic so it is a second derivation and not a
/// second call to the first one.
fn republished_chunks(preimage: ByteArray) -> List<ByteArray> {
  [
    preimage |> bytearray.take(chunk_bytes_k),
    preimage |> bytearray.drop(chunk_bytes_k),
  ]
}

/// Content addressing, stated as the property that makes healing work: an
/// unrelated party who obtains the same preimage derives the same chunks and
/// the same digest vector, so the certificate they build differs from the
/// original in nothing but the min-Ada reclaim authority — and the original
/// certificate still verifies against the republished bytes. A mid-game yank of
/// the raw chunks is therefore fees and delay, not a lost dispute.
test republished_carriage_is_byte_identical_and_certifies() {
  let (tx_id, compact_cbor, witness_set_compact_cbor) = nine_field_fixture()
  let preimage = field_preimage(2)
  let original = field_preimage_certificate_v1(owner_hash, tx_id, 2, preimage)
  let healed = field_preimage_certificate_v1(healer_hash, tx_id, 2, preimage)
  and {
    original.chunk_digests == healed.chunk_digests,
    original.total_length == healed.total_length,
    // Same content address, so the same asset name: a healed certificate is
    // discoverable exactly where the yanked one was.
    field_preimage_certificate_asset_name(original.tx_id, original.field_index) == field_preimage_certificate_asset_name(
      healed.tx_id,
      healed.field_index,
    ),
    verify_field_preimage_certificate_v1(
      original,
      compact_cbor,
      witness_set_compact_cbor,
      republished_chunks(preimage),
    ),
    verify_field_preimage_certificate_v1(
      healed,
      compact_cbor,
      witness_set_compact_cbor,
      republished_chunks(preimage),
    ),
  }
}

// ---------------------------------------------------------------------------
// Producer / consumer agreement
// ---------------------------------------------------------------------------

fn key_output(datum: Data) -> Output {
  Output {
    address: address.from_verification_key(owner_hash),
    value: assets.from_lovelace(2000000),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn input_of(index: Int, output: Output) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: zero_hash32,
      output_index: index,
    },
    output,
  }
}

/// The end of the ladder: carriage produced here, consumed through the door in
/// `native_tx_field_access_v1`, with nothing in between. This is the assertion
/// that the split rule and the consumer's chunk arithmetic are the same rule —
/// item 397 crosses the `K` boundary, so reading it stitches bytes out of both
/// chunks and would return the wrong 38 bytes if the boundary were off by one.
test certified_carriage_opens_through_the_access_door() {
  let preimage = straddle_preimage()
  expect [chunk_0, chunk_1] = split_field_preimage(preimage)
  let witness_set = NativeTxWitnessSetCompact {
    addr_tx_wits_hash: zero_hash32,
    script_tx_wits_hash: zero_hash32,
    redeemer_tx_wits_hash: zero_hash32,
  }
  let body = NativeTxBodyCompact {
    spend_inputs_hash: zero_hash32,
    reference_inputs_hash: field_commitment(preimage),
    outputs_hash: zero_hash32,
    fee: 0,
    validity_interval_start: 0,
    validity_interval_end: 0,
    required_observers_hash: zero_hash32,
    required_signers_hash: zero_hash32,
    mint_hash: zero_hash32,
    script_integrity_hash: zero_hash32,
    auxiliary_data_hash: zero_hash32,
    network_id: 255,
  }
  let tx_id = tx_id_of(body)
  let certificate_datum: Data = field_preimage_certificate_v1(
    owner_hash,
    tx_id,
    1,
    preimage,
  )
  let reference_inputs = [
    input_of(
      0,
      Output {
        address: address.from_script(certificate_policy_id),
        value: assets.from_lovelace(2000000)
          |> assets.add(
              certificate_policy_id,
              field_preimage_certificate_asset_name(tx_id, 1),
              1,
            ),
        datum: InlineDatum(certificate_datum),
        reference_script: None,
      },
    ),
    input_of(1, key_output(builtin.b_data(chunk_0))),
    input_of(2, key_output(builtin.b_data(chunk_1))),
  ]
  let view = authenticated_field_view(
    VerifiedMidgardNativeTxCompact {
      tx_id,
      version: native_tx_version_v1,
      tx_compact: NativeTxCompact {
        body,
        witness_set_hash: blake2b_256(
          encode_native_tx_witness_set_compact(witness_set),
        ),
        validity_code: 0,
      },
    },
    witness_set,
    1,
    Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] },
    reference_inputs,
    certificate_policy_id,
  )
  and {
    field_item_count(view) == 400,
    field_item_extent(view, 397) == Pair(15885, 38),
    field_item_at(view, 397) == #"825820f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1190007",
  }
}

// ---------------------------------------------------------------------------
// Phase-4 exit measurement — the three-chunk corner (spec §8.10, rows 1–6)
// ---------------------------------------------------------------------------
//
// #556's dispute bench established the two-chunk reconstruction; the corner
// this lane owes is the *three*-chunk one, opened through the real door with
// the four reference inputs a consuming step would actually carry (one
// certificate, three chunks). The rows below come in a controlled family: every
// one of them builds the same fixture and opens the same door, and they differ
// only in which items are read, so a difference between two rows is one read
// and nothing else. Absolute figures are dominated by fixture construction and
// are not budget verdicts — spec §8.10 says which differences are the
// measurement.

/// Nine of the straddle vector's items — 360 bytes, item-aligned at the
/// stride fields 0 and 1 share. Doubling a nine-item block keeps every
/// power-of-two multiple item-aligned, which is what lets the corner fixture be
/// built in seven concatenations instead of eight hundred.
fn nine_item_block() -> ByteArray {
  bytearray.take(straddle_block, 9 * spend_input_stride)
}

/// The largest field-1 preimage that fits the §5.4 aggregate cap: 819 items
/// behind the §5.1 `99 0333` header, 32,763 bytes, five short of 32,768.
///
/// It is the three-chunk corner because 819 items is the first item count whose
/// preimage exceeds `2 · K` and the last that does not exceed the cap: the
/// split is `[15,900, 15,900, 963]`, both chunk boundaries fall inside items,
/// and the ragged tail is the smallest of the three. A fixed-stride field is
/// used on purpose — §8.6 refuses a variable-width field's tier-3 count, so a
/// variable-width corner could not be opened and read at all.
///
/// The certification side has its own corner, `mint_corner_preimage` in
/// `validators/field-preimage-certificate-handlers.test.ak`: the same length,
/// but opaque filler, because certification never looks inside an item. The
/// two are named apart so each is greppable to the claim it serves.
fn door_corner_preimage() -> ByteArray {
  let items = bytearray.take(grow(nine_item_block(), 32760), 32760)
  bytearray.concat(#"990333", items)
}

/// Item `index`'s 38 payload bytes, derived from the nine-item block rather
/// than from the preimage the door reads — a second derivation, so a read that
/// lands one item off is a failure and not a coincidence.
fn corner_item(index: Int) -> ByteArray {
  let block_index = index % 9
  bytearray.slice(
    nine_item_block(),
    block_index * spend_input_stride + 2,
    block_index * spend_input_stride + 39,
  )
}

/// The corner's four reference inputs, in the order a step's redeemer names
/// them: the certificate at 0, then chunks 0..2 at 1..3.
fn corner_reference_inputs(tx_id: ByteArray) -> List<Input> {
  expect [chunk_0,
        chunk_1, chunk_2] = split_field_preimage(door_corner_preimage())
  let certificate_datum: Data = field_preimage_certificate_v1(
    owner_hash,
    tx_id,
    1,
    door_corner_preimage(),
  )
  [
    input_of(
      0,
      Output {
        address: address.from_script(certificate_policy_id),
        value: assets.from_lovelace(2000000)
          |> assets.add(
              certificate_policy_id,
              field_preimage_certificate_asset_name(tx_id, 1),
              1,
            ),
        datum: InlineDatum(certificate_datum),
        reference_script: None,
      },
    ),
    input_of(1, key_output(builtin.b_data(chunk_0))),
    input_of(2, key_output(builtin.b_data(chunk_1))),
    input_of(3, key_output(builtin.b_data(chunk_2))),
  ]
}

fn door_corner_body() -> NativeTxBodyCompact {
  NativeTxBodyCompact {
    spend_inputs_hash: zero_hash32,
    reference_inputs_hash: field_commitment(door_corner_preimage()),
    outputs_hash: zero_hash32,
    fee: 0,
    validity_interval_start: 0,
    validity_interval_end: 0,
    required_observers_hash: zero_hash32,
    required_signers_hash: zero_hash32,
    mint_hash: zero_hash32,
    script_integrity_hash: zero_hash32,
    auxiliary_data_hash: zero_hash32,
    network_id: 255,
  }
}

/// Everything a consuming step holds *before* it opens the door: the
/// authenticated compact transaction, its witness set, and the four reference
/// inputs. Factored out so the control row below can build exactly this and
/// stop, which is what makes the door open itself an attributable difference
/// rather than a figure dominated by a 32 KB fixture.
fn corner_step_inputs() -> (
  VerifiedMidgardNativeTxCompact,
  NativeTxWitnessSetCompact,
  List<Input>,
) {
  let witness_set = NativeTxWitnessSetCompact {
    addr_tx_wits_hash: zero_hash32,
    script_tx_wits_hash: zero_hash32,
    redeemer_tx_wits_hash: zero_hash32,
  }
  let body = door_corner_body()
  let tx_id = tx_id_of(body)
  (
    VerifiedMidgardNativeTxCompact {
      tx_id,
      version: native_tx_version_v1,
      tx_compact: NativeTxCompact {
        body,
        witness_set_hash: blake2b_256(
          encode_native_tx_witness_set_compact(witness_set),
        ),
        validity_code: 0,
      },
    },
    witness_set,
    corner_reference_inputs(tx_id),
  )
}

/// Opens the corner through `authenticated_field_view` exactly as a step would:
/// tier-3 carriage, positional reference-input indices, the certificate policy
/// id arriving as a parameter (§8.6's wiring constraint).
fn corner_view() -> FieldViewV1 {
  let (verified, witness_set, reference_inputs) = corner_step_inputs()
  authenticated_field_view(
    verified,
    witness_set,
    1,
    Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2, 3] },
    reference_inputs,
    certificate_policy_id,
  )
}

/// Row 0 — the control. Builds the whole corner fixture the way row 1 does —
/// the 32,763-byte preimage, the §8.4 split, the certificate (three
/// `blake2b_256` over the preimage), the four reference inputs, the committed
/// body and its §3 tx-id — and then stops without opening the door. Row 1
/// against this row is the door open proper; every absolute figure in the table
/// is otherwise dominated by this fixture, exactly as #556's bench found and
/// spec §12.5 restates.
test tier3_corner_fixture_only() {
  let (verified, witness_set, reference_inputs) = corner_step_inputs()
  and {
    verified.version == native_tx_version_v1,
    witness_set.addr_tx_wits_hash == zero_hash32,
    list.length(reference_inputs) == max_tier3_chunk_count + 1,
  }
}

/// The fixture is the corner it is quoted as, asserted rather than described:
/// three chunks, both boundaries interior to an item, five bytes of headroom
/// under the §5.4 cap. Spec §8.10's rows are quoted at these numbers, so this
/// is what keeps them quoted where they were taken.
test tier3_corner_fixture_sits_at_the_three_chunk_corner() {
  let preimage = door_corner_preimage()
  let total_length = bytearray.length(preimage)
  and {
    total_length == 32763,
    total_length > 2 * chunk_bytes_k,
    total_length + 5 == max_transaction_aggregate_field_bytes,
    total_length + spend_input_stride > max_transaction_aggregate_field_bytes,
    list.map(split_field_preimage(preimage), bytearray.length) == [
      chunk_bytes_k, chunk_bytes_k, 963,
    ],
    expected_chunk_count(total_length) == max_tier3_chunk_count,
    field_item_count(corner_view()) == 819,
    // Both boundaries fall *inside* an item, which is what makes rows 4 and 5
    // straddling reads rather than aligned ones: item 397's payload spans
    // 15,885..15,923 across chunk 0's end at 15,900, and item 794's spans
    // 31,765..31,803 across chunk 1's end at 31,800. Item 818 is the last, and
    // it lies wholly inside the 963-byte ragged tail.
    field_item_extent(corner_view(), 397) == Pair(15885, 38),
    field_item_extent(corner_view(), 794) == Pair(31765, 38),
    field_item_extent(corner_view(), 818) == Pair(32725, 38),
  }
}

/// Row 1 — the door alone. Four reference inputs resolved, the certificate's
/// token and `(tx_id, field_index)` matched, the split shape checked, the count
/// derived arithmetically from the mint-verified `total_length`. No chunk is
/// hashed: §8.6's "no chunk hash spent" for a fixed-stride count is what this
/// row is the control for.
test tier3_corner_open_only() {
  field_item_count(corner_view()) == 819
}

/// Row 2 — one item read wholly inside chunk 0.
test tier3_corner_one_read() {
  let view = corner_view()
  and {
    field_item_count(view) == 819,
    field_item_at(view, 0) == corner_item(0),
  }
}

/// Row 3 — two item reads, both wholly inside chunk 0. Row 3 against row 2 is
/// one repeated in-chunk read, which is what makes tier 3's per-read
/// re-verification visible as a number.
test tier3_corner_two_reads() {
  let view = corner_view()
  and {
    field_item_count(view) == 819,
    field_item_at(view, 0) == corner_item(0),
    field_item_at(view, 1) == corner_item(1),
  }
}

/// Row 4 — the first boundary. Item 397's payload begins at 15,885 and ends at
/// 15,923, so it is stitched out of chunk 0 and chunk 1.
test tier3_corner_straddling_read() {
  let view = corner_view()
  and {
    field_item_count(view) == 819,
    field_item_at(view, 397) == corner_item(397),
  }
}

/// Row 5 — the second boundary, the one a two-chunk fixture cannot reach. Item
/// 794's payload begins at 31,765 and crosses 31,800 into the ragged tail.
test tier3_corner_second_straddling_read() {
  let view = corner_view()
  and {
    field_item_count(view) == 819,
    field_item_at(view, 794) == corner_item(794),
  }
}

/// Row 6 — the last item, wholly inside the 963-byte ragged tail. Against row 2
/// this is the same read against a chunk sixteen times smaller, which is what
/// shows the per-read cost is the chunk's size and not the preimage's.
test tier3_corner_last_chunk_read() {
  let view = corner_view()
  and {
    field_item_count(view) == 819,
    field_item_at(view, 818) == corner_item(818),
  }
}

/// The corner rows are not vacuous. Rows 2–6 measure reads that returned the
/// right bytes; this is the same fixture with one byte of the **ragged tail**
/// changed, and the read is refused because `read_chunked_range` verifies the
/// chunk it lands in against the certificate's mint-verified digest vector. It
/// neutralises row 6 specifically — the tail is the chunk only row 6 touches —
/// so a digest vector that stopped being consulted for the last chunk would
/// turn this red and nothing else.
test tier3_corner_refuses_a_tampered_tail_chunk() fail {
  let (verified, witness_set, reference_inputs) = corner_step_inputs()
  expect [chunk_0,
        chunk_1, chunk_2] = split_field_preimage(door_corner_preimage())
  let tampered_tail = bytearray.concat(
    bytearray.take(chunk_2, bytearray.length(chunk_2) - 1),
    #"ff",
  )
  expect bytearray.length(tampered_tail) == bytearray.length(chunk_2)
  expect tampered_tail != chunk_2
  expect [certificate_input, ..] = reference_inputs
  let view = authenticated_field_view(
    verified,
    witness_set,
    1,
    Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2, 3] },
    [
      certificate_input,
      input_of(1, key_output(builtin.b_data(chunk_0))),
      input_of(2, key_output(builtin.b_data(chunk_1))),
      input_of(3, key_output(builtin.b_data(tampered_tail))),
    ],
    certificate_policy_id,
  )
  field_item_at(view, 818) == corner_item(818)
}
