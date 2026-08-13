//// Seam suite for the `canonical-decodability` rule — `docs/spec/midgard-tx.md`
//// §12.7.
////
//// Three things are pinned here and nowhere else.
////
//// 1. **The verdict table.** One vector per way §5.1 can be left, each asserted
////    against its own code rather than against "not grammatical", so a fixture
////    that stops exercising the rule it was written for goes red instead of
////    quietly passing on a different refusal.
//// 2. **Agreement with the door, in both directions.** For every vector the
////    table calls ungrammatical, the §8.8 whole-materialising door *aborts* on
////    the same bytes; for every vector it calls grammatical, the door opens
////    them. This is the property the whole family rests on: the verdict is the
////    door's grammar restated as a decision, and if the two ever disagreed the
////    family would either convict a field the machine can read or fail to
////    convict one it cannot.
//// 3. **The cost claim** §12.7 publishes, as the rows
////    `scripts/canonical-decodability-exec-ledger-v1.json` pins.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/canonical_decodability/rule.{
  BodyFieldClaim, CommittedFieldVerdictV1, committed_field_verdict_v1,
  envelope_verdict_v1, miscounted_field_preimage_v1, verdict_code_count,
  verdict_grammatical, verdict_missing_array_header, verdict_missing_item_header,
  verdict_non_minimal_array_header, verdict_non_minimal_item_header,
  verdict_not_an_array_header, verdict_not_an_item_header,
  verdict_trailing_bytes, verdict_truncated_array_header,
  verdict_truncated_item_header, verdict_truncated_item_payload,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_compact_cbor_v1}
use midgard/native_tx_field_access_v1.{
  Inline, authenticated_whole_field_view, chunk_bytes_k, encode_field_preimage,
  expected_chunk_count, field_item_count, max_tier3_chunk_count,
  max_transaction_aggregate_field_bytes,
}

/// §2.5 slot 2 (outputs). Variable-width, so `whole_view` settles §5.1 by
/// walking every item rather than by stride arithmetic — which is the grammar
/// this rule restates. A fixed-stride slot would exercise a different check.
const outputs_field_index: Int = 2

// ---------------------------------------------------------------------------
// The verdict table
// ---------------------------------------------------------------------------

/// §5.1's shortest admissible preimage, and the format's empty field.
const empty_field_preimage: ByteArray = #"80"

/// One byte short of it. `#""` has no array header at all.
const no_bytes: ByteArray = #""

/// `9a` is a well-formed four-byte CBOR array head and outside §5.1's
/// acceptance set, which caps at `99 NNNN`.
const four_byte_array_head: ByteArray = #"9a00000001"

/// `98 05` spells a count the one-byte form spells in one byte.
const non_minimal_array_head: ByteArray = #"9805"

/// A `98` head with no count byte after it.
const truncated_array_head: ByteArray = #"98"

/// `81` declaring one item, with nothing left to start one.
const item_header_absent: ByteArray = #"81"

/// `81` followed by `00`, a CBOR uint where §5.1 requires a byte-string head.
const item_header_wrong_major: ByteArray = #"8100"

/// `81 58 05` — a two-byte item head for a length the one-byte form covers.
const non_minimal_item_head: ByteArray = #"815805"

/// `81 58` — a two-byte item head with its length byte missing.
const truncated_item_head: ByteArray = #"8158"

/// `81 42 ff` — an item declaring two payload bytes with one left.
const truncated_item_payload: ByteArray = #"8142ff"

/// A well-formed empty envelope with a byte after it.
const trailing_byte: ByteArray = #"8041"

/// Every vector above, paired with the verdict it must earn.
fn verdict_table() -> List<Pair<ByteArray, Int>> {
  [
    Pair(empty_field_preimage, verdict_grammatical),
    Pair(encode_field_preimage([#"deadbeef"]), verdict_grammatical),
    Pair(no_bytes, verdict_missing_array_header),
    Pair(four_byte_array_head, verdict_not_an_array_header),
    Pair(non_minimal_array_head, verdict_non_minimal_array_header),
    Pair(truncated_array_head, verdict_truncated_array_header),
    Pair(item_header_absent, verdict_missing_item_header),
    Pair(item_header_wrong_major, verdict_not_an_item_header),
    Pair(non_minimal_item_head, verdict_non_minimal_item_header),
    Pair(truncated_item_head, verdict_truncated_item_header),
    Pair(truncated_item_payload, verdict_truncated_item_payload),
    Pair(trailing_byte, verdict_trailing_bytes),
  ]
}

/// Every row of the table earns exactly the verdict it is written for.
test canonical_decodability_verdict_table_is_exact() {
  list.all(
    verdict_table(),
    fn(row) {
      let Pair(preimage, verdict) = row
      envelope_verdict_v1(preimage) == verdict
    },
  )
}

/// Every code the table asserts is distinct, and the table covers the whole
/// code space. Without this the table could assert eleven rows against three
/// codes and still be "exact".
test canonical_decodability_verdict_table_covers_every_code() {
  let codes = verdict_table()
    |> list.map(fn(row) { row.2nd })
    |> list.unique
  and {
    list.length(codes) == verdict_code_count,
    list.all(
      codes,
      fn(code) {
        and {
          code >= 0,
          code < verdict_code_count,
        }
      },
    ),
  }
}

/// §5.1's boundary between the header widths, in both directions and at both
/// the array head and the item head. 23/24 is the one-byte/two-byte boundary
/// and 255/256 the two-byte/three-byte one; the minimal form of each count is
/// grammatical and the wider spelling of the same count is not.
test canonical_decodability_verdict_pins_the_minimal_width_boundaries() {
  and {
    // Array head: 23 items in one byte, 24 in two, 256 in three.
    envelope_verdict_v1(miscounted_field_preimage_v1(23, [])) == verdict_missing_item_header,
    envelope_verdict_v1(#"9818") == verdict_missing_item_header,
    envelope_verdict_v1(#"9817") == verdict_non_minimal_array_header,
    envelope_verdict_v1(#"990100") == verdict_missing_item_header,
    envelope_verdict_v1(#"9900ff") == verdict_non_minimal_array_header,
    // Item head: a 24-byte item needs `58`, a 23-byte item may not use it.
    envelope_verdict_v1(#"815818") == verdict_truncated_item_payload,
    envelope_verdict_v1(#"815817") == verdict_non_minimal_item_header,
    envelope_verdict_v1(#"81590100") == verdict_truncated_item_payload,
    envelope_verdict_v1(#"815900ff") == verdict_non_minimal_item_header,
  }
}

/// The declared-versus-actual mismatch in both directions, built through the
/// one producer that can express it.
test canonical_decodability_verdict_pins_declared_count_mismatches() {
  and {
    // Fewer items than declared: the walk runs out of preimage.
    envelope_verdict_v1(miscounted_field_preimage_v1(2, [#"deadbeef"])) == verdict_missing_item_header,
    // More items than declared: the preimage runs out of walk.
    envelope_verdict_v1(miscounted_field_preimage_v1(1, [#"deadbeef", #"beef"])) == verdict_trailing_bytes,
    // Exactly as declared: grammatical, and byte-identical to the honest
    // producer's output, so the deliberately-wrong builder is only wrong when
    // it is asked to be.
    miscounted_field_preimage_v1(2, [#"deadbeef", #"beef"]) == encode_field_preimage(
      [#"deadbeef", #"beef"],
    ),
    envelope_verdict_v1(miscounted_field_preimage_v1(2, [#"deadbeef", #"beef"])) == verdict_grammatical,
  }
}

/// A grammatical envelope stays grammatical at every item cardinality the
/// three header widths cover, so the walk's own recursion is exercised past the
/// one- and two-item shapes the table uses.
test canonical_decodability_verdict_holds_across_the_header_widths() {
  list.all(
    [0, 1, 23, 24, 255, 256],
    fn(count) {
      envelope_verdict_v1(
        encode_field_preimage(list.repeat(#"deadbeef", count)),
      ) == verdict_grammatical
    },
  )
}

// ---------------------------------------------------------------------------
// Agreement with the §8.8 door
// ---------------------------------------------------------------------------

fn committed_block(preimage: ByteArray) -> fixture.NativeBlockFixtureV1 {
  fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    outputs_field_index,
    preimage,
  )
}

/// Open the accused field through the real §8.8 whole-materialising door. This
/// is the code path §12.7 leaves exactly as it is, so agreement is asserted
/// against it rather than against a re-statement of it.
fn door_item_count(preimage: ByteArray) -> Int {
  door_item_count_at(outputs_field_index, preimage)
}

fn door_item_count_at(field_index: Int, preimage: ByteArray) -> Int {
  let block = fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    field_index,
    preimage,
  )
  field_item_count(
    authenticated_whole_field_view(
      verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
      block.witness_set_compact,
      field_index,
      Inline { preimage },
      [],
      fixture.test_certificate_policy_id,
    ),
  )
}

/// Forward agreement: every vector the verdict calls grammatical, the door
/// opens — and opens to the item count the envelope declares.
test canonical_decodability_door_opens_every_grammatical_vector() {
  and {
    door_item_count(empty_field_preimage) == 0,
    door_item_count(encode_field_preimage([#"deadbeef"])) == 1,
    door_item_count(encode_field_preimage(list.repeat(#"deadbeef", 24))) == 24,
  }
}

// Reverse agreement. One `fail` selector per ungrammatical vector, because a
// single fold over the table would prove only that *some* vector aborts: an
// abort ends the test, so the vectors after it are never reached.

test canonical_decodability_door_refuses_no_bytes() fail {
  door_item_count(no_bytes) >= 0
}

test canonical_decodability_door_refuses_a_four_byte_array_head() fail {
  door_item_count(four_byte_array_head) >= 0
}

test canonical_decodability_door_refuses_a_non_minimal_array_head() fail {
  door_item_count(non_minimal_array_head) >= 0
}

test canonical_decodability_door_refuses_a_truncated_array_head() fail {
  door_item_count(truncated_array_head) >= 0
}

test canonical_decodability_door_refuses_an_absent_item_header() fail {
  door_item_count(item_header_absent) >= 0
}

test canonical_decodability_door_refuses_a_wrong_major_item_header() fail {
  door_item_count(item_header_wrong_major) >= 0
}

test canonical_decodability_door_refuses_a_non_minimal_item_header() fail {
  door_item_count(non_minimal_item_head) >= 0
}

test canonical_decodability_door_refuses_a_truncated_item_header() fail {
  door_item_count(truncated_item_head) >= 0
}

test canonical_decodability_door_refuses_a_truncated_item_payload() fail {
  door_item_count(truncated_item_payload) >= 0
}

test canonical_decodability_door_refuses_a_trailing_byte() fail {
  door_item_count(trailing_byte) >= 0
}

// ---------------------------------------------------------------------------
// The residue §12.7 names, measured rather than described
// ---------------------------------------------------------------------------
//
// §5.1's grammar is not the whole of what `whole_view` refuses, and this fault
// kind is §5.1's grammar. The pair below is the exact shape §12.7's residue
// paragraph names: a committed preimage the verdict calls grammatical and the
// door still aborts on, because the refusal is §7.4's fixed-stride arithmetic
// rather than the envelope. It is pinned here so that the residue moves in this
// file when it is closed, rather than being rediscovered by whoever next reads
// the door.

/// §2.5 slot 0 (spend inputs) — fixed-stride, so the door settles §5.1's count
/// by arithmetic against `spend_input_stride` instead of walking.
const spend_inputs_field_index: Int = 0

/// A perfectly well-formed §5.1 envelope whose one item is four bytes rather
/// than field 0's 38.
fn wrong_stride_envelope() -> ByteArray {
  encode_field_preimage([#"deadbeef"])
}

/// The verdict is blind to the stride, and must be: it is a function of the
/// bytes alone, and a stride is a function of the slot.
test canonical_decodability_verdict_is_blind_to_the_fixed_stride_check() {
  envelope_verdict_v1(wrong_stride_envelope()) == verdict_grammatical
}

/// The same bytes, committed at a fixed-stride slot, still abort the door — the
/// stall this fault kind does not reach, and §12.7 says so rather than implying
/// otherwise.
test canonical_decodability_door_still_refuses_a_wrong_stride_field() fail {
  door_item_count_at(spend_inputs_field_index, wrong_stride_envelope()) >= 0
}

/// The same bytes at a *variable-width* slot open cleanly, which is what makes
/// the selector above a measurement of the stride check and not of the fixture.
test canonical_decodability_door_opens_the_same_bytes_at_a_walked_field() {
  door_item_count_at(outputs_field_index, wrong_stride_envelope()) == 1
}

// ---------------------------------------------------------------------------
// Cost claim — the rows `scripts/canonical-decodability-exec-ledger-v1.json`
// pins (§12.7)
// ---------------------------------------------------------------------------

/// §5.4's per-field byte bound, which is also `whole_view`'s per-opening bound
/// and `certified_chunks`' cap on a tier-3 `total_length`. Above it no carriage
/// this family accepts can reach.
const field_byte_bound: Int = max_transaction_aggregate_field_bytes

/// The two item cardinalities the per-item term is priced at. Both are carried
/// inline: `3 + 2·count` stays far under §8.3's `K`, so the difference between
/// the two rows is the walk and nothing else.
const lower_item_cardinality: Int = 250

const upper_item_cardinality: Int = 500

/// A §5.1 envelope over `count` minimum-width items with one byte appended.
/// Ungrammatical by trailing content, which is the only refusal that costs the
/// **whole** walk before it fires — every other one stops at the byte that
/// causes it, so this is the shape a per-item cost claim has to be taken at.
fn minimum_width_trailing_preimage(count: Int) -> ByteArray {
  bytearray.concat(encode_field_preimage(list.repeat(#"00", count)), #"41")
}

fn inline_verdict(preimage: ByteArray) -> Int {
  let block = committed_block(preimage)
  let CommittedFieldVerdictV1 { verdict, .. } = committed_field_verdict_v1(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
    BodyFieldClaim {
      field_index: outputs_field_index,
      carriage: Inline { preimage },
    },
    [],
    fixture.test_certificate_policy_id,
  )
  verdict
}

fn certified_verdict(preimage: ByteArray) -> Int {
  let block = committed_block(preimage)
  let CommittedFieldVerdictV1 { verdict, .. } = committed_field_verdict_v1(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
    BodyFieldClaim {
      field_index: outputs_field_index,
      carriage: fixture.certified_carriage_v1(preimage),
    },
    fixture.certified_reference_inputs_v1(
      block.tx_id,
      outputs_field_index,
      preimage,
    ),
    fixture.test_certificate_policy_id,
  )
  verdict
}

/// **Measured row.** The byte term at §5.4's per-field bound: 32,768 committed
/// bytes carried on §8.4's chunked route at §8.3's maximum chunk count, hashed
/// once against §4's commitment and decided. One item, so the per-item term
/// below is out of this reading.
test canonical_decodability_verdict_at_the_field_byte_bound() {
  certified_verdict(
    fixture.ungrammatical_trailing_preimage_v1(field_byte_bound),
  ) == verdict_trailing_bytes
}

/// **Measured control.** The same fixture built and its shape asserted, with no
/// door opened and no verdict taken, so the row above brackets the rule's own
/// share rather than claiming to isolate it.
test canonical_decodability_verdict_fixture_only_at_the_field_byte_bound() {
  let preimage = fixture.ungrammatical_trailing_preimage_v1(field_byte_bound)
  let block = committed_block(preimage)
  and {
    bytearray.length(preimage) == field_byte_bound,
    bytearray.length(block.compact_cbor) > 0,
  }
}

/// **Measured row.** The per-item term at the lower cardinality.
test canonical_decodability_verdict_at_two_hundred_fifty_items() {
  inline_verdict(minimum_width_trailing_preimage(lower_item_cardinality)) == verdict_trailing_bytes
}

/// **Measured control** for the row above: the same fixture built and its shape
/// asserted, with no door opened and no verdict taken.
///
/// These two controls are not decoration and they are what this ledger adds to
/// §8.11's precedent. A whole-test reading includes fixture construction, and
/// this lane's fixture is `encode_field_preimage` over a list of `count` items
/// — a fold of `concat` whose own cost grows with the cardinality being priced.
/// Differencing the two *rows* alone would therefore charge the walk for the
/// producer as well, and the number would look like the door's `walk_to_end`
/// by coincidence rather than by measurement.
test canonical_decodability_verdict_fixture_only_at_two_hundred_fifty_items() {
  let preimage = minimum_width_trailing_preimage(lower_item_cardinality)
  let block = committed_block(preimage)
  and {
    bytearray.length(preimage) == 2 + 2 * lower_item_cardinality + 1,
    bytearray.length(block.compact_cbor) > 0,
  }
}

/// **Measured row.** The per-item term at the upper cardinality. The pair
/// prices the walk; the difference is items and nothing else.
test canonical_decodability_verdict_at_five_hundred_items() {
  inline_verdict(minimum_width_trailing_preimage(upper_item_cardinality)) == verdict_trailing_bytes
}

/// **Measured control** for the row above, on the same terms.
test canonical_decodability_verdict_fixture_only_at_five_hundred_items() {
  let preimage = minimum_width_trailing_preimage(upper_item_cardinality)
  let block = committed_block(preimage)
  and {
    bytearray.length(preimage) == 3 + 2 * upper_item_cardinality + 1,
    bytearray.length(block.compact_cbor) > 0,
  }
}

/// **Ledger shape guard.** Every property a re-take could quietly shrink,
/// asserted where shrinking it turns this selector red before a cheaper row is
/// pinned: the bound row really sits at §5.4's byte bound, really needs tier 3,
/// really uses §8.3's maximum chunk count, and really carries one item; and the
/// two cardinality rows really differ by the item count alone.
test canonical_decodability_ledger_fixture_sits_at_the_field_byte_bound() {
  let preimage = fixture.ungrammatical_trailing_preimage_v1(field_byte_bound)
  let lower = minimum_width_trailing_preimage(lower_item_cardinality)
  let upper = minimum_width_trailing_preimage(upper_item_cardinality)
  and {
    bytearray.length(preimage) == field_byte_bound,
    bytearray.length(preimage) > chunk_bytes_k,
    expected_chunk_count(bytearray.length(preimage)) == max_tier3_chunk_count,
    envelope_verdict_v1(preimage) == verdict_trailing_bytes,
    // One item plus the trailing byte: `81` ‖ `59 LLLL` ‖ payload ‖ `41`.
    bytearray.at(preimage, 0) == 0x81,
    // Both cardinality rows fit tier 1 and differ only in item count.
    bytearray.length(lower) == 2 + 2 * lower_item_cardinality + 1,
    bytearray.length(upper) == 3 + 2 * upper_item_cardinality + 1,
    bytearray.length(upper) < chunk_bytes_k,
    envelope_verdict_v1(lower) == verdict_trailing_bytes,
    envelope_verdict_v1(upper) == verdict_trailing_bytes,
  }
}

/// **Ledger discrimination guard.** Neutralising the door's
/// `expect field_commitment(preimage) == expected_hash` in
/// `lib/midgard/native-tx-field-access-v1.ak` turns this red and nothing else
/// in the bound row moves: the certificate is a real one over real chunks of
/// the right lengths at the right count, so every §8.4/§8.6 check
/// `certified_chunks` runs still passes and only the §4 hash separates the
/// substituted bytes from the committed ones. It is also what makes the bound
/// row's carriage falsifiable — without it, tier 3 would be authenticated by a
/// manifest nobody checked against the block.
test canonical_decodability_verdict_refuses_substituted_certified_chunks() fail {
  let committed = fixture.ungrammatical_trailing_preimage_v1(field_byte_bound)
  let substituted = bytearray.concat(
    encode_field_preimage(
      [fixture.repeated_bytes_v1(#"ff", field_byte_bound - 5)],
    ),
    #"41",
  )
  let block = committed_block(committed)
  let CommittedFieldVerdictV1 { verdict, .. } = committed_field_verdict_v1(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
    BodyFieldClaim {
      field_index: outputs_field_index,
      carriage: fixture.certified_carriage_v1(substituted),
    },
    fixture.certified_reference_inputs_v1(
      block.tx_id,
      outputs_field_index,
      substituted,
    ),
    fixture.test_certificate_policy_id,
  )
  verdict >= 0
}
