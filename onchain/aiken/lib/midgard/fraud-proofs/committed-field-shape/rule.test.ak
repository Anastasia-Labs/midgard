//// Seam suite for the `committed-field-shape` rule — `docs/spec/midgard-tx.md`
//// §12.8.
////
//// Four things are pinned here and nowhere else.
////
//// 1. **The verdict table.** One vector per code, each asserted against its own
////    code rather than against "not admissible", so a fixture that stops
////    exercising the rule it was written for goes red instead of quietly
////    passing on a different refusal.
//// 2. **The boundary against §12.7**, in both directions and over the whole
////    table: nothing this rule convicts is a byte string §12.7 convicts, and
////    everything §12.7 convicts is rendered here as the one non-convicting code
////    that names it. The two fault kinds partition the committed byte strings a
////    door refuses; if they ever overlapped, one committed field would finalize
////    under two fault kinds.
//// 3. **Agreement with the door, in the two directions that decide outcomes.**
////    For every `(slot, bytes)` pair the table calls admissible, the §8.8
////    whole-materialising door *opens* the field; for every pair it convicts,
////    the door *aborts*. This is the property the family rests on: the verdict
////    is the door's own §7.4/§5.4 refusals restated as a decision, and the whole
////    point of the fault kind is that the door's abort is the stall. The
////    remaining direction — the door opening a field this rule does not call
////    admissible — is **not** claimed, and is pinned as unclaimed by
////    `committed_field_shape_defers_a_fixed_stride_field_the_door_opens` so that
////    a later reader meets it as a documented asymmetry rather than as a bug.
//// 4. **The cost claim** §12.8 publishes, as the rows
////    `scripts/committed-field-shape-exec-ledger-v1.json` pins.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/canonical_decodability/rule.{
  BodyFieldClaim, envelope_verdict_v1, verdict_grammatical,
} as canonical_decodability_rule
use midgard/fraud_proofs/committed_field_shape/rule.{
  CommittedFieldShapeVerdictV1, committed_field_shape_v1,
  committed_field_shape_verdict_v1, fixed_stride_field_indices_v1,
  is_committed_field_shape_violation_v1, shape_verdict_admissible,
  shape_verdict_code_count, shape_verdict_field_byte_bound,
  shape_verdict_not_an_envelope, shape_verdict_wrong_stride,
  sized_field_envelope_v1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_compact_cbor_v1}
use midgard/native_tx_field_access_v1.{
  Inline, authenticated_whole_field_view, chunk_bytes_k, encode_field_preimage,
  expected_chunk_count, field_count, field_item_count, field_stride,
  max_tier3_chunk_count, max_transaction_aggregate_field_bytes,
  spend_input_stride, walk_derived_stride,
}

/// §2.5 slot 0 (spend inputs) — fixed stride 40, so the door settles count
/// consistency by §7.4 arithmetic instead of by walking.
const spend_inputs_field_index: Int = 0

/// §2.5 slot 2 (outputs) — variable width, so §7.4's arithmetic never runs and
/// §5.4's byte bound is the only rule this family can find left there.
const outputs_field_index: Int = 2

/// §2.5 slot 7 (address witnesses) — fixed stride 103, in the witness-set half.
const address_witnesses_field_index: Int = 7

// ---------------------------------------------------------------------------
// The vectors
// ---------------------------------------------------------------------------

/// §5.1's empty field. `header_len + stride·0 == 1` at every fixed-stride slot,
/// so it is admissible everywhere — the one envelope no slot can refuse.
const empty_field_preimage: ByteArray = #"80"

/// A §5.1 envelope over one four-byte item: six bytes. `1 + 40·1 = 41`, so slot
/// 0 refuses it; slot 2 opens it.
fn wrong_stride_preimage() -> ByteArray {
  encode_field_preimage([#"deadbeef"])
}

/// Slot 0's honest shape: one 38-byte item, `81 ‖ 58 26 ‖ 38` = 41 bytes.
fn one_spend_input_preimage() -> ByteArray {
  encode_field_preimage([fixture.repeated_bytes_v1(#"00", 38)])
}

/// Slot 0's honest shape at two items: `1 + 40·2 = 81` bytes.
fn two_spend_input_preimage() -> ByteArray {
  encode_field_preimage(list.repeat(fixture.repeated_bytes_v1(#"00", 38), 2))
}

/// One byte too many for slot 0's one-item shape: 42 bytes where 41 is the only
/// admissible length at count 1.
fn one_byte_over_stride_preimage() -> ByteArray {
  encode_field_preimage([fixture.repeated_bytes_v1(#"00", 39)])
}

/// One byte too few: 40 bytes.
fn one_byte_under_stride_preimage() -> ByteArray {
  encode_field_preimage([fixture.repeated_bytes_v1(#"00", 37)])
}

/// Slot 7's honest shape: one 101-byte item, `1 + 103·1 = 104` bytes.
fn one_address_witness_preimage() -> ByteArray {
  encode_field_preimage([fixture.repeated_bytes_v1(#"00", 101)])
}

/// Exactly §5.4's per-field bound, and a well-formed envelope.
fn at_the_byte_bound_preimage() -> ByteArray {
  sized_field_envelope_v1(max_transaction_aggregate_field_bytes, #"00")
}

/// One byte above it, and well-formed in every other respect.
fn above_the_byte_bound_preimage() -> ByteArray {
  sized_field_envelope_v1(max_transaction_aggregate_field_bytes + 1, #"00")
}

/// `9a` is a well-formed four-byte CBOR array head and outside §5.1's
/// acceptance set — §12.7's fault, and this rule says so rather than convicting.
const four_byte_array_head: ByteArray = #"9a00000001"

/// A well-formed empty envelope with a byte after it: §5.1 admits no trailing
/// content, so §12.7 convicts it and this rule defers.
const trailing_byte: ByteArray = #"8041"

/// One row of the table: a slot, the bytes committed at it, and the verdict the
/// pair must earn. A record rather than a triple because the whole point of this
/// fault kind is that the first two members are *both* arguments to the verdict,
/// and a positional triple is exactly where that gets transposed.
type ShapeVectorV1 {
  field_index: Int,
  preimage: ByteArray,
  verdict: Int,
}

/// Every vector above, at the slot it is written for, paired with the verdict it
/// must earn.
fn verdict_table() -> List<ShapeVectorV1> {
  [
    ShapeVectorV1 {
      field_index: outputs_field_index,
      preimage: empty_field_preimage,
      verdict: shape_verdict_admissible,
    },
    ShapeVectorV1 {
      field_index: spend_inputs_field_index,
      preimage: empty_field_preimage,
      verdict: shape_verdict_admissible,
    },
    ShapeVectorV1 {
      field_index: spend_inputs_field_index,
      preimage: one_spend_input_preimage(),
      verdict: shape_verdict_admissible,
    },
    ShapeVectorV1 {
      field_index: spend_inputs_field_index,
      preimage: two_spend_input_preimage(),
      verdict: shape_verdict_admissible,
    },
    ShapeVectorV1 {
      field_index: address_witnesses_field_index,
      preimage: one_address_witness_preimage(),
      verdict: shape_verdict_admissible,
    },
    ShapeVectorV1 {
      field_index: outputs_field_index,
      preimage: wrong_stride_preimage(),
      verdict: shape_verdict_admissible,
    },
    ShapeVectorV1 {
      field_index: outputs_field_index,
      preimage: at_the_byte_bound_preimage(),
      verdict: shape_verdict_admissible,
    },
    ShapeVectorV1 {
      field_index: spend_inputs_field_index,
      preimage: wrong_stride_preimage(),
      verdict: shape_verdict_wrong_stride,
    },
    ShapeVectorV1 {
      field_index: spend_inputs_field_index,
      preimage: one_byte_over_stride_preimage(),
      verdict: shape_verdict_wrong_stride,
    },
    ShapeVectorV1 {
      field_index: spend_inputs_field_index,
      preimage: one_byte_under_stride_preimage(),
      verdict: shape_verdict_wrong_stride,
    },
    ShapeVectorV1 {
      field_index: address_witnesses_field_index,
      preimage: wrong_stride_preimage(),
      verdict: shape_verdict_wrong_stride,
    },
    ShapeVectorV1 {
      field_index: outputs_field_index,
      preimage: above_the_byte_bound_preimage(),
      verdict: shape_verdict_field_byte_bound,
    },
    ShapeVectorV1 {
      field_index: spend_inputs_field_index,
      preimage: above_the_byte_bound_preimage(),
      verdict: shape_verdict_field_byte_bound,
    },
    ShapeVectorV1 {
      field_index: outputs_field_index,
      preimage: #"",
      verdict: shape_verdict_not_an_envelope,
    },
    ShapeVectorV1 {
      field_index: outputs_field_index,
      preimage: four_byte_array_head,
      verdict: shape_verdict_not_an_envelope,
    },
    ShapeVectorV1 {
      field_index: outputs_field_index,
      preimage: trailing_byte,
      verdict: shape_verdict_not_an_envelope,
    },
    ShapeVectorV1 {
      field_index: spend_inputs_field_index,
      preimage: trailing_byte,
      verdict: shape_verdict_not_an_envelope,
    },
  ]
}

/// Every row of the table earns exactly the verdict it is written for.
test committed_field_shape_verdict_table_is_exact() {
  list.all(
    verdict_table(),
    fn(row) {
      committed_field_shape_verdict_v1(row.field_index, row.preimage) == row.verdict
    },
  )
}

/// Every code the table asserts is distinct, and the table covers the whole code
/// space. Without this the table could assert seventeen rows against two codes
/// and still be "exact".
test committed_field_shape_verdict_table_covers_every_code() {
  let codes = verdict_table()
    |> list.map(fn(row) { row.verdict })
    |> list.unique
  and {
    list.length(codes) == shape_verdict_code_count,
    list.all(
      codes,
      fn(code) {
        and {
          code >= 0,
          code < shape_verdict_code_count,
        }
      },
    ),
  }
}

// ---------------------------------------------------------------------------
// The boundary against §12.7
// ---------------------------------------------------------------------------

/// **The disjointness property, over the whole table and in both directions.**
///
/// Forward: nothing this rule convicts is a byte string §12.7 convicts — every
/// convicting row is `verdict_grammatical` under §12.7's bytes-only verdict.
/// Reverse: every byte string §12.7 convicts is rendered here as
/// `shape_verdict_not_an_envelope`, which this family's adjudication refuses.
///
/// Together they say the two fault kinds partition the committed byte strings a
/// door refuses. If they ever overlapped, one committed field would be faultable
/// under two kinds and §12.1's one-spelling rule would be broken across sections
/// rather than within one.
test committed_field_shape_is_disjoint_from_canonical_decodability() {
  list.all(
    verdict_table(),
    fn(row) {
      let shape = committed_field_shape_verdict_v1(
        row.field_index,
        row.preimage,
      )
      let envelope = envelope_verdict_v1(row.preimage)
      and {
        // Convicting here implies §12.7 does not convict.
        !is_committed_field_shape_violation_v1(row.field_index, shape) || envelope == verdict_grammatical,
        // §12.7 convicting implies this rule renders the deferring code.
        envelope == verdict_grammatical || shape == shape_verdict_not_an_envelope,
      }
    },
  )
}

/// The deferring code is genuinely non-convicting at every slot, so the boundary
/// above is enforced at the adjudication and not merely rendered at the verdict.
test committed_field_shape_never_convicts_a_non_envelope() {
  list.all(
    list.range(0, field_count - 1),
    fn(field_index) {
      !is_committed_field_shape_violation_v1(
        field_index,
        shape_verdict_not_an_envelope,
      )
    },
  )
}

// ---------------------------------------------------------------------------
// The slot half of the verdict
// ---------------------------------------------------------------------------

/// §5.3's stride table, as this family reads it: five fixed-stride slots and
/// four walked ones. Pinned as a list so the spec table, the TypeScript twin and
/// the vectors are all checked against one reading of §5.3 rather than three.
test committed_field_shape_pins_the_fixed_stride_slots() {
  and {
    fixed_stride_field_indices_v1() == [0, 1, 3, 4, 7],
    field_stride(spend_inputs_field_index) == spend_input_stride,
    field_stride(outputs_field_index) == walk_derived_stride,
  }
}

/// **The same bytes, every slot.** One §5.1 envelope, asked of all nine slots:
/// the five fixed-stride ones convict it and the four walked ones do not. This
/// is what "a function of `(field_index, preimage)`" means, measured rather than
/// asserted, and it is the property §12.7's bytes-only verdict cannot express.
test committed_field_shape_verdict_depends_on_the_slot() {
  let preimage = wrong_stride_preimage()
  list.all(
    list.range(0, field_count - 1),
    fn(field_index) {
      let verdict = committed_field_shape_verdict_v1(field_index, preimage)
      if list.has(fixed_stride_field_indices_v1(), field_index) {
        verdict == shape_verdict_wrong_stride
      } else {
        verdict == shape_verdict_admissible
      }
    },
  )
}

/// §5.4's bound is a bound and not a clamp: exactly at it the envelope is
/// admissible, one byte above it the verdict convicts. At a variable-width slot,
/// so §7.4's arithmetic is out of the reading.
test committed_field_shape_pins_the_byte_bound_boundary() {
  and {
    bytearray.length(at_the_byte_bound_preimage()) == max_transaction_aggregate_field_bytes,
    bytearray.length(above_the_byte_bound_preimage()) == max_transaction_aggregate_field_bytes + 1,
    committed_field_shape_verdict_v1(
      outputs_field_index,
      at_the_byte_bound_preimage(),
    ) == shape_verdict_admissible,
    committed_field_shape_verdict_v1(
      outputs_field_index,
      above_the_byte_bound_preimage(),
    ) == shape_verdict_field_byte_bound,
  }
}

/// **The byte bound is checked before the stride, and one field earns one
/// accusation.** An oversize envelope at a fixed-stride slot also fails §7.4's
/// arithmetic, and is convicted as the byte-bound violation alone, because
/// that is the `expect` `whole_view` reaches first (§12.1).
test committed_field_shape_byte_bound_precedes_the_stride() {
  let preimage = above_the_byte_bound_preimage()
  and {
    committed_field_shape_verdict_v1(spend_inputs_field_index, preimage) == shape_verdict_field_byte_bound,
    // The stride arithmetic really does fail for these bytes, so the row above
    // is an ordering pin and not an accident of the fixture.
    1 + spend_input_stride * 1 != bytearray.length(preimage),
  }
}

/// §7.4's arithmetic is an equality, refused in both directions: one byte over
/// the slot's stride and one byte under it both convict.
test committed_field_shape_pins_the_stride_boundary() {
  and {
    bytearray.length(one_spend_input_preimage()) == 1 + spend_input_stride,
    committed_field_shape_verdict_v1(
      spend_inputs_field_index,
      one_spend_input_preimage(),
    ) == shape_verdict_admissible,
    bytearray.length(one_byte_over_stride_preimage()) == 2 + spend_input_stride,
    committed_field_shape_verdict_v1(
      spend_inputs_field_index,
      one_byte_over_stride_preimage(),
    ) == shape_verdict_wrong_stride,
    bytearray.length(one_byte_under_stride_preimage()) == spend_input_stride,
    committed_field_shape_verdict_v1(
      spend_inputs_field_index,
      one_byte_under_stride_preimage(),
    ) == shape_verdict_wrong_stride,
  }
}

/// The arithmetic holds across the header widths, so the `header_len` term is
/// really being read out of the envelope rather than assumed to be one byte.
test committed_field_shape_holds_across_the_header_widths() {
  list.all(
    [0, 1, 23, 24, 255, 256],
    fn(count) {
      let preimage = encode_field_preimage(
        list.repeat(fixture.repeated_bytes_v1(#"00", 38), count),
      )
      committed_field_shape_verdict_v1(spend_inputs_field_index, preimage) == shape_verdict_admissible
    },
  )
}

// ---------------------------------------------------------------------------
// Agreement with the §8.8 door
// ---------------------------------------------------------------------------

/// Open the accused field through the real §8.8 whole-materialising door. This
/// is the code path §12.8 leaves exactly as it is, so agreement is asserted
/// against it rather than against a re-statement of it.
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

/// Forward agreement: every pair the verdict calls admissible, the door opens —
/// and opens to the item count the envelope declares.
test committed_field_shape_door_opens_every_admissible_vector() {
  and {
    door_item_count_at(outputs_field_index, empty_field_preimage) == 0,
    door_item_count_at(spend_inputs_field_index, empty_field_preimage) == 0,
    door_item_count_at(spend_inputs_field_index, one_spend_input_preimage()) == 1,
    door_item_count_at(spend_inputs_field_index, two_spend_input_preimage()) == 2,
    door_item_count_at(
      address_witnesses_field_index,
      one_address_witness_preimage(),
    ) == 1,
    door_item_count_at(outputs_field_index, wrong_stride_preimage()) == 1,
  }
}

// Reverse agreement. One `fail` selector per convicting vector, because a single
// fold over the table would prove only that *some* vector aborts: an abort ends
// the test, so the vectors after it are never reached.

test committed_field_shape_door_refuses_a_wrong_stride_field() fail {
  door_item_count_at(spend_inputs_field_index, wrong_stride_preimage()) >= 0
}

test committed_field_shape_door_refuses_one_byte_over_the_stride() fail {
  door_item_count_at(spend_inputs_field_index, one_byte_over_stride_preimage()) >= 0
}

test committed_field_shape_door_refuses_one_byte_under_the_stride() fail {
  door_item_count_at(spend_inputs_field_index, one_byte_under_stride_preimage()) >= 0
}

test committed_field_shape_door_refuses_a_wrong_stride_witness_field() fail {
  door_item_count_at(address_witnesses_field_index, wrong_stride_preimage()) >= 0
}

test committed_field_shape_door_refuses_an_oversize_field() fail {
  door_item_count_at(outputs_field_index, above_the_byte_bound_preimage()) >= 0
}

test committed_field_shape_door_refuses_an_oversize_fixed_stride_field() fail {
  door_item_count_at(spend_inputs_field_index, above_the_byte_bound_preimage()) >= 0
}

/// The companion positive at the boundary: the same door opens the same slot at
/// exactly §5.4's bound, so the two `fail` selectors above trap the bound and
/// not the fixture.
test committed_field_shape_door_opens_the_field_at_the_byte_bound() {
  door_item_count_at(outputs_field_index, at_the_byte_bound_preimage()) == 1
}

/// **The one direction of the agreement this family deliberately does not
/// claim, pinned so that nobody later mistakes it for a bug.**
///
/// `verdict == admissible` implies the door opens, and `verdict` convicting
/// implies the door aborts. The *converse* of the first does not hold at a
/// fixed-stride slot: `whole_view` settles §5.1 there by arithmetic and never
/// walks the items (§7.4 item 2 puts per-item wrapper canonicality at the
/// accessor instead), so a preimage whose stride arithmetic holds opens the
/// door whatever its item bytes are. The bytes below are `81` followed by forty
/// bytes that are not a §5.1 item head at all: `1 + 40·1 = 41` satisfies §7.4,
/// the door opens, §12.7 convicts, and this rule renders the deferring code.
///
/// It is fail-safe by construction rather than by luck — code 1 does not
/// convict, so the disagreement can only ever leave a field to §12.7, never
/// take one from it — and the accessor still refuses the read.
test committed_field_shape_defers_a_fixed_stride_field_the_door_opens() {
  let preimage = bytearray.concat(
    #"81",
    fixture.repeated_bytes_v1(#"ff", spend_input_stride),
  )
  and {
    bytearray.length(preimage) == 1 + spend_input_stride,
    envelope_verdict_v1(preimage) != verdict_grammatical,
    committed_field_shape_verdict_v1(spend_inputs_field_index, preimage) == shape_verdict_not_an_envelope,
    !is_committed_field_shape_violation_v1(
      spend_inputs_field_index,
      shape_verdict_not_an_envelope,
    ),
    door_item_count_at(spend_inputs_field_index, preimage) == 1,
  }
}

// ---------------------------------------------------------------------------
// Cost claim — the rows `scripts/committed-field-shape-exec-ledger-v1.json`
// pins (§12.8)
// ---------------------------------------------------------------------------

/// The two item cardinalities the per-item term is priced at. Both are carried
/// inline: `header_len + count` stays far under §8.3's `K`, so the difference
/// between the two rows is the walk and nothing else.
const lower_item_cardinality: Int = 250

const upper_item_cardinality: Int = 500

/// A §5.1 envelope over `count` **empty** items — §5.1's narrowest, one byte
/// each. It is grammatical, so `envelope_verdict_v1` pays the whole walk before
/// this rule's own two questions are asked, which is the shape a per-item cost
/// claim has to be taken at; and at slot 0 it is `header_len + 40·count` short
/// of its own length, so it convicts.
fn minimum_width_preimage(count: Int) -> ByteArray {
  encode_field_preimage(list.repeat(#"", count))
}

fn committed_block(
  field_index: Int,
  preimage: ByteArray,
) -> fixture.NativeBlockFixtureV1 {
  fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    field_index,
    preimage,
  )
}

fn inline_verdict(field_index: Int, preimage: ByteArray) -> Int {
  let block = committed_block(field_index, preimage)
  let CommittedFieldShapeVerdictV1 { verdict, .. } = committed_field_shape_v1(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
    BodyFieldClaim { field_index, carriage: Inline { preimage } },
    [],
    fixture.test_certificate_policy_id,
  )
  verdict
}

fn certified_verdict(field_index: Int, preimage: ByteArray) -> Int {
  let block = committed_block(field_index, preimage)
  let CommittedFieldShapeVerdictV1 { verdict, .. } = committed_field_shape_v1(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
    BodyFieldClaim {
      field_index,
      carriage: fixture.certified_carriage_v1(preimage),
    },
    fixture.certified_reference_inputs_v1(block.tx_id, field_index, preimage),
    fixture.test_certificate_policy_id,
  )
  verdict
}

/// **Measured row.** The byte term at §5.4's per-field bound: 32,768 committed
/// bytes carried on §8.4's chunked route at §8.3's maximum chunk count, hashed
/// once against §4's commitment and decided at slot 0. One item, so the per-item
/// term below is out of this reading.
test committed_field_shape_verdict_at_the_field_byte_bound() {
  certified_verdict(spend_inputs_field_index, at_the_byte_bound_preimage()) == shape_verdict_wrong_stride
}

/// **Measured control.** The same fixture built and its shape asserted, with no
/// door opened and no verdict taken, so the row above brackets the rule's own
/// share rather than claiming to isolate it.
test committed_field_shape_verdict_fixture_only_at_the_field_byte_bound() {
  let preimage = at_the_byte_bound_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  and {
    bytearray.length(preimage) == max_transaction_aggregate_field_bytes,
    bytearray.length(block.compact_cbor) > 0,
  }
}

/// **Measured row.** The per-item term at the lower cardinality.
test committed_field_shape_verdict_at_two_hundred_fifty_items() {
  inline_verdict(
    spend_inputs_field_index,
    minimum_width_preimage(lower_item_cardinality),
  ) == shape_verdict_wrong_stride
}

/// **Measured control** for the row above: the same fixture built and its shape
/// asserted, with no door opened and no verdict taken.
///
/// These two controls are not decoration and they are what this ledger inherits
/// from §12.7's precedent. A whole-test reading includes fixture construction,
/// and this lane's fixture is `encode_field_preimage` over a list of `count`
/// items — a fold of `concat` whose own cost grows with the cardinality being
/// priced. Differencing the two *rows* alone would therefore charge the walk for
/// the producer as well.
test committed_field_shape_verdict_fixture_only_at_two_hundred_fifty_items() {
  let preimage = minimum_width_preimage(lower_item_cardinality)
  let block = committed_block(spend_inputs_field_index, preimage)
  and {
    bytearray.length(preimage) == 2 + lower_item_cardinality,
    bytearray.length(block.compact_cbor) > 0,
  }
}

/// **Measured row.** The per-item term at the upper cardinality. The pair prices
/// the walk; the difference is items and nothing else.
test committed_field_shape_verdict_at_five_hundred_items() {
  inline_verdict(
    spend_inputs_field_index,
    minimum_width_preimage(upper_item_cardinality),
  ) == shape_verdict_wrong_stride
}

/// **Measured control** for the row above, on the same terms.
test committed_field_shape_verdict_fixture_only_at_five_hundred_items() {
  let preimage = minimum_width_preimage(upper_item_cardinality)
  let block = committed_block(spend_inputs_field_index, preimage)
  and {
    bytearray.length(preimage) == 3 + upper_item_cardinality,
    bytearray.length(block.compact_cbor) > 0,
  }
}

/// This family's single-transaction ceiling, **measured** rather than fitted
/// (#580, 2026-08-15).
///
/// §12.8 and this ledger's note published "about 1,074 items" as this lane's
/// own arithmetic — `(13,200,000 − 261,998) / 12,040.88` — and deliberately did
/// not reconcile it with the ~1,077 §12.7 publishes for its fixtures, because
/// reconciling two extrapolations is not a reconciliation. The four selectors
/// below replace the fit with a reading, taken exactly as §12.7's twin is:
/// same shape as the two cardinality rows, carried to the cardinality at which
/// the rule's own share (row minus its fixture-only control) crosses GOAL_SPEC
/// §3.3's 13,200,000-memory basis, bisected on the net memory axis with the
/// pinned fork. **The reconciliation is now two readings rather than two fits:
/// 1,074 here against §12.7's 1,076, and the two-item gap is this family's
/// higher intercept — the §7.4 stride arithmetic it asks after the walk.**
///
/// They are **not** ledger rows, for the reason §12.7's twin states: a
/// whole-test reading at this cardinality is about 24.1M mem, and a
/// within-basis ledger records only rows whose raw reading fits the basis.
///
/// **#606 re-measurement, 2026-08-16 — the ceiling moved down to 1,074.**
/// Identically to §12.7's twin, and by the same constant: the E2 certificate
/// repair's welded `field_hash` and its new door equality add **+9,200 memory /
/// +1,472,000 cpu** to this family's own net at every cardinality — both ledger
/// cardinality rows moved by exactly that with their fixture-only controls
/// unmoved, so the marginal is untouched (12,024 mem/item across this boundary
/// pair) and only the intercept rose. #580's margin at 1,075 was 3,762 memory
/// units, which the constant consumes: 1,075 now nets **13,205,438** mem and is
/// over the basis, while 1,074 nets **13,193,414** (margin 6,586). The
/// two-item gap against §12.7 survives the move, which is what a constant
/// common to both families predicts.
const measured_item_ceiling: Int = 1074

const first_item_cardinality_over_the_basis: Int = 1075

/// **Measured row.** The largest item cardinality this family adjudicates
/// inside the §3.3 memory basis.
test committed_field_shape_verdict_at_the_measured_item_ceiling() {
  inline_verdict(
    spend_inputs_field_index,
    minimum_width_preimage(measured_item_ceiling),
  ) == shape_verdict_wrong_stride
}

/// **Measured control** for the row above, on the cardinality rows' terms.
test committed_field_shape_verdict_fixture_only_at_the_measured_item_ceiling() {
  let preimage = minimum_width_preimage(measured_item_ceiling)
  let block = committed_block(spend_inputs_field_index, preimage)
  and {
    bytearray.length(preimage) == 3 + measured_item_ceiling,
    bytearray.length(block.compact_cbor) > 0,
  }
}

/// **Measured row.** One item further — the first cardinality whose net exceeds
/// the basis, which is what makes the ceiling a crossing rather than a bound.
test committed_field_shape_verdict_at_the_first_item_over_the_basis() {
  inline_verdict(
    spend_inputs_field_index,
    minimum_width_preimage(first_item_cardinality_over_the_basis),
  ) == shape_verdict_wrong_stride
}

/// **Measured control** for the row above, on the same terms.
test committed_field_shape_verdict_fixture_only_at_the_first_item_over_the_basis() {
  let preimage = minimum_width_preimage(first_item_cardinality_over_the_basis)
  let block = committed_block(spend_inputs_field_index, preimage)
  and {
    bytearray.length(preimage) == 3 + first_item_cardinality_over_the_basis,
    bytearray.length(block.compact_cbor) > 0,
  }
}

/// **Ceiling shape guard**, on §12.7's twin's terms: the two cardinalities are
/// adjacent, both still fit tier 1 so the reading is the walk and not carriage,
/// and both are grammatical envelopes convicted on §7.4 — the shape this
/// family's per-item term is priced at.
test committed_field_shape_measured_ceiling_shape_is_the_cardinality_rows_shape() {
  let ceiling = minimum_width_preimage(measured_item_ceiling)
  let over = minimum_width_preimage(first_item_cardinality_over_the_basis)
  and {
    first_item_cardinality_over_the_basis == measured_item_ceiling + 1,
    measured_item_ceiling > upper_item_cardinality,
    bytearray.length(ceiling) < chunk_bytes_k,
    bytearray.length(over) < chunk_bytes_k,
    envelope_verdict_v1(ceiling) == verdict_grammatical,
    envelope_verdict_v1(over) == verdict_grammatical,
  }
}

/// **Ledger shape guard.** Every property a re-take could quietly shrink,
/// asserted where shrinking it turns this selector red before a cheaper row is
/// pinned: the bound row really sits at §5.4's byte bound, really needs tier 3,
/// really uses §8.3's maximum chunk count, really carries one item and really
/// convicts on §7.4; and the two cardinality rows really differ by the item
/// count alone and are really grammatical, which is what makes each of them pay
/// the whole walk.
test committed_field_shape_ledger_fixture_sits_at_the_field_byte_bound() {
  let preimage = at_the_byte_bound_preimage()
  let lower = minimum_width_preimage(lower_item_cardinality)
  let upper = minimum_width_preimage(upper_item_cardinality)
  and {
    bytearray.length(preimage) == max_transaction_aggregate_field_bytes,
    bytearray.length(preimage) > chunk_bytes_k,
    expected_chunk_count(bytearray.length(preimage)) == max_tier3_chunk_count,
    envelope_verdict_v1(preimage) == verdict_grammatical,
    committed_field_shape_verdict_v1(spend_inputs_field_index, preimage) == shape_verdict_wrong_stride,
    // One item: `81` ‖ `59 LLLL` ‖ payload.
    bytearray.at(preimage, 0) == 0x81,
    // Both cardinality rows fit tier 1, are grammatical, and differ only in
    // item count.
    bytearray.length(lower) == 2 + lower_item_cardinality,
    bytearray.length(upper) == 3 + upper_item_cardinality,
    bytearray.length(upper) < chunk_bytes_k,
    envelope_verdict_v1(lower) == verdict_grammatical,
    envelope_verdict_v1(upper) == verdict_grammatical,
    committed_field_shape_verdict_v1(spend_inputs_field_index, lower) == shape_verdict_wrong_stride,
    committed_field_shape_verdict_v1(spend_inputs_field_index, upper) == shape_verdict_wrong_stride,
  }
}

/// **Ledger discrimination guard.** Neutralising the door's
/// `expect field_commitment(preimage) == expected_hash` in
/// `lib/midgard/native-tx-field-access-v1.ak` turns this red and nothing else in
/// the bound row moves: the certificate is a real one over real chunks of the
/// right lengths at the right count, so every §8.4/§8.6 check `certified_chunks`
/// runs still passes and only the §4 hash separates the substituted bytes from
/// the committed ones. It is also what makes the bound row's carriage
/// falsifiable — without it, tier 3 would be authenticated by a manifest nobody
/// checked against the block.
test committed_field_shape_verdict_refuses_substituted_certified_chunks() fail {
  let committed = at_the_byte_bound_preimage()
  let substituted = sized_field_envelope_v1(
    max_transaction_aggregate_field_bytes,
    #"ff",
  )
  let block = committed_block(spend_inputs_field_index, committed)
  let CommittedFieldShapeVerdictV1 { verdict, .. } = committed_field_shape_v1(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
    BodyFieldClaim {
      field_index: spend_inputs_field_index,
      carriage: fixture.certified_carriage_v1(substituted),
    },
    fixture.certified_reference_inputs_v1(
      block.tx_id,
      spend_inputs_field_index,
      substituted,
    ),
    fixture.test_certificate_policy_id,
  )
  verdict >= 0
}
