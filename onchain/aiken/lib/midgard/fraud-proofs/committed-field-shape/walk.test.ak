//// The committed-field-shape verdict taken as step 01 takes it
//// (`docs/spec/midgard-tx.md` §12.8): §12.7's §5.1 walk one batch per
//// transaction, and the slot's shape questions asked of the batch that ends it.
////
//// Two things are pinned here.
////
//// 1. **Equivalence with the single-evaluation verdict.** The verdict step 01
////    reaches over any number of batches is the verdict the family rendered
////    when it read a whole field in one evaluation. `one_evaluation_verdict`
////    below is that verdict, transcribed as it stood, and is the oracle: a
////    fuzzed comparison at every slot, and fixed comparisons at the batch
////    boundaries and the largest field, where fuzzed inputs do not reach.
//// 2. **What each batch boundary renders at a fixed-stride slot**, where the
////    walk's own verdict and this family's are different numbers: a
////    grammatical field there is a conviction, and every refusal of §5.1 is
////    `shape_verdict_not_an_envelope`.

use aiken/collection/list
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/fraud_proofs/canonical_decodability/rule.{
  EnvelopeItems, EnvelopeVerdict, EnvelopeWalkV1, envelope_batch_items,
  envelope_first_batch_v1, envelope_items_batch_v1, envelope_verdict_v1,
  verdict_grammatical,
} as canonical_decodability_rule
use midgard/fraud_proofs/canonical_decodability/walk_fixture.{
  array_head, one_byte_items, one_byte_run, worst_batch_preimage,
}
use midgard/fraud_proofs/committed_field_shape/rule.{
  committed_field_shape_verdict_v1, shape_verdict_admissible,
  shape_verdict_after_walk_v1, shape_verdict_field_byte_bound,
  shape_verdict_not_an_envelope, shape_verdict_wrong_stride,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/native_tx_field_access_v1.{
  field_count, field_stride, max_transaction_aggregate_field_bytes,
  walk_derived_stride,
}

const b: Int = envelope_batch_items

/// The largest item count a field reaches: §5.4's 32,768-byte bound filled
/// with one-byte items under a three-byte array head.
const max_item_count: Int = max_transaction_aggregate_field_bytes - 3

/// §2.5 slot 0 (spend inputs), stride 40.
const spend_inputs_field_index: Int = 0

/// §2.5 slot 2 (outputs), walked.
const outputs_field_index: Int = 2

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// The verdict as it stood when the family read a field in one evaluation:
/// header first, then §12.7's verdict, then the byte bound and the stride. Kept
/// here, verbatim in its decisions, as the oracle the walked verdict is held to.
fn one_evaluation_verdict(field_index: Int, preimage: ByteArray) -> Int {
  let stride = field_stride(field_index)
  when one_evaluation_header(preimage) is {
    None -> shape_verdict_not_an_envelope
    Some(Pair(header_len, count)) ->
      if envelope_verdict_v1(preimage) != verdict_grammatical {
        shape_verdict_not_an_envelope
      } else {
        let total_length = bytearray.length(preimage)
        if total_length > max_transaction_aggregate_field_bytes {
          shape_verdict_field_byte_bound
        } else if stride == walk_derived_stride {
          shape_verdict_admissible
        } else if header_len + stride * count == total_length {
          shape_verdict_admissible
        } else {
          shape_verdict_wrong_stride
        }
      }
  }
}

fn one_evaluation_header(preimage: ByteArray) -> Option<Pair<Int, Int>> {
  let total_length = bytearray.length(preimage)
  if total_length == 0 {
    None
  } else {
    let tag = bytearray.at(preimage, 0)
    if tag >= 128 && tag <= 151 {
      Some(Pair(1, tag - 128))
    } else if tag == 152 {
      if total_length < 2 || bytearray.at(preimage, 1) < 24 {
        None
      } else {
        Some(Pair(2, bytearray.at(preimage, 1)))
      }
    } else if tag == 153 {
      if total_length < 3 {
        None
      } else {
        let count = bytearray.at(preimage, 1) * 256 + bytearray.at(preimage, 2)
        if count <= 0xff {
          None
        } else {
          Some(Pair(3, count))
        }
      }
    } else {
      None
    }
  }
}

/// The verdict step 01 reaches: the walk one batch at a time, then the shape
/// questions of the batch that ends it — and how many batches it took.
fn walked_verdict(field_index: Int, preimage: ByteArray) -> Pair<Int, Int> {
  continue_walk(field_index, preimage, envelope_first_batch_v1(preimage), 1)
}

fn continue_walk(
  field_index: Int,
  preimage: ByteArray,
  walk: EnvelopeWalkV1,
  batches: Int,
) -> Pair<Int, Int> {
  when walk is {
    EnvelopeVerdict { verdict } ->
      Pair(shape_verdict_after_walk_v1(field_index, preimage, verdict), batches)
    EnvelopeItems { remaining, offset } ->
      continue_walk(
        field_index,
        preimage,
        envelope_items_batch_v1(preimage, remaining, offset),
        batches + 1,
      )
  }
}

/// The walked verdict, the one-evaluation verdict and the published verdict
/// agree at every slot.
fn agrees_at_every_slot(preimage: ByteArray) -> Bool {
  agrees_at(list.range(0, field_count - 1), preimage)
}

fn agrees_at(field_indices: List<Int>, preimage: ByteArray) -> Bool {
  list.all(
    field_indices,
    fn(field_index) {
      let oracle = one_evaluation_verdict(field_index, preimage)
      and {
        walked_verdict(field_index, preimage).1st == oracle,
        committed_field_shape_verdict_v1(field_index, preimage) == oracle,
      }
    },
  )
}

/// `count` one-byte items, the one at `bad_index` replaced by `bad_item`.
fn one_byte_items_with(
  count: Int,
  bad_index: Int,
  bad_item: ByteArray,
) -> ByteArray {
  bytearray.concat(
    array_head(count),
    bytearray.concat(
      one_byte_run(bad_index),
      bytearray.concat(bad_item, one_byte_run(count - bad_index - 1)),
    ),
  )
}

/// `count` items of exactly `stride` bytes under a minimal array head: the
/// honest shape at a slot of that stride.
fn stride_items(stride: Int, count: Int) -> ByteArray {
  bytearray.concat(
    array_head(count),
    fixture.repeated_bytes_v1(
      bytearray.concat(
        bytearray.concat(#"58", bytearray.from_int_big_endian(stride - 2, 1)),
        fixture.repeated_bytes_v1(#"00", stride - 2),
      ),
      count,
    ),
  )
}

// ---------------------------------------------------------------------------
// Batch boundaries at a fixed-stride slot
// ---------------------------------------------------------------------------

test committed_field_shape_walk_one_item_under_a_batch_convicts_in_one_step() {
  walked_verdict(spend_inputs_field_index, one_byte_items(b - 1)) == Pair(
    shape_verdict_wrong_stride,
    1,
  )
}

test committed_field_shape_walk_exactly_a_batch_convicts_in_one_step() {
  walked_verdict(spend_inputs_field_index, one_byte_items(b)) == Pair(
    shape_verdict_wrong_stride,
    1,
  )
}

test committed_field_shape_walk_one_item_over_a_batch_convicts_in_two_steps() {
  walked_verdict(spend_inputs_field_index, one_byte_items(b + 1)) == Pair(
    shape_verdict_wrong_stride,
    2,
  )
}

/// The same bytes at a walked slot are admissible, after the same two steps.
test committed_field_shape_walk_one_item_over_a_batch_is_admissible_at_a_walked_slot() {
  walked_verdict(outputs_field_index, one_byte_items(b + 1)) == Pair(
    shape_verdict_admissible,
    2,
  )
}

/// Honest stride items are admissible at their slot and convicted at a slot of
/// another stride. Slot 0's stride fits at most 819 items in §5.4's bound, so
/// its honest fields are one batch; slot 3's 30-byte stride fits 1,091, so its
/// honest fields can take two.
test committed_field_shape_walk_honest_stride_items_are_admissible() {
  and {
    walked_verdict(spend_inputs_field_index, stride_items(40, 819)) == Pair(
      shape_verdict_admissible,
      1,
    ),
    walked_verdict(3, stride_items(30, b + 1)) == Pair(
      shape_verdict_admissible,
      2,
    ),
    walked_verdict(spend_inputs_field_index, stride_items(30, b + 1)) == Pair(
      shape_verdict_wrong_stride,
      2,
    ),
    walked_verdict(spend_inputs_field_index, stride_items(40, 820)) == Pair(
      shape_verdict_field_byte_bound,
      1,
    ),
  }
}

test committed_field_shape_walk_bad_item_first_in_the_first_batch() {
  walked_verdict(spend_inputs_field_index, one_byte_items_with(b + 1, 0, #"00")) == Pair(
    shape_verdict_not_an_envelope,
    1,
  )
}

test committed_field_shape_walk_bad_item_last_in_the_first_batch() {
  walked_verdict(
    spend_inputs_field_index,
    one_byte_items_with(b + 1, b - 1, #"00"),
  ) == Pair(shape_verdict_not_an_envelope, 1)
}

test committed_field_shape_walk_bad_item_first_in_a_resumed_batch() {
  walked_verdict(
    spend_inputs_field_index,
    one_byte_items_with(2 * b + 1, b, #"00"),
  ) == Pair(shape_verdict_not_an_envelope, 2)
}

test committed_field_shape_walk_bad_item_last_in_a_resumed_batch() {
  walked_verdict(
    spend_inputs_field_index,
    one_byte_items_with(2 * b + 1, 2 * b - 1, #"5805"),
  ) == Pair(shape_verdict_not_an_envelope, 2)
}

/// A batch that reads its full share and lands on the last byte with items
/// still owed ends the walk at once, and is no envelope.
test committed_field_shape_walk_items_owed_at_the_last_byte_end_the_walk_at_once() {
  walked_verdict(
    spend_inputs_field_index,
    bytearray.concat(array_head(b + 1), one_byte_run(b)),
  ) == Pair(shape_verdict_not_an_envelope, 1)
}

/// Item heads are read eight at a time. A run of eight that lands exactly on
/// the last byte with items still owed is no envelope — at a fixed-stride slot,
/// where reading it as grammatical would convict it, and at a walked one.
test committed_field_shape_walk_eight_items_end_on_the_last_byte_with_items_owed() {
  let vectors = [
    #"894040404040404040",
    bytearray.concat(array_head(17), one_byte_run(16)),
    bytearray.concat(#"98ff", one_byte_run(8)),
  ]
  list.all(
    vectors,
    fn(preimage) {
      and {
        walked_verdict(spend_inputs_field_index, preimage) == Pair(
          shape_verdict_not_an_envelope,
          1,
        ),
        walked_verdict(outputs_field_index, preimage) == Pair(
          shape_verdict_not_an_envelope,
          1,
        ),
        agrees_at_every_slot(preimage),
      }
    },
  )
}

test committed_field_shape_walk_trailing_bytes_after_a_resume() {
  walked_verdict(
    spend_inputs_field_index,
    bytearray.concat(one_byte_items(b + 1), #"40"),
  ) == Pair(shape_verdict_not_an_envelope, 2)
}

// ---------------------------------------------------------------------------
// The largest field
// ---------------------------------------------------------------------------

/// 32,765 one-byte items fill §5.4's byte bound exactly: convicted at slot 0
/// and admissible at slot 2, each in `ceil(32,765 / B)` steps.
test committed_field_shape_walk_at_the_maximum_item_count() {
  let preimage = one_byte_items(max_item_count)
  let steps = ( max_item_count + b - 1 ) / b
  and {
    bytearray.length(preimage) == max_transaction_aggregate_field_bytes,
    walked_verdict(spend_inputs_field_index, preimage) == Pair(
      shape_verdict_wrong_stride,
      steps,
    ),
    walked_verdict(outputs_field_index, preimage) == Pair(
      shape_verdict_admissible,
      steps,
    ),
  }
}

/// One item more than the bound admits, at a walked slot: the byte bound, after
/// the whole walk.
test committed_field_shape_walk_one_item_over_the_byte_bound() {
  walked_verdict(outputs_field_index, one_byte_items(max_item_count + 1)) == Pair(
    shape_verdict_field_byte_bound,
    ( max_item_count + b ) / b,
  )
}

/// The same field declaring one item more than it carries.
test committed_field_shape_walk_at_the_maximum_item_count_one_item_short() {
  walked_verdict(
    spend_inputs_field_index,
    bytearray.concat(
      array_head(max_item_count + 1),
      one_byte_run(max_item_count),
    ),
  ).1st == shape_verdict_not_an_envelope
}

// ---------------------------------------------------------------------------
// Equivalence with the single-evaluation verdict
// ---------------------------------------------------------------------------

/// Every fixed vector above, and the worst batch, at every slot.
test committed_field_shape_walk_agrees_with_one_evaluation_across_batches() {
  list.all(
    [
      one_byte_items(b - 1),
      one_byte_items(b),
      one_byte_items(b + 1),
      one_byte_items(2 * b + 1),
      one_byte_items_with(b + 1, 0, #"00"),
      one_byte_items_with(b + 1, b - 1, #"00"),
      one_byte_items_with(2 * b + 1, b, #"00"),
      one_byte_items_with(2 * b + 1, 2 * b - 1, #"5805"),
      bytearray.concat(array_head(b + 1), one_byte_run(b)),
      bytearray.concat(one_byte_items(b + 1), #"40"),
      stride_items(40, 819),
      stride_items(40, 820),
      stride_items(30, b + 1),
      stride_items(103, 300),
      worst_batch_preimage(),
    ],
    agrees_at_every_slot,
  )
}

/// The largest field and its neighbours, at one slot of each stride (§5.3's
/// four: 40, walked, 30 and 103).
test committed_field_shape_walk_agrees_with_one_evaluation_at_the_largest_field() {
  list.all(
    [
      one_byte_items(max_item_count),
      one_byte_items(max_item_count + 1),
      bytearray.concat(
        array_head(max_item_count + 1),
        one_byte_run(max_item_count),
      ),
    ],
    fn(preimage) { agrees_at([0, 2, 3, 7], preimage) },
  )
}

/// Byte strings built from the pieces §5.1's decisions and the slots' strides
/// turn on, behind an array head that declares the item count the body
/// carries, or a few more or fewer.
fn shaped_field() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.both(
      fuzz.list_between(
        fuzz.one_of(
          [
            #"40",
            #"41aa",
            #"5817",
            #"58",
            #"59",
            #"590100",
            #"00",
            #"5a",
            #"5818000000000000000000000000000000000000000000000000",
            bytearray.concat(#"581c", fixture.repeated_bytes_v1(#"00", 28)),
            bytearray.concat(#"5826", fixture.repeated_bytes_v1(#"00", 38)),
            bytearray.concat(#"5865", fixture.repeated_bytes_v1(#"00", 101)),
          ],
        ),
        0,
        30,
      ),
      fuzz.one_of([-1, 0, 0, 0, 1, 2, 9]),
    ),
    fn(pieces_and_extra) {
      let (pieces, extra) = pieces_and_extra
      let declared = list.length(pieces) + extra
      bytearray.concat(
        if declared < 0 {
          #""
        } else {
          array_head(declared)
        },
        list.foldr(pieces, #"", bytearray.concat),
      )
    },
  )
}

test committed_field_shape_walk_agrees_with_one_evaluation(
  preimage via shaped_field(),
) {
  agrees_at_every_slot(preimage)
}

/// The same comparison over byte strings with no array head guaranteed, so the
/// header refusals are reached too.
test committed_field_shape_walk_agrees_with_one_evaluation_on_any_bytes(
  preimage via fuzz.bytearray_between(0, 24),
) {
  agrees_at_every_slot(preimage)
}
