//// The batched §5.1 walk step 01 takes one transaction at a time
//// (`docs/spec/midgard-tx.md` §12.7).
////
//// Three things are pinned here.
////
//// 1. **Batch boundaries.** A field one item under, exactly at and one item
////    over `envelope_batch_items`, and a bad item at the first and at the last
////    position of a batch, each earn the verdict a whole-field reading earns,
////    in the number of batches the count implies.
//// 2. **The largest field.** §5.4's 32,768-byte bound filled with one-byte
////    items is the largest item count any carriage reaches, and it walks to
////    `verdict_grammatical` in `ceil(32,765 / B)` batches.
//// 3. **Agreement with a plain reading of §5.1.** A fuzzed comparison against
////    `plain_envelope_verdict`, a direct transcription of the grammar one item
////    at a time, kept here as the test's oracle.

use aiken/collection/list
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/fraud_proofs/canonical_decodability/rule.{
  EnvelopeItems, EnvelopeVerdict, EnvelopeWalkV1, envelope_array_head_v1,
  envelope_batch_items, envelope_first_batch_v1, envelope_items_batch_v1,
  envelope_verdict_v1, verdict_grammatical, verdict_missing_array_header,
  verdict_missing_item_header, verdict_non_minimal_array_header,
  verdict_non_minimal_item_header, verdict_not_an_array_header,
  verdict_not_an_item_header, verdict_trailing_bytes,
  verdict_truncated_array_header, verdict_truncated_item_header,
  verdict_truncated_item_payload,
}
use midgard/fraud_proofs/canonical_decodability/walk_fixture.{
  array_head, one_byte_items, one_byte_run, worst_batch_preimage,
}
use midgard/native_tx_field_access_v1.{max_transaction_aggregate_field_bytes}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Every batch the walk takes, run to the end: the verdict and how many
/// batches (transactions) it took.
fn walk_to_verdict(preimage: ByteArray) -> Pair<Int, Int> {
  continue_walk(preimage, envelope_first_batch_v1(preimage), 1)
}

fn continue_walk(
  preimage: ByteArray,
  walk: EnvelopeWalkV1,
  batches: Int,
) -> Pair<Int, Int> {
  when walk is {
    EnvelopeVerdict { verdict } -> Pair(verdict, batches)
    EnvelopeItems { remaining, offset } ->
      continue_walk(
        preimage,
        envelope_items_batch_v1(preimage, remaining, offset),
        batches + 1,
      )
  }
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

const b: Int = envelope_batch_items

/// The largest field §5.4 admits holds this many one-byte items under a
/// three-byte array head.
const max_item_count: Int = max_transaction_aggregate_field_bytes - 3

// ---------------------------------------------------------------------------
// Batch boundaries
// ---------------------------------------------------------------------------

test canonical_decodability_walk_one_item_under_a_batch_is_one_step() {
  walk_to_verdict(one_byte_items(b - 1)) == Pair(verdict_grammatical, 1)
}

test canonical_decodability_walk_exactly_a_batch_is_one_step() {
  walk_to_verdict(one_byte_items(b)) == Pair(verdict_grammatical, 1)
}

/// One item over: the first batch stops with exactly one item owed at the
/// byte after the first `B` items, and the second settles it.
test canonical_decodability_walk_one_item_over_a_batch_resumes_once() {
  let preimage = one_byte_items(b + 1)
  and {
    envelope_first_batch_v1(preimage) == EnvelopeItems {
      remaining: 1,
      offset: 3 + b,
    },
    walk_to_verdict(preimage) == Pair(verdict_grammatical, 2),
  }
}

/// Trailing bytes are seen only by the batch that reads the last item.
test canonical_decodability_walk_trailing_bytes_after_a_resume() {
  let preimage = bytearray.concat(one_byte_items(b + 1), #"40")
  walk_to_verdict(preimage) == Pair(verdict_trailing_bytes, 2)
}

/// A bad head at the first position of the first batch.
test canonical_decodability_walk_bad_item_first_in_the_first_batch() {
  walk_to_verdict(one_byte_items_with(b + 1, 0, #"00")) == Pair(
    verdict_not_an_item_header,
    1,
  )
}

/// A bad head at the last position of the first batch: still decided by the
/// first batch, not deferred to a successor.
test canonical_decodability_walk_bad_item_last_in_the_first_batch() {
  walk_to_verdict(one_byte_items_with(b + 1, b - 1, #"00")) == Pair(
    verdict_not_an_item_header,
    1,
  )
}

/// A bad head at the first position of the second batch.
test canonical_decodability_walk_bad_item_first_in_a_resumed_batch() {
  let preimage = one_byte_items_with(2 * b + 1, b, #"00")
  and {
    envelope_first_batch_v1(preimage) == EnvelopeItems {
      remaining: b + 1,
      offset: 3 + b,
    },
    walk_to_verdict(preimage) == Pair(verdict_not_an_item_header, 2),
  }
}

/// A bad head at the last position of the second batch.
test canonical_decodability_walk_bad_item_last_in_a_resumed_batch() {
  walk_to_verdict(one_byte_items_with(2 * b + 1, 2 * b - 1, #"5805")) == Pair(
    verdict_non_minimal_item_header,
    2,
  )
}

/// A payload that runs off the end at the last position of a batch with items
/// still owed is the truncated payload, not a resume.
test canonical_decodability_walk_payload_overrun_at_a_batch_end() {
  let preimage = bytearray.concat(
    array_head(b + 1),
    bytearray.concat(one_byte_run(b - 1), #"42ff"),
  )
  walk_to_verdict(preimage) == Pair(verdict_truncated_item_payload, 1)
}

/// A batch that reads its full share and lands on the last byte with items
/// still owed is the missing item header at once.
test canonical_decodability_walk_preimage_ends_at_a_batch_end_with_items_owed() {
  let preimage = bytearray.concat(array_head(b + 1), one_byte_run(b))
  walk_to_verdict(preimage) == Pair(verdict_missing_item_header, 1)
}

/// The last byte of the preimage is an item head: a one-byte head with no
/// payload is an item, a wider head is a truncated one.
test canonical_decodability_walk_item_head_in_the_last_byte() {
  and {
    envelope_verdict_v1(#"8140") == verdict_grammatical,
    envelope_verdict_v1(#"8141") == verdict_truncated_item_payload,
    envelope_verdict_v1(#"8158") == verdict_truncated_item_header,
    envelope_verdict_v1(#"8159") == verdict_truncated_item_header,
    envelope_verdict_v1(#"815901") == verdict_truncated_item_header,
    envelope_verdict_v1(#"815a") == verdict_not_an_item_header,
    envelope_verdict_v1(#"8100") == verdict_not_an_item_header,
    envelope_verdict_v1(#"824040") == verdict_grammatical,
    envelope_verdict_v1(#"8240") == verdict_missing_item_header,
  }
}

// ---------------------------------------------------------------------------
// The largest field
// ---------------------------------------------------------------------------

/// 32,765 one-byte items fill §5.4's byte bound exactly: the largest item
/// count a field reaches, walked to its verdict in `ceil(32,765 / B)` steps.
test canonical_decodability_walk_at_the_maximum_item_count() {
  let preimage = one_byte_items(max_item_count)
  and {
    bytearray.length(preimage) == max_transaction_aggregate_field_bytes,
    walk_to_verdict(preimage) == Pair(
      verdict_grammatical,
      ( max_item_count + b - 1 ) / b,
    ),
  }
}

/// The same field with one more item declared than it carries.
test canonical_decodability_walk_at_the_maximum_item_count_one_item_short() {
  let preimage = bytearray.concat(
    array_head(max_item_count + 1),
    one_byte_run(max_item_count),
  )
  walk_to_verdict(preimage).1st == verdict_missing_item_header
}

/// The worst batch a 32,768-byte field can present: every item at the two-byte
/// head, and as many three-byte heads as the remaining bytes pay for. It is
/// the shape the step-01 worst-batch budget row is read at.
test canonical_decodability_walk_worst_batch_shape_is_one_full_batch() {
  let preimage = worst_batch_preimage()
  and {
    bytearray.length(preimage) <= max_transaction_aggregate_field_bytes,
    max_transaction_aggregate_field_bytes - bytearray.length(preimage) < 233,
    envelope_array_head_v1(preimage) == EnvelopeItems {
      remaining: b,
      offset: 3,
    },
    walk_to_verdict(preimage) == Pair(verdict_grammatical, 1),
  }
}

// ---------------------------------------------------------------------------
// Agreement with a plain reading of §5.1
// ---------------------------------------------------------------------------

/// §5.1 read one item at a time with every bound checked before its read: the
/// oracle the batched walk is compared against.
fn plain_envelope_verdict(preimage: ByteArray) -> Int {
  let total_length = bytearray.length(preimage)
  if total_length == 0 {
    verdict_missing_array_header
  } else {
    let tag = bytearray.at(preimage, 0)
    if tag >= 128 && tag <= 151 {
      plain_items(preimage, total_length, 1, tag - 128)
    } else if tag == 152 {
      if total_length < 2 {
        verdict_truncated_array_header
      } else if bytearray.at(preimage, 1) < 24 {
        verdict_non_minimal_array_header
      } else {
        plain_items(preimage, total_length, 2, bytearray.at(preimage, 1))
      }
    } else if tag == 153 {
      if total_length < 3 {
        verdict_truncated_array_header
      } else {
        let count = bytearray.at(preimage, 1) * 256 + bytearray.at(preimage, 2)
        if count <= 0xff {
          verdict_non_minimal_array_header
        } else {
          plain_items(preimage, total_length, 3, count)
        }
      }
    } else {
      verdict_not_an_array_header
    }
  }
}

fn plain_items(
  preimage: ByteArray,
  total_length: Int,
  offset: Int,
  remaining: Int,
) -> Int {
  if remaining == 0 {
    if offset == total_length {
      verdict_grammatical
    } else {
      verdict_trailing_bytes
    }
  } else if offset >= total_length {
    verdict_missing_item_header
  } else {
    let tag = bytearray.at(preimage, offset)
    if tag >= 64 && tag <= 87 {
      plain_payload(preimage, total_length, offset + 1, tag - 64, remaining)
    } else if tag == 88 {
      if offset + 2 > total_length {
        verdict_truncated_item_header
      } else if bytearray.at(preimage, offset + 1) < 24 {
        verdict_non_minimal_item_header
      } else {
        plain_payload(
          preimage,
          total_length,
          offset + 2,
          bytearray.at(preimage, offset + 1),
          remaining,
        )
      }
    } else if tag == 89 {
      if offset + 3 > total_length {
        verdict_truncated_item_header
      } else {
        let length = bytearray.at(preimage, offset + 1) * 256 + bytearray.at(
          preimage,
          offset + 2,
        )
        if length <= 0xff {
          verdict_non_minimal_item_header
        } else {
          plain_payload(preimage, total_length, offset + 3, length, remaining)
        }
      }
    } else {
      verdict_not_an_item_header
    }
  }
}

fn plain_payload(
  preimage: ByteArray,
  total_length: Int,
  payload_offset: Int,
  length: Int,
  remaining: Int,
) -> Int {
  if payload_offset + length > total_length {
    verdict_truncated_item_payload
  } else {
    plain_items(preimage, total_length, payload_offset + length, remaining - 1)
  }
}

/// Byte strings built from the pieces §5.1's decisions turn on: every head
/// width at and either side of its minimal boundary, heads cut short, payload
/// bytes, and bytes that are no head at all.
fn envelope_like() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.list_between(
      fuzz.one_of(
        [
          #"40", #"41", #"42", #"57", #"00", #"ff", #"58", #"5817", #"5818",
          #"58ff", #"59", #"5900", #"5900ff", #"590100", #"5aff", #"80", #"81",
          #"82", #"83", #"97", #"98", #"9817", #"9818", #"99", #"9900ff",
          #"990100", #"9a", #"aa", #"4100", #"420000",
        ],
      ),
      0,
      24,
    ),
    fn(pieces) { list.foldr(pieces, #"", bytearray.concat) },
  )
}

test canonical_decodability_walk_agrees_with_a_plain_reading(
  preimage via envelope_like(),
) {
  envelope_verdict_v1(preimage) == plain_envelope_verdict(preimage)
}

/// The same comparison behind an array head that declares the item count the
/// body carries, so the fuzzer reaches the item walk rather than stopping at
/// the head.
test canonical_decodability_walk_agrees_with_a_plain_reading_behind_a_head(
  pieces via fuzz.list_between(
    fuzz.one_of(
      [
        #"40", #"41aa", #"5818000000000000000000000000000000000000000000000000",
        #"5817", #"58", #"59", #"590100", #"00", #"5a",
      ],
    ),
    0,
    30,
  ),
) {
  let preimage = bytearray.concat(
    array_head(list.length(pieces)),
    list.foldr(pieces, #"", bytearray.concat),
  )
  envelope_verdict_v1(preimage) == plain_envelope_verdict(preimage)
}
