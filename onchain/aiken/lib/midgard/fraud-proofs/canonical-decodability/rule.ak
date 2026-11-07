//// ## `canonical-decodability` — normative rule (`docs/spec/midgard-tx.md` §12.7)
////
//// **Rule.** Under §4 an operator commits `blake2b_256(preimage_i)` for each of
//// §2.5's nine fields, and §5.1 says what a preimage may be: a minimal-width
//// definite array header followed by exactly `N` minimal-width
//// byte-string-wrapped items, ending exactly at the preimage's last byte. A
//// block that commits anything else has committed a field no reader can open.
////
//// **Violation `canonical-decodability`.** A committed preimage that is not a
//// §5.1 envelope. Every §8.8 view door ends in `whole_view`, which settles
//// §5.1 with `expect` — so such a field **aborts** every consumer, including
//// the `CanonicalDecode` phase whose job is to render a verdict about it. No
//// step is producible by anyone and the dispute stalls rather than rejecting:
//// an operator escape hatch, and the one path that phase exists to close. The
//// owner ruling of 2026-08-11 closes it by direct fault rather than by
//// softening the door, and this module is that fault's rule.
////
//// ### Why the check needs no decoder
////
//// The same shape as `da-hash-preimage`'s (GOAL_SPEC.md Q44), one level down.
//// There, the leaf is framed at fixed widths so the body preimage is a slice
//// and the id is a hash of it; here, the preimage is authenticated by §4's flat
//// hash directly — the prover supplies bytes, the door hashes them against the
//// positionally-extracted commitment, and wrong bytes simply fail (§7.3). What
//// the two share is that **nothing is decoded to decide the fault**: the
//// verdict below is a total function of the authenticated bytes, so it has an
//// answer for every byte string rather than an abort for the ones the format
//// does not admit. For an honest field the answer is `verdict_grammatical` and
//// the challenge can never finalize; for a committed non-envelope the same
//// total computation convicts.
////
//// ### The one thing this rule must not do
////
//// It must not reach the verdict through code that aborts. Both view doors
//// materialise a `FieldViewV1` and therefore run §5.1's walk under `expect`;
//// calling either would reproduce the stall inside the family built to end it.
//// `authenticated_committed_preimage` exists for exactly this reason and is the
//// only door entry point this module uses.
////
//// ### The boundary this rule does not cross
////
//// A bad **item** inside a well-formed envelope is a different claim, and §12's
//// `fault_item_predicate` (§12.3) is the one that adjudicates it. This rule
//// reads no item's content, applies no per-field §5.3/§5.5/§5.6 encoder rule,
//// and consults no declared `NativeTxFieldPreimageLengthsV1` length. It asks
//// one question — is this byte string an envelope — and answers it. §12.7
//// states the boundary in full.

use aiken/builtin.{bytearray_to_integer, quotient_integer, slice_bytearray}
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/field_opening_v1.{first_witness_set_field_index}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxWitnessSetCompact, VerifiedTxSourceView,
}
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, authenticated_committed_preimage, field_count,
}

// ---------------------------------------------------------------------------
// The §5.1 verdict codes
// ---------------------------------------------------------------------------

// Ten codes, one per way §5.1 can be left, plus `0` for the grammar being
// satisfied. They are `Int` rather than a sum type because they travel in
// computation-thread state between two transactions and a thread datum is a
// wire form (§10.6's reason, one level out): an off-chain twin has to be able
// to write and read the value, and `demo/midgard-sdk`'s
// `canonical-decodability.ts` mirrors these constants by name.
//
// **They are diagnostic, not load-bearing.** `is_canonical_decodability_violation_v1`
// asks only whether the verdict is `verdict_grammatical`, so a mis-assigned
// non-zero code would convict exactly the same set of blocks. Distinguishing
// them is what makes the family's vectors able to say *which* rule of §5.1 a
// fixture leaves, which is the difference between a suite that pins the grammar
// and one that pins "something was wrong".

/// The bytes are a §5.1 envelope. The only verdict that is not a violation.
pub const verdict_grammatical: Int = 0

/// Zero bytes. There is no array header to read, and §5.1's shortest admissible
/// preimage is the one-byte empty field `80`.
pub const verdict_missing_array_header: Int = 1

/// The leading byte is not a §5.1 `definite_array_header`. This includes CBOR
/// array heads §5.1 deliberately excludes — the four-byte `9a` form is
/// well-formed CBOR and outside the grammar (`decode_field_array_header_at`
/// refuses it too).
pub const verdict_not_an_array_header: Int = 2

/// A `98`/`99` array header whose count would fit a narrower form. §5.1 admits
/// one spelling per count (§6.1).
pub const verdict_non_minimal_array_header: Int = 3

/// A `98`/`99` array header whose own width runs past the end of the preimage.
pub const verdict_truncated_array_header: Int = 4

/// The header declared more items than the preimage has bytes left to start.
pub const verdict_missing_item_header: Int = 5

/// An item's leading byte is not a §5.1 `definite_bytes_header`.
pub const verdict_not_an_item_header: Int = 6

/// A `58`/`59` item header whose length would fit a narrower form.
pub const verdict_non_minimal_item_header: Int = 7

/// A `58`/`59` item header whose own width runs past the end of the preimage.
pub const verdict_truncated_item_header: Int = 8

/// An item whose declared payload runs past the end of the preimage.
pub const verdict_truncated_item_payload: Int = 9

/// The declared items were all read and bytes remain. §5.1 admits no trailing
/// content.
pub const verdict_trailing_bytes: Int = 10

/// One past the largest verdict code. Every value this module returns is in
/// `0 ..< verdict_code_count`, which is what lets a step-02 refuse a state
/// carrying a code no walk produces.
pub const verdict_code_count: Int = 11

// ---------------------------------------------------------------------------
// The verdict — a total function over arbitrary bytes
// ---------------------------------------------------------------------------

/// How many items one step reads, chosen by measurement. The worst batch a
/// 32,768-byte field can present to one step — every item at the two-byte head,
/// with as many three-byte heads as the remaining bytes pay for — read through
/// the whole step-01 validator under tier-3 carriage is the pair of
/// `step_01_walk` budget rows in `demo/midgard-core/src/script-budgets.json`.
pub const envelope_batch_items: Int = 1000

/// Where a §5.1 walk stands: finished with a verdict, or between two batches
/// with `remaining` items still to read from byte `offset`.
///
/// `EnvelopeItems` is only ever produced with `remaining >= 1` and
/// `offset < bytearray.length(preimage)`: an exhausted count or an exhausted
/// preimage is already a verdict.
pub type EnvelopeWalkV1 {
  EnvelopeVerdict { verdict: Int }
  EnvelopeItems { remaining: Int, offset: Int }
}

/// §5.1 grammaticality as a **verdict**, over any byte string at all: the
/// array head, then every batch until one ends the walk.
///
/// **Total by construction, and the property is the point.** There is no
/// `expect`, no `fail` and no partial builtin call on an unchecked index here
/// or in anything it calls: every byte below is read either behind the bound
/// that makes it safe or through `slice_bytearray`, which answers an
/// out-of-range read with no bytes rather than an abort. A verdict that could
/// abort would be the door again, and the door is what this family exists to
/// route around.
///
/// **It is the door's grammar, restated as a decision rather than reused as
/// one.** `decode_field_array_header_at`, `item_header_at` and `walk_to_end`
/// decide exactly this predicate in `native_tx_field_access_v1`, and they decide
/// it by aborting. This function agrees with them on every input — the agreement
/// is asserted in both directions in `rule.test.ak` and, at the seam that
/// matters, by the validator selectors that convict a preimage the door refuses
/// to open. Restating rather than reusing is deliberate and is the one
/// duplication §6.1's one-verdict rule tolerates here, because the alternative
/// is a shared reader that returns an option, which would put a silently
/// clampable `None` on the door's own hot path.
///
/// Step 01 takes the same walk one batch per transaction; this is that walk
/// run to its end in one evaluation, for callers whose fields are small enough
/// to afford it.
pub fn envelope_verdict_v1(preimage: ByteArray) -> Int {
  run_envelope_walk(preimage, envelope_first_batch_v1(preimage))
}

/// The walk's first batch: the array head, then up to `envelope_batch_items`
/// items — what step 01 reads on the thread's first transaction.
pub fn envelope_first_batch_v1(preimage: ByteArray) -> EnvelopeWalkV1 {
  when envelope_array_head_v1(preimage) is {
    EnvelopeItems { remaining, offset } ->
      envelope_items_batch_v1(preimage, remaining, offset)
    verdict -> verdict
  }
}

fn run_envelope_walk(preimage: ByteArray, walk: EnvelopeWalkV1) -> Int {
  when walk is {
    EnvelopeVerdict { verdict } -> verdict
    EnvelopeItems { remaining, offset } ->
      run_envelope_walk(
        preimage,
        envelope_items_batch_v1(preimage, remaining, offset),
      )
  }
}

/// §5.1's `definite_array_header(N)`, read at offset 0. Every way the header
/// itself can be wrong is a verdict here; otherwise the walk starts at the
/// first item with `N` items to read — including `N = 0`, which the first
/// batch settles.
pub fn envelope_array_head_v1(preimage: ByteArray) -> EnvelopeWalkV1 {
  let total_length = bytearray.length(preimage)
  if total_length == 0 {
    EnvelopeVerdict(verdict_missing_array_header)
  } else {
    let tag = bytearray.at(preimage, 0)
    if and {
      tag >= 128,
      tag <= 151,
    } {
      // `80 + N` for N <= 23. The header is one byte and is already read.
      EnvelopeItems { remaining: tag - 128, offset: 1 }
    } else if tag == 152 {
      if total_length < 2 {
        EnvelopeVerdict(verdict_truncated_array_header)
      } else {
        let count = bytearray.at(preimage, 1)
        if count < 24 {
          EnvelopeVerdict(verdict_non_minimal_array_header)
        } else {
          EnvelopeItems { remaining: count, offset: 2 }
        }
      }
    } else if tag == 153 {
      if total_length < 3 {
        EnvelopeVerdict(verdict_truncated_array_header)
      } else {
        let count = bytearray.at(preimage, 1) * 256 + bytearray.at(preimage, 2)
        if count <= 0xff {
          EnvelopeVerdict(verdict_non_minimal_array_header)
        } else {
          EnvelopeItems { remaining: count, offset: 3 }
        }
      }
    } else {
      EnvelopeVerdict(verdict_not_an_array_header)
    }
  }
}

/// Reads up to `envelope_batch_items` of the `remaining` items from `offset`
/// and says where the walk stands after them.
///
/// The two ways to leave §5.1 that only the *end* of the walk can see are both
/// decided here: items that run out of preimage (`verdict_missing_item_header`,
/// and the truncation codes) and preimage that runs out of items
/// (`verdict_trailing_bytes`). A batch that reads its full share and lands
/// exactly on the last byte with items still owed is the missing item header
/// at once rather than one transaction later.
pub fn envelope_items_batch_v1(
  preimage: ByteArray,
  remaining: Int,
  offset: Int,
) -> EnvelopeWalkV1 {
  let total_length = bytearray.length(preimage)
  if remaining > envelope_batch_items {
    let reached = item_heads_verdict_in(
      preimage,
      total_length,
      offset,
      envelope_batch_items,
    )
    if reached < total_length {
      EnvelopeItems {
        remaining: remaining - envelope_batch_items,
        offset: reached,
      }
    } else if reached == total_length {
      EnvelopeVerdict(verdict_missing_item_header)
    } else {
      EnvelopeVerdict(stopped_walk_verdict(reached))
    }
  } else {
    let reached = item_heads_verdict_in(
      preimage,
      total_length,
      offset,
      remaining,
    )
    if reached == total_length {
      EnvelopeVerdict(verdict_grammatical)
    } else if reached < total_length {
      EnvelopeVerdict(verdict_trailing_bytes)
    } else {
      EnvelopeVerdict(stopped_walk_verdict(reached))
    }
  }
}

// ---------------------------------------------------------------------------
// The item-head walk
// ---------------------------------------------------------------------------

// A walk position is one `Int` with two readings, so that the hot loop
// allocates nothing and tests nothing between items:
//
// - `0 ..= total_length + 65_538`: a byte offset. Past `total_length` it is an
//   item payload that ran off the end of the preimage, which every later read
//   sees as "no bytes left" and which ends the walk as
//   `verdict_truncated_item_payload`.
// - `stopped_walk_base + code`: the walk stopped on `code`. A stopped position
//   is also past the end of every preimage, so every later item read keeps it
//   unchanged.
//
// That is what lets eight item heads be read back to back with one check
// after them instead of one after each.

/// Larger than any offset a preimage carried by an L1 transaction can reach.
const stopped_walk_base: Int = 4_294_967_296

fn stopped_walk_verdict(reached: Int) -> Int {
  if reached >= stopped_walk_base {
    reached - stopped_walk_base
  } else {
    verdict_truncated_item_payload
  }
}

/// One §5.1 `definite_bytes_header` and its payload, read at `offset`.
///
/// Returns the offset just past the item's payload, or a stopped position (see
/// above). Reads go through `slice_bytearray`, so a read at or past the end of
/// the preimage returns fewer bytes rather than aborting, and every such case
/// is resolved by `item_head_at_end`. The common heads are decided from the
/// first two bytes at once:
///
/// - `0x4000 ..< 0x5800`: `40 + L`, a one-byte head; the item ends `L` bytes
///   after it.
/// - `0x5800 ..< 0x5900`: `58 LL`; minimal only for `LL >= 24`.
/// - `0x5900 ..< 0x5a00`: `59 LLLL`, read again three bytes wide; minimal only
///   for `LLLL >= 256`.
/// - anything at or above `0x5a00` is not an item head.
pub fn item_head_verdict_in(
  preimage: ByteArray,
  total_length: Int,
  offset: Int,
) -> Int {
  let head = bytearray_to_integer(True, slice_bytearray(offset, 2, preimage))
  if head < 0x5800 {
    if head >= 0x4000 {
      offset + quotient_integer(head, 256) - 63
    } else {
      item_head_at_end(preimage, total_length, offset)
    }
  } else if head < 0x5900 {
    if head < 0x5818 {
      stopped_walk_base + verdict_non_minimal_item_header
    } else {
      // offset + 2 + (head - 0x5800)
      offset + head - 0x57fe
    }
  } else if head < 0x5a00 {
    let wide_head = bytearray_to_integer(
      True,
      slice_bytearray(offset, 3, preimage),
    )
    if wide_head < 0x590100 {
      if wide_head < 0x10000 {
        // Fewer than three bytes were left to read.
        stopped_walk_base + verdict_truncated_item_header
      } else {
        stopped_walk_base + verdict_non_minimal_item_header
      }
    } else {
      // offset + 3 + (wide_head - 0x590000)
      offset + wide_head - 0x58fffd
    }
  } else {
    item_head_at_end(preimage, total_length, offset)
  }
}

/// The reads `item_head_verdict_in` cannot settle from two bytes: an item head
/// in the preimage's last byte, a position at or past its end, and a first
/// byte that is no item head at all.
fn item_head_at_end(preimage: ByteArray, total_length: Int, offset: Int) -> Int {
  if offset + 1 == total_length {
    let tag = bytearray.at(preimage, offset)
    if tag < 64 {
      stopped_walk_base + verdict_not_an_item_header
    } else if tag < 88 {
      offset + tag - 63
    } else if tag < 90 {
      stopped_walk_base + verdict_truncated_item_header
    } else {
      stopped_walk_base + verdict_not_an_item_header
    }
  } else if offset < total_length {
    stopped_walk_base + verdict_not_an_item_header
  } else if offset == total_length {
    stopped_walk_base + verdict_missing_item_header
  } else if offset < stopped_walk_base {
    stopped_walk_base + verdict_truncated_item_payload
  } else {
    offset
  }
}

/// `count` item heads from `offset`, eight at a time. Returns the position
/// after the last one (see above); a stopped or overrun position ends the walk
/// at the next eight-item boundary at the latest.
fn item_heads_verdict_in(
  preimage: ByteArray,
  total_length: Int,
  offset: Int,
  count: Int,
) -> Int {
  if count >= 8 {
    let reached = item_head_verdict_in(
      preimage,
      total_length,
      item_head_verdict_in(
        preimage,
        total_length,
        item_head_verdict_in(
          preimage,
          total_length,
          item_head_verdict_in(
            preimage,
            total_length,
            item_head_verdict_in(
              preimage,
              total_length,
              item_head_verdict_in(
                preimage,
                total_length,
                item_head_verdict_in(
                  preimage,
                  total_length,
                  item_head_verdict_in(preimage, total_length, offset),
                ),
              ),
            ),
          ),
        ),
      ),
    )
    if reached > total_length {
      reached
    } else {
      item_heads_verdict_in(preimage, total_length, reached, count - 8)
    }
  } else if count == 0 {
    offset
  } else {
    let reached = item_head_verdict_in(preimage, total_length, offset)
    if reached > total_length {
      reached
    } else {
      item_heads_verdict_in(preimage, total_length, reached, count - 1)
    }
  }
}

// ---------------------------------------------------------------------------
// The claim, and the verdict over an authenticated field
// ---------------------------------------------------------------------------

/// What a prover claims and carries: **which** of §2.5's nine slots is accused,
/// and how that slot's committed bytes reach the step (§8's ladder).
///
/// The §2.5 split is structural for `field_opening_v1`'s reason. Fields 0–5 are
/// committed by the body and the door consults no witness set at all; fields 6–8
/// live in the witness set and the door will only read one that re-derives to
/// the compact structure's committed `witness_set_hash`. Two constructors rather
/// than one optional member means a claim cannot carry a witness set the door
/// would ignore and cannot omit one the door silently needs — either would be a
/// second spelling of one accusation (§6.1).
///
/// **The field index is a redeemer argument here, and that is not the hazard it
/// is at a read site.** `field_opening_v1` insists the index be a call-site
/// literal because a step that means to read field 0 must not be steerable onto
/// field 1. This step reads nothing: the index *is* the accusation's address
/// (§12.1), so it has to be free. It is safe because the door extracts the
/// expected commitment for the slot the index names and refuses any bytes that
/// do not hash to it — so a verdict rendered under index `i` is a verdict about
/// the bytes slot `i` committed, whichever `i` the prover chose. §4's field-hash
/// aliasing (fields 0/1 and 3/4 commit identically for identical content) does
/// not weaken that: aliased slots commit the *same bytes*, so a non-envelope
/// convicted under either index is a non-envelope under both.
///
/// Constructor order is wire format: `BodyFieldClaim` is Constr 0,
/// `WitnessFieldClaim` Constr 1.
pub type CommittedFieldClaimV1 {
  /// §2.5 fields 0–5.
  BodyFieldClaim { field_index: Int, carriage: FieldCarriageV1 }
  /// §2.5 fields 6–8. `witness_set` is unauthenticated on arrival and is
  /// checked against the block-committed `witness_set_hash` inside the door.
  WitnessFieldClaim {
    field_index: Int,
    witness_set: NativeTxWitnessSetCompact,
    carriage: FieldCarriageV1,
  }
}

/// The witness set handed to the door for a body field.
///
/// The door short-circuits before looking at it for `field_index < 6`, so the
/// unread value is constructed *here* rather than taken from the claim — there
/// is no argument for a prover to fill, and nothing can ride in under a member
/// the door will not read. Same idiom, same reason, as
/// `field_opening_v1.unread_witness_set`; it is not shared because that one is
/// private to a module this rule must not otherwise depend on for reads.
fn unread_witness_set() -> NativeTxWitnessSetCompact {
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: #"",
    script_tx_wits_hash: #"",
    redeemer_tx_wits_hash: #"",
  }
}

/// Authenticate the claimed slot's committed preimage against §4's commitment
/// and return the slot with its bytes.
///
/// `verified` is the compact structure the block's counted `transactions_root`
/// committed — a family step-01 holds it already authenticated from
/// `pass_native_tx_to_next_step`, which is what makes the trailing
/// `witness_set_hash` inside it a *committed* value rather than the prover's
/// (see `field_opening_v1`'s module header for why that distinction decides
/// whether fields 6–8 are reachable at all).
///
/// The §2.5 half the claim was written for is asserted against the half its
/// index lives in, before any byte is read. Both directions are refused: a body
/// claim at a witness-set index would hand the door the placeholder above as if
/// it were the transaction's witness set, and a witness claim at a body index
/// would carry one the door discards.
///
/// The bytes come back undecided. Every step of the walk calls this, so every
/// batch reads bytes the door has just held to the commitment, and nothing
/// about them has to be carried from one transaction to the next.
pub fn authenticated_claim_preimage_v1(
  verified: VerifiedTxSourceView,
  claim: CommittedFieldClaimV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<Int, ByteArray> {
  when claim is {
    BodyFieldClaim { field_index, carriage } -> {
      expect field_index < first_witness_set_field_index
      Pair(
        field_index,
        authenticated_committed_preimage(
          verified,
          unread_witness_set(),
          field_index,
          carriage,
          reference_inputs,
          certificate_policy_id,
        ),
      )
    }
    WitnessFieldClaim { field_index, witness_set, carriage } -> {
      expect field_index >= first_witness_set_field_index
      Pair(
        field_index,
        authenticated_committed_preimage(
          verified,
          witness_set,
          field_index,
          carriage,
          reference_inputs,
          certificate_policy_id,
        ),
      )
    }
  }
}

/// The adjudicated violation predicate, stated over the two values step-01 pins
/// into the computation thread.
///
/// The shape check is not decoration. A thread datum is written by step-01 and
/// read by step-02 across a transaction boundary, and §12.1's rule that every
/// scalar a code carries has exactly one spelling applies to it: a state naming
/// a field outside 0..8, or carrying a code no walk produces, is a state
/// step-01 could not have written, and re-checking it at the adjudication is
/// what stops the pair `(field_index, verdict)` from being two accusations that
/// finalize identically. Both bounds are refusals rather than clamps (§7.3).
pub fn is_canonical_decodability_violation_v1(
  field_index: Int,
  verdict: Int,
) -> Bool {
  expect field_index >= 0
  expect field_index < field_count
  expect verdict >= 0
  expect verdict < verdict_code_count
  verdict != verdict_grammatical
}

// ---------------------------------------------------------------------------
// Producer-side helper — the §5.1 envelope, built to be broken
// ---------------------------------------------------------------------------

/// `encode_field_preimage`'s deliberately-wrong twin: an envelope whose array
/// header declares `declared_count` while the body actually carries `items`.
///
/// It exists because the honest producer cannot express the fixtures this
/// family adjudicates. `encode_field_preimage` takes a list and writes the
/// count it measures, so no argument to it produces a miscounted envelope —
/// which is the shape §5.1 refuses at `walk_to_end` and the shape a malicious
/// operator commits. This is the only place in the tree that writes a §5.1
/// header disagreeing with its own content, and it is `pub` so that the
/// cross-language vectors and both step modules build their fixtures from one
/// construction rather than three hand-written byte strings.
pub fn miscounted_field_preimage_v1(
  declared_count: Int,
  items: List<ByteArray>,
) -> ByteArray {
  bytearray.concat(
    midgard_field_array_header(declared_count),
    list.foldr(
      items,
      #"",
      fn(item, encoded_tail) {
        bytearray.concat(midgard_definite_bytes_header(item), encoded_tail)
      },
    ),
  )
}

/// §5.1's `definite_array_header(N)` in minimal width, written without the
/// bounds `encode_field_array_header` enforces so a fixture may declare a count
/// its content does not carry.
fn midgard_field_array_header(count: Int) -> ByteArray {
  if count <= 23 {
    bytearray.push(#"", 128 + count)
  } else if count <= 255 {
    bytearray.concat(#"98", bytearray.from_int_big_endian(count, 1))
  } else {
    bytearray.concat(#"99", bytearray.from_int_big_endian(count, 2))
  }
}

/// §5.1's `definite_bytes_header(L)` in minimal width, prefixed to `item`.
fn midgard_definite_bytes_header(item: ByteArray) -> ByteArray {
  let length = bytearray.length(item)
  if length <= 23 {
    bytearray.concat(bytearray.push(#"", 64 + length), item)
  } else if length <= 255 {
    bytearray.concat(
      bytearray.concat(#"58", bytearray.from_int_big_endian(length, 1)),
      item,
    )
  } else {
    bytearray.concat(
      bytearray.concat(#"59", bytearray.from_int_big_endian(length, 2)),
      item,
    )
  }
}
