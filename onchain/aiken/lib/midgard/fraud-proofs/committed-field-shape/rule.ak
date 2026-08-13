//// ## `committed-field-shape` — normative rule (`docs/spec/midgard-tx.md` §12.8)
////
//// **Rule.** §12.7 closed the stall for a committed preimage that is not a §5.1
//// envelope. It could not close two others, and said so in its own residue
//// paragraph: `whole_view` refuses more than §5.1's grammar, and two of its
//// other `expect`s abort on bytes that *are* a §5.1 envelope.
////
//// **Violation `committed-field-shape`.** The committed preimage of field `i` is
//// a §5.1 envelope that slot `i`'s own §7.4/§5.4 rules refuse:
////
//// - **§7.4 fixed-stride arithmetic.** For fields 0, 1, 3, 4 and 7 the door
////   settles count consistency as `header_len + stride·N == total_length`
////   rather than by walking. An envelope whose items are not the slot's stride
////   satisfies §12.7's verdict — code 0 — and still aborts the door.
//// - **§5.4 per-field byte bound.** `whole_view` refuses a `total_length` above
////   `max_transaction_aggregate_field_bytes`, and an oversize but otherwise
////   well-formed envelope is likewise §12.7 verdict 0 and still aborts.
////
//// Both leave the identical stall §12.7 exists to end: the door aborts, no
//// `CanonicalDecode` step is producible by anyone, the dispute stalls rather
//// than rejecting, and the operator has bought the outcome by committing bytes
//// nobody can open. The owner ruling of 2026-08-13 closes them by option (a) of
//// §12.7's own two — a **sibling** fault kind stated over `(field_index,
//// preimage)` — and this module is that fault's rule.
////
//// ### Why a sibling and not a widening
////
//// §12.7's verdict is a function of the bytes **alone**, and that is exactly
//// what buys its disjointness against §12.3: a section that applies no
//// per-field rule cannot be accused of adjudicating an item. Both shapes above
//// are functions of the bytes **and the slot** — a stride is a property of the
//// field index, and so is the bound a `field_index`-addressed accusation must
//// name. Folding them into §12.7 would have made that section field-index
//// dependent and destroyed the boundary it rests on. Stating them one section
//// over costs one more fault kind and keeps both boundaries exact.
////
//// ### The two boundaries this rule does not cross
////
//// **Against §12.7 (bytes-only).** This rule convicts only bytes §12.7's
//// verdict calls *grammatical*. `shape_verdict_not_an_envelope` is the code it
//// renders for everything else, and that code does not convict: a committed
//// non-envelope is §12.7's fault and is refused at this family's adjudication,
//// not merely absent from it. The two fault kinds therefore partition the
//// committed byte strings a door refuses, and no field is faultable under both.
////
//// **Against §12.3 (`fault_item_predicate`).** This rule reads no item's
//// content, applies no §5.3/§5.5/§5.6 encoder rule, and consults no declared
//// `NativeTxFieldPreimageLengthsV1` length. It asks one question — would slot
//// `i`'s door open this envelope — and answers it. A bad *item* inside an
//// envelope the door does open is §12.3's, and stays §12.3's: the door opens
//// it, so there is no construction-time stall here to remove.
////
//// ### The one thing this rule must not do
////
//// It must not reach the verdict through code that aborts. Both view doors
//// materialise a `FieldViewV1` and therefore run the very `expect`s under
//// accusation; calling either would reproduce the stall inside the family built
//// to end it. `authenticated_committed_preimage` — §12.7's non-aborting reader,
//// reused unchanged — is the only door entry point this module uses.

use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/canonical_decodability/rule.{
  BodyFieldClaim, CommittedFieldClaimV1, WitnessFieldClaim, envelope_verdict_v1,
  verdict_grammatical,
}
use midgard/fraud_proofs/field_opening_v1.{first_witness_set_field_index}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxWitnessSetCompact, VerifiedMidgardNativeTxCompact,
}
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, authenticated_committed_preimage, encode_field_preimage,
  field_count, field_stride, max_transaction_aggregate_field_bytes,
  walk_derived_stride,
}

// ---------------------------------------------------------------------------
// The verdict codes
// ---------------------------------------------------------------------------

// Four codes: two that do not convict and two that do. They are `Int` rather
// than a sum type for §12.7's reason one level out — they travel in computation
// thread state between two transactions and a thread datum is a wire form, so an
// off-chain twin has to write and read the value, and `demo/midgard-sdk`'s
// `committed-field-shape-v1.ts` mirrors these constants by name.
//
// Unlike §12.7's, **these codes are load-bearing in one place**: the two
// non-convicting codes are non-convicting for different reasons, and the second
// of them is this family's boundary against §12.7. Confusing them would either
// convict a field §12.7 owns or leave one of this section's own shapes unfiled.

/// Slot `field_index`'s door opens these bytes: the envelope is inside §5.4's
/// byte bound and, at a fixed-stride slot, its items are the slot's stride. The
/// honest answer, and not a violation.
pub const shape_verdict_admissible: Int = 0

/// The bytes are not a §5.1 envelope. **§12.7's fault, not this one's**, and
/// this code is where the two fault kinds are held apart: it is rendered rather
/// than convicted, and this family's adjudication refuses it.
pub const shape_verdict_not_an_envelope: Int = 1

/// §5.4. A §5.1 envelope whose `total_length` is above
/// `max_transaction_aggregate_field_bytes`. Checked before the stride, because
/// that is the order `whole_view` refuses in and one committed field must have
/// exactly one accusation (§12.1).
pub const shape_verdict_field_byte_bound: Int = 2

/// §7.4. A §5.1 envelope at one of the five fixed-stride slots whose
/// `header_len + stride·N` is not its `total_length`.
pub const shape_verdict_wrong_stride: Int = 3

/// One past the largest verdict code. Every value this module returns is in
/// `0 ..< shape_verdict_code_count`, which is what lets a step-02 refuse a state
/// carrying a code no verdict produces.
pub const shape_verdict_code_count: Int = 4

// ---------------------------------------------------------------------------
// The verdict — total over arbitrary bytes at each of §2.5's nine slots
// ---------------------------------------------------------------------------

/// Slot `field_index`'s §7.4/§5.4 shape rules, as a **verdict** over any byte
/// string at all.
///
/// **Total over the bytes, and the property is the point.** There is no
/// `expect`, no `fail` and no partial builtin call on an unchecked index below:
/// `bytearray.at` aborts out of range, so every read is preceded by the bound
/// that makes it safe, and the bound's failure is a *return value*. A verdict
/// that could abort on operator-committed bytes would be the door again, and the
/// door is what this family exists to route around.
///
/// **The field index is bounded rather than decided over, and the asymmetry is
/// deliberate.** `field_stride` aborts outside §2.5's nine slots, and that abort
/// is this function's index bound. The two arguments have different owners: the
/// preimage is the *operator's* and an abort on it is the stall under
/// adjudication, while the index is the *prover's* and §7.3 says an abort is the
/// correct answer to a prover supplying something outside the format. Every
/// caller reaches this function through `authenticated_committed_preimage`,
/// which bounds the index in `field_door_prologue` before a byte is read.
///
/// **The order of the three questions is `whole_view`'s order**, and that is a
/// §12.1 obligation rather than a style choice. A committed field must have one
/// accusation, not two: an oversize envelope at a fixed-stride slot whose stride
/// also fails is convicted as the byte-bound violation alone, because the byte
/// bound is the `expect` the door reaches first and the one an honest reader of
/// the block would name.
///
/// **§12.7's verdict is the guard, not a re-statement of it.** The single §5.1
/// decision procedure lives in `canonical_decodability/rule.envelope_verdict_v1`
/// and is imported rather than restated: two spellings of §5.1 would be exactly
/// the drift §6.1 forbids, and the disjointness of the two fault kinds is only
/// as good as their agreement on what an envelope is. This family adds no
/// grammar of its own; it adds the two questions the grammar cannot ask.
///
/// **What is claimed about the door, and what is not.** A convicting verdict
/// implies `whole_view` aborts on the same bytes at the same slot, and
/// `shape_verdict_admissible` implies it opens them. The converse of the second
/// is deliberately not claimed: at a fixed-stride slot `whole_view` settles §5.1
/// by arithmetic and never walks the items (§7.4 item 2 puts per-item wrapper
/// canonicality at the accessor), so bytes whose stride arithmetic holds while
/// their item heads are not §5.1's open the door and earn
/// `shape_verdict_not_an_envelope` here. That gap is fail-safe by construction
/// rather than by luck — the deferring code does not convict, so the
/// disagreement can only ever leave a field to §12.7, never take one from it —
/// and `rule.test.ak` pins it as unclaimed rather than leaving it to be
/// rediscovered.
pub fn committed_field_shape_verdict_v1(
  field_index: Int,
  preimage: ByteArray,
) -> Int {
  let stride = field_stride(field_index)
  when minimal_array_header_v1(preimage) is {
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

/// §5.1's `definite_array_header(N)` at offset 0, as an **option** rather than
/// as an abort. Returns `(header_len, N)`.
///
/// `decode_field_array_header` decides the same thing on the door's hot path and
/// decides it by aborting, which is why it is not called here — §12.7 states the
/// reason at length and it has not changed: a shared option-returning reader
/// would put a silently clampable `None` where the doors read. This is the
/// narrow half of that duplication, not a second copy of the grammar: it reads
/// the three header widths and stops. Whether the *items* satisfy §5.1 is
/// `envelope_verdict_v1`'s answer and is asked of it.
///
/// `None` is reachable and means what it says — `#""`, a `9a` head, a
/// non-minimal `98 17`, a truncated `99 00`. The caller renders
/// `shape_verdict_not_an_envelope` for it, which is §12.7's fault and not this
/// one's.
fn minimal_array_header_v1(preimage: ByteArray) -> Option<Pair<Int, Int>> {
  let total_length = bytearray.length(preimage)
  if total_length == 0 {
    None
  } else {
    let tag = bytearray.at(preimage, 0)
    if and {
      tag >= 128,
      tag <= 151,
    } {
      Some(Pair(1, tag - 128))
    } else if tag == 152 {
      if total_length < 2 {
        None
      } else {
        let count = bytearray.at(preimage, 1)
        if count < 24 {
          None
        } else {
          Some(Pair(2, count))
        }
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

// ---------------------------------------------------------------------------
// The claim, and the verdict over an authenticated field
// ---------------------------------------------------------------------------

/// The evidence one step-01 derives and pins into the computation thread: the
/// slot accused and the shape verdict its committed bytes earned there.
pub type CommittedFieldShapeVerdictV1 {
  field_index: Int,
  verdict: Int,
}

/// The witness set handed to the door for a body field.
///
/// The door short-circuits before looking at it for `field_index < 6`, so the
/// unread value is constructed *here* rather than taken from the claim — there
/// is no argument for a prover to fill, and nothing can ride in under a member
/// the door will not read. Same idiom, same reason, as §12.7's.
fn unread_witness_set() -> NativeTxWitnessSetCompact {
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: #"",
    script_tx_wits_hash: #"",
    redeemer_tx_wits_hash: #"",
  }
}

/// Authenticate the claimed slot's committed preimage against §4's commitment
/// and render slot-`i`'s shape verdict over it.
///
/// **`CommittedFieldClaimV1` is §12.7's type, reused rather than re-declared.**
/// The accusation two sibling fault kinds make is the same accusation — which of
/// §2.5's nine slots, and how that slot's committed bytes reach the step — and
/// §6.1's one-spelling rule applies to a wire type as much as to a scalar: a
/// second structurally identical claim would be a second spelling of one
/// accusation, would double the surface #579's regeneration has to publish, and
/// would let the two families' body/witness split drift apart. What is *not*
/// shared is the verdict and the state it travels in: those code spaces differ,
/// and a §12.7 state must never satisfy this family's adjudication.
///
/// `verified` is the compact structure the block's counted `transactions_root`
/// committed — a family step-01 holds it already authenticated from
/// `pass_native_tx_to_next_step`, which is what makes the trailing
/// `witness_set_hash` inside it a *committed* value rather than the prover's.
///
/// The §2.5 half the claim was written for is asserted against the half its
/// index lives in, before any byte is read. Both directions are refused: a body
/// claim at a witness-set index would hand the door the placeholder above as if
/// it were the transaction's witness set, and a witness claim at a body index
/// would carry one the door discards.
pub fn committed_field_shape_v1(
  verified: VerifiedMidgardNativeTxCompact,
  claim: CommittedFieldClaimV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> CommittedFieldShapeVerdictV1 {
  when claim is {
    BodyFieldClaim { field_index, carriage } -> {
      expect field_index < first_witness_set_field_index
      CommittedFieldShapeVerdictV1 {
        field_index,
        verdict: authenticated_field_shape_verdict(
          verified,
          unread_witness_set(),
          field_index,
          carriage,
          reference_inputs,
          certificate_policy_id,
        ),
      }
    }
    WitnessFieldClaim { field_index, witness_set, carriage } -> {
      expect field_index >= first_witness_set_field_index
      CommittedFieldShapeVerdictV1 {
        field_index,
        verdict: authenticated_field_shape_verdict(
          verified,
          witness_set,
          field_index,
          carriage,
          reference_inputs,
          certificate_policy_id,
        ),
      }
    }
  }
}

/// Authenticate then decide, in that order and in one place: the door hashes the
/// carried bytes against §4's positionally-extracted commitment and the verdict
/// is taken over what it returns, at the slot the claim named. Written once so
/// neither claim arm can drift into deciding over bytes the other arm would have
/// authenticated differently.
fn authenticated_field_shape_verdict(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Int {
  committed_field_shape_verdict_v1(
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

/// The adjudicated violation predicate, stated over the two values step-01 pins
/// into the computation thread.
///
/// **The `shape_verdict_not_an_envelope` refusal is the disjointness boundary,
/// enforced rather than described.** A committed non-envelope is §12.7's fault;
/// admitting it here would let one committed field finalize under two fault
/// kinds, which is §12.1's one-spelling rule broken across sections rather than
/// within one. The predicate names the two convicting codes explicitly instead
/// of testing `verdict != shape_verdict_admissible`, so a code added later is
/// non-convicting until someone says otherwise.
///
/// The shape checks are not decoration either. A thread datum is written by
/// step-01 and read by step-02 across a transaction boundary: a state naming a
/// field outside 0..8, or carrying a code no verdict produces, is a state
/// step-01 could not have written. Both bounds are refusals rather than clamps
/// (§7.3).
pub fn is_committed_field_shape_violation_v1(
  field_index: Int,
  verdict: Int,
) -> Bool {
  expect field_index >= 0
  expect field_index < field_count
  expect verdict >= 0
  expect verdict < shape_verdict_code_count
  or {
    verdict == shape_verdict_field_byte_bound,
    verdict == shape_verdict_wrong_stride,
  }
}

// ---------------------------------------------------------------------------
// Producer-side helper — the §5.1 envelope, built at a chosen length
// ---------------------------------------------------------------------------

/// A §5.1 envelope of exactly `total_length` bytes carrying one item.
///
/// It exists because the shapes this family adjudicates are lengths, not
/// contents: an envelope that is grammatical and the wrong length for its slot
/// is the whole of the §7.4 accusation, and one above §5.4's bound is the whole
/// of the §5.4 one. `encode_field_preimage` takes a list of items and the caller
/// would have to solve for the payload width each time; solving it once here is
/// what lets the cross-language vectors and both step modules build their
/// fixtures from one construction rather than from arithmetic repeated at every
/// call site.
///
/// The item is `59 LLLL`-headed, so `total_length` must leave a payload above
/// 255 bytes — below that the minimal item header is narrower and the length
/// would not be the one asked for. Smaller fixtures are built from
/// `encode_field_preimage` directly.
pub fn sized_field_envelope_v1(total_length: Int, fill: ByteArray) -> ByteArray {
  let payload_length = total_length - 4
  expect payload_length > 0xff
  encode_field_preimage([repeat_bytes_v1(fill, payload_length)])
}

/// `fill` repeated `times` over, built by doubling.
///
/// A left fold of `concat` is quadratic in the result's length and does not fit
/// the test budget at the widths §5.4 admits; doubling is `O(log times)` concats
/// over geometrically growing arguments. `native_binding_fixture_v1` carries the
/// same idiom for the same reason and is not reused: this rule module is linked
/// by two validators, and a producer a validator links against must not drag a
/// fixture tree behind it.
fn repeat_bytes_v1(fill: ByteArray, times: Int) -> ByteArray {
  if times <= 0 {
    #""
  } else if times == 1 {
    fill
  } else {
    let half = repeat_bytes_v1(fill, times / 2)
    let doubled = bytearray.concat(half, half)
    if times % 2 == 0 {
      doubled
    } else {
      bytearray.concat(doubled, fill)
    }
  }
}

/// The five slots §5.3 gives a fixed stride, and the four it walks. Published so
/// that a vector set, a cross-language twin and a spec table can be checked
/// against one another rather than against three transcriptions of §5.3.
pub fn fixed_stride_field_indices_v1() -> List<Int> {
  list.filter(
    list.range(0, field_count - 1),
    fn(field_index) { field_stride(field_index) != walk_derived_stride },
  )
}
