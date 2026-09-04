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

use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/field_opening_v1.{first_witness_set_field_index}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxWitnessSetCompact, VerifiedMidgardNativeTxCompact,
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

/// §5.1 grammaticality as a **verdict**, over any byte string at all.
///
/// **Total by construction, and the property is the point.** There is no
/// `expect`, no `fail` and no partial builtin call on an unchecked index in
/// this function or the two it recurses through: `bytearray.at` aborts out of
/// range, so every read below is preceded by the bound that makes it safe, and
/// the bound's failure is a *return value*. A verdict that could abort would be
/// the door again, and the door is what this family exists to route around.
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
pub fn envelope_verdict_v1(preimage: ByteArray) -> Int {
  let total_length = bytearray.length(preimage)
  if total_length == 0 {
    verdict_missing_array_header
  } else {
    let tag = bytearray.at(preimage, 0)
    if and {
      tag >= 128,
      tag <= 151,
    } {
      // `80 + N` for N <= 23. The header is one byte and is already read.
      walk_items(preimage, total_length, 1, tag - 128)
    } else if tag == 152 {
      if total_length < 2 {
        verdict_truncated_array_header
      } else {
        let count = bytearray.at(preimage, 1)
        if count < 24 {
          verdict_non_minimal_array_header
        } else {
          walk_items(preimage, total_length, 2, count)
        }
      }
    } else if tag == 153 {
      if total_length < 3 {
        verdict_truncated_array_header
      } else {
        let count = bytearray.at(preimage, 1) * 256 + bytearray.at(preimage, 2)
        if count <= 0xff {
          verdict_non_minimal_array_header
        } else {
          walk_items(preimage, total_length, 3, count)
        }
      }
    } else {
      verdict_not_an_array_header
    }
  }
}

/// Reads `remaining` enveloped items from `offset` and decides where they end.
///
/// The two ways to leave §5.1 that only the *end* of the walk can see are both
/// here: items that run out of preimage (`verdict_missing_item_header`, and the
/// truncation codes below it) and preimage that runs out of items
/// (`verdict_trailing_bytes`). Everything else is a per-item head decision.
fn walk_items(
  preimage: ByteArray,
  total_length: Int,
  offset: Int,
  remaining: Int,
) -> Int {
  if remaining <= 0 {
    if offset == total_length {
      verdict_grammatical
    } else {
      verdict_trailing_bytes
    }
  } else if offset >= total_length {
    verdict_missing_item_header
  } else {
    let tag = bytearray.at(preimage, offset)
    if and {
      tag >= 64,
      tag <= 87,
    } {
      // `40 + L` for L <= 23.
      advance_item(preimage, total_length, offset + 1, tag - 64, remaining)
    } else if tag == 88 {
      if offset + 2 > total_length {
        verdict_truncated_item_header
      } else {
        let length = bytearray.at(preimage, offset + 1)
        if length < 24 {
          verdict_non_minimal_item_header
        } else {
          advance_item(preimage, total_length, offset + 2, length, remaining)
        }
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
          advance_item(preimage, total_length, offset + 3, length, remaining)
        }
      }
    } else {
      verdict_not_an_item_header
    }
  }
}

/// Steps over one item's payload, or reports that the payload leaves the
/// preimage. Split out so that the bound is written once for all three header
/// widths rather than three times — a bound written three times is a bound one
/// of whose copies will one day be wrong.
fn advance_item(
  preimage: ByteArray,
  total_length: Int,
  payload_offset: Int,
  length: Int,
  remaining: Int,
) -> Int {
  if payload_offset + length > total_length {
    verdict_truncated_item_payload
  } else {
    walk_items(preimage, total_length, payload_offset + length, remaining - 1)
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

/// The evidence one step-01 derives and pins into the computation thread: the
/// slot accused and the verdict its committed bytes earned.
pub type CommittedFieldVerdictV1 {
  field_index: Int,
  verdict: Int,
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
/// and render §5.1's verdict over it.
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
pub fn committed_field_verdict_v1(
  verified: VerifiedMidgardNativeTxCompact,
  claim: CommittedFieldClaimV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> CommittedFieldVerdictV1 {
  when claim is {
    BodyFieldClaim { field_index, carriage } -> {
      expect field_index < first_witness_set_field_index
      CommittedFieldVerdictV1 {
        field_index,
        verdict: authenticated_field_verdict(
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
      CommittedFieldVerdictV1 {
        field_index,
        verdict: authenticated_field_verdict(
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

/// Authenticate then decide, in that order and in one place: the door hashes
/// the carried bytes against §4's positionally-extracted commitment and the
/// verdict is taken over what it returns. Written once so neither claim arm can
/// drift into deciding over bytes the other arm would have authenticated
/// differently.
fn authenticated_field_verdict(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Int {
  envelope_verdict_v1(
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
