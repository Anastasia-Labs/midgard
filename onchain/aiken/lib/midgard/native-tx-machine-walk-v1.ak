//// The dispute machine's walk core — `docs/spec/midgard-tx.md` §10.
////
//// `midgard/native_tx_field_access_v1` is the one door: it authenticates a
//// field's preimage against the positionally-extracted flat hash and hands
//// back a `FieldViewV1` to slice. This module is what the machine does *with*
//// that view when one transaction is not enough — it turns a view into a
//// **resumable walk**.
////
//// Three properties this module owns:
////
////   * **Authenticate-once is structural, not a convention** (§7.1). A walk
////     opens a view exactly once per transaction and then carries a
////     *position*. Every further item costs a wrapper decode and a slice —
////     never another `blake2b_256` over the preimage — so a dispute touching a
////     field pays that field's full-preimage hash check once however many
////     items it reads.
////   * **Positions, not bytes** (§7.6). `FieldWalkCheckpointV1` is five
////     scalars and one 32-byte transaction id. Its wire form is exactly
////     `field_walk_checkpoint_bytes` long *whatever the field holds*, so it
////     cannot be smuggling preimage content: two walks that reach the same
////     position over different preimage bytes serialise identically.
////   * **Arithmetic where the format bought it** (§5.3, §10.4). Fields 0/1,
////     3/4 and 7 have fixed strides, so relocating inside them is arithmetic
////     and their checkpoints are self-authenticating in O(1). Only the
////     variable-width fields 2/5/6/8 actually walk.
////
//// **What a checkpoint does and does not authenticate** (§10.2). For a
//// fixed-stride field the pair `(next_item_index, next_offset)` satisfies
//// `next_offset == header_len + stride · next_item_index`, so
//// `resume_field_walk` re-derives it and a forged position simply fails. For a
//// variable-width field the position is a function of the preimage that costs
//// a full re-walk to recompute, so it is deliberately *not* recomputed here:
//// its integrity is inductive, from the step chain that produced it — the
//// opening position is derived rather than supplied, and every advance is a
//// walk actually performed over authenticated bytes. What is enforced
//// unconditionally is that a checkpoint cannot be pointed at bytes it did not
//// come from: identity (`tx_id`, `field_index`) and shape (`total_length`,
//// `item_count`) must match the freshly authenticated view, and the offset
//// must land on a decodable §5.1 item head inside it.
////
//// **What §10.6 gets from this module, exactly.** `FieldWalkCheckpointV1` is
//// an opaque type and its decoder is private, so outside this module a
//// position can only have been derived from an authenticated view, advanced
//// over authenticated bytes, or put through `resume_field_walk_from_commitment`
//// — the digest check and then the §10.2 binding check. There is in
//// particular no way to hand `walk_next` a position of one's own choosing,
//// which is the shape the residual §10.2 item 7 gap would otherwise take, and
//// no way to reach the checkpoint-taking resume at all. What this module
//// cannot decide is where a caller's `committed` digest came from: a validator
//// that sourced it from a redeemer rather than from thread state would be back
//// to trusting the prover's arithmetic on a variable-width position. That half
//// of §10.6 is normative on dispute entry points and is not something a
//// library can hold for them.
////
//// Nothing here reads a §5.1 head on its own: every step goes through the
//// door's `field_item_header_at`, so one logical field keeps exactly one
//// verdict (§6.1).

use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/native_tx/components.{decode_midgard_tx_input_cbor}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTxInput, NativeTxWitnessSetCompact, VerifiedMidgardNativeTxCompact,
}
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, FieldViewV1, authenticated_field_view,
  authenticated_resumable_field_view_with_commitment, field_count,
  field_count_requires_certification, field_header_len, field_item_at,
  field_item_count, field_item_header_at, field_read_range, field_total_length,
  field_view_stride, fixed_item_wrapper_bytes, max_field_item_count,
  max_transaction_aggregate_field_bytes,
  provisional_field_item_count_for_certification, spend_input_item_bytes,
  spend_input_stride, walk_derived_stride,
}

// ---------------------------------------------------------------------------
// §10.2 the checkpoint
// ---------------------------------------------------------------------------

/// Where a walk over one field of one transaction has got to — and nothing
/// else (§7.6).
///
/// `next_item_index` items are done; `next_offset` is the byte offset of the
/// **wrapper** of the item that is not. `total_length` and `item_count` pin the
/// shape of the view the positions were taken against, so a checkpoint from a
/// differently-shaped carriage of the same field cannot be resumed.
///
/// There is no accumulator here on purpose. What a rule *learns* from a walk is
/// the rule's own business and already has a home — the machine's `work_root`
/// commits it — while the walk core's contract is only "where I was". Folding
/// an accumulator in here would make every consuming rule share one state
/// shape and would put rule-specific bytes one refactor away from a structure
/// §7.6 requires to stay positional.
///
/// **Opaque, and that is the §10.6 mechanism.** The constructor is not
/// exported, and neither is `decode_field_walk_checkpoint`, so outside this
/// module a value of this type can only have come from `open_field_walk` (a
/// position derived from an authenticated view), from an advance
/// (`walk_next`/`walk_fold`/`walk_skip`, each a walk actually performed over
/// authenticated bytes), or from `resume_field_walk_from_commitment` (a
/// position that hashed to a digest the caller supplied and then passed the
/// §10.2 binding check). There is no fourth way in. That is what makes the
/// inductive argument above a property of the type rather than a convention:
/// a literal `FieldWalkCheckpointV1` at a prover-chosen offset — the one thing
/// §10.2 item 7 cannot catch — is not a thing a caller can write.
pub opaque type FieldWalkCheckpointV1 {
  /// The L2 transaction whose field this walks. Bound to the tx-id-verified
  /// compact structures at open and at resume, so a checkpoint cannot be
  /// replayed against another transaction.
  tx_id: ByteArray,
  /// §2.5's positional field index. Plain hashing (§4) removed field-index
  /// domain separation, so fields 0/1 and 3/4 alias on identical content: the
  /// index is what tells them apart and it therefore travels with the
  /// position.
  field_index: Int,
  /// The authenticated preimage length the walk was opened against.
  total_length: Int,
  /// The authenticated item count (§5.2). A provisional grammar view under
  /// any carriage tier cannot create this semantic checkpoint: its header
  /// count becomes authenticated only at the grammar terminal.
  item_count: Int,
  /// Items `[0, next_item_index)` are done.
  next_item_index: Int,
  /// Byte offset of item `next_item_index`'s §5.1 wrapper; equals
  /// `total_length` exactly when the walk is complete.
  next_offset: Int,
}

/// Provisional envelope-certification position under any carriage tier.
///
/// `declared_count` is deliberately named rather than called an item count:
/// the authenticated header asserted it, but no semantic family may rely on
/// it until this checkpoint reaches both `next_item_index == declared_count`
/// and `next_offset == total_length`. The type is opaque for the same inductive
/// reason as `FieldWalkCheckpointV1`: a variable-width offset can only be
/// opened, advanced over authenticated chunk bytes, or resumed from the exact
/// digest the preceding computation-thread state committed.
pub opaque type FieldGrammarCheckpointV1 {
  tx_id: ByteArray,
  field_index: Int,
  /// The positional §4 commitment extracted by the field-access door. It
  /// prevents a same-transaction/same-length checkpoint from being replayed
  /// against different witness content.
  field_commitment: ByteArray,
  total_length: Int,
  declared_count: Int,
  next_item_index: Int,
  next_offset: Int,
}

/// Domain separation for the checkpoint commitment — ASCII
/// `MidgardFieldWalkCheckpointV1`. New surface: none of §4's prohibited
/// counted-scheme domains is reused.
const checkpoint_domain: ByteArray =
  #"4d6964676172644669656c6457616c6b436865636b706f696e745631"

/// ASCII `MidgardFieldGrammarCheckpointV1`. A provisional grammar position is
/// never interchangeable with a semantic walk position even when their
/// scalar coordinates happen to match.
const grammar_checkpoint_domain: ByteArray =
  #"4d6964676172644669656c644772616d6d6172436865636b706f696e745631"

/// §10.3. The wire form's length is a **constant**, independent of the field,
/// the carriage tier, the preimage and the position. That is the property that
/// makes "positions, not bytes" checkable rather than merely asserted.
pub const field_walk_checkpoint_bytes: Int = 53

/// `87`, tx id, field index, field commitment, and four fixed-width scalars.
pub const field_grammar_checkpoint_bytes: Int = 87

// ---------------------------------------------------------------------------
// §10.1 / §10.2 opening and resuming
// ---------------------------------------------------------------------------

/// Opens the provisional grammar-certification phase for a variable-width
/// field under any carriage tier. The field door authenticates the whole
/// preimage hash (tiers 1–2) or the certificate/chunk manifest (tier 3), plus
/// positional commitment, total length and header bytes; only the header's
/// count remains provisional.
pub fn open_field_grammar_certification(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldGrammarCheckpointV1> {
  let Pair(field_commitment, view) = authenticated_resumable_field_view_with_commitment(
    verified,
    witness_set,
    field_index,
    carriage,
    reference_inputs,
    certificate_policy_id,
  )
  expect field_count_requires_certification(view)
  let declared_count = provisional_field_item_count_for_certification(view)
  Pair(
    view,
    FieldGrammarCheckpointV1 {
      tx_id: verified.tx_id,
      field_index,
      field_commitment,
      total_length: field_total_length(view),
      declared_count,
      next_item_index: 0,
      next_offset: field_header_len(view),
    },
  )
}

/// Advance over at most `budget` canonical item envelopes without interpreting
/// their payloads. Every resume starts at the prior checkpoint's offset; no
/// prefix is materialised or re-walked.
pub fn certify_field_grammar(
  view: FieldViewV1,
  checkpoint: FieldGrammarCheckpointV1,
  budget: Int,
) -> FieldGrammarCheckpointV1 {
  expect budget >= 0
  if or {
    budget == 0,
    field_grammar_is_complete(checkpoint),
  } {
    checkpoint
  } else {
    expect checkpoint.next_item_index < checkpoint.declared_count
    let Pair(payload_offset, length) = field_item_header_at(
      view,
      checkpoint.next_offset,
    )
    let next_offset = payload_offset + length
    let next_item_index = checkpoint.next_item_index + 1
    expect next_offset <= checkpoint.total_length
    expect
      if next_item_index == checkpoint.declared_count {
        next_offset == checkpoint.total_length
      } else {
        next_offset < checkpoint.total_length
      }
    certify_field_grammar(
      view,
      FieldGrammarCheckpointV1 { ..checkpoint, next_item_index, next_offset },
      budget - 1,
    )
  }
}

pub fn field_grammar_is_complete(checkpoint: FieldGrammarCheckpointV1) -> Bool {
  and {
    checkpoint.next_item_index == checkpoint.declared_count,
    checkpoint.next_offset == checkpoint.total_length,
  }
}

/// Resume provisional certification from the exact position committed by the
/// preceding computation-thread state.
pub fn resume_field_grammar_certification_from_commitment(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  committed: ByteArray,
  checkpoint_bytes: ByteArray,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldGrammarCheckpointV1> {
  let checkpoint = decode_field_grammar_checkpoint(checkpoint_bytes)
  expect field_grammar_checkpoint_hash(checkpoint) == committed
  let Pair(field_commitment, view) = authenticated_resumable_field_view_with_commitment(
    verified,
    witness_set,
    checkpoint.field_index,
    carriage,
    reference_inputs,
    certificate_policy_id,
  )
  expect field_count_requires_certification(view)
  expect
    grammar_checkpoint_is_bound_to_view(
      verified.tx_id,
      field_commitment,
      view,
      checkpoint,
    )
  Pair(view, checkpoint)
}

fn grammar_checkpoint_is_bound_to_view(
  tx_id: ByteArray,
  field_commitment: ByteArray,
  view: FieldViewV1,
  checkpoint: FieldGrammarCheckpointV1,
) -> Bool {
  let declared_count = provisional_field_item_count_for_certification(view)
  let header_len = field_header_len(view)
  and {
    checkpoint.tx_id == tx_id,
    checkpoint.field_commitment == field_commitment,
    checkpoint.total_length == field_total_length(view),
    checkpoint.declared_count == declared_count,
    checkpoint.next_item_index >= 0,
    checkpoint.next_item_index <= declared_count,
    checkpoint.next_offset >= header_len,
    checkpoint.next_offset <= checkpoint.total_length,
    if checkpoint.next_item_index == declared_count {
      checkpoint.next_offset == checkpoint.total_length
    } else {
      let Pair(payload_offset, length) = field_item_header_at(
        view,
        checkpoint.next_offset,
      )
      payload_offset + length <= checkpoint.total_length
    },
  }
}

/// Terminal grammar certification is the only constructor for a semantic
/// walk from any provisional view. The returned semantic checkpoint starts at
/// item zero with the now-certified count; subsequent transactions use the
/// ordinary committed semantic-walk resume and never repeat this envelope
/// pass.
pub fn open_certified_field_walk_from_grammar_commitment(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  committed: ByteArray,
  checkpoint_bytes: ByteArray,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let Pair(view, grammar) = resume_field_grammar_certification_from_commitment(
    verified,
    witness_set,
    committed,
    checkpoint_bytes,
    carriage,
    reference_inputs,
    certificate_policy_id,
  )
  expect field_grammar_is_complete(grammar)
  Pair(
    view,
    walk_at_start_with_count(
      grammar.tx_id,
      grammar.field_index,
      view,
      grammar.declared_count,
    ),
  )
}

/// Open a walk over field `field_index` of the transaction committed by
/// `verified`.
///
/// The view is authenticated through the one door, so the expected hash is
/// extracted positionally from the committed compact structures and never
/// supplied (§4). The opening position is *derived* from that authenticated
/// view — it is not a redeemer argument — which is the base case the inductive
/// argument in this module's header rests on.
pub fn open_field_walk(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let view = authenticated_field_view(
    verified,
    witness_set,
    field_index,
    carriage,
    reference_inputs,
    certificate_policy_id,
  )
  Pair(view, walk_at_start(verified.tx_id, field_index, view))
}

fn walk_at_start(
  tx_id: ByteArray,
  field_index: Int,
  view: FieldViewV1,
) -> FieldWalkCheckpointV1 {
  walk_at_start_with_count(tx_id, field_index, view, field_item_count(view))
}

fn walk_at_start_with_count(
  tx_id: ByteArray,
  field_index: Int,
  view: FieldViewV1,
  item_count: Int,
) -> FieldWalkCheckpointV1 {
  FieldWalkCheckpointV1 {
    tx_id,
    field_index,
    total_length: field_total_length(view),
    item_count,
    next_item_index: 0,
    next_offset: field_header_len(view),
  }
}

/// Resume a carried walk in a follow-on transaction.
///
/// The field is named by the **checkpoint**, not by a fresh redeemer argument,
/// so a resume cannot be pointed at a different slot than the one that was
/// opened; and `tx_id` must be this transaction's, so a checkpoint cannot be
/// replayed across disputes. This transaction pays its own single
/// authenticate-once hash — it holds different bytes in a different script
/// context and has no way not to — but it re-pays none of the *items* the
/// earlier transaction walked, which is what AC2's "without re-paying completed
/// work" means here.
///
/// **Deliberately private.** §10.6 is normative — "Dispute entry points MUST
/// resume through the commitment, never from a redeemer-supplied checkpoint" —
/// and this is the form that takes a checkpoint the caller already trusts. A
/// variable-width position is explicitly not recomputable (§10.2 item 7), so
/// exporting this would let a caller hand a prover-chosen starting position
/// straight through the one check that cannot catch it. Privacy here is not on
/// its own enough — a caller that could still *build* a checkpoint would simply
/// skip the resume and call `walk_next` — which is why the type is opaque and
/// the decoder private as well. Between the three, the only route to a position
/// a caller chose is `resume_field_walk_from_commitment`, and that route runs
/// through the digest.
///
/// Returns the freshly authenticated view; the position is the checkpoint the
/// commitment pinned.
fn resume_field_walk(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  checkpoint: FieldWalkCheckpointV1,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> FieldViewV1 {
  expect checkpoint.tx_id == verified.tx_id
  let Pair(_field_commitment, view) = authenticated_resumable_field_view_with_commitment(
    verified,
    witness_set,
    checkpoint.field_index,
    carriage,
    reference_inputs,
    certificate_policy_id,
  )
  expect checkpoint_is_bound_to_view(view, checkpoint)
  view
}

/// Resume from **thread state** rather than from a checkpoint the caller
/// already trusts — the form every real step uses (§10.6).
///
/// A computation thread carries `committed`, the 32-byte
/// `field_walk_checkpoint_hash` of the position it stopped at, and the step
/// re-supplies the 53 positional bytes that hash to it. That is what makes
/// "positions, not bytes" a working arrangement instead of a restriction: the
/// thread's state stays one digest whatever the field holds, and the bytes it
/// commits are re-derivable by anyone from public data.
///
/// It is also what closes the one gap `resume_field_walk` cannot close on its
/// own. A variable-width position is not recomputable in O(1) (see this
/// module's header), so a caller that took a raw checkpoint from a redeemer
/// would be trusting the prover's arithmetic. Here the position is pinned by a
/// digest the *previous* step committed, so the only positions that can be
/// resumed are ones a walk over authenticated bytes actually reached.
pub fn resume_field_walk_from_commitment(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  committed: ByteArray,
  checkpoint_bytes: ByteArray,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let checkpoint = decode_field_walk_checkpoint(checkpoint_bytes)
  expect field_walk_checkpoint_hash(checkpoint) == committed
  let view = resume_field_walk(
    verified,
    witness_set,
    checkpoint,
    carriage,
    reference_inputs,
    certificate_policy_id,
  )
  Pair(view, checkpoint)
}

/// The §10.2 binding check, split out because it is the whole of what a resume
/// can honestly verify and it should be readable as one thing.
///
/// Three of the six bounds below are §10.8 backstops rather than live guards,
/// and it is worth saying which and why. A checkpoint only reaches here through
/// `decode_field_walk_checkpoint`, which re-encodes what it read and demands the
/// input back — so `encode_field_walk_checkpoint`'s own domain assertions have
/// already run on this value. Two of them (`next_item_index >= 0`,
/// `next_item_index <= item_count`) are word-for-word the same condition, and a
/// third (`next_offset <= total_length`) is the same condition on the
/// checkpoint's own `total_length`, which the first clause here pins to the
/// view's. They stay because this function is meant to read as the complete
/// statement of what a resume verifies, not as the residue left over after
/// another function's bounds; but no fixture can make one of them fire, and
/// §10.8 rows 4, 5 and 7 say so.
fn checkpoint_is_bound_to_view(
  view: FieldViewV1,
  checkpoint: FieldWalkCheckpointV1,
) -> Bool {
  let stride = field_view_stride(view)
  let header_len = field_header_len(view)
  and {
    checkpoint.total_length == field_total_length(view),
    checkpoint.item_count == if field_count_requires_certification(view) {
      provisional_field_item_count_for_certification(view)
    } else {
      field_item_count(view)
    },
    checkpoint.next_item_index >= 0,
    checkpoint.next_item_index <= checkpoint.item_count,
    // Live, and the only one of the four bounds that is: a position aimed into
    // the §5.1 array header passes the encoder and can find a decodable head
    // there. `resume_rejects_an_offset_inside_the_array_header` isolates it.
    checkpoint.next_offset >= header_len,
    checkpoint.next_offset <= checkpoint.total_length,
    if checkpoint.next_item_index == checkpoint.item_count {
      // §5.1 leaves no trailing bytes, so a finished walk has exactly one
      // admissible offset whichever kind of field it was. This branch reads no
      // bytes, so it is also the whole of what refuses a walk that declares
      // itself finished where it happened to stop —
      // `resume_rejects_a_walk_that_declares_itself_finished_early`.
      checkpoint.next_offset == checkpoint.total_length
    } else if stride > walk_derived_stride {
      // §10.2 — self-authenticating in O(1): the position is a function of the
      // index alone, so a forged offset cannot survive.
      checkpoint.next_offset == header_len + stride * checkpoint.next_item_index
    } else {
      // Not recomputable without the full re-walk this whole design exists to
      // avoid. What is checkable in O(1) is that the offset lands on a
      // decodable §5.1 head whose item ends inside the authenticated bytes.
      // `resume_rejects_a_position_whose_item_runs_past_the_field` isolates the
      // extent comparison; the head decode belongs to the door.
      let Pair(payload_offset, length) = field_item_header_at(
        view,
        checkpoint.next_offset,
      )
      payload_offset + length <= checkpoint.total_length
    },
  }
}

// ---------------------------------------------------------------------------
// §10.4 advancing
// ---------------------------------------------------------------------------

pub fn walk_is_complete(checkpoint: FieldWalkCheckpointV1) -> Bool {
  checkpoint.next_item_index == checkpoint.item_count
}

/// Items still to visit.
pub fn walk_remaining(checkpoint: FieldWalkCheckpointV1) -> Int {
  checkpoint.item_count - checkpoint.next_item_index
}

/// Which of §2.5's nine slots this walk is over.
///
/// A reader, not a route in: §10.6's clause withholds the *constructor* and the
/// decoder, because those turn caller-chosen data into a position. Reading a
/// scalar out of a position a caller already holds grants nothing — the position
/// was derived, advanced, or commitment-resumed to get there — and a consumer
/// that cannot ask has to carry the field index alongside the checkpoint, which
/// is a second copy of a fact the checkpoint exists to hold. §12's fault
/// statements ask, so that a statement about field 0 cannot be adjudicated
/// against a walk over field 1: §4's plain hashing removed field-index domain
/// separation, so nothing else would notice.
pub fn walk_field_index(checkpoint: FieldWalkCheckpointV1) -> Int {
  checkpoint.field_index
}

/// The index of the item this walk will read next — items
/// `[0, walk_next_item_index)` are done.
pub fn walk_next_item_index(checkpoint: FieldWalkCheckpointV1) -> Int {
  checkpoint.next_item_index
}

/// The L2 transaction this walk is over.
///
/// `walk_field_index`'s reasoning, one axis over: the field index says *which
/// slot*, this says *whose*. Both were set from the tx-id-verified compact
/// structures at open (`open_field_walk`) or re-checked against them at resume
/// (`resume_field_walk`), so what comes back is the transaction the door
/// authenticated and not a caller's assertion about it.
///
/// §12's adjudications ask, so that an accusation is bound to the transaction
/// it accuses: without this a fault statement is a claim about *some* field of
/// *some* transaction, and the same bytes would prove against any transaction
/// whose field happened to hold a matching item.
pub fn walk_tx_id(checkpoint: FieldWalkCheckpointV1) -> ByteArray {
  checkpoint.tx_id
}

/// The item the checkpoint points at, plus the checkpoint one past it.
///
/// This is the walk's single step and the only place a position advances. It
/// reads from `next_offset` — never from item 0 — which is what makes a
/// resumed walk cost its own items and nothing more.
pub fn walk_next(
  view: FieldViewV1,
  checkpoint: FieldWalkCheckpointV1,
) -> Pair<ByteArray, FieldWalkCheckpointV1> {
  // §10.8 row 13, a backstop. Every complete checkpoint a caller can hold sits
  // at `total_length` — `walk_at_start` derives it, the advances compute it,
  // §10.2 item 5 refuses any other at resume, and the opaque type leaves no
  // fourth way to obtain one — so the head read below would refuse anything
  // this line does. It is the line that notices if that ever stops being true.
  expect !walk_is_complete(checkpoint)
  let Pair(payload_offset, length) = field_item_header_at(
    view,
    checkpoint.next_offset,
  )
  let stride = field_view_stride(view)
  // §7.2: arithmetic locates an item, it does not excuse reading its wrapper.
  // For a fixed-stride field the stride pins the wrapper's one admissible
  // spelling, and the walk holds it to that.
  //
  // The `length` conjunct is the live one —
  // `walk_next_refuses_a_wrapper_whose_length_misses_the_stride` isolates it.
  // The `payload_offset` conjunct is §10.8 row 15, a backstop no vector can
  // isolate:
  // `field_item_header_at` reads minimal-width heads only, so the payload
  // offset is a function of the declared length (≤23 → +1, 24..255 → +2,
  // ≥256 → +3), and every §5.3 stride's `stride − 2` (28, 38, 101) falls in
  // the middle band. Given the length conjunct the offset conjunct cannot
  // fail. It stays because that implication lives in another module's head
  // reader, and this is the line that notices if it ever stops holding.
  expect or {
      stride == walk_derived_stride,
      and {
        payload_offset == checkpoint.next_offset + fixed_item_wrapper_bytes,
        length == stride - fixed_item_wrapper_bytes,
      },
    }
  let next_offset = payload_offset + length
  let next_item_index = checkpoint.next_item_index + 1
  // §7.3 abort, never clamp — and §5.1's no-trailing-bytes rule at the end.
  //
  // The bounds line is §10.8 row 16, a backstop: the read two lines down goes
  // through `field_read_range`, which refuses any extent outside the
  // authenticated bytes, so no fixture can attribute a refusal to this line
  // rather than to that one. The no-trailing-bytes line below is live and
  // `walk_next_refuses_a_final_advance_that_misses_the_end` isolates it.
  expect next_offset <= checkpoint.total_length
  expect or {
      next_item_index < checkpoint.item_count,
      next_offset == checkpoint.total_length,
    }
  Pair(
    field_read_range(view, payload_offset, length),
    FieldWalkCheckpointV1 { ..checkpoint, next_item_index, next_offset },
  )
}

/// Visit at most `budget` items, folding each into `state`.
///
/// The budget is what makes a walk interruptible: a step takes as many items as
/// its transaction can afford, commits the returned checkpoint, and the next
/// step resumes from it. `walk_is_complete` on the returned checkpoint is how a
/// caller learns whether it finished.
///
/// `step` receives the item's index alongside its bytes, because index-sensitive
/// rules (order, dedup, "the item at position k") are most of what walks a
/// field, and recovering the index from a fold state is exactly the sort of
/// bookkeeping this core exists to remove.
pub fn walk_fold(
  view: FieldViewV1,
  checkpoint: FieldWalkCheckpointV1,
  budget: Int,
  state: a,
  step: fn(a, Int, ByteArray) -> a,
) -> Pair<a, FieldWalkCheckpointV1> {
  expect budget >= 0
  if or {
    budget == 0,
    walk_is_complete(checkpoint),
  } {
    Pair(state, checkpoint)
  } else {
    let index = checkpoint.next_item_index
    let Pair(item, advanced) = walk_next(view, checkpoint)
    walk_fold(view, advanced, budget - 1, step(state, index, item), step)
  }
}

/// Relocate `count` items forward without visiting them.
///
/// For a fixed-stride field this is one multiplication (§10.4) — the whole
/// point of §5.3's fixed 3-byte output index. For a variable-width field there
/// is nothing to compute from, so it walks; the cost difference is the format's
/// and not something this module can paper over.
pub fn walk_skip(
  view: FieldViewV1,
  checkpoint: FieldWalkCheckpointV1,
  count: Int,
) -> FieldWalkCheckpointV1 {
  expect count >= 0
  let target = checkpoint.next_item_index + count
  expect target <= checkpoint.item_count
  let stride = field_view_stride(view)
  if stride > walk_derived_stride {
    FieldWalkCheckpointV1 {
      ..checkpoint,
      next_item_index: target,
      next_offset: field_header_len(view) + stride * target,
    }
  } else {
    walk_skip_by_walking(view, checkpoint, count)
  }
}

fn walk_skip_by_walking(
  view: FieldViewV1,
  checkpoint: FieldWalkCheckpointV1,
  count: Int,
) -> FieldWalkCheckpointV1 {
  if count <= 0 {
    checkpoint
  } else {
    let Pair(_item, advanced) = walk_next(view, checkpoint)
    walk_skip_by_walking(view, advanced, count - 1)
  }
}

// ---------------------------------------------------------------------------
// §10.5 the fixed-stride shortcut: spend and reference inputs
// ---------------------------------------------------------------------------

/// §5.3 / §10.5. A spend- or reference-input item is exactly
/// `spend_input_item_bytes` at `spend_input_stride`, so item `index` is located
/// by one multiplication and read by one slice: **no walk is entered and the
/// cost does not grow with `index`.** That is the return on the format's sole
/// deliberately non-minimal encoding, the fixed 3-byte output index.
///
/// The wrapper is still decoded and held to the stride (§7.2), so the O(1)
/// path admits exactly one byte form for one logical item.
///
/// The stride guard is the one doing the refusing, and
/// `spend_input_at_refuses_a_variable_width_view_of_input_shaped_items` in the
/// seam suite isolates it. The width check below is §10.8 row 22: it cannot be
/// isolated by any vector and is not meant to be, because once the stride guard
/// has passed,
/// `field_item_extent` already pins `length == stride − fixed_item_wrapper_bytes`,
/// which is `spend_input_item_bytes` by construction. It stays as a
/// constants-consistency backstop — the one line that goes red if
/// `spend_input_stride`, `spend_input_item_bytes` and `fixed_item_wrapper_bytes`
/// ever stop agreeing — not as a live guard with test coverage owed to it.
pub fn spend_input_at(view: FieldViewV1, index: Int) -> MidgardTxInput {
  expect field_view_stride(view) == spend_input_stride
  let item = field_item_at(view, index)
  expect bytearray.length(item) == spend_input_item_bytes
  decode_midgard_tx_input_cbor(item)
}

/// The authenticated item count of a field-0/1 view (§5.2). Guarded on the
/// stride so a variable-width view cannot be read as inputs by accident.
pub fn spend_input_count(view: FieldViewV1) -> Int {
  expect field_view_stride(view) == spend_input_stride
  field_item_count(view)
}

// ---------------------------------------------------------------------------
// §10.3 checkpoint wire form
// ---------------------------------------------------------------------------

/// The checkpoint as thread-carriable bytes.
///
/// Fixed-width scalars, not canonical-minimal ones: §5.3 already establishes
/// that this format pins a fixed width where a constant size is worth more than
/// minimality, and here it is worth a great deal. A constant length is what
/// lets a test *prove* the structure carries no preimage content instead of
/// re-reading the encoder and taking its word.
pub fn encode_field_walk_checkpoint(
  checkpoint: FieldWalkCheckpointV1,
) -> ByteArray {
  expect bytearray.length(checkpoint.tx_id) == 32
  expect checkpoint.field_index >= 0
  expect checkpoint.field_index < field_count
  expect checkpoint.total_length > 0
  expect checkpoint.total_length <= max_transaction_aggregate_field_bytes
  expect checkpoint.item_count >= 0
  expect checkpoint.item_count <= max_field_item_count
  expect checkpoint.next_item_index >= 0
  expect checkpoint.next_item_index <= checkpoint.item_count
  expect checkpoint.next_offset >= 0
  expect checkpoint.next_offset <= checkpoint.total_length
  #"86"
    |> bytearray.concat(#"5820")
    |> bytearray.concat(checkpoint.tx_id)
    |> bytearray.concat(#"41")
    |> bytearray.concat(
        bytearray.from_int_big_endian(checkpoint.field_index, 1),
      )
    |> bytearray.concat(#"43")
    |> bytearray.concat(
        bytearray.from_int_big_endian(checkpoint.total_length, 3),
      )
    |> bytearray.concat(#"43")
    |> bytearray.concat(bytearray.from_int_big_endian(checkpoint.item_count, 3))
    |> bytearray.concat(#"43")
    |> bytearray.concat(
        bytearray.from_int_big_endian(checkpoint.next_item_index, 3),
      )
    |> bytearray.concat(#"43")
    |> bytearray.concat(
        bytearray.from_int_big_endian(checkpoint.next_offset, 3),
      )
}

/// Fail-closed inverse of `encode_field_walk_checkpoint`. The bounds are not
/// re-listed: re-encoding the decoded value and demanding the input back is
/// both the canonicity check (§6.1 — one admissible spelling) and the range
/// check, without a second reader of the same grammar to drift from the first.
///
/// **Deliberately private, for the same reason the type is opaque.** A public
/// decoder is a public constructor: it turns 53 caller-chosen bytes into a
/// position, which is exactly the capability `FieldWalkCheckpointV1`'s opacity
/// exists to withhold. The only way through it is
/// `resume_field_walk_from_commitment`, which decodes and then holds the result
/// to a digest and to the §10.2 binding check.
fn decode_field_walk_checkpoint(bytes: ByteArray) -> FieldWalkCheckpointV1 {
  expect bytearray.length(bytes) == field_walk_checkpoint_bytes
  let checkpoint = FieldWalkCheckpointV1 {
    tx_id: bytearray.slice(bytes, 3, 34),
    field_index: bytearray.at(bytes, 36),
    total_length: bytearray.to_int_big_endian(bytearray.slice(bytes, 38, 40)),
    item_count: bytearray.to_int_big_endian(bytearray.slice(bytes, 42, 44)),
    next_item_index: bytearray.to_int_big_endian(bytearray.slice(bytes, 46, 48)),
    next_offset: bytearray.to_int_big_endian(bytearray.slice(bytes, 50, 52)),
  }
  expect encode_field_walk_checkpoint(checkpoint) == bytes
  checkpoint
}

/// The 32-byte commitment a computation thread carries in place of the
/// checkpoint itself, so that thread state stays fixed-size whatever it is
/// tracking.
pub fn field_walk_checkpoint_hash(
  checkpoint: FieldWalkCheckpointV1,
) -> ByteArray {
  blake2b_256(
    bytearray.concat(
      checkpoint_domain,
      encode_field_walk_checkpoint(checkpoint),
    ),
  )
}

/// Fixed-width provisional grammar checkpoint. The additional 32-byte field
/// commitment is the content identity a semantic walk checkpoint inherits
/// inductively after terminal certification.
pub fn encode_field_grammar_checkpoint(
  checkpoint: FieldGrammarCheckpointV1,
) -> ByteArray {
  expect bytearray.length(checkpoint.tx_id) == 32
  expect checkpoint.field_index >= 0
  expect checkpoint.field_index < field_count
  expect bytearray.length(checkpoint.field_commitment) == 32
  expect checkpoint.total_length > 0
  expect checkpoint.total_length <= max_transaction_aggregate_field_bytes
  expect checkpoint.declared_count >= 0
  expect checkpoint.declared_count <= max_field_item_count
  expect checkpoint.next_item_index >= 0
  expect checkpoint.next_item_index <= checkpoint.declared_count
  expect checkpoint.next_offset >= 0
  expect checkpoint.next_offset <= checkpoint.total_length
  #"87"
    |> bytearray.concat(#"5820")
    |> bytearray.concat(checkpoint.tx_id)
    |> bytearray.concat(#"41")
    |> bytearray.concat(
        bytearray.from_int_big_endian(checkpoint.field_index, 1),
      )
    |> bytearray.concat(#"5820")
    |> bytearray.concat(checkpoint.field_commitment)
    |> bytearray.concat(#"43")
    |> bytearray.concat(
        bytearray.from_int_big_endian(checkpoint.total_length, 3),
      )
    |> bytearray.concat(#"43")
    |> bytearray.concat(
        bytearray.from_int_big_endian(checkpoint.declared_count, 3),
      )
    |> bytearray.concat(#"43")
    |> bytearray.concat(
        bytearray.from_int_big_endian(checkpoint.next_item_index, 3),
      )
    |> bytearray.concat(#"43")
    |> bytearray.concat(
        bytearray.from_int_big_endian(checkpoint.next_offset, 3),
      )
}

fn decode_field_grammar_checkpoint(bytes: ByteArray) -> FieldGrammarCheckpointV1 {
  expect bytearray.length(bytes) == field_grammar_checkpoint_bytes
  let checkpoint = FieldGrammarCheckpointV1 {
    tx_id: bytearray.slice(bytes, 3, 34),
    field_index: bytearray.at(bytes, 36),
    field_commitment: bytearray.slice(bytes, 39, 70),
    total_length: bytearray.to_int_big_endian(bytearray.slice(bytes, 72, 74)),
    declared_count: bytearray.to_int_big_endian(bytearray.slice(bytes, 76, 78)),
    next_item_index: bytearray.to_int_big_endian(bytearray.slice(bytes, 80, 82)),
    next_offset: bytearray.to_int_big_endian(bytearray.slice(bytes, 84, 86)),
  }
  expect encode_field_grammar_checkpoint(checkpoint) == bytes
  checkpoint
}

pub fn field_grammar_checkpoint_hash(
  checkpoint: FieldGrammarCheckpointV1,
) -> ByteArray {
  blake2b_256(
    bytearray.concat(
      grammar_checkpoint_domain,
      encode_field_grammar_checkpoint(checkpoint),
    ),
  )
}
