//// Witness-minimal fault statements — `docs/spec/midgard-tx.md` §12.
////
//// §10 says how a dispute reaches one item's bytes and how it carries its place
//// between transactions; §11 says what a rule may do inside those bytes. This
//// module says what a challenger **claims**, and it exists because the two
//// layers below it make a much smaller claim possible than the counted scheme
//// did: a fault statement names one wrong thing and re-supplies nothing.
////
//// **What "witness-minimal" means here, precisely.** A statement is scalars, a
//// unit id, and positions. It carries:
////
////   * the disputed transaction id, and — for the fault kind that has one — the
////     field index and item index: the *address* of the fault, never its
////     content;
////   * for a per-asset fault, exactly one `(policy_id, asset_name)` unit, which
////     is that kind's whole address; and
////   * the quantity the operator committed, for the arithmetic to disagree with.
////
//// It carries **no preimage bytes**. The fraudulent item's bytes reach the
//// adjudication from the authenticated `FieldViewV1` — through §10's walk, which
//// read them from bytes the §8.8 door already hashed against the positionally
//// extracted field commitment (§7.1). A step therefore pays the authenticate-once
//// check for the fields it touches and nothing beyond it, and a statement's wire
//// length is `fault_statement_frame_bytes` plus the two name encodings — a
//// function of the named unit alone. The seam suite runs the consequence rather
//// than claiming it: the same logical fault in two transactions whose fields
//// share no item at any index is accused by two statements that differ in
//// nothing but the 32 tx-id bytes, and each proves against its own.
////
//// The tx id is **adjudicated**, not merely carried. Every `prove_*` function
//// checks it against `walk_tx_id` — the id the §8.8 door authenticated against —
//// so a statement is bound to the transaction it accuses. A statement that
//// proved against any transaction holding a matching item would be a smaller
//// witness only because it said less, and what it stopped saying is whom it
//// accuses.
////
//// **Why this type is not opaque, where §10.6's is.** A `FieldWalkCheckpointV1`
//// is *trusted position state*: a caller that could write one would be reading
//// authenticated bytes at an offset nobody checked, so §10.6 withholds the
//// constructor. A `FaultStatementV1` is the opposite — it is an **accusation**,
//// and an accusation anybody may write is what a permissionless dispute game
//// needs. Nothing here is believed because it was stated: the `prove_*`
//// functions adjudicate a statement against authenticated bytes, and a statement
//// that is not true simply fails to prove. Opacity would restrict who can accuse
//// without making any accusation safer.
////
//// **Per-asset conservation isolates one asset id** (§12.4). The two sweeps
//// below name a unit and read only that unit: an output's value is reached
//// through §11.1's Value bookmark, which never materialises it, and the mint
//// field is swept with the same monotone discipline one level up. A statement
//// about a unit a transaction does not hold is still answerable — the sweep
//// reports `0`, and the `0` is evidence — because both sweeps enforce the
//// canonical key order that makes absence observable (§5.5, §5.6, §11.1) and
//// because each carries whether it *finished*. Both halves travel as sweep
//// values rather than as integers, so the adjudication can require that the two
//// numbers it subtracts were measured over the accused transaction, over the
//// right fields, about the statement's own unit, and to a verdict — and so that
//// a sweep stopped by its budget can never be read as absence.
////
//// **A conservation adjudication is a single invocation** (§12.4), and that is
//// the contract rather than a limitation this module happens to have. The two
//// sweep types have no wire form, no decoder and no commitment-resume
//// constructor — deliberately, since a sweep is a *running measurement* and not
//// a position, and a resumable one would need §10.3's whole apparatus repeated
//// over state that §12.5's own measurement says a variable-width field cannot
//// afford to carry across steps anyway. What the sweeps do carry across is
//// **budget rounds inside one invocation**: a caller advances a sweep as many
//// times as it likes, and the order key that licenses the mint side's early stop
//// travels in the sweep rather than in a recursion argument, so it spans those
//// rounds. Both sweeps also open only at item 0 of their field, so a sweep can
//// never be an answer about a *suffix* — which is the shape a re-open
//// masquerading as a resume would have taken.

use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/intra_item_bytes_v1.{byte_in, head_at, slice_exact}
use midgard/native_tx_field_access_v1.{
  FieldViewV1, field_count, max_field_item_count,
}
use midgard/native_tx_intra_item_v1.{
  canonical_key_precedes_v1, max_asset_name_bytes, open_value_bookmark,
  policy_id_bytes, value_quantity_of,
}
use midgard/native_tx_machine_walk_v1.{
  FieldWalkCheckpointV1, walk_field_index, walk_is_complete, walk_next,
  walk_next_item_index, walk_skip, walk_tx_id,
}

// ---------------------------------------------------------------------------
// §12.1 the statement
// ---------------------------------------------------------------------------

/// One item of one field fails a per-item predicate the format admits but the
/// protocol forbids. The witness is that item's *index*.
pub const fault_item_predicate: Int = 1

/// The net quantity of exactly one `(policy_id, asset_name)` unit disagrees
/// with the quantity the operator committed. The witness is that unit.
pub const fault_asset_conservation: Int = 2

/// §2.5's positional field index for outputs — the field a per-asset sweep
/// reads values out of (§5.5). Spelled the repo's way
/// (`ledger_output_proof_v1.output_field_index`, `ledger_output_commitment_v1`)
/// so §2.5's slot 2 has one name across the tree.
///
/// Private: it is an *internal* invariant of the two sweep constructors below,
/// which name the field themselves rather than taking it from a caller. A
/// consumer never picks the field, so exporting the number would only invite
/// one to.
const output_field_index: Int = 2

/// §2.5's positional field index for mint (§5.6). Private for
/// `output_field_index`'s reason.
const mint_field_index: Int = 5

/// The largest magnitude a statement's `claimed` quantity may carry: the §12.2
/// wire form spells it in eight bytes. Private — it is a property of the wire
/// form, enforced by `encode_fault_statement`, not a knob.
const max_claimed_magnitude: Int = 18446744073709551615

/// Domain separation for the statement commitment — ASCII
/// `MidgardNativeTxFaultStatementV1`. New surface: none of §4's prohibited
/// counted-scheme domains is reused, and it is distinct from §10.3's and
/// §11.3's.
const statement_domain: ByteArray =
  #"4d6964676172644e617469766554784661756c7453746174656d656e745631"

/// A challenger's claim that exactly one thing about a transaction is wrong.
///
/// **Every field a code has no meaning for has exactly one spelling**, and
/// `fault_statement_shape_is_exact` is where that is pinned. `policy_id`,
/// `asset_name` and `claimed` are the item fault's three: it names an item, not
/// a unit or a quantity. `field_index` and `item_index` are the conservation
/// fault's two, and they are pinned for a reason worth stating, because the
/// earlier spelling of this docstring got it wrong. A conservation statement's
/// address is its **unit**: the adjudication reads fields 2 *and* 5 in full, from
/// item 0 of each, so there is no single field for `field_index` to name and no
/// starting item for `item_index` to pick. They are therefore fixed at
/// `output_field_index` and `0`, and the fixing is load-bearing rather than
/// cosmetic — an unpinned scalar that no adjudication reads is one that changes
/// `fault_statement_hash` without changing any verdict, so two distinct
/// commitments would adjudicate identically.
pub type FaultStatementV1 {
  tx_id: ByteArray,
  code: Int,
  field_index: Int,
  item_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
  claimed: Int,
}

/// The per-code shape rule, split out because it is what makes the statement a
/// claim about one thing rather than a record with optional parts.
///
/// A `fault_item_predicate` statement that also named a unit would be two
/// accusations in one witness, and the adjudication would have no way to say
/// which of them the refusal belonged to. A `fault_asset_conservation` statement
/// that named a field or a starting item would be worse than redundant: §12.4
/// reads fields 2 and 5 from item 0 of each and consults neither scalar, so a
/// free `field_index` would be a byte that moves the statement's commitment
/// without moving any verdict — two hashes, one adjudication.
///
/// Run at encode time *and* again at both adjudications, because a
/// `FaultStatementV1` is a plain type a challenger may write by hand.
fn fault_statement_shape_is_exact(statement: FaultStatementV1) -> Bool {
  and {
    // §12.2 — the accused transaction is named by a full 32-byte id.
    bytearray.length(statement.tx_id) == 32,
    // §2.5 — the address's field is one of the nine slots.
    statement.field_index >= 0,
    statement.field_index < field_count,
    // §5.2 — and its item is one a field can hold.
    statement.item_index >= 0,
    statement.item_index <= max_field_item_count,
    if statement.code == fault_asset_conservation {
      and {
        // §12.1 — a conservation fault names a full 28-byte policy id: a short
        // one would name a *set* of policies and the sweeps would find none.
        bytearray.length(statement.policy_id) == policy_id_bytes,
        // §5.3 — and an asset name the grammar admits.
        bytearray.length(statement.asset_name) <= max_asset_name_bytes,
        // §12.4 — the address of a conservation fault is its unit, so the two
        // positional scalars get exactly one spelling each. This is the clause
        // that stops `field_index` from being an unadjudicated byte: without it
        // two statements differing only there hash differently and adjudicate
        // identically.
        statement.field_index == output_field_index,
        statement.item_index == 0,
      }
    } else {
      and {
        // §12.1 — there is no third code.
        statement.code == fault_item_predicate,
        // §12.1 — an item fault names no unit and claims no quantity; either
        // would make it two accusations in one witness.
        statement.policy_id == #"",
        statement.asset_name == #"",
        statement.claimed == 0,
      }
    },
  }
}

// ---------------------------------------------------------------------------
// §12.2 statement wire form
// ---------------------------------------------------------------------------

/// Everything in the §12.2 wire form except the two name encodings: the `87`
/// head, the transaction id and its wrapper, the three fixed-width scalars and
/// their wrappers, and the nine-byte signed quantity and its wrapper.
pub const fault_statement_frame_bytes: Int = 53

/// The `fault_item_predicate` form's total width — **constant**, because that
/// fault names no unit and both name encodings are the empty `40`. A statement
/// that a transaction's item 7 is fraudulent is 55 bytes whether the field holds
/// eight items or eight hundred, and whatever those items say.
pub const item_fault_statement_bytes: Int = fault_statement_frame_bytes + 2

/// The statement as thread-carriable bytes.
///
/// Fixed-width scalars for §10.3's reason. The two name payloads are the one
/// variable part, and they are variable in the *unit*, never in the preimage:
/// a statement's length is `fault_statement_frame_bytes + |policy_id| +
/// |asset_name|`, so its size reveals which asset is accused and nothing about
/// the transaction accused of holding it.
///
/// `claimed` is signed and quantities are not, so it travels as a sign byte and
/// an eight-byte magnitude rather than as a CBOR `int`: two's complement would
/// spend a ninth byte to say the same thing, and a CBOR `int` would be
/// variable-width, which is the one property this form exists to deny.
pub fn encode_fault_statement(statement: FaultStatementV1) -> ByteArray {
  // §12.1 — only a well-shaped statement has a wire form: the shape rule is what
  // makes the frame's fixed slots mean one thing per code.
  expect fault_statement_shape_is_exact(statement)
  let magnitude = if statement.claimed < 0 {
    -statement.claimed
  } else {
    statement.claimed
  }
  // §12.2 — the quantity is an eight-byte magnitude and a sign byte, so a wider
  // number has no spelling here. Asserted rather than left to the builtin, which
  // would abort with nothing attributable said (§7.3).
  expect magnitude <= max_claimed_magnitude
  let sign = if statement.claimed < 0 {
    1
  } else {
    0
  }
  #"87"
    |> bytearray.concat(#"5820")
    |> bytearray.concat(statement.tx_id)
    |> bytearray.concat(#"41")
    |> bytearray.concat(bytearray.from_int_big_endian(statement.code, 1))
    |> bytearray.concat(#"41")
    |> bytearray.concat(bytearray.from_int_big_endian(statement.field_index, 1))
    |> bytearray.concat(#"43")
    |> bytearray.concat(bytearray.from_int_big_endian(statement.item_index, 3))
    |> bytearray.concat(encode_name(statement.policy_id))
    |> bytearray.concat(encode_name(statement.asset_name))
    |> bytearray.concat(#"49")
    |> bytearray.concat(bytearray.from_int_big_endian(sign, 1))
    |> bytearray.concat(bytearray.from_int_big_endian(magnitude, 8))
}

/// §5.1's minimal-width byte-string head over a name of at most 32 bytes. The
/// two widths this reaches — one byte up to 23, two bytes above — are the same
/// two §5.1 admits at those lengths, so a name has exactly one spelling here and
/// in the preimage it is compared against.
fn encode_name(name: ByteArray) -> ByteArray {
  let length = bytearray.length(name)
  // §5.3 — a name is at most 32 bytes, which is what keeps this reader to §5.1's
  // two minimal-width head forms.
  expect length <= max_asset_name_bytes
  if length <= 23 {
    bytearray.from_int_big_endian(0x40 + length, 1)
      |> bytearray.concat(name)
  } else {
    #"58"
      |> bytearray.concat(bytearray.from_int_big_endian(length, 1))
      |> bytearray.concat(name)
  }
}

/// Fail-closed inverse of `encode_fault_statement`. The bounds are not
/// re-listed: re-encoding the decoded value and demanding the input back is
/// simultaneously the canonicity check (§6.1 — one admissible spelling), the
/// range check, and the per-code shape check, without a second reader of the
/// same grammar to drift from the first.
///
/// Public, where §10.3's decoder is private, for the reason this module's header
/// gives: turning caller-chosen bytes into a *position* is a capability; turning
/// them into an *accusation* is what a challenger does.
pub fn decode_fault_statement(bytes: ByteArray) -> FaultStatementV1 {
  // The §12.2 layout up to here is fixed: `87`, `58 20` + 32, `41` + 1,
  // `41` + 1, `43` + 3 — so the policy's own head begins at offset 43.
  let Pair(policy_payload, policy_length) = head_at(bytes, 43, 2)
  let name_offset = policy_payload + policy_length
  let Pair(name_payload, name_length) = head_at(bytes, name_offset, 2)
  let quantity_offset = name_payload + name_length
  // §12.2 — the quantity element's nine-byte wrapper. A backstop (§12.6 row 15):
  // the re-encode below produces this byte, so an input carrying another cannot
  // equal it.
  expect byte_in(bytes, quantity_offset) == 0x49
  let sign = byte_in(bytes, quantity_offset + 1)
  // §12.2 — the sign byte is `00` or `01`. A backstop (§12.6 row 16), confirmed
  // by neutralisation: the re-encode spells the sign from the decoded value.
  expect or {
      sign == 0,
      sign == 1,
    }
  let magnitude = bytearray.to_int_big_endian(
    slice_exact(bytes, quantity_offset + 2, 8),
  )
  let statement = FaultStatementV1 {
    tx_id: slice_exact(bytes, 3, 32),
    code: byte_in(bytes, 36),
    field_index: byte_in(bytes, 38),
    item_index: bytearray.to_int_big_endian(slice_exact(bytes, 40, 3)),
    policy_id: slice_exact(bytes, policy_payload, policy_length),
    asset_name: slice_exact(bytes, name_payload, name_length),
    claimed: if sign == 1 {
      -magnitude
    } else {
      magnitude
    },
  }
  // §6.1 — re-encode what was read and demand the input back. This one line is
  // the canonicity check, the range check and the per-code shape check at once.
  expect encode_fault_statement(statement) == bytes
  statement
}

/// The 32-byte commitment a computation thread carries in place of the
/// statement itself, on the same terms §10.3 gives a checkpoint.
pub fn fault_statement_hash(statement: FaultStatementV1) -> ByteArray {
  blake2b_256(
    bytearray.concat(statement_domain, encode_fault_statement(statement)),
  )
}

// ---------------------------------------------------------------------------
// §12.3 proving a single bad item
// ---------------------------------------------------------------------------

/// Adjudicate a `fault_item_predicate` statement: is item `statement.item_index`
/// of `statement.field_index` one that `predicate` refuses?
///
/// **Exactly one item is read.** The walk relocates to the named index —
/// arithmetic on a fixed-stride field (§10.5), a walk on a variable-width one —
/// and `walk_next` reads that item and no other. Nothing about the item is
/// supplied by the challenger; the statement carries its address, and the bytes
/// come from the view the door authenticated. That is the whole of AC1 for this
/// fault kind, and `item_fault_reads_exactly_the_named_item` pins it against the
/// checkpoint the adjudication hands back.
///
/// The returned checkpoint is the position just past the accused item, so an
/// adjudication that runs out of budget composes with §10.4's fold rather than
/// starting over.
pub fn prove_item_fault(
  view: FieldViewV1,
  checkpoint: FieldWalkCheckpointV1,
  statement: FaultStatementV1,
  predicate: fn(Int, ByteArray) -> Bool,
) -> Pair<Bool, FieldWalkCheckpointV1> {
  // §12.3 — this adjudication answers one code, and a conservation statement
  // adjudicated as an item fault would be answering a question nobody asked.
  expect statement.code == fault_item_predicate
  // §12.1 — the shape rule is re-run here rather than assumed, because a
  // statement is a plain type and need not have come through §12.2's encoder.
  expect fault_statement_shape_is_exact(statement)
  // The statement accuses one transaction, and the walk is over one
  // transaction: they must be the same one. `walk_tx_id` reports the id the
  // §8.8 door authenticated against, so this is the line that turns "item 4 of
  // field 6 is fraudulent" from a claim about *some* transaction into a claim
  // about *this* one. Without it a statement would prove against any
  // transaction whose field 6 happened to hold a refused item at index 4.
  expect walk_tx_id(checkpoint) == statement.tx_id
  // The field is named by the statement *and* by the walk, and they must agree:
  // §4's plain hashing removed field-index domain separation, so a view of
  // field 1 answers every read a view of field 0 would (§10.2). Without this
  // line a statement about a spend input could be adjudicated against the
  // reference inputs.
  expect walk_field_index(checkpoint) == statement.field_index
  let advanced = walk_skip(
    view,
    checkpoint,
    statement.item_index - walk_next_item_index(checkpoint),
  )
  let Pair(item, next) = walk_next(view, advanced)
  Pair(!predicate(statement.item_index, item), next)
}

// ---------------------------------------------------------------------------
// §12.4 per-asset conservation
// ---------------------------------------------------------------------------

/// The outputs side of §12.4's equation, measured or still being measured.
///
/// **Why a type and not an `Int`.** The equation is
/// `outputs_total - mint_total`, and both halves are budgeted folds over
/// authenticated bytes. A bare `Int` says the number but none of the four things
/// that make it an answer: *whose* transaction it was taken over, *which* field,
/// *which unit*, and — the one a budgeted fold cannot do without — whether the
/// measurement **finished**. A caller handing `asset_conservation_fault_is_proven`
/// two integers it computed some other way is exactly the full-field re-supply
/// AC1 forbids, one step removed; a caller handing it two sweeps is handing it
/// two things only a sweep over the door's bytes can have produced.
///
/// **Why two types rather than one record with a field naming the side.** The
/// two sides are not the same measurement. This one reads every item and orders
/// nothing at field level; the mint one stops early on §5.6's key order and
/// therefore has to carry the last key it read. One record spanning both would
/// carry that key as a field that is dead here, and would re-decide which side it
/// was on at every entry point. As two types the question is asked once, by the
/// compiler: `accumulate_output_unit` cannot be handed a mint sweep,
/// `sweep_mint_unit` cannot be handed an outputs fold, and
/// `asset_conservation_fault_is_proven` cannot be handed its two arguments the
/// wrong way round. Three refusals that were runtime `expect`s — two carrying
/// vectors, the third one no fixture could isolate — are now three signatures
/// (§12.6).
///
/// **The measurement belongs to one invocation, and the type is what makes that
/// enforcement rather than a promise.** There is deliberately no encoder, no
/// decoder and no commitment-resume constructor here — the opposite of §10.3's
/// checkpoint — so no value of either sweep type crosses a step boundary, and the
/// only way to hold one is to have opened it in this script context over a view
/// the §8.8 door authenticated here. Together with the opener's refusal to start
/// anywhere but item 0, that settles what re-opening a sweep in a follow-on
/// transaction is: a fresh measurement of the whole field, never a resume of a
/// partial one — and a measurement of a *suffix*, which is what a mid-field open
/// would be, cannot be obtained at all. What a sweep does carry across is **budget
/// rounds inside one invocation**: a caller advances it as many times as it likes,
/// and the mint side's order key travels in the sweep so the check spans those
/// rounds. §12.4 states the consequence plainly — a transaction whose fields 2 and
/// 5 do not fit one invocation's budget is outside this adjudication's reach — and
/// §12.5's reading 2 is why that boundary sits where it does.
///
/// `is_final` is the load-bearing field. A fold that stopped because it ran out
/// of budget has measured a *prefix*, and a prefix of a sum is not the sum: an
/// unfinished outputs fold under-reports, and an unfinished mint sweep reports
/// `0` for a unit it simply has not reached yet. §12.4 says "a zero answer is
/// evidence, on both sides", and it is evidence precisely and only when the
/// sweep reached a verdict — the unit was found, or a key that sorts at or after
/// it was met, or the field ran out. Budget exhaustion is none of those, and it
/// is the one state that must never be spelled the same way as absence.
///
/// Opaque, unlike `FaultStatementV1`. A statement is an accusation and anybody
/// may write one (§12.1); a sweep is *evidence* — it asserts that a walk over
/// authenticated bytes reached a verdict — and a caller that could write one
/// could write `is_final: True` over a number it invented. That is §10.6's
/// distinction applied one layer up.
pub opaque type OutputUnitSweepV1 {
  /// The transaction the walk was authenticated against (`walk_tx_id`), so the
  /// measurement is bound to a transaction and not merely to some bytes.
  tx_id: ByteArray,
  /// The unit, carried so the adjudication can check that both halves of the
  /// equation are about the same one *and* about the statement's one.
  policy_id: ByteArray,
  asset_name: ByteArray,
  /// The position the next budget round continues from.
  checkpoint: FieldWalkCheckpointV1,
  /// The running sum over the outputs read so far.
  quantity: Int,
  /// Whether a verdict has been reached. See the type's own note — this is the
  /// field that keeps budget exhaustion from being spelled as absence.
  is_final: Bool,
}

/// The mint side of §12.4's equation (§5.6).
///
/// `OutputUnitSweepV1`'s note holds word for word — why a type and not an `Int`,
/// why two types and not one, why the measurement is one invocation's, and what
/// `is_final` is for. This type adds the one field the two sides do not share.
pub opaque type MintUnitSweepV1 {
  tx_id: ByteArray,
  policy_id: ByteArray,
  asset_name: ByteArray,
  checkpoint: FieldWalkCheckpointV1,
  /// §5.6's last policy key this sweep has read, carried across **budget
  /// rounds**. The strictly-increasing check is what licenses stopping early,
  /// and a check that reset at every round would not span the boundary between
  /// two of them — the one place an unordered field could slip a key past a
  /// sweep that paused. It lives in the sweep rather than in a recursion
  /// argument for exactly that reason.
  previous_policy: ByteArray,
  /// The matched entry's quantity, or `0` until a verdict says otherwise.
  quantity: Int,
  is_final: Bool,
}

/// Begin a per-asset measurement over the outputs field (§5.5).
pub fn open_output_unit_sweep(
  checkpoint: FieldWalkCheckpointV1,
  policy_id: ByteArray,
  asset_name: ByteArray,
) -> OutputUnitSweepV1 {
  // §12.4 — the four conditions a position must meet to be the start of a
  // measurement; `sweep_may_open` is where each is spelled and why.
  expect sweep_may_open(checkpoint, output_field_index, policy_id, asset_name)
  OutputUnitSweepV1 {
    tx_id: walk_tx_id(checkpoint),
    policy_id,
    asset_name,
    checkpoint,
    quantity: 0,
    // A walk that is complete at item 0 is a walk over an empty field, and an
    // empty field holds no units: `0` is the finished answer rather than an
    // unstarted one.
    is_final: walk_is_complete(checkpoint),
  }
}

/// Begin a per-asset measurement over the mint field (§5.6).
pub fn open_mint_unit_sweep(
  checkpoint: FieldWalkCheckpointV1,
  policy_id: ByteArray,
  asset_name: ByteArray,
) -> MintUnitSweepV1 {
  // §12.4 — as above; the only difference between the two sides here is which
  // §2.5 slot the constructor names.
  expect sweep_may_open(checkpoint, mint_field_index, policy_id, asset_name)
  MintUnitSweepV1 {
    tx_id: walk_tx_id(checkpoint),
    policy_id,
    asset_name,
    checkpoint,
    previous_policy: #"",
    quantity: 0,
    // `open_output_unit_sweep`'s reason: an empty mint field mints nothing, and
    // that `0` is measured.
    is_final: walk_is_complete(checkpoint),
  }
}

/// What both sides require of the position they are opened at.
///
/// The field is named by the constructor, never by a caller: §12.4 has exactly
/// two sides and each knows its own slot, so there is no argument to get wrong.
fn sweep_may_open(
  checkpoint: FieldWalkCheckpointV1,
  field_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
) -> Bool {
  and {
    // §12.4 — the sweep's field is the walk's. §4's plain hashing removed
    // field-index domain separation, so a view of field 6 answers every read a
    // view of field 2 would and nothing else would notice.
    walk_field_index(checkpoint) == field_index,
    // §12.4 — **a sweep measures a whole field, so it begins at item 0.** This
    // is what makes a mint `0` mean "the transaction does not mint this unit"
    // rather than "the part of the field I was aimed at does not". Forward
    // relocation is free and public (§10.4's `walk_skip`), so without this line a
    // challenger opens on the *suffix* past the target's policy group, meets a
    // key that sorts after the request, and comes back `is_final` with a
    // fabricated `0` that every check in `sweep_answers_statement` accepts —
    // proving a conservation fault that does not exist. It is also what makes
    // re-opening in a later transaction a fresh whole-field measurement rather
    // than a resume of a partial one.
    walk_next_item_index(checkpoint) == 0,
    // §12.1 — the unit is a full 28-byte policy id: a short one would name a set
    // of policies and the sweep would find none of them.
    bytearray.length(policy_id) == policy_id_bytes,
    // §5.3 — and an asset name the grammar admits.
    bytearray.length(asset_name) <= max_asset_name_bytes,
  }
}

/// The position this fold has reached — a **reader**, not a resume route.
///
/// It is §10.3's checkpoint, so a caller that has finished measuring may go on
/// walking field 2 from here, and `mint_unit_quantity` hands its own back for
/// that reason. What no caller can do is feed it to `open_output_unit_sweep` and
/// call the result a continuation: the opener admits item 0 and nothing else, so
/// every sweep is a measurement of its field from the start.
pub fn output_sweep_checkpoint(
  sweep: OutputUnitSweepV1,
) -> FieldWalkCheckpointV1 {
  sweep.checkpoint
}

/// Whether the measurement reached a verdict. A caller drives its fold through
/// as many budget rounds as it needs until this is `True`; an adjudication
/// refuses one for which it is not.
pub fn output_sweep_is_final(sweep: OutputUnitSweepV1) -> Bool {
  sweep.is_final
}

/// The measured quantity — **only** from a finished fold.
///
/// Reading a prefix's running total as if it were the answer is exactly what
/// §12.4's "a zero answer is evidence" rules out, so this fails closed rather
/// than handing back a number whose meaning depends on a field the caller might
/// not have looked at.
pub fn output_sweep_quantity(sweep: OutputUnitSweepV1) -> Int {
  // §12.4 — budget exhaustion is not absence, and a running total is not a sum.
  expect sweep.is_final
  sweep.quantity
}

/// `output_sweep_checkpoint`'s mint twin, on the same terms.
pub fn mint_sweep_checkpoint(sweep: MintUnitSweepV1) -> FieldWalkCheckpointV1 {
  sweep.checkpoint
}

/// `output_sweep_is_final`'s mint twin. Here the three verdicts are the unit's
/// group being found, a key sorting at or after it being met, or the field
/// running out — see `sweep_mint_unit`.
pub fn mint_sweep_is_final(sweep: MintUnitSweepV1) -> Bool {
  sweep.is_final
}

/// `output_sweep_quantity`'s mint twin, and the same fail-closed rule: an
/// unfinished mint sweep's running `0` is the exact fabrication §12.4 exists to
/// refuse.
pub fn mint_sweep_quantity(sweep: MintUnitSweepV1) -> Int {
  // §12.4 — a `0` that was not measured is not evidence of absence.
  expect sweep.is_final
  sweep.quantity
}

/// Add the named unit's quantity in the next `budget` output items to the
/// sweep's running total.
///
/// One unit, one lookup per output, no output materialised: each item is opened
/// as a §11.1 Value bookmark and asked for `(policy_id, asset_name)` exactly
/// once. The bookmark's structural pass is what makes each `0` evidence rather
/// than an unobserved absence, and asking once per bookmark is inside its
/// monotone floor by construction.
///
/// Budgeted like every other §10 fold, so a caller may take the field in as many
/// rounds as its own recursion budget wants — but **within one invocation**: a
/// fold is a running measurement, not a position, and §12.4's contract is that
/// the field it measures is measured whole, here. The outputs side finishes
/// exactly when the walk does — every output holds a value, so every output must
/// be read; there is no early stop here and none is sound.
pub fn accumulate_output_unit(
  view: FieldViewV1,
  sweep: OutputUnitSweepV1,
  budget: Int,
) -> OutputUnitSweepV1 {
  // §12.4 — a budget is a count of items, and a negative one is not a smaller
  // fold but a nonterminating one.
  expect budget >= 0
  fold_output_unit(view, sweep, budget)
}

fn fold_output_unit(
  view: FieldViewV1,
  sweep: OutputUnitSweepV1,
  budget: Int,
) -> OutputUnitSweepV1 {
  if walk_is_complete(sweep.checkpoint) {
    OutputUnitSweepV1 { ..sweep, is_final: True }
  } else if budget == 0 {
    // Out of budget with outputs still unread: a prefix, and it says so.
    sweep
  } else {
    let Pair(item, advanced) = walk_next(view, sweep.checkpoint)
    let Pair(quantity, _bookmark) = value_quantity_of(
      open_value_bookmark(item),
      sweep.policy_id,
      sweep.asset_name,
    )
    fold_output_unit(
      view,
      OutputUnitSweepV1 {
        ..sweep,
        checkpoint: advanced,
        quantity: sweep.quantity + quantity,
      },
      budget - 1,
    )
  }
}

/// Advance the mint sweep by at most `budget` policy items.
///
/// §5.6's policy items are in canonical key order, so this sweep is monotone for
/// §11.1's reason and for the same payoff: it stops at the first policy that
/// sorts at or after the requested one. Three states end it — the unit's group
/// was found, a group that sorts at or after it was met, or the field ran out —
/// and each is a verdict, which is what makes the `0` of the last two evidence
/// instead of "not seen yet". Running out of budget is **not** one of them: the
/// sweep comes back with `is_final` still false and the next budget round,
/// inside the same invocation, resumes it.
///
/// The order is enforced as it sweeps, across budget rounds, over exactly the
/// prefix the sweep reads — `previous_policy` lives in the sweep rather than in
/// a recursion argument, so a round compares its first key against the last key
/// of the round before. That prefix is precisely what the early stop rests on: an
/// unordered prefix would let a sweep pass the unit and then meet it, and the `0`
/// it reported on the way would have been false.
///
/// **What it does not do**, stated because the payoff above is exactly why: it
/// reads no key at or after the one it stopped on, so it cannot see a §5.6
/// violation *there* — a field repeating the requested policy after the matched
/// group would have its second entry's quantity dropped. Curing that inside this
/// sweep means reading every remaining item, which is the early stop, which is
/// the property §12.4 is built on. What is out of reach *there* is out of reach
/// for the transaction as a whole only if nothing else establishes it, and
/// something does: a field's canonical order is settled once, at admission, by
/// the node's canonical decode of the submitted transaction — the residue and its
/// owner are named in §12.4. This sweep re-checks the part it can reach for free.
/// The same shape holds for §11.1's `sweep_to_unit` one level down.
pub fn sweep_mint_unit(
  view: FieldViewV1,
  sweep: MintUnitSweepV1,
  budget: Int,
) -> MintUnitSweepV1 {
  // §12.4 — `accumulate_output_unit`'s reason: a budget counts items.
  expect budget >= 0
  if sweep.is_final {
    sweep
  } else {
    sweep_mint_policies(view, sweep, budget)
  }
}

fn sweep_mint_policies(
  view: FieldViewV1,
  sweep: MintUnitSweepV1,
  budget: Int,
) -> MintUnitSweepV1 {
  if walk_is_complete(sweep.checkpoint) {
    // Every key sorted before the request and the field ran out. Absent, and
    // the field was read to the end to say so.
    MintUnitSweepV1 { ..sweep, is_final: True }
  } else if budget == 0 {
    // Out of budget short of a verdict. Deliberately *not* final: this is the
    // state a `Pair(0, checkpoint)` return would have made indistinguishable
    // from genuine absence.
    sweep
  } else {
    let Pair(item, advanced) = walk_next(view, sweep.checkpoint)
    let Pair(group_policy, assets_offset) = mint_item_head(item)
    // §5.6: strictly increasing policy keys. `previous_policy` comes off the
    // sweep, so this spans budget rounds; the first key of the whole sweep has
    // no predecessor, and a real policy id is 28 bytes, never empty.
    expect or {
        sweep.previous_policy == #"",
        canonical_key_precedes_v1(sweep.previous_policy, group_policy),
      }
    let stepped = MintUnitSweepV1 {
      ..sweep,
      checkpoint: advanced,
      previous_policy: group_policy,
    }
    if group_policy == sweep.policy_id {
      MintUnitSweepV1 {
        ..stepped,
        quantity: mint_item_asset_quantity(
          item,
          assets_offset,
          sweep.asset_name,
        ),
        is_final: True,
      }
    } else if canonical_key_precedes_v1(group_policy, sweep.policy_id) {
      sweep_mint_policies(view, stepped, budget - 1)
    } else {
      // The field is ordered and this group already sorts after the request, so
      // no later group carries it either. Quantity stays `0`, and it is now a
      // measured `0`.
      MintUnitSweepV1 { ..stepped, is_final: True }
    }
  }
}

/// The named unit's mint quantity — positive for a mint, negative for a burn,
/// `0` when the transaction does not touch it.
///
/// **The whole mint side in one call**, which after §12.4's single-invocation
/// contract is the normal way to take it rather than a shortcut past the sweep
/// seam: the sweep types exist so that the *adjudication* can require evidence,
/// and a caller that only wants the number should not have to spell three calls
/// to get it. It fails closed when `budget` did not reach a verdict, because
/// `mint_sweep_quantity` does: there is no spelling of this function that hands
/// back a `0` it did not measure. The sweep seam stays public underneath it for
/// the caller that wants to interleave rounds with other work, and
/// `asset_conservation_fault_is_proven` takes the sweep and not this pair.
pub fn mint_unit_quantity(
  view: FieldViewV1,
  checkpoint: FieldWalkCheckpointV1,
  policy_id: ByteArray,
  asset_name: ByteArray,
  budget: Int,
) -> Pair<Int, FieldWalkCheckpointV1> {
  let swept = sweep_mint_unit(
    view,
    open_mint_unit_sweep(checkpoint, policy_id, asset_name),
    budget,
  )
  Pair(mint_sweep_quantity(swept), mint_sweep_checkpoint(swept))
}

/// §5.6's `82 ‖ 58 1C policy_id ‖ map(k) ‖ assets` — the policy id, and the
/// offset its asset map begins at.
///
/// Both halves come back from one call because the sweep needs both — the key to
/// compare, the offset to read the group behind it — and a second reader for the
/// same preamble would be a second verdict on one grammar, which §6.1 is
/// precisely about not having.
fn mint_item_head(item: ByteArray) -> Pair<ByteArray, Int> {
  // §5.6 — a mint item is the two-element array and nothing else.
  expect byte_in(item, 0) == 0x82
  let Pair(policy_offset, policy_length) = head_at(item, 1, 2)
  // §5.6 — its key is a full 28-byte policy id.
  expect policy_length == policy_id_bytes
  Pair(
    slice_exact(item, policy_offset, policy_length),
    policy_offset + policy_length,
  )
}

/// The quantity of `asset_name` inside one §5.6 policy item, `0` if absent.
/// `map_offset` is `mint_item_head`'s second component.
///
/// The group's own key order is enforced here for the same reason the field's is
/// enforced above: without it the scan could pass the name and then meet it, and
/// the `0` would be fabricated. Unlike the field-level sweep this one never
/// resumes — a policy item is read whole by a single `walk_next` — so its order
/// check spans the entire group it is asked about. Quantities are checked
/// non-zero because §5.6 gives an absent asset exactly one spelling, and a zero
/// entry would be a second one — which is precisely how a conservation fault
/// would be hidden.
fn mint_item_asset_quantity(
  item: ByteArray,
  map_offset: Int,
  asset_name: ByteArray,
) -> Int {
  let Pair(after_map_head, asset_count) = head_at(item, map_offset, 5)
  // §5.6 — a policy group is non-empty; §5.5/§5.6's encoders give an empty one
  // no spelling, so an empty map here is not a group with nothing in it but a
  // field that was not produced by them.
  expect asset_count > 0
  scan_mint_assets(item, after_map_head, asset_count, asset_name, #"", False)
}

/// §5.6's asset map, scanned monotonically for one name.
///
/// **Why this is not §11.1's `scan_assets`,** which reads a §5.5 asset map with
/// the same shape one level down. Two of §11.1's own invariants are wrong here
/// and would be wrong silently: a §5.5 value quantity is strictly *positive*
/// where a §5.6 mint quantity is a signed `int` whose only forbidden value is
/// zero (a burn is the whole point), and §11.1 scans a value's group as part of a
/// bookmark's structural pass over the *entire* item, where this scan is entered
/// only for the one group a §12.4 sweep matched and never re-enters it. Sharing a
/// reader would mean a reader parameterised by "which sign rule" and "who owns
/// the cursor", which is a §6.1 hazard rather than a saving: one function with
/// two verdicts in it. The two therefore stay separate readers of two grammars
/// that resemble each other, and each says so. §11.1's `scan_assets` carries the
/// matching note.
fn scan_mint_assets(
  item: ByteArray,
  offset: Int,
  remaining: Int,
  asset_name: ByteArray,
  previous_name: ByteArray,
  seen_any: Bool,
) -> Int {
  if remaining <= 0 {
    0
  } else {
    let Pair(name_offset, name_length) = head_at(item, offset, 2)
    // §5.3 — an asset name is at most 32 bytes.
    expect name_length <= max_asset_name_bytes
    let name = slice_exact(item, name_offset, name_length)
    // §5.6 — keys strictly increase inside a group. `seen_any` rather than an
    // empty `previous_name`, because §5.3 admits the empty asset name and it
    // would otherwise be indistinguishable from "no predecessor".
    expect or {
        !seen_any,
        canonical_key_precedes_v1(previous_name, name),
      }
    let Pair(after_quantity, quantity) = mint_quantity_at(
      item,
      name_offset + name_length,
    )
    // §5.6 — a mint quantity is non-zero. A zero entry would be a second
    // spelling of an absent asset, which is exactly where a conservation fault
    // would hide; note this is *not* §5.5's rule, which is strictly positive.
    expect quantity != 0
    if name == asset_name {
      quantity
    } else if canonical_key_precedes_v1(name, asset_name) {
      scan_mint_assets(
        item,
        after_quantity,
        remaining - 1,
        asset_name,
        name,
        True,
      )
    } else {
      // Ordered, and this name already sorts after the request.
      0
    }
  }
}

/// A mint quantity is signed (§5.6 admits burns), so the head may be major 0 or
/// major 1. Both go through §11's one head reader, which is what keeps the
/// minimality bound identical to every other number in these grammars (§6.1);
/// a major outside `{0, 1}` aborts there rather than here.
fn mint_quantity_at(item: ByteArray, offset: Int) -> Pair<Int, Int> {
  if byte_in(item, offset) / 32 == 0 {
    head_at(item, offset, 0)
  } else {
    let Pair(after, magnitude) = head_at(item, offset, 1)
    Pair(after, -1 - magnitude)
  }
}

/// Adjudicate a `fault_asset_conservation` statement.
///
/// The net creation of one unit by a transaction is what its outputs hold minus
/// what its mint field says it made: `outputs_total - mint_total`. The statement
/// is **proven** when that disagrees with the quantity the operator committed.
///
/// The two totals are not integers a caller chose. Each arrives as the sweep
/// that produced it, and what this checks before doing any arithmetic are the
/// four ways a number could fail to be an answer to *this* statement: it was
/// measured over another transaction, over the wrong field, about a unit the
/// statement does not name, or over a prefix rather than to a verdict. The
/// second of those four is not a line below — it is the signature: an
/// `OutputUnitSweepV1` can only have been opened on a field-2 walk and a
/// `MintUnitSweepV1` on a field-5 walk, and the two arguments cannot be
/// exchanged. Binding both sides to the statement's own unit also binds them to
/// each other, so the two halves of the subtraction cannot be about different
/// assets. A budget-exhausted sweep is refused here as well as at the two
/// quantity readers, because a fault "proven" against a prefix is a fault that
/// does not exist.
///
/// The statement's own `field_index` and `item_index` are adjudicated too, by
/// the shape rule: §12.4 reads both fields from item 0, so the only spellings
/// that mean anything are the ones `fault_statement_shape_is_exact` pins. That
/// is what stops two statements differing only in `field_index` — two distinct
/// `fault_statement_hash`es — from being the same accusation.
///
/// This is deliberately the equation over the transaction's *own committed
/// fields*. The resolved-input half of a full conservation law lives in the
/// machine's ledger state and not in this document's domain (§12.4), so it is
/// not smuggled in here as a redeemer argument — which would be exactly the
/// full-field re-supply AC1 forbids.
pub fn asset_conservation_fault_is_proven(
  statement: FaultStatementV1,
  outputs: OutputUnitSweepV1,
  mint: MintUnitSweepV1,
) -> Bool {
  // §12.4 — this adjudication answers one code; an item statement carries no
  // unit for the two sweeps to be about.
  expect statement.code == fault_asset_conservation
  // §12.1 — the shape rule again, for `prove_item_fault`'s reason, and here it
  // is also what pins the two positional scalars this path does not read.
  expect fault_statement_shape_is_exact(statement)
  // §12.4 — the outputs total is an answer to this statement and not to some
  // other question.
  expect
    sweep_answers_statement(
      statement,
      outputs.tx_id,
      outputs.policy_id,
      outputs.asset_name,
      outputs.is_final,
    )
  // §12.4 — and the mint total likewise, on identical terms.
  expect
    sweep_answers_statement(
      statement,
      mint.tx_id,
      mint.policy_id,
      mint.asset_name,
      mint.is_final,
    )
  output_sweep_quantity(outputs) - mint_sweep_quantity(mint) != statement.claimed
}

/// Is this sweep an answer to this statement? Split out because both halves are
/// held to it identically, and because "which of these did the caller get wrong"
/// is easier to read as one list than as two inlined ones.
///
/// It takes the four scalars rather than a sweep because the two sides are two
/// types now — which is the point: the fifth condition, "and it was measured over
/// the right field", is discharged by the caller's parameter types before this
/// function is reached.
fn sweep_answers_statement(
  statement: FaultStatementV1,
  tx_id: ByteArray,
  policy_id: ByteArray,
  asset_name: ByteArray,
  is_final: Bool,
) -> Bool {
  and {
    tx_id == statement.tx_id,
    policy_id == statement.policy_id,
    asset_name == statement.asset_name,
    is_final,
  }
}
