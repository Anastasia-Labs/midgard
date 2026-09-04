//// How a **family step downstream of its own step-01** reaches one of the nine
//// committed fields — `docs/spec/midgard-tx.md` §4, §8.8, §10.
////
//// `midgard/native_tx_field_access_v1.authenticated_field_view` is the one
//// door, and it takes the tx-id-verified *compact structures*, never a
//// free-standing field hash. A family step-01 has those structures in hand
//// (`pass_native_tx_to_next_step` hands them over); a step-02 and beyond does
//// not — it arrives holding whatever the computation thread's datum carried
//// forward. This module is the bridge, and it is a bridge with exactly one
//// plank: **the thread carries the transaction id, and the step re-derives the
//// compact structures from redeemer bytes bound to it.**
////
//// Why the tx-id and not the field hash. Under §4's plain hashing a field
//// commitment carries no field index and no domain tag, so field 0 and field 1
//// preimages with the same items hash alike, and a hash on its own says
//// nothing about which slot it came from. Carrying the *hash* forward is what
//// forced every downstream step to re-hash a whole reproduced item list to use
//// it — the shape measured as the Q1X-F6 defect (#551), where reproducing the
//// spend-input collection put Q10/Q11 past the ledger's memory cap at the
//// admissible 296-input cardinality. Carrying the *tx-id* forward instead
//// costs the same 32 bytes of thread state and buys the door: positional
//// extraction happens inside it, and the read that follows is a slice.
////
//// What binds what:
////
////   * the **anchor** comes from thread state, which the family's own step-01
////     wrote off the compact structure the block's `transactions_root`
////     committed. It is never a redeemer argument here.
////   * `native_tx_compact_cbor` comes from the redeemer, and
////     `verify_native_tx_compact_cbor_v1` re-derives the id from it: wrong
////     bytes simply fail. Nothing is trusted about its provenance.
////   * the field index is a **literal at the call site**, never a redeemer
////     argument, so a step that means to read field 0 cannot be steered onto
////     field 1 — which matters precisely because §4 removed the domain
////     separation that used to make that impossible.
////   * the carriage tier is the prover's choice among §8's three, and the door
////     verifies whichever is named — for every field, including 6–8 under
////     tier 3 since #606's repair (owner ruling 2026-08-16). The §8.6
////     certificate datum carries a mint-welded `field_hash`, and the door
////     requires it to equal the commitment reached from the **anchored**
////     structures — for a witness-set field, the chain that reaches the
////     anchored `witness_set_hash` — so a certificate minted against a
////     fabricated witness set wears the fabricated hash and fails at the
////     door. The former fields-6–8 tier-3 refusal
////     (`carriage_reaches_the_anchor`, §8.3 erratum E2 limit 3) lifted with
////     that weld.
////
//// A step that opens **more than one field** of the same transaction says so
//// once. `anchored_native_tx` verifies the anchor and hands back an
//// `AnchoredNativeTxV1`; `anchored_field_view` and `anchored_field_walk` then
//// open each field against it, one carriage per field, with the §2.5 pairing
//// re-run every time — the one per-field guard this module owns, since #606's
//// repair deleted the second (`carriage_reaches_the_anchor`; see
//// `field_pairs_with`). The single-field `opened_field_view` and
//// `opened_field_walk` below are exactly that sequence with one field in
//// it. Why the split is worth a type: re-deriving the transaction id costs
//// roughly 211k memory units, and a two-field step through the single-field
//// entry points paid it twice over — while the obvious cheap fix, reaching
//// `authenticated_field_view` directly with one pre-verified structure, buys
//// the saving by stepping outside that guard.
////
//// The `Body`/`Witness` split is §2.5's field table made structural. Fields
//// 0–5 are committed by the transaction body and the door reads them without
//// consulting any witness set; fields 6–8 live in the witness set, and the
//// door will only read them once the supplied `NativeTxWitnessSetCompact`
//// re-derives to a `witness_set_hash` **the thread anchored**. Having two
//// constructors rather than one optional field means a step cannot supply a
//// witness set that is silently ignored, and cannot omit one that is silently
//// required: `opened_field_view` refuses the mismatch.
////
//// The anchor is split the same way, and for a reason that is easy to get
//// wrong: **the transaction id does not commit the witness set.** §3's
//// two-level split hashes the *body* CBOR alone, so of the §2.5 compact
//// structure's three parts — body, `witness_set_hash`, `validity_code` — the id
//// covers exactly one. Re-deriving the id from redeemer bytes therefore pins
//// fields 0–5 and pins nothing whatever about fields 6–8: a prover may hand
//// over the genuine body followed by any trailing 34 bytes it likes, and those
//// bytes re-derive to the same transaction id. So the `witness_set_hash` has to
//// arrive from somewhere the prover does not choose, and the only such place is
//// the same thread state the id arrives from — see `NativeTxAnchorV1`.

use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_compact_cbor_v1}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxWitnessSetCompact, VerifiedMidgardNativeTxCompact,
}
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, FieldViewV1, authenticated_field_view, field_item_count,
}
use midgard/native_tx_machine_walk_v1.{
  FieldGrammarCheckpointV1, FieldWalkCheckpointV1,
  open_certified_field_walk_from_grammar_commitment,
  open_field_grammar_certification, open_field_walk,
  resume_field_grammar_certification_from_commitment,
  resume_field_walk_from_commitment, walk_fold, walk_is_complete,
}

// §2.5's positional field table, named. Call sites pass one of these literals
// rather than a redeemer argument, so which slot a step reads is a property of
// the compiled validator. Spelling them out is not decoration: §4 removed
// field-index domain separation, so fields 0 and 1 — and 3 and 4 — commit
// identically for identical items, and the index is the *only* thing that tells
// a reference-input opening from a spend-input one.

pub const spend_inputs_field_index: Int = 0

pub const reference_inputs_field_index: Int = 1

pub const outputs_field_index: Int = 2

pub const required_observers_field_index: Int = 3

pub const required_signers_field_index: Int = 4

pub const mint_field_index: Int = 5

pub const script_witnesses_field_index: Int = 6

pub const address_witnesses_field_index: Int = 7

pub const redeemers_field_index: Int = 8

/// §2.5's split point: fields 0–5 are the body's, 6–8 the witness set's. Named
/// off the table rather than restated as its own literal, so the boundary and
/// the indices cannot drift apart.
pub const first_witness_set_field_index: Int = script_witnesses_field_index

/// Everything a downstream step needs to re-open one field of the transaction
/// its thread is disputing, and nothing it does not.
///
/// Constructor order is wire format: `BodyFieldOpening` is Constr 0,
/// `WitnessFieldOpening` Constr 1.
pub type FieldOpeningV1 {
  /// Fields 0–5. No witness set is consulted, so none is carried — a step
  /// reading the body cannot be handed one to ignore.
  BodyFieldOpening {
    native_tx_compact_cbor: ByteArray,
    carriage: FieldCarriageV1,
  }
  /// Fields 6–8. `witness_set` is unauthenticated on arrival, and so — for
  /// these fields — is `native_tx_compact_cbor`'s trailing `witness_set_hash`:
  /// the transaction id does not commit it (see the module header). Both are
  /// checked against the thread's `WitnessAnchor` before anything is read.
  WitnessFieldOpening {
    native_tx_compact_cbor: ByteArray,
    witness_set: NativeTxWitnessSetCompact,
    carriage: FieldCarriageV1,
  }
}

/// The transaction-identifying half of an opening: everything that binds the
/// disputed transaction to the thread's anchor, with **no carriage attached**.
///
/// `FieldOpeningV1` pairs one of these with exactly one carriage, because a step
/// that opens one field has exactly one preimage to carry. A step that opens
/// *two* has two preimages and still only one transaction, and that asymmetry is
/// what this type exists for: the anchor is paid once (`anchored_native_tx`) and
/// each field then names its own carriage (`anchored_field_view`,
/// `anchored_field_walk`).
///
/// **Internal representation, not a frozen wire surface.** `FieldOpeningV1` is
/// a redeemer field on the Q1x step validators, so its constructor order is wire
/// format and cannot move; this type appears in no datum, redeemer or validator
/// parameter, has no SDK twin, and is only ever built inside the caller that
/// immediately hands it to `anchored_native_tx`. Its constructors are laid out in
/// `FieldOpeningV1`'s order for readability — same §2.5 split, same order — and
/// nothing off-chain depends on that.
pub type NativeTxOpeningV1 {
  /// Fields 0–5. No witness set is consulted, so none is carried.
  BodyTxOpening { native_tx_compact_cbor: ByteArray }
  /// Fields 6–8. Both members are unauthenticated on arrival and are checked
  /// against the thread's `WitnessAnchor` before anything is read.
  WitnessTxOpening {
    native_tx_compact_cbor: ByteArray,
    witness_set: NativeTxWitnessSetCompact,
  }
}

/// What a family's own step-01 must put in thread state for a downstream step
/// to re-open a field of the transaction the thread is disputing.
///
/// Both constructors are call-site literals, never redeemer arguments: which
/// one a step uses is a property of the compiled validator, exactly as the
/// field index is.
///
/// The two arms exist because §3's tx-id preimage is the body alone. For fields
/// 0–5 the id is a complete anchor and `BodyAnchor` carries it. For fields 6–8
/// it is not an anchor at all — the `witness_set_hash` sits *outside* the
/// preimage — so `WitnessAnchor` carries that hash alongside it. The value must
/// be the one read off the compact structure the block's counted
/// `transactions_root` committed (`pass_native_tx_to_next_step` hands the
/// step-01 that structure already authenticated); anything a later redeemer
/// supplies is the prover's own and anchors nothing.
///
/// Constructor order is wire format: `BodyAnchor` is Constr 0, `WitnessAnchor`
/// Constr 1.
pub type NativeTxAnchorV1 {
  /// Fields 0–5. 32 bytes of thread state.
  BodyAnchor { tx_id: ByteArray }
  /// Fields 6–8. 64 bytes of thread state — the same width the retired
  /// per-collection-hash idiom cost, spent on a value that covers all three
  /// witness-set fields instead of one.
  WitnessAnchor { tx_id: ByteArray, witness_set_hash: ByteArray }
}

/// The witness set handed to the door for a body field.
///
/// The door's signature takes one for every field, but for `field_index < 6` it
/// short-circuits before looking (§2.5's table is positional and the body's
/// commitments are in the body). Constructing the unread value **here** rather
/// than taking one from the redeemer is the point: there is no argument for a
/// prover to fill, so nothing can ride in under a field the door will not read.
fn unread_witness_set() -> NativeTxWitnessSetCompact {
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: #"",
    script_tx_wits_hash: #"",
    redeemer_tx_wits_hash: #"",
  }
}

/// A disputed transaction whose **anchor has already been paid for**: the
/// compact structures re-derived from redeemer bytes and checked against thread
/// state, once, however many fields the step goes on to open.
///
/// Opaque on purpose. `anchored_native_tx` is the only way to obtain one, so
/// "these structures were checked against the thread's anchor" is a property of
/// the value rather than a claim its holder makes about itself. A step cannot
/// assemble the cheap version by hand and pass it off as the checked one.
///
/// `opens_witness_set_fields` records which §2.5 half the opening was for. It
/// is not a convenience: the split has to stay enforced **per field opened**,
/// because a handle checked out under a body opening must still be refused at
/// field 6 — otherwise the multi-field form would be a way around the very
/// pairing the single-field form enforces.
pub opaque type AnchoredNativeTxV1 {
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  opens_witness_set_fields: Bool,
}

/// Pay the anchor once (§7.1, one level out from the door's own
/// authenticate-once).
///
/// Both levels §2.5 has are checked here, exactly as the single-field entry
/// points always checked them: `verify_native_tx_compact_cbor_v1` re-derives the
/// transaction id from the supplied bytes, and for a witness-set opening the
/// re-derived compact structure's `witness_set_hash` must be the **anchored**
/// one. What is *not* checked here is anything per-field — the field index and
/// the carriage tier are `anchored_field_view`'s and `anchored_field_walk`'s,
/// because a handle exists precisely to be opened at more than one of each.
///
/// **Why this is a separate function at all.** `verify_native_tx_compact_cbor_v1`
/// re-derives the id by hashing the body CBOR, and that hash is the dominant
/// per-opening cost — measured at roughly 211k memory units on the Q21 vector.
/// A step opening two fields through the single-field entry points paid it
/// twice, and paid it a third time if it also needed the compact structure for
/// itself. The saving is not available by reaching `authenticated_field_view`
/// directly: that route leaves the caller outside the §2.5 pairing, which is
/// the guard #575 closed a reproduced forgery with. So the opening that pays
/// once lives here, where the guard still applies to every field it opens.
pub fn anchored_native_tx(
  opening: NativeTxOpeningV1,
  anchor: NativeTxAnchorV1,
) -> AnchoredNativeTxV1 {
  when opening is {
    BodyTxOpening { native_tx_compact_cbor } -> {
      // 1. The thread anchored a body dispute. A `WitnessAnchor` here would mean
      //    the family's own step-01 recorded a witness-set anchor and this step
      //    is opening it without the witness set that anchor names, so the two
      //    §2.5 halves have to agree before any byte is read.
      expect BodyAnchor { tx_id } = anchor
      AnchoredNativeTxV1 {
        verified: verify_native_tx_compact_cbor_v1(
          tx_id,
          native_tx_compact_cbor,
        ),
        witness_set: unread_witness_set(),
        opens_witness_set_fields: False,
      }
    }
    WitnessTxOpening { native_tx_compact_cbor, witness_set } -> {
      // 1. The thread anchored a witness-set dispute. A `BodyAnchor` carries no
      //    `witness_set_hash`, so there would be nothing for check 2 to
      //    authenticate the supplied witness set against — the two §2.5 halves
      //    have to agree before any byte is read.
      expect WitnessAnchor { tx_id, witness_set_hash } = anchor
      let verified = verify_native_tx_compact_cbor_v1(
        tx_id,
        native_tx_compact_cbor,
      )
      // 2. The check the transaction id cannot make for itself. Without it a
      //    prover supplies the genuine body — which re-derives to `tx_id` and so
      //    satisfies check 1 — followed by a `witness_set_hash` of its own
      //    choosing, and then "authenticates" a witness set of its own choosing
      //    against it. The empty witness set is the useful forgery: it makes
      //    every absence rule (§2.5 fields 6 and 7) true of every transaction,
      //    which is a slashing proof against an honest operator.
      expect verified.tx_compact.witness_set_hash == witness_set_hash
      AnchoredNativeTxV1 {
        verified,
        witness_set,
        opens_witness_set_fields: True,
      }
    }
  }
}

/// The native-transaction version the anchored compact structure was committed
/// under.
///
/// **Anchored.** `verify_native_tx_compact_cbor_v1` requires the version byte to
/// be present in the compact bytes *and* feeds it to `native_tx_id_for_version`
/// alongside the body, so it is inside §3's id derivation rather than beside it.
pub fn anchored_native_tx_version(anchored: AnchoredNativeTxV1) -> Int {
  anchored.verified.version
}

/// The §2.5 `validity_code` sitting in the compact bytes this handle was built
/// from. **Named for what it is not**: every other function here says `anchored`
/// because the anchor covers what it returns, and this one does not, so it does
/// not claim it.
///
/// **NOT anchored, and that is not a defect this function can fix.** §3's id
/// preimage is the body CBOR alone, so of the compact structure's four parts —
/// version, body, `witness_set_hash`, `validity_code` — re-deriving the id pins
/// the first two and says nothing about the last two. A prover may hand over the
/// genuine body followed by any trailing `validity_code` it likes and the bytes
/// still re-derive to the committed transaction id. A rule that asserts something
/// *about* this value is asserting it about the prover's bytes; a rule that needs
/// the committed value must reach it the way the block committed it, through the
/// structure its own step-01 was handed.
///
/// It is exposed anyway, and narrowly, because the transition-trace family's
/// `validity_code == 0` read predates the handle and is unchanged by it — the
/// alternative to naming the limit here was leaving that call site to obtain the
/// whole `VerifiedMidgardNativeTxCompact` and take the value off it silently.
/// That call site is sound for a reason this handle knows nothing about: its
/// compact bytes come out of a `transactions_root` leaf whose exact bytes it
/// re-checks, so the value is committed by the *root*, not by the anchor. Any new
/// caller owes the same argument in its own words, which is what the name is for.
///
/// **There is deliberately no accessor for `witness_set_hash`.** For a
/// `WitnessTxOpening` handle it is the anchored value (checked in
/// `anchored_native_tx`), but for a `BodyTxOpening` handle it is the prover's,
/// and the handle does not say which arm produced it. Handing it out would put
/// the one ingredient #575's reproduced forgery needs — an unanchored
/// `witness_set_hash` to authenticate an arbitrary witness set against — one
/// `authenticated_field_view` call away from a value the type system labels
/// anchored. Reading a witness-set field goes through `anchored_field_view` or
/// `anchored_field_walk`, which is where the §2.5 pairing lives.
pub fn unanchored_validity_code_of(anchored: AnchoredNativeTxV1) -> Int {
  anchored.verified.tx_compact.validity_code
}

/// Authenticate field `field_index` of an already-anchored transaction and hand
/// back a view to slice.
///
/// Every guard the single-field `opened_field_view` applies is applied here, and
/// applied **per field**: the §2.5 body/witness pairing against the half the
/// handle was opened under. Opening a second field re-runs it; only the anchor
/// is shared.
///
/// §7.1 authenticate-once is the caller's to exploit exactly as it is through
/// the door itself: open once per field per step, read the view many times.
pub fn anchored_field_view(
  anchored: AnchoredNativeTxV1,
  field_index: Int,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> FieldViewV1 {
  // 1. The per-field guard, re-run on every opening this handle serves: the
  //    §2.5 half the anchor was paid under against the half `field_index`
  //    lives in.
  expect field_pairs_with(anchored, field_index)
  authenticated_field_view(
    anchored.verified,
    anchored.witness_set,
    field_index,
    carriage,
    reference_inputs,
    certificate_policy_id,
  )
}

/// The same per-field authentication, opened as a **walk** instead of a view
/// (§10).
///
/// Returns the view together with the walk's derived starting position, which
/// is what `fold_opened_field` and any resumable family rule take. The position
/// comes from `open_field_walk`, so it is derived from the authenticated view
/// and is never a redeemer argument (§10.2).
pub fn anchored_field_walk(
  anchored: AnchoredNativeTxV1,
  field_index: Int,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  // 1. The same per-field guard `anchored_field_view` applies, for the same
  //    reason: the walk shares the anchor, not the pairing.
  expect field_pairs_with(anchored, field_index)
  open_field_walk(
    anchored.verified,
    anchored.witness_set,
    field_index,
    carriage,
    reference_inputs,
    certificate_policy_id,
  )
}

/// Authenticate field `field_index` of the transaction the thread is disputing
/// and hand back a view to slice.
///
/// `anchor` is thread state; `opening` is the redeemer's. The pairing is
/// checked in both levels §2.5 has — `verify_native_tx_compact_cbor_v1`
/// re-derives the id from the supplied bytes, and for a witness-set field the
/// re-derived compact structure's `witness_set_hash` must be the anchored one —
/// so the only compact structures that reach the door are the ones the thread
/// authenticated.
///
/// The single-field spelling of `anchored_native_tx` followed by
/// `anchored_field_view`, and nothing else: one field, so the anchor is paid
/// once either way. A step opening two or more fields should hold the handle
/// itself rather than call this twice.
pub fn opened_field_view(
  opening: FieldOpeningV1,
  anchor: NativeTxAnchorV1,
  field_index: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> FieldViewV1 {
  anchored_field_view(
    anchored_native_tx(tx_opening_of(opening), anchor),
    field_index,
    carriage_of(opening),
    reference_inputs,
    certificate_policy_id,
  )
}

/// The same authentication, opened as a **walk** instead of a view (§10).
///
/// Returns the view together with the walk's derived starting position, which
/// is what `fold_opened_field` and any resumable family rule take. The position
/// comes from `open_field_walk`, so it is derived from the authenticated view
/// and is never a redeemer argument (§10.2).
pub fn opened_field_walk(
  opening: FieldOpeningV1,
  anchor: NativeTxAnchorV1,
  field_index: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  anchored_field_walk(
    anchored_native_tx(tx_opening_of(opening), anchor),
    field_index,
    carriage_of(opening),
    reference_inputs,
    certificate_policy_id,
  )
}

/// Resume a field walk from a checkpoint commitment carried by computation
/// thread state. This is the follow-on counterpart to `opened_field_walk`:
/// the transaction/witness anchor and the per-field pairing are re-checked in
/// the fresh L1 context, while the opaque checkpoint is admitted only through
/// `resume_field_walk_from_commitment`'s hash and view-binding checks.
pub fn resume_opened_field_walk(
  opening: FieldOpeningV1,
  anchor: NativeTxAnchorV1,
  field_index: Int,
  committed_checkpoint_hash: ByteArray,
  checkpoint_bytes: ByteArray,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let anchored = anchored_native_tx(tx_opening_of(opening), anchor)
  expect field_pairs_with(anchored, field_index)
  resume_field_walk_from_commitment(
    anchored.verified,
    anchored.witness_set,
    committed_checkpoint_hash,
    checkpoint_bytes,
    carriage_of(opening),
    reference_inputs,
    certificate_policy_id,
  )
}

/// Begin the provisional, bounded envelope certification required before a
/// variable-width field may become a resumable semantic walk. The transaction
/// and witness-set anchors, positional field identity, and carriage all pass
/// through the same family opening boundary as an ordinary walk; only the
/// view constructor differs, because it deliberately does not pay an O(N)
/// whole-field walk in this transaction.
pub fn opened_field_grammar_certification(
  opening: FieldOpeningV1,
  anchor: NativeTxAnchorV1,
  field_index: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldGrammarCheckpointV1> {
  let anchored = anchored_native_tx(tx_opening_of(opening), anchor)
  expect field_pairs_with(anchored, field_index)
  open_field_grammar_certification(
    anchored.verified,
    anchored.witness_set,
    field_index,
    carriage_of(opening),
    reference_inputs,
    certificate_policy_id,
  )
}

/// Resume provisional envelope certification only from the checkpoint digest
/// held by computation-thread state. The redeemer's bytes are never accepted
/// as provenance for a variable-width position.
pub fn resume_opened_field_grammar_certification(
  opening: FieldOpeningV1,
  anchor: NativeTxAnchorV1,
  field_index: Int,
  committed_checkpoint_hash: ByteArray,
  checkpoint_bytes: ByteArray,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldGrammarCheckpointV1> {
  let anchored = anchored_native_tx(tx_opening_of(opening), anchor)
  expect field_pairs_with(anchored, field_index)
  resume_field_grammar_certification_from_commitment(
    anchored.verified,
    anchored.witness_set,
    committed_checkpoint_hash,
    checkpoint_bytes,
    carriage_of(opening),
    reference_inputs,
    certificate_policy_id,
  )
}

/// Convert a terminal grammar certificate committed by thread state into the
/// semantic walk's derived item-zero checkpoint. This is the only bridge
/// between the two checkpoint domains; a provisional count cannot reach a
/// family accumulator through any non-terminal route.
pub fn opened_certified_field_walk_from_grammar(
  opening: FieldOpeningV1,
  anchor: NativeTxAnchorV1,
  field_index: Int,
  committed_checkpoint_hash: ByteArray,
  checkpoint_bytes: ByteArray,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let anchored = anchored_native_tx(tx_opening_of(opening), anchor)
  expect field_pairs_with(anchored, field_index)
  open_certified_field_walk_from_grammar_commitment(
    anchored.verified,
    anchored.witness_set,
    committed_checkpoint_hash,
    checkpoint_bytes,
    carriage_of(opening),
    reference_inputs,
    certificate_policy_id,
  )
}

/// Asserts that this field may be opened against this handle.
///
/// One thing pairs up, and it is per-field rather than per-anchor, which is
/// why it cannot live in `anchored_native_tx`: the **§2.5 half** the handle
/// was opened under is the half this field lives in. An opening that carries
/// no witness set may not be read at a witness-set index — the door would
/// otherwise be handed this module's internal placeholder as if it were the
/// transaction's real witness set — and an opening that carries one may not be
/// read at a body index, where it would be silently ignored. Neither direction
/// is reachable from an honest family, which is exactly why it is asserted
/// rather than assumed.
///
/// Until #606 (owner ruling 2026-08-16) a second per-field guard lived here:
/// `carriage_reaches_the_anchor`, the §8.3-erratum-E2-limit-3 refusal of
/// tier-3 carriage at fields 6–8. It lifted with the repair — the §8.6
/// certificate datum now carries a mint-welded `field_hash` and the door
/// itself requires it to equal the commitment derived from the anchored
/// structures, so every tier reaches the anchor for every field and the
/// refusal had nothing left to refuse.
fn field_pairs_with(anchored: AnchoredNativeTxV1, field_index: Int) -> Bool {
  expect
    anchored.opens_witness_set_fields == (
      field_index >= first_witness_set_field_index
    )
  True
}

/// The carriage-free half of a single-field opening.
fn tx_opening_of(opening: FieldOpeningV1) -> NativeTxOpeningV1 {
  when opening is {
    BodyFieldOpening { native_tx_compact_cbor, .. } ->
      BodyTxOpening { native_tx_compact_cbor }
    WitnessFieldOpening { native_tx_compact_cbor, witness_set, .. } ->
      WitnessTxOpening { native_tx_compact_cbor, witness_set }
  }
}

// `carriage_reaches_the_anchor` — the §8.3-erratum-E2-limit-3 refusal of
// tier-3 carriage at the witness-set fields — lived here until #606's repair
// (owner ruling 2026-08-16) and was deleted with it rather than left as a
// guard that cannot fail. Its argument was that the §8.6 certificate's
// authority was a token named `(tx_id, field_index)`, minted against a
// `witness_set_hash` the minter's own redeemer supplied — a value §3's id
// preimage does not cover — so for fields 6–8 the certificate bound the
// preimage to nothing the thread anchored. The repair moved the binding into
// the datum: the mint welds `field_hash` to the verified chunk vector, and
// the door refuses a certificate whose `field_hash` differs from the
// commitment it derived from the anchored structures. A certificate minted
// against a fabricated witness set wears the fabricated hash and fails that
// equality, so every tier now reaches the anchor for every field. E2's
// disposition in `docs/spec/midgard-tx.md` §8.3 records the resolution.

fn carriage_of(opening: FieldOpeningV1) -> FieldCarriageV1 {
  when opening is {
    BodyFieldOpening { carriage, .. } -> carriage
    WitnessFieldOpening { carriage, .. } -> carriage
  }
}

/// Fold every item of an opened field, in order, exactly once.
///
/// The absence rules — "the required signature is not in the witness set", "the
/// required native script is not in the script witnesses" — are the ones that
/// genuinely have to see the whole field, and this is how they see it. It goes
/// through `walk_fold` rather than indexing, because for the variable-width
/// fields (2, 5, 6, 8) `field_item_at(view, i)` re-walks from item 0 every
/// time: indexing an N-item field costs O(N²) head decodes where a walk costs
/// O(N). The fixed-stride fields would be fine either way; using one form for
/// both is what keeps the difference from being an accident of which family
/// wrote the loop.
///
/// The budget is the field's own authenticated item count and completion is
/// asserted, so this is the *non*-resumable case by construction: a rule that
/// says "no item satisfies P" is only true of a walk that reached the end. A
/// family that cannot afford the whole field in one transaction wants
/// `midgard/native_tx_machine_walk_v1` directly and a checkpoint in its thread
/// state, not this.
///
/// It takes `opened_field_walk`'s `Pair` whole rather than a view and a
/// checkpoint separately. Two arguments would let a caller cross them — the
/// view of one field, the checkpoint of another — and completion is a property
/// of the *checkpoint* alone (`walk_is_complete` compares its own
/// `next_item_index` against its own `item_count`), so a checkpoint taken over
/// a shorter field would report a finished walk over a longer one and an
/// absence claim would be asserted over items nothing ever read. No call site
/// crosses them today; taking the pair means none can.
///
/// **Two limits, both loud, both documented in `docs/spec/midgard-tx.md` §8.3
/// erratum E2.** For a variable-width field this is O(N) by construction, so a
/// family that folds one is buying a cost that grows with the disputed
/// transaction — measured for the only such caller in this wave
/// (`missing-native-script-tx` step-06 over field 6) at ~2.7× the GOAL_SPEC §3.3
/// memory basis at C20.6's 224-witness envelope, fitting to roughly 81. And
/// `field_item_count` is only *authenticated* for a variable-width field under
/// carriage tiers 1–2: under tier 3 it aborts rather than hand back the §5.1
/// header's self-assertion, so such a field cannot be folded under Certified
/// carriage at all. Both aborts are unconditional — nothing here clamps — and
/// resolving either is §10's resumable walk, i.e. #565/#579.
pub fn fold_opened_field(
  opened: Pair<FieldViewV1, FieldWalkCheckpointV1>,
  state: a,
  step: fn(a, Int, ByteArray) -> a,
) -> a {
  let Pair(view, checkpoint) = opened
  let Pair(folded, final_checkpoint) = walk_fold(
    view,
    checkpoint,
    field_item_count(view),
    state,
    step,
  )
  expect walk_is_complete(final_checkpoint)
  folded
}
