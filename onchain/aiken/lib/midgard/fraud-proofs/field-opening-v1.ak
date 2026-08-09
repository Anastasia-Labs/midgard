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
////     verifies whichever is named — except that a **witness-set** field may
////     not name tier 3. The certificate that stands in for the door's own hash
////     check under tier 3 is minted against a `witness_set_hash` the minter's
////     redeemer supplied, and §3's id preimage does not cover that value, so
////     for fields 6–8 the certificate binds the preimage to nothing this
////     thread anchored. `carriage_reaches_the_anchor` refuses it; §8.3
////     erratum E2 limit 3 records the cost and #579 owns the repair.
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
  Certified, FieldCarriageV1, FieldViewV1, Inline, RawUtxo,
  authenticated_field_view, field_item_count,
}
use midgard/native_tx_machine_walk_v1.{
  FieldWalkCheckpointV1, open_field_walk, walk_fold, walk_is_complete,
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
/// §7.1 authenticate-once is the caller's to exploit exactly as it is through
/// the door itself: open once per field per step, read the view many times.
pub fn opened_field_view(
  opening: FieldOpeningV1,
  anchor: NativeTxAnchorV1,
  field_index: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> FieldViewV1 {
  let Pair(verified, witness_set) = resolved_sources(
    opening,
    anchor,
    field_index,
  )
  authenticated_field_view(
    verified,
    witness_set,
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
  let Pair(verified, witness_set) = resolved_sources(
    opening,
    anchor,
    field_index,
  )
  open_field_walk(
    verified,
    witness_set,
    field_index,
    carriage_of(opening),
    reference_inputs,
    certificate_policy_id,
  )
}

/// The anchored compact structures and the witness set the door will consult,
/// with the §2.5 body/witness split enforced against `field_index` on both the
/// opening and the anchor.
///
/// Three constructors pair up here — opening, anchor, field index — and all
/// three must agree. The opening/index pairing keeps a witness set from riding
/// in under a field that will not read one, or from being omitted under a field
/// that must; the anchor/opening pairing is what makes the witness-set read
/// *authenticated at all*, and the paragraph in the module header is the whole
/// argument for why.
///
/// A fourth thing has to agree, and it is the one a reader would not predict:
/// the **carriage tier**. Tiers 1–2 are authenticated by the door itself, but
/// tier 3's binding is a §8.6 certificate minted against a `witness_set_hash`
/// its own redeemer supplied — a value §3's transaction id does not cover — so a
/// tier-3 carriage of a witness-set field reaches no anchor at all.
/// `carriage_reaches_the_anchor` carries that argument in full.
fn resolved_sources(
  opening: FieldOpeningV1,
  anchor: NativeTxAnchorV1,
  field_index: Int,
) -> Pair<VerifiedMidgardNativeTxCompact, NativeTxWitnessSetCompact> {
  // Checked once, over both arms, so the clause that admits tier 3 for a body
  // field is as load-bearing as the one that refuses it for a witness field and
  // neither can be widened without a test going red.
  expect carriage_reaches_the_anchor(carriage_of(opening), field_index)
  when opening is {
    BodyFieldOpening { native_tx_compact_cbor, .. } -> {
      expect field_index < first_witness_set_field_index
      expect BodyAnchor { tx_id } = anchor
      Pair(
        verify_native_tx_compact_cbor_v1(tx_id, native_tx_compact_cbor),
        unread_witness_set(),
      )
    }
    WitnessFieldOpening { native_tx_compact_cbor, witness_set, .. } -> {
      expect field_index >= first_witness_set_field_index
      expect WitnessAnchor { tx_id, witness_set_hash } = anchor
      let verified = verify_native_tx_compact_cbor_v1(
        tx_id,
        native_tx_compact_cbor,
      )
      // The check the transaction id cannot make for itself. Without it a
      // prover supplies the genuine body — which re-derives to `tx_id` and so
      // satisfies everything above — followed by a `witness_set_hash` of its own
      // choosing, and then "authenticates" a witness set of its own choosing
      // against it. The empty witness set is the useful forgery: it makes every
      // absence rule (§2.5 fields 6 and 7) true of every transaction, which is a
      // slashing proof against an honest operator.
      expect verified.tx_compact.witness_set_hash == witness_set_hash
      Pair(verified, witness_set)
    }
  }
}

/// Whether `carriage` can be authenticated back to the thread's anchor for
/// `field_index`. True for tiers 1–2 always; for tier 3 only on a **body**
/// field.
///
/// **Why tier 3 does not reach the anchor for fields 6–8.** Tiers 1 and 2 hand
/// the door the whole preimage, and the door hashes it against
/// `field_commitment_at(body, witness_set, field_index)` — a value derived from
/// structures this module has already pinned to thread state, so the content is
/// bound to the disputed transaction by the door itself. Tier 3 cannot do that:
/// §8.4 exists precisely because the preimage is too large to hold, so the door
/// never hashes it and the *certificate* is the binding instead. The
/// certificate's authority is a §8.6 token named `(tx_id, field_index)`, and
/// what stands behind that name is the minting policy's own check —
/// `native_tx_carriage_v1.verify_field_preimage_certificate_v1` — that the
/// chunks hash to `field_commitment_at(body, witness_set, field_index)` for the
/// transaction it re-derived.
///
/// That check is sound for fields 0–5 and unsound for 6–8, for the same reason
/// `NativeTxAnchorV1` has two arms. The minter re-derives the transaction id
/// from *its own redeemer's* `native_tx_compact_cbor` and takes the
/// `witness_set_hash` off the tail of those same bytes — and §3's id preimage
/// is the body alone, so that tail is the minter's caller's to choose. A
/// certifier may therefore present the genuine body (which re-derives to the
/// committed id, so the token gets the committed transaction's name) followed
/// by the `witness_set_hash` of any witness set it likes, and certify a field-6,
/// -7 or -8 preimage that transaction never committed. The token then names the
/// honest transaction while the digest manifest under it describes a fabricated
/// field, and the door — which discards its own `expected_hash` on the Certified
/// arm — has nothing left to catch it with. The useful forgeries are both
/// directions of the §2.5 absence rules: an empty field 7 makes
/// "the required signature is absent" true of every transaction, and a
/// fabricated 256-item field 7 makes an "invalid signature" fault provable
/// against a signature the transaction never carried.
///
/// So a witness-set field is refused tier 3 here, at the one place every family
/// step reaches the door through. This is a **limit, not a repair**: it costs
/// fields 6–8 the ability to be carried above the §8.3 tier-2 bound, which is
/// recorded as limit 3 of `docs/spec/midgard-tx.md` §8.3 erratum E2. The repair
/// is to fold the field commitment into the §8.6 asset name so the token cannot
/// be borrowed for a preimage the transaction did not commit; that is a change
/// to a frozen wire format and to a landed minting policy, and it is assigned
/// to [#579](https://github.com/Anastasia-Labs/midgard/issues/579) — see E2.
///
/// The abort is unconditional. Nothing here falls back to tier 1 or 2 on the
/// prover's behalf: a step handed an inadmissible carriage fails, which is
/// §7.3's abort-never-clamp rule applied to the tier ladder.
fn carriage_reaches_the_anchor(
  carriage: FieldCarriageV1,
  field_index: Int,
) -> Bool {
  when carriage is {
    Inline { .. } -> True
    RawUtxo { .. } -> True
    Certified { .. } -> field_index < first_witness_set_field_index
  }
}

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
