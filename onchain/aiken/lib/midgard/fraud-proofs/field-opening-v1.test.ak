//// The guards `midgard/fraud_proofs/field_opening_v1` exists for, isolated.
////
//// Everything else the module does is exercised through the thirteen Q1x family
//// step suites that consume it — the tx-id binding, the door's
//// authenticate-once check, the witness-set re-derivation, abort-never-clamp on
//// an out-of-range index. What those suites cannot reach is the §2.5 body/
//// witness split itself, because no honest family ever pairs a body opening
//// with a witness field index: each call site names one literal and hands over
//// the matching constructor. A guard no vector can trip is a guard nobody knows
//// is there, so both directions are asserted here.
////
//// The §2.5 *anchor* split gets the same treatment, and it needs it more. Its
//// premise — that a prover can rewrite the compact structure's trailing
//// `witness_set_hash` and still re-derive the committed transaction id — is a
//// property of §3's two-level hashing that nothing else in the tree states out
//// loud, so it is asserted here as a positive test before the guard that
//// depends on it is asserted as a negative one.
////
//// The **carriage-tier** split at the end of the file is the same argument one
//// level out. §2.5's anchor closes the inline forgery; tier 3 reopened it,
//// because a certificate is minted against a `witness_set_hash` its own
//// redeemer supplied and §3's id does not cover that value. Those vectors state
//// the premise executably in both directions — the §8.6 minting predicate
//// accepts the fabrication on a witness field and refuses it on a body field —
//// before asserting the refusal that follows from it.
////
//// The positive rows are here for the same reason a negative test needs a
//// twin: without them a module that refused *everything* would pass the `fail`
//// tests. `certified_carriage_still_opens_on_a_body_field` is that twin for the
//// tier-3 refusal specifically, and it is the row that goes red if the refusal
//// is ever widened into a ban.

use aiken/collection/list
use aiken/primitive/bytearray
use cardano/transaction.{Input}
use midgard/fraud_proofs/field_opening_v1.{
  AnchoredNativeTxV1, BodyAnchor, BodyTxOpening, FieldOpeningV1,
  NativeTxAnchorV1, WitnessAnchor, WitnessTxOpening,
  address_witnesses_field_index, anchored_field_view, anchored_field_walk,
  anchored_native_tx, first_witness_set_field_index, fold_opened_field,
  mint_field_index, opened_field_view, opened_field_walk, redeemers_field_index,
  reference_inputs_field_index, script_witnesses_field_index,
  spend_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_witness_set_compact, verify_native_tx_compact_cbor_v1,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_tx_input,
}
use midgard/native_tx_carriage_v1.{
  field_preimage_certificate_v1, split_field_preimage,
  verify_field_preimage_certificate_v1,
}
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, FieldViewV1, Inline, chunk_bytes_k, encode_field_preimage,
  field_item_at, field_item_count,
}

fn block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

/// The compact structure a prover forges when it wants a witness-set field to
/// read empty: the committed transaction's body, followed by the
/// `witness_set_hash` of the empty witness set.
fn forged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.forged_witness_set_block_v1(
    block(),
    fixture.empty_witness_set_compact(),
  )
}

fn body_anchor() -> NativeTxAnchorV1 {
  BodyAnchor { tx_id: block().tx_id }
}

fn witness_anchor() -> NativeTxAnchorV1 {
  WitnessAnchor {
    tx_id: block().tx_id,
    witness_set_hash: block().compact.witness_set_hash,
  }
}

fn body_opening() -> FieldOpeningV1 {
  let committed = block()
  fixture.inline_body_opening(
    committed.compact_cbor,
    committed.tx.body.inputs |> list.map(encode_midgard_tx_input),
  )
}

fn witness_opening() -> FieldOpeningV1 {
  let committed = block()
  fixture.inline_witness_opening(
    committed.compact_cbor,
    committed.witness_set_compact,
    committed.tx.witness_set.addr_tx_wits
      |> list.map(encode_midgard_address_witness),
  )
}

fn open_at(
  opening: FieldOpeningV1,
  anchor: NativeTxAnchorV1,
  field_index: Int,
) -> Int {
  field_item_count(
    opened_field_view(
      opening,
      anchor,
      field_index,
      [],
      fixture.test_certificate_policy_id,
    ),
  )
}

/// The split point is the table's, not a second literal that could drift.
test first_witness_set_field_index_is_the_script_witness_slot() {
  and {
    first_witness_set_field_index == script_witnesses_field_index,
    first_witness_set_field_index == 6,
  }
}

/// Positive: a body opening at a body index reads the body's field.
test body_opening_reads_a_body_field() {
  open_at(body_opening(), body_anchor(), spend_inputs_field_index) == list.length(
    block().tx.body.inputs,
  )
}

/// Positive: a witness opening at a witness index reads the witness set's
/// field.
test witness_opening_reads_a_witness_field() {
  open_at(witness_opening(), witness_anchor(), address_witnesses_field_index) == list.length(
    block().tx.witness_set.addr_tx_wits,
  )
}

/// A body opening carries no witness set, so using one at a witness-set index
/// would mean the door was handed the module's internal placeholder as if it
/// were the transaction's real witness set. It is refused instead.
test body_opening_is_refused_at_a_witness_field_index() fail {
  open_at(body_opening(), witness_anchor(), address_witnesses_field_index) == 0
}

/// The mirror: a witness opening at a body index would carry a witness set the
/// door never consults, which is exactly the "supply something that is silently
/// ignored" shape the two constructors exist to prevent.
test witness_opening_is_refused_at_a_body_field_index() fail {
  open_at(witness_opening(), body_anchor(), spend_inputs_field_index) == 0
}

/// A body anchor cannot authenticate a witness-set read: it carries no
/// `witness_set_hash`, and §3's transaction id does not supply one. Pairing it
/// with a witness opening is refused rather than silently downgraded to an
/// id-only check.
test witness_opening_is_refused_under_a_body_anchor() fail {
  open_at(witness_opening(), body_anchor(), address_witnesses_field_index) == 0
}

/// The mirror, for completeness of the 2×2: a body read does not take a witness
/// anchor either, so the two anchors cannot be used interchangeably at call
/// sites and a step's §2.5 level is fixed at compile time.
test body_opening_is_refused_under_a_witness_anchor() fail {
  open_at(body_opening(), witness_anchor(), spend_inputs_field_index) == 0
}

/// The walk entry point enforces the same split — it resolves its sources
/// through the same private path, and this is the assertion that says so rather
/// than assuming it.
test opened_field_walk_is_refused_on_a_mismatched_split() fail {
  let Pair(_view, _checkpoint) = opened_field_walk(
    body_opening(),
    body_anchor(),
    script_witnesses_field_index,
    [],
    fixture.test_certificate_policy_id,
  )
  True
}

// ## One anchor, many fields
//
// `anchored_native_tx` exists so a step that opens two fields of one transaction
// pays the tx-id derivation once instead of twice. What that must not buy is a
// weaker check: the §2.5 half and the carriage tier are properties of the
// *field*, not of the anchor, so both have to be re-run on every opening the
// handle serves. The rows below assert exactly that — one positive that the
// second field really opens, and three negatives that the second opening is
// still guarded.

fn anchored_body_tx() -> AnchoredNativeTxV1 {
  anchored_native_tx(
    BodyTxOpening { native_tx_compact_cbor: block().compact_cbor },
    body_anchor(),
  )
}

fn inline_items(items: List<ByteArray>) -> FieldCarriageV1 {
  Inline { preimage: encode_field_preimage(items) }
}

fn anchored_view(
  anchored: AnchoredNativeTxV1,
  field_index: Int,
  items: List<ByteArray>,
) -> FieldViewV1 {
  anchored_field_view(
    anchored,
    field_index,
    inline_items(items),
    [],
    fixture.test_certificate_policy_id,
  )
}

fn spend_input_items() -> List<ByteArray> {
  block().tx.body.inputs |> list.map(encode_midgard_tx_input)
}

fn reference_input_items() -> List<ByteArray> {
  block().tx.body.reference_inputs |> list.map(encode_midgard_tx_input)
}

/// Positive: one anchor, two fields, and each field authenticated against its
/// **own** commitment.
///
/// Fields 0 and 1 are the pair that makes this worth asserting. §4 removed
/// field-index domain separation, so their commitments are computed the same
/// way and differ only because their items do; a handle that carried a field's
/// identity rather than the transaction's would open both to the same bytes and
/// nothing would say so.
test one_anchor_opens_two_body_fields_against_their_own_commitments() {
  let anchored = anchored_body_tx()
  expect [spend_item] = spend_input_items()
  expect [reference_item] = reference_input_items()
  and {
    spend_item != reference_item,
    field_item_at(
      anchored_view(anchored, spend_inputs_field_index, spend_input_items()),
      0,
    ) == spend_item,
    field_item_at(
      anchored_view(
        anchored,
        reference_inputs_field_index,
        reference_input_items(),
      ),
      0,
    ) == reference_item,
  }
}

/// The door's §7.1 hash check is still per field: field 1's preimage does not
/// open field 0, even though the same anchor authenticated both.
test one_anchor_refuses_a_field_carried_with_another_fields_preimage() fail {
  field_item_count(
    anchored_view(
      anchored_body_tx(),
      spend_inputs_field_index,
      reference_input_items(),
    ),
  ) == 0
}

/// The §2.5 split survives the handle. A body opening carries no witness set,
/// so a witness-set index would hand the door the module's internal placeholder
/// — the mismatch `opened_field_view` refuses, refused here on the second
/// opening rather than the first.
test one_anchor_refuses_a_witness_field_on_a_body_handle() fail {
  field_item_count(
    anchored_view(anchored_body_tx(), script_witnesses_field_index, []),
  ) == 0
}

/// And it survives it on the walk entry point too, which is the one the
/// transition-trace family opens its two fields through.
test one_anchor_walk_refuses_a_witness_field_on_a_body_handle() fail {
  let Pair(_view, _checkpoint) = anchored_field_walk(
    anchored_body_tx(),
    script_witnesses_field_index,
    inline_items([]),
    [],
    fixture.test_certificate_policy_id,
  )
  True
}

// ## The §3 two-level split, and what it does not commit

/// **The premise.** The forged compact structure re-derives to the *same*
/// transaction id as the committed one.
///
/// This is not an incidental fact — it is the entire reason `NativeTxAnchorV1`
/// has two arms. §3 hashes the body CBOR alone, so of the §2.5 compact
/// structure's three parts the id covers one; rewriting the trailing
/// `witness_set_hash` is invisible to it. `verify_native_tx_compact_cbor_v1`
/// therefore *accepts* these bytes, and everything downstream of it that trusts
/// only the id is already lost.
test forged_witness_set_hash_re_derives_the_same_tx_id() {
  let forged = forged_block()
  and {
    // Different bytes, different committed witness set …
    forged.compact_cbor != block().compact_cbor,
    forged.compact.witness_set_hash != block().compact.witness_set_hash,
    // … and the id-only check cannot tell.
    verify_native_tx_compact_cbor_v1(block().tx_id, forged.compact_cbor).tx_id == block().tx_id,
  }
}

/// **The guard.** Both sides forged — the compact structure's trailing hash and
/// the witness set that matches it — and the door refuses, because the
/// `witness_set_hash` it checks against is the thread's, not the redeemer's.
///
/// Neutralise the `witness_set_hash` equality in `anchored_native_tx` and this
/// test passes with `field_item_count == 0`: every §2.5 absence rule over fields
/// 6–8 becomes true of every transaction ever committed.
test forged_witness_set_and_compact_is_refused_under_the_anchor() fail {
  let forged = forged_block()
  open_at(
    fixture.inline_witness_opening(
      forged.compact_cbor,
      forged.witness_set_compact,
      [],
    ),
    witness_anchor(),
    address_witnesses_field_index,
  ) == 0
}

/// The same forgery *does* open when the thread anchored it — which is what
/// makes the previous test a statement about the anchor rather than about the
/// forged bytes being malformed. Nothing here is wrong with the bytes; they are
/// simply not the ones the block committed.
test forged_witness_set_and_compact_opens_under_its_own_anchor() {
  let forged = forged_block()
  open_at(
    fixture.inline_witness_opening(
      forged.compact_cbor,
      forged.witness_set_compact,
      [],
    ),
    WitnessAnchor {
      tx_id: forged.tx_id,
      witness_set_hash: forged.compact.witness_set_hash,
    },
    address_witnesses_field_index,
  ) == 0
}

/// `fold_opened_field` visits every item exactly once and in order. The absence
/// rules that use it are only sound if that is true, and "in order" is not
/// something their own `or`-folds can observe.
test fold_opened_field_visits_every_item_in_order() {
  let opened = opened_field_walk(
    witness_opening(),
    witness_anchor(),
    address_witnesses_field_index,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(view, _walk) = opened
  let visited = fold_opened_field(
    opened,
    [],
    fn(indices, index, _item) { list.push(indices, index) },
  )
  visited == ( list.range(0, field_item_count(view) - 1) |> list.reverse )
}

// ## Tier 3 (§8.4 Certified) on a witness-set field
//
// The §2.5 anchor split above closes the *inline* forgery: a prover cannot
// rewrite the compact structure's trailing `witness_set_hash` and authenticate
// its own witness set against it, because the hash the door checks is the
// thread's. Tier 3 reopened the same hole one level out, and these vectors are
// the argument for `carriage_reaches_the_anchor`.
//
// Under tiers 1–2 the door hashes the whole preimage against
// `field_commitment_at(body, witness_set, field_index)`, so the *content* is
// bound to structures this module pinned. Under tier 3 there is no preimage to
// hash — that is what tier 3 is for — and the binding is delegated to the §8.6
// certificate, whose authority is a token named `(tx_id, field_index)`. The
// minting policy behind that name derives its own `expected_hash` from the
// `witness_set_hash` on the tail of *its* redeemer's `native_tx_compact_cbor`,
// and §3's id preimage is the body alone, so for fields 6–8 that tail is the
// certifier's to choose. The first two vectors below are the premise, stated
// executably in both directions: the minting predicate **accepts** a
// certificate binding the committed transaction's field 7 to a preimage it
// never committed, and **refuses** the same fabrication at a body field, where
// the id-committed body is the whole of `expected_hash`. The defect is
// therefore exactly the witness-set fields, and the refusal is scoped exactly
// to them.
//
// The fabricated preimage is 256 canonical address witnesses against a
// transaction that committed one. The same bytes are re-used at fields 6 and 8:
// under tier 3 the door never decodes an item's meaning, and re-encoding the
// vector per field would only pay for a claim these tests do not make.

/// 256 items at the §5.3 address-witness stride of 103 plus a 3-byte header is
/// 26,371 bytes — over `chunk_bytes_k`, so the fabrication is admissible tier-3
/// carriage rather than a tier-1 preimage wearing a certificate.
const forged_witness_count: Int = 256

fn forged_witness_items() -> List<ByteArray> {
  list.repeat(
    encode_midgard_address_witness(fixture.sample_address_witness()),
    forged_witness_count,
  )
}

fn forged_witness_preimage() -> ByteArray {
  encode_field_preimage(forged_witness_items())
}

/// The opening a step would hand over: the **honest** committed compact
/// structure and the **honest** committed witness set, so every §2.5 anchor
/// check above passes on its own terms, with tier-3 carriage of the fabrication.
fn certified_witness_opening(preimage: ByteArray) -> FieldOpeningV1 {
  fixture.certified_witness_opening(
    block().compact_cbor,
    block().witness_set_compact,
    preimage,
  )
}

fn certified_body_opening(preimage: ByteArray) -> FieldOpeningV1 {
  fixture.certified_body_opening(block().compact_cbor, preimage)
}

fn certified_reference_inputs(
  field_index: Int,
  preimage: ByteArray,
) -> List<Input> {
  fixture.certified_reference_inputs_v1(block().tx_id, field_index, preimage)
}

fn certificate_owner() -> ByteArray {
  fixture.fraud_prover_key_hash
}

fn open_with_references(
  opening: FieldOpeningV1,
  anchor: NativeTxAnchorV1,
  field_index: Int,
  reference_inputs: List<Input>,
) -> FieldViewV1 {
  opened_field_view(
    opening,
    anchor,
    field_index,
    reference_inputs,
    fixture.test_certificate_policy_id,
  )
}

/// The vector is what it claims to be: over the tier-2/3 boundary, split into
/// two chunks, and far wider than the field the transaction actually committed.
test tier_three_forgery_vector_is_admissible_carriage() {
  let preimage = forged_witness_preimage()
  and {
    bytearray.length(preimage) > chunk_bytes_k,
    list.length(split_field_preimage(preimage)) == 2,
    forged_witness_count > list.length(block().tx.witness_set.addr_tx_wits),
  }
}

/// **The premise, witness side.** The §8.6 minting predicate certifies a
/// fabricated field-7 preimage under the *committed* transaction's name.
///
/// The certifier supplies the genuine body — so the id it re-derives, and hence
/// the token's `(tx_id, field_index)` name, is the committed transaction's —
/// followed by the `witness_set_hash` of a witness set of its own construction.
/// Everything the minter checks is satisfied. The certificate that comes out is
/// a valid §8.6 certificate that says the committed transaction's field 7 is
/// 256 signatures it never carried.
test tier_three_certificate_over_a_witness_field_binds_to_nothing_committed() {
  let honest = block()
  let preimage = forged_witness_preimage()
  let forged_witness_set = fixture.witness_set_compact_of_items(
    forged_witness_items(),
    [],
    [],
  )
  let forged = fixture.forged_witness_set_block_v1(honest, forged_witness_set)
  and {
    // The token still gets the committed transaction's name …
    verify_native_tx_compact_cbor_v1(honest.tx_id, forged.compact_cbor).tx_id == honest.tx_id,
    // … and the minting predicate signs off on the fabrication under it.
    verify_field_preimage_certificate_v1(
      field_preimage_certificate_v1(
        certificate_owner(),
        honest.tx_id,
        address_witnesses_field_index,
        preimage,
      ),
      forged.compact_cbor,
      encode_native_tx_witness_set_compact(forged_witness_set),
      split_field_preimage(preimage),
    ),
  }
}

/// **The premise, body side — and the reason the refusal is scoped rather than
/// total.** The same fabrication offered as a *body* field's certificate is
/// refused by the same minting predicate, because there `expected_hash` is
/// derived from the body alone and the body is exactly what §3's transaction id
/// commits. Tier 3 over fields 0–5 is bound; tier 3 over 6–8 is not.
test tier_three_certificate_over_a_body_field_is_bound_by_the_tx_id() {
  let honest = block()
  let preimage = forged_witness_preimage()
  !verify_field_preimage_certificate_v1(
    field_preimage_certificate_v1(
      certificate_owner(),
      honest.tx_id,
      mint_field_index,
      preimage,
    ),
    honest.compact_cbor,
    encode_native_tx_witness_set_compact(honest.witness_set_compact),
    split_field_preimage(preimage),
  )
}

/// **The guard, field 7.** Honest compact structure, honest witness set, honest
/// `WitnessAnchor` — every §2.5 check above passes — and the door is still
/// refused, because tier 3 cannot reach the anchor for a witness-set field.
///
/// Neutralise `carriage_reaches_the_anchor` in `field_pairs_with` and this test
/// passes with `field_item_count == 256`: an invalid-signature or
/// missing-signature fault becomes provable against a transaction whose field 7
/// the prover wrote itself.
test certified_carriage_is_refused_at_the_address_witness_field() fail {
  let preimage = forged_witness_preimage()
  field_item_count(
    open_with_references(
      certified_witness_opening(preimage),
      witness_anchor(),
      address_witnesses_field_index,
      certified_reference_inputs(address_witnesses_field_index, preimage),
    ),
  ) == forged_witness_count
}

/// **The guard, field 6.** Field 6 is variable-width, so `field_item_count`
/// would abort under tier 3 in any case (§8.3 E2 limit 2) — which is why the
/// read, not the count, is what this asserts. Without the refusal the door
/// serves an item out of a preimage the transaction never committed.
test certified_carriage_is_refused_at_the_script_witness_field() fail {
  let preimage = forged_witness_preimage()
  field_item_at(
    open_with_references(
      certified_witness_opening(preimage),
      witness_anchor(),
      script_witnesses_field_index,
      certified_reference_inputs(script_witnesses_field_index, preimage),
    ),
    0,
  ) == encode_midgard_address_witness(fixture.sample_address_witness())
}

/// **The guard, field 8.** The third witness-set field, closed by the same
/// clause; enumerated rather than assumed, because the boundary is an
/// inequality and an inequality is exactly what an off-by-one hides in.
test certified_carriage_is_refused_at_the_redeemer_field() fail {
  let preimage = forged_witness_preimage()
  field_item_at(
    open_with_references(
      certified_witness_opening(preimage),
      witness_anchor(),
      redeemers_field_index,
      certified_reference_inputs(redeemers_field_index, preimage),
    ),
    0,
  ) == encode_midgard_address_witness(fixture.sample_address_witness())
}

/// The walk entry point resolves its sources through the same private path, so
/// the refusal reaches `opened_field_walk` too. Asserted rather than assumed:
/// a family that opens a witness field as a walk is the exact shape §10's
/// resumable work will add more of.
test opened_field_walk_refuses_certified_carriage_on_a_witness_field() fail {
  let preimage = forged_witness_preimage()
  let Pair(view, _checkpoint) = opened_field_walk(
    certified_witness_opening(preimage),
    witness_anchor(),
    address_witnesses_field_index,
    certified_reference_inputs(address_witnesses_field_index, preimage),
    fixture.test_certificate_policy_id,
  )
  field_item_count(view) == forged_witness_count
}

/// **The neutralisation twin.** The refusal is scoped to fields 6–8, not a ban
/// on tier 3: the identical carriage, certificate and chunks open on a body
/// field through the same call.
///
/// This is what stops the guards above from passing for the wrong reason. A
/// `field_pairs_with` that refused every `Certified` carriage would satisfy all
/// four `fail` tests and break tier 3 for the six fields it is sound for; this
/// row goes red the moment that happens. The certificate here could not in fact
/// be minted — the row above proves the minting predicate refuses it — because
/// what is being pinned is the *door's* scope, and the door defers a tier-3
/// field's content binding to §8.6 for every field it accepts.
test certified_carriage_still_opens_on_a_body_field() {
  let preimage = forged_witness_preimage()
  field_item_at(
    open_with_references(
      certified_body_opening(preimage),
      body_anchor(),
      mint_field_index,
      certified_reference_inputs(mint_field_index, preimage),
    ),
    0,
  ) == encode_midgard_address_witness(fixture.sample_address_witness())
}

// ## The tier guard on the *second* opening of a live handle
//
// The four `fail` rows above all reach the tier guard through the single-field
// entry points, where the anchor and the carriage arrive together in one
// `FieldOpeningV1`. `anchored_native_tx` splits them: the anchor is paid once and
// each later opening names its own field and its own carriage. That is exactly the
// shape in which a tier check can be lost — `field_pairs_with` is called from
// `anchored_field_view` and `anchored_field_walk` rather than from
// `anchored_native_tx`, and an edit that hoisted it into the constructor would
// still satisfy every row above while leaving the second opening of a handle
// unguarded.
//
// So the refusal is asserted again here, per opening rather than transitively: a
// witness handle whose first field opens honestly under tier 1, then offered the
// tier-3 fabrication at the same witness field. The positive first open is inside
// the test body on purpose — it is what makes the row a statement about the
// *second* opening rather than about a handle that never worked.

fn anchored_witness_tx() -> AnchoredNativeTxV1 {
  let committed = block()
  anchored_native_tx(
    WitnessTxOpening {
      native_tx_compact_cbor: committed.compact_cbor,
      witness_set: committed.witness_set_compact,
    },
    witness_anchor(),
  )
}

fn address_witness_items() -> List<ByteArray> {
  block().tx.witness_set.addr_tx_wits
    |> list.map(encode_midgard_address_witness)
}

fn anchored_view_with_references(
  anchored: AnchoredNativeTxV1,
  field_index: Int,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
) -> FieldViewV1 {
  anchored_field_view(
    anchored,
    field_index,
    carriage,
    reference_inputs,
    fixture.test_certificate_policy_id,
  )
}

/// **The guard, second opening, view entry point.** Field 7 opens under tier-1
/// carriage of the committed items — so the handle is demonstrably live — and the
/// very next opening of that same live handle is refused when it carries the
/// tier-3 fabrication.
///
/// Neutralise `carriage_reaches_the_anchor` in `field_pairs_with`, or hoist that
/// call out of `anchored_field_view` into `anchored_native_tx`, and this test
/// passes with `field_item_count == 256`.
test one_anchor_refuses_certified_carriage_at_a_witness_field_on_the_second_open() fail {
  let anchored = anchored_witness_tx()
  let honest = address_witness_items()
  expect
    field_item_count(
      anchored_view(anchored, address_witnesses_field_index, honest),
    ) == list.length(honest)
  let preimage = forged_witness_preimage()
  field_item_count(
    anchored_view_with_references(
      anchored,
      address_witnesses_field_index,
      fixture.certified_carriage_v1(preimage),
      certified_reference_inputs(address_witnesses_field_index, preimage),
    ),
  ) == forged_witness_count
}

/// The same, through the walk entry point, which is the one the transition-trace
/// family opens its fields through and the one §10's resumable work will add more
/// callers of. `anchored_field_walk` has its own `field_pairs_with` call, so this
/// is a second guard rather than the same guard reached twice.
test one_anchor_walk_refuses_certified_carriage_at_a_witness_field_on_the_second_open() fail {
  let anchored = anchored_witness_tx()
  let honest = address_witness_items()
  expect
    field_item_count(
      anchored_view(anchored, address_witnesses_field_index, honest),
    ) == list.length(honest)
  let preimage = forged_witness_preimage()
  let Pair(view, _checkpoint) = anchored_field_walk(
    anchored,
    address_witnesses_field_index,
    fixture.certified_carriage_v1(preimage),
    certified_reference_inputs(address_witnesses_field_index, preimage),
    fixture.test_certificate_policy_id,
  )
  field_item_count(view) == forged_witness_count
}

/// **The neutralisation twin for the handle path.** The refusal is scoped to
/// fields 6–8 here too, not a ban on tier 3 for anything a handle serves: a body
/// handle opens field 0 under tier-1 carriage and then field 5 under the identical
/// tier-3 carriage and certificate the two rows above are refused for.
///
/// Without this row an `anchored_field_view` that rejected every `Certified`
/// carriage outright would satisfy both `fail` tests above and silently break
/// tier 3 for the six fields it is sound for.
test one_anchor_still_opens_certified_carriage_on_a_body_field_second() {
  let anchored = anchored_body_tx()
  let preimage = forged_witness_preimage()
  expect [spend_item] = spend_input_items()
  and {
    field_item_at(
      anchored_view(anchored, spend_inputs_field_index, spend_input_items()),
      0,
    ) == spend_item,
    field_item_at(
      anchored_view_with_references(
        anchored,
        mint_field_index,
        fixture.certified_carriage_v1(preimage),
        certified_reference_inputs(mint_field_index, preimage),
      ),
      0,
    ) == encode_midgard_address_witness(fixture.sample_address_witness()),
  }
}
