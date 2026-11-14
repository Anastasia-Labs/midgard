//// How the **validation machine** reaches one item of one of the nine
//// committed fields — `docs/spec/midgard-tx.md` §4, §7, §8.8, §10.
////
//// This module exists because the machine used to reach an item a way §4
//// makes unsatisfiable. Every per-item step in
//// `midgard/validation_machine_v1` used to carry a *counted* pair — a
//// `bounded_collection_v1.ItemProofV1` folding a per-item Merkle frontier and a
//// `bounded_item_v1.ChunkProofV1` folding the item's chunks — and check the
//// first against the compact structure's own field hash. Since #567 those nine
//// hashes are §4's flat `blake2b_256` over the field's whole §5.1 preimage, so a
//// per-item Merkle root and a flat preimage hash could only agree by collision:
//// the predicate was unsatisfiable at fifteen sites in eight phases (#592,
//// sizing on #593). The same class `fraud_proofs/native_tx/transaction.ak`
//// documents on `verify_midgard_transaction_field_chunk_v1` and the same class
//// #587 retired for the tx-order receipt chain.
////
//// The replacement is §8's one door. `authenticated_field_view` hashes the
//// field's preimage **once** against the flat commitment (tiers 1–2) or reaches
//// it through a mint-verified digest manifest (tier 3), and every item is then
//// reached by offset-and-slice inside bytes that are already authenticated.
//// Nothing per-item is supplied by the prover any more: an item's bytes, its
//// length and the field's item count are all *derived* from the view, so there is
//// no per-item evidence left to forge.
////
//// ## What a step carries instead
////
//// A `FieldCarriageV1` and two integers — which field, which item. That is the
//// whole per-item wire surface. The carriage tier is the prover's choice among
//// §8's three and the door verifies whichever is named; owner ruling on #593
//// (Ruling 1, option c) admits all three to machine-step evidence, which is why
//// `MachineFieldDoorV1` carries both of the door's context arguments:
//// `reference_inputs` for tiers 2–3 and the §8.6 certificate minting policy id
//// for tier 3. The policy id is a **parameter of the validation-trace
//// validators**, threaded from there; the catalogue, deployment-registry and
//// format-pin churn that parameter causes is recorded for #579's single
//// regeneration batch (rider 2 of its owner-authorized scope amendment) and is
//// never edited in this lane.
////
//// Only tiers 1–3 together reach the largest genuine-Cardano field this program
//// measures — a 16,225-byte field-2 preimage (`blob-chunk-boundary-v1`) — which
//// is why a tier-1-only rebind was refused: it would green a vector for a step
//// no prover could construct on L1.
////
//// ## The field index is checked, not trusted
////
//// §4 removed field-index domain separation: two fields with the same items
//// commit to the same 32 bytes, and a flat hash says nothing about which slot it
//// came from. So `field_index` arriving from a step's evidence is checked at the
//// call site against the slot that site means: a literal at the six phases that
//// read one field, `control.field_index` for `CanonicalDecode`, which walks all
//// nine, and fields 0-or-1 for `InputSets`, which reads both and pins the choice
//// through the successor's `source_kind`. A mismatch fails closed, exactly as the
//// retired `ItemProofV1.field_index` guard did.
////
//// ## Why the machine materialises the whole preimage, at every tier
////
//// Every opening here goes through `authenticated_whole_field_view`, not
//// through `authenticated_field_view`'s lazy tier-3 form, and the reason is a
//// property of the machine rather than a preference:
////
////   * **The phases consume the item count.** Each of them takes a field's
////     §5.2 count from the field itself on the first item step, carries it in
////     control state, and then requires the scan to reach it. Tier 3's lazy view
////     refuses to answer `field_item_count` for a variable-width field, because
////     nothing there authenticates the §5.1 header number — and a count a
////     prover could understate is a count that lets it skip the items past its
////     claim. So a lazy tier-3 view cannot serve fields 2, 5, 6 or 8 at all
////     here, which is precisely the field-2 case the 16,225-byte measurement is
////     about.
////   * **The phases consume whole items.** Under tier 1–2 the door hashes the
////     preimage once, so every byte is authenticated whether or not a step reads
////     it. Under a lazy tier-3 view only the chunks a read lands in are hashed,
////     and "which chunks did this step's walk happen to touch" is not a property
////     a rule should have to reason about.
////
//// Materialising fixes both for one `blake2b_256` over at most
//// `max_transaction_aggregate_field_bytes` per step — the cost §12.5 already
//// prices as the tier-2 per-step full-preimage re-hash, now reached from tier 3
//// as well. Item access is then pure slicing at every tier, and
//// `machine_field_item_chunk` is a slice rather than a chunk-digest check.
////
//// The two derived quantities that are still **functions rather than record
//// fields** are so because they cost real work at sites that do not need them:
////
////   * `machine_field_item_commitment` builds a `bounded_item_v1` frontier over
////     the item's chunks. Only the four phases that hand a partially-read item
////     to a *later* step through their control state need it, and paying for it
////     at the eleven sites that do not would be pure cost.
////   * `machine_field_item_bytes` copies the item out of the preimage; the sites
////     that only need its length never ask.
////
//// ## Where the counted commitments still live, and why that is not this defect
////
//// `bounded_item_v1` is **not** retired here. Four phases hand a partially-read
//// item forward across steps (`PhaseANativeScripts`'s token-head scan,
//// `ScriptSources`'s stage-zero and mint folds, and the redeemer item proof), and
//// their control state carries an `item_commitment` that later steps open chunk
//// proofs against. That commitment is a genuine Merkle root over the item's
//// chunks, so opening it is satisfiable and sound — it is not the §4 mismatch
//// #592 names. What changes is its *provenance*: it is now **derived** here from
//// bytes the door authenticated, instead of arriving from a prover's
//// `ItemProofV1`. The continuation sites are untouched and their control
//// encodings do not move, which is why eight phases of trace state, and every
//// successor the machine's tests pin, stay exactly as they were.
////
//// ## Hand-off boundary: committed garbage is #596's, not this module's
////
//// The §8.8 door **aborts** on committed bytes that are not a §5.1 envelope.
//// For a prover supplying the wrong bytes that is the correct fail-closed
//// answer (§7.3) and it stays. For an *operator-committed* ungrammatical
//// preimage it means no `CanonicalDecode` step is producible by anyone, so the
//// two verdicts that phase used to render for that case —
//// `FieldItemWidthIllegal` and `FieldPreimageLengthMismatch` — no longer
//// cover it. Under the retired counted scheme the case was not even expressible
//// (the commitment *was* a root over items, so items always existed).
////
//// Owner ruling #593 Ruling 2 closes that hatch by direct fault rather than by
//// softening the door: a new §12 fault kind, "the committed preimage of field
//// *i* is not a §5.1 envelope", owned by **#596**. This module's boundary is
//// therefore exact and is stated so a reader meets it here:
////
////   * still rendered by `CanonicalDecode`, and reachable — a declared
////     `NativeTxFieldPreimageLengthsV1` length that disagrees with the
////     authenticated preimage's own length (`FieldPreimageLengthMismatch`), and
////     an item whose §5.1-authenticated width is not a legal encoding for its
////     field (`FieldItemWidthIllegal`);
////   * **not** rendered here, and #596's — a committed preimage whose §5.1
////     envelope does not decode at all. The door aborts; no verdict is emitted.

use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/bounded_item_v1
use midgard/field_grammar_certificate_v1.{
  certified_field_grammar_at, certified_field_item_count,
  certified_field_item_extent,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxWitnessSetCompact, VerifiedTxSourceView,
}
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, FieldViewV1, ProvisionalWhole, Whole,
  authenticated_materialised_field_preimage_by_commitment,
  authenticated_materialised_resumable_field_view_by_commitment,
  authenticated_materialised_resumable_field_view_with_commitment,
  authenticated_whole_field_view, authenticated_whole_field_view_by_commitment,
  authenticated_whole_fixed_field_view, field_fixed_item_extent,
  field_item_count, field_item_extent, field_read_range, field_stride,
  walk_derived_stride, walked_field_view, whole_view_item_count,
  whole_view_read_range,
}

/// The two arguments §8's door needs that a machine step has no other way to
/// reach: the transaction's reference inputs (tiers 2–3) and the §8.6
/// certificate minting policy id (tier 3).
///
/// One record rather than two threaded parameters, because it is threaded
/// through eight phases and thirty-odd entry points and the pair always travels
/// together. Every validation-trace validator has `tx` in hand for the first and
/// takes the second as a validator parameter (#593 Ruling 1).
pub type MachineFieldDoorV1 {
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
}

/// One item of one field, reached through the door.
///
/// Holds only what is cheap and always needed: the authenticated view, the two
/// indices the step named, and the item's `(offset, length)` inside the
/// preimage. Everything else is a function — see the module header on why that
/// is load-bearing rather than stylistic.
///
/// `item_length` is the item's **payload** length, its §5.1 byte-string wrapper
/// stripped: exactly what the retired `ItemProofV1.item_length` and
/// `ChunkProofV1.total_length` both carried, so the call sites that compared
/// those two to each other now have one authenticated number instead.
pub type MachineFieldItemV1 {
  view: FieldViewV1,
  field_index: Int,
  item_index: Int,
  item_offset: Int,
  item_length: Int,
}

/// Authenticate field `field_index` and locate item `item_index` in it.
///
/// The item is located by `field_item_extent`, which is stride arithmetic for
/// the six fixed-width fields (§5.3) and an envelope walk for fields 2, 5, 6 and
/// 8. Both read the item's own §5.1 definite byte-string wrapper — one, two or
/// three bytes as `field_item_header_at` decodes it, pinned to the two-byte
/// `58 LL` form for the six fixed-stride fields — so a non-canonical spelling of
/// the same payload is refused rather than opened (§6.1).
pub fn open_machine_field_item(
  door: MachineFieldDoorV1,
  verified: VerifiedTxSourceView,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  item_index: Int,
  carriage: FieldCarriageV1,
) -> MachineFieldItemV1 {
  let view = authenticated_whole_field_view(
    verified,
    witness_set,
    field_index,
    carriage,
    door.reference_inputs,
    door.certificate_policy_id,
  )
  let Pair(item_offset, item_length) = field_item_extent(view, item_index)
  MachineFieldItemV1 { view, field_index, item_index, item_offset, item_length }
}

/// Open a commitment extracted from an already authenticated machine control.
/// The caller still pins the field index to the selected semantic arm.
pub fn open_machine_field_item_by_commitment(
  door: MachineFieldDoorV1,
  tx_id: ByteArray,
  field_commitment: ByteArray,
  field_index: Int,
  item_index: Int,
  carriage: FieldCarriageV1,
) -> MachineFieldItemV1 {
  let view = authenticated_whole_field_view_by_commitment(
    tx_id,
    field_index,
    field_commitment,
    carriage,
    door.reference_inputs,
    door.certificate_policy_id,
  )
  let Pair(item_offset, item_length) = field_item_extent(view, item_index)
  MachineFieldItemV1 { view, field_index, item_index, item_offset, item_length }
}

// ---------------------------------------------------------------------------
// Certified route for a variable-width field (2, 5, 6, 8)
// ---------------------------------------------------------------------------
//
// `open_machine_field_item` walks a variable-width field's envelope twice —
// once to settle the count at view construction, once to reach the item — so
// a step's cost grows with the field's item count. The certified route reads
// the item through the field grammar certificate instead
// (`midgard/field_grammar_certificate_v1`), so its cost no longer grows with
// the field's item count: the preimage is hashed once against the same
// commitment, the certificate bound to that commitment supplies the item
// count, and the item is reached from the nearest waypoint in at most
// `field_grammar_waypoint_interval` header reads.
//
// The item it returns is the item `open_machine_field_item` returns for the
// same field and index. A `Complete` certificate bound to the commitment the
// bytes hash to records exactly what `whole_view`'s walk establishes — the
// header's declared item count, consumed to the field's last byte — and its
// waypoints are offsets into those same bytes, so the view is the same
// `Whole` view and every accessor below answers identically on either route.
// The certificate is itself a pure function of the committed field, which is
// why the route can be the prover's choice without the step's successor
// depending on it.

/// How a step reaches an item of a variable-width field: by walking the
/// field (`open_machine_field_item`), or through the field grammar
/// certificate at reference input `cert_ref_input_index`.
///
/// Carried in a redeemer beside a step's evidence, never inside it: either
/// route yields the same item, so the route is not part of what the step
/// commits. `WalkFieldItem` is Constr 0 with no fields — the same Data as the
/// `Void` an unused redeemer carries.
pub type MachineFieldItemRouteV1 {
  WalkFieldItem
  CertifiedFieldItem { cert_ref_input_index: Int }
}

/// `open_machine_field_item` for a variable-width field, by `route`: the
/// field is hash-checked once through the resumable door, then either settled
/// by `whole_view`'s walk (`WalkFieldItem`) or read through the field grammar
/// certificate at reference input `cert_ref_input_index`, minted under
/// `grammar_certificate_policy_id` (`CertifiedFieldItem`). The certified route
/// fails unless that certificate is `Complete` and bound to this transaction,
/// field, commitment and length, and unless `field_index` is variable-width (a
/// fixed-stride field has no certificate).
pub fn open_machine_variable_field_item(
  door: MachineFieldDoorV1,
  grammar_certificate_policy_id: PolicyId,
  route: MachineFieldItemRouteV1,
  verified: VerifiedTxSourceView,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  item_index: Int,
  carriage: FieldCarriageV1,
) -> MachineFieldItemV1 {
  let Pair(field_commitment, view) = authenticated_materialised_resumable_field_view_with_commitment(
    verified,
    witness_set,
    field_index,
    carriage,
    door.reference_inputs,
    door.certificate_policy_id,
  )
  routed_machine_field_item(
    door,
    grammar_certificate_policy_id,
    route,
    verified.tx_id,
    field_commitment,
    field_index,
    item_index,
    view,
  )
}

/// `open_machine_variable_field_item` for a commitment extracted from an
/// already authenticated machine control, as
/// `open_machine_field_item_by_commitment` is to `open_machine_field_item`.
/// The caller still pins the field index to the commitment it extracted.
pub fn open_machine_variable_field_item_by_commitment(
  door: MachineFieldDoorV1,
  grammar_certificate_policy_id: PolicyId,
  route: MachineFieldItemRouteV1,
  tx_id: ByteArray,
  field_commitment: ByteArray,
  field_index: Int,
  item_index: Int,
  carriage: FieldCarriageV1,
) -> MachineFieldItemV1 {
  routed_machine_field_item(
    door,
    grammar_certificate_policy_id,
    route,
    tx_id,
    field_commitment,
    field_index,
    item_index,
    authenticated_materialised_resumable_field_view_by_commitment(
      tx_id,
      field_index,
      field_commitment,
      carriage,
      door.reference_inputs,
      door.certificate_policy_id,
    ),
  )
}

/// The half both routed openers share, from the hash-checked view on.
fn routed_machine_field_item(
  door: MachineFieldDoorV1,
  grammar_certificate_policy_id: PolicyId,
  route: MachineFieldItemRouteV1,
  tx_id: ByteArray,
  field_commitment: ByteArray,
  field_index: Int,
  item_index: Int,
  view: FieldViewV1,
) -> MachineFieldItemV1 {
  when route is {
    WalkFieldItem -> {
      let view = walked_field_view(view)
      let Pair(item_offset, item_length) = field_item_extent(view, item_index)
      MachineFieldItemV1 {
        view,
        field_index,
        item_index,
        item_offset,
        item_length,
      }
    }
    CertifiedFieldItem { cert_ref_input_index } ->
      certified_machine_field_item(
        door,
        grammar_certificate_policy_id,
        cert_ref_input_index,
        tx_id,
        field_commitment,
        field_index,
        item_index,
        view,
      )
  }
}

/// `open_machine_variable_field_item` on the certified route alone, for a
/// stage that reads its variable-width field only through the field grammar
/// certificate at reference input `cert_ref_input_index`.
pub fn open_machine_certified_field_item(
  door: MachineFieldDoorV1,
  grammar_certificate_policy_id: PolicyId,
  cert_ref_input_index: Int,
  verified: VerifiedTxSourceView,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  item_index: Int,
  carriage: FieldCarriageV1,
) -> MachineFieldItemV1 {
  let Pair(field_commitment, view) = authenticated_materialised_resumable_field_view_with_commitment(
    verified,
    witness_set,
    field_index,
    carriage,
    door.reference_inputs,
    door.certificate_policy_id,
  )
  certified_machine_field_item(
    door,
    grammar_certificate_policy_id,
    cert_ref_input_index,
    verified.tx_id,
    field_commitment,
    field_index,
    item_index,
    view,
  )
}

/// `open_machine_variable_field_item_by_commitment` on the certified route
/// alone, for a stage that reads its variable-width field only through the
/// field grammar certificate at reference input `cert_ref_input_index`.
pub fn open_machine_certified_field_item_by_commitment(
  door: MachineFieldDoorV1,
  grammar_certificate_policy_id: PolicyId,
  cert_ref_input_index: Int,
  tx_id: ByteArray,
  field_commitment: ByteArray,
  field_index: Int,
  item_index: Int,
  carriage: FieldCarriageV1,
) -> MachineFieldItemV1 {
  certified_machine_field_item(
    door,
    grammar_certificate_policy_id,
    cert_ref_input_index,
    tx_id,
    field_commitment,
    field_index,
    item_index,
    authenticated_materialised_resumable_field_view_by_commitment(
      tx_id,
      field_index,
      field_commitment,
      carriage,
      door.reference_inputs,
      door.certificate_policy_id,
    ),
  )
}

/// `open_machine_certified_field_item_by_commitment`, read straight to the
/// two things a stage that only commits to the item needs: the certified item
/// count and the item's payload bytes. The payload is the one
/// `machine_field_item_bytes` reads from the opened item, and its length is
/// that item's `machine_field_item_length`.
///
/// Fails for a fixed-stride field, which has no certificate, as the certified
/// route does. Past that the preimage is only hash-checked: the certificate,
/// bound to that commitment and the preimage's length, settles the rest of
/// what the resumable door checks (see
/// `authenticated_materialised_field_preimage_by_commitment`), and the view
/// the item is read through is the `ProvisionalWhole` view that door opens,
/// its count the certified one.
pub fn open_machine_certified_field_payload_by_commitment(
  door: MachineFieldDoorV1,
  grammar_certificate_policy_id: PolicyId,
  cert_ref_input_index: Int,
  tx_id: ByteArray,
  field_commitment: ByteArray,
  field_index: Int,
  item_index: Int,
  carriage: FieldCarriageV1,
) -> Pair<Int, ByteArray> {
  expect field_stride(field_index) == walk_derived_stride
  let bytes = authenticated_materialised_field_preimage_by_commitment(
    tx_id,
    field_index,
    field_commitment,
    carriage,
    door.reference_inputs,
    door.certificate_policy_id,
  )
  let certified = certified_field_grammar_at(
    door.reference_inputs,
    cert_ref_input_index,
    grammar_certificate_policy_id,
    tx_id,
    field_index,
    field_commitment,
    bytearray.length(bytes),
  )
  let item_count = certified_field_item_count(certified)
  let view = ProvisionalWhole {
    bytes,
    count: item_count,
    stride: walk_derived_stride,
  }
  let Pair(item_offset, item_length) = certified_field_item_extent(
    view,
    certified,
    item_index,
  )
  Pair(item_count, field_read_range(view, item_offset, item_length))
}

/// The certified route from the hash-checked view on: bind the certificate to
/// the hash-checked bytes, locate the item from its waypoint, and hand back
/// the `Whole` view `whole_view` would have built, its count the certified
/// one.
fn certified_machine_field_item(
  door: MachineFieldDoorV1,
  grammar_certificate_policy_id: PolicyId,
  cert_ref_input_index: Int,
  tx_id: ByteArray,
  field_commitment: ByteArray,
  field_index: Int,
  item_index: Int,
  view: FieldViewV1,
) -> MachineFieldItemV1 {
  // Only a variable-width field opens provisional; a fixed-stride one opens
  // `Whole` and is refused here.
  expect ProvisionalWhole { bytes, stride, .. } = view
  let certified = certified_field_grammar_at(
    door.reference_inputs,
    cert_ref_input_index,
    grammar_certificate_policy_id,
    tx_id,
    field_index,
    field_commitment,
    bytearray.length(bytes),
  )
  let Pair(item_offset, item_length) = certified_field_item_extent(
    view,
    certified,
    item_index,
  )
  MachineFieldItemV1 {
    view: Whole { bytes, count: certified_field_item_count(certified), stride },
    field_index,
    item_index,
    item_offset,
    item_length,
  }
}

/// The field's authenticated §5.2 item count, with no item located.
///
/// For the one phase whose *item index* is derived from the count — the CEK
/// observer context, which builds its list back to front — the count has to be
/// known before an item can be opened. Everywhere else the count comes off an
/// already-opened item.
pub fn machine_field_count(
  door: MachineFieldDoorV1,
  verified: VerifiedTxSourceView,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  carriage: FieldCarriageV1,
) -> Int {
  field_item_count(
    authenticated_whole_field_view(
      verified,
      witness_set,
      field_index,
      carriage,
      door.reference_inputs,
      door.certificate_policy_id,
    ),
  )
}

/// §5.2's item count for the field this item was opened in, authenticated.
///
/// Always answerable here — the view is materialised whole, so §5.1's
/// count-consistency walk ran at construction and the header number is a
/// checked fact rather than a claim. That is the whole reason this module does
/// not use the lazy tier-3 view: see the module header.
pub fn machine_field_item_count(item: MachineFieldItemV1) -> Int {
  field_item_count(item.view)
}

/// The item's whole payload, read out of the authenticated preimage.
pub fn machine_field_item_bytes(item: MachineFieldItemV1) -> ByteArray {
  field_read_range(item.view, item.item_offset, item.item_length)
}

/// Chunk `chunk_index` of the item, in `bounded_item_v1.chunk_bytes` units —
/// the same window the retired `ChunkProofV1.chunk` carried, now sliced out of
/// authenticated bytes instead of opened against a per-item root.
///
/// The stepping stays because the phases that use it step *within* one item
/// across several machine steps, and their control state counts chunks — not to
/// save hashes. Every view here is materialised whole (see the module header),
/// so the preimage was hashed once at construction and this is pure slicing at
/// every tier; §8.4's "only the chunks a read lands in are hashed" is
/// `authenticated_field_view`'s promise, which this module deliberately does not
/// take.
pub fn machine_field_item_chunk(
  item: MachineFieldItemV1,
  chunk_index: Int,
) -> ByteArray {
  let length = bounded_item_v1.expected_chunk_length(
    item.item_length,
    chunk_index,
  )
  field_read_range(
    item.view,
    item.item_offset + chunk_index * bounded_item_v1.chunk_bytes,
    length,
  )
}

/// The `bounded_item_v1` commitment over this item's chunks, **derived** from
/// authenticated bytes.
///
/// The four phases that hand a partially-read item to a later step put this in
/// their control state, and the later step opens chunk proofs against it. It is
/// a real Merkle root over the item's chunks, so those openings are satisfiable
/// and sound; what the rebind changed is that the root is now computed here from
/// bytes the door authenticated, instead of being asserted by a prover's
/// `ItemProofV1` against a flat field hash it could never match.
pub fn machine_field_item_commitment(item: MachineFieldItemV1) -> ByteArray {
  bounded_item_v1.from_bytes(
    item.field_index,
    item.item_index,
    machine_field_item_bytes(item),
  )
}

/// The item's payload length as a step's guards see it. Named so a call site
/// reads as a claim about the *authenticated* item rather than about a
/// prover-supplied number.
pub fn machine_field_item_length(item: MachineFieldItemV1) -> Int {
  item.item_length
}

/// The number of preimage bytes before the **next** item's §5.1 wrapper.
///
/// The walk position a phase carries forward (§10.2). Derived, not supplied:
/// `item_offset` is where this item's payload starts and `item_length` is how
/// long that payload is, so the sum is the first byte past this item — which,
/// §5.1 items being concatenated with nothing between them, is exactly where the
/// next item's wrapper begins. This item's own wrapper sits *before*
/// `item_offset` and its width (one, two or three bytes) does not enter the
/// arithmetic.
pub fn machine_field_next_item_offset(item: MachineFieldItemV1) -> Int {
  item.item_offset + item.item_length
}

/// Whether `bytes` is the item's payload. Used where a step is handed candidate
/// bytes from somewhere other than the carriage — a published proof item — and
/// the door's job is to say whether they are the committed ones.
pub fn machine_field_item_bytes_match(
  item: MachineFieldItemV1,
  bytes: ByteArray,
) -> Bool {
  and {
    bytearray.length(bytes) == item.item_length,
    machine_field_item_bytes(item) == bytes,
  }
}

// ---------------------------------------------------------------------------
// Fixed-stride door (§5.3)
// ---------------------------------------------------------------------------
//
// The three functions below are `open_machine_field_item`,
// `machine_field_item_count` and `machine_field_item_bytes` restricted to a
// §5.3 fixed-stride field. They authenticate the same bytes with the same
// checks — see the "Fixed-stride specialisation" section of
// `midgard/native_tx_field_access_v1` for why each restriction is a refusal
// rather than a relaxation — and exist so a phase that only ever opens a
// fixed-stride field does not link the variable-width envelope walk or tier
// 3's lazy chunk reader. Publication margin, not behaviour: every field these
// are called on has a literal index whose `field_stride` is fixed by §5.3.

/// `open_machine_field_item` for a §5.3 fixed-stride field.
pub fn open_machine_fixed_field_item(
  door: MachineFieldDoorV1,
  verified: VerifiedTxSourceView,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  item_index: Int,
  carriage: FieldCarriageV1,
) -> MachineFieldItemV1 {
  let view = authenticated_whole_fixed_field_view(
    verified,
    witness_set,
    field_index,
    carriage,
    door.reference_inputs,
    door.certificate_policy_id,
  )
  let Pair(item_offset, item_length) = field_fixed_item_extent(view, item_index)
  MachineFieldItemV1 { view, field_index, item_index, item_offset, item_length }
}

/// `open_machine_fixed_field_item` at the item `seen` places from the end of
/// the field: index `count - seen - 1`, with the count read off the same
/// authenticated view. For a scan that reads an ascending field from its tail
/// (`InputSets`), so the step names no index.
pub fn open_machine_fixed_field_tail_item(
  door: MachineFieldDoorV1,
  verified: VerifiedTxSourceView,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  seen: Int,
  carriage: FieldCarriageV1,
) -> MachineFieldItemV1 {
  let view = authenticated_whole_fixed_field_view(
    verified,
    witness_set,
    field_index,
    carriage,
    door.reference_inputs,
    door.certificate_policy_id,
  )
  let item_index = whole_view_item_count(view) - seen - 1
  let Pair(item_offset, item_length) = field_fixed_item_extent(view, item_index)
  MachineFieldItemV1 { view, field_index, item_index, item_offset, item_length }
}

/// `machine_field_item_count` for an item opened through the fixed-stride door.
pub fn machine_fixed_field_item_count(item: MachineFieldItemV1) -> Int {
  whole_view_item_count(item.view)
}

/// `machine_field_item_bytes` for an item opened through the fixed-stride door.
pub fn machine_fixed_field_item_bytes(item: MachineFieldItemV1) -> ByteArray {
  whole_view_read_range(item.view, item.item_offset, item.item_length)
}
