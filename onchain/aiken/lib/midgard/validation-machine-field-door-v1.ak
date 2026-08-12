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
//// `reject_invalid_field_type` and `reject_field_preimage_size` — no longer
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
////     authenticated preimage's own length (`reject_field_preimage_size`), and
////     an item whose §5.1-authenticated width is not a legal encoding for its
////     field (`reject_invalid_field_type`);
////   * **not** rendered here, and #596's — a committed preimage whose §5.1
////     envelope does not decode at all. The door aborts; no verdict is emitted.

use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/types.{
  NativeTxWitnessSetCompact, VerifiedMidgardNativeTxCompact,
}
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, FieldViewV1, authenticated_whole_field_view, field_item_count,
  field_item_extent, field_item_header_at, field_read_range,
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
  verified: VerifiedMidgardNativeTxCompact,
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

/// The same opening, with the item located from a **carried walk position**
/// instead of from its index (§10.2, §10.5).
///
/// `CanonicalDecode` already carries the position: its control's
/// `encoded_length` is the number of preimage bytes before the item it is about
/// to read, which is precisely the offset of that item's §5.1 wrapper. Using it
/// turns a variable-width field's O(index) walk into one header decode, and it
/// is not a new trust assumption: the offset is a position, never a claim about
/// content, and `field_item_header_at` reads it through the authenticated view,
/// so an offset that does not land on an item's wrapper fails closed.
///
/// The caller still says which item index it believes it is at; nothing here
/// checks the two against each other, because for a variable-width field only
/// the walk relates them and re-walking is the cost this entry point exists to
/// avoid. What binds them is the phase's own successor exactness: the control
/// that carries `encoded_length` carries `item_index` beside it, and both are
/// re-derived from the previous step's authenticated read.
pub fn open_machine_field_item_at(
  door: MachineFieldDoorV1,
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
  item_index: Int,
  item_wrapper_offset: Int,
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
  let Pair(item_offset, item_length) = field_item_header_at(
    view,
    item_wrapper_offset,
  )
  MachineFieldItemV1 { view, field_index, item_index, item_offset, item_length }
}

/// The field's authenticated §5.2 item count, with no item located.
///
/// For the one phase whose *item index* is derived from the count — the CEK
/// observer context, which builds its list back to front — the count has to be
/// known before an item can be opened. Everywhere else the count comes off an
/// already-opened item.
pub fn machine_field_count(
  door: MachineFieldDoorV1,
  verified: VerifiedMidgardNativeTxCompact,
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

/// How many `bounded_item_v1` chunks this item spans.
pub fn machine_field_item_chunk_count(item: MachineFieldItemV1) -> Int {
  bounded_item_v1.chunk_count(item.item_length)
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
