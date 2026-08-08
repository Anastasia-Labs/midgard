//// Seam vectors for the dispute machine's walk core
//// (`docs/spec/midgard-tx.md` §10).
////
//// Everything here is asserted at the **machine seam** — `open_field_walk`,
//// `resume_field_walk_from_commitment`, `walk_fold`, `walk_next`, `walk_skip`,
//// `spend_input_at`, and the checkpoint wire form. Nothing reaches inside the
//// walk: no test names a private helper, reads a §5.1 head itself, or asserts
//// an intermediate offset the seam does not already hand back. That is
//// deliberate — the walk's internals are exactly the part #571's intra-item
//// cases are expected to change, and a suite that pinned them would have to be
//// rewritten alongside them.
////
//// The four claims, and how each is actually established here:
////
////   * **Authenticate-once** (§7.1, §10.2). A cost claim, so it is measured
////     rather than asserted: `authenticate_once_*` below are four tests over
////     the same 64-item field that differ only in how the items are reached —
////     one open and one read; one open and sixty-four reads; one open, the
////     same sixty-four relocations the control performs, and sixty-four reads;
////     and sixty-four opens with those same relocations and reads. The runner's
////     per-test row is the evidence, and §10.7 quotes all four; the assertions
////     only pin that they compute the same answer, so the numbers are
////     comparable at all.
////   * **Resumable checkpoints** (§10.2, §10.6). `resume_*` interrupts a walk
////     mid-field, carries the position through the thread's 32-byte
////     commitment and the 53 positional bytes that hash to it, re-opens under
////     a *different carriage tier* — the follow-on transaction is a different
////     transaction — and finishes. The answer must equal the uninterrupted
////     walk's, and the second transaction must not re-read the first's items.
////   * **Positions, not bytes** (§7.6, §10.3). `no_preimage_bytes_*` walks two
////     preimages that share every width and differ in every byte the §5.1/§5.3
////     grammar leaves free — the array header, the item wrappers and the heads
////     inside them are forced to agree and nothing else does — and demands
////     byte-identical checkpoints. A structure carrying preimage content could
////     not produce that. Each pair asserts its own disjointness rather than
////     claiming it, so the fixture cannot quietly weaken.
////   * **Arithmetic spend-input access** (§5.3, §10.5). `spend_input_*` reads
////     item 0 and item 295 of a 296-item field through views built the same
////     way; the runner reporting the same cost for both is the claim.
////
//// **How a forgery is spelled here.** `FieldWalkCheckpointV1` is opaque and
//// its decoder is private (§10.6), so no test can write a checkpoint literal —
//// which is the point, since that capability is precisely the one a prover
//// must not have either. Every negative vector below therefore forges the way
//// a prover would: it takes the 53 carried bytes of an honest position and
//// patches one §10.3 scalar in place, then presents them with the digest a
//// thread would have committed. The patch helpers address the wire form by its
//// documented offsets rather than through the encoder, so a fixture cannot
//// drift with the encoder without the round-trip test noticing.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_witness_set_compact,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTxInput, MidgardVersionedScript, NativeCardanoScript,
  NativeTxBodyCompact, NativeTxCompact, NativeTxWitnessSetCompact,
  PlutusV3Script, VerifiedMidgardNativeTxCompact, native_tx_version_v1,
}
use midgard/native_tx_field_access_v1.{
  Certified, FieldCarriageV1, FieldPreimageCertificateV1, FieldViewV1, Inline,
  RawUtxo, chunk_bytes_k, encode_field_array_header, encode_field_preimage,
  field_commitment, field_item_extent, field_preimage_certificate_asset_name,
  maximum_cardano_spend_redeemer_count, spend_input_item_bytes,
  spend_input_stride,
}
use midgard/native_tx_machine_walk_v1.{
  FieldWalkCheckpointV1, encode_field_walk_checkpoint,
  field_walk_checkpoint_bytes, field_walk_checkpoint_hash, open_field_walk,
  resume_field_walk_from_commitment, spend_input_at, spend_input_count,
  walk_fold, walk_is_complete, walk_next, walk_remaining, walk_skip,
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const zero_hash32: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const sample_tx_id: ByteArray =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const other_tx_id: ByteArray =
  #"3333333333333333333333333333333333333333333333333333333333333333"

const certificate_policy_id: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

const item_tx_id_a: ByteArray =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const item_tx_id_b: ByteArray =
  #"5555555555555555555555555555555555555555555555555555555555555555"

const owner_hash: ByteArray = #"66666666666666666666666666666666666666666666666666666666"

/// A field-0/1 item at `index`: 38 bytes whatever `index` is, because §5.3
/// spells the output index in the fixed three-byte `19 XXXX` form. Built from
/// constants rather than digests so that a fixture with 296 items costs
/// nothing that could be mistaken for walk cost.
fn input_item(tx_id: ByteArray, index: Int) -> ByteArray {
  encode_midgard_tx_input(MidgardTxInput { tx_id, output_index: index })
}

/// A field-6 item at `index`, deliberately varying in width so the
/// variable-width path has real work to do.
fn script_item(index: Int) -> ByteArray {
  encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: PlutusV3Script,
      script_bytes: bytearray.take(
        item_tx_id_a,
        4 + index % 7,
      ),
    },
  )
}

/// `script_item`'s content-disjoint twin: the same width at every index, and
/// every byte the §5.1 grammar leaves free is different. The language tag is
/// one of the free bytes — `NativeCardanoScript` serialises to `00` where
/// `PlutusV3Script` serialises to `03`, both one byte — so the twins share only
/// the `82` array head and the `4L` byte-string head the width forces.
fn other_script_item(index: Int) -> ByteArray {
  encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: bytearray.take(
        item_tx_id_b,
        4 + index % 7,
      ),
    },
  )
}

fn input_items(tx_id: ByteArray, next: Int, remaining: Int) -> List<ByteArray> {
  if remaining <= 0 {
    []
  } else {
    [input_item(tx_id, next), ..input_items(tx_id, next + 1, remaining - 1)]
  }
}

fn script_items(next: Int, remaining: Int) -> List<ByteArray> {
  if remaining <= 0 {
    []
  } else {
    [script_item(next), ..script_items(next + 1, remaining - 1)]
  }
}

fn other_script_items(next: Int, remaining: Int) -> List<ByteArray> {
  if remaining <= 0 {
    []
  } else {
    [
      other_script_item(next),
      ..other_script_items(next + 1, remaining - 1)
    ]
  }
}

/// The committed structures that carry `preimage`'s flat hash in
/// `field_index`'s slot, under `tx_id`. Every other slot holds a hash the
/// preimage cannot match, so a test that reaches the wrong slot fails rather
/// than passing by accident.
fn source(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
) -> Pair<VerifiedMidgardNativeTxCompact, NativeTxWitnessSetCompact> {
  let committed = field_commitment(preimage)
  let slot =
    fn(index: Int) -> ByteArray {
      if index == field_index {
        committed
      } else {
        zero_hash32
      }
    }
  let witness_set =
    NativeTxWitnessSetCompact {
      addr_tx_wits_hash: slot(7),
      script_tx_wits_hash: slot(6),
      redeemer_tx_wits_hash: slot(8),
    }
  let body =
    NativeTxBodyCompact {
      spend_inputs_hash: slot(0),
      reference_inputs_hash: slot(1),
      outputs_hash: slot(2),
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers_hash: slot(3),
      required_signers_hash: slot(4),
      mint_hash: slot(5),
      script_integrity_hash: zero_hash32,
      auxiliary_data_hash: zero_hash32,
      network_id: 255,
    }
  Pair(
    VerifiedMidgardNativeTxCompact {
      tx_id,
      version: native_tx_version_v1,
      tx_compact: NativeTxCompact {
        body,
        witness_set_hash: blake2b_256(
          encode_native_tx_witness_set_compact(witness_set),
        ),
        validity_code: 0,
      },
    },
    witness_set,
  )
}

fn key_output(datum: Data) -> Output {
  Output {
    address: address.from_verification_key(owner_hash),
    value: assets.from_lovelace(2000000),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn input_of(index: Int, output: Output) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: item_tx_id_b,
      output_index: index,
    },
    output,
  }
}

fn certificate_for(
  field_index: Int,
  preimage: ByteArray,
  chunks: List<ByteArray>,
) -> Input {
  let datum: Data =
    FieldPreimageCertificateV1 {
      owner: owner_hash,
      tx_id: sample_tx_id,
      field_index,
      total_length: bytearray.length(preimage),
      chunk_digests: list.map(chunks, blake2b_256),
    }
  Input {
    output_reference: OutputReference {
      transaction_id: item_tx_id_a,
      output_index: 0,
    },
    output: Output {
      address: address.from_script(certificate_policy_id),
      value: assets.from_lovelace(2000000)
        |> assets.add(
            certificate_policy_id,
            field_preimage_certificate_asset_name(sample_tx_id, field_index),
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

/// The seam's opening call, with the fixture noise folded away.
fn open(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let Pair(verified, witness_set) = source(tx_id, field_index, preimage)
  open_field_walk(
    verified,
    witness_set,
    field_index,
    carriage,
    reference_inputs,
    certificate_policy_id,
  )
}

/// Tier 1 — the step's own redeemer carries the bytes (§8.1).
fn open_inline(
  field_index: Int,
  preimage: ByteArray,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  open(sample_tx_id, field_index, preimage, Inline { preimage }, [])
}

/// The same opening, against committed structures the caller already holds.
///
/// The authenticate-once measurement needs this: `open_inline` rebuilds the
/// compact structures, and building them hashes too, so a control that
/// re-opened through it would be measuring the fixture as much as the door.
/// Here the only thing a second open pays for is the door's own
/// full-preimage `blake2b_256`, which is exactly the cost under test.
fn open_inline_from(
  committed: Pair<VerifiedMidgardNativeTxCompact, NativeTxWitnessSetCompact>,
  field_index: Int,
  preimage: ByteArray,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let Pair(verified, witness_set) = committed
  open_field_walk(
    verified,
    witness_set,
    field_index,
    Inline { preimage },
    [],
    certificate_policy_id,
  )
}

/// Tier 2 — one nothing-but-bytes inline datum at the prover's key address
/// (§8.2). Used for the *follow-on* transaction so that resuming is shown to
/// work across a carriage change, not just across a second call.
fn open_raw_utxo(
  field_index: Int,
  preimage: ByteArray,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  open(
    sample_tx_id,
    field_index,
    preimage,
    RawUtxo { ref_input_index: 0 },
    [input_of(0, key_output(builtin.b_data(preimage)))],
  )
}

/// Tier 3 — deterministic chunks plus one certified digest manifest (§8.4).
fn open_certified(
  field_index: Int,
  preimage: ByteArray,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let chunks =
    [
      bytearray.take(preimage, chunk_bytes_k),
      bytearray.drop(preimage, chunk_bytes_k),
    ]
  expect [chunk_0, chunk_1] = chunks
  open(
    sample_tx_id,
    field_index,
    preimage,
    Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] },
    [
      certificate_for(field_index, preimage, chunks),
      input_of(1, key_output(builtin.b_data(chunk_0))),
      input_of(2, key_output(builtin.b_data(chunk_1))),
    ],
  )
}

/// A cheap order-sensitive summary of the items a walk visited: it is
/// position-dependent, so a resumed walk that silently restarted, skipped, or
/// repeated an item cannot produce the same number as the uninterrupted one.
fn tally(state: Int, index: Int, item: ByteArray) -> Int {
  state * 31 + ( index + 1 ) * bytearray.length(item) + bytearray.at(item, 0)
}

const walk_field_item_count: Int = 20

fn walk_preimage() -> ByteArray {
  encode_field_preimage(script_items(0, walk_field_item_count))
}

const authenticate_once_item_count: Int = 64

fn authenticate_once_preimage() -> ByteArray {
  encode_field_preimage(input_items(item_tx_id_a, 0, authenticate_once_item_count))
}

fn spend_input_preimage() -> ByteArray {
  encode_field_preimage(
    input_items(item_tx_id_a, 0, maximum_cardano_spend_redeemer_count),
  )
}

// ---------------------------------------------------------------------------
// Forging a carried position, the way a prover would
// ---------------------------------------------------------------------------

/// §10.3's commitment domain, ASCII `MidgardFieldWalkCheckpointV1`, re-spelled
/// here rather than imported: the module's copy is private, and a fixture that
/// borrowed it could not tell a changed domain from an unchanged one.
/// `resume_accepts_the_position_the_thread_committed` pins the two together, so
/// a divergence shows up as one failing positive rather than as a file full of
/// negatives quietly refusing on the wrong guard.
const checkpoint_domain: ByteArray =
  #"4d6964676172644669656c6457616c6b436865636b706f696e745631"

/// The digest a thread commits for `wire`.
fn thread_commitment(wire: ByteArray) -> ByteArray {
  blake2b_256(bytearray.concat(checkpoint_domain, wire))
}

/// §10.3's byte offsets of the four three-byte scalars, counted off the wire
/// form `86 ‖ 5820 tx_id ‖ 41 field_index ‖ 43 total_length ‖ 43 item_count ‖
/// 43 next_item_index ‖ 43 next_offset`.
const wire_total_length_at: Int = 38

const wire_item_count_at: Int = 42

const wire_next_item_index_at: Int = 46

const wire_next_offset_at: Int = 50

fn wire_scalar(wire: ByteArray, at: Int) -> Int {
  bytearray.to_int_big_endian(bytearray.slice(wire, at, at + 2))
}

/// Overwrite one three-byte scalar in place. The carried bytes travel in the
/// clear, so this is the prover's actual capability: forging a position is
/// patching them and re-committing.
fn patch_wire_scalar(wire: ByteArray, at: Int, value: Int) -> ByteArray {
  bytearray.take(wire, at)
    |> bytearray.concat(bytearray.from_int_big_endian(value, 3))
    |> bytearray.concat(bytearray.drop(wire, at + 3))
}

/// Resume `wire` through the §10.6 entry point — the only one there is — with
/// the thread commitment set to that wire form's own digest.
///
/// A negative vector aimed at the §10.2 *binding* check has to get past the
/// commitment check first, and committing the bytes under test is how: it puts
/// the fixture in the position of a thread that committed a position and is now
/// having it bound to freshly authenticated bytes, which is exactly where the
/// binding check earns its keep.
fn resume_wire(
  field_index: Int,
  preimage: ByteArray,
  wire: ByteArray,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let Pair(verified, witness_set) = source(sample_tx_id, field_index, preimage)
  resume_field_walk_from_commitment(
    verified,
    witness_set,
    thread_commitment(wire),
    wire,
    Inline { preimage },
    [],
    certificate_policy_id,
  )
}

// ---------------------------------------------------------------------------
// §7.1 / §10.2 — authenticate-once, measured
// ---------------------------------------------------------------------------
//
// The four tests below are a measurement, not an assertion. Each computes the
// same tally over the same 64-item field-0 preimage; they differ only in how
// many times the preimage is authenticated to get it, and in how the items are
// reached:
//
//   * `..._one_open_one_read`          — 1 open,  0 relocations,  1 item read
//   * `..._one_open_every_read`        — 1 open,  0 relocations, 64 items read
//   * `..._one_open_every_relocation`  — 1 open, 64 relocations, 64 items read
//   * `..._reopen_per_item_costs_more` — 64 opens, 64 relocations, 64 items read
//
// If the hash check rode along with each item read, the last two would cost the
// same. The runner's memory column is the evidence that they do not, and the
// third row is what lets the comparison hold the access pattern fixed instead
// of varying two things at once — §10.7 item 1 quotes all four.
// The assertions here exist so the four are computing comparable work.

/// One open, one item. The floor: everything a single-item dispute pays.
test authenticate_once_one_open_one_read() {
  let preimage = authenticate_once_preimage()
  let committed = source(sample_tx_id, 0, preimage)
  let Pair(view, start) = open_inline_from(committed, 0, preimage)
  let Pair(item, next) = walk_next(view, start)
  and {
    item == input_item(item_tx_id_a, 0),
    walk_remaining(next) == authenticate_once_item_count - 1,
  }
}

/// One open, every item. The *whole field* walked for one hash check — this is
/// the claim "however many items it reads".
test authenticate_once_one_open_every_read() {
  let preimage = authenticate_once_preimage()
  let committed = source(sample_tx_id, 0, preimage)
  let Pair(view, start) = open_inline_from(committed, 0, preimage)
  let Pair(total, done) =
    walk_fold(view, start, authenticate_once_item_count, 0, tally)
  and {
    walk_is_complete(done),
    total == expected_tally(0, authenticate_once_item_count),
  }
}

/// The control: the same 64 items, the same answer, the same fixture, but the
/// field is re-authenticated for every one of them.
///
/// **What the gap is made of.** §10.7 has this measurement re-taken whenever
/// the grammar moves, and the margin over the GOAL_SPEC §3.3 basis is 0.57%, so the
/// attribution has to be right or the next re-take will mis-model it. The gap
/// against `authenticate_once_one_open_every_read` is 4.49 M memory units
/// (13.27 M against 8.78 M), and it is **not** 63 extra hash checks alone: it
/// decomposes into roughly 3.48 M of extra door-opens and roughly 1.01 M of
/// access-pattern difference, because this control relocates with `walk_skip`
/// once per item and the fold does not relocate at all.
///
/// The conclusion is unchanged, and it survives holding the relocations
/// constant: one open plus the same 64 relocations costs 9.79 M against this
/// control's 13.27 M, still either side of the 13.2 M basis. Door-opens are
/// what move it across the line; the access pattern is why the raw gap is
/// larger than the hashing alone accounts for.
test authenticate_once_reopen_per_item_costs_more() {
  let preimage = authenticate_once_preimage()
  let committed = source(sample_tx_id, 0, preimage)
  let total = reopen_each(committed, preimage, 0, 0)
  total == expected_tally(0, authenticate_once_item_count)
}

fn reopen_each(
  committed: Pair<VerifiedMidgardNativeTxCompact, NativeTxWitnessSetCompact>,
  preimage: ByteArray,
  index: Int,
  state: Int,
) -> Int {
  if index >= authenticate_once_item_count {
    state
  } else {
    let Pair(view, start) = open_inline_from(committed, 0, preimage)
    let checkpoint = walk_skip(view, start, index)
    let Pair(item, _next) = walk_next(view, checkpoint)
    reopen_each(committed, preimage, index + 1, tally(state, index, item))
  }
}

/// The control that holds the **access pattern** constant instead of the item
/// count: one open, then the same sixty-four `walk_skip` relocations and the
/// same sixty-four reads the test above performs, differing from it only in how
/// many times the door is opened.
///
/// This is the second number §10.7 item 1 quotes. Without it the 9.79 M would
/// have to be re-derived by hand at every re-take, and §10.7 tells maintainers
/// to re-take from the suite — so the suite has to be able to produce it. Read
/// the runner's three field-0 rows together: this one against
/// `..._reopen_per_item_costs_more` isolates the door-opens (same relocations),
/// and against `..._one_open_every_read` isolates the relocations (same single
/// open).
test authenticate_once_one_open_every_relocation() {
  let preimage = authenticate_once_preimage()
  let committed = source(sample_tx_id, 0, preimage)
  let Pair(view, start) = open_inline_from(committed, 0, preimage)
  let total = relocate_each(view, start, 0, 0)
  total == expected_tally(0, authenticate_once_item_count)
}

fn relocate_each(
  view: FieldViewV1,
  start: FieldWalkCheckpointV1,
  index: Int,
  state: Int,
) -> Int {
  if index >= authenticate_once_item_count {
    state
  } else {
    let checkpoint = walk_skip(view, start, index)
    let Pair(item, _next) = walk_next(view, checkpoint)
    relocate_each(view, start, index + 1, tally(state, index, item))
  }
}

/// The expected tally, computed without the walk so the walk is not marking its
/// own homework.
fn expected_tally(state: Int, remaining: Int) -> Int {
  expected_tally_from(state, 0, remaining)
}

fn expected_tally_from(state: Int, index: Int, remaining: Int) -> Int {
  if remaining <= 0 {
    state
  } else {
    expected_tally_from(
      tally(state, index, input_item(item_tx_id_a, index)),
      index + 1,
      remaining - 1,
    )
  }
}

/// Authenticate-once holds across the ladder, not just at tier 1: a view is a
/// view, and the walk never sees the carriage that produced it. Tier 2 carries
/// the same 64 items through a reference input and the walk is unchanged.
test authenticate_once_holds_under_tier_two() {
  let preimage = authenticate_once_preimage()
  let Pair(view, start) = open_raw_utxo(0, preimage)
  let Pair(total, done) =
    walk_fold(view, start, authenticate_once_item_count, 0, tally)
  and {
    walk_is_complete(done),
    total == expected_tally(0, authenticate_once_item_count),
  }
}

/// Tier 3 completes the ladder. The certified carriage never pays a
/// full-preimage hash at all — the certificate did that at mint — so the
/// walk's per-item cost is a chunk verify, and the seam is identical.
test authenticate_once_holds_under_tier_three() {
  let item_count = chunk_bytes_k / spend_input_stride + 1
  let preimage =
    bytearray.concat(
      encode_field_array_header(item_count),
      repeat_bytes(wrapped_input_item(), item_count),
    )
  let Pair(view, start) = open_certified(0, preimage)
  let checkpoint = walk_skip(view, start, item_count - 1)
  let Pair(item, done) = walk_next(view, checkpoint)
  and {
    bytearray.length(preimage) > chunk_bytes_k,
    spend_input_count(view) == item_count,
    item == input_item(item_tx_id_a, 0),
    walk_is_complete(done),
  }
}

fn wrapped_input_item() -> ByteArray {
  bytearray.concat(#"5826", input_item(item_tx_id_a, 0))
}

fn repeat_bytes(unit: ByteArray, times: Int) -> ByteArray {
  if times <= 0 {
    #""
  } else if times == 1 {
    unit
  } else {
    let half = repeat_bytes(unit, times / 2)
    let doubled = bytearray.concat(half, half)
    if times % 2 == 0 {
      doubled
    } else {
      bytearray.concat(doubled, unit)
    }
  }
}

// ---------------------------------------------------------------------------
// §10.2 / §10.6 — interrupt and resume
// ---------------------------------------------------------------------------

/// The uninterrupted answer, for the resumed walks to be measured against.
fn uninterrupted_tally() -> Int {
  let preimage = walk_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(total, done) = walk_fold(view, start, walk_field_item_count, 0, tally)
  expect walk_is_complete(done)
  total
}

/// AC2, whole. Transaction one exhausts its budget after seven items and
/// commits `field_walk_checkpoint_hash`; the thread carries that digest and
/// nothing else. Transaction two re-supplies the 53 positional bytes, re-opens
/// the field under a **different carriage tier**, and finishes — and lands on
/// the answer the uninterrupted walk gives.
///
/// The interruption is real: transaction two's fold budget is exactly the
/// thirteen items transaction one did not reach, so a walk that restarted at
/// item 0 would run out of budget short of the end and `walk_is_complete` would
/// be false.
test resume_finishes_an_interrupted_walk_across_a_carriage_change() {
  let preimage = walk_preimage()
  let budget_one = 7

  // ---- transaction one: tier 1, seven items, commit the position ----
  let Pair(view_one, start) = open_inline(6, preimage)
  let Pair(partial, paused) = walk_fold(view_one, start, budget_one, 0, tally)
  let thread_commitment = field_walk_checkpoint_hash(paused)
  let carried = encode_field_walk_checkpoint(paused)

  // ---- transaction two: tier 2, the rest, from the committed position ----
  let Pair(verified, witness_set) = source(sample_tx_id, 6, preimage)
  let Pair(view_two, resumed) =
    resume_field_walk_from_commitment(
      verified,
      witness_set,
      thread_commitment,
      carried,
      RawUtxo { ref_input_index: 0 },
      [input_of(0, key_output(builtin.b_data(preimage)))],
      certificate_policy_id,
    )
  let Pair(total, done) =
    walk_fold(
      view_two,
      resumed,
      walk_field_item_count - budget_one,
      partial,
      tally,
    )
  and {
    !walk_is_complete(paused),
    walk_remaining(paused) == walk_field_item_count - budget_one,
    walk_is_complete(done),
    total == uninterrupted_tally(),
  }
}

/// Resuming twice is resuming once: a walk can be interrupted as often as the
/// budget requires and still land on one answer.
test resume_survives_two_interruptions() {
  let preimage = walk_preimage()
  let Pair(view_one, start) = open_inline(6, preimage)
  let Pair(state_one, pause_one) = walk_fold(view_one, start, 4, 0, tally)
  let Pair(view_two, resume_one) = resume_at(preimage, pause_one)
  let Pair(state_two, pause_two) =
    walk_fold(view_two, resume_one, 9, state_one, tally)
  let Pair(view_three, resume_two) = resume_at(preimage, pause_two)
  let Pair(total, done) =
    walk_fold(
      view_three,
      resume_two,
      walk_field_item_count - 13,
      state_two,
      tally,
    )
  and {
    walk_remaining(pause_one) == walk_field_item_count - 4,
    walk_remaining(pause_two) == walk_field_item_count - 13,
    walk_is_complete(done),
    total == uninterrupted_tally(),
  }
}

/// The full round trip a thread actually performs: commit the digest, carry
/// the bytes, decode them back, re-derive the digest.
fn resume_at(
  preimage: ByteArray,
  checkpoint: FieldWalkCheckpointV1,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let Pair(verified, witness_set) = source(sample_tx_id, 6, preimage)
  resume_field_walk_from_commitment(
    verified,
    witness_set,
    field_walk_checkpoint_hash(checkpoint),
    encode_field_walk_checkpoint(checkpoint),
    Inline { preimage },
    [],
    certificate_policy_id,
  )
}

/// A resumed transaction reads *its own* items and no others. The evidence is
/// budget rather than cost: with a budget of exactly the remaining items the
/// walk completes, and with one fewer it does not — which is only possible if
/// resuming consumed none of the budget on the seven items already done.
test resume_does_not_re_read_completed_items() {
  let preimage = walk_preimage()
  let Pair(view_one, start) = open_inline(6, preimage)
  let Pair(_partial, paused) = walk_fold(view_one, start, 7, 0, tally)
  let Pair(view_two, resumed) = resume_at(preimage, paused)
  let Pair(_a, exact) =
    walk_fold(view_two, resumed, walk_field_item_count - 7, 0, tally)
  let Pair(_b, one_short) =
    walk_fold(view_two, resumed, walk_field_item_count - 8, 0, tally)
  and {
    walk_is_complete(exact),
    !walk_is_complete(one_short),
    walk_remaining(one_short) == 1,
  }
}

/// The fixed-stride shortcut is resumable too, and there the carried position
/// is self-authenticating: `resume` recomputes it from the index alone.
test resume_works_on_a_fixed_stride_field() {
  let preimage = authenticate_once_preimage()
  let Pair(view_one, start) = open_inline(0, preimage)
  let Pair(_partial, paused) = walk_fold(view_one, start, 40, 0, tally)
  let Pair(verified, witness_set) = source(sample_tx_id, 0, preimage)
  let Pair(view_two, resumed) =
    resume_field_walk_from_commitment(
      verified,
      witness_set,
      field_walk_checkpoint_hash(paused),
      encode_field_walk_checkpoint(paused),
      Inline { preimage },
      [],
      certificate_policy_id,
    )
  let Pair(item, _next) = walk_next(view_two, resumed)
  and {
    walk_remaining(resumed) == authenticate_once_item_count - 40,
    item == input_item(item_tx_id_a, 40),
  }
}

// ---------------------------------------------------------------------------
// §7.6 / §10.3 — positions, not bytes
// ---------------------------------------------------------------------------

/// The strongest form of "no preimage bytes" this seam can state.
///
/// Two field-0 preimages share every width and every position, and differ in
/// **every byte the grammar leaves free**: different item tx-ids and different
/// output indices throughout. What they necessarily share is only what a fixed
/// 38-byte item has no freedom in — the array header, each item's `58 26`
/// wrapper, and the `82`, `58 20` and `19` heads inside it — which is why the
/// claim is stated this way rather than as "no content in common". A field-0
/// item cannot be made to differ there and still be a field-0 item.
///
/// Walked to the same position, their checkpoints are byte-identical and commit
/// to the same digest. A structure carrying any preimage content could not do
/// that. The disjointness is asserted here rather than described, so the
/// fixture cannot weaken without the test noticing.
test no_preimage_bytes_reach_the_checkpoint() {
  let preimage_a = encode_field_preimage(input_items(item_tx_id_a, 0, 12))
  let preimage_b =
    encode_field_preimage(input_items(item_tx_id_b, disjoint_index_base, 12))
  let Pair(view_a, start_a) = open_inline(0, preimage_a)
  let Pair(view_b, start_b) = open_inline(0, preimage_b)
  let Pair(_a, paused_a) = walk_fold(view_a, start_a, 5, 0, tally)
  let Pair(_b, paused_b) = walk_fold(view_b, start_b, 5, 0, tally)
  and {
    // The bytes really are different, or the rest of this proves nothing.
    bytearray.length(preimage_a) == bytearray.length(preimage_b),
    input_items_are_content_disjoint(0, 12),
    encode_field_walk_checkpoint(paused_a) == encode_field_walk_checkpoint(
      paused_b,
    ),
    field_walk_checkpoint_hash(paused_a) == field_walk_checkpoint_hash(paused_b),
  }
}

/// The starting output index of the disjoint twin. Chosen past 255 so that
/// *both* bytes of §5.3's fixed two-byte index differ from the 0..11 the other
/// preimage uses — the high byte would otherwise be `00` in both and the pair
/// would share a byte it did not have to.
const disjoint_index_base: Int = 300

/// The same claim for a variable-width field, where an implementation tempted
/// to cache "where the items are" would have the most to gain from carrying
/// bytes. Two field-6 preimages with identical item widths, walked to the same
/// position, still serialise identically — and here the free bytes include the
/// language tag as well as the payload, so the pair shares only the `82` head
/// and the width-forced byte-string head.
test no_preimage_bytes_reach_a_variable_width_checkpoint() {
  let preimage_a = walk_preimage()
  let preimage_b =
    encode_field_preimage(other_script_items(0, walk_field_item_count))
  let Pair(view_a, start_a) = open_inline(6, preimage_a)
  let Pair(view_b, start_b) = open_inline(6, preimage_b)
  let Pair(_a, paused_a) = walk_fold(view_a, start_a, 11, 0, tally)
  let Pair(_b, paused_b) = walk_fold(view_b, start_b, 11, 0, tally)
  and {
    bytearray.length(preimage_a) == bytearray.length(preimage_b),
    script_items_are_content_disjoint(0, walk_field_item_count),
    encode_field_walk_checkpoint(paused_a) == encode_field_walk_checkpoint(
      paused_b,
    ),
  }
}

/// True when `a` and `b` differ at **every** byte of `[offset, offset+length)`.
/// Not "differ somewhere" — the fixtures above claim disjointness, and
/// disjointness is a per-byte property.
fn differs_everywhere(
  a: ByteArray,
  b: ByteArray,
  offset: Int,
  length: Int,
) -> Bool {
  if length <= 0 {
    True
  } else {
    and {
      bytearray.at(a, offset) != bytearray.at(b, offset),
      differs_everywhere(a, b, offset + 1, length - 1),
    }
  }
}

/// A field-0 item is `82 ‖ 58 20 ‖ tx_id(32) ‖ 19 ‖ index(2)`, so bytes 0..2
/// and 35 are grammar-forced and bytes 3..34 and 36..37 are free. Every free
/// byte of the twins differs, at every index.
fn input_items_are_content_disjoint(index: Int, remaining: Int) -> Bool {
  if remaining <= 0 {
    True
  } else {
    let a = input_item(item_tx_id_a, index)
    let b = input_item(item_tx_id_b, disjoint_index_base + index)
    and {
      bytearray.length(a) == bytearray.length(b),
      differs_everywhere(a, b, 3, 32),
      differs_everywhere(a, b, 36, 2),
      input_items_are_content_disjoint(index + 1, remaining - 1),
    }
  }
}

/// A field-6 item is `82 ‖ language(1) ‖ 4L ‖ payload`, so byte 0 and the
/// width-forced byte-string head at 2 are the only bytes the twins must share.
/// The language tag at 1 and every payload byte from 3 on differ.
fn script_items_are_content_disjoint(index: Int, remaining: Int) -> Bool {
  if remaining <= 0 {
    True
  } else {
    let a = script_item(index)
    let b = other_script_item(index)
    and {
      bytearray.length(a) == bytearray.length(b),
      differs_everywhere(a, b, 1, 1),
      differs_everywhere(a, b, 3, bytearray.length(a) - 3),
      script_items_are_content_disjoint(index + 1, remaining - 1),
    }
  }
}

/// §10.3's constant width, at both ends of a fixed-stride field and of a
/// variable-width one. A wire form whose length never moves cannot be
/// smuggling content whose length does.
test checkpoint_wire_form_is_constant_width() {
  let fixed = authenticate_once_preimage()
  let variable = walk_preimage()
  let widths =
    [
      checkpoint_width(0, fixed, 0),
      checkpoint_width(0, fixed, 63),
      checkpoint_width(0, fixed, authenticate_once_item_count),
      checkpoint_width(6, variable, 0),
      checkpoint_width(6, variable, 13),
      checkpoint_width(6, variable, walk_field_item_count),
    ]
  list.all(widths, fn(width) { width == field_walk_checkpoint_bytes })
}

/// And at the format's largest position: item 295 of a 296-item field is still
/// 53 bytes, so the wire form does not grow with the preimage either.
test checkpoint_wire_form_is_constant_at_the_spend_input_maximum() {
  checkpoint_width(
    0,
    spend_input_preimage(),
    maximum_cardano_spend_redeemer_count - 1,
  ) == field_walk_checkpoint_bytes
}

fn checkpoint_width(field_index: Int, preimage: ByteArray, at: Int) -> Int {
  let Pair(view, start) = open_inline(field_index, preimage)
  bytearray.length(encode_field_walk_checkpoint(walk_skip(view, start, at)))
}

/// The wire form round-trips, so a thread that carries the bytes recovers
/// exactly the position that was committed.
///
/// Stated at the seam, because the decoder is private (§10.6): the round trip
/// that matters is the one a resuming step performs, and that is the only one
/// there is. This also pins the wire-form offsets the negative vectors patch —
/// `wire_scalar` reading `encode_field_walk_checkpoint`'s output back at
/// §10.3's documented positions is what makes those patches land where they
/// claim to.
test checkpoint_round_trips_through_its_wire_form() {
  let preimage = walk_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(_state, paused) = walk_fold(view, start, 9, 0, tally)
  let carried = encode_field_walk_checkpoint(paused)
  let Pair(_view, recovered) = resume_wire(6, preimage, carried)
  and {
    recovered == paused,
    wire_scalar(carried, wire_total_length_at) == bytearray.length(preimage),
    wire_scalar(carried, wire_item_count_at) == walk_field_item_count,
    wire_scalar(carried, wire_next_item_index_at) == 9,
  }
}

// ---------------------------------------------------------------------------
// §5.3 / §10.5 — the spend-input arithmetic slice
// ---------------------------------------------------------------------------
//
// The two tests below build the same 296-item field-0 view — §5.4's operative
// spend maximum — and differ only in which index they read. Equal cost is the
// claim: a lookup that walked would cost 295 more envelope decodes at the far
// end than at the near one.

test spend_input_lookup_at_index_0() {
  let Pair(view, _start) = open_inline(0, spend_input_preimage())
  let input = spend_input_at(view, 0)
  and {
    input.output_index == 0,
    input.tx_id == item_tx_id_a,
  }
}

test spend_input_lookup_at_index_295() {
  let Pair(view, _start) = open_inline(0, spend_input_preimage())
  let input = spend_input_at(view, 295)
  and {
    input.output_index == 295,
    input.tx_id == item_tx_id_a,
  }
}

/// The arithmetic itself, stated at the seam rather than measured: item `i`
/// sits at `header_len + 40·i`, its payload two bytes further in, and it is
/// 38 bytes wide — for the first item, the last, and one in the middle.
test spend_input_extent_is_pure_arithmetic() {
  let Pair(view, _start) = open_inline(0, spend_input_preimage())
  let header_len = 3
  let extent_matches =
    fn(index: Int) -> Bool {
      let Pair(offset, length) = field_item_extent(view, index)
      and {
        offset == header_len + spend_input_stride * index + 2,
        length == spend_input_item_bytes,
      }
    }
  and {
    spend_input_count(view) == maximum_cardano_spend_redeemer_count,
    extent_matches(0),
    extent_matches(1),
    extent_matches(147),
    extent_matches(295),
  }
}

/// The counterpart cost claim for a variable-width field, so the flat-in-index
/// numbers above are read against something that is not flat. `walk_skip` on
/// field 6 has nothing to multiply and must walk; these two differ only in how
/// far.
test variable_width_skip_to_index_1() {
  let Pair(view, start) = open_inline(6, walk_preimage())
  let Pair(item, _next) = walk_next(view, walk_skip(view, start, 1))
  item == script_item(1)
}

test variable_width_skip_to_index_19() {
  let Pair(view, start) = open_inline(6, walk_preimage())
  let Pair(item, _next) = walk_next(view, walk_skip(view, start, 19))
  item == script_item(19)
}

// ---------------------------------------------------------------------------
// Negative vectors — what the seam refuses
// ---------------------------------------------------------------------------
//
// Every vector below consumes the guarded call's result. Aiken discards an
// unused `let` binding outright — no error call survives it — so a negative
// vector that only bound the result would pass whether or not the guard fired.
//
// That is the coarse version of the hazard. The finer one is a vector that
// *does* consume the result but trips a **deeper** guard than the one it names,
// leaving the named guard with no coverage at all. So each guard the walk core
// documents gets a vector that isolates it: a fixture where every deeper guard
// is satisfied and only the named one can refuse. Each of those is proved
// load-bearing by neutralising its guard and watching that test — and only that
// test — go red. Where a vector is a composite that cannot isolate anything, it
// says so and names the vector that does.
//
// Seven of the core's twenty-three operational refusals cannot be isolated by
// any fixture, because a check that runs earlier refuses everything they would.
// Those are **not** covered here and are not meant to be: they are consistency
// backstops, and §10.8 is the enumeration, one row each, naming the check that
// makes each of them unreachable. The rule this file follows is that a guard is
// either isolated below or listed there — never quietly uncovered.

/// Forces a resumed view to be observed, so the guard under test is actually
/// reached.
fn resumed_item(
  view: FieldViewV1,
  checkpoint: FieldWalkCheckpointV1,
) -> ByteArray {
  let Pair(item, _next) = walk_next(view, checkpoint)
  item
}

/// A checkpoint is bound to one transaction. Replaying it against another
/// dispute must fail even when the field bytes are identical, because §4's
/// plain hashing gives identical content identical commitments.
///
/// Isolating: the commitment matches, the field bytes authenticate under the
/// other transaction's committed structures, and every clause of the binding
/// check would pass — only the `tx_id` equality refuses.
test resume_rejects_a_checkpoint_from_another_transaction() fail {
  let preimage = walk_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(_state, paused) = walk_fold(view, start, 5, 0, tally)
  let Pair(verified, witness_set) = source(other_tx_id, 6, preimage)
  let Pair(resumed, resumed_checkpoint) =
    resume_field_walk_from_commitment(
      verified,
      witness_set,
      field_walk_checkpoint_hash(paused),
      encode_field_walk_checkpoint(paused),
      Inline { preimage },
      [],
      certificate_policy_id,
    )
  resumed_item(resumed, resumed_checkpoint) == script_item(5)
}

/// The thread commits the position. A checkpoint the thread did not commit —
/// here the honest one advanced by a single item — cannot be resumed, which is
/// what stops a prover from choosing its own starting position on a
/// variable-width field.
test resume_rejects_a_position_the_thread_did_not_commit() fail {
  let preimage = walk_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(_state, paused) = walk_fold(view, start, 5, 0, tally)
  let Pair(_item, advanced) = walk_next(view, paused)
  let Pair(verified, witness_set) = source(sample_tx_id, 6, preimage)
  let Pair(resumed_view, resumed_checkpoint) =
    resume_field_walk_from_commitment(
      verified,
      witness_set,
      field_walk_checkpoint_hash(paused),
      encode_field_walk_checkpoint(advanced),
      Inline { preimage },
      [],
      certificate_policy_id,
    )
  resumed_item(resumed_view, resumed_checkpoint) == script_item(6)
}

/// The same thread commitment accepts the position it actually committed, so
/// the vector above is failing on the commitment and not on some unrelated
/// guard the fixture happens to trip.
///
/// It also pins this file's copy of §10.3's domain string against the module's:
/// every negative vector below commits its forged bytes with `thread_commitment`,
/// so if the two ever diverged those vectors would start refusing on the
/// commitment check instead of on the guard they name — and this assertion is
/// what would go red first.
test resume_accepts_the_position_the_thread_committed() {
  let preimage = walk_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(_state, paused) = walk_fold(view, start, 5, 0, tally)
  let Pair(verified, witness_set) = source(sample_tx_id, 6, preimage)
  let Pair(resumed_view, resumed_checkpoint) =
    resume_field_walk_from_commitment(
      verified,
      witness_set,
      field_walk_checkpoint_hash(paused),
      encode_field_walk_checkpoint(paused),
      Inline { preimage },
      [],
      certificate_policy_id,
    )
  and {
    thread_commitment(encode_field_walk_checkpoint(paused)) == field_walk_checkpoint_hash(
      paused,
    ),
    resumed_item(resumed_view, resumed_checkpoint) == script_item(5),
  }
}

/// On a fixed-stride field the position is a function of the index, so a
/// forged offset is caught outright rather than merely uncommitted.
///
/// Isolating: shape and bounds all agree with the fresh view, so §10.2 item 6's
/// recompute is the only clause that can refuse.
test resume_rejects_a_forged_fixed_stride_offset() fail {
  let preimage = authenticate_once_preimage()
  let Pair(view, start) = open_inline(0, preimage)
  let Pair(_state, paused) = walk_fold(view, start, 5, 0, tally)
  let carried = encode_field_walk_checkpoint(paused)
  let forged =
    patch_wire_scalar(
      carried,
      wire_next_offset_at,
      wire_scalar(carried, wire_next_offset_at) + spend_input_stride,
    )
  let Pair(resumed_view, resumed) = resume_wire(0, preimage, forged)
  resumed_item(resumed_view, resumed) == input_item(item_tx_id_a, 6)
}

/// §10.2 item 3, `total_length` alone. The checkpoint agrees with the fresh
/// view on the item count, the index is in range, the offset is in range and
/// still recomputes from the stride — the *only* disagreement is the
/// authenticated preimage length the position was taken against.
///
/// This is what `resume_rejects_a_reshaped_view` below cannot do: a genuinely
/// reshaped carriage disagrees on the length, the count and the offset at once,
/// so deleting any one of the three clauses leaves the other two refusing.
test resume_rejects_a_forged_total_length() fail {
  let preimage = authenticate_once_preimage()
  let Pair(view, start) = open_inline(0, preimage)
  let Pair(_state, paused) = walk_fold(view, start, 5, 0, tally)
  let carried = encode_field_walk_checkpoint(paused)
  let forged =
    patch_wire_scalar(
      carried,
      wire_total_length_at,
      wire_scalar(carried, wire_total_length_at) + 1,
    )
  let Pair(resumed_view, resumed) = resume_wire(0, preimage, forged)
  resumed_item(resumed_view, resumed) == input_item(item_tx_id_a, 5)
}

/// §10.2 item 3, `item_count` alone. One item fewer than the view actually
/// holds: the length still matches, index 5 is still inside the forged count,
/// and the offset still recomputes, so nothing but the count clause is left to
/// refuse it.
///
/// A short count is the dangerous direction — it is what a prover would forge
/// to make a walk report itself finished before it had read the item that
/// faults it.
test resume_rejects_a_forged_item_count() fail {
  let preimage = authenticate_once_preimage()
  let Pair(view, start) = open_inline(0, preimage)
  let Pair(_state, paused) = walk_fold(view, start, 5, 0, tally)
  let carried = encode_field_walk_checkpoint(paused)
  let forged =
    patch_wire_scalar(
      carried,
      wire_item_count_at,
      wire_scalar(carried, wire_item_count_at) - 1,
    )
  let Pair(resumed_view, resumed) = resume_wire(0, preimage, forged)
  resumed_item(resumed_view, resumed) == input_item(item_tx_id_a, 5)
}

/// A checkpoint carries the shape it was taken against, so it cannot be pointed
/// at a differently-shaped carriage of the same field.
///
/// **A composite, and it isolates nothing.** A 64-item checkpoint against a
/// 12-item view disagrees on `total_length`, on `item_count` and on the
/// fixed-stride offset recompute simultaneously, so it stays red however many
/// of those three clauses are removed — it is the realistic attack, not the
/// coverage. The two vectors above pin the shape clauses individually and
/// `resume_rejects_a_forged_fixed_stride_offset` pins the third.
test resume_rejects_a_reshaped_view() fail {
  let preimage = authenticate_once_preimage()
  let shorter = encode_field_preimage(input_items(item_tx_id_a, 0, 12))
  let Pair(view, start) = open_inline(0, preimage)
  let Pair(_state, paused) = walk_fold(view, start, 5, 0, tally)
  let Pair(resumed_view, resumed) =
    resume_wire(0, shorter, encode_field_walk_checkpoint(paused))
  resumed_item(resumed_view, resumed) == input_item(item_tx_id_a, 5)
}

/// §10.2 item 4's `header_len ≤ next_offset` clause, isolated: a position aimed
/// **into the §5.1 array header itself**.
///
/// Sixty-four items is not an arbitrary size. It makes the header `98 40`, and
/// `40` read as a §5.1 item head is a well-formed zero-length byte string — so
/// at offset 1 item 7's fallback clause finds a decodable head whose item ends
/// at offset 2, inside the field, and passes. Every other clause passes too.
/// Only "the offset is at least `header_len`" stands between a walk and a
/// position inside the structure that says how many items there are.
///
/// It has to be a variable-width field: item 6's recompute puts a fixed-stride
/// offset at `header_len + stride · index`, which is never below `header_len`.
test resume_rejects_an_offset_inside_the_array_header() fail {
  let preimage = encode_field_preimage(script_items(0, header_probe_item_count))
  let Pair(_view, start) = open_inline(6, preimage)
  let forged =
    patch_wire_scalar(encode_field_walk_checkpoint(start), wire_next_offset_at, 1)
  let Pair(_view, resumed) = resume_wire(6, preimage, forged)
  walk_remaining(resumed) == header_probe_item_count
}

/// Chosen so §5.1 spells the array header `98 40` and its count byte is itself
/// a valid item head. Anything below 24 gives a one-byte header with no interior
/// to aim at, and anything outside 64..87 gives a count byte no head reader
/// accepts.
const header_probe_item_count: Int = 64

/// §10.2 item 5, isolated — and the forgery it describes is the one worth
/// caring about: a prover declaring a walk finished at the point it had reached,
/// so that the item which faults the transaction is never read.
///
/// The checkpoint is an honest five-item position with `next_item_index`
/// patched up to `item_count`. Shape and bounds all still agree with the fresh
/// view, and because the walk claims to be complete the binding check takes its
/// first branch and reads no bytes at all — so no deeper guard is even
/// consulted. `next_offset == total_length` is the whole of what can refuse it.
test resume_rejects_a_walk_that_declares_itself_finished_early() fail {
  let preimage = walk_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(_state, paused) = walk_fold(view, start, 5, 0, tally)
  let carried = encode_field_walk_checkpoint(paused)
  let forged =
    patch_wire_scalar(
      carried,
      wire_next_item_index_at,
      wire_scalar(carried, wire_item_count_at),
    )
  let Pair(_view, resumed) = resume_wire(6, preimage, forged)
  walk_is_complete(resumed)
}

/// A twelve-byte field-6 preimage — `82 ‖ 44 aa bb cc dd ‖ 45 58 20 11 22 33` —
/// whose second item's payload opens with `58 20`. At offset 7 that is a
/// perfectly well-formed §5.1 head announcing a 32-byte item, and only three
/// bytes of the field remain after it.
fn overrunning_item_preimage() -> ByteArray {
  #"8244aabbccdd455820112233"
}

const overrunning_item_offset: Int = 7

/// §10.2 item 7's extent clause, isolated. This is the closest a resume gets to
/// the hazard item 7 admits it cannot close, so it is the clause most worth
/// having a vector for.
///
/// The offset lands inside an item's payload, which item 7 by design does not
/// detect — a variable-width position is not recomputable. What it *does* check
/// is that whatever head it finds there describes an item ending inside the
/// authenticated bytes, and here it does not: `58 20` at offset 7 announces
/// thirty-two bytes where three remain. Every other clause of the binding check
/// passes, and the head decodes cleanly, so the extent comparison is the only
/// thing left.
///
/// The vector stops at the resume deliberately. Stepping the returned position
/// would abort in `field_read_range` as well, and a vector that let it do so
/// would be a composite proving nothing about this clause.
test resume_rejects_a_position_whose_item_runs_past_the_field() fail {
  let preimage = overrunning_item_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(_item, advanced) = walk_next(view, start)
  let forged =
    patch_wire_scalar(
      encode_field_walk_checkpoint(advanced),
      wire_next_offset_at,
      overrunning_item_offset,
    )
  let Pair(_view, resumed) = resume_wire(6, preimage, forged)
  walk_remaining(resumed) == 1
}

/// §5.1 leaves no trailing bytes and §7.3 aborts rather than clamps: the step
/// past the last item is an error, not an empty read.
///
/// The natural case, and a composite — necessarily so. Every complete
/// checkpoint a caller can hold sits at `total_length`: `walk_at_start` derives
/// it, the advances compute it, and §10.2 item 5 refuses any other at resume,
/// with the opaque type leaving no fourth way to obtain one. So removing
/// `walk_next`'s completeness guard only moves the abort one line down, to the
/// head read that runs off the end of the authenticated bytes. That guard is
/// §10.8's backstop row 4; `resume_rejects_a_walk_that_declares_itself_finished_early`
/// is what pins the clause that makes it redundant.
test walk_next_refuses_to_step_past_the_end() fail {
  let preimage = walk_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(_state, done) = walk_fold(view, start, walk_field_item_count, 0, tally)
  let Pair(_item, _next) = walk_next(view, done)
  True
}

/// A ten-byte field-6 preimage — `82 ‖ 44 aa 41 cc dd ‖ 43 ee ff 99` — in which
/// offset 3 is a `41` head *inside item 0's payload*: it decodes, and the
/// one-byte item it describes ends at offset 5, comfortably inside the field.
/// Resuming there and taking one step therefore reaches `next_item_index ==
/// item_count` at offset 5 rather than at `total_length`.
fn short_final_advance_preimage() -> ByteArray {
  #"8244aa41ccdd43eeff99"
}

const short_final_advance_offset: Int = 3

/// §5.1's no-trailing-bytes rule at the advance, isolated (§10.4: "the final
/// advance MUST land exactly on `total_length`").
///
/// Every deeper guard in the step is satisfied: the head at offset 3 decodes,
/// the field is variable-width so the §7.2 stride clause does not apply, and
/// the advance to offset 5 stays well inside the authenticated bytes — so the
/// bounds check cannot be what refuses. Only the clause that says the *last*
/// item must end the field can.
///
/// The resume that plants the position is honest about its own job: §10.2 item
/// 7 checks that the item at offset 3 ends inside the field, and it does. That
/// is exactly the residual item-7 gap, and this vector is the guard that
/// notices one step later.
test walk_next_refuses_a_final_advance_that_misses_the_end() fail {
  let preimage = short_final_advance_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(_item, advanced) = walk_next(view, start)
  let forged =
    patch_wire_scalar(
      encode_field_walk_checkpoint(advanced),
      wire_next_offset_at,
      short_final_advance_offset,
    )
  let Pair(resumed_view, resumed) = resume_wire(6, preimage, forged)
  resumed_item(resumed_view, resumed) == #"cc"
}

/// A two-item field-0 preimage whose **first** slot fills the whole 40-byte
/// stride but spells its §5.1 wrapper with `head` instead of the `58 26` the
/// stride admits.
///
/// The view constructs: §7.4's count check is arithmetic over the total length
/// (`header_len + stride · count`), which this satisfies, and per-item wrapper
/// canonicity is deliberately deferred to the read. So the only thing standing
/// between a miswrapped slot and a step is `walk_next`'s §7.2 check, which is
/// what makes this fixture able to isolate it.
///
/// The second slot is canonical on purpose: it leaves a next item to step to,
/// so the step's `next_item_index < item_count` clause is satisfied rather than
/// tripped, and the vector cannot pass on the no-trailing-bytes rule instead.
fn miswrapped_stride_preimage(head: ByteArray) -> ByteArray {
  encode_field_array_header(2)
    |> bytearray.concat(head)
    |> bytearray.concat(
        slot_filler(spend_input_stride - bytearray.length(head)),
      )
    |> bytearray.concat(wrapped_input_item())
}

fn slot_filler(length: Int) -> ByteArray {
  bytearray.take(repeat_bytes(item_tx_id_a, 2), length)
}

/// §7.2 / §10.4, the wrapper's declared length. On a fixed-stride field the
/// stride pins the item's one admissible spelling; here the head is well-formed
/// §5.1 and lands the payload exactly where the stride says, but declares 32
/// bytes where the stride admits only `stride − 2`.
///
/// This is the guard §10.4 states as "the wrapper MUST be the one form the
/// stride admits", and before this vector it had no test at all.
test walk_next_refuses_a_wrapper_whose_length_misses_the_stride() fail {
  let Pair(view, start) = open_inline(0, miswrapped_stride_preimage(#"5820"))
  let Pair(item, _next) = walk_next(view, start)
  item == item_tx_id_a
}

/// §7.2 / §10.4 again, from the other side: a one-byte `4L` head is inside
/// §5.1's grammar and decodes without complaint — it just puts the payload one
/// byte earlier than a stride-40 item's does, and declares a shorter one.
///
/// This vector pins the `expect or` **as a whole** and does not isolate its
/// `payload_offset` conjunct — nothing can. Minimal-width heads make the
/// payload offset a function of the declared length, and every §5.3 stride
/// lands the admissible length in the two-byte band, so the length conjunct
/// already refuses everything the offset conjunct would. Kept anyway: a
/// one-byte head is a distinct thing a prover would try, and this is one of
/// the two cheapest vectors in the file.
test walk_next_refuses_a_one_byte_wrapper_on_a_fixed_stride_field() fail {
  let Pair(view, start) = open_inline(0, miswrapped_stride_preimage(#"57"))
  let Pair(item, _next) = walk_next(view, start)
  item == bytearray.take(item_tx_id_a, 23)
}

/// `walk_skip` is bounded by the authenticated item count, not by the bytes it
/// happens to land in.
///
/// A composite on the variable-width path: with the bound removed this walks,
/// and the walk runs off the end into `walk_next`'s completeness guard. The
/// fixed-stride vector below is the isolating one, because there a relocation
/// reads nothing at all.
test walk_skip_refuses_to_pass_the_item_count() fail {
  let preimage = walk_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let past = walk_skip(view, start, walk_field_item_count + 1)
  walk_remaining(past) == -1
}

/// `walk_skip`'s bound, isolated. On a fixed-stride field relocating is one
/// multiplication and touches no bytes, so an over-long target produces a
/// checkpoint rather than tripping any read guard — which makes this the only
/// shape of fixture in which the bound is demonstrably the thing refusing.
test walk_skip_refuses_to_pass_the_item_count_on_a_fixed_stride_field() fail {
  let preimage = authenticate_once_preimage()
  let Pair(view, start) = open_inline(0, preimage)
  let past = walk_skip(view, start, authenticate_once_item_count + 1)
  walk_remaining(past) == -1
}

/// A relocation moves forward. Backwards it would run the fixed-stride
/// arithmetic on a negative index and hand back a position below the header,
/// which is a checkpoint no walk reached — so the count is held non-negative
/// rather than left to the target bound, which a backward step satisfies.
///
/// Isolating: `target = 63` is inside the item count, so the bound below has
/// nothing to say about it.
test walk_skip_refuses_a_negative_relocation() fail {
  let preimage = authenticate_once_preimage()
  let Pair(view, start) = open_inline(0, preimage)
  walk_remaining(walk_skip(view, start, -1)) == authenticate_once_item_count + 1
}

/// A budget is a count of items, and a negative one is not "no items" — the
/// recursion tests `budget == 0`, so a negative budget would step straight past
/// it and fold the whole field, which is the opposite of what a step that
/// budgeted nothing asked for.
///
/// Isolating: with the guard removed this walks to completion and lands on the
/// uninterrupted answer, which is what the assertion below catches.
test walk_fold_refuses_a_negative_budget() fail {
  let preimage = walk_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(total, done) = walk_fold(view, start, -1, 0, tally)
  and {
    walk_is_complete(done),
    total == uninterrupted_tally(),
  }
}

/// The O(1) input shortcut is guarded on the stride, so a variable-width view
/// cannot be read as inputs by accident — fields 6 and 0 alias on any item that
/// happens to be 38 bytes, which is exactly the fixture here: a field-6 view
/// whose items *are* well-formed spend inputs.
///
/// Isolating, and it has to be. A field-6 view of ordinary script items is
/// refused by the width check inside `spend_input_at` before the stride guard's
/// absence could be noticed, so a fixture built from those proves nothing about
/// the stride. Here every check downstream of the stride guard passes — the
/// item is 38 bytes and decodes to a `MidgardTxInput` — and only the stride
/// tells field 6 from field 0.
test spend_input_at_refuses_a_variable_width_view_of_input_shaped_items() fail {
  let Pair(view, _start) =
    open_inline(6, encode_field_preimage(input_items(item_tx_id_a, 0, 2)))
  let input = spend_input_at(view, 0)
  and {
    input.output_index == 0,
    input.tx_id == item_tx_id_a,
  }
}

/// §7.3 again, on the arithmetic path: an index past the count aborts rather
/// than returning clamped bytes.
test spend_input_at_refuses_an_out_of_range_index() fail {
  let Pair(view, _start) = open_inline(0, spend_input_preimage())
  spend_input_at(view, maximum_cardano_spend_redeemer_count).output_index == maximum_cardano_spend_redeemer_count
}

/// The count accessor carries the same stride guard as the item accessor, and
/// needs its own vector: `spend_input_at`'s guard being load-bearing says
/// nothing about this one. A field-6 view of input-shaped items would otherwise
/// report a spend-input count for a field that has no spend inputs in it.
test spend_input_count_refuses_a_variable_width_view() fail {
  let Pair(view, _start) =
    open_inline(6, encode_field_preimage(input_items(item_tx_id_a, 0, 2)))
  spend_input_count(view) == 2
}

/// §6.1: the checkpoint wire form has exactly one admissible spelling, so a
/// re-spelled encoding of the same position is refused rather than accepted as
/// equivalent.
///
/// Isolating, and it takes some care to be so. The thread commitment here is
/// the digest of the **canonical** bytes, not of the respelling — so a decoder
/// that accepted `85` would recover exactly `paused`, whose digest matches, and
/// the resume would go through and hand back a usable position. Committing the
/// respelled bytes instead would have let the commitment check refuse and
/// proved nothing about canonicity.
test checkpoint_decode_refuses_a_non_canonical_spelling() fail {
  let preimage = walk_preimage()
  let Pair(view, start) = open_inline(6, preimage)
  let Pair(_state, paused) = walk_fold(view, start, 5, 0, tally)
  let carried = encode_field_walk_checkpoint(paused)
  // Same length, same six values, different leading array head: `85` where the
  // one admissible spelling has `86`. A decoder that read the fields by offset
  // and stopped there would accept it.
  let respelled = bytearray.concat(#"85", bytearray.drop(carried, 1))
  let Pair(verified, witness_set) = source(sample_tx_id, 6, preimage)
  let Pair(_view, resumed) =
    resume_field_walk_from_commitment(
      verified,
      witness_set,
      field_walk_checkpoint_hash(paused),
      respelled,
      Inline { preimage },
      [],
      certificate_policy_id,
    )
  and {
    bytearray.length(respelled) == field_walk_checkpoint_bytes,
    walk_remaining(resumed) == walk_field_item_count - 5,
  }
}

/// §8.6 / §7.4: a variable-width field carried under tier 3 has no
/// authenticated item count, and a walk needs one. It aborts rather than open a
/// walk over a number nobody checked.
test tier_three_variable_width_field_cannot_be_walked() fail {
  let item = script_item(0)
  let wrapped = bytearray.concat(#"48", bytearray.take(item, 8))
  let count = chunk_bytes_k / bytearray.length(wrapped) + 1
  let preimage =
    bytearray.concat(
      encode_field_array_header(count),
      repeat_bytes(wrapped, count),
    )
  let Pair(_view, _start) = open_certified(6, preimage)
  True
}
