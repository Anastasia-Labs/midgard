//// Seam vectors for witness-minimal fault statements —
//// `docs/spec/midgard-tx.md` §12.
////
//// Everything here is asserted at the module's public seam:
//// `encode_fault_statement` / `decode_fault_statement` /
//// `fault_statement_hash`, `prove_item_fault`, the two sweep types'
//// seam (`open_*_unit_sweep`, `accumulate_output_unit`, `sweep_mint_unit`,
//// `output_sweep_*`, `mint_sweep_*`), `mint_unit_quantity` and
//// `asset_conservation_fault_is_proven`. No test names a private helper or pins
//// an intermediate offset the seam does not hand back.
////
//// **Two of §12.4's refusals have no vector here on purpose**, because they are
//// no longer refusals: `OutputUnitSweepV1` and `MintUnitSweepV1` are separate
//// types, so driving a mint sweep through the outputs fold and handing
//// `asset_conservation_fault_is_proven` its two arguments transposed are type
//// errors. A `fail` test cannot be written for code that does not compile, and
//// §12.6's rows say so rather than leaving the reader to wonder where the
//// vectors went.
////
//// Three claims, and how each is established here:
////
////   * **The witness is minimal** (§12.1, AC1). Two structural facts, both
////     asserted per byte rather than argued. An item-fault statement is
////     `item_fault_statement_bytes` long *whatever the field holds*; and the
////     same logical fault in two transactions whose preimages share no item at
////     any index is accused by two statements that differ in nothing but the 32
////     tx-id bytes, each proving against its own — which a witness carrying item
////     bytes could not do, since it would grow with the twenty items it named.
////     The adjudication side is pinned by
////     `item_fault_reads_exactly_the_named_item`: the checkpoint it returns is
////     one past the accused index, so exactly one item was consumed. The
////     statement is bound to the transaction it accuses, and
////     `item_fault_refuses_a_statement_about_another_transaction` is what makes
////     the tx-id half of the address load-bearing rather than decorative.
////   * **A conservation fault isolates one asset id** (§12.4, AC2). The sweeps
////     are driven over a field whose outputs hold many units and a mint field
////     with many policies, and asked for one; the answer is that unit's
////     arithmetic and no other's. Absence answers `0`, and all three things
////     that make that `0` evidence are shown to refuse when removed — an
////     unordered mint field cannot be swept (within one budget round *and*
////     across two), a zero-quantity entry is not a second spelling of an absent
////     asset, and a sweep that ran out of budget is not an answer at all. A
////     fourth is shown here for the first time: a sweep opened anywhere but
////     item 0 has measured a *suffix*, and on the mint side a suffix past the
////     target's group answers a final `0` about a unit the transaction really
////     minted. `asset_conservation_fault_is_proven` takes the two sweeps rather
////     than two integers, so the vectors can show it refusing a measurement of
////     another transaction, of another unit, and of a prefix.
////   * **The tier-2 per-step re-hash is measured, not asserted** (§12.5, AC3).
////     `tier2_*` are four rows differing only in how many times the door is
////     opened over the same fixture at the tier-2 bound, so the runner's
////     per-test row *is* the per-step cost and §12.5 quotes it. Their
////     assertions only pin that both arms compute the same answer, so that the
////     numbers are comparable at all.
////
//// **How a forgery is spelled here.** `FieldWalkCheckpointV1` is opaque
//// (§10.6), so no test writes a position. A `FaultStatementV1` by contrast is
//// an accusation anyone may write (§12.1), so the negative vectors here are
//// *false accusations* — statements naming the wrong field, the wrong
//// direction, an honest item, or a quantity that happens to be right — and the
//// claim is that each fails to prove rather than that it cannot be uttered.

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
use midgard/native_tx_fault_statement_v1.{
  FaultStatementV1, MintUnitSweepV1, OutputUnitSweepV1, accumulate_output_unit,
  asset_conservation_fault_is_proven, decode_fault_statement,
  encode_fault_statement, fault_asset_conservation, fault_item_predicate,
  fault_statement_frame_bytes, fault_statement_hash, item_fault_statement_bytes,
  mint_sweep_checkpoint, mint_sweep_is_final, mint_sweep_quantity,
  mint_unit_quantity, open_mint_unit_sweep, open_output_unit_sweep,
  output_sweep_checkpoint, output_sweep_is_final, output_sweep_quantity,
  prove_item_fault, sweep_mint_unit,
}
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, FieldViewV1, Inline, RawUtxo, chunk_bytes_k,
  encode_field_preimage, field_commitment, field_total_length,
}
use midgard/native_tx_machine_walk_v1.{
  FieldWalkCheckpointV1, open_field_walk, walk_next_item_index, walk_skip,
}

// ---------------------------------------------------------------------------
// Committed-structure fixtures (the §10 suite's idiom, unchanged)
// ---------------------------------------------------------------------------

const zero_hash32: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const sample_tx_id: ByteArray =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const certificate_policy_id: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

const item_tx_id_a: ByteArray =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const item_tx_id_b: ByteArray =
  #"5555555555555555555555555555555555555555555555555555555555555555"

const owner_hash: ByteArray =
  #"66666666666666666666666666666666666666666666666666666666"

/// The 29-byte Midgard address payload §5.5 admits, matching §11.1's suite.
const address_payload: ByteArray =
  #"60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

fn source(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
) -> Pair<VerifiedMidgardNativeTxCompact, NativeTxWitnessSetCompact> {
  let committed = field_commitment(preimage)
  let slot = fn(index: Int) -> ByteArray {
    if index == field_index {
      committed
    } else {
      zero_hash32
    }
  }
  let witness_set = NativeTxWitnessSetCompact {
    addr_tx_wits_hash: slot(7),
    script_tx_wits_hash: slot(6),
    redeemer_tx_wits_hash: slot(8),
  }
  let body = NativeTxBodyCompact {
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

/// Opened against a named transaction, because §12's adjudications are bound to
/// one: a fixture that could only ever be `sample_tx_id` could not tell a bound
/// adjudication from an unbound one.
fn open_inline_for(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  open(tx_id, field_index, preimage, Inline { preimage }, [])
}

fn open_inline(
  field_index: Int,
  preimage: ByteArray,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  open_inline_for(sample_tx_id, field_index, preimage)
}

/// Tier 2 (§8.2) — one nothing-but-bytes inline datum at the prover's key
/// address. Taken from committed structures the caller already holds, so that a
/// second open pays for the door and not for rebuilding the fixture.
fn open_raw_utxo_from(
  committed: Pair<VerifiedMidgardNativeTxCompact, NativeTxWitnessSetCompact>,
  field_index: Int,
  preimage: ByteArray,
) -> Pair<FieldViewV1, FieldWalkCheckpointV1> {
  let Pair(verified, witness_set) = committed
  open_field_walk(
    verified,
    witness_set,
    field_index,
    RawUtxo { ref_input_index: 0 },
    [input_of(0, key_output(builtin.b_data(preimage)))],
    certificate_policy_id,
  )
}

// ---------------------------------------------------------------------------
// Item fixtures
// ---------------------------------------------------------------------------

fn input_item(tx_id: ByteArray, index: Int) -> ByteArray {
  encode_midgard_tx_input(MidgardTxInput { tx_id, output_index: index })
}

fn input_items(tx_id: ByteArray, next: Int, remaining: Int) -> List<ByteArray> {
  if remaining <= 0 {
    []
  } else {
    [input_item(tx_id, next), ..input_items(tx_id, next + 1, remaining - 1)]
  }
}

/// A field-6 item whose width varies with the index, so the variable-width walk
/// has real work to do.
fn script_item(index: Int) -> ByteArray {
  encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: PlutusV3Script,
      script_bytes: bytearray.take(item_tx_id_a, 4 + index % 7),
    },
  )
}

/// `script_item`'s content-disjoint twin at every index: the same width, and
/// every byte the §5.1/§5.3 grammar leaves free is different. The language tag
/// is one of the free bytes — `NativeCardanoScript` serialises to `00` where
/// `PlutusV3Script` serialises to `03`.
fn other_script_item(index: Int) -> ByteArray {
  encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: bytearray.take(item_tx_id_b, 4 + index % 7),
    },
  )
}

fn script_items(next: Int, remaining: Int) -> List<ByteArray> {
  if remaining <= 0 {
    []
  } else {
    [script_item(next), ..script_items(next + 1, remaining - 1)]
  }
}

// ---------------------------------------------------------------------------
// §5.5 output items and §5.6 mint items, hand-built so a fixture can be
// perturbed in exactly one place (§11.1's suite uses the same construction).
// ---------------------------------------------------------------------------

fn uint_bytes(value: Int) -> ByteArray {
  expect value >= 0
  if value <= 23 {
    bytearray.from_int_big_endian(value, 1)
  } else if value <= 255 {
    bytearray.concat(#"18", bytearray.from_int_big_endian(value, 1))
  } else if value <= 65535 {
    bytearray.concat(#"19", bytearray.from_int_big_endian(value, 2))
  } else {
    expect value <= 4294967295
    bytearray.concat(#"1a", bytearray.from_int_big_endian(value, 4))
  }
}

/// A canonical minimal CBOR `int`, positive or negative. §5.6 admits both: a
/// burn is a negative mint quantity.
fn int_bytes(value: Int) -> ByteArray {
  if value >= 0 {
    uint_bytes(value)
  } else {
    let magnitude = -1 - value
    expect magnitude >= 0
    if magnitude <= 23 {
      bytearray.from_int_big_endian(32 + magnitude, 1)
    } else {
      expect magnitude <= 255
      bytearray.concat(#"38", bytearray.from_int_big_endian(magnitude, 1))
    }
  }
}

fn bytes_item(payload: ByteArray) -> ByteArray {
  let length = bytearray.length(payload)
  expect length <= 255
  if length <= 23 {
    bytearray.concat(bytearray.from_int_big_endian(64 + length, 1), payload)
  } else {
    #"58"
      |> bytearray.concat(bytearray.from_int_big_endian(length, 1))
      |> bytearray.concat(payload)
  }
}

fn map_header(entries: Int) -> ByteArray {
  expect entries >= 0
  expect entries <= 23
  bytearray.from_int_big_endian(160 + entries, 1)
}

/// Policy ids already in §5.5/§5.6 canonical order for `index in 0..7`: same
/// length, so the comparison falls through to the byte compare.
fn policy_id(index: Int) -> ByteArray {
  bytearray.concat(
    bytearray.from_int_big_endian(index, 1),
    #"b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0",
  )
}

fn asset_name(index: Int) -> ByteArray {
  bytearray.concat(#"c0c0c0", bytearray.from_int_big_endian(index, 1))
}

fn asset_entries(entries: List<Pair<ByteArray, Int>>) -> ByteArray {
  list.foldr(
    entries,
    #"",
    fn(entry, tail) {
      let Pair(name, quantity) = entry
      bytes_item(name)
        |> bytearray.concat(int_bytes(quantity))
        |> bytearray.concat(tail)
    },
  )
}

fn output_item(
  lovelace: Int,
  groups: List<Pair<ByteArray, List<Pair<ByteArray, Int>>>>,
) -> ByteArray {
  #"a2"
    |> bytearray.concat(#"00")
    |> bytearray.concat(bytes_item(address_payload))
    |> bytearray.concat(#"01")
    |> bytearray.concat(#"82")
    |> bytearray.concat(uint_bytes(lovelace))
    |> bytearray.concat(map_header(list.length(groups)))
    |> bytearray.concat(
        list.foldr(
          groups,
          #"",
          fn(group, tail) {
            let Pair(policy, entries) = group
            bytes_item(policy)
              |> bytearray.concat(map_header(list.length(entries)))
              |> bytearray.concat(asset_entries(entries))
              |> bytearray.concat(tail)
          },
        ),
      )
}

/// §5.6 — `82 ‖ 58 1C policy_id ‖ map(k) ‖ asset entries`.
fn mint_item(
  policy: ByteArray,
  entries: List<Pair<ByteArray, Int>>,
) -> ByteArray {
  #"82"
    |> bytearray.concat(bytes_item(policy))
    |> bytearray.concat(map_header(list.length(entries)))
    |> bytearray.concat(asset_entries(entries))
}

fn group(
  policy_index: Int,
  entries: List<Pair<ByteArray, Int>>,
) -> Pair<ByteArray, List<Pair<ByteArray, Int>>> {
  Pair(policy_id(policy_index), entries)
}

/// The unit every conservation vector below is about: policy 3, asset 1. Every
/// other unit in the fixtures is present in a different quantity, so a sweep
/// that answered about the wrong one could not report the right number.
const target_policy: ByteArray =
  #"03b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0"

const target_name: ByteArray = #"c0c0c001"

/// Three outputs. The target unit appears in outputs 0 and 2 at 7 and 11, and
/// not at all in output 1 — so the total is 18 and an accumulator that stopped
/// at the first miss, or that read a neighbouring unit, gets a different number.
fn outputs_preimage() -> ByteArray {
  encode_field_preimage(
    [
      output_item(
        5_000_000,
        [
          group(1, [Pair(asset_name(0), 100)]),
          group(3, [Pair(asset_name(0), 5), Pair(asset_name(1), 7)]),
          group(5, [Pair(asset_name(2), 9)]),
        ],
      ),
      output_item(2_000_000, [group(0, [Pair(asset_name(1), 55)])]),
      output_item(
        3_000_000,
        [
          group(3, [Pair(asset_name(1), 11), Pair(asset_name(4), 13)]),
          group(7, [Pair(asset_name(0), 17)]),
        ],
      ),
    ],
  )
}

/// Four mint policies in canonical order; the target unit is minted 4, and the
/// units either side of it carry different quantities.
fn mint_preimage() -> ByteArray {
  encode_field_preimage(
    [
      mint_item(policy_id(0), [Pair(asset_name(0), 21)]),
      mint_item(
        policy_id(3),
        [Pair(asset_name(0), 2), Pair(asset_name(1), 4), Pair(asset_name(6), 6)],
      ),
      mint_item(policy_id(4), [Pair(asset_name(1), -8)]),
      mint_item(policy_id(6), [Pair(asset_name(3), 30)]),
    ],
  )
}

/// The same four policies with two of them transposed: §5.6 requires strictly
/// increasing keys, and without that the sweep could pass the target and then
/// meet it.
fn unordered_mint_preimage() -> ByteArray {
  encode_field_preimage(
    [
      mint_item(policy_id(0), [Pair(asset_name(0), 21)]),
      mint_item(policy_id(4), [Pair(asset_name(1), -8)]),
      mint_item(
        policy_id(3),
        [Pair(asset_name(0), 2), Pair(asset_name(1), 4), Pair(asset_name(6), 6)],
      ),
      mint_item(policy_id(6), [Pair(asset_name(3), 30)]),
    ],
  )
}

/// §5.6 gives an absent asset exactly one spelling. A zero entry would be a
/// second one — and it is the one a conservation fault would hide behind.
fn zero_quantity_mint_preimage() -> ByteArray {
  encode_field_preimage([mint_item(policy_id(3), [Pair(asset_name(1), 0)])])
}

// ---------------------------------------------------------------------------
// Statement fixtures
// ---------------------------------------------------------------------------

fn item_statement_against(
  tx_id: ByteArray,
  field_index: Int,
  item_index: Int,
) -> FaultStatementV1 {
  FaultStatementV1 {
    tx_id,
    code: fault_item_predicate,
    field_index,
    item_index,
    policy_id: #"",
    asset_name: #"",
    claimed: 0,
  }
}

fn item_statement(field_index: Int, item_index: Int) -> FaultStatementV1 {
  item_statement_against(sample_tx_id, field_index, item_index)
}

fn conservation_statement(claimed: Int) -> FaultStatementV1 {
  FaultStatementV1 {
    tx_id: sample_tx_id,
    code: fault_asset_conservation,
    field_index: 2,
    item_index: 0,
    policy_id: target_policy,
    asset_name: target_name,
    claimed,
  }
}

/// A finished outputs-side measurement over `outputs_preimage`, taken against a
/// named transaction. Budget 3 is the field's whole item count, so the fold
/// reaches the end of the walk and the sweep is final.
fn finished_outputs_sweep(
  tx_id: ByteArray,
  policy: ByteArray,
  name: ByteArray,
) -> OutputUnitSweepV1 {
  let Pair(view, start) = open_inline_for(tx_id, 2, outputs_preimage())
  accumulate_output_unit(view, open_output_unit_sweep(start, policy, name), 3)
}

/// The mint-side twin. Budget 4 is `mint_preimage`'s policy count, so every
/// request below either finds its group or meets a key that sorts after it.
fn finished_mint_sweep(
  tx_id: ByteArray,
  policy: ByteArray,
  name: ByteArray,
) -> MintUnitSweepV1 {
  let Pair(view, start) = open_inline_for(tx_id, 5, mint_preimage())
  sweep_mint_unit(view, open_mint_unit_sweep(start, policy, name), 4)
}

/// The per-item predicate the item-fault vectors adjudicate: a field-6 item is
/// honest when it is one of the two shapes the fixtures build, and item 4 of
/// `fraudulent_scripts` is deliberately neither. A predicate is a caller's, not
/// this module's — §12.3 adjudicates one, it does not define the protocol's.
fn script_item_is_honest(index: Int, item: ByteArray) -> Bool {
  item == script_item(index)
}

/// The twin predicate, for the twin transaction: honest there is what
/// `other_script_item` builds.
fn other_script_item_is_honest(index: Int, item: ByteArray) -> Bool {
  item == other_script_item(index)
}

/// Twenty script items with item 4 replaced by the twin's byte form, so exactly
/// one item fails `script_item_is_honest` and every other index is honest at
/// the same widths.
fn fraudulent_scripts() -> ByteArray {
  encode_field_preimage(
    list.map(
      list.range(0, 19),
      fn(index) {
        if index == 4 {
          other_script_item(index)
        } else {
          script_item(index)
        }
      },
    ),
  )
}

/// `fraudulent_scripts`'s content-disjoint twin: the same twenty widths, every
/// item built by the other producer, and the same index 4 carrying the shape its
/// own predicate refuses. No item of this preimage equals the item at its index
/// in `fraudulent_scripts`, which is what makes one statement serving both a
/// claim about the witness rather than about the fixtures.
fn fraudulent_other_scripts() -> ByteArray {
  encode_field_preimage(
    list.map(
      list.range(0, 19),
      fn(index) {
        if index == 4 {
          script_item(index)
        } else {
          other_script_item(index)
        }
      },
    ),
  )
}

/// True when the two fixtures share no item at any index — asserted rather than
/// assumed, because the disjointness is what the AC1 vector rests on.
fn every_item_differs(next: Int, remaining: Int) -> Bool {
  if remaining <= 0 {
    True
  } else {
    let mine = if next == 4 {
      other_script_item(next)
    } else {
      script_item(next)
    }
    let theirs = if next == 4 {
      script_item(next)
    } else {
      other_script_item(next)
    }
    and {
      mine != theirs,
      bytearray.length(mine) == bytearray.length(theirs),
      every_item_differs(next + 1, remaining - 1),
    }
  }
}

/// §12.2's domain string as raw ASCII — `MidgardNativeTxFaultStatementV1`,
/// spelled here from the specification rather than imported, so the commitment
/// formula is cross-checked against §12.2 instead of against itself.
const statement_domain: ByteArray =
  #"4d6964676172644e617469766554784661756c7453746174656d656e745631"

// ---------------------------------------------------------------------------
// §12.1 / §12.2 — the witness is minimal
// ---------------------------------------------------------------------------

/// AC1, structural half. An item-fault statement is the same width whatever the
/// field holds and however many items it has: the accusation names an address,
/// never content.
test item_fault_statement_is_constant_width() {
  let small = encode_fault_statement(item_statement(6, 0))
  let large = encode_fault_statement(item_statement(6, 65535))
  let other_field = encode_fault_statement(item_statement(0, 295))
  and {
    bytearray.length(small) == item_fault_statement_bytes,
    bytearray.length(large) == item_fault_statement_bytes,
    bytearray.length(other_field) == item_fault_statement_bytes,
  }
}

/// AC1, the decisive half. Two wire statements adjudicate the same logical fault
/// in two transactions whose field-6 preimages share no item at any index, and
/// each proves against its own. A witness carrying the fraudulent item could not
/// do that, because the item it carried would be wrong for one of the two; so
/// this is the property rather than an argument for it.
///
/// The two wires differ in **exactly** the 32 tx-id bytes and nothing else —
/// asserted per byte below — so what the vector shows is that the witness scales
/// with the *address* of the fault and not with the transaction's content: the
/// two preimages are 20 items of wholly different bytes, and the accusation about
/// them differs by the one field that names which transaction is accused.
///
/// A single wire proving against both would be the *stronger*-looking claim and
/// the wrong one: two disjoint field-6 preimages have different field
/// commitments and therefore belong to different transactions, so one statement
/// answering for both would only show that the statement is bound to no
/// transaction at all. `item_fault_refuses_a_statement_about_another_transaction`
/// is the vector for that.
///
/// The disjointness is not assumed: `every_item_differs` walks both fixtures and
/// requires each index to hold different bytes at the same width, so the two
/// preimages agree on exactly what §5.1/§5.3 forces and on nothing else.
///
/// Both adjudications are driven from `decode_fault_statement(wire)` rather than
/// from a statement value in hand, so the bytes really are the whole of what
/// crosses between the challenger and each adjudication.
test fault_statement_carries_no_preimage_bytes() {
  let honest = fraudulent_scripts()
  let disjoint = fraudulent_other_scripts()
  let wire_a = encode_fault_statement(
    item_statement_against(item_tx_id_a, 6, 4),
  )
  let wire_b = encode_fault_statement(
    item_statement_against(item_tx_id_b, 6, 4),
  )
  let Pair(view_a, start_a) = open_inline_for(item_tx_id_a, 6, honest)
  let Pair(proven_a, _a) = prove_item_fault(
    view_a,
    start_a,
    decode_fault_statement(wire_a),
    script_item_is_honest,
  )
  let Pair(view_b, start_b) = open_inline_for(item_tx_id_b, 6, disjoint)
  let Pair(proven_b, _b) = prove_item_fault(
    view_b,
    start_b,
    decode_fault_statement(wire_b),
    other_script_item_is_honest,
  )
  and {
    honest != disjoint,
    bytearray.length(honest) == bytearray.length(disjoint),
    every_item_differs(0, 20),
    bytearray.length(wire_a) == item_fault_statement_bytes,
    bytearray.length(wire_b) == item_fault_statement_bytes,
    // The tx id occupies offsets 3..35 of the §12.2 frame; everything before and
    // after it is byte-identical, so the two accusations differ only in whom
    // they accuse.
    bytearray.take(wire_a, 3) == bytearray.take(wire_b, 3),
    bytearray.drop(wire_a, 35) == bytearray.drop(wire_b, 35),
    bytearray.take(bytearray.drop(wire_a, 3), 32) == item_tx_id_a,
    bytearray.take(bytearray.drop(wire_b, 3), 32) == item_tx_id_b,
    proven_a,
    proven_b,
  }
}

/// The binding the vector above rests on. The same accusation, aimed at a
/// transaction that is not the one the door authenticated, does not prove — it
/// is refused outright, because a statement about another transaction is a
/// malformed input to this adjudication rather than a false claim about this
/// one.
///
/// Aimed at `fraudulent_scripts` item 4, which really *is* fraudulent under
/// `script_item_is_honest`: everything except the tx id lines up, so the tx-id
/// check is the only thing this vector can be going green off.
test item_fault_refuses_a_statement_about_another_transaction() fail {
  let Pair(view, start) = open_inline_for(item_tx_id_a, 6, fraudulent_scripts())
  let Pair(proven, _next) = prove_item_fault(
    view,
    start,
    item_statement_against(item_tx_id_b, 6, 4),
    script_item_is_honest,
  )
  proven
}

/// §12.2's thread-carried commitment, cross-checked against the section's own
/// formula rather than against another call to the same function: the hash is
/// `blake2b_256` over the ASCII domain concatenated with the wire form, and
/// nothing about the accused transaction enters it.
test fault_statement_hash_is_the_domain_over_the_wire_form() {
  let item = item_statement(6, 4)
  let asset = conservation_statement(-42)
  and {
    fault_statement_hash(item) == blake2b_256(
      bytearray.concat(statement_domain, encode_fault_statement(item)),
    ),
    fault_statement_hash(asset) == blake2b_256(
      bytearray.concat(statement_domain, encode_fault_statement(asset)),
    ),
    bytearray.length(fault_statement_hash(item)) == 32,
    fault_statement_hash(item) != fault_statement_hash(asset),
  }
}

/// The §12.2 wire form has one admissible spelling, and the round trip is what
/// establishes it.
test fault_statement_round_trips_through_its_wire_form() {
  let item = item_statement(8, 12)
  let asset = conservation_statement(-42)
  and {
    decode_fault_statement(encode_fault_statement(item)) == item,
    decode_fault_statement(encode_fault_statement(asset)) == asset,
    // The unit is the only thing that moves the width, and it moves it by
    // exactly its own two encodings: `58 1C` + 28, and `44` + 4.
    bytearray.length(encode_fault_statement(asset)) == fault_statement_frame_bytes + 30 + 5,
  }
}

/// A statement that names a unit under a code that has no unit is two
/// accusations in one witness, and §12.1 refuses it rather than picking one.
test encode_refuses_an_item_fault_that_names_a_unit() fail {
  encode_fault_statement(
    FaultStatementV1 { ..item_statement(6, 1), policy_id: target_policy },
  ) != #""
}

/// The same, for the quantity half.
test encode_refuses_an_item_fault_that_claims_a_quantity() fail {
  encode_fault_statement(
    FaultStatementV1 { ..item_statement(6, 1), claimed: 5 },
  ) != #""
}

/// A conservation statement must name a full 28-byte policy id: a short one
/// would name a *set* of policies, and the lookup it drives would name none.
test encode_refuses_a_short_policy_id() fail {
  encode_fault_statement(
    FaultStatementV1 {
      ..conservation_statement(0),
      policy_id: bytearray.take(target_policy, 27),
    },
  ) != #""
}

/// The conservation code's two positional scalars have one spelling each, and
/// the encoder is where a challenger meets that rule first. A statement that
/// named field 5 would be a perfectly plausible accusation about the mint side —
/// which is exactly the problem: §12.4 reads both fields, so "which field" is not
/// a question the accusation gets to answer, and a scalar the adjudication never
/// reads would move the statement's commitment without moving its verdict.
test encode_refuses_a_conservation_statement_that_names_another_field() fail {
  encode_fault_statement(
    FaultStatementV1 { ..conservation_statement(7), field_index: 5 },
  ) != #""
}

/// The item half of the same rule.
test encode_refuses_a_conservation_statement_that_names_a_starting_item() fail {
  encode_fault_statement(
    FaultStatementV1 { ..conservation_statement(7), item_index: 4 },
  ) != #""
}

/// §6.1 — one admissible spelling. The sign byte is the narrowest place to show
/// it: `00` and `01` are the only two values, and a third decodes to nothing.
/// Its offset is 79 — `87`, `58 20` + 32, `41` + 1, `41` + 1, `43` + 3,
/// `58 1C` + 28, `44` + 4, `49` — and the round-trip test above is what stops
/// that arithmetic from drifting away from the encoder.
///
/// The body asserts the **equality**, not its negation: a `fail` test whose body
/// is `decoded != original` stays green when the guard is removed, because the
/// neutralised decoder returns exactly the original. Written this way the vector
/// turns red the moment the refusal stops happening, which is the only form in
/// which it is evidence.
test decode_refuses_a_non_canonical_spelling() fail {
  let bytes = encode_fault_statement(conservation_statement(7))
  let patched = bytearray.concat(
    bytearray.take(bytes, 79),
    bytearray.concat(#"02", bytearray.drop(bytes, 80)),
  )
  decode_fault_statement(patched) == conservation_statement(7)
}

/// The vector that isolates the re-encode itself. The decoder reads `code` at
/// offset 36, `field_index` at 38 and `item_index` at 40 and never looks at the
/// `87` head or the `41`/`43` wrappers that separate them — so a corrupted array
/// head decodes to a perfectly well-formed statement, and the only thing that
/// notices is "re-encode what you read and demand the input back".
test decode_refuses_an_unvalidated_wrapper_byte() fail {
  let bytes = encode_fault_statement(conservation_statement(7))
  let patched = bytearray.concat(#"88", bytearray.drop(bytes, 1))
  decode_fault_statement(patched) == conservation_statement(7)
}

// ---------------------------------------------------------------------------
// §12.3 — proving a single bad item
// ---------------------------------------------------------------------------

/// AC1, adjudication half. The statement addresses item 4; the returned
/// checkpoint sits at item 5, so exactly one item was consumed — the relocation
/// touched no bytes and nothing beyond the accused item was read.
test item_fault_reads_exactly_the_named_item() {
  let Pair(view, start) = open_inline(6, fraudulent_scripts())
  let Pair(proven, next) = prove_item_fault(
    view,
    start,
    item_statement(6, 4),
    script_item_is_honest,
  )
  and {
    proven,
    walk_next_item_index(next) == 5,
    walk_next_item_index(start) == 0,
  }
}

/// The same field, the same predicate, an honest index: the accusation is
/// answered rather than believed.
test item_fault_does_not_prove_an_honest_item() {
  let Pair(view, start) = open_inline(6, fraudulent_scripts())
  let Pair(proven, next) = prove_item_fault(
    view,
    start,
    item_statement(6, 3),
    script_item_is_honest,
  )
  and {
    !proven,
    walk_next_item_index(next) == 4,
  }
}

/// An adjudication that has already passed the accused item cannot go back for
/// it. §10.4's relocation is forward-only, and that is what stops a statement
/// from being re-adjudicated at a position the walk has left.
test item_fault_refuses_a_backwards_statement() fail {
  let Pair(view, start) = open_inline(6, fraudulent_scripts())
  let Pair(_first, after) = prove_item_fault(
    view,
    start,
    item_statement(6, 6),
    script_item_is_honest,
  )
  let Pair(proven, _next) = prove_item_fault(
    view,
    after,
    item_statement(6, 4),
    script_item_is_honest,
  )
  proven
}

/// §4's plain hashing removed field-index domain separation, so a view of one
/// field answers every read a view of another would. The statement's field and
/// the walk's must therefore agree, and this is the line that makes them.
test item_fault_refuses_a_statement_about_another_field() fail {
  let Pair(view, start) = open_inline(6, fraudulent_scripts())
  let Pair(proven, _next) = prove_item_fault(
    view,
    start,
    item_statement(8, 4),
    script_item_is_honest,
  )
  proven
}

/// A `FaultStatementV1` is a plain type, so a caller can write one without
/// going through the encoder — which is exactly what a challenger does. §12.3
/// therefore re-runs the shape rule at adjudication rather than assuming the
/// statement arrived through §12.2's wire form.
test item_fault_refuses_a_malformed_statement() fail {
  let Pair(view, start) = open_inline(6, fraudulent_scripts())
  let Pair(proven, _next) = prove_item_fault(
    view,
    start,
    FaultStatementV1 {
      ..item_statement(6, 4),
      tx_id: bytearray.take(sample_tx_id, 31),
    },
    script_item_is_honest,
  )
  proven
}

/// A conservation statement is not an item statement, and §12.3 will not
/// adjudicate one as the other.
///
/// The statement is aimed at field 6 item 4 — the fraudulent one — rather than
/// left at the conservation fixture's field 2 item 0, so that the code check is
/// the only thing standing between this vector and a `proven` verdict. Aimed
/// anywhere else the vector would go green off the field guard or off an honest
/// item, and would say nothing about the code.
test item_fault_refuses_a_conservation_statement() fail {
  let Pair(view, start) = open_inline(6, fraudulent_scripts())
  let Pair(proven, _next) = prove_item_fault(
    view,
    start,
    FaultStatementV1 {
      ..conservation_statement(0),
      field_index: 6,
      item_index: 4,
    },
    script_item_is_honest,
  )
  proven
}

// ---------------------------------------------------------------------------
// §12.4 — per-asset conservation isolates one asset id
// ---------------------------------------------------------------------------

/// AC2. Three outputs holding eight units between them, one unit asked for: the
/// answer is 7 + 0 + 11. Every other unit in the fixture carries a different
/// quantity, so an accumulator that read a neighbour — the same policy's asset
/// 0, or the same asset under policy 1 — cannot produce 18.
test conservation_reads_one_asset_out_of_many() {
  let Pair(view, start) = open_inline(2, outputs_preimage())
  let swept = accumulate_output_unit(
    view,
    open_output_unit_sweep(start, target_policy, target_name),
    3,
  )
  and {
    output_sweep_is_final(swept),
    output_sweep_quantity(swept) == 18,
    walk_next_item_index(output_sweep_checkpoint(swept)) == 3,
  }
}

/// A unit no output holds answers `0`, and the `0` is evidence: §11.1's
/// structural pass ran on every output, so the sweep did not merely fail to
/// notice it.
test conservation_reports_absence_as_zero() {
  let Pair(view, start) = open_inline(2, outputs_preimage())
  let swept = accumulate_output_unit(
    view,
    open_output_unit_sweep(start, target_policy, asset_name(9)),
    3,
  )
  and {
    output_sweep_is_final(swept),
    output_sweep_quantity(swept) == 0,
  }
}

/// The fold is budgeted like every other §10 fold, so a caller may take the
/// field in as many rounds as it wants — **inside one invocation**, which is
/// §12.4's contract. Two rounds of one and two outputs reach the same total as
/// one round of three, and the intermediate position is a real one.
///
/// The intermediate sweep is **not** final and its running 7 is unreadable
/// through `output_sweep_quantity`; only the sweep that reached the end of the
/// field answers. That is the outputs-side half of "a prefix of a sum is not the
/// sum".
test conservation_accumulates_across_a_budget_boundary() {
  let Pair(view, start) = open_inline(2, outputs_preimage())
  let middle = accumulate_output_unit(
    view,
    open_output_unit_sweep(start, target_policy, target_name),
    1,
  )
  let finished = accumulate_output_unit(view, middle, 2)
  and {
    !output_sweep_is_final(middle),
    output_sweep_is_final(finished),
    output_sweep_quantity(finished) == 18,
    walk_next_item_index(output_sweep_checkpoint(middle)) == 1,
    walk_next_item_index(output_sweep_checkpoint(finished)) == 3,
  }
}

/// The other half: an unfinished outputs fold will not hand back its running
/// total, so the 7 measured over output 0 alone cannot be mistaken for the 18
/// the field actually holds.
test conservation_refuses_the_running_total_of_an_unfinished_fold() fail {
  let Pair(view, start) = open_inline(2, outputs_preimage())
  let middle = accumulate_output_unit(
    view,
    open_output_unit_sweep(start, target_policy, target_name),
    1,
  )
  output_sweep_quantity(middle) == 7
}

/// The outputs accumulator is guarded on the field, so a walk over some other
/// field cannot be read as outputs by accident.
test conservation_refuses_a_walk_over_another_field() fail {
  let Pair(view, start) = open_inline(6, fraudulent_scripts())
  let swept = accumulate_output_unit(
    view,
    open_output_unit_sweep(start, target_policy, target_name),
    1,
  )
  output_sweep_quantity(swept) == 0
}

/// The blocker on the outputs side: a fold that was opened somewhere other than
/// item 0 has measured a **suffix**, and a suffix of a sum is not the sum.
///
/// Forward relocation is free and public (§10.4's `walk_skip`), so this is a
/// position a challenger can stand at without writing a checkpoint. Opened at
/// output 1 the fold misses output 0's 7, reaches the end of the field, and comes
/// back final with 11 — a number that is not the transaction's, against a
/// statement that would then prove a conservation fault the operator did not
/// commit. The opener's "item 0 and nothing else" is the only line between the
/// two.
test conservation_refuses_a_fold_opened_past_the_start() fail {
  let Pair(view, start) = open_inline(2, outputs_preimage())
  let past_the_first_output = walk_skip(view, start, 1)
  let swept = accumulate_output_unit(
    view,
    open_output_unit_sweep(past_the_first_output, target_policy, target_name),
    3,
  )
  and {
    output_sweep_is_final(swept),
    output_sweep_quantity(swept) == 11,
  }
}

/// AC2, mint half. Four policies, the target's group holding three assets: the
/// answer is that one asset's 4, and every other entry in the fixture is a
/// different number.
test mint_unit_quantity_reads_one_asset_out_of_many() {
  let Pair(view, start) = open_inline(5, mint_preimage())
  let Pair(quantity, _next) = mint_unit_quantity(
    view,
    start,
    target_policy,
    target_name,
    4,
  )
  quantity == 4
}

/// §5.6 admits burns, and a burn is a negative quantity rather than an absent
/// one — which is exactly the distinction a conservation fault turns on.
test mint_unit_quantity_reads_a_burn() {
  let Pair(view, start) = open_inline(5, mint_preimage())
  let Pair(quantity, _next) = mint_unit_quantity(
    view,
    start,
    policy_id(4),
    asset_name(1),
    4,
  )
  quantity == -8
}

/// A policy the mint field does not carry, and a name the target's group does
/// not carry: both answer `0`, and the sweep stops at the first key that sorts
/// after the request rather than reading to the end.
test mint_unit_quantity_reports_absence_as_zero() {
  let Pair(view, start) = open_inline(5, mint_preimage())
  let Pair(missing_policy, _a) = mint_unit_quantity(
    view,
    start,
    policy_id(2),
    asset_name(0),
    4,
  )
  let Pair(view_b, start_b) = open_inline(5, mint_preimage())
  let Pair(missing_name, _b) = mint_unit_quantity(
    view_b,
    start_b,
    target_policy,
    asset_name(9),
    4,
  )
  and {
    missing_policy == 0,
    missing_name == 0,
  }
}

/// Half one of what makes a mint `0` evidence: the field's own key order. Under
/// an unordered field the sweep could pass the target and then meet it, so the
/// `0` it reported on the way would have been false.
test mint_unit_quantity_refuses_an_unordered_field() fail {
  let Pair(view, start) = open_inline(5, unordered_mint_preimage())
  let Pair(quantity, _next) = mint_unit_quantity(
    view,
    start,
    policy_id(6),
    asset_name(3),
    4,
  )
  quantity == 30
}

/// Half two: §5.6 gives an absent asset one spelling, and a zero-quantity entry
/// would be a second one — the one a conservation fault would hide behind.
test mint_unit_quantity_refuses_a_zero_quantity() fail {
  let Pair(view, start) = open_inline(5, zero_quantity_mint_preimage())
  let Pair(quantity, _next) = mint_unit_quantity(
    view,
    start,
    target_policy,
    target_name,
    4,
  )
  quantity == 0
}

/// The mint sweep is budgeted and continuable like the outputs fold. Two rounds
/// of one policy item reach the same 4 as one round of four, and the round that
/// ran out of budget says so rather than answering.
///
/// The intermediate sweep has read policy 0 and not yet reached policy 3, so its
/// internal quantity is `0` — the exact state that must not be readable as
/// "the transaction does not mint this unit".
test mint_unit_quantity_resumes_across_a_budget_boundary() {
  let Pair(view, start) = open_inline(5, mint_preimage())
  let middle = sweep_mint_unit(
    view,
    open_mint_unit_sweep(start, target_policy, target_name),
    1,
  )
  let finished = sweep_mint_unit(view, middle, 1)
  and {
    !mint_sweep_is_final(middle),
    walk_next_item_index(mint_sweep_checkpoint(middle)) == 1,
    mint_sweep_is_final(finished),
    mint_sweep_quantity(finished) == 4,
    walk_next_item_index(mint_sweep_checkpoint(finished)) == 2,
  }
}

/// The blocker on the mint side, and the sharper of the two: a sweep opened past
/// the target's own policy group fabricates an absence.
///
/// `mint_preimage` holds policies 0, 3, 4 and 6, and the target is 3. Opened at
/// item 2 — one `walk_skip` past the group that carries it — the sweep's first
/// key is policy 4, which sorts after the request, so the monotone early stop
/// fires, `is_final` is true, and the quantity is `0` for a unit the transaction
/// mints 4 of. Every check in the adjudication accepts that `0`: it was measured
/// over the accused transaction, over field 5, about the statement's unit, and to
/// a verdict. Only "a sweep begins at item 0" tells it apart from real absence.
test mint_unit_quantity_refuses_a_sweep_opened_past_the_target() fail {
  let Pair(view, start) = open_inline(5, mint_preimage())
  let past_the_target = walk_skip(view, start, 2)
  let swept = sweep_mint_unit(
    view,
    open_mint_unit_sweep(past_the_target, target_policy, target_name),
    4,
  )
  and {
    mint_sweep_is_final(swept),
    mint_sweep_quantity(swept) == 0,
  }
}

/// The blocker this seam exists to close: a sweep stopped by its budget is not
/// absence, and will not be read as `0`.
///
/// One policy item of budget over `mint_preimage` gets past policy 0 and no
/// further; the target's group is the *next* item. Answering `0` here would be
/// answering about a unit the sweep never reached.
test mint_unit_quantity_refuses_a_budget_exhausted_sweep() fail {
  let Pair(view, start) = open_inline(5, mint_preimage())
  let Pair(quantity, _next) = mint_unit_quantity(
    view,
    start,
    target_policy,
    target_name,
    1,
  )
  quantity == 0
}

/// §5.6's key order is enforced **across budget rounds**, which is the one place
/// a `previous_policy` scoped to a single call could not reach.
///
/// Named for what the vector does rather than for what an earlier draft hoped it
/// showed: §12.4's contract is a single invocation (a sweep has no wire form and
/// cannot cross a step), so the boundary this spans is the boundary between two
/// `sweep_mint_unit` calls, not between two transactions.
///
/// `unordered_mint_preimage` holds policies 0, 4, 3, 6. The first round reads
/// items 0 and 1 and stops at policy 4; the second round's first item is policy
/// 3, which sorts before it. The violation is therefore visible only to a sweep
/// that carried its last key over the boundary — a sweep that restarted its
/// order check at every call would accept this field and could then report an
/// absence it had already walked past.
test mint_unit_quantity_refuses_an_unordered_field_across_budget_rounds() fail {
  let Pair(view, start) = open_inline(5, unordered_mint_preimage())
  let middle = sweep_mint_unit(
    view,
    open_mint_unit_sweep(start, policy_id(6), asset_name(3)),
    2,
  )
  let finished = sweep_mint_unit(view, middle, 2)
  mint_sweep_quantity(finished) == 30
}

/// The mint accumulator is guarded on the field for the outputs accumulator's
/// reason.
test mint_unit_quantity_refuses_a_walk_over_another_field() fail {
  let Pair(view, start) = open_inline(2, outputs_preimage())
  let Pair(quantity, _next) = mint_unit_quantity(
    view,
    start,
    target_policy,
    target_name,
    3,
  )
  quantity == 0
}

/// AC2, end to end. One unit, both fields, one arithmetic statement: the
/// outputs hold 18 of it and the mint made 4, so the transaction's own fields
/// account for a net 14. An operator committing 14 is not faulted; one
/// committing anything else is — and the two legs differ in nothing but the
/// claimed number.
test asset_conservation_fault_is_exactly_the_disagreement() {
  let outputs = finished_outputs_sweep(sample_tx_id, target_policy, target_name)
  let mint = finished_mint_sweep(sample_tx_id, target_policy, target_name)
  and {
    output_sweep_quantity(outputs) == 18,
    mint_sweep_quantity(mint) == 4,
    !asset_conservation_fault_is_proven(
      conservation_statement(14),
      outputs,
      mint,
    ),
    asset_conservation_fault_is_proven(
      conservation_statement(13),
      outputs,
      mint,
    ),
    asset_conservation_fault_is_proven(
      conservation_statement(18),
      outputs,
      mint,
    ),
  }
}

/// The two totals are evidence, not arithmetic a caller did elsewhere: an
/// adjudication is refused unless both sweeps were taken over the transaction
/// the statement accuses.
///
/// Both sweeps here measure the honest 18 and 4, so the equation itself would
/// answer "not faulted" — the vector can only be going green off the tx-id
/// binding.
test conservation_refuses_sweeps_of_another_transaction() fail {
  let outputs = finished_outputs_sweep(item_tx_id_a, target_policy, target_name)
  let mint = finished_mint_sweep(item_tx_id_a, target_policy, target_name)
  // 13 against a real 14, so the arithmetic alone would say "proven".
  asset_conservation_fault_is_proven(conservation_statement(13), outputs, mint)
}

/// The unit binding, on the mint side. A sweep about policy 4's asset 1 is a
/// perfectly good measurement — it answers −8 — but it is not a measurement of
/// the unit this statement names, and pairing it with the target's outputs total
/// would fabricate a net 26 against a committed 14.
test conservation_refuses_a_sweep_about_another_unit() fail {
  let outputs = finished_outputs_sweep(sample_tx_id, target_policy, target_name)
  let mint = finished_mint_sweep(sample_tx_id, policy_id(4), asset_name(1))
  asset_conservation_fault_is_proven(conservation_statement(14), outputs, mint)
}

/// The blocker, at the adjudication rather than at the sweep. A mint sweep given
/// one policy item of budget has not reached the target's group; read as `0` it
/// makes the transaction's own fields say a net 18 where they say 14, and a
/// fault that does not exist is "proven" against an honest operator.
///
/// The claimed quantity is the honest 14, so a finished pair of sweeps returns
/// `False` here. Only the fabrication returns `True` — which is what makes this
/// vector evidence about the `is_final` guard specifically.
test conservation_refuses_a_budget_exhausted_mint_sweep() fail {
  let outputs = finished_outputs_sweep(sample_tx_id, target_policy, target_name)
  let Pair(view, start) = open_inline(5, mint_preimage())
  let unfinished = sweep_mint_unit(
    view,
    open_mint_unit_sweep(start, target_policy, target_name),
    1,
  )
  asset_conservation_fault_is_proven(
    conservation_statement(14),
    outputs,
    unfinished,
  )
}

/// The same, on the outputs side: a fold that read one of three outputs has
/// measured 7 where the field holds 18.
test conservation_refuses_a_budget_exhausted_outputs_fold() fail {
  let Pair(view, start) = open_inline(2, outputs_preimage())
  let unfinished = accumulate_output_unit(
    view,
    open_output_unit_sweep(start, target_policy, target_name),
    1,
  )
  let mint = finished_mint_sweep(sample_tx_id, target_policy, target_name)
  asset_conservation_fault_is_proven(
    conservation_statement(14),
    unfinished,
    mint,
  )
}

/// A conservation statement's `field_index` is adjudicated, not merely carried.
///
/// §12.4 reads fields 2 and 5 from item 0 of each and consults this scalar for
/// nothing, so it gets exactly one spelling (§12.1). Without that rule a byte
/// nobody reads still enters `fault_statement_hash`, and two statements
/// differing only here would be two distinct thread commitments reaching the
/// same verdict off the same evidence. The claimed 13 against a real 14 means
/// the arithmetic alone would say "proven", so the vector can only be going
/// green off the shape rule.
test conservation_refuses_a_statement_that_names_another_field() fail {
  let outputs = finished_outputs_sweep(sample_tx_id, target_policy, target_name)
  let mint = finished_mint_sweep(sample_tx_id, target_policy, target_name)
  asset_conservation_fault_is_proven(
    FaultStatementV1 { ..conservation_statement(13), field_index: 5 },
    outputs,
    mint,
  )
}

/// The same for `item_index`: a conservation sweep begins at item 0 by
/// construction, so a statement naming any other start names nothing.
test conservation_refuses_a_statement_that_names_a_starting_item() fail {
  let outputs = finished_outputs_sweep(sample_tx_id, target_policy, target_name)
  let mint = finished_mint_sweep(sample_tx_id, target_policy, target_name)
  asset_conservation_fault_is_proven(
    FaultStatementV1 { ..conservation_statement(13), item_index: 1 },
    outputs,
    mint,
  )
}

/// An item statement is not a conservation statement, and §12.4 will not
/// adjudicate one as the other.
test conservation_refuses_an_item_statement() fail {
  let outputs = finished_outputs_sweep(sample_tx_id, target_policy, target_name)
  let mint = finished_mint_sweep(sample_tx_id, target_policy, target_name)
  asset_conservation_fault_is_proven(item_statement(2, 0), outputs, mint)
}

/// §12.3's reason, on the other adjudication: a statement written rather than
/// decoded is still held to the shape rule.
///
/// The claimed quantity is 13 against a real 14, so the arithmetic *would* say
/// "proven" if the shape rule were not there. Claiming the honest 14 would leave
/// this vector green for the arithmetic's reason rather than the shape rule's,
/// and it would then say nothing.
test conservation_refuses_a_malformed_statement() fail {
  let outputs = finished_outputs_sweep(sample_tx_id, target_policy, target_name)
  let mint = finished_mint_sweep(sample_tx_id, target_policy, target_name)
  asset_conservation_fault_is_proven(
    FaultStatementV1 {
      ..conservation_statement(13),
      asset_name: bytearray.concat(
        target_name,
        #"00000000000000000000000000000000000000000000000000000000000000",
      ),
    },
    outputs,
    mint,
  )
}

// ---------------------------------------------------------------------------
// §12.5 — the tier-2 per-step full-preimage re-hash
//
// Four rows, and the claim is the two differences between them. Each pair holds
// the fixture, the field and the reads constant and varies only how many times
// the §8.8 door is opened over tier-2 carriage — which is exactly what a second
// *step* over the same field costs, since §10.2 makes a resuming transaction pay
// its own §7.1 hash check and nothing of the items already walked.
// ---------------------------------------------------------------------------

/// 397 field-1 items at §5.3's stride 40: 15,883 bytes, the largest fixed-stride
/// field that still fits `chunk_bytes_k` and therefore still carries under
/// tier 2 (§8.3).
fn tier2_fixed_stride_preimage() -> ByteArray {
  encode_field_preimage(input_items(item_tx_id_a, 0, 397))
}

/// A field-6 preimage at the same bound whose items are variable-width, so its
/// view construction pays §7.4's full-content walk where the fixed-stride one
/// pays an arithmetic identity. 1,442 items — a whole number of `script_item`'s
/// seven-wide cycle, so the total is exact rather than approximately right.
fn tier2_variable_width_preimage() -> ByteArray {
  encode_field_preimage(script_items(0, 1442))
}

/// Both fixtures sit at the tier-2 bound rather than merely being large, and
/// this is the line that notices if either drifts. A measurement quoted against
/// `chunk_bytes_k` has to be taken there.
test tier2_fixtures_sit_at_the_tier_two_bound() {
  let fixed = bytearray.length(tier2_fixed_stride_preimage())
  let variable = bytearray.length(tier2_variable_width_preimage())
  and {
    fixed == 15883,
    variable == 15865,
    fixed <= chunk_bytes_k,
    variable <= chunk_bytes_k,
  }
}

/// §12.5 row 1 — one tier-2 step over a fixed-stride field at the bound.
test tier2_fixed_stride_one_open() {
  let preimage = tier2_fixed_stride_preimage()
  let committed = source(sample_tx_id, 1, preimage)
  let Pair(view, _start) = open_raw_utxo_from(committed, 1, preimage)
  field_total_length(view) == 15883
}

/// §12.5 row 2 — the same field, the same fixture, two openings. The difference
/// from row 1 is one step's tier-2 re-hash and re-validation.
test tier2_fixed_stride_two_opens() {
  let preimage = tier2_fixed_stride_preimage()
  let committed = source(sample_tx_id, 1, preimage)
  let Pair(first, _a) = open_raw_utxo_from(committed, 1, preimage)
  let Pair(second, _b) = open_raw_utxo_from(committed, 1, preimage)
  and {
    field_total_length(first) == 15883,
    field_total_length(second) == 15883,
  }
}

/// §12.5 row 3 — one tier-2 step over a variable-width field at the bound.
test tier2_variable_width_one_open() {
  let preimage = tier2_variable_width_preimage()
  let committed = source(sample_tx_id, 6, preimage)
  let Pair(view, _start) = open_raw_utxo_from(committed, 6, preimage)
  field_total_length(view) == bytearray.length(preimage)
}

/// §12.5 row 4 — the same, twice. The difference from row 3 is one step's
/// tier-2 re-hash *plus* §7.4's full-content re-walk, which is the number a
/// variable-width dispute actually re-pays per step.
test tier2_variable_width_two_opens() {
  let preimage = tier2_variable_width_preimage()
  let committed = source(sample_tx_id, 6, preimage)
  let Pair(first, _a) = open_raw_utxo_from(committed, 6, preimage)
  let Pair(second, _b) = open_raw_utxo_from(committed, 6, preimage)
  and {
    field_total_length(first) == bytearray.length(preimage),
    field_total_length(second) == bytearray.length(preimage),
  }
}
