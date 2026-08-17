use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_224, blake2b_256}
use aiken/fuzz
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, suffix}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/cek_constant_v1
use midgard/cek_data_scan_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, decode_native_tx_field_preimage_lengths_v1,
  decode_native_tx_witness_set_compact, encode_native_tx_body_compact,
  encode_native_tx_field_preimage_lengths_v1, native_tx_canonical_size_v1,
  native_tx_id_for_version, native_tx_proof_commitment_v1,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_redeemer_witness,
  encode_midgard_tx_input, encode_midgard_tx_output,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_byte_list_preimage_cbor,
  decode_midgard_tx_mint_preimage_cbor,
}
use midgard/fraud_proofs/native_tx/transaction.{
  midgard_transaction_proof_commitment_v1, midgard_transaction_proof_source_v1,
  midgard_transaction_to_compact,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardAddressWitness, MidgardExecutionUnits,
  MidgardRedeemerWitness, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, MidgardTxInput, MidgardTxOutput, MidgardValue,
  MidgardVersionedScript, NativeCardanoScript, NativeTxBodyCompact,
  NativeTxCompact, NativeTxFieldPreimageLengthsV1, NativeTxWitnessSetCompact,
  PlutusV3Script, PubKeyCredential, RewardRedeemer, ScriptCredential,
  SpendRedeemer, TxIsValid, native_tx_version_v1,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_v1
use midgard/mpf_proof_fold_v1
use midgard/native_script_scan_v1
use midgard/native_tx_field_access_v1
use midgard/redeemer_item_proof_v1
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const h_28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_28_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

/// §8's door context for a step whose carriage is tier 1.
///
/// Every hand-written step in this module names an `Inline` carriage — the
/// preimage travels in the evidence — so the door needs neither the
/// consuming transaction's reference inputs (§8.2, tier 2) nor the §8.6
/// certificate minting policy id (§8.4, tier 3). Both are supplied empty, and
/// that is a *fact about these vectors* rather than a convenience: the tier-2
/// and tier-3 ladders are exercised where they belong, in
/// `native-tx-field-access-v1.test.ak`, against the door itself. A step here
/// that accidentally named a reference tier would fail closed rather than pass
/// with an unused context.
fn inline_field_door() -> field_door.MachineFieldDoorV1 {
  field_door.MachineFieldDoorV1 {
    reference_inputs: [],
    certificate_policy_id: #"",
  }
}

/// A tier-1 carriage over a field whose §5.1 preimage is exactly `items`.
///
/// The replacement for the retired `singleton_field_item_proofs` collection
/// half: instead of asserting a per-item Merkle opening against the field's
/// flat commitment, a step now carries the field's whole preimage and the door
/// hashes it once (§4, §8.8). Built through `encode_field_preimage`, so a
/// fixture cannot drift away from §5.1's array header and byte-string wrappers.
fn inline_field_carriage(
  items: List<ByteArray>,
) -> native_tx_field_access_v1.FieldCarriageV1 {
  native_tx_field_access_v1.Inline {
    preimage: native_tx_field_access_v1.encode_field_preimage(items),
  }
}

const maximum_mint_policy_item_cbor =
  #"82581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab88058200000000000000000000000000000000000000000000000000000000000000000015820000000000000000000000000000000000000000000000000000000000000000101582000000000000000000000000000000000000000000000000000000000000000020158200000000000000000000000000000000000000000000000000000000000000003015820000000000000000000000000000000000000000000000000000000000000000401582000000000000000000000000000000000000000000000000000000000000000050158200000000000000000000000000000000000000000000000000000000000000006015820000000000000000000000000000000000000000000000000000000000000000701582000000000000000000000000000000000000000000000000000000000000000080158200000000000000000000000000000000000000000000000000000000000000009015820000000000000000000000000000000000000000000000000000000000000000a015820000000000000000000000000000000000000000000000000000000000000000b015820000000000000000000000000000000000000000000000000000000000000000c015820000000000000000000000000000000000000000000000000000000000000000d015820000000000000000000000000000000000000000000000000000000000000000e015820000000000000000000000000000000000000000000000000000000000000000f0158200000000000000000000000000000000000000000000000000000000000000010015820000000000000000000000000000000000000000000000000000000000000001101582000000000000000000000000000000000000000000000000000000000000000120158200000000000000000000000000000000000000000000000000000000000000013015820000000000000000000000000000000000000000000000000000000000000001401582000000000000000000000000000000000000000000000000000000000000000150158200000000000000000000000000000000000000000000000000000000000000016015820000000000000000000000000000000000000000000000000000000000000001701582000000000000000000000000000000000000000000000000000000000000000180158200000000000000000000000000000000000000000000000000000000000000019015820000000000000000000000000000000000000000000000000000000000000001a015820000000000000000000000000000000000000000000000000000000000000001b015820000000000000000000000000000000000000000000000000000000000000001c015820000000000000000000000000000000000000000000000000000000000000001d015820000000000000000000000000000000000000000000000000000000000000001e015820000000000000000000000000000000000000000000000000000000000000001f0158200000000000000000000000000000000000000000000000000000000000000020015820000000000000000000000000000000000000000000000000000000000000002101582000000000000000000000000000000000000000000000000000000000000000220158200000000000000000000000000000000000000000000000000000000000000023015820000000000000000000000000000000000000000000000000000000000000002401582000000000000000000000000000000000000000000000000000000000000000250158200000000000000000000000000000000000000000000000000000000000000026015820000000000000000000000000000000000000000000000000000000000000002701582000000000000000000000000000000000000000000000000000000000000000280158200000000000000000000000000000000000000000000000000000000000000029015820000000000000000000000000000000000000000000000000000000000000002a015820000000000000000000000000000000000000000000000000000000000000002b015820000000000000000000000000000000000000000000000000000000000000002c015820000000000000000000000000000000000000000000000000000000000000002d015820000000000000000000000000000000000000000000000000000000000000002e015820000000000000000000000000000000000000000000000000000000000000002f0158200000000000000000000000000000000000000000000000000000000000000030015820000000000000000000000000000000000000000000000000000000000000003101582000000000000000000000000000000000000000000000000000000000000000320158200000000000000000000000000000000000000000000000000000000000000033015820000000000000000000000000000000000000000000000000000000000000003401582000000000000000000000000000000000000000000000000000000000000000350158200000000000000000000000000000000000000000000000000000000000000036015820000000000000000000000000000000000000000000000000000000000000003701582000000000000000000000000000000000000000000000000000000000000000380158200000000000000000000000000000000000000000000000000000000000000039015820000000000000000000000000000000000000000000000000000000000000003a015820000000000000000000000000000000000000000000000000000000000000003b015820000000000000000000000000000000000000000000000000000000000000003c015820000000000000000000000000000000000000000000000000000000000000003d015820000000000000000000000000000000000000000000000000000000000000003e015820000000000000000000000000000000000000000000000000000000000000003f0158200000000000000000000000000000000000000000000000000000000000000040015820000000000000000000000000000000000000000000000000000000000000004101582000000000000000000000000000000000000000000000000000000000000000420158200000000000000000000000000000000000000000000000000000000000000043015820000000000000000000000000000000000000000000000000000000000000004401582000000000000000000000000000000000000000000000000000000000000000450158200000000000000000000000000000000000000000000000000000000000000046015820000000000000000000000000000000000000000000000000000000000000004701582000000000000000000000000000000000000000000000000000000000000000480158200000000000000000000000000000000000000000000000000000000000000049015820000000000000000000000000000000000000000000000000000000000000004a015820000000000000000000000000000000000000000000000000000000000000004b015820000000000000000000000000000000000000000000000000000000000000004c015820000000000000000000000000000000000000000000000000000000000000004d015820000000000000000000000000000000000000000000000000000000000000004e015820000000000000000000000000000000000000000000000000000000000000004f0158200000000000000000000000000000000000000000000000000000000000000050015820000000000000000000000000000000000000000000000000000000000000005101582000000000000000000000000000000000000000000000000000000000000000520158200000000000000000000000000000000000000000000000000000000000000053015820000000000000000000000000000000000000000000000000000000000000005401582000000000000000000000000000000000000000000000000000000000000000550158200000000000000000000000000000000000000000000000000000000000000056015820000000000000000000000000000000000000000000000000000000000000005701582000000000000000000000000000000000000000000000000000000000000000580158200000000000000000000000000000000000000000000000000000000000000059015820000000000000000000000000000000000000000000000000000000000000005a015820000000000000000000000000000000000000000000000000000000000000005b015820000000000000000000000000000000000000000000000000000000000000005c015820000000000000000000000000000000000000000000000000000000000000005d015820000000000000000000000000000000000000000000000000000000000000005e015820000000000000000000000000000000000000000000000000000000000000005f0158200000000000000000000000000000000000000000000000000000000000000060015820000000000000000000000000000000000000000000000000000000000000006101582000000000000000000000000000000000000000000000000000000000000000620158200000000000000000000000000000000000000000000000000000000000000063015820000000000000000000000000000000000000000000000000000000000000006401582000000000000000000000000000000000000000000000000000000000000000650158200000000000000000000000000000000000000000000000000000000000000066015820000000000000000000000000000000000000000000000000000000000000006701582000000000000000000000000000000000000000000000000000000000000000680158200000000000000000000000000000000000000000000000000000000000000069015820000000000000000000000000000000000000000000000000000000000000006a015820000000000000000000000000000000000000000000000000000000000000006b015820000000000000000000000000000000000000000000000000000000000000006c015820000000000000000000000000000000000000000000000000000000000000006d015820000000000000000000000000000000000000000000000000000000000000006e015820000000000000000000000000000000000000000000000000000000000000006f0158200000000000000000000000000000000000000000000000000000000000000070015820000000000000000000000000000000000000000000000000000000000000007101582000000000000000000000000000000000000000000000000000000000000000720158200000000000000000000000000000000000000000000000000000000000000073015820000000000000000000000000000000000000000000000000000000000000007401582000000000000000000000000000000000000000000000000000000000000000750158200000000000000000000000000000000000000000000000000000000000000076015820000000000000000000000000000000000000000000000000000000000000007701582000000000000000000000000000000000000000000000000000000000000000780158200000000000000000000000000000000000000000000000000000000000000079015820000000000000000000000000000000000000000000000000000000000000007a015820000000000000000000000000000000000000000000000000000000000000007b015820000000000000000000000000000000000000000000000000000000000000007c015820000000000000000000000000000000000000000000000000000000000000007d015820000000000000000000000000000000000000000000000000000000000000007e015820000000000000000000000000000000000000000000000000000000000000007f01"

/// §5.1/§5.6 — the enveloped field-5 preimage carrying that one policy item.
///
/// The item's own bytes are unchanged by the flat reversion; what changed is
/// that field 5 is an enveloped **list** of per-policy items rather than the
/// retired raw map, so the preimage is the item under a §5.1 wrapper. Built
/// through the canonical encoder rather than hand-spelled, so a fixture cannot
/// drift away from §5.1's array and byte-string heads.
fn maximum_mint_preimage_cbor() -> ByteArray {
  native_tx_field_access_v1.encode_field_preimage(
    [maximum_mint_policy_item_cbor],
  )
}

const validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

// C49 (#540). The C70 target snapshot: the preprod epoch-303 protocol
// parameters pinned by `PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1`
// (demo/midgard-validation/tests/helpers/ordered-collection-boundary-v1.ts:78),
// which is the same snapshot every TypeScript Cardano boundary corpus is built
// against. These three numbers are the only free parameters in the min-fee and
// min-Ada formulas; a C70 parameter change re-derives both.
const target_snapshot_min_fee_a = 44

const target_snapshot_min_fee_b = 155_381

const target_snapshot_coins_per_utxo_byte = 4_310

// `validation_context_cbor` re-encoded at the target snapshot. The tuple is
// [1, "midgard-consensus-v1", 100, 0, 44, 155381, 100] -- identical to the
// profile-neutral context above except that `min_fee_a`/`min_fee_b` now carry
// the snapshot values instead of the zero-fee defaults.
const target_snapshot_validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d7631186400182c1a00025ef51864"

const reject_min_fee_code = #"455f4d494e5f464545"

const reject_value_not_preserved_code =
  #"455f56414c55455f4e4f545f505245534552564544"

test native_v1_optional_posix_time_sentinel_and_adjacent_invalid_are_exact() {
  and {
    !validation_machine_v1.validity_interval_is_malformed(-1, -1),
    !validation_machine_v1.validity_interval_is_malformed(0, 0),
    !validation_machine_v1.validity_interval_is_malformed(0, 1),
    validation_machine_v1.validity_interval_is_malformed(-2, -1),
    validation_machine_v1.validity_interval_is_malformed(-1, -2),
    validation_machine_v1.validity_interval_is_malformed(1, 0),
  }
}

test purpose_kind_to_redeemer_pointer_mapping_is_exact() {
  and {
    validation_machine_v1.redeemer_tag_for_purpose_kind_v1(0) == Some(0),
    validation_machine_v1.redeemer_tag_for_purpose_kind_v1(1) == Some(1),
    validation_machine_v1.redeemer_tag_for_purpose_kind_v1(2) == Some(3),
    validation_machine_v1.redeemer_tag_for_purpose_kind_v1(3) == Some(6),
    validation_machine_v1.redeemer_tag_for_purpose_kind_v1(-1) == None,
    validation_machine_v1.redeemer_tag_for_purpose_kind_v1(4) == None,
    validation_machine_v1.redeemer_pointer_matches_purpose_v1(0, 7, 0, 7),
    validation_machine_v1.redeemer_pointer_matches_purpose_v1(1, 7, 1, 7),
    validation_machine_v1.redeemer_pointer_matches_purpose_v1(2, 7, 3, 7),
    validation_machine_v1.redeemer_pointer_matches_purpose_v1(3, 7, 6, 7),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(0, 7, 0, 8),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(1, 7, 1, 8),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(2, 7, 3, 8),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(3, 7, 6, 8),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(-1, 7, 0, 7),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(4, 7, 6, 7),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(0, 7, -1, 7),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(0, 7, 2, 7),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(0, 7, 4, 7),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(0, 7, 5, 7),
    !validation_machine_v1.redeemer_pointer_matches_purpose_v1(0, 7, 7, 7),
  }
}

fn v1_program_envelope() -> ByteArray {
  cek_proof_v1.encode_program_envelope_v1(1, 1, 0, h_a, 3, 144)
}

fn empty_mpf_proof_descriptor() -> mpf_proof_fold_v1.ProofDescriptorV1 {
  mpf_proof_fold_v1.ProofDescriptorV1 {
    version: 1,
    frame_count: 0,
    terminal_cursor: 0,
    peaks: [],
  }
}

fn empty_v1_transaction() -> MidgardTransaction {
  let empty_mint: Data = []
  MidgardTransaction {
    version: native_tx_version_v1,
    validity: TxIsValid,
    body: MidgardTransactionBody {
      inputs: [],
      reference_inputs: [],
      outputs: [],
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers: [],
      required_signers: [],
      mint: empty_mint,
      script_integrity_hash: zero,
      auxiliary_data_hash: zero,
      network_id: 255,
    },
    witness_set: MidgardTransactionWitnessSet {
      addr_tx_wits: [],
      script_tx_wits: [],
      redeemer_tx_wits: [],
    },
  }
}

fn repeated_input(input: MidgardTxInput, count: Int) -> List<MidgardTxInput> {
  if count <= 0 {
    []
  } else {
    [input, ..repeated_input(input, count - 1)]
  }
}

fn address_witness_key_hashes(
  witnesses: List<MidgardAddressWitness>,
) -> List<ByteArray> {
  when witnesses is {
    [] -> []
    [witness, ..rest] ->
      [
        blake2b_224(witness.verification_key),
        ..address_witness_key_hashes(rest)
      ]
  }
}

fn signer_frontier_for_tx(
  tx: MidgardTransaction,
) -> validation_merkle_v1.BuiltFrontier {
  tx.witness_set.addr_tx_wits
    |> address_witness_key_hashes
    |> list.sort(bytearray.compare)
    |> list.unique
    |> list.map(script_proof_v1.signer_leaf_hash)
    |> validation_merkle_v1.build_frontier
}

fn contains_non_native_script(scripts: List<MidgardVersionedScript>) -> Bool {
  when scripts is {
    [] -> False
    [script, ..rest] -> or {
        script.language != NativeCardanoScript,
        contains_non_native_script(rest),
      }
  }
}

fn signatures_scan_witness(
  tx: MidgardTransaction,
  stage: Int,
  address_count: Int,
  required_count: Int,
  address_seen: Int,
  required_seen: Int,
  previous_order_key: ByteArray,
  previous_signer_hash: ByteArray,
  signer_frontier: validation_merkle_v1.BuiltFrontier,
  invalid_signature_seen: Int,
) -> ByteArray {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  validation_machine_v1.encode_signatures_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    stage,
    address_count,
    required_count,
    address_seen,
    required_seen,
    previous_order_key,
    previous_signer_hash,
    signer_frontier.count,
    signer_frontier.peaks,
    invalid_signature_seen,
  )
}

fn initial_signatures_witness(tx: MidgardTransaction) -> ByteArray {
  signatures_scan_witness(
    tx,
    0,
    if tx.witness_set.addr_tx_wits == [] {
      0
    } else {
      -1
    },
    if tx.body.required_signers == [] {
      0
    } else {
      -1
    },
    0,
    0,
    #"",
    #"",
    validation_merkle_v1.build_frontier([]),
    0,
  )
}

fn signatures_step_fixture(
  tx: MidgardTransaction,
  ledger_delta_root: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = initial_signatures_witness(tx)
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.Signatures,
      program_counter: 4,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.Signatures,
        4,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root,
    },
    work_witness_cbor,
  )
}

fn phase_a_native_step_fixture(
  tx: MidgardTransaction,
  ledger_delta_root: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let Pair(signature_pre, _signature_work_witness_cbor) = signatures_step_fixture(
    tx,
    ledger_delta_root,
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let script_count = if tx.witness_set.script_tx_wits == [] {
    0
  } else {
    -1
  }
  let work_witness_cbor = validation_machine_v1.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    0,
    script_count,
    0,
    0,
    0,
    #"",
    0,
    #"",
    0,
    0,
    -1,
    signer_frontier.count,
    signer_frontier.peaks,
    #"",
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      ..signature_pre,
      phase: validation_trace_v1.PhaseANativeScripts,
      program_counter: 5,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.PhaseANativeScripts,
        5,
        work_witness_cbor,
      ),
    },
    work_witness_cbor,
  )
}

fn phase_a_native_scan_witness(
  tx: MidgardTransaction,
  stage: Int,
  script_count: Int,
  script_seen: Int,
  contains_non_native_script: Int,
  item_length: Int,
  item_commitment: ByteArray,
  cursor: Int,
  stack_root: ByteArray,
  stack_depth: Int,
  node_count: Int,
  result: Int,
) -> ByteArray {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  validation_machine_v1.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    stage,
    script_count,
    script_seen,
    contains_non_native_script,
    item_length,
    item_commitment,
    cursor,
    stack_root,
    stack_depth,
    node_count,
    result,
    signer_frontier.count,
    signer_frontier.peaks,
    #"",
  )
}

fn next_phase_a_native_state(
  pre: validation_trace_v1.ValidationMachineStateV1,
  work_witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      pre.program_counter + 1,
      work_witness_cbor,
    ),
  }
}

/// The single-chunk `bounded_item_v1` proof over item 0 of a single-item field.
///
/// This is what survives of the retired `singleton_field_item_proofs`. Its
/// collection half is gone: no step opens a per-item collection proof against a
/// §4 flat field commitment any more, and the carriage that replaced it is
/// `inline_field_carriage([key])`. The **chunk** half stays because four phases
/// hand a partially-read item forward across steps and their control state
/// carries a genuine `bounded_item_v1` root the later step opens against
/// (`NativeScriptTokenWitness`, `NativeExecutionScanWitness.first_chunk_proof`,
/// the redeemer item proof) — a satisfiable opening the flat reversion never
/// touched.
fn singleton_field_item_chunk_proof(
  field_index: Int,
  key: ByteArray,
) -> bounded_item_v1.ChunkProofV1 {
  let chunk_frontier = [
    validation_merkle_v1.FrontierPeak {
      height: 0,
      hash: bounded_item_v1.hash_chunk(field_index, 0, 0, key),
    },
  ]
  bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index,
    item_index: 0,
    total_length: bytearray.length(key),
    chunk_index: 0,
    chunk: key,
    frontier: chunk_frontier,
    siblings: [],
  }
}

fn indexed_inline_source_leaves(
  script: MidgardVersionedScript,
  source_index: Int,
  source_count: Int,
) -> List<ByteArray> {
  if source_index == source_count {
    []
  } else {
    [
      script_proof_v1.source_leaf_hash(0, cbor.serialise(source_index), script),
      ..indexed_inline_source_leaves(script, source_index + 1, source_count)
    ]
  }
}

fn indexed_spend_purpose_leaves(
  purpose_index: Int,
  purpose_count: Int,
) -> List<ByteArray> {
  if purpose_index == purpose_count {
    []
  } else {
    [
      script_proof_v1.purpose_leaf_hash(
        0,
        purpose_index,
        h_28_a,
        cbor.serialise(purpose_index),
      ),
      ..indexed_spend_purpose_leaves(purpose_index + 1, purpose_count)
    ]
  }
}

fn merkle_parent_layer(leaves: List<ByteArray>) -> List<ByteArray> {
  when leaves is {
    [] -> []
    [left, right, ..rest] ->
      [
        validation_merkle_v1.hash_branch(left, right),
        ..merkle_parent_layer(rest)
      ]
    _ -> fail @"power-of-two membership layer must contain complete pairs"
  }
}

fn first_leaf_power_of_two_siblings(leaves: List<ByteArray>) -> List<ByteArray> {
  when leaves is {
    [_leaf] -> []
    [_leaf, sibling, ..] ->
      [sibling, ..first_leaf_power_of_two_siblings(merkle_parent_layer(leaves))]
    _ -> fail @"membership tree must be a non-empty power of two"
  }
}

fn input_sets_scan_witness(
  tx: MidgardTransaction,
  spend_count: Int,
  reference_count: Int,
  spend_seen: Int,
  reference_seen: Int,
  previous_key: ByteArray,
  schedule_hash: ByteArray,
) -> ByteArray {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  validation_machine_v1.encode_input_sets_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    spend_count,
    reference_count,
    spend_seen,
    reference_seen,
    previous_key,
    schedule_hash,
  )
}

fn input_sets_pre(
  tx: MidgardTransaction,
  program_counter: Int,
  work_witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.InputSets,
    program_counter,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.InputSets,
      program_counter,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
}

// The canonical exact-rejection successor. It deliberately CARRIES FORWARD
// `pre.ledger_delta_root` (via `..pre`) rather than clearing it: the claimed
// delta commitment is immutable context, and clearing it here is what made
// every rejection one-step unprovable from a realistic (non-empty) pre-state.
// The rejecting terminal's no-op obligation lives in
// `encode_terminal_rejection_witness` (empty operation list, post root = prior
// root) and in the claim layer, not in this field.
fn exact_rejection_post(
  pre: validation_trace_v1.ValidationMachineStateV1,
  rejection_code: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      pre.program_counter + 1,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
}

// A realistic NON-EMPTY claimed ledger delta root: the frontier commitment over
// one authenticated deletion and one authenticated insertion. This is the shape
// every real transaction carries (it spends inputs and creates outputs), and it
// is therefore the shape the rejection machinery must work against. Using
// `frontier_commitment(0, [])` for rejection fixtures was the workaround that
// hid the `rejected_successor_is_exact` / `immutable_context_matches`
// contradiction, because it is the one pre-state in which the contradiction is
// invisible.
fn non_empty_claimed_delta_root() -> ByteArray {
  let descriptor = empty_mpf_proof_descriptor()
  let deletion = validation_machine_v1.ledger_delta_operation_leaf_hash(
    0,
    #"010203",
    #"",
    descriptor,
  )
  let insertion = validation_machine_v1.ledger_delta_operation_leaf_hash(
    1,
    #"0405",
    #"060708",
    descriptor,
  )
  let frontier = validation_merkle_v1.build_frontier([deletion, insertion])
  validation_merkle_v1.frontier_commitment(frontier.count, frontier.peaks)
}

fn phase_a_script_preconditions_fixture(
  tx: MidgardTransaction,
  ledger_delta_root: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let Pair(native_pre, _native_work_witness_cbor) = phase_a_native_step_fixture(
    tx,
    ledger_delta_root,
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let work_witness_cbor = validation_machine_v1.encode_phase_a_script_preconditions_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    contains_non_native_script(tx.witness_set.script_tx_wits),
    0,
    0,
    #"",
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      ..native_pre,
      phase: validation_trace_v1.PhaseAScriptPreconditions,
      program_counter: 6,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.PhaseAScriptPreconditions,
        6,
        work_witness_cbor,
      ),
    },
    work_witness_cbor,
  )
}

test phase_a_script_preconditions_control_cbor_is_canonical() {
  let tx = empty_v1_transaction()
  let Pair(_pre, witness_cbor) = phase_a_script_preconditions_fixture(tx, h_a)
  when cbor.deserialise(witness_cbor) is {
    None -> False
    Some(data) -> list.length(builtin.un_list_data(data)) == 11
  }
}

/// The field-3 preimage of the duplicate-observer fixture: two items, both the
/// same 28-byte observer hash.
///
/// Under the retired counted scheme this had to be a pair of per-item openings
/// with a shared collection frontier and a sibling each. Under §8's door the
/// duplicate *is* the preimage, and which of the two items a step reads is the
/// `item_index` it names — which is the whole point of the fixture, and is now
/// visible in one line instead of thirty.
fn duplicate_observer_field_carriage() -> native_tx_field_access_v1.FieldCarriageV1 {
  inline_field_carriage([h_28_a, h_28_a])
}

fn observer_hash_at(item_index: Int) -> ByteArray {
  bytearray.from_int_big_endian(item_index + 1, 28)
}

fn observer_hashes_from(item_index: Int, item_count: Int) -> List<ByteArray> {
  if item_index == item_count {
    []
  } else {
    [
      observer_hash_at(item_index),
      ..observer_hashes_from(item_index + 1, item_count)
    ]
  }
}

fn observer_collection_leaves_from(
  item_index: Int,
  item_count: Int,
) -> List<ByteArray> {
  if item_index == item_count {
    []
  } else {
    let observer_hash = observer_hash_at(item_index)
    let item_commitment = bounded_item_v1.from_bytes(
      3,
      item_index,
      observer_hash,
    )
    [
      bounded_collection_v1.hash_item(3, item_index, 28, item_commitment),
      ..observer_collection_leaves_from(item_index + 1, item_count)
    ]
  }
}

fn observer_collection_subtree_root(start: Int, size: Int) -> ByteArray {
  if size == 1 {
    let observer_hash = observer_hash_at(start)
    bounded_collection_v1.hash_item(
      3,
      start,
      28,
      bounded_item_v1.from_bytes(3, start, observer_hash),
    )
  } else {
    let half = size / 2
    validation_merkle_v1.hash_branch(
      observer_collection_subtree_root(start, half),
      observer_collection_subtree_root(start + half, half),
    )
  }
}

/// The field-3 preimage of an `item_count`-observer fixture, as a tier-1
/// carriage.
///
/// Replaces `observer_field_item_proofs`: no per-item frontier, no siblings
/// argument. The item a step reads is chosen by the `item_index` in its
/// evidence, and the field's §5.2 count is the authenticated header of these
/// very bytes, so a step cannot understate it.
fn observer_field_carriage(
  item_count: Int,
) -> native_tx_field_access_v1.FieldCarriageV1 {
  inline_field_carriage(observer_hashes_from(0, item_count))
}

fn resolve_inputs_step_fixture(
  tx: MidgardTransaction,
  prior_ledger_root: ByteArray,
  cursor: Int,
  accumulator: ByteArray,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = validation_machine_v1.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    cursor,
    accumulator,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    None,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root,
      phase: validation_trace_v1.ResolveInputs,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ResolveInputs,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      // Re-based off `frontier_commitment(0, [])`: this fixture backs rejection
      // proofs, so it must carry the realistic non-empty claimed delta.
      ledger_delta_root: non_empty_claimed_delta_root(),
    },
    work_witness_cbor,
  )
}

fn script_sources_output_step_fixture_with_resolved_items(
  tx: MidgardTransaction,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  replay_cursor: Int,
  resolved_item_frontier: validation_merkle_v1.BuiltFrontier,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  spend_index: Int,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  output_cursor: Int,
  output_frontier: validation_merkle_v1.BuiltFrontier,
  output_total_count: Int,
  receive_scan: validation_machine_v1.ReceivePurposeScanControlV1,
  observer_scan: validation_machine_v1.ObserverPurposeScanControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  // The mint fold only carries the `policy_count == -1` "not yet begun"
  // sentinel while the machine is at stage 6 or earlier. `script_sources_stage_six`
  // always leaves stage 6 with `policy_count >= 0` (validation-machine-v1.ak:9548
  // for the empty-mint case, :9553 otherwise), and both stage-7 binders --
  // `mint_fold_control_is_well_formed` (:12072) on the evidence path and
  // `script_sources_stage_seven_control_is_bound` (:10005) on the deployed path --
  // reject the sentinel from stage 7 on. Fixtures at stage >= 7 must therefore
  // use the settled fold, matching `script_discovery_step_fixture_with_output_frontier`.
  let mint_fold = if stage >= 7 {
    validation_machine_v1.MintFoldControlV1 {
      ..validation_machine_v1.empty_mint_fold_control(),
      policy_count: 0,
    }
  } else {
    validation_machine_v1.empty_mint_fold_control()
  }
  let work_witness_cbor = validation_machine_v1.encode_script_sources_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    resolved_input_count,
    resolved_inputs_accumulator,
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    resolved_item_frontier.peaks,
    stage,
    source_frontier.count,
    source_frontier.peaks,
    redeemer_frontier.count,
    redeemer_frontier.peaks,
    replay_cursor,
    replay_accumulator,
    replay_remaining_schedule_hash,
    spend_index,
    purpose_frontier.count,
    purpose_frontier.peaks,
    output_cursor,
    output_frontier.count,
    output_frontier.peaks,
    output_total_count,
    receive_scan,
    source_frontier.count,
    redeemer_frontier.count,
    observer_scan,
    mint_fold,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.ScriptSources,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
    },
    work_witness_cbor,
  )
}

test observer_purpose_scan_control_cbor_is_canonical() {
  let encoded = validation_machine_v1.encode_observer_purpose_scan_control(
    validation_machine_v1.empty_observer_purpose_scan_control(),
  )
  and {
    encoded == #"83004000",
    when cbor.deserialise(encoded) is {
      None -> False
      Some(data) -> list.length(builtin.un_list_data(data)) == 3
    },
  }
}

test receive_purpose_scan_control_cbor_is_canonical() {
  let encoded = validation_machine_v1.encode_receive_purpose_scan_control(
    validation_machine_v1.empty_receive_purpose_scan_control(),
  )
  encoded == #"86008000404080"
}

fn script_sources_output_step_fixture(
  tx: MidgardTransaction,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  replay_cursor: Int,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  spend_index: Int,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  output_cursor: Int,
  output_frontier: validation_merkle_v1.BuiltFrontier,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  script_sources_output_step_fixture_with_resolved_items(
    tx,
    resolved_input_count,
    resolved_inputs_accumulator,
    stage,
    source_frontier,
    redeemer_frontier,
    replay_cursor,
    validation_merkle_v1.build_frontier([]),
    replay_accumulator,
    replay_remaining_schedule_hash,
    spend_index,
    purpose_frontier,
    output_cursor,
    output_frontier,
    output_frontier.count,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    validation_machine_v1.empty_observer_purpose_scan_control(),
    program_counter,
  )
}

fn script_sources_mint_fold_fixture(
  tx: MidgardTransaction,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  mint_fold: validation_machine_v1.MintFoldControlV1,
  stage: Int,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let work_witness_cbor = validation_machine_v1.encode_script_sources_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    0,
    validation_machine_v1.initial_resolution_accumulator(),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    empty_frontier.peaks,
    stage,
    0,
    empty_frontier.peaks,
    0,
    empty_frontier.peaks,
    0,
    validation_machine_v1.initial_resolution_accumulator(),
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    purpose_frontier.count,
    purpose_frontier.peaks,
    0,
    0,
    empty_frontier.peaks,
    0,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    0,
    0,
    validation_machine_v1.empty_observer_purpose_scan_control(),
    mint_fold,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.ScriptSources,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
    },
    work_witness_cbor,
  )
}

type AuthenticatedOutputMaterialV1 {
  item_commitment: ByteArray,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  initial: ledger_output_proof_v1.LedgerOutputProofControlV1,
  first_successor: ledger_output_proof_v1.LedgerOutputProofControlV1,
  terminal: ledger_output_proof_v1.LedgerOutputProofControlV1,
  descriptor_cbor: ByteArray,
}

fn advanced_output_proof(
  result: Option<ledger_output_proof_v1.LedgerOutputProofStepResultV1>,
) -> ledger_output_proof_v1.LedgerOutputProofControlV1 {
  expect Some(ledger_output_proof_v1.LedgerOutputProofAdvanced { control }) = result
  control
}

fn authenticated_output_material(
  output_cbor: ByteArray,
) -> AuthenticatedOutputMaterialV1 {
  let chunk_proof = singleton_field_item_chunk_proof(2, output_cbor)
  let item_commitment = bounded_item_v1.from_bytes(2, 0, output_cbor)
  let initial = ledger_output_proof_v1.initial_control_v1(
    0,
    bytearray.length(output_cbor),
    item_commitment,
  )
  let first_successor = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      initial,
      ledger_output_proof_v1.LedgerOutputProofChunks {
        chunk_proof,
        next_chunk_proof: None,
      },
    ),
  )
  let optional_field = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      first_successor,
      ledger_output_proof_v1.LedgerOutputProofChunks {
        chunk_proof,
        next_chunk_proof: None,
      },
    ),
  )
  let scan_terminal = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      optional_field,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let value_ready = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      scan_terminal,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let value_finalize = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      value_ready,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let value_terminal = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      value_finalize,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let terminal = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      value_terminal,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  expect Some(cardano_tx_out) = ledger_output_proof_v1.cardano_tx_out_summary_v1(
    terminal,
  )
  expect Some(midgard_tx_out) = ledger_output_proof_v1.midgard_tx_out_summary_v1(
    terminal,
  )
  expect Some(cardano_spend_datum) = ledger_output_proof_v1.cardano_spend_datum_summary_v1(
    terminal,
  )
  let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: terminal.output_index,
    total_length: terminal.total_length,
    item_commitment: terminal.item_commitment,
    address: terminal.output_scan.address,
    lovelace: terminal.output_scan.lovelace,
    asset_count: terminal.output_scan.asset_count,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(
      terminal.output_scan.asset_count,
      terminal.output_scan.asset_peaks,
    ),
    cardano_value_size: terminal.output_scan.cardano_value_size,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out,
    midgard_tx_out,
    cardano_spend_datum,
  }
  AuthenticatedOutputMaterialV1 {
    item_commitment,
    chunk_proof,
    initial,
    first_successor,
    terminal,
    descriptor_cbor: ledger_output_commitment_v1.encode(descriptor),
  }
}

fn script_sources_active_output_proof_fixture(
  tx: MidgardTransaction,
  output_cursor: Int,
  output_frontier: validation_merkle_v1.BuiltFrontier,
  receive_scan: validation_machine_v1.ReceivePurposeScanControlV1,
  output_proof: ledger_output_proof_v1.LedgerOutputProofControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let control = validation_machine_v1.ScriptSourcesControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    stage: 5,
    source_count: 0,
    source_peaks: validation_merkle_v1.empty_frontier(),
    redeemer_count: 0,
    redeemer_peaks: validation_merkle_v1.empty_frontier(),
    replay_cursor: 0,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    spend_index: 0,
    purpose_count: 0,
    purpose_peaks: validation_merkle_v1.empty_frontier(),
    output_cursor,
    output_count: output_frontier.count,
    output_peaks: output_frontier.peaks,
    output_total_count: output_frontier.count,
    receive_scan,
    source_total_count: 0,
    redeemer_total_count: 0,
    observer_scan: validation_machine_v1.empty_observer_purpose_scan_control(),
    discovery: validation_machine_v1.empty_script_discovery_control(),
    output_proof: Some(output_proof),
    pending_source_cbor: #"",
    mint_fold: validation_machine_v1.empty_mint_fold_control(),
    resolution_schedule_hash: validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
  let work_witness_cbor = validation_machine_v1.encode_script_sources_output_proof_witness(
    control,
    output_proof,
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.ScriptSources,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
    },
    work_witness_cbor,
  )
}

fn script_sources_step_fixture_with_resolved_items(
  tx: MidgardTransaction,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  replay_cursor: Int,
  resolved_item_frontier: validation_merkle_v1.BuiltFrontier,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  spend_index: Int,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  script_sources_output_step_fixture_with_resolved_items(
    tx,
    resolved_input_count,
    resolved_inputs_accumulator,
    stage,
    source_frontier,
    redeemer_frontier,
    replay_cursor,
    resolved_item_frontier,
    replay_accumulator,
    replay_remaining_schedule_hash,
    spend_index,
    purpose_frontier,
    0,
    validation_merkle_v1.build_frontier([]),
    0,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    validation_machine_v1.empty_observer_purpose_scan_control(),
    program_counter,
  )
}

fn script_sources_receive_scan_step_fixture(
  tx: MidgardTransaction,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  output_cursor: Int,
  output_frontier: validation_merkle_v1.BuiltFrontier,
  receive_scan: validation_machine_v1.ReceivePurposeScanControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    7,
    validation_merkle_v1.build_frontier([]),
    validation_merkle_v1.build_frontier([]),
    0,
    validation_merkle_v1.build_frontier([]),
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    purpose_frontier,
    output_cursor,
    output_frontier,
    output_frontier.count,
    receive_scan,
    validation_machine_v1.empty_observer_purpose_scan_control(),
    program_counter,
  )
}

fn script_sources_step_fixture(
  tx: MidgardTransaction,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  replay_cursor: Int,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  spend_index: Int,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  script_sources_step_fixture_with_resolved_items(
    tx,
    resolved_input_count,
    resolved_inputs_accumulator,
    stage,
    source_frontier,
    redeemer_frontier,
    replay_cursor,
    validation_merkle_v1.build_frontier([]),
    replay_accumulator,
    replay_remaining_schedule_hash,
    spend_index,
    purpose_frontier,
    program_counter,
  )
}

/// Stage-1 state whose work witness carries a pending redeemer-item control
/// hash (the 31-item `encode_script_sources_redeemer_item_witness` form). This
/// is the shape `script_sources_stage_one` commits to between the
/// `TransactionRedeemerItemBeginWitness` step and the terminal item step.
fn script_sources_pending_redeemer_item_fixture(
  tx: MidgardTransaction,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_total_count: Int,
  redeemer_item_control_hash: ByteArray,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let control = validation_machine_v1.ScriptSourcesControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    stage: 1,
    source_count: 0,
    source_peaks: empty_frontier.peaks,
    redeemer_count: redeemer_frontier.count,
    redeemer_peaks: redeemer_frontier.peaks,
    replay_cursor: 0,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    spend_index: 0,
    purpose_count: 0,
    purpose_peaks: empty_frontier.peaks,
    output_cursor: 0,
    output_count: 0,
    output_peaks: empty_frontier.peaks,
    output_total_count: 0,
    receive_scan: validation_machine_v1.empty_receive_purpose_scan_control(),
    source_total_count: 0,
    redeemer_total_count,
    observer_scan: validation_machine_v1.empty_observer_purpose_scan_control(),
    discovery: validation_machine_v1.empty_script_discovery_control(),
    output_proof: None,
    pending_source_cbor: redeemer_item_control_hash,
    mint_fold: validation_machine_v1.empty_mint_fold_control(),
    resolution_schedule_hash: validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
  let work_witness_cbor = validation_machine_v1.encode_script_sources_redeemer_item_witness(
    control,
    redeemer_item_control_hash,
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.ScriptSources,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
    },
    work_witness_cbor,
  )
}

fn script_discovery_step_fixture_with_output_frontier(
  tx: MidgardTransaction,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  output_cursor: Int,
  output_frontier: validation_merkle_v1.BuiltFrontier,
  output_descriptor_frontier: validation_merkle_v1.BuiltFrontier,
  discovery: validation_machine_v1.ScriptDiscoveryControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let base_control = validation_machine_v1.ScriptSourcesControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    stage,
    source_count: source_frontier.count,
    source_peaks: source_frontier.peaks,
    redeemer_count: redeemer_frontier.count,
    redeemer_peaks: redeemer_frontier.peaks,
    replay_cursor: 0,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    spend_index: 0,
    purpose_count: purpose_frontier.count,
    purpose_peaks: purpose_frontier.peaks,
    output_cursor,
    output_count: output_frontier.count,
    output_peaks: output_frontier.peaks,
    output_total_count: output_frontier.count,
    receive_scan: validation_machine_v1.ReceivePurposeScanControlV1 {
      ..validation_machine_v1.empty_receive_purpose_scan_control(),
      descriptor_peaks: output_descriptor_frontier.peaks,
    },
    source_total_count: source_frontier.count,
    redeemer_total_count: redeemer_frontier.count,
    observer_scan: validation_machine_v1.empty_observer_purpose_scan_control(),
    discovery,
    output_proof: None,
    pending_source_cbor: #"",
    mint_fold: validation_machine_v1.MintFoldControlV1 {
      ..validation_machine_v1.empty_mint_fold_control(),
      policy_count: 0,
    },
    resolution_schedule_hash: validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
  let work_witness_cbor = validation_machine_v1.encode_script_sources_discovery_witness(
    base_control,
    stage,
    discovery,
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.ScriptSources,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
    },
    work_witness_cbor,
  )
}

fn script_discovery_step_fixture(
  tx: MidgardTransaction,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  discovery: validation_machine_v1.ScriptDiscoveryControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  script_discovery_step_fixture_with_output_frontier(
    tx,
    stage,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    0,
    validation_merkle_v1.build_frontier([]),
    validation_merkle_v1.build_frontier([]),
    discovery,
    program_counter,
  )
}

fn native_scripts_step_fixture(
  tx: MidgardTransaction,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  execution_frontier: validation_merkle_v1.BuiltFrontier,
  execution_cursor: Int,
  language_bitmap: Int,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let control = validation_machine_v1.NativeScriptsControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    spend_input_count: 0,
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    source_count: source_frontier.count,
    source_peaks: source_frontier.peaks,
    redeemer_count: redeemer_frontier.count,
    redeemer_peaks: redeemer_frontier.peaks,
    purpose_count: purpose_frontier.count,
    purpose_peaks: purpose_frontier.peaks,
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    output_descriptor_peaks: validation_merkle_v1.empty_frontier(),
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
    execution_count: execution_frontier.count,
    execution_peaks: execution_frontier.peaks,
    execution_cursor,
    language_bitmap,
    resolution_schedule_hash: validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
  let work_witness_cbor = validation_machine_v1.encode_native_scripts_control_v1(
    control,
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.NativeScripts,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.NativeScripts,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
    },
    work_witness_cbor,
  )
}

fn empty_native_control_fixture(
  tx: MidgardTransaction,
) -> validation_machine_v1.NativeScriptsControlV1 {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  validation_machine_v1.NativeScriptsControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    spend_input_count: 0,
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    source_count: 0,
    source_peaks: validation_merkle_v1.empty_frontier(),
    redeemer_count: 0,
    redeemer_peaks: validation_merkle_v1.empty_frontier(),
    purpose_count: 0,
    purpose_peaks: validation_merkle_v1.empty_frontier(),
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    output_descriptor_peaks: validation_merkle_v1.empty_frontier(),
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
    execution_count: 0,
    execution_peaks: validation_merkle_v1.empty_frontier(),
    execution_cursor: 0,
    language_bitmap: 0,
    resolution_schedule_hash: validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

fn cek_state_fixture(
  tx: MidgardTransaction,
  control: validation_machine_v1.NativeScriptsControlV1,
  context_control_cbor: ByteArray,
  execution_cursor: Int,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let program_envelope_hash = if context_control_cbor == #"" {
    #""
  } else {
    h_b
  }
  let work_witness_cbor = validation_machine_v1.encode_cek_witness_v1(
    control,
    context_control_cbor,
    execution_cursor,
    0,
    0,
    #"",
    0,
    0,
    program_envelope_hash,
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.Cek,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.Cek,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
    },
    work_witness_cbor,
  )
}

fn empty_cek_context_control_fixture(
  stage: Int,
) -> validation_machine_v1.CekContextControlV1 {
  let redeemer_context = validation_machine_v1.initial_cek_redeemer_context_control_v1()
  validation_machine_v1.CekContextControlV1 {
    stage,
    language_tag: 3,
    program_term_root: h_a,
    program_envelope_hash: h_b,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: h_28_a,
    subject: #"00",
    redeemer_leaf: h_c,
    redeemer_context_control_hash: if stage == 0 {
      #""
    } else {
      validation_machine_v1.hash_cek_redeemer_context_control_v1(
        redeemer_context,
      )
    },
    execution_memory_limit: 10,
    execution_cpu_limit: 20,
    reference_items: cek_data_v1.empty_data_list_summary_v1(),
    spend_items: cek_data_v1.empty_data_list_summary_v1(),
    output_items: cek_data_v1.empty_data_list_summary_v1(),
    signer_items: cek_data_v1.empty_data_list_summary_v1(),
    observer_count: 0,
    observer_items: cek_data_v1.empty_data_pair_summary_v1(),
    previous_observer: #"",
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_cursor: 0,
    current_mint_policy: #"",
    current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
    mint_policies: cek_data_v1.empty_data_pair_summary_v1(),
    mint_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
  }
}

fn verify_cek_context_transition_fixture(
  tx: MidgardTransaction,
  native_control: validation_machine_v1.NativeScriptsControlV1,
  context_control: validation_machine_v1.CekContextControlV1,
  successor_control: validation_machine_v1.CekContextControlV1,
  auxiliary: validation_machine_v1.ValidationAuxiliaryWitnessV1,
  program_counter: Int,
) -> Bool {
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    program_counter,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(successor_control),
    0,
    program_counter + 1,
  )
  validation_machine_v1.verify_cek_observer_context_step(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary,
    native_control,
    context_control,
    0,
    0,
    0,
    inline_field_door(),
  )
}

fn verify_cek_context_source_transition_fixture(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  native_control: validation_machine_v1.NativeScriptsControlV1,
  context_control: validation_machine_v1.CekContextControlV1,
  successor_control: validation_machine_v1.CekContextControlV1,
  auxiliary: validation_machine_v1.ValidationAuxiliaryWitnessV1,
  program_counter: Int,
) -> Bool {
  let base = empty_v1_transaction()
  let Pair(base_pre, work_witness_cbor) = cek_state_fixture(
    base,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    program_counter,
  )
  let Pair(base_post, _) = cek_state_fixture(
    base,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(successor_control),
    0,
    program_counter + 1,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..base_pre,
    transaction_id,
    transaction_commitment,
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..base_post,
    transaction_id,
    transaction_commitment,
  }
  validation_machine_v1.verify_cek_observer_context_step(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary,
    native_control,
    context_control,
    0,
    0,
    0,
    inline_field_door(),
  )
}

fn cek_context_native_control_fixture(
  tx: MidgardTransaction,
) -> validation_machine_v1.NativeScriptsControlV1 {
  let singleton = validation_merkle_v1.build_frontier([h_a])
  validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    purpose_count: 1,
    purpose_peaks: singleton.peaks,
    execution_count: 1,
    execution_peaks: singleton.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
}

fn test_power_of_two(exponent: Int) -> Int {
  if exponent == 0 {
    1
  } else {
    2 * test_power_of_two(exponent - 1)
  }
}

fn maximum_mint_subtree_root(start: Int, size: Int) -> ByteArray {
  if size == 1 {
    validation_machine_v1.mint_asset_leaf_hash(
      h_28_a,
      bytearray.from_int_big_endian(start, 32),
      1,
    )
  } else {
    let half = size / 2
    validation_merkle_v1.hash_branch(
      maximum_mint_subtree_root(start, half),
      maximum_mint_subtree_root(start + half, half),
    )
  }
}

fn maximum_mint_last_asset_siblings(level: Int) -> List<ByteArray> {
  if level == 7 {
    []
  } else {
    let sibling_size = test_power_of_two(level)
    [
      maximum_mint_subtree_root(128 - 2 * sibling_size, sibling_size),
      ..maximum_mint_last_asset_siblings(level + 1)
    ]
  }
}

fn singleton_root(key: ByteArray, value: ByteArray) -> ByteArray {
  combine(suffix(blake2b_256(key), 0), blake2b_256(value))
}

fn canonical_step_fixture() -> Pair<
  validation_trace_v1.ValidationMachineStateV1,
  validation_machine_v1.ValidationOneStepEvidenceV1,
> {
  let tx = empty_v1_transaction()
  let context_cbor = validation_context_cbor
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  // A field scan that has not yet counted the field's items carries the
  // `item_count == -1` sentinel; `verify_canonical_decode_empty` requires it.
  let work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    context_cbor,
    0,
    0,
    0,
    -1,
    0,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 0,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      0,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_a,
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      1,
      validation_machine_v1.encode_transaction_field_scan_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        context_cbor,
        1,
        0,
        0,
        -1,
        0,
      ),
    ),
  }
  Pair(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
  )
}

test canonical_v1_decode_is_independently_verified_on_l1() {
  let Pair(pre, evidence) = canonical_step_fixture()
  validation_machine_v1.verify_canonical_decode_one_step_v1(
    pre,
    evidence,
    inline_field_door(),
  )
}

test canonical_decode_authenticates_one_bounded_script_item() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"010203",
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let item_cbor = encode_midgard_versioned_script(script)
  let work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    6,
    0,
    0,
    -1,
    0,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      7,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      8,
      validation_machine_v1.encode_transaction_field_scan_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        7,
        0,
        0,
        -1,
        0,
      ),
    ),
  }
  validation_machine_v1.verify_canonical_decode_one_step_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      // §8.1 tier 1: the field-6 preimage travels in the evidence and the door
      // hashes it once against `script_tx_wits_hash`. The retired form asserted
      // a per-item collection opening against that same flat hash, which is the
      // unsatisfiable predicate #592 is about.
      auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
        field_index: 6,
        item_index: 0,
        carriage: inline_field_carriage([item_cbor]),
      },
    },
    inline_field_door(),
  )
}

test canonical_decode_rejects_a_legacy_field_seven_script_proof() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"010203",
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let item_cbor = encode_midgard_versioned_script(script)
  let work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    6,
    0,
    0,
    -1,
    0,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      7,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      8,
      validation_machine_v1.encode_transaction_field_scan_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        7,
        0,
        0,
        -1,
        0,
      ),
    ),
  }
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let honest_carriage = inline_field_carriage([item_cbor])
  // C29, rewritten for #592. The substitution is now refused by the machine's
  // own field-index check rather than by domain-separated hashing. §4 removed
  // field-index domain separation — two fields holding the same items commit to
  // the same 32 bytes and a flat hash says nothing about which slot it came
  // from — so §8's door authenticates whichever slot the step's *control* names
  // and it is `field_index == control.field_index` inside
  // `verify_canonical_decode_chunk` that refuses evidence pointing at the
  // retired field-7 slot. Every conjunct below is pinned to its own measured
  // disposition, as the earlier `test ... fail` form could not be.
  and {
    // The substitution is a real one and not a collision: this transaction's
    // field-6 preimage does not commit to the same 32 bytes as its (empty)
    // field-7 slot.
    native_tx_field_access_v1.field_commitment(
      native_tx_field_access_v1.encode_field_preimage([item_cbor]),
    ) != native_tx_field_access_v1.empty_field_commitment,
    // Field-7-flavoured evidence does not authenticate a field-6 item on the
    // semantic path ...
    !validation_machine_v1.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      7,
      0,
      honest_carriage,
    ),
    // ... nor on the deployed one-step path.
    !validation_machine_v1.verify_canonical_decode_one_step_v1(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition,
        auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
          field_index: 7,
          item_index: 0,
          carriage: honest_carriage,
        },
      },
      inline_field_door(),
    ),
    // And an item index the authenticated field does not have is refused for
    // the same reason: the index is checked against the control's cursor, never
    // taken from the evidence.
    !validation_machine_v1.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      6,
      1,
      honest_carriage,
    ),
  }
}

// V-9 (#532). This was `test ... fail` over a single call to
// `verify_canonical_decode_one_step_v1`, so it went green as soon as any guard
// anywhere under that call was false and the name could not establish that the
// claimed successor is what fired. Single-conjunct mutation controls (each
// conjunct neutralized to `True` in turn, aiken-fork v1.1.23, this exact
// selector):
//
//   verify_canonical_decode_one_step_v1
//     1  pre.phase == CanonicalDecode                       -> pass
//     2  structural_transition_is_valid(pre, transition)    -> pass
//     3  verify_canonical_decode(pre, transition, aux)      -> FAIL
//
//   verify_canonical_decode_chunk (reached through conjunct 3)
//     1..8   control binding, non-empty field commitment, first/continuing
//            chunk, positive authenticated item count, the evidence's
//            field/item index equalities, the chunk-index bound  -> all pass
//     9      the claimed-successor exactness branch                -> FAIL
//
// (Conjunct numbering re-measured after #592: the retired `verify_item` /
// `verify_chunk` openings and the four proof length/commitment equalities they
// needed are gone, so the arm is shorter. The disposition is unchanged.)
//
// So the structural transition IS valid here — the vector differs from the
// passing sibling `canonical_decode_authenticates_one_bounded_script_item` in
// nothing but the claimed successor, and conjunct 9 of the chunk arm (the
// claimed-successor exactness branch, as the control above measures) is the sole
// falsifier. The dispositions are now pinned individually below, with that
// sibling's correct successor as the differential.
test canonical_decode_rejects_a_wrong_claimed_successor() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"010203",
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let item_cbor = encode_midgard_versioned_script(script)
  let work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    6,
    0,
    0,
    -1,
    0,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      7,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let rejection_code = #"455f4649454c445f505245494d4147455f53495a45"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      8,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  // The exact successor the sibling positive test claims for this same pre,
  // work witness, and auxiliary: the field-6 scan advancing to field 7.
  let correct_successor = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      8,
      validation_machine_v1.encode_transaction_field_scan_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        7,
        0,
        0,
        -1,
        0,
      ),
    ),
  }
  let carriage = inline_field_carriage([item_cbor])
  let auxiliary = validation_machine_v1.TransactionFieldChunkWitness {
    field_index: 6,
    item_index: 0,
    carriage,
  }
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: terminal,
  }
  let correct_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: correct_successor,
  }
  and {
    // Conjunct 1 of the one-step guard holds.
    pre.phase == validation_trace_v1.CanonicalDecode,
    // Conjunct 2 holds: the rejected successor is a structurally valid
    // successor state, so nothing about well-formedness, the work-root
    // binding, the program counter, the immutable context, the phase
    // succession, or the budget is what rejects it.
    validation_machine_v1.structural_transition_is_valid(pre, transition),
    // Conjunct 3 is the sole falsifier, and it falsifies through the chunk arm.
    !validation_machine_v1.verify_canonical_decode_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition,
        auxiliary,
      },
      inline_field_door(),
    ),
    !validation_machine_v1.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      6,
      0,
      carriage,
    ),
    !validation_machine_v1.verify_canonical_decode_one_step_v1(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition,
        auxiliary,
      },
      inline_field_door(),
    ),
    // Differential: the identical pre, work witness and §8 carriage authenticate
    // once the claimed successor is the exact one, so the claimed successor is
    // the only thing this vector rejects.
    validation_machine_v1.verify_canonical_decode_one_step_v1(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: correct_transition,
        auxiliary,
      },
      inline_field_door(),
    ),
  }
}

test compact_binding_is_independently_verified_on_l1() {
  let tx = empty_v1_transaction()
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let transaction_commitment = midgard_transaction_proof_commitment_v1(tx)
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let context_cbor = validation_context_cbor
  let compact_witness_cbor = validation_machine_v1.encode_compact_binding_witness(
    tx_id,
    transaction_commitment,
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    context_cbor,
  )
  let compact_pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment,
    validation_context_hash: validation_trace_v1.hash_validation_context(
      context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.CompactBinding,
    program_counter: 9,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CompactBinding,
      9,
      compact_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_a,
  }
  let input_post = validation_trace_v1.ValidationMachineStateV1 {
    ..compact_pre,
    phase: validation_trace_v1.StaticLedgerRules,
    program_counter: compact_pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.StaticLedgerRules,
      compact_pre.program_counter + 1,
      validation_machine_v1.encode_static_rules_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        context_cbor,
      ),
    ),
  }
  validation_machine_v1.verify_one_step(
    compact_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: compact_witness_cbor,
      claimed_successor: input_post,
    },
    inline_field_door(),
  )
}

test static_rules_accept_the_exact_proof_context_and_advance_to_input_sets() {
  let tx = empty_v1_transaction()
  let compact = midgard_transaction_to_compact(tx)
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = validation_machine_v1.encode_static_rules_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.StaticLedgerRules,
    program_counter: 2,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.StaticLedgerRules,
      2,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_a,
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.InputSets,
    program_counter: 3,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.InputSets,
      3,
      validation_machine_v1.encode_input_sets_scan_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        0,
        0,
        0,
        0,
        #"",
        validation_machine_v1.empty_resolution_schedule_hash(),
      ),
    ),
  }
  validation_machine_v1.verify_static_ledger_rules_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
  )
}

test static_rules_prove_a_network_mismatch_is_an_exact_no_op() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id: 1 },
  }
  let compact = midgard_transaction_to_compact(tx)
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = validation_machine_v1.encode_static_rules_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
  )
  let rejection_code = #"455f4e4554574f524b5f49445f4d49534d41544348"
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.StaticLedgerRules,
    program_counter: 2,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.StaticLedgerRules,
      2,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    // Deliberately a realistic NON-EMPTY claimed delta root. This test was RED
    // on a production defect: `rejected_successor_is_exact` demanded
    // `post.ledger_delta_root == frontier_commitment(0, [])` while
    // `immutable_context_matches` (reached via `structural_transition_is_valid`,
    // which the `verify_one_step` entry point applies) demands
    // `pre.ledger_delta_root == post.ledger_delta_root`. Those were jointly
    // unsatisfiable for any `pre` whose delta root was not already the empty
    // commitment -- i.e. for every real transaction. The clearing clause has
    // been deleted from production. Do NOT make any rejection test green by
    // setting `pre.ledger_delta_root` to `frontier_commitment(0, [])`: that is
    // the workaround that hid the defect, and it exercises the one pre-state in
    // which the defect is invisible.
    ledger_delta_root: non_empty_claimed_delta_root(),
  }
  // An exact rejection carries the claimed ledger delta forward unchanged;
  // `exact_rejection_post` is the canonical successor shape.
  let terminal = exact_rejection_post(pre, rejection_code)
  validation_machine_v1.verify_one_step(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: terminal,
    },
    inline_field_door(),
  )
}

// Shared StaticLedgerRules fixture for the delta-root controls below. `valid`
// selects between a network-conformant transaction (whose true one-step is an
// advance) and a network-mismatched one (whose true one-step is a rejection).
// `claimed_delta_root` is the operator's immutable claimed delta commitment.
fn static_rules_delta_root_fixture(
  valid: Bool,
  claimed_delta_root: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let base = empty_v1_transaction()
  let tx = if valid {
    base
  } else {
    MidgardTransaction {
      ..base,
      body: MidgardTransactionBody { ..base.body, network_id: 1 },
    }
  }
  let compact = midgard_transaction_to_compact(tx)
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = validation_machine_v1.encode_static_rules_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.StaticLedgerRules,
      program_counter: 2,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.StaticLedgerRules,
        2,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: claimed_delta_root,
    },
    work_witness_cbor,
  )
}

const network_mismatch_rejection_code: ByteArray =
  #"455f4e4554574f524b5f49445f4d49534d41544348"

// POSITIVE CONTROL for the restored capability (GOAL_SPEC §3 invariant 9).
// This is the governing adversarial case: an operator commits an `Accepted`
// descriptor -- mandatory for every normal L2 source -- over a transaction that
// truly rejects, and therefore carries a real, NON-EMPTY claimed delta root.
// Before the fix no challenger could construct any winning rejection successor
// from such a pre-state, so a dishonest operator won every dispute by default.
// The test also pins that the claim is carried forward, not cleared.
test rejection_is_provable_from_a_non_empty_claimed_delta_root() {
  let claimed = non_empty_claimed_delta_root()
  let Pair(pre, work_witness_cbor) = static_rules_delta_root_fixture(
    False,
    claimed,
  )
  let terminal = exact_rejection_post(pre, network_mismatch_rejection_code)
  and {
    claimed != validation_merkle_v1.frontier_commitment(0, []),
    terminal.ledger_delta_root == claimed,
    validation_machine_v1.verify_one_step(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: terminal,
      },
      inline_field_door(),
    ),
  }
}

// NEGATIVE CONTROL -- mutation (clearing). The old, deleted production clause
// demanded exactly this successor shape. It must now be REJECTED, because the
// claimed delta root is immutable context. This is the control that stops
// anyone "restoring" the clearing clause, or exempting the field from
// `immutable_context_matches`, without turning a test red.
test rejection_successor_that_clears_the_claimed_delta_is_rejected() {
  let Pair(pre, work_witness_cbor) = static_rules_delta_root_fixture(
    False,
    non_empty_claimed_delta_root(),
  )
  let cleared_terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..exact_rejection_post(pre, network_mismatch_rejection_code),
    ledger_delta_root: validation_merkle_v1.frontier_commitment(0, []),
  }
  !validation_machine_v1.verify_one_step(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: cleared_terminal,
    },
    inline_field_door(),
  )
}

// NEGATIVE CONTROL -- mutation (rewriting). Immutability holds in both
// directions: the successor may not substitute any other delta root either.
test rejection_successor_that_rewrites_the_claimed_delta_is_rejected() {
  let Pair(pre, work_witness_cbor) = static_rules_delta_root_fixture(
    False,
    non_empty_claimed_delta_root(),
  )
  let rewritten_terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..exact_rejection_post(pre, network_mismatch_rejection_code),
    ledger_delta_root: h_b,
  }
  !validation_machine_v1.verify_one_step(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: rewritten_terminal,
    },
    inline_field_door(),
  )
}

// NEGATIVE CONTROL -- valid-block rejection (GOAL_SPEC §3 invariant 9). THE
// critical control for the delta-root fix: deleting the clearing clause must
// not have made honest blocks challengeable. Here the transaction satisfies the
// static ledger rules, so its true one-step is an advance to `InputSets`; a
// claimed rejection terminal -- otherwise perfectly well-formed, and carrying
// the claimed delta forward exactly as the now-passing rejection tests do --
// must still be unprovable.
test a_valid_static_rules_transition_cannot_be_proven_rejected() {
  let Pair(pre, work_witness_cbor) = static_rules_delta_root_fixture(
    True,
    non_empty_claimed_delta_root(),
  )
  !validation_machine_v1.verify_one_step(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(
        pre,
        network_mismatch_rejection_code,
      ),
    },
    inline_field_door(),
  )
}

// The ONE deliberately-empty claimed-delta case. An honest, self-consistent
// forced-invalid trace built by the TypeScript reference carries
// `hashMidgardValidationLedgerDeltaV1([]) == frontier_commitment(0, [])` on
// every state, so an empty claim is spec-legal in exactly this scenario. It is
// kept as a single labelled case rather than as the framing for the whole
// rejection surface: as the only framing it hid the defect, because it is the
// one pre-state in which the deleted clause was satisfiable.
test an_honest_forced_invalid_trace_may_carry_an_empty_claimed_delta() {
  let empty_claim = validation_merkle_v1.frontier_commitment(0, [])
  let Pair(pre, work_witness_cbor) = static_rules_delta_root_fixture(
    False,
    empty_claim,
  )
  let terminal = exact_rejection_post(pre, network_mismatch_rejection_code)
  and {
    pre.source_kind == validation_trace_v1.Forced,
    terminal.ledger_delta_root == empty_claim,
    validation_machine_v1.verify_one_step(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: terminal,
      },
      inline_field_door(),
    ),
  }
}

test input_sets_rejects_a_successor_in_the_wrong_phase() {
  let tx = empty_v1_transaction()
  let work_witness_cbor = input_sets_scan_witness(
    tx,
    0,
    0,
    0,
    0,
    #"",
    validation_machine_v1.empty_resolution_schedule_hash(),
  )
  let input_pre = input_sets_pre(tx, 2, work_witness_cbor)
  let static_post = validation_trace_v1.ValidationMachineStateV1 {
    ..input_pre,
    phase: validation_trace_v1.StaticLedgerRules,
    program_counter: 3,
    work_root: h_c,
  }
  !validation_machine_v1.verify_input_sets_empty_semantics_v1(
    input_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: static_post,
    },
  )
}

test input_sets_proves_an_empty_input_forced_transaction_is_an_exact_no_op() {
  let tx = empty_v1_transaction()
  let work_witness_cbor = input_sets_scan_witness(
    tx,
    0,
    0,
    0,
    0,
    #"",
    validation_machine_v1.empty_resolution_schedule_hash(),
  )
  let input_pre = input_sets_pre(tx, 2, work_witness_cbor)
  let rejection_code = #"455f454d5054595f494e50555453"
  validation_machine_v1.verify_input_sets_empty_semantics_v1(
    input_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(input_pre, rejection_code),
    },
  )
}

test input_sets_accepts_bounded_disjoint_spend_and_reference_inputs() {
  let spend_input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let reference_input = MidgardTxInput { tx_id: h_b, output_index: 1 }
  let spend_key = encode_midgard_tx_input(spend_input)
  let reference_key = encode_midgard_tx_input(reference_input)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [spend_input],
      reference_inputs: [reference_input],
    },
  }
  let initial_schedule = validation_machine_v1.empty_resolution_schedule_hash()
  let first_schedule = validation_machine_v1.resolution_schedule_node_hash(
    1,
    reference_key,
    initial_schedule,
  )
  let full_schedule = validation_machine_v1.resolution_schedule_node_hash(
    0,
    spend_key,
    first_schedule,
  )
  let first_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    -1,
    0,
    0,
    #"",
    initial_schedule,
  )
  let first_pre = input_sets_pre(tx, 2, first_witness_cbor)
  let second_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    1,
    0,
    1,
    reference_key,
    first_schedule,
  )
  let second_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..first_pre,
    program_counter: 3,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.InputSets,
      3,
      second_witness_cbor,
    ),
  }
  let reference_carriage = inline_field_carriage([reference_key])
  let first_is_valid = validation_machine_v1.verify_input_sets_item_semantics_v1(
    first_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: first_witness_cbor,
      claimed_successor: second_pre,
    },
    inline_field_door(),
    1,
    0,
    reference_carriage,
  )
  let signatures_witness_cbor = initial_signatures_witness(tx)
  let signatures_post = validation_trace_v1.ValidationMachineStateV1 {
    ..second_pre,
    phase: validation_trace_v1.Signatures,
    program_counter: 4,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      4,
      signatures_witness_cbor,
    ),
  }
  let spend_carriage = inline_field_carriage([spend_key])
  and {
    first_is_valid,
    validation_machine_v1.verify_input_sets_item_semantics_v1(
      second_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: second_witness_cbor,
        claimed_successor: signatures_post,
      },
      inline_field_door(),
      0,
      0,
      spend_carriage,
    ),
    full_schedule == validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

test input_sets_proves_spend_reference_overlap_unilaterally() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [input],
      reference_inputs: [input],
    },
  }
  let initial_schedule = validation_machine_v1.empty_resolution_schedule_hash()
  let first_schedule = validation_machine_v1.resolution_schedule_node_hash(
    1,
    key,
    initial_schedule,
  )
  let first_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    -1,
    0,
    0,
    #"",
    initial_schedule,
  )
  let first_pre = input_sets_pre(tx, 2, first_witness_cbor)
  let second_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    1,
    0,
    1,
    key,
    first_schedule,
  )
  let second_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..first_pre,
    program_counter: 3,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.InputSets,
      3,
      second_witness_cbor,
    ),
  }
  let reference_carriage = inline_field_carriage([key])
  let spend_carriage = inline_field_carriage([key])
  let first_is_valid = validation_machine_v1.verify_input_sets_item_semantics_v1(
    first_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: first_witness_cbor,
      claimed_successor: second_pre,
    },
    inline_field_door(),
    1,
    0,
    reference_carriage,
  )
  let rejection_code = #"455f4455504c49434154455f494e5055545f494e5f5458"
  and {
    first_is_valid,
    validation_machine_v1.verify_input_sets_item_semantics_v1(
      second_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: second_witness_cbor,
        claimed_successor: exact_rejection_post(second_pre, rejection_code),
      },
      inline_field_door(),
      0,
      0,
      spend_carriage,
    ),
  }
}

test input_sets_proves_a_malformed_validity_interval_after_the_final_item() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [input],
      validity_interval_start: 10,
      validity_interval_end: 9,
    },
  }
  let work_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    0,
    0,
    0,
    #"",
    validation_machine_v1.empty_resolution_schedule_hash(),
  )
  let pre = input_sets_pre(tx, 2, work_witness_cbor)
  let carriage = inline_field_carriage([key])
  let rejection_code = #"455f494e56414c49445f56414c49444954595f494e54455256414c5f464f524d4154"
  validation_machine_v1.verify_input_sets_item_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(pre, rejection_code),
    },
    inline_field_door(),
    0,
    0,
    carriage,
  )
}

test signatures_accepts_an_empty_required_signer_and_witness_set() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
  }
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(tx, h_a)
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = validation_merkle_v1.build_frontier([])
  let required_witness_cbor = signatures_scan_witness(
    tx,
    1,
    0,
    0,
    0,
    0,
    #"",
    #"",
    signer_frontier,
    0,
  )
  let required_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 5,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      5,
      required_witness_cbor,
    ),
  }
  let handoff_witness_cbor = signatures_scan_witness(
    tx,
    2,
    0,
    0,
    0,
    0,
    #"",
    #"",
    signer_frontier,
    0,
  )
  let handoff_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..required_pre,
    program_counter: 6,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      6,
      handoff_witness_cbor,
    ),
  }
  let next_work_witness_cbor = validation_machine_v1.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    0,
    0,
    0,
    0,
    0,
    #"",
    0,
    #"",
    0,
    0,
    // A stage-0 phase-A native scan control MUST carry `result == -1`:
    // `phase_a_native_control_is_bound` rejects any other value at stage 0, and
    // `reset_phase_a_native_control` produces -1. Production
    // `verify_signatures_handoff` used to emit 0 here, which was a defect --
    // `result == 0` at a completed stage MEANS "native script evaluated false
    // -> reject", so the handoff asserted a verdict the machine had not
    // computed and produced an unusable successor. The emitter has been fixed;
    // do NOT "fix" this expectation to 0. See
    // `signatures_handoff_rejects_a_stage_zero_control_claiming_a_verdict`.
    -1,
    signer_frontier.count,
    signer_frontier.peaks,
    #"",
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..handoff_pre,
    phase: validation_trace_v1.PhaseANativeScripts,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      7,
      next_work_witness_cbor,
    ),
  }
  and {
    validation_machine_v1.verify_signatures_advance_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: required_pre,
      },
    ),
    validation_machine_v1.verify_signatures_advance_semantics_v1(
      required_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: required_witness_cbor,
        claimed_successor: handoff_pre,
      },
    ),
    validation_machine_v1.verify_signatures_handoff_semantics_v1(
      handoff_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: handoff_witness_cbor,
        claimed_successor: post,
      },
    ),
  }
}

// Builds the Signatures->PhaseANativeScripts handoff, claiming a successor
// whose stage-0 phase-A native scan control carries the supplied `result`.
fn signatures_handoff_with_claimed_result(result: Int) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
  }
  let Pair(pre, _work_witness_cbor) = signatures_step_fixture(tx, h_a)
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = validation_merkle_v1.build_frontier([])
  let handoff_witness_cbor = signatures_scan_witness(
    tx,
    2,
    0,
    0,
    0,
    0,
    #"",
    #"",
    signer_frontier,
    0,
  )
  let handoff_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 6,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      6,
      handoff_witness_cbor,
    ),
  }
  let next_work_witness_cbor = validation_machine_v1.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    0,
    0,
    0,
    0,
    0,
    #"",
    0,
    #"",
    0,
    0,
    result,
    signer_frontier.count,
    signer_frontier.peaks,
    #"",
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..handoff_pre,
    phase: validation_trace_v1.PhaseANativeScripts,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      7,
      next_work_witness_cbor,
    ),
  }
  validation_machine_v1.verify_signatures_handoff_semantics_v1(
    handoff_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: handoff_witness_cbor,
      claimed_successor: post,
    },
  )
}

// NEGATIVE CONTROL -- mutation (GOAL_SPEC §3 invariant 9) for the sentinel fix.
// `result == 0` at a completed stage MEANS "this native script evaluated false
// -> reject" (`rejected_successor_is_exact` is invoked on it), and `result == 1`
// means "evaluated true". Neither is a legal stage-0 value: nothing has been
// evaluated yet. Both must be REJECTED. This is the control proving the defect
// was fixed at the EMITTER (`verify_signatures_handoff` now emits -1) and not
// by relaxing `phase_a_native_control_is_bound` to accept 0 -- a relaxation
// would diverge from the TypeScript reference (invariant 8) and make two
// encodings of the same semantic state both bindable.
test signatures_handoff_rejects_a_stage_zero_control_claiming_a_verdict() {
  and {
    !signatures_handoff_with_claimed_result(0),
    !signatures_handoff_with_claimed_result(1),
  }
}

// POSITIVE pair for the control above: the sentinel is what production emits.
test signatures_handoff_emits_the_not_yet_evaluated_sentinel() {
  signatures_handoff_with_claimed_result(-1)
}

test signatures_proves_a_missing_required_signer_is_an_exact_no_op() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      required_signers: [h_28_a],
    },
  }
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(
    tx,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let signer_frontier = validation_merkle_v1.build_frontier([])
  let required_witness_cbor = signatures_scan_witness(
    tx,
    1,
    0,
    -1,
    0,
    0,
    #"",
    #"",
    signer_frontier,
    0,
  )
  let required_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 5,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      5,
      required_witness_cbor,
    ),
  }
  let rejection_code = #"455f4d495353494e475f52455155495245445f5749544e455353"
  and {
    validation_machine_v1.verify_signatures_advance_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: required_pre,
      },
    ),
    validation_machine_v1.verify_required_signer_item_semantics_v1(
      required_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: required_witness_cbor,
        claimed_successor: exact_rejection_post(required_pre, rejection_code),
      },
      inline_field_door(),
      inline_field_carriage([h_28_a]),
      validation_machine_v1.EmptySignerSetProof { peaks: [] },
    ),
  }
}

test signatures_proves_an_invalid_address_signature_is_an_exact_no_op() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: [
        MidgardAddressWitness {
          verification_key: zero,
          signature: zero |> bytearray.concat(zero),
        },
      ],
    },
  }
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(
    tx,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let address_witness = MidgardAddressWitness {
    verification_key: zero,
    signature: zero |> bytearray.concat(zero),
  }
  let address_witness_cbor = encode_midgard_address_witness(address_witness)
  let signer_hash = blake2b_224(address_witness.verification_key)
  let signer_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.signer_leaf_hash(signer_hash)],
  )
  let required_witness_cbor = signatures_scan_witness(
    tx,
    1,
    1,
    0,
    1,
    0,
    #"",
    #"",
    signer_frontier,
    1,
  )
  let required_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 5,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      5,
      required_witness_cbor,
    ),
  }
  let carriage = inline_field_carriage([address_witness_cbor])
  let rejection_code = #"455f494e56414c49445f5349474e4154555245"
  and {
    validation_machine_v1.verify_signature_address_item_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: required_pre,
      },
      inline_field_door(),
      7,
      0,
      carriage,
    ),
    validation_machine_v1.verify_signatures_advance_semantics_v1(
      required_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: required_witness_cbor,
        claimed_successor: exact_rejection_post(required_pre, rejection_code),
      },
    ),
  }
}

test phase_a_native_scripts_evaluates_a_valid_timelock_on_l1() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      validity_interval_start: 100,
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [
        MidgardVersionedScript {
          language: NativeCardanoScript,
          script_bytes: #"82041864",
        },
      ],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_native_step_fixture(tx, h_a)
  let item_cbor = encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: #"82041864",
    },
  )
  let carriage = inline_field_carriage([item_cbor])
  let chunk_proof = singleton_field_item_chunk_proof(6, item_cbor)
  let token_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    3,
    #"",
    0,
    0,
    -1,
  )
  let token_pre = next_phase_a_native_state(pre, token_work_witness_cbor)
  let payload_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    7,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    5,
    #"",
    0,
    1,
    -1,
  )
  let payload_pre = next_phase_a_native_state(
    token_pre,
    payload_work_witness_cbor,
  )
  let finalize_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    2,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    bytearray.length(item_cbor),
    #"",
    0,
    1,
    1,
  )
  let finalize_pre = next_phase_a_native_state(
    payload_pre,
    finalize_work_witness_cbor,
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let next_work_witness_cbor = validation_machine_v1.encode_phase_a_script_preconditions_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    False,
    0,
    0,
    #"",
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..finalize_pre,
    phase: validation_trace_v1.PhaseAScriptPreconditions,
    program_counter: finalize_pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseAScriptPreconditions,
      finalize_pre.program_counter + 1,
      next_work_witness_cbor,
    ),
  }
  and {
    validation_machine_v1.verify_phase_a_native_item_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: token_pre,
      },
      inline_field_door(),
      6,
      0,
      carriage,
    ),
    validation_machine_v1.verify_phase_a_native_token_head_semantics_v1(
      token_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: token_work_witness_cbor,
        claimed_successor: payload_pre,
      },
      chunk_proof,
      None,
    ),
    validation_machine_v1.verify_phase_a_native_timelock_payload_semantics_v1(
      payload_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: payload_work_witness_cbor,
        claimed_successor: finalize_pre,
      },
      chunk_proof,
      None,
    ),
    validation_machine_v1.verify_phase_a_native_advance_semantics_v1(
      finalize_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: finalize_work_witness_cbor,
        claimed_successor: post,
      },
    ),
  }
}

test phase_a_native_scripts_resolves_a_nested_signature_one_step_at_a_time() {
  let base = empty_v1_transaction()
  let address_witness = MidgardAddressWitness {
    verification_key: zero,
    signature: zero |> bytearray.concat(zero),
  }
  let signer_hash = blake2b_224(address_witness.verification_key)
  let script_bytes = #"8201818200581c"
    |> bytearray.concat(signer_hash)
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes,
  }
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: [address_witness],
      script_tx_wits: [script],
    },
  }
  let Pair(pre, _initial_work_witness_cbor) = phase_a_native_step_fixture(
    tx,
    h_a,
  )
  let item_cbor = encode_midgard_versioned_script(script)
  let chunk_proof = singleton_field_item_chunk_proof(6, item_cbor)
  let payload_offset = bytearray.length(item_cbor) - bytearray.length(
    script_bytes,
  )
  let child_offset = payload_offset + 3
  let item_length = bytearray.length(item_cbor)
  let item_commitment = bounded_item_v1.from_bytes(6, 0, item_cbor)
  let container_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    item_length,
    item_commitment,
    payload_offset,
    #"",
    0,
    0,
    -1,
  )
  let container_pre = next_phase_a_native_state(
    pre,
    container_work_witness_cbor,
  )
  let container_payload_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    4,
    1,
    0,
    0,
    item_length,
    item_commitment,
    payload_offset + 2,
    #"",
    0,
    1,
    -1,
  )
  let container_payload_pre = next_phase_a_native_state(
    container_pre,
    container_payload_work_witness_cbor,
  )
  let frame = native_script_scan_v1.NativeScriptFrameV1 {
    tail: #"",
    kind: native_script_scan_v1.all_node,
    child_count: 1,
    remaining: 1,
    valid_count: 0,
    required: 0,
  }
  let stack_root = native_script_scan_v1.hash_frame_v1(frame)
  let signature_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    item_length,
    item_commitment,
    child_offset,
    stack_root,
    1,
    1,
    -1,
  )
  let signature_pre = next_phase_a_native_state(
    container_payload_pre,
    signature_work_witness_cbor,
  )
  let signature_payload_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    3,
    1,
    0,
    0,
    item_length,
    item_commitment,
    child_offset + 2,
    stack_root,
    1,
    2,
    -1,
  )
  let signature_payload_pre = next_phase_a_native_state(
    signature_pre,
    signature_payload_work_witness_cbor,
  )
  let frame_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    2,
    1,
    0,
    0,
    item_length,
    item_commitment,
    item_length,
    stack_root,
    1,
    2,
    1,
  )
  let frame_pre = next_phase_a_native_state(
    signature_payload_pre,
    frame_work_witness_cbor,
  )
  let complete_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    2,
    1,
    0,
    0,
    item_length,
    item_commitment,
    item_length,
    #"",
    0,
    2,
    1,
  )
  let complete_pre = next_phase_a_native_state(
    frame_pre,
    complete_work_witness_cbor,
  )
  let signer_frontier = signer_frontier_for_tx(tx)
  and {
    validation_machine_v1.verify_phase_a_native_token_head_semantics_v1(
      container_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: container_work_witness_cbor,
        claimed_successor: container_payload_pre,
      },
      chunk_proof,
      None,
    ),
    validation_machine_v1.verify_phase_a_native_all_or_any_container_frame_payload_semantics_v1(
      container_payload_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: container_payload_work_witness_cbor,
        claimed_successor: signature_pre,
      },
      chunk_proof,
      None,
    ),
    validation_machine_v1.verify_phase_a_native_token_head_semantics_v1(
      signature_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: signature_work_witness_cbor,
        claimed_successor: signature_payload_pre,
      },
      chunk_proof,
      None,
    ),
    validation_machine_v1.verify_phase_a_native_signature_membership_payload_semantics_v1(
      signature_payload_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: signature_payload_work_witness_cbor,
        claimed_successor: frame_pre,
      },
      chunk_proof,
      None,
      signer_frontier.peaks,
      0,
      [],
    ),
    validation_machine_v1.verify_phase_a_native_frame_semantics_v1(
      frame_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: frame_work_witness_cbor,
        claimed_successor: complete_pre,
      },
      frame,
    ),
  }
}

test phase_a_native_scripts_proves_an_unsatisfied_script_is_an_exact_no_op() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [
        MidgardVersionedScript {
          language: NativeCardanoScript,
          script_bytes: #"8200581c"
            |> bytearray.concat(h_28_a),
        },
      ],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_native_step_fixture(
    tx,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let item_cbor = encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: #"8200581c"
        |> bytearray.concat(h_28_a),
    },
  )
  let carriage = inline_field_carriage([item_cbor])
  let chunk_proof = singleton_field_item_chunk_proof(6, item_cbor)
  let token_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    4,
    #"",
    0,
    0,
    -1,
  )
  let token_pre = next_phase_a_native_state(pre, token_work_witness_cbor)
  let payload_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    3,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    6,
    #"",
    0,
    1,
    -1,
  )
  let payload_pre = next_phase_a_native_state(
    token_pre,
    payload_work_witness_cbor,
  )
  let finalize_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    2,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    bytearray.length(item_cbor),
    #"",
    0,
    1,
    0,
  )
  let finalize_pre = next_phase_a_native_state(
    payload_pre,
    finalize_work_witness_cbor,
  )
  let rejection_code = #"455f4e41544956455f5343524950545f494e56414c4944"
  // An exact rejection also clears the ledger delta to the empty frontier
  // commitment; `exact_rejection_post` is the canonical successor shape.
  let terminal = exact_rejection_post(finalize_pre, rejection_code)
  and {
    validation_machine_v1.verify_phase_a_native_item_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: token_pre,
      },
      inline_field_door(),
      6,
      0,
      carriage,
    ),
    validation_machine_v1.verify_phase_a_native_token_head_semantics_v1(
      token_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: token_work_witness_cbor,
        claimed_successor: payload_pre,
      },
      chunk_proof,
      None,
    ),
    validation_machine_v1.verify_phase_a_native_signature_empty_payload_semantics_v1(
      payload_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: payload_work_witness_cbor,
        claimed_successor: finalize_pre,
      },
      chunk_proof,
      None,
      [],
    ),
    validation_machine_v1.verify_phase_a_native_advance_semantics_v1(
      finalize_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: finalize_work_witness_cbor,
        claimed_successor: terminal,
      },
    ),
  }
}

test phase_a_script_preconditions_advances_an_empty_bundle_to_resolution() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_script_preconditions_fixture(
    tx,
    h_a,
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.ResolveInputs,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      7,
      validation_machine_v1.encode_resolve_inputs_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        0,
        validation_machine_v1.initial_resolution_accumulator(),
        validation_machine_v1.transaction_resolution_schedule_hash(
          tx.body.inputs,
          tx.body.reference_inputs,
        ),
        signer_frontier.count,
        validation_merkle_v1.frontier_commitment(
          signer_frontier.count,
          signer_frontier.peaks,
        ),
        None,
        validation_machine_v1.transaction_resolution_schedule_hash(
          tx.body.inputs,
          tx.body.reference_inputs,
        ),
      ),
    ),
  }
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
}

test phase_a_script_preconditions_proves_duplicate_observers_are_a_no_op() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      required_observers: [h_28_a, h_28_a],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_script_preconditions_fixture(
    tx,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let next_work_witness_cbor = validation_machine_v1.encode_phase_a_script_preconditions_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    contains_non_native_script(tx.witness_set.script_tx_wits),
    2,
    1,
    h_28_a,
  )
  let next = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.PhaseAScriptPreconditions,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseAScriptPreconditions,
      7,
      next_work_witness_cbor,
    ),
  }
  let rejection_code = #"455f494e56414c49445f4649454c445f54595045"
  let terminal = exact_rejection_post(next, rejection_code)
  // The duplicate is now the field's own §5.1 preimage rather than two
  // openings over one frontier, and which of its two identical items a step
  // reads is the `item_index` it names.
  let observers_carriage = duplicate_observer_field_carriage()
  let first_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: next,
    },
    auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
      field_index: 3,
      item_index: 0,
      carriage: observers_carriage,
    },
  }
  let second_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: next_work_witness_cbor,
      claimed_successor: terminal,
    },
    auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
      field_index: 3,
      item_index: 1,
      carriage: observers_carriage,
    },
  }
  and {
    validation_machine_v1.verify_one_step_evidence(
      pre,
      first_evidence,
      inline_field_door(),
    ),
    validation_machine_v1.verify_one_step_evidence(
      next,
      second_evidence,
      inline_field_door(),
    ),
  }
}

test phase_a_script_preconditions_require_integrity_for_plutus_bytes() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [
        MidgardVersionedScript {
          language: PlutusV3Script,
          script_bytes: v1_program_envelope(),
        },
      ],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_script_preconditions_fixture(
    tx,
    non_empty_claimed_delta_root(),
  )
  let rejection_code = #"455f494e56414c49445f4649454c445f54595045"
  // Canonical exact-rejection successor: it carries the non-empty claimed delta
  // forward. See the note on
  // `static_rules_prove_a_network_mismatch_is_an_exact_no_op`.
  let terminal = exact_rejection_post(pre, rejection_code)
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: terminal,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
}

test resolve_inputs_checks_validity_before_ledger_lookups() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
  }
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    h_c,
    0,
    accumulator,
    7,
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let resolution_schedule_hash = validation_machine_v1.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let next_work_witness_cbor = validation_machine_v1.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    1,
    accumulator,
    resolution_schedule_hash,
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    None,
    resolution_schedule_hash,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      8,
      next_work_witness_cbor,
    ),
  }
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
}

fn resolve_inputs_authenticated_membership_step(step_to_verify: Int) -> Bool {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let output_cbor = #"a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0"
  let material = authenticated_output_material(output_cbor)
  let descriptor_cbor = material.descriptor_cbor
  let root = singleton_root(key, descriptor_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [input] },
  }
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    root,
    1,
    accumulator,
    8,
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let empty_schedule_hash = validation_machine_v1.empty_resolution_schedule_hash()
  let signer_frontier_commitment = validation_merkle_v1.frontier_commitment(
    signer_frontier.count,
    signer_frontier.peaks,
  )
  let pending_initial = validation_machine_v1.ResolveInputOutputProofV1 {
    source_kind: 0,
    key,
    next_schedule_hash: empty_schedule_hash,
    descriptor_cbor,
    output_proof: material.initial,
  }
  let begin_post_cbor = validation_machine_v1.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    1,
    accumulator,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    signer_frontier_commitment,
    Some(pending_initial),
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  let begin_post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 9,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      9,
      begin_post_cbor,
    ),
  }
  let pending_first_successor = validation_machine_v1.ResolveInputOutputProofV1 {
    ..pending_initial,
    output_proof: material.first_successor,
  }
  let step_post_cbor = validation_machine_v1.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    1,
    accumulator,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    signer_frontier_commitment,
    Some(pending_first_successor),
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  let step_post = validation_trace_v1.ValidationMachineStateV1 {
    ..begin_post,
    program_counter: 10,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      10,
      step_post_cbor,
    ),
  }
  let pending_terminal = validation_machine_v1.ResolveInputOutputProofV1 {
    ..pending_initial,
    output_proof: material.terminal,
  }
  let finalize_pre_cbor = validation_machine_v1.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    1,
    accumulator,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    signer_frontier_commitment,
    Some(pending_terminal),
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  let finalize_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..step_post,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      10,
      finalize_pre_cbor,
    ),
  }
  let next_accumulator = validation_machine_v1.resolved_input_accumulator_successor(
    accumulator,
    0,
    key,
    descriptor_cbor,
  )
  let finalize_post_cbor = validation_machine_v1.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    2,
    next_accumulator,
    empty_schedule_hash,
    signer_frontier.count,
    signer_frontier_commitment,
    None,
    validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  let finalize_post = validation_trace_v1.ValidationMachineStateV1 {
    ..finalize_pre,
    program_counter: 11,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      11,
      finalize_post_cbor,
    ),
  }
  if step_to_verify == 0 {
    validation_machine_v1.verify_resolve_inputs_membership_begin_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: begin_post,
      },
      0,
      key,
      empty_schedule_hash,
      descriptor_cbor,
      [],
      validation_machine_v1.NoSignerSetProof,
    )
  } else if step_to_verify == 1 {
    validation_machine_v1.verify_resolve_inputs_membership_step_semantics_v1(
      begin_post,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: begin_post_cbor,
        claimed_successor: step_post,
      },
      ledger_output_proof_v1.LedgerOutputProofChunks {
        chunk_proof: material.chunk_proof,
        next_chunk_proof: None,
      },
    )
  } else {
    and {
      step_to_verify == 2,
      validation_machine_v1.verify_resolve_inputs_membership_finalize_semantics_v1(
        finalize_pre,
        validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: finalize_pre_cbor,
          claimed_successor: finalize_post,
        },
        descriptor_cbor,
        validation_machine_v1.NoSignerSetProof,
      ),
    }
  }
}

test resolve_inputs_begins_authenticated_membership_output() {
  resolve_inputs_authenticated_membership_step(0)
}

test resolve_inputs_empty_pending_witness_decodes_as_exact_array_11() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [input] },
  }
  let Pair(_, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    null_hash,
    1,
    validation_machine_v1.initial_resolution_accumulator(),
    8,
  )
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let fields = builtin.un_list_data(data)
  expect Some(pending_data) = list.at(fields, 9)
  expect Some(schedule_data) = list.at(fields, 10)
  and {
    list.length(fields) == 11,
    builtin.un_b_data(pending_data) == #"00",
    builtin.un_b_data(schedule_data) == validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

test resolve_inputs_streams_authenticated_membership_output() {
  resolve_inputs_authenticated_membership_step(1)
}

test resolve_inputs_finalizes_authenticated_membership_output() {
  resolve_inputs_authenticated_membership_step(2)
}

test resolve_inputs_proves_non_membership_as_an_exact_no_op() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [input] },
  }
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    null_hash,
    1,
    accumulator,
    8,
  )
  let rejection_code = #"455f494e5055545f4e4f545f464f554e44"
  // Canonical exact-rejection successor: it carries the non-empty claimed delta
  // (supplied by `resolve_inputs_step_fixture`) forward. See the note on
  // `static_rules_prove_a_network_mismatch_is_an_exact_no_op`.
  let terminal = exact_rejection_post(pre, rejection_code)
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: terminal,
      },
      auxiliary: validation_machine_v1.ScheduledLedgerNonMembershipWitness {
        source_kind: 0,
        key,
        next_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
        proof: [],
      },
    },
    inline_field_door(),
  )
}

test resolve_inputs_rejects_an_invalid_validity_interval_exactly() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      validity_interval_start: 101,
    },
  }
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    h_c,
    0,
    accumulator,
    7,
  )
  let rejection_code = #"455f56414c49444954595f494e54455256414c5f4d49534d41544348"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      8,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: terminal,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
}

test resolve_inputs_finalizes_into_script_source_resolution() {
  let tx = empty_v1_transaction()
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    h_c,
    1,
    accumulator,
    8,
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.ScriptSources,
    program_counter: 9,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      9,
      validation_machine_v1.encode_script_sources_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        0,
        accumulator,
        signer_frontier.count,
        validation_merkle_v1.frontier_commitment(
          signer_frontier.count,
          signer_frontier.peaks,
        ),
        validation_merkle_v1.empty_frontier(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        validation_machine_v1.initial_resolution_accumulator(),
        validation_machine_v1.empty_resolution_schedule_hash(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        validation_machine_v1.empty_receive_purpose_scan_control(),
        0,
        0,
        validation_machine_v1.empty_observer_purpose_scan_control(),
        validation_machine_v1.empty_mint_fold_control(),
        validation_machine_v1.transaction_resolution_schedule_hash(
          tx.body.inputs,
          tx.body.reference_inputs,
        ),
      ),
    ),
  }
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
}

test script_sources_commits_inline_reference_script_candidates() {
  let base = empty_v1_transaction()
  let payload = v1_program_envelope()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: payload,
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.source_leaf_hash(0, #"00", script)],
  )
  let item_cbor = encode_midgard_versioned_script(script)
  let carriage = inline_field_carriage([item_cbor])
  let pending = validation_machine_v1.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment: bounded_item_v1.from_bytes(6, 0, item_cbor),
    hash_control: blake2b_224_trace_v1.initial_control_v1(
      bytearray.length(payload) + 1,
    ),
  }
  let initial_control = stage_zero_script_sources_control(
    tx,
    empty_frontier,
    0,
    #"",
  )
  let pending_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..initial_control,
    source_total_count: 1,
    pending_source_cbor: validation_machine_v1.encode_inline_source_hash_control_v1(
      pending,
    ),
  }
  let committed_control = stage_zero_script_sources_control(
    tx,
    source_frontier,
    1,
    #"",
  )
  let final_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..committed_control,
    stage: 1,
  }
  let pre = stage_zero_script_sources_state(tx, initial_control, 9)
  let pending_post = stage_zero_script_sources_state(tx, pending_control, 10)
  let committed_pre = stage_zero_script_sources_state(tx, committed_control, 10)
  let final_post = stage_zero_script_sources_state(tx, final_control, 11)
  and {
    validation_machine_v1.verify_script_sources_stage_zero_begin_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: stage_zero_script_sources_work_witness(
          initial_control,
        ),
        claimed_successor: pending_post,
      },
      inline_field_door(),
      6,
      0,
      carriage,
    ),
    validation_machine_v1.verify_script_sources_stage_zero_finish_semantics_v1(
      committed_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: stage_zero_script_sources_work_witness(
          committed_control,
        ),
        claimed_successor: final_post,
      },
    ),
  }
}

test script_sources_commits_unique_supported_redeemers() {
  let base = empty_v1_transaction()
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"00",
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let redeemer_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.redeemer_leaf_hash(0, redeemer)],
  )
  let redeemer_cbor = encode_midgard_redeemer_witness(redeemer)
  let carriage = inline_field_carriage([redeemer_cbor])
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(8, 0, redeemer_cbor)
  // Since `320ed869` the stage-1 redeemer scan no longer commits the redeemer
  // leaf in a single step. `TransactionRedeemerItemBeginWitness` only opens the
  // item: it records `redeemer_total_count` and a pending
  // `redeemer_item_proof_v1` control hash (validation-machine-v1.ak:8580-8615).
  // A chain of `RedeemerItemStepWitness` steps then advances that control, and
  // only the terminal step appends the redeemer leaf and clears the pending
  // hash (:8637-8658). This test covers the open step, the first chained step,
  // and the stage-1 -> stage-2 finish from a completed scan.
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_data,
    0,
    1,
    bytearray.length(redeemer_cbor),
    redeemer_item_commitment,
    -1,
    -1,
  )
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    10,
  )
  let Pair(begin_post, begin_work_witness_cbor) = script_sources_pending_redeemer_item_fixture(
    tx,
    empty_frontier,
    1,
    redeemer_item_proof_v1.hash_control_v1(item_control),
    11,
  )
  let Pair(header_post, _) = script_sources_pending_redeemer_item_fixture(
    tx,
    empty_frontier,
    1,
    redeemer_item_proof_v1.hash_control_v1(tail_control),
    12,
  )
  // A stage-1 state after the scan completed: the redeemer leaf is committed
  // and no item proof is pending.
  let Pair(item_post, item_work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    empty_frontier,
    redeemer_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  let Pair(final_post, _) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    2,
    empty_frontier,
    redeemer_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    12,
  )
  let begin_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: begin_post,
  }
  let header_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: begin_work_witness_cbor,
    claimed_successor: header_post,
  }
  let final_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: item_work_witness_cbor,
    claimed_successor: final_post,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_one_finish_raw_semantics_v1(
      item_post,
      final_transition,
    ),
    validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: begin_transition,
        auxiliary: validation_machine_v1.TransactionRedeemerItemBeginWitness {
          carriage,
        },
      },
      inline_field_door(),
    ),
    validation_machine_v1.verify_one_step_evidence(
      begin_post,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: header_transition,
        auxiliary: validation_machine_v1.RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: header_witness,
        },
      },
      inline_field_door(),
    ),
    validation_machine_v1.verify_one_step_evidence(
      item_post,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: final_transition,
        auxiliary: validation_machine_v1.NoAuxiliaryWitness,
      },
      inline_field_door(),
    ),
  }
}

test script_sources_stage_one_redeemer_family_guard() {
  let base = empty_v1_transaction()
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"00",
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let redeemer_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.redeemer_leaf_hash(0, redeemer)],
  )
  let redeemer_cbor = encode_midgard_redeemer_witness(redeemer)
  let carriage = inline_field_carriage([redeemer_cbor])
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(8, 0, redeemer_cbor)
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_data,
    0,
    1,
    bytearray.length(redeemer_cbor),
    redeemer_item_commitment,
    -1,
    -1,
  )
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    10,
  )
  let Pair(begin_post, begin_work_witness_cbor) = script_sources_pending_redeemer_item_fixture(
    tx,
    empty_frontier,
    1,
    redeemer_item_proof_v1.hash_control_v1(item_control),
    11,
  )
  let Pair(header_post, _) = script_sources_pending_redeemer_item_fixture(
    tx,
    empty_frontier,
    1,
    redeemer_item_proof_v1.hash_control_v1(tail_control),
    12,
  )
  let Pair(item_post, item_work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    empty_frontier,
    redeemer_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  let Pair(final_post, _) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    2,
    empty_frontier,
    redeemer_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    12,
  )
  let begin_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: begin_post,
  }
  let header_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: begin_work_witness_cbor,
    claimed_successor: header_post,
  }
  let final_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: item_work_witness_cbor,
    claimed_successor: final_post,
  }
  let begin_auxiliary = validation_machine_v1.TransactionRedeemerItemBeginWitness {
    carriage,
  }
  let header_auxiliary = validation_machine_v1.RedeemerItemStepWitness {
    redeemer_control: None,
    control: item_control,
    witness: header_witness,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_one_redeemer_semantics_v1(
      pre,
      begin_transition,
      begin_auxiliary,
      inline_field_door(),
    ),
    validation_machine_v1.verify_script_sources_stage_one_redeemer_semantics_v1(
      begin_post,
      header_transition,
      header_auxiliary,
      inline_field_door(),
    ),
    validation_machine_v1.verify_script_sources_stage_one_semantics_v1(
      item_post,
      final_transition,
      validation_machine_v1.NoAuxiliaryWitness,
      inline_field_door(),
    ),
    !validation_machine_v1.verify_script_sources_stage_one_redeemer_semantics_v1(
      item_post,
      final_transition,
      validation_machine_v1.NoAuxiliaryWitness,
      inline_field_door(),
    ),
  }
}

test script_sources_stage_one_accepts_more_than_sixteen_sources() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: list.repeat(script, 17),
    },
  }
  let source_frontier = validation_merkle_v1.build_frontier(
    indexed_inline_source_leaves(script, 0, 17),
  )
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    10,
  )
  let Pair(post, _) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    2,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_one_finish_raw_semantics_v1(
      pre,
      transition,
    ),
    validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition,
        auxiliary: validation_machine_v1.NoAuxiliaryWitness,
      },
      inline_field_door(),
    ),
  }
}

test script_sources_binds_resolved_input_replay_schedule() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [input] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let schedule = validation_machine_v1.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    1,
    accumulator,
    2,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  let Pair(post, _) = script_sources_step_fixture(
    tx,
    1,
    accumulator,
    3,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    schedule,
    0,
    empty_frontier,
    12,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
}

type ResolutionScheduleTerminalVectorV1 {
  original_schedule_hash: ByteArray,
  terminal_accumulator: ByteArray,
  terminal_frontier_commitment: ByteArray,
  penultimate_accumulator: ByteArray,
  penultimate_remaining_schedule_hash: ByteArray,
  penultimate_spend_index: Int,
  penultimate_resolved_item_peaks: List<validation_merkle_v1.FrontierPeak>,
  last_source_kind: Int,
  last_key: ByteArray,
  last_value: ByteArray,
}

const maximum_input_schedule_last_key =
  #"82582000000000000000000000000000000000000000000000000000000000000000001901b1"

const maximum_input_schedule_last_value =
  #"90011901b11829582075905d5cce7bbf39a2d566bf6f5ee2f9074354777ca4408ebd73e9d6d77cc9fb581d60111111111111111111111111111111111111111111111111111111111a00989680005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358208e037f86edc06fd63fbc01bf5e587d66127e73513943f7d1c294a3b6649b72b7183c18528358208e037f86edc06fd63fbc01bf5e587d66127e73513943f7d1c294a3b6649b72b7183c18528358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304"

fn maximum_spend_input_schedule_terminal_vector() -> ResolutionScheduleTerminalVectorV1 {
  ResolutionScheduleTerminalVectorV1 {
    original_schedule_hash: #"b1276de655cef9d7a3bc33abb49024dd36462f2cd2486dd25d9f7347563eab35",
    terminal_accumulator: #"fb2154b7f046126c8cda87674c2822c3dad1ac294531845e6e6b49c0035e4858",
    terminal_frontier_commitment: #"8d0f7813e70f9bf63f84b78365a38e5c00efb807ac4f5ed16d21f0eff4b5e5b7",
    penultimate_accumulator: #"7f4f9eb3bfca6b31c4c6781cc36b58f0305c7cafbfbbe4dbfef9ad2370f8cbdc",
    penultimate_remaining_schedule_hash: #"6d5383cff6142a4e20298cf82b71837886eeb853245d068a8cf43a540e1d2b3d",
    penultimate_spend_index: 433,
    penultimate_resolved_item_peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"490d3ccf112900b8af68cdf9873ea682bc41f5e3a18ce80e8cbaad97c7afe1f0",
      },
      validation_merkle_v1.FrontierPeak {
        height: 4,
        hash: #"d01cd11a1af0cc1270e6f3f3a7dfff15c52f7710c1c12bdd3245c068f328c53b",
      },
      validation_merkle_v1.FrontierPeak {
        height: 5,
        hash: #"4bd02c803151903ed7bb3a09e8c7823ba0391d78edfc6dacaf66badc3950d9bf",
      },
      validation_merkle_v1.FrontierPeak {
        height: 7,
        hash: #"66b7b616caa9588056af4d6191ecd23ba077e9c7644e70bb2666fb08f759a0a6",
      },
      validation_merkle_v1.FrontierPeak {
        height: 8,
        hash: #"998486423647076a38bdd62961e92f2c404cd7c9f3ce8f862890f806e82eb543",
      },
    ],
    last_source_kind: 0,
    last_key: maximum_input_schedule_last_key,
    last_value: maximum_input_schedule_last_value,
  }
}

fn maximum_reference_input_schedule_terminal_vector() -> ResolutionScheduleTerminalVectorV1 {
  ResolutionScheduleTerminalVectorV1 {
    original_schedule_hash: #"320bc3d5140983c35d48db52cbf4e722de03b8ea618ef56b6a7f8b6fc6162e56",
    terminal_accumulator: #"e0254c2da094b13f9857dd7f442bd346f74ccf0f4a1337bd1328983d80156421",
    terminal_frontier_commitment: #"b4757e20d40f58687e5421545c69f0399d6de0026a8e9728ec6e9bb13b510a46",
    penultimate_accumulator: #"cef5b3dc9392a0a6f359309a1b6ef88bc48196c7d85e4b2ab2b539a778fec226",
    penultimate_remaining_schedule_hash: #"11e90c7b9d22faa10a757fe7cbcfc12f5784cb6c546408583d36ce0dd5d49329",
    penultimate_spend_index: 1,
    penultimate_resolved_item_peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"c830521b7ad8c959ae55b31633e207388fae52161373666e11f0cfb8f2636258",
      },
      validation_merkle_v1.FrontierPeak {
        height: 4,
        hash: #"fb7125a9b42801c60668ae85fbb63c9688ae6f1db51b66e808f887beb33ad939",
      },
      validation_merkle_v1.FrontierPeak {
        height: 5,
        hash: #"d434d9b86a8a039067633f60db0b7028cd8c0e20d7738f136c71ba4ebb17f600",
      },
      validation_merkle_v1.FrontierPeak {
        height: 7,
        hash: #"a0b8592e98857754eb7c710a2b73418391aef2bda4fcbd1e1bef9db50945bed7",
      },
      validation_merkle_v1.FrontierPeak {
        height: 8,
        hash: #"7d334c4861036c40c26095c4cb1d9db17b736a5a4108beb136aa4988ce854fe7",
      },
    ],
    last_source_kind: 1,
    last_key: maximum_input_schedule_last_key,
    last_value: maximum_input_schedule_last_value,
  }
}

fn resolution_schedule_terminal_control(
  vector: ResolutionScheduleTerminalVectorV1,
) -> validation_machine_v1.ScriptSourcesControlV1 {
  let base = stage_zero_script_sources_control(
    empty_v1_transaction(),
    validation_merkle_v1.build_frontier([]),
    0,
    #"",
  )
  validation_machine_v1.ScriptSourcesControlV1 {
    ..base,
    resolved_input_count: 434,
    resolved_inputs_accumulator: vector.terminal_accumulator,
    resolved_item_peaks: vector.penultimate_resolved_item_peaks,
    stage: 3,
    replay_cursor: 433,
    replay_accumulator: vector.penultimate_accumulator,
    replay_remaining_schedule_hash: vector.penultimate_remaining_schedule_hash,
    spend_index: vector.penultimate_spend_index,
    resolution_schedule_hash: vector.original_schedule_hash,
  }
}

fn resolution_schedule_terminal_state(
  control: validation_machine_v1.ScriptSourcesControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let tx = empty_v1_transaction()
  let compact = midgard_transaction_to_compact(tx)
  let transaction_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.ScriptSources,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
    },
    work_witness_cbor,
  )
}

fn verify_maximum_input_schedule_terminal_vector(
  vector: ResolutionScheduleTerminalVectorV1,
) -> Bool {
  let pre_control = resolution_schedule_terminal_control(vector)
  let terminal_accumulator = validation_machine_v1.resolved_input_accumulator_successor(
    pre_control.replay_accumulator,
    vector.last_source_kind,
    vector.last_key,
    vector.last_value,
  )
  let terminal_resolved_item_peaks = validation_merkle_v1.append_leaf(
    pre_control.replay_cursor,
    pre_control.resolved_item_peaks,
    script_proof_v1.resolved_context_item_leaf_hash(
      vector.last_source_kind,
      pre_control.replay_cursor,
      vector.last_key,
      vector.last_value,
    ),
  )
  let terminal_spend_index = pre_control.spend_index + 1 - vector.last_source_kind
  let terminal_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..pre_control,
    resolved_item_peaks: terminal_resolved_item_peaks,
    replay_cursor: 434,
    replay_accumulator: terminal_accumulator,
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    spend_index: terminal_spend_index,
  }
  let finish_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..terminal_control,
    stage: 4,
  }
  let Pair(pre, pre_work_witness_cbor) = resolution_schedule_terminal_state(
    pre_control,
    12,
  )
  let Pair(terminal, terminal_work_witness_cbor) = resolution_schedule_terminal_state(
    terminal_control,
    13,
  )
  let Pair(finish, _) = resolution_schedule_terminal_state(finish_control, 14)
  let replay_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: pre_work_witness_cbor,
    claimed_successor: terminal,
  }
  let replay_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: replay_transition,
    auxiliary: validation_machine_v1.ResolvedInputReplayWitness {
      source_kind: vector.last_source_kind,
      key: vector.last_key,
      next_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
      value: vector.last_value,
    },
  }
  let finish_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: terminal_work_witness_cbor,
    claimed_successor: finish,
  }
  let finish_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: finish_transition,
    auxiliary: validation_machine_v1.NoAuxiliaryWitness,
  }
  let premature_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: pre_work_witness_cbor,
    claimed_successor: finish,
  }
  let malformed_terminal_cursor_peaks = validation_merkle_v1.append_leaf(
    434,
    terminal_resolved_item_peaks,
    h_a,
  )
  let malformed_terminal_cursor_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..terminal_control,
    resolved_item_peaks: malformed_terminal_cursor_peaks,
    replay_cursor: 435,
  }
  let malformed_terminal_cursor_finish_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..malformed_terminal_cursor_control,
    stage: 4,
  }
  let
      Pair(malformed_terminal_cursor,
      malformed_terminal_cursor_work_witness_cbor)
    = resolution_schedule_terminal_state(malformed_terminal_cursor_control, 13)
  let Pair(malformed_terminal_cursor_finish, _) = resolution_schedule_terminal_state(
    malformed_terminal_cursor_finish_control,
    14,
  )
  let malformed_terminal_cursor_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: malformed_terminal_cursor_work_witness_cbor,
    claimed_successor: malformed_terminal_cursor_finish,
  }
  let malformed_terminal_accumulator_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..terminal_control,
    replay_accumulator: h_a,
  }
  let malformed_terminal_accumulator_finish_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..malformed_terminal_accumulator_control,
    stage: 4,
  }
  let
      Pair(malformed_terminal_accumulator,
      malformed_terminal_accumulator_work_witness_cbor)
    = resolution_schedule_terminal_state(
    malformed_terminal_accumulator_control,
    13,
  )
  let Pair(malformed_terminal_accumulator_finish, _) = resolution_schedule_terminal_state(
    malformed_terminal_accumulator_finish_control,
    14,
  )
  let malformed_terminal_accumulator_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: malformed_terminal_accumulator_work_witness_cbor,
    claimed_successor: malformed_terminal_accumulator_finish,
  }
  let stage_two_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..pre_control,
    resolved_item_peaks: [],
    stage: 2,
    replay_cursor: 0,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    spend_index: 0,
  }
  let stage_three_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..stage_two_control,
    stage: 3,
    replay_remaining_schedule_hash: vector.original_schedule_hash,
  }
  let altered_head_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..stage_three_control,
    resolution_schedule_hash: h_a,
  }
  let Pair(stage_two, stage_two_work_witness_cbor) = resolution_schedule_terminal_state(
    stage_two_control,
    10,
  )
  let Pair(stage_three, _) = resolution_schedule_terminal_state(
    stage_three_control,
    11,
  )
  let Pair(altered_head, _) = resolution_schedule_terminal_state(
    altered_head_control,
    11,
  )
  let stage_two_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_two_work_witness_cbor,
    claimed_successor: stage_three,
  }
  let altered_head_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_two_work_witness_cbor,
    claimed_successor: altered_head,
  }
  let mutated_source_kind = 1 - vector.last_source_kind
  let mutated_key = bytearray.concat(
    builtin.slice_bytearray(
      0,
      bytearray.length(vector.last_key) - 1,
      vector.last_key,
    ),
    #"ff",
  )
  and {
    terminal_accumulator == vector.terminal_accumulator,
    validation_merkle_v1.frontier_commitment(434, terminal_resolved_item_peaks) == vector.terminal_frontier_commitment,
    bytearray.length(cbor.serialise(replay_evidence)) < 16_384,
    bytearray.length(cbor.serialise(finish_evidence)) < 16_384,
    validation_machine_v1.verify_script_sources_stage_two_semantics_v1(
      stage_two,
      stage_two_transition,
      validation_machine_v1.NoAuxiliaryWitness,
    ),
    !validation_machine_v1.verify_script_sources_stage_two_semantics_v1(
      stage_two,
      altered_head_transition,
      validation_machine_v1.NoAuxiliaryWitness,
    ),
    validation_machine_v1.verify_script_sources_stage_three_replay_semantics_v1(
      pre,
      replay_transition,
      vector.last_source_kind,
      vector.last_key,
      validation_machine_v1.empty_resolution_schedule_hash(),
      vector.last_value,
    ),
    !validation_machine_v1.verify_script_sources_stage_three_replay_semantics_v1(
      pre,
      replay_transition,
      mutated_source_kind,
      vector.last_key,
      validation_machine_v1.empty_resolution_schedule_hash(),
      vector.last_value,
    ),
    !validation_machine_v1.verify_script_sources_stage_three_replay_semantics_v1(
      pre,
      replay_transition,
      vector.last_source_kind,
      mutated_key,
      validation_machine_v1.empty_resolution_schedule_hash(),
      vector.last_value,
    ),
    !validation_machine_v1.verify_script_sources_stage_three_replay_semantics_v1(
      pre,
      replay_transition,
      vector.last_source_kind,
      vector.last_key,
      h_a,
      vector.last_value,
    ),
    !validation_machine_v1.verify_script_sources_stage_three_finish_semantics_v1(
      pre,
      premature_transition,
    ),
    !validation_machine_v1.verify_script_sources_stage_three_finish_semantics_v1(
      malformed_terminal_cursor,
      malformed_terminal_cursor_transition,
    ),
    !validation_machine_v1.verify_script_sources_stage_three_finish_semantics_v1(
      malformed_terminal_accumulator,
      malformed_terminal_accumulator_transition,
    ),
    validation_machine_v1.verify_script_sources_stage_three_finish_semantics_v1(
      terminal,
      finish_transition,
    ),
  }
}

// The TypeScript producer exhaustively derives these terminal controls from
// the checked 434-spend Cardano boundary fixture. This Aiken argument contains
// only the final bounded replay item plus logarithmic frontier peaks.
test maximum_spend_input_schedule_terminal_matches_typescript() {
  verify_maximum_input_schedule_terminal_vector(
    maximum_spend_input_schedule_terminal_vector(),
  )
}

// The mixed checked boundary contains one spend and 433 reference inputs.
test maximum_reference_input_schedule_terminal_matches_typescript() {
  verify_maximum_input_schedule_terminal_vector(
    maximum_reference_input_schedule_terminal_vector(),
  )
}

/// Replay values are compact ledger-output descriptors, not raw output CBOR:
/// `script_sources_replay_item` decodes the value with
/// `ledger_output_commitment_v1.decode` and reads the resolved address and the
/// reference-script fields out of it.
fn replay_output_descriptor_fixture(
  output_index: Int,
  output_cbor: ByteArray,
  address: ByteArray,
  reference_script: Option<MidgardVersionedScript>,
) -> ByteArray {
  expect Some(cardano_tx_out) = script_context_v1.tx_out_summary_v1(
    output_cbor,
    False,
  )
  expect Some(midgard_tx_out) = script_context_v1.tx_out_summary_v1(
    output_cbor,
    True,
  )
  expect Some(cardano_spend_datum) = script_context_v1.spend_datum_summary_v1(
    output_cbor,
  )
  let base = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index,
    total_length: bytearray.length(output_cbor),
    item_commitment: bounded_item_v1.from_bytes(2, output_index, output_cbor),
    address,
    lovelace: 0,
    asset_count: 0,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
    cardano_value_size: 0,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out,
    midgard_tx_out,
    cardano_spend_datum,
  }
  let descriptor = when reference_script is {
    None -> base
    Some(script) -> {
      let script_cbor = encode_midgard_versioned_script(script)
      ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
        ..base,
        reference_script_language: script_proof_v1.language_tag(script.language),
        reference_script_hash: script_proof_v1.versioned_script_hash(script),
        reference_script_total_length: bytearray.length(script_cbor),
        reference_script_item_commitment: bounded_item_v1.from_bytes(
          2,
          output_index,
          script_cbor,
        ),
      }
    }
  }
  ledger_output_commitment_v1.encode(descriptor)
}

test script_sources_replay_appends_reference_scripts() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: Some(script),
  }
  let value = replay_output_descriptor_fixture(
    0,
    encode_midgard_tx_output(output),
    #"70" |> bytearray.concat(h_28_a),
    Some(script),
  )
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, reference_inputs: [input] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.source_leaf_hash(1, key, script)],
  )
  let resolved_item_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.resolved_context_item_leaf_hash(1, 0, key, value)],
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let next_accumulator = validation_machine_v1.resolved_input_accumulator_successor(
    accumulator,
    1,
    key,
    value,
  )
  let schedule = validation_machine_v1.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    1,
    next_accumulator,
    3,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    schedule,
    0,
    empty_frontier,
    12,
  )
  let Pair(post, _) = script_sources_step_fixture_with_resolved_items(
    tx,
    1,
    next_accumulator,
    3,
    source_frontier,
    empty_frontier,
    1,
    resolved_item_frontier,
    next_accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    13,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.ResolvedInputReplayWitness {
        source_kind: 1,
        key,
        next_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
        value,
      },
    },
    inline_field_door(),
  )
}

test script_sources_replay_appends_script_credential_spend_purposes() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let value = replay_output_descriptor_fixture(
    0,
    encode_midgard_tx_output(output),
    #"70" |> bytearray.concat(h_28_a),
    None,
  )
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [input] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, key)],
  )
  let resolved_item_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.resolved_context_item_leaf_hash(0, 0, key, value)],
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let next_accumulator = validation_machine_v1.resolved_input_accumulator_successor(
    accumulator,
    0,
    key,
    value,
  )
  let schedule = validation_machine_v1.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    1,
    next_accumulator,
    3,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    schedule,
    0,
    empty_frontier,
    12,
  )
  let Pair(post, _) = script_sources_step_fixture_with_resolved_items(
    tx,
    1,
    next_accumulator,
    3,
    empty_frontier,
    empty_frontier,
    1,
    resolved_item_frontier,
    next_accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    1,
    purpose_frontier,
    13,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.ResolvedInputReplayWitness {
        source_kind: 0,
        key,
        next_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
        value,
      },
    },
    inline_field_door(),
  )
}

/// The one output every stage-4 output-commitment fixture in this module folds.
fn canonical_output_commitment_output() -> MidgardTxOutput {
  MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
}

fn canonical_output_commitment_output_cbor() -> ByteArray {
  encode_midgard_tx_output(canonical_output_commitment_output())
}

/// One stage-4 output-commitment step, with the two quantities the fold commits
/// about the item left free so a caller can forge either.
///
/// **Why the forgery moved from the evidence to the claimed successor.** Under
/// the retired counted scheme the prover asserted the item's
/// `(item_length, item_commitment)` in an `ItemProofV1` and
/// `bounded_collection_v1.verify_item` bound that tuple into `outputs_hash` —
/// the predicate #592 measured as unsatisfiable under §4. Both quantities are
/// now *derived* from bytes §8's door authenticated
/// (`machine_field_item_commitment` / `machine_field_item_count`), so no field
/// is left in the evidence to forge: a prover who changes either has changed
/// the field preimage, and the door's single `blake2b_256` against
/// `outputs_hash` refuses it by aborting (§7.3) rather than by returning
/// `False`. What a prover can still get wrong is what it *claims the fold
/// accumulated*, and that is what these two parameters perturb — the same
/// `C21-STAGE4` Option A soundness claim, checked at the surface where it is
/// still decidable.
fn canonical_output_commitment_step_with_successor(
  step_to_verify: Int,
  claimed_output_leaf: ByteArray,
  claimed_output_total_count: Int,
) -> Bool {
  let output = canonical_output_commitment_output()
  let output_cbor = canonical_output_commitment_output_cbor()
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    4,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    14,
  )
  let claimed_output_frontier = validation_merkle_v1.build_frontier(
    [claimed_output_leaf],
  )
  let Pair(after_item, after_item_work_witness_cbor) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    4,
    empty_frontier,
    empty_frontier,
    0,
    validation_merkle_v1.build_frontier([]),
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    claimed_output_frontier,
    claimed_output_total_count,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    validation_machine_v1.empty_observer_purpose_scan_control(),
    15,
  )
  let Pair(post, _) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    5,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    output_frontier,
    16,
  )
  if step_to_verify == 0 {
    validation_machine_v1.verify_script_sources_stage_four_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: after_item,
      },
      validation_machine_v1.TransactionRedeemerItemBeginWitness {
        carriage: inline_field_carriage([output_cbor]),
      },
      inline_field_door(),
    )
  } else {
    and {
      step_to_verify == 1,
      validation_machine_v1.verify_script_sources_stage_four_semantics_v1(
        after_item,
        validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: after_item_work_witness_cbor,
          claimed_successor: post,
        },
        validation_machine_v1.NoAuxiliaryWitness,
        inline_field_door(),
      ),
    }
  }
}

fn canonical_output_commitment_step(step_to_verify: Int) -> Bool {
  canonical_output_commitment_step_with_successor(
    step_to_verify,
    script_proof_v1.output_leaf_hash(
      0,
      canonical_output_commitment_output_cbor(),
    ),
    1,
  )
}

test script_sources_commits_an_authenticated_output_item() {
  canonical_output_commitment_step(0)
}

/// Supersedes `script_sources_rejects_a_substituted_complete_output_item`:
/// stage 4 does not reveal item bytes, so substitution attacks what the fold
/// accumulates.
///
/// The forgery is in the **claimed successor's** output leaf rather than in an
/// `ItemProofV1.item_commitment`, because the commitment the fold appends is
/// derived from door-authenticated bytes and is not a prover field any more
/// (see `canonical_output_commitment_step_with_successor`). The claim refuted is
/// unchanged and is `C21-STAGE4` Option A's: the leaf appended at stage 4 must
/// be `output_item_leaf_hash` over the *authenticated* item's `bounded_item_v1`
/// commitment, and nothing else.
test script_sources_rejects_a_forged_output_item_commitment() {
  !canonical_output_commitment_step_with_successor(
    0,
    script_proof_v1.output_item_leaf_hash(
      0,
      #"00000000000000000000000000000000000000000000000000000000000000ff",
    ),
    1,
  )
}

/// Was `script_sources_rejects_a_forged_output_item_length`, renamed for the
/// quantity it now forges (#592).
///
/// The old row forged `ItemProofV1.item_length`, whose only effect was on the
/// `bounded_collection_v1.verify_item` membership leaf — the conjunct the flat
/// reversion retired. The item's length is derived from authenticated bytes
/// now, so the surviving count claim a prover controls is the fold's
/// `output_total_count`, forged to 2 against a one-output field with the appended
/// leaf held honest.
///
/// **Which conjunct refutes it, measured rather than assumed.** The forgery lands
/// in the *claimed successor*, and this is the fold's opening step, so the pre
/// state's `output_total_count` is still the `0` sentinel and
/// `active_total_count` is therefore the field's own authenticated count — 1.
/// `item_count == active_total_count` is `1 == 1` and passes; neutralizing it to
/// `True` leaves this row green. The falsifier is
/// `script_sources_control_successor_is_exact`, which requires the successor to
/// carry `output_total_count: active_total_count` and so refuses the claimed 2.
/// That is the right refutation for a forged successor, and it is the whole of
/// this row's claim.
///
/// The named count binding gets its own row below, because "the step may not
/// commit a count the field denies" is a separate claim from "the successor must
/// be exact", and neutralizing either one leaves the other's row red.
test script_sources_rejects_a_forged_output_total_count() {
  !canonical_output_commitment_step_with_successor(
    0,
    script_proof_v1.output_leaf_hash(
      0,
      canonical_output_commitment_output_cbor(),
    ),
    2,
  )
}

/// `item_count == active_total_count` (validation-machine-v1.ak:9213) as the
/// **sole** falsifier — the conjunct the row above names and does not exercise.
///
/// The fold is entered with a carried `output_total_count` of 2 already in the
/// pre state's control, so `active_total_count` is that carried 2 rather than the
/// field's count, and the honest successor for such a control is one that carries
/// 2 forward. Successor exactness therefore **passes**: the successor built here
/// is exactly `{..control, output_count + 1, output_peaks appended honestly,
/// output_total_count: 2}`. What fails is the field itself disagreeing — field 2
/// authenticates one item, so a step claiming a two-output fold is refused. This
/// is the attack the count binding exists for: a carried count that the disputed
/// transaction's own field-2 preimage does not support, which is how a fold's
/// completion test would be reached without visiting every output.
test script_sources_rejects_an_output_total_count_the_field_denies() {
  let output = canonical_output_commitment_output()
  let output_cbor = canonical_output_commitment_output_cbor()
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    4,
    empty_frontier,
    empty_frontier,
    0,
    validation_merkle_v1.build_frontier([]),
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    2,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    validation_machine_v1.empty_observer_purpose_scan_control(),
    14,
  )
  let Pair(successor, _) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    4,
    empty_frontier,
    empty_frontier,
    0,
    validation_merkle_v1.build_frontier([]),
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    output_frontier,
    2,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    validation_machine_v1.empty_observer_purpose_scan_control(),
    15,
  )
  !validation_machine_v1.verify_script_sources_stage_four_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: successor,
    },
    validation_machine_v1.TransactionRedeemerItemBeginWitness {
      carriage: inline_field_carriage([output_cbor]),
    },
    inline_field_door(),
  )
}

test script_sources_finishes_authenticated_output_commitment() {
  canonical_output_commitment_step(1)
}

fn authenticated_output_proof_step(step_to_verify: Int) -> Bool {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let material = authenticated_output_material(output_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(before_begin, before_begin_work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    5,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    output_frontier,
    16,
  )
  let Pair(before_step, before_step_work_witness_cbor) = script_sources_active_output_proof_fixture(
    tx,
    0,
    output_frontier,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    material.initial,
    17,
  )
  let Pair(after_step, _) = script_sources_active_output_proof_fixture(
    tx,
    0,
    output_frontier,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    material.first_successor,
    18,
  )
  let begin_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: before_begin_work_witness_cbor,
    claimed_successor: before_step,
  }
  let begin_auxiliary = validation_machine_v1.LedgerOutputProofBeginWitness {
    output_index: 0,
    total_length: bytearray.length(output_cbor),
    item_commitment: material.item_commitment,
    siblings: [],
  }
  if step_to_verify == 0 {
    validation_machine_v1.verify_script_sources_output_proof_begin_semantics_v1(
      before_begin,
      begin_transition,
      0,
      bytearray.length(output_cbor),
      material.item_commitment,
      [],
    )
  } else if step_to_verify == 2 {
    !validation_machine_v1.verify_script_sources_non_output_semantics_v1(
      before_begin,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: begin_transition,
        auxiliary: begin_auxiliary,
      },
      inline_field_door(),
    )
  } else {
    and {
      step_to_verify == 1,
      validation_machine_v1.verify_script_sources_output_proof_step_semantics_v1(
        before_step,
        validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: before_step_work_witness_cbor,
          claimed_successor: after_step,
        },
        ledger_output_proof_v1.LedgerOutputProofChunks {
          chunk_proof: material.chunk_proof,
          next_chunk_proof: None,
        },
      ),
    }
  }
}

test script_sources_begins_an_authenticated_output_proof() {
  authenticated_output_proof_step(0)
}

test script_sources_advances_one_bounded_output_proof_step() {
  authenticated_output_proof_step(1)
}

test script_sources_output_proof_cannot_use_non_output_resolver() {
  authenticated_output_proof_step(2)
}

test script_sources_collects_protected_receive_source() {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let material = authenticated_output_material(output_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let receive_source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, h_28_a, h_28_a)],
  )
  let output_descriptor_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_descriptor_leaf_hash(0, material.descriptor_cbor)],
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_active_output_proof_fixture(
    tx,
    0,
    output_frontier,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    material.terminal,
    22,
  )
  let Pair(post, _) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    5,
    empty_frontier,
    empty_frontier,
    0,
    empty_frontier,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    1,
    output_frontier,
    1,
    validation_machine_v1.ReceivePurposeScanControlV1 {
      source_count: receive_source_frontier.count,
      source_peaks: receive_source_frontier.peaks,
      receive_count: 0,
      previous_hash: #"",
      candidate_hash: #"",
      descriptor_peaks: output_descriptor_frontier.peaks,
    },
    validation_machine_v1.empty_observer_purpose_scan_control(),
    23,
  )
  validation_machine_v1.verify_script_sources_output_proof_finalize_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    material.descriptor_cbor,
    validation_machine_v1.NoSignerSetProof,
  )
}

fn protected_receive_purpose_step(step_to_verify: Int) -> Bool {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let material = authenticated_output_material(output_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let receive_source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, h_28_a, h_28_a)],
  )
  let output_descriptor_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_descriptor_leaf_hash(0, material.descriptor_cbor)],
  )
  let empty_receive_scan = validation_machine_v1.ReceivePurposeScanControlV1 {
    source_count: receive_source_frontier.count,
    source_peaks: receive_source_frontier.peaks,
    receive_count: 0,
    previous_hash: #"",
    candidate_hash: #"",
    descriptor_peaks: output_descriptor_frontier.peaks,
  }
  let selected_receive_scan = validation_machine_v1.ReceivePurposeScanControlV1 {
    source_count: receive_source_frontier.count,
    source_peaks: receive_source_frontier.peaks,
    receive_count: 0,
    previous_hash: #"",
    candidate_hash: h_28_a,
    descriptor_peaks: output_descriptor_frontier.peaks,
  }
  let committed_receive_scan = validation_machine_v1.ReceivePurposeScanControlV1 {
    source_count: receive_source_frontier.count,
    source_peaks: receive_source_frontier.peaks,
    receive_count: 1,
    previous_hash: h_28_a,
    candidate_hash: #"",
    descriptor_peaks: output_descriptor_frontier.peaks,
  }
  let receive_purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, h_28_a, h_28_a)],
  )
  let Pair(pre, work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    0,
    output_frontier,
    empty_receive_scan,
    15,
  )
  let Pair(after_candidate, after_candidate_work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    1,
    output_frontier,
    selected_receive_scan,
    16,
  )
  let Pair(after_append, after_append_work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    receive_purpose_frontier,
    0,
    output_frontier,
    committed_receive_scan,
    17,
  )
  let Pair(after_rescan, after_rescan_work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    receive_purpose_frontier,
    1,
    output_frontier,
    committed_receive_scan,
    18,
  )
  let Pair(post, _) = script_discovery_step_fixture_with_output_frontier(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    receive_purpose_frontier,
    1,
    output_frontier,
    output_descriptor_frontier,
    validation_machine_v1.empty_script_discovery_control(),
    19,
  )
  let select_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: after_candidate,
  }
  let append_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: after_candidate_work_witness_cbor,
    claimed_successor: after_append,
  }
  let rescan_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: after_append_work_witness_cbor,
    claimed_successor: after_rescan,
  }
  let finish_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: after_rescan_work_witness_cbor,
    claimed_successor: post,
  }
  if step_to_verify == 0 {
    and {
      validation_machine_v1.verify_script_sources_stage_seven_receive_semantics_v1(
        pre,
        select_transition,
        3,
        0,
        h_28_a,
        h_28_a,
        [],
      ),
      validation_machine_v1.verify_one_step_evidence(
        pre,
        validation_machine_v1.ValidationOneStepEvidenceV1 {
          transition: select_transition,
          auxiliary: validation_machine_v1.ScriptPurposeScanWitness {
            purpose_kind: 3,
            purpose_index: 0,
            script_hash: h_28_a,
            subject: h_28_a,
            siblings: [],
          },
        },
        inline_field_door(),
      ),
    }
  } else if step_to_verify == 1 {
    and {
      validation_machine_v1.verify_script_sources_stage_seven_finish_semantics_v1(
        after_candidate,
        append_transition,
      ),
      validation_machine_v1.verify_one_step_evidence(
        after_candidate,
        validation_machine_v1.ValidationOneStepEvidenceV1 {
          transition: append_transition,
          auxiliary: validation_machine_v1.NoAuxiliaryWitness,
        },
        inline_field_door(),
      ),
    }
  } else if step_to_verify == 2 {
    and {
      validation_machine_v1.verify_script_sources_stage_seven_receive_semantics_v1(
        after_append,
        rescan_transition,
        3,
        0,
        h_28_a,
        h_28_a,
        [],
      ),
      validation_machine_v1.verify_one_step_evidence(
        after_append,
        validation_machine_v1.ValidationOneStepEvidenceV1 {
          transition: rescan_transition,
          auxiliary: validation_machine_v1.ScriptPurposeScanWitness {
            purpose_kind: 3,
            purpose_index: 0,
            script_hash: h_28_a,
            subject: h_28_a,
            siblings: [],
          },
        },
        inline_field_door(),
      ),
    }
  } else {
    and {
      step_to_verify == 3,
      validation_machine_v1.verify_script_sources_stage_seven_finish_semantics_v1(
        after_rescan,
        finish_transition,
      ),
      validation_machine_v1.verify_one_step_evidence(
        after_rescan,
        validation_machine_v1.ValidationOneStepEvidenceV1 {
          transition: finish_transition,
          auxiliary: validation_machine_v1.NoAuxiliaryWitness,
        },
        inline_field_door(),
      ),
    }
  }
}

test script_sources_selects_canonical_protected_receive_purpose() {
  protected_receive_purpose_step(0)
}

test script_sources_appends_protected_receive_purpose() {
  protected_receive_purpose_step(1)
}

test script_sources_rescans_receive_sources_for_distinctness() {
  protected_receive_purpose_step(2)
}

test script_sources_finishes_protected_receive_purpose_scan() {
  protected_receive_purpose_step(3)
}

test script_sources_rejects_an_unsigned_protected_pubkey_output() {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let material = authenticated_output_material(output_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let Pair(pre, work_witness_cbor) = script_sources_active_output_proof_fixture(
    tx,
    0,
    output_frontier,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    material.terminal,
    15,
  )
  let rejection_code = #"455f4d495353494e475f52455155495245445f5749544e455353"
  // An exact rejection also clears the ledger delta to the empty frontier
  // commitment; `exact_rejection_post` is the canonical successor shape.
  let terminal = exact_rejection_post(pre, rejection_code)
  validation_machine_v1.verify_script_sources_output_proof_finalize_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: terminal,
    },
    material.descriptor_cbor,
    validation_machine_v1.EmptySignerSetProof { peaks: [] },
  )
}

/// The mint fold's three steps, under §8's field-access door (#592).
///
/// **This vector's begin step was a tripwire and is now a positive assertion.**
/// Under the counted scheme `script_sources_begin_mint_policy` authenticated its
/// policy item with `bounded_collection_v1.verify_item(mint_hash, ...)` while
/// `mint_hash` had already become §4's *flat* `blake2b_256` over field 5's §5.1
/// enveloped preimage — a hash with no leaves, which
/// `bounded-collection-v1.ak`'s own docstring says `verify_item` cannot be
/// satisfied against. No fixture could make the begin step true, so this test
/// asserted `!begin_step` beside the reason, with a note that the row "turns red
/// the moment stage six is rebound onto the flat door".
///
/// #592 is that rebind, so the note is honoured here: the begin step now carries
/// a §8.1 tier-1 carriage over the field-5 preimage, the door hashes it once
/// against `mint_hash`, and the policy item's length and `bounded_item_v1`
/// commitment are derived from those authenticated bytes into the fold control.
/// All three steps are asserted **positively**.
///
/// The asset and finish steps are unchanged: they authenticate against
/// `fold.item_commitment`, which the fold control carries and which is a genuine
/// Merkle root over the item's chunks, so their openings were always
/// satisfiable. What moved is only the provenance of that root — derived at the
/// begin step from door-authenticated bytes instead of asserted by an
/// `ItemProofV1`.
test script_sources_streams_mint_and_burn_into_authenticated_frontiers() {
  // §5.6 — the enveloped list of per-policy items, not the retired raw map.
  let mint_cbor = native_tx_field_access_v1.encode_field_preimage(
    [
      #"82581c"
        |> bytearray.concat(h_28_a)
        |> bytearray.concat(#"a1410124"),
    ],
  )
  let policy_item_cbor = #"82581c"
    |> bytearray.concat(h_28_a)
    |> bytearray.concat(#"a1410124")
  let mint = decode_midgard_tx_mint_preimage_cbor(mint_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let mint_purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(1, 0, h_28_a, h_28_a)],
  )
  let mint_asset_frontier = validation_merkle_v1.build_frontier(
    [validation_machine_v1.mint_asset_leaf_hash(h_28_a, #"01", -5)],
  )
  let carriage = inline_field_carriage([policy_item_cbor])
  let chunk_proof = singleton_field_item_chunk_proof(5, policy_item_cbor)
  let initial_fold = validation_machine_v1.empty_mint_fold_control()
  let active_fold = validation_machine_v1.MintFoldControlV1 {
    policy_count: 1,
    policy_cursor: 0,
    previous_policy: #"",
    active_policy: h_28_a,
    item_length: bytearray.length(policy_item_cbor),
    item_commitment: bounded_item_v1.from_bytes(5, 0, policy_item_cbor),
    item_cursor: 32,
    assets_remaining: 1,
    policy_asset_cursor: 0,
    previous_asset: #"",
    asset_count: 0,
    asset_peaks: empty_frontier.peaks,
  }
  let completed_fold = validation_machine_v1.MintFoldControlV1 {
    policy_count: 1,
    policy_cursor: 1,
    previous_policy: h_28_a,
    active_policy: #"",
    item_length: 0,
    item_commitment: #"",
    item_cursor: 0,
    assets_remaining: 0,
    policy_asset_cursor: 0,
    previous_asset: #"",
    asset_count: 1,
    asset_peaks: mint_asset_frontier.peaks,
  }
  let Pair(begin_pre, begin_work_witness_cbor) = script_sources_mint_fold_fixture(
    tx,
    empty_frontier,
    initial_fold,
    6,
    17,
  )
  let Pair(begin_post, _) = script_sources_mint_fold_fixture(
    tx,
    mint_purpose_frontier,
    active_fold,
    6,
    18,
  )
  let begin_step = validation_machine_v1.verify_script_sources_stage_six_semantics_v1(
    begin_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: begin_work_witness_cbor,
      claimed_successor: begin_post,
    },
    validation_machine_v1.TransactionFieldChunkWitness {
      field_index: 5,
      item_index: 0,
      carriage,
    },
    inline_field_door(),
  )
  let Pair(asset_pre, asset_work_witness_cbor) = script_sources_mint_fold_fixture(
    tx,
    mint_purpose_frontier,
    active_fold,
    6,
    18,
  )
  let Pair(asset_post, _) = script_sources_mint_fold_fixture(
    tx,
    mint_purpose_frontier,
    completed_fold,
    6,
    19,
  )
  let asset_step = validation_machine_v1.verify_script_sources_stage_six_semantics_v1(
    asset_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: asset_work_witness_cbor,
      claimed_successor: asset_post,
    },
    validation_machine_v1.MintFoldAssetWitness {
      chunk_proof,
      next_chunk_proof: None,
    },
    inline_field_door(),
  )
  let Pair(finish_pre, finish_work_witness_cbor) = script_sources_mint_fold_fixture(
    tx,
    mint_purpose_frontier,
    completed_fold,
    6,
    19,
  )
  let Pair(finish_post, _) = script_sources_mint_fold_fixture(
    tx,
    mint_purpose_frontier,
    completed_fold,
    7,
    20,
  )
  let finish_step = validation_machine_v1.verify_script_sources_stage_six_semantics_v1(
    finish_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: finish_work_witness_cbor,
      claimed_successor: finish_post,
    },
    validation_machine_v1.NoAuxiliaryWitness,
    inline_field_door(),
  )
  and {
    // The rebound call site, asserted positively — see this test's note for what
    // it used to assert and why.
    begin_step,
    // …and the tripwire's own reason, kept as a live statement rather than a
    // claim about history: field 5's commitment is the flat `blake2b_256` over
    // its §5.1 preimage, and the door reaches the policy item by authenticating
    // exactly those bytes. A carriage over anything else does not open.
    native_tx_field_access_v1.field_commitment(mint_cbor) == midgard_transaction_to_compact(
      tx,
    ).body.mint_hash,
    asset_step,
    finish_step,
  }
}

fn maximum_mint_asset_leaves(
  asset_index: Int,
  asset_count: Int,
) -> List<ByteArray> {
  if asset_index == asset_count {
    []
  } else {
    [
      validation_machine_v1.mint_asset_leaf_hash(
        h_28_a,
        bytearray.from_int_big_endian(asset_index, 32),
        1,
      ),
      ..maximum_mint_asset_leaves(asset_index + 1, asset_count)
    ]
  }
}

fn crossing_mint_fold_step(withhold_next_chunk: Bool) -> Bool {
  let mint = decode_midgard_tx_mint_preimage_cbor(maximum_mint_preimage_cbor())
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  // §5.6's policy item is now a named fixture in its own right rather than the
  // retired raw map's tail with a fresh head glued on.
  let item_cbor = maximum_mint_policy_item_cbor
  let first_chunk = builtin.slice_bytearray(
    0,
    bounded_item_v1.chunk_bytes,
    item_cbor,
  )
  let second_chunk = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes,
    bytearray.length(item_cbor) - bounded_item_v1.chunk_bytes,
    item_cbor,
  )
  let first_hash = bounded_item_v1.hash_chunk(5, 0, 0, first_chunk)
  let second_hash = bounded_item_v1.hash_chunk(5, 0, 1, second_chunk)
  let chunk_frontier = validation_merkle_v1.build_frontier(
    [first_hash, second_hash],
  )
  let item_commitment = bounded_item_v1.commitment(
    5,
    0,
    bytearray.length(item_cbor),
    chunk_frontier.peaks,
  )
  let current_chunk_proof = bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 5,
    item_index: 0,
    total_length: bytearray.length(item_cbor),
    chunk_index: 0,
    chunk: first_chunk,
    frontier: chunk_frontier.peaks,
    siblings: [second_hash],
  }
  let next_chunk_proof = bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 5,
    item_index: 0,
    total_length: bytearray.length(item_cbor),
    chunk_index: 1,
    chunk: second_chunk,
    frontier: chunk_frontier.peaks,
    siblings: [first_hash],
  }
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(1, 0, h_28_a, h_28_a)],
  )
  let prior_asset_frontier = validation_merkle_v1.build_frontier(
    maximum_mint_asset_leaves(0, 116),
  )
  let next_asset_frontier = validation_merkle_v1.build_frontier(
    maximum_mint_asset_leaves(0, 117),
  )
  let pre_fold = validation_machine_v1.MintFoldControlV1 {
    policy_count: 1,
    policy_cursor: 0,
    previous_policy: #"",
    active_policy: h_28_a,
    item_length: bytearray.length(item_cbor),
    item_commitment,
    item_cursor: 4_093,
    assets_remaining: 12,
    policy_asset_cursor: 116,
    previous_asset: bytearray.from_int_big_endian(115, 32),
    asset_count: 116,
    asset_peaks: prior_asset_frontier.peaks,
  }
  let post_fold = validation_machine_v1.MintFoldControlV1 {
    ..pre_fold,
    item_cursor: 4_128,
    assets_remaining: 11,
    policy_asset_cursor: 117,
    previous_asset: bytearray.from_int_big_endian(116, 32),
    asset_count: 117,
    asset_peaks: next_asset_frontier.peaks,
  }
  let Pair(pre, work_witness_cbor) = script_sources_mint_fold_fixture(
    tx,
    purpose_frontier,
    pre_fold,
    6,
    51,
  )
  let Pair(post, _) = script_sources_mint_fold_fixture(
    tx,
    purpose_frontier,
    post_fold,
    6,
    52,
  )
  validation_machine_v1.verify_script_sources_stage_six_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    validation_machine_v1.MintFoldAssetWitness {
      chunk_proof: current_chunk_proof,
      next_chunk_proof: if withhold_next_chunk {
        None
      } else {
        Some(next_chunk_proof)
      },
    },
    inline_field_door(),
  )
}

test script_sources_mint_fold_authenticates_a_chunk_boundary() {
  crossing_mint_fold_step(False)
}

test script_sources_mint_fold_fails_closed_when_boundary_data_is_withheld() fail {
  crossing_mint_fold_step(True)
}

fn canonical_observer_purpose_step(step_to_verify: Int) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, required_observers: [h_28_a] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let observer_purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(2, 0, h_28_a, h_28_a)],
  )
  let final_purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(2, 0, h_28_a, h_28_a)],
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    18,
  )
  let Pair(after_observer, after_observer_work_witness_cbor) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    empty_frontier,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    observer_purpose_frontier,
    0,
    empty_frontier,
    0,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    validation_machine_v1.ObserverPurposeScanControlV1 {
      total_count: 1,
      seen: 1,
      previous_hash: h_28_a,
    },
    19,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    final_purpose_frontier,
    validation_machine_v1.empty_script_discovery_control(),
    20,
  )
  let carriage = inline_field_carriage([h_28_a])
  let observer_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: after_observer,
  }
  let finish_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: after_observer_work_witness_cbor,
    claimed_successor: post,
  }
  if step_to_verify == 0 {
    and {
      validation_machine_v1.verify_script_sources_stage_seven_observer_semantics_v1(
        pre,
        observer_transition,
        inline_field_door(),
        3,
        0,
        carriage,
      ),
      validation_machine_v1.verify_one_step_evidence(
        pre,
        validation_machine_v1.ValidationOneStepEvidenceV1 {
          transition: observer_transition,
          auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
            field_index: 3,
            item_index: 0,
            carriage,
          },
        },
        inline_field_door(),
      ),
    }
  } else {
    and {
      step_to_verify == 1,
      validation_machine_v1.verify_script_sources_stage_seven_finish_semantics_v1(
        after_observer,
        finish_transition,
      ),
      validation_machine_v1.verify_one_step_evidence(
        after_observer,
        validation_machine_v1.ValidationOneStepEvidenceV1 {
          transition: finish_transition,
          auxiliary: validation_machine_v1.NoAuxiliaryWitness,
        },
        inline_field_door(),
      ),
    }
  }
}

test script_sources_appends_canonical_observer_purposes() {
  canonical_observer_purpose_step(0)
}

test script_sources_finishes_canonical_observer_purposes() {
  canonical_observer_purpose_step(1)
}

// ---------------------------------------------------------------------------
// Successor-encoding controls.
//
// Every ScriptSources successor must be committed in the single canonical
// encoding produced by `encode_script_sources_witness` (definite-length array
// header, `encode_definite_bytes` for every bytestring). Re-serialising the
// decoded `Data` items with `builtin.list_data |> cbor.serialise` is Plutus
// `serialiseData`, which emits an indefinite `9f..ff` array and chunks
// bytestrings over 64 bytes as `5f..ff`. The two forms are distinct byte
// strings, so committing the wrong one wedges the trace: the next step's
// control binding re-derives the canonical encoding and rejects.
//
// These controls pin both directions -- the canonical successor is accepted and
// the `serialiseData` successor (as well as a semantically mutated successor)
// is still rejected -- so a fix here cannot degrade into widened acceptance.
// ---------------------------------------------------------------------------

/// Re-serialise a work witness through Plutus `serialiseData`. The result
/// decodes to the same `Data` but is a different byte string than the canonical
/// encoding, which is exactly what the successor commitments must discriminate.
fn serialise_data_reencoding(work_witness_cbor: ByteArray) -> ByteArray {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  data |> builtin.un_list_data |> builtin.list_data |> cbor.serialise
}

/// A machine state committing to an arbitrary ScriptSources work witness, used
/// to build deliberately wrong successors without going through a fixture.
fn script_sources_state_for_work_witness(
  tx: MidgardTransaction,
  work_witness_cbor: ByteArray,
  program_counter: Int,
) -> validation_trace_v1.ValidationMachineStateV1 {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ScriptSources,
    program_counter,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      program_counter,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
}

test script_sources_stage_seven_finish_binds_canonical_successor_encoding() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    18,
  )
  let Pair(post, post_witness_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    validation_machine_v1.empty_script_discovery_control(),
    19,
  )
  // Same successor state, but committed in the `serialiseData` encoding.
  let reencoded_post = script_sources_state_for_work_witness(
    tx,
    serialise_data_reencoding(post_witness_cbor),
    19,
  )
  // Semantically wrong successor: stage 9 instead of the mandated stage 8.
  let Pair(mutated_post, _) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    validation_machine_v1.empty_script_discovery_control(),
    19,
  )
  let canonical_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    // The canonical encoding really is distinct from the `serialiseData` form.
    bytearray.take(post_witness_cbor, 1) == #"98",
    bytearray.take(serialise_data_reencoding(post_witness_cbor), 1) == #"9f",
    // Positive: both proof paths accept the canonical successor.
    validation_machine_v1.verify_script_sources_stage_seven_finish_semantics_v1(
      pre,
      canonical_transition,
    ),
    validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: canonical_transition,
        auxiliary: validation_machine_v1.NoAuxiliaryWitness,
      },
      inline_field_door(),
    ),
    // Control: the `serialiseData` successor is still rejected.
    !validation_machine_v1.verify_script_sources_stage_seven_finish_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: reencoded_post,
      },
    ),
    !validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor,
          claimed_successor: reencoded_post,
        },
        auxiliary: validation_machine_v1.NoAuxiliaryWitness,
      },
      inline_field_door(),
    ),
    // Control: a mutated successor stage is still rejected.
    !validation_machine_v1.verify_script_sources_stage_seven_finish_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: mutated_post,
      },
    ),
  }
}

test script_sources_stage_seven_observer_binds_canonical_successor_encoding() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, required_observers: [h_28_a] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let observer_purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(2, 0, h_28_a, h_28_a)],
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    18,
  )
  let Pair(after_observer, after_observer_witness_cbor) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    empty_frontier,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    observer_purpose_frontier,
    0,
    empty_frontier,
    0,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    validation_machine_v1.ObserverPurposeScanControlV1 {
      total_count: 1,
      seen: 1,
      previous_hash: h_28_a,
    },
    19,
  )
  // Same successor state, but committed in the `serialiseData` encoding.
  let reencoded_after_observer = script_sources_state_for_work_witness(
    tx,
    serialise_data_reencoding(after_observer_witness_cbor),
    19,
  )
  // Semantically wrong successor: the observer purpose was never appended.
  let Pair(mutated_after_observer, _) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    empty_frontier,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    0,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    validation_machine_v1.ObserverPurposeScanControlV1 {
      total_count: 1,
      seen: 1,
      previous_hash: h_28_a,
    },
    19,
  )
  let carriage = inline_field_carriage([h_28_a])
  and {
    validation_machine_v1.verify_script_sources_stage_seven_observer_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: after_observer,
      },
      inline_field_door(),
      3,
      0,
      carriage,
    ),
    !validation_machine_v1.verify_script_sources_stage_seven_observer_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: reencoded_after_observer,
      },
      inline_field_door(),
      3,
      0,
      carriage,
    ),
    !validation_machine_v1.verify_script_sources_stage_seven_observer_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: mutated_after_observer,
      },
      inline_field_door(),
      3,
      0,
      carriage,
    ),
  }
}

test script_sources_stage_seven_receive_binds_canonical_successor_encoding() {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let material = authenticated_output_material(output_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let receive_source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, h_28_a, h_28_a)],
  )
  let output_descriptor_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_descriptor_leaf_hash(0, material.descriptor_cbor)],
  )
  let empty_receive_scan = validation_machine_v1.ReceivePurposeScanControlV1 {
    source_count: receive_source_frontier.count,
    source_peaks: receive_source_frontier.peaks,
    receive_count: 0,
    previous_hash: #"",
    candidate_hash: #"",
    descriptor_peaks: output_descriptor_frontier.peaks,
  }
  let selected_receive_scan = validation_machine_v1.ReceivePurposeScanControlV1 {
    ..empty_receive_scan,
    candidate_hash: h_28_a,
  }
  let Pair(pre, work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    0,
    output_frontier,
    empty_receive_scan,
    15,
  )
  let Pair(after_candidate, after_candidate_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    1,
    output_frontier,
    selected_receive_scan,
    16,
  )
  // Same successor state, but committed in the `serialiseData` encoding.
  let reencoded_after_candidate = script_sources_state_for_work_witness(
    tx,
    serialise_data_reencoding(after_candidate_witness_cbor),
    16,
  )
  // Semantically wrong successor: the output cursor did not advance.
  let Pair(mutated_after_candidate, _) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    0,
    output_frontier,
    selected_receive_scan,
    16,
  )
  and {
    validation_machine_v1.verify_script_sources_stage_seven_receive_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: after_candidate,
      },
      3,
      0,
      h_28_a,
      h_28_a,
      [],
    ),
    !validation_machine_v1.verify_script_sources_stage_seven_receive_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: reencoded_after_candidate,
      },
      3,
      0,
      h_28_a,
      h_28_a,
      [],
    ),
    !validation_machine_v1.verify_script_sources_stage_seven_receive_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: mutated_after_candidate,
      },
      3,
      0,
      h_28_a,
      h_28_a,
      [],
    ),
  }
}

test script_sources_stage_one_finish_raw_binds_canonical_successor_encoding() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let source_frontier = validation_merkle_v1.build_frontier(
    indexed_inline_source_leaves(script, 0, 1),
  )
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    10,
  )
  let Pair(post, post_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    2,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  // Same successor state, but committed in the `serialiseData` encoding.
  let reencoded_post = script_sources_state_for_work_witness(
    tx,
    serialise_data_reencoding(post_witness_cbor),
    11,
  )
  // Semantically wrong successor: stage 3 instead of the mandated stage 2.
  let Pair(mutated_post, _) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    3,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  and {
    // The witness carries a bytestring past the 64-byte `serialiseData`
    // chunking threshold, which is what made the rebuilt prefix unmatchable.
    bytearray.length(midgard_transaction_proof_source_v1(tx).compact_cbor) > 64,
    validation_machine_v1.verify_script_sources_stage_one_finish_raw_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
    ),
    !validation_machine_v1.verify_script_sources_stage_one_finish_raw_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: reencoded_post,
      },
    ),
    !validation_machine_v1.verify_script_sources_stage_one_finish_raw_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: mutated_post,
      },
    ),
  }
}

test script_sources_prepares_more_than_sixteen_purposes_for_discovery() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    indexed_spend_purpose_leaves(0, 17),
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    purpose_frontier,
    0,
    empty_frontier,
    18,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    validation_machine_v1.empty_script_discovery_control(),
    19,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
}

test script_sources_discovers_more_than_sixteen_purposes() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let first_peak_leaves = indexed_spend_purpose_leaves(0, 16)
  let purpose_frontier = validation_merkle_v1.build_frontier(
    indexed_spend_purpose_leaves(0, 17),
  )
  let empty_discovery = validation_machine_v1.empty_script_discovery_control()
  let selected_purpose = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: h_28_a,
    current_subject: cbor.serialise(0),
  }
  let Pair(stage_eight, stage_eight_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    empty_discovery,
    19,
  )
  let Pair(stage_nine, _) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    selected_purpose,
    20,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_eight_cbor,
    claimed_successor: stage_nine,
  }
  let siblings = first_leaf_power_of_two_siblings(first_peak_leaves)
  and {
    validation_machine_v1.verify_script_sources_stage_eight_purpose_semantics_v1(
      stage_eight,
      transition,
      0,
      0,
      h_28_a,
      cbor.serialise(0),
      siblings,
    ),
    validation_machine_v1.verify_one_step_evidence(
      stage_eight,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition,
        auxiliary: validation_machine_v1.ScriptPurposeScanWitness {
          purpose_kind: 0,
          purpose_index: 0,
          script_hash: h_28_a,
          subject: cbor.serialise(0),
          siblings,
        },
      },
      inline_field_door(),
    ),
  }
}

test script_sources_stage_eight_finishes_empty_discovery_exactly() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let empty_discovery = validation_machine_v1.empty_script_discovery_control()
  let Pair(stage_eight, stage_eight_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    19,
  )
  let Pair(stage_eleven, _) = script_discovery_step_fixture(
    tx,
    11,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    20,
  )
  validation_machine_v1.verify_script_sources_stage_eight_finish_semantics_v1(
    stage_eight,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: stage_eight_cbor,
      claimed_successor: stage_eleven,
    },
  )
}

test script_sources_discovers_an_exact_non_native_execution() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
      redeemer_tx_wits: [redeemer],
    },
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let redeemer_cbor = encode_midgard_redeemer_witness(redeemer)
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(8, 0, redeemer_cbor)
  let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
    0,
    redeemer_item_commitment,
  )
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    #"0102",
  )
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let empty_discovery = validation_machine_v1.empty_script_discovery_control()
  let selected_purpose = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: script_hash,
    current_subject: #"0102",
  }
  let selected_source = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..selected_purpose,
    source_cursor: 1,
    matched_source_index: 0,
    matched_language_tag: 3,
    matched_source_leaf: source_leaf,
    used_inline_bitmap: 1,
  }
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(redeemer_cbor),
    redeemer_item_commitment,
    -1,
    -1,
  )
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let tail_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenTail,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  let selected_item = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..selected_source,
    redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      item_control,
    ),
  }
  let selected_tail = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..selected_source,
    redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      tail_control,
    ),
  }
  let execution_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.execution_leaf_hash(
        3,
        purpose_leaf,
        source_leaf,
        redeemer_leaf,
      ),
    ],
  )
  let completed = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    purpose_cursor: 1,
    used_inline_bitmap: 1,
    used_redeemer_bitmap: 1,
    execution_count: execution_frontier.count,
    execution_peaks: execution_frontier.peaks,
  }
  let Pair(stage_eight, stage_eight_cbor) = script_discovery_step_fixture(
    tx,
    8,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    empty_discovery,
    20,
  )
  let Pair(stage_nine, stage_nine_cbor) = script_discovery_step_fixture(
    tx,
    9,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_purpose,
    21,
  )
  let Pair(stage_ten, stage_ten_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_source,
    22,
  )
  let Pair(stage_ten_item, stage_ten_item_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_item,
    23,
  )
  let Pair(stage_ten_tail, stage_ten_tail_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_tail,
    24,
  )
  let Pair(stage_eight_done, _) = script_discovery_step_fixture(
    tx,
    8,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    completed,
    25,
  )
  let stage_ten_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_ten_cbor,
    claimed_successor: stage_ten_item,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_eight_purpose_semantics_v1(
      stage_eight,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: stage_eight_cbor,
        claimed_successor: stage_nine,
      },
      0,
      0,
      script_hash,
      #"0102",
      [],
    ),
    validation_machine_v1.verify_script_sources_stage_nine_effectful_match_semantics_v1(
      stage_nine,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: stage_nine_cbor,
        claimed_successor: stage_ten,
      },
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    validation_machine_v1.verify_one_step_evidence(
      stage_ten,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: stage_ten_transition,
        auxiliary: validation_machine_v1.RedeemerScanBeginWitness {
          item_index: 0,
          item_count: 1,
          total_length: bytearray.length(redeemer_cbor),
          item_commitment: redeemer_item_commitment,
          siblings: [],
        },
      },
      inline_field_door(),
    ),
    validation_machine_v1.verify_one_step_evidence(
      stage_ten_item,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: stage_ten_item_cbor,
          claimed_successor: stage_ten_tail,
        },
        auxiliary: validation_machine_v1.RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: header_witness,
        },
      },
      inline_field_door(),
    ),
    validation_machine_v1.verify_one_step_evidence(
      stage_ten_tail,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: stage_ten_tail_cbor,
          claimed_successor: stage_eight_done,
        },
        auxiliary: validation_machine_v1.RedeemerItemStepWitness {
          redeemer_control: None,
          control: tail_control,
          witness: tail_witness,
        },
      },
      inline_field_door(),
    ),
  }
}

test script_sources_stage_ten_redeemer_family_guards() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
      redeemer_tx_wits: [redeemer],
    },
  }
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let redeemer_cbor = encode_midgard_redeemer_witness(redeemer)
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(8, 0, redeemer_cbor)
  let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
    0,
    redeemer_item_commitment,
  )
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    #"0102",
  )
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let empty_discovery = validation_machine_v1.empty_script_discovery_control()
  let selected_purpose = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: script_hash,
    current_subject: #"0102",
  }
  let selected_source = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..selected_purpose,
    source_cursor: 1,
    matched_source_index: 0,
    matched_language_tag: 3,
    matched_source_leaf: source_leaf,
    used_inline_bitmap: 1,
  }
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(redeemer_cbor),
    redeemer_item_commitment,
    -1,
    -1,
  )
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let tail_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenTail,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  let selected_item = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..selected_source,
    redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      item_control,
    ),
  }
  let selected_tail = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..selected_source,
    redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      tail_control,
    ),
  }
  let execution_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.execution_leaf_hash(
        3,
        purpose_leaf,
        source_leaf,
        redeemer_leaf,
      ),
    ],
  )
  let completed = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    purpose_cursor: 1,
    used_inline_bitmap: 1,
    used_redeemer_bitmap: 1,
    execution_count: execution_frontier.count,
    execution_peaks: execution_frontier.peaks,
  }
  let Pair(stage_ten, stage_ten_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_source,
    22,
  )
  let Pair(stage_ten_item, stage_ten_item_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_item,
    23,
  )
  let Pair(stage_ten_tail, stage_ten_tail_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_tail,
    24,
  )
  let Pair(stage_eight_done, _) = script_discovery_step_fixture(
    tx,
    8,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    completed,
    25,
  )
  let stage_ten_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_ten_cbor,
    claimed_successor: stage_ten_item,
  }
  let stage_ten_item_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_ten_item_cbor,
    claimed_successor: stage_ten_tail,
  }
  let stage_ten_tail_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_ten_tail_cbor,
    claimed_successor: stage_eight_done,
  }
  let scan_begin_auxiliary = validation_machine_v1.RedeemerScanBeginWitness {
    item_index: 0,
    item_count: 1,
    total_length: bytearray.length(redeemer_cbor),
    item_commitment: redeemer_item_commitment,
    siblings: [],
  }
  let header_auxiliary = validation_machine_v1.RedeemerItemStepWitness {
    redeemer_control: None,
    control: item_control,
    witness: header_witness,
  }
  let tail_auxiliary = validation_machine_v1.RedeemerItemStepWitness {
    redeemer_control: None,
    control: tail_control,
    witness: tail_witness,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_ten_semantics_v1(
      stage_ten,
      stage_ten_transition,
      scan_begin_auxiliary,
    ),
    validation_machine_v1.verify_script_sources_stage_ten_mismatch_semantics_v1(
      stage_ten,
      stage_ten_transition,
      scan_begin_auxiliary,
    ),
    !validation_machine_v1.verify_script_sources_stage_ten_match_semantics_v1(
      stage_ten,
      stage_ten_transition,
      scan_begin_auxiliary,
    ),
    validation_machine_v1.verify_script_sources_stage_ten_semantics_v1(
      stage_ten_item,
      stage_ten_item_transition,
      header_auxiliary,
    ),
    validation_machine_v1.verify_script_sources_stage_ten_mismatch_semantics_v1(
      stage_ten_item,
      stage_ten_item_transition,
      header_auxiliary,
    ),
    !validation_machine_v1.verify_script_sources_stage_ten_match_semantics_v1(
      stage_ten_item,
      stage_ten_item_transition,
      header_auxiliary,
    ),
    validation_machine_v1.verify_script_sources_stage_ten_semantics_v1(
      stage_ten_tail,
      stage_ten_tail_transition,
      tail_auxiliary,
    ),
    validation_machine_v1.verify_script_sources_stage_ten_match_semantics_v1(
      stage_ten_tail,
      stage_ten_tail_transition,
      tail_auxiliary,
    ),
    !validation_machine_v1.verify_script_sources_stage_ten_mismatch_semantics_v1(
      stage_ten_tail,
      stage_ten_tail_transition,
      tail_auxiliary,
    ),
  }
}

test script_sources_stage_ten_proves_mismatch_and_missing_redeemer_exactly() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let singleton_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 1, h_28_a, #"0102")],
  )
  let discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..validation_machine_v1.empty_script_discovery_control(),
    source_cursor: 1,
    current_purpose_kind: 0,
    current_purpose_index: 1,
    current_script_hash: h_28_a,
    current_subject: #"0102",
    matched_source_index: 0,
    matched_language_tag: 3,
    matched_source_leaf: redeemer_leaf,
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    10,
    singleton_frontier,
    singleton_frontier,
    purpose_frontier,
    discovery,
    30,
  )
  let Pair(after_mismatch, after_mismatch_work_witness_cbor) = script_discovery_step_fixture(
    tx,
    10,
    singleton_frontier,
    singleton_frontier,
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..discovery,
      redeemer_cursor: 1,
    },
    31,
  )
  and {
    pre.program_counter == 30,
    work_witness_cbor != #"",
    after_mismatch.program_counter == 31,
    after_mismatch_work_witness_cbor != #"",
  }
}

test script_sources_stage_nine_accepts_an_exact_mismatch() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, #"0102")],
  )
  let discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..validation_machine_v1.empty_script_discovery_control(),
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: h_28_a,
    current_subject: #"0102",
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    9,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    discovery,
    24,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    empty_v1_transaction(),
    9,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..discovery,
      source_cursor: 1,
    },
    25,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_nine_mismatch_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !validation_machine_v1.verify_script_sources_stage_nine_effectful_match_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !validation_machine_v1.verify_script_sources_stage_nine_missing_semantics_v1(
      pre,
      transition,
    ),
  }
}

test script_sources_stage_nine_accepts_an_exact_effectful_match() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, script_hash, #"0102")],
  )
  let discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..validation_machine_v1.empty_script_discovery_control(),
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: script_hash,
    current_subject: #"0102",
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    9,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    discovery,
    26,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    empty_v1_transaction(),
    10,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..discovery,
      source_cursor: 1,
      matched_source_index: 0,
      matched_language_tag: 3,
      matched_source_leaf: source_leaf,
      used_inline_bitmap: 1,
    },
    27,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_nine_effectful_match_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !validation_machine_v1.verify_script_sources_stage_nine_mismatch_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !validation_machine_v1.verify_script_sources_stage_nine_native_match_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
  }
}

test script_sources_stage_nine_accepts_an_exact_native_match() {
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: #"820400",
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    #"0102",
  )
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.execution_leaf_hash(0, purpose_leaf, source_leaf, #"")],
  )
  let discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..validation_machine_v1.empty_script_discovery_control(),
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: script_hash,
    current_subject: #"0102",
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    9,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    discovery,
    28,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    empty_v1_transaction(),
    8,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..validation_machine_v1.empty_script_discovery_control(),
      purpose_cursor: 1,
      used_inline_bitmap: 1,
      execution_count: execution_frontier.count,
      execution_peaks: execution_frontier.peaks,
    },
    29,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_nine_native_match_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !validation_machine_v1.verify_script_sources_stage_nine_mismatch_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      0,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !validation_machine_v1.verify_script_sources_stage_nine_effectful_match_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      0,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
  }
}

test script_sources_stage_eleven_audits_inline_sources_exactly() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, script_hash, #"0102")],
  )
  let execution_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..validation_machine_v1.empty_script_discovery_control(),
    purpose_cursor: 1,
    used_inline_bitmap: 1,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    11,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    30,
  )
  let Pair(after_source, after_source_work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    11,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..discovery,
      source_cursor: 1,
    },
    31,
  )
  let Pair(after_finish, after_finish_work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    12,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..discovery,
      source_cursor: 1,
    },
    32,
  )
  let Pair(native_scripts, _) = native_scripts_step_fixture(
    empty_v1_transaction(),
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    0,
    0,
    33,
  )
  and {
    validation_machine_v1.verify_script_sources_stage_eleven_source_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: after_source,
      },
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    validation_machine_v1.verify_script_sources_stage_eleven_finish_semantics_v1(
      after_source,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: after_source_work_witness_cbor,
        claimed_successor: after_finish,
      },
    ),
    validation_machine_v1.verify_script_sources_stage_twelve_finish_semantics_v1(
      after_finish,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: after_finish_work_witness_cbor,
        claimed_successor: native_scripts,
      },
    ),
  }
}

test script_sources_stage_eleven_rejects_an_unused_inline_source_exactly() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    11,
    source_frontier,
    empty_frontier,
    empty_frontier,
    validation_machine_v1.empty_script_discovery_control(),
    30,
  )
  validation_machine_v1.verify_script_sources_stage_eleven_source_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(
        pre,
        #"455f494e56414c49445f4649454c445f54595045",
      ),
    },
    0,
    0,
    #"00",
    3,
    script_hash,
    bytearray.length(script_cbor),
    script_item_commitment,
    [],
  )
}

test script_sources_stage_twelve_audits_redeemers_exactly() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, #"0102")
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.execution_leaf_hash(
        3,
        purpose_leaf,
        redeemer_leaf,
        redeemer_leaf,
      ),
    ],
  )
  let used_discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..validation_machine_v1.empty_script_discovery_control(),
    purpose_cursor: 1,
    used_redeemer_bitmap: 1,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let Pair(used_pre, used_work_witness_cbor) = script_discovery_step_fixture(
    tx,
    12,
    empty_frontier,
    redeemer_frontier,
    purpose_frontier,
    used_discovery,
    40,
  )
  let Pair(used_post, _) = script_discovery_step_fixture(
    tx,
    12,
    empty_frontier,
    redeemer_frontier,
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..used_discovery,
      redeemer_cursor: 1,
    },
    41,
  )
  let Pair(unused_pre, unused_work_witness_cbor) = script_discovery_step_fixture(
    tx,
    12,
    empty_frontier,
    redeemer_frontier,
    empty_frontier,
    validation_machine_v1.empty_script_discovery_control(),
    40,
  )
  and {
    used_pre.program_counter == 40,
    used_work_witness_cbor != #"",
    used_post.program_counter == 41,
    unused_pre.program_counter == 40,
    unused_work_witness_cbor != #"",
  }
}

test script_sources_stage_twelve_redeemer_family_guard() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
      redeemer_tx_wits: [redeemer],
    },
  }
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, script_hash, #"0102")],
  )
  let redeemer_cbor = encode_midgard_redeemer_witness(redeemer)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(8, 0, redeemer_cbor)
  let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
    0,
    redeemer_item_commitment,
  )
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let empty_discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..validation_machine_v1.empty_script_discovery_control(),
    purpose_cursor: 1,
    source_cursor: 1,
    used_inline_bitmap: 1,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
  }
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(redeemer_cbor),
    redeemer_item_commitment,
    -1,
    -1,
  )
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    12,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    empty_discovery,
    32,
  )
  let Pair(begin_post, begin_work_witness_cbor) = script_discovery_step_fixture(
    tx,
    12,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..empty_discovery,
      redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
        item_control,
      ),
    },
    33,
  )
  let Pair(item_post, _) = script_discovery_step_fixture(
    tx,
    12,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..empty_discovery,
      redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
        tail_control,
      ),
    },
    34,
  )
  let begin_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: begin_post,
  }
  let item_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: begin_work_witness_cbor,
    claimed_successor: item_post,
  }
  let Pair(finish_pre, Pair(finish_work_witness_cbor, Pair(finish_post, _))) = stage_twelve_finish_fixture(
    #"",
  )
  let finish_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: finish_work_witness_cbor,
    claimed_successor: finish_post,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_twelve_redeemer_semantics_v1(
      pre,
      begin_transition,
      validation_machine_v1.RedeemerScanBeginWitness {
        item_index: 0,
        item_count: 1,
        total_length: bytearray.length(redeemer_cbor),
        item_commitment: redeemer_item_commitment,
        siblings: [],
      },
    ),
    validation_machine_v1.verify_script_sources_stage_twelve_redeemer_semantics_v1(
      begin_post,
      item_transition,
      validation_machine_v1.RedeemerItemStepWitness {
        redeemer_control: None,
        control: item_control,
        witness: header_witness,
      },
    ),
    !validation_machine_v1.verify_script_sources_stage_twelve_redeemer_semantics_v1(
      finish_pre,
      finish_transition,
      validation_machine_v1.NoAuxiliaryWitness,
    ),
  }
}

test script_sources_proves_a_missing_script_source_exactly() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let script_hash = h_28_a
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, script_hash, script_hash)],
  )
  let discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..validation_machine_v1.empty_script_discovery_control(),
    current_purpose_kind: 3,
    current_purpose_index: 0,
    current_script_hash: script_hash,
    current_subject: script_hash,
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    24,
  )
  let rejection_code = #"455f4d495353494e475f52455155495245445f5749544e455353"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 25,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      25,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  validation_machine_v1.verify_script_sources_stage_nine_missing_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: terminal,
    },
  )
}

fn late_phase_a_native_scan_witness(
  tx: MidgardTransaction,
  continuation_cbor: ByteArray,
  stage: Int,
  item_length: Int,
  item_commitment: ByteArray,
  cursor: Int,
  node_count: Int,
  result: Int,
) -> ByteArray {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  validation_machine_v1.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    blake2b_256(continuation_cbor),
    stage,
    1,
    0,
    0,
    item_length,
    item_commitment,
    cursor,
    #"",
    0,
    node_count,
    result,
    signer_frontier.count,
    signer_frontier.peaks,
    continuation_cbor,
  )
}

test native_scripts_stream_an_authenticated_execution_and_return_on_l1() {
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    // TimelockStart(0)
    script_bytes: #"820400",
  }
  let base = empty_v1_transaction()
  let valid_tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, validity_interval_start: 0 },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let chunk_proof = singleton_field_item_chunk_proof(6, script_cbor)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    0,
    script_hash,
    bytearray.length(script_cbor),
    bounded_item_v1.from_bytes(6, 0, script_cbor),
  )
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    #"0102",
  )
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.execution_leaf_hash(0, purpose_leaf, source_leaf, #"")],
  )
  let Pair(pre, work_witness_cbor) = native_scripts_step_fixture(
    valid_tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    0,
    0,
    30,
  )
  let Pair(post, _) = native_scripts_step_fixture(
    valid_tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    1,
    0,
    34,
  )
  let payload_offset = bytearray.length(script_cbor) - bytearray.length(
    script.script_bytes,
  )
  let entry_work_witness_cbor = late_phase_a_native_scan_witness(
    valid_tx,
    work_witness_cbor,
    1,
    bytearray.length(script_cbor),
    bounded_item_v1.from_bytes(6, 0, script_cbor),
    payload_offset,
    0,
    -1,
  )
  let entry_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.PhaseANativeScripts,
    program_counter: 31,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      31,
      entry_work_witness_cbor,
    ),
  }
  let payload_work_witness_cbor = late_phase_a_native_scan_witness(
    valid_tx,
    work_witness_cbor,
    7,
    bytearray.length(script_cbor),
    bounded_item_v1.from_bytes(6, 0, script_cbor),
    payload_offset + 2,
    1,
    -1,
  )
  let payload_pre = next_phase_a_native_state(
    entry_pre,
    payload_work_witness_cbor,
  )
  let finalize_work_witness_cbor = late_phase_a_native_scan_witness(
    valid_tx,
    work_witness_cbor,
    2,
    bytearray.length(script_cbor),
    bounded_item_v1.from_bytes(6, 0, script_cbor),
    bytearray.length(script_cbor),
    1,
    1,
  )
  let finalize_pre = next_phase_a_native_state(
    payload_pre,
    finalize_work_witness_cbor,
  )
  and {
    validation_machine_v1.verify_native_scripts_native_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: entry_pre,
      },
      0,
      0,
      0,
      script_hash,
      #"0102",
      [],
      0,
      0,
      #"00",
      bytearray.length(script_cbor),
      bounded_item_v1.from_bytes(6, 0, script_cbor),
      [],
      #"",
      [],
      chunk_proof,
      [],
    ),
    validation_machine_v1.verify_phase_a_native_token_head_semantics_v1(
      entry_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: entry_work_witness_cbor,
        claimed_successor: payload_pre,
      },
      chunk_proof,
      None,
    ),
    validation_machine_v1.verify_phase_a_native_timelock_payload_semantics_v1(
      payload_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: payload_work_witness_cbor,
        claimed_successor: finalize_pre,
      },
      chunk_proof,
      None,
    ),
    validation_machine_v1.verify_phase_a_native_advance_semantics_v1(
      finalize_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: finalize_work_witness_cbor,
        claimed_successor: post,
      },
    ),
  }
}

test script_integrity_is_recomputed_before_cek() {
  let empty_integrity_hash = #"01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53"
  let base = empty_v1_transaction()
  let valid_tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      script_integrity_hash: empty_integrity_hash,
    },
  }
  let valid_control = empty_native_control_fixture(valid_tx)
  let valid_source = midgard_transaction_proof_source_v1(valid_tx)
  let valid_compact = midgard_transaction_to_compact(valid_tx)
  let valid_witness_set = decode_native_tx_witness_set_compact(
    valid_source.witness_set_compact_cbor,
  )
  let authentication_witness_cbor = validation_machine_v1.encode_script_integrity_witness_v1(
    valid_control,
  )
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(valid_compact.body),
  )
  let authentication_pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(valid_tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ScriptIntegrity,
    program_counter: 34,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      34,
      authentication_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    // Re-based off `frontier_commitment(0, [])`: this fixture backs rejection
    // proofs, so it must carry the realistic non-empty claimed delta.
    ledger_delta_root: non_empty_claimed_delta_root(),
  }
  let compact_witness_cbor = validation_machine_v1.encode_script_integrity_compact_witness_v1(
    valid_control,
  )
  let compact_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..authentication_pre,
    program_counter: 35,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      35,
      compact_witness_cbor,
    ),
  }
  let witness_set_witness_cbor = validation_machine_v1.encode_script_integrity_witness_set_witness_v1(
    valid_control,
    valid_compact.body.script_integrity_hash,
    valid_compact.witness_set_hash,
  )
  let witness_set_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..compact_pre,
    program_counter: 36,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      36,
      witness_set_witness_cbor,
    ),
  }
  let finalize_witness_cbor = validation_machine_v1.encode_script_integrity_finalize_witness_v1(
    valid_control,
    valid_compact.body.script_integrity_hash,
    valid_witness_set.redeemer_tx_wits_hash,
  )
  let finalize_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..witness_set_pre,
    program_counter: 37,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      37,
      finalize_witness_cbor,
    ),
  }
  let valid_post = validation_trace_v1.ValidationMachineStateV1 {
    ..finalize_pre,
    phase: validation_trace_v1.Cek,
    program_counter: 38,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      38,
      validation_machine_v1.encode_cek_witness_v1(
        valid_control,
        #"",
        0,
        0,
        0,
        #"",
        0,
        0,
        #"",
      ),
    ),
  }
  let authentication_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: authentication_witness_cbor,
    claimed_successor: compact_pre,
  }
  let compact_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: compact_witness_cbor,
    claimed_successor: witness_set_pre,
  }
  let witness_set_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: witness_set_witness_cbor,
    claimed_successor: finalize_pre,
  }
  let finalize_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: finalize_witness_cbor,
    claimed_successor: valid_post,
  }
  let invalid_tx = base
  let invalid_control = empty_native_control_fixture(invalid_tx)
  let invalid_source = midgard_transaction_proof_source_v1(invalid_tx)
  let invalid_witness_set = decode_native_tx_witness_set_compact(
    invalid_source.witness_set_compact_cbor,
  )
  let invalid_compact = midgard_transaction_to_compact(invalid_tx)
  let invalid_tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(invalid_compact.body),
  )
  let invalid_finalize_witness_cbor = validation_machine_v1.encode_script_integrity_finalize_witness_v1(
    invalid_control,
    invalid_compact.body.script_integrity_hash,
    invalid_witness_set.redeemer_tx_wits_hash,
  )
  let invalid_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..authentication_pre,
    transaction_id: invalid_tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(invalid_tx),
    program_counter: 40,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      40,
      invalid_finalize_witness_cbor,
    ),
  }
  let rejection_code = #"455f494e56414c49445f4649454c445f54595045"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..invalid_pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      41,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        invalid_pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  and {
    validation_machine_v1.verify_script_integrity_authentication_semantics_v1(
      authentication_pre,
      authentication_transition,
    ),
    validation_machine_v1.verify_script_integrity_compact_semantics_v1(
      compact_pre,
      compact_transition,
    ),
    validation_machine_v1.verify_script_integrity_witness_set_semantics_v1(
      witness_set_pre,
      witness_set_transition,
    ),
    validation_machine_v1.verify_script_integrity_finalize_semantics_v1(
      finalize_pre,
      finalize_transition,
    ),
    validation_machine_v1.verify_script_integrity_finalize_semantics_v1(
      invalid_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: invalid_finalize_witness_cbor,
        claimed_successor: terminal,
      },
    ),
  }
}

test resolved_input_accumulator_vector_is_stable() {
  let initial = validation_machine_v1.initial_resolution_accumulator()
  and {
    initial == #"07eb401e2f7e5de17444414ec48a5d9dca455dea72f4675cc2b08bf5b4e39979",
    validation_machine_v1.resolved_input_accumulator_successor(
      initial,
      0,
      #"010203",
      #"040506",
    ) == #"97e2dbdabf1ac8b5046e02f46c8d081ade2d81296b174bf77b9b8c69bd59c9c0",
  }
}

test mint_asset_leaf_vector_is_stable() {
  validation_machine_v1.mint_asset_leaf_hash(
    #"11111111111111111111111111111111111111111111111111111111",
    #"abcd",
    -7,
  ) == #"4813bd9aad26eea82fa41280aefd50c848041a2a6cf27be416e1873c2876a479"
}

test ledger_delta_operation_frontier_vectors_are_stable() {
  let proof_descriptor = empty_mpf_proof_descriptor()
  let deletion = validation_machine_v1.ledger_delta_operation_leaf_hash(
    0,
    #"010203",
    #"",
    proof_descriptor,
  )
  let insertion = validation_machine_v1.ledger_delta_operation_leaf_hash(
    1,
    #"0405",
    #"060708",
    proof_descriptor,
  )
  let frontier = validation_merkle_v1.build_frontier([deletion, insertion])
  and {
    deletion == #"d70952a4347195627444cfbb1874f6857de1ad78f095460b76fc826cd267a589",
    insertion == #"f8bc7029f5f58f0436ebdf6cbbb85bd9adac05d5f6dc1b9238c8166a517aa8db",
    validation_merkle_v1.frontier_commitment(frontier.count, frontier.peaks) == #"b6d017c71f3fc974f620b22764385bf9ad56ee5627009e57dbeb9418e486dcb2",
  }
}

test ledger_delta_terminal_step_binds_more_than_128_operations() {
  // This is the off-chain-built frontier for 129 identical authenticated
  // deletions. Supplying the compact peaks measures only the actual terminal
  // resolver; constructing 129 leaves inside the test would charge fixture
  // setup to the transition.
  let operation_frontier = validation_merkle_v1.BuiltFrontier {
    count: 129,
    peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"d70952a4347195627444cfbb1874f6857de1ad78f095460b76fc826cd267a589",
      },
      validation_merkle_v1.FrontierPeak {
        height: 7,
        hash: #"c4029f40ad899fb153acbf60401c4b0187fc7ce03060d3cbeae09ea32de11cc9",
      },
    ],
  }
  let control = validation_machine_v1.LedgerDeltaControlV1 {
    resolved_input_count: 0,
    resolved_inputs_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    output_count: 0,
    output_descriptor_peaks: [],
    stage: 2,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    current_ledger_root: h_b,
    output_cursor: 0,
    operation_count: operation_frontier.count,
    operation_peaks: operation_frontier.peaks,
    pending_mutation_cbor: #"",
  }
  let work_witness_cbor = validation_machine_v1.encode_ledger_delta_control_v1(
    control,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: h_a,
    transaction_commitment: h_b,
    validation_context_hash: h_c,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.LedgerDelta,
    program_counter: 46,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      46,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_merkle_v1.frontier_commitment(
      operation_frontier.count,
      operation_frontier.peaks,
    ),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 47,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      47,
      validation_machine_v1.encode_terminal_acceptance_witness_v1(
        h_b,
        operation_frontier.count,
        operation_frontier.peaks,
      ),
    ),
    verdict: validation_trace_v1.Accepted,
  }
  validation_machine_v1.verify_ledger_delta_terminal_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
  )
}

fn maximum_program_envelope_selection_is_exact() -> Bool {
  let tx = empty_v1_transaction()
  let maximum_program_envelope = cek_proof_v1.encode_program_envelope_v1(
    1,
    1,
    0,
    h_a,
    cek_proof_v1.max_program_node_count,
    cek_proof_v1.max_program_material_byte_length,
  )
  let maximum_program_envelope_hash = cek_proof_v1.hash_program_envelope_v1(
    1,
    1,
    0,
    h_a,
    cek_proof_v1.max_program_node_count,
    cek_proof_v1.max_program_material_byte_length,
  )
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: maximum_program_envelope,
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let first_chunk_proof = singleton_field_item_chunk_proof(6, script_cbor)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let subject = #"00"
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    3,
    script_hash,
    bytearray.length(script_cbor),
    script_item_commitment,
  )
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    subject,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    3,
    purpose_leaf,
    source_leaf,
    h_c,
  )
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier([execution_leaf])
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    source_count: 1,
    source_peaks: source_frontier.peaks,
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  let context_control = validation_machine_v1.CekContextControlV1 {
    stage: 0,
    language_tag: 3,
    program_term_root: h_a,
    program_envelope_hash: maximum_program_envelope_hash,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash,
    subject,
    redeemer_leaf: h_c,
    redeemer_context_control_hash: #"",
    execution_memory_limit: 0,
    execution_cpu_limit: 0,
    reference_items: cek_data_v1.empty_data_list_summary_v1(),
    spend_items: cek_data_v1.empty_data_list_summary_v1(),
    output_items: cek_data_v1.empty_data_list_summary_v1(),
    signer_items: cek_data_v1.empty_data_list_summary_v1(),
    observer_count: 0,
    observer_items: cek_data_v1.empty_data_pair_summary_v1(),
    previous_observer: #"",
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_cursor: 0,
    current_mint_policy: #"",
    current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
    mint_policies: cek_data_v1.empty_data_pair_summary_v1(),
    mint_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(tx, control, #"", 0, 40)
  let Pair(post_base, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    41,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..post_base,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      41,
      validation_machine_v1.encode_cek_witness_v1(
        control,
        validation_machine_v1.encode_cek_context_control_v1(context_control),
        0,
        0,
        0,
        #"",
        0,
        0,
        maximum_program_envelope_hash,
      ),
    ),
  }
  let tampered_post = validation_trace_v1.ValidationMachineStateV1 {
    ..post,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      41,
      validation_machine_v1.encode_cek_witness_v1(
        control,
        validation_machine_v1.encode_cek_context_control_v1(
          validation_machine_v1.CekContextControlV1 {
            ..context_control,
            program_envelope_hash: h_b,
          },
        ),
        0,
        0,
        0,
        #"",
        0,
        0,
        h_b,
      ),
    ),
  }
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary: validation_machine_v1.NativeExecutionScanWitness {
      execution_index: 0,
      language_tag: 3,
      purpose_kind: 0,
      purpose_index: 0,
      script_hash,
      subject,
      purpose_siblings: [],
      source_index: 0,
      origin_kind: 0,
      source_key: #"00",
      script_total_length: bytearray.length(script_cbor),
      script_item_commitment,
      source_siblings: [],
      redeemer_leaf: h_c,
      execution_siblings: [],
      first_chunk_proof,
    },
  }
  and {
    bytearray.length(maximum_program_envelope) == 50,
    maximum_program_envelope_hash == #"e4fe7f19ef343b55fef5a5c4a80383dd4bbe4bc7009db0a3214bfec086584697",
    bytearray.length(cbor.serialise(evidence)) < 16_384,
    validation_machine_v1.verify_one_step_evidence(
      pre,
      evidence,
      inline_field_door(),
    ),
    !validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor,
          claimed_successor: tampered_post,
        },
        auxiliary: evidence.auxiliary,
      },
      inline_field_door(),
    ),
  }
}

test cek_execution_selection_authenticates_program_and_context_subject() {
  maximum_program_envelope_selection_is_exact()
}

test cek_effectful_selection_rejects_a_51_byte_program_envelope() {
  let oversized = builtin.replicate_byte(
    cek_proof_v1.max_program_envelope_cbor_bytes + 1,
    0,
  )
  and {
    bytearray.length(oversized) == 51,
    cek_proof_v1.inspect_program_envelope_v1(oversized) == None,
  }
}

// Expected-failure inversion keeps the payload byte opaque. The two exact
// pieces stay within Aiken's 8,192-byte `replicate_byte` optimizer bound while
// preserving the 16,379-byte Cardano maximum on the single generated case.
test maximum_native_then_effectful_cek_selection_is_bounded(
  fill_byte via fuzz.byte(),
) fail once {
  let tx = empty_v1_transaction()
  let native_script_bytes = builtin.replicate_byte(8_192, fill_byte)
    |> bytearray.concat(builtin.replicate_byte(8_187, fill_byte))
  let native_script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: native_script_bytes,
  }
  let native_script_cbor = encode_midgard_versioned_script(native_script)
  let native_commitment = bounded_item_v1.from_bytes(6, 0, native_script_cbor)
  let chunk_0 = builtin.slice_bytearray(
    0,
    bounded_item_v1.chunk_bytes,
    native_script_cbor,
  )
  let chunk_1 = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes,
    bounded_item_v1.chunk_bytes,
    native_script_cbor,
  )
  let chunk_2 = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes * 2,
    bounded_item_v1.chunk_bytes,
    native_script_cbor,
  )
  let chunk_3 = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes * 3,
    bounded_item_v1.chunk_bytes,
    native_script_cbor,
  )
  let chunk_4 = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes * 4,
    4,
    native_script_cbor,
  )
  let hash_0 = bounded_item_v1.hash_chunk(6, 0, 0, chunk_0)
  let hash_1 = bounded_item_v1.hash_chunk(6, 0, 1, chunk_1)
  let hash_2 = bounded_item_v1.hash_chunk(6, 0, 2, chunk_2)
  let hash_3 = bounded_item_v1.hash_chunk(6, 0, 3, chunk_3)
  let hash_4 = bounded_item_v1.hash_chunk(6, 0, 4, chunk_4)
  let chunk_frontier = validation_merkle_v1.build_frontier(
    [hash_0, hash_1, hash_2, hash_3, hash_4],
  )
  let first_chunk_proof = bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 6,
    item_index: 0,
    total_length: bytearray.length(native_script_cbor),
    chunk_index: 0,
    chunk: chunk_0,
    frontier: chunk_frontier.peaks,
    siblings: [hash_1, validation_merkle_v1.hash_branch(hash_2, hash_3)],
  }
  let effectful_script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let effectful_script_cbor = encode_midgard_versioned_script(effectful_script)
  let native_hash = script_proof_v1.versioned_script_hash(native_script)
  let effectful_hash = script_proof_v1.versioned_script_hash(effectful_script)
  let native_source = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    0,
    native_hash,
    bytearray.length(native_script_cbor),
    native_commitment,
  )
  let effectful_source = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"01",
    3,
    effectful_hash,
    bytearray.length(effectful_script_cbor),
    bounded_item_v1.from_bytes(6, 1, effectful_script_cbor),
  )
  let native_purpose = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    native_hash,
    #"00",
  )
  let effectful_purpose = script_proof_v1.purpose_leaf_hash(
    0,
    1,
    effectful_hash,
    #"01",
  )
  let native_execution = script_proof_v1.execution_leaf_hash(
    0,
    native_purpose,
    native_source,
    #"",
  )
  let effectful_execution = script_proof_v1.execution_leaf_hash(
    3,
    effectful_purpose,
    effectful_source,
    h_c,
  )
  let source_frontier = validation_merkle_v1.build_frontier(
    [native_source, effectful_source],
  )
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [native_purpose, effectful_purpose],
  )
  let execution_frontier = validation_merkle_v1.build_frontier(
    [native_execution, effectful_execution],
  )
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    source_count: 2,
    source_peaks: source_frontier.peaks,
    purpose_count: 2,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 2,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 2,
    language_bitmap: 1,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(tx, control, #"", 0, 43)
  let Pair(post, _) = cek_state_fixture(tx, control, #"", 1, 44)
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary: validation_machine_v1.NativeExecutionScanWitness {
      execution_index: 0,
      language_tag: 0,
      purpose_kind: 0,
      purpose_index: 0,
      script_hash: native_hash,
      subject: #"00",
      purpose_siblings: [effectful_purpose],
      source_index: 0,
      origin_kind: 0,
      source_key: #"00",
      script_total_length: bytearray.length(native_script_cbor),
      script_item_commitment: native_commitment,
      source_siblings: [effectful_source],
      redeemer_leaf: #"",
      execution_siblings: [effectful_execution],
      first_chunk_proof,
    },
  }
  !and {
    bytearray.length(native_script_bytes) == 16_379,
    bytearray.length(native_script_cbor) == 16_384,
    bytearray.length(first_chunk_proof.chunk) == bounded_item_v1.chunk_bytes,
    bytearray.length(cbor.serialise(evidence)) < 16_384,
    validation_machine_v1.verify_cek_execution_selection(
      pre,
      evidence.transition,
      evidence.auxiliary,
      control,
      0,
      0,
      0,
    ),
  }
}

test cross_language_cek_context_control_vectors() {
  let context = validation_machine_v1.CekContextControlV1 {
    stage: 0,
    language_tag: 3,
    program_term_root: h_a,
    program_envelope_hash: h_b,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: h_28_b,
    subject: #"00",
    redeemer_leaf: h_c,
    redeemer_context_control_hash: #"",
    execution_memory_limit: 0,
    execution_cpu_limit: 0,
    reference_items: cek_data_v1.empty_data_list_summary_v1(),
    spend_items: cek_data_v1.empty_data_list_summary_v1(),
    output_items: cek_data_v1.empty_data_list_summary_v1(),
    signer_items: cek_data_v1.empty_data_list_summary_v1(),
    observer_count: 0,
    observer_items: cek_data_v1.empty_data_pair_summary_v1(),
    previous_observer: #"",
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_cursor: 0,
    current_mint_policy: #"",
    current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
    mint_policies: cek_data_v1.empty_data_pair_summary_v1(),
    mint_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
  }
  and {
    validation_machine_v1.encode_cek_context_control_v1(context) == #"981900035820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa5820bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb0000581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb41005820cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc4000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa5600000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba200000040834000000040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba200000083400000",
    validation_machine_v1.hash_cek_redeemer_context_control_v1(
      validation_machine_v1.initial_cek_redeemer_context_control_v1(),
    ) == #"c56f80a85b46a479d9a766ce7050e825484f42da6453a3ec217afc6b03d5f938",
  }
}

test plutus_v3_receive_selection_rejects_with_an_exact_noop() {
  let tx = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let first_chunk_proof = singleton_field_item_chunk_proof(6, script_cbor)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    3,
    script_hash,
    bytearray.length(script_cbor),
    script_item_commitment,
  )
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    3,
    0,
    script_hash,
    script_hash,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    3,
    purpose_leaf,
    source_leaf,
    h_c,
  )
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier([execution_leaf])
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    source_count: 1,
    source_peaks: source_frontier.peaks,
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(tx, control, #"", 0, 40)
  let rejection_code = #"455f504c555455535f5343524950545f494e56414c4944"
  // Canonical exact-rejection successor: it carries the pre-state's non-empty
  // claimed delta forward. See the note on
  // `static_rules_prove_a_network_mismatch_is_an_exact_no_op`.
  let post = exact_rejection_post(pre, rejection_code)
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NativeExecutionScanWitness {
        execution_index: 0,
        language_tag: 3,
        purpose_kind: 3,
        purpose_index: 0,
        script_hash,
        subject: script_hash,
        purpose_siblings: [],
        source_index: 0,
        origin_kind: 0,
        source_key: #"00",
        script_total_length: bytearray.length(script_cbor),
        script_item_commitment,
        source_siblings: [],
        redeemer_leaf: h_c,
        execution_siblings: [],
        first_chunk_proof,
      },
    },
    inline_field_door(),
  )
}

test cek_redeemer_selection_binds_the_data_scan_initial_state() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"00",
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    redeemer_count: 1,
    redeemer_peaks: redeemer_frontier.peaks,
  }
  // Stage-0 redeemer selection is a *chain*, not a single step
  // (`verify_cek_redeemer_selection`, validation-machine-v1.ak:14967).
  // `RedeemerScanBeginWitness` only opens a bounded `mode_descriptor`
  // `redeemer_item_proof_v1` control and parks its hash in
  // `redeemer_context_control_hash` while the stage stays 0 (:15015-15020);
  // the stage only advances to 1 -- binding the *initial* redeemer-context
  // (data-scan) control and the authenticated execution-unit limits -- once a
  // `RedeemerItemStepWitness` drives that item control to
  // `stage_terminal` (:15053-15074). The whole chain is exercised here.
  let redeemer_item_cbor = encode_midgard_redeemer_witness(redeemer)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(
    8,
    0,
    redeemer_item_cbor,
  )
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(
    8,
    redeemer_item_cbor,
  )
  expect Some(redeemer_tag) = validation_machine_v1.redeemer_tag_for_purpose_kind_v1(
    0,
  )
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(redeemer_item_cbor),
    redeemer_item_commitment,
    redeemer_tag,
    0,
  )
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let tail_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenTail,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  let initial_context = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(0),
    redeemer_leaf,
    execution_memory_limit: 0,
    execution_cpu_limit: 0,
  }
  let opened_context = validation_machine_v1.CekContextControlV1 {
    ..initial_context,
    redeemer_context_control_hash: redeemer_item_proof_v1.hash_control_v1(
      item_control,
    ),
  }
  let tail_context = validation_machine_v1.CekContextControlV1 {
    ..initial_context,
    redeemer_context_control_hash: redeemer_item_proof_v1.hash_control_v1(
      tail_control,
    ),
  }
  let redeemer_context = validation_machine_v1.initial_cek_redeemer_context_control_v1()
  let selected_context = validation_machine_v1.CekContextControlV1 {
    ..initial_context,
    stage: 1,
    redeemer_context_control_hash: validation_machine_v1.hash_cek_redeemer_context_control_v1(
      redeemer_context,
    ),
    execution_memory_limit: redeemer.execution_units.memory,
    execution_cpu_limit: redeemer.execution_units.steps,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(initial_context),
    0,
    41,
  )
  let Pair(opened, opened_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(opened_context),
    0,
    42,
  )
  let Pair(tail, tail_work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(tail_context),
    0,
    43,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(selected_context),
    0,
    44,
  )
  and {
    validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor,
          claimed_successor: opened,
        },
        auxiliary: validation_machine_v1.RedeemerScanBeginWitness {
          item_index: 0,
          item_count: 1,
          total_length: bytearray.length(redeemer_item_cbor),
          item_commitment: redeemer_item_commitment,
          siblings: [],
        },
      },
      inline_field_door(),
    ),
    validation_machine_v1.verify_one_step_evidence(
      opened,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: opened_witness_cbor,
          claimed_successor: tail,
        },
        auxiliary: validation_machine_v1.RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: header_witness,
        },
      },
      inline_field_door(),
    ),
    validation_machine_v1.verify_one_step_evidence(
      tail,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: tail_work_witness_cbor,
          claimed_successor: post,
        },
        auxiliary: validation_machine_v1.RedeemerItemStepWitness {
          redeemer_control: None,
          control: tail_control,
          witness: tail_witness,
        },
      },
      inline_field_door(),
    ),
  }
}

fn exact_output_descriptor_fixture(
  output_index: Int,
  output_cbor: ByteArray,
) -> ByteArray {
  expect Some(cardano_tx_out) = script_context_v1.tx_out_summary_v1(
    output_cbor,
    False,
  )
  expect Some(midgard_tx_out) = script_context_v1.tx_out_summary_v1(
    output_cbor,
    True,
  )
  expect Some(cardano_spend_datum) = script_context_v1.spend_datum_summary_v1(
    output_cbor,
  )
  ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      version: ledger_output_commitment_v1.version,
      output_index,
      total_length: bytearray.length(output_cbor),
      item_commitment: bounded_item_v1.from_bytes(2, output_index, output_cbor),
      address: #"60" |> bytearray.concat(h_28_a),
      lovelace: 1,
      asset_count: 0,
      asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
      cardano_value_size: 0,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out,
      midgard_tx_out,
      cardano_spend_datum,
    },
  )
}

test cek_context_spend_item_uses_authenticated_descriptor_frontier() {
  let tx = empty_v1_transaction()
  let input = MidgardTxInput { tx_id: h_b, output_index: 2 }
  let key = encode_midgard_tx_input(input)
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 1, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let value = encode_midgard_tx_output(output)
  let descriptor_cbor = exact_output_descriptor_fixture(2, value)
  let substituted_key = encode_midgard_tx_input(
    MidgardTxInput { tx_id: h_b, output_index: 3 },
  )
  let resolved_leaf = script_proof_v1.resolved_context_item_leaf_hash(
    0,
    0,
    key,
    descriptor_cbor,
  )
  let resolved_frontier = validation_merkle_v1.build_frontier([resolved_leaf])
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, key)
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier([h_a])
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    resolved_input_count: 1,
    resolved_inputs_accumulator: validation_machine_v1.resolved_input_accumulator_successor(
      validation_machine_v1.initial_resolution_accumulator(),
      0,
      key,
      descriptor_cbor,
    ),
    spend_input_count: 1,
    resolved_item_peaks: resolved_frontier.peaks,
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  let context_control = validation_machine_v1.CekContextControlV1 {
    stage: 2,
    language_tag: 3,
    program_term_root: h_a,
    program_envelope_hash: h_b,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: h_28_a,
    subject: key,
    redeemer_leaf: h_c,
    redeemer_context_control_hash: validation_machine_v1.hash_cek_redeemer_context_control_v1(
      validation_machine_v1.initial_cek_redeemer_context_control_v1(),
    ),
    execution_memory_limit: 10,
    execution_cpu_limit: 20,
    reference_items: cek_data_v1.empty_data_list_summary_v1(),
    spend_items: cek_data_v1.empty_data_list_summary_v1(),
    output_items: cek_data_v1.empty_data_list_summary_v1(),
    signer_items: cek_data_v1.empty_data_list_summary_v1(),
    observer_count: 0,
    observer_items: cek_data_v1.empty_data_pair_summary_v1(),
    previous_observer: #"",
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_cursor: 0,
    current_mint_policy: #"",
    current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
    mint_policies: cek_data_v1.empty_data_pair_summary_v1(),
    mint_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
  }
  expect Some(next_spend_items) = script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
    1,
    resolved_frontier.peaks,
    1,
    0,
    0,
    key,
    descriptor_cbor,
    [],
    False,
    context_control.spend_items,
  )
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    41,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(
      validation_machine_v1.CekContextControlV1 {
        ..context_control,
        spend_items: next_spend_items,
      },
    ),
    0,
    42,
  )
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary: validation_machine_v1.CekResolvedContextItemWitness {
      source_kind: 0,
      item_index: 0,
      key,
      descriptor_cbor,
      siblings: [],
    },
  }
  and {
    validation_machine_v1.verify_cek_spend_context_step(
      pre,
      evidence.transition,
      evidence.auxiliary,
      control,
      context_control,
      0,
      0,
      0,
    ),
    script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
      1,
      resolved_frontier.peaks,
      1,
      0,
      0,
      substituted_key,
      descriptor_cbor,
      [],
      False,
      context_control.spend_items,
    ) == None,
  }
}

test cek_context_reference_item_uses_authenticated_descriptor_frontier() {
  let tx = empty_v1_transaction()
  let input = MidgardTxInput { tx_id: h_b, output_index: 2 }
  let key = encode_midgard_tx_input(input)
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 1, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let descriptor_cbor = exact_output_descriptor_fixture(
    2,
    encode_midgard_tx_output(output),
  )
  let resolved_leaf = script_proof_v1.resolved_context_item_leaf_hash(
    1,
    0,
    key,
    descriptor_cbor,
  )
  let resolved_frontier = validation_merkle_v1.build_frontier([resolved_leaf])
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    resolved_input_count: 1,
    resolved_inputs_accumulator: validation_machine_v1.resolved_input_accumulator_successor(
      validation_machine_v1.initial_resolution_accumulator(),
      1,
      key,
      descriptor_cbor,
    ),
    spend_input_count: 0,
    resolved_item_peaks: resolved_frontier.peaks,
    execution_cursor: 0,
  }
  let context_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(1),
    language_tag: 128,
  }
  expect Some(next_reference_items) = script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
    1,
    resolved_frontier.peaks,
    0,
    1,
    0,
    key,
    descriptor_cbor,
    [],
    True,
    context_control.reference_items,
  )
  let next_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    reference_items: next_reference_items,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    42,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(next_control),
    0,
    43,
  )
  validation_machine_v1.verify_cek_reference_context_step(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    validation_machine_v1.CekResolvedContextItemWitness {
      source_kind: 1,
      item_index: 0,
      key,
      descriptor_cbor,
      siblings: [],
    },
    control,
    context_control,
    0,
    0,
    0,
  )
}

test cek_context_output_item_uses_authenticated_descriptor_frontier() {
  let tx = empty_v1_transaction()
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 1, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let descriptor_cbor = exact_output_descriptor_fixture(0, output_cbor)
  let descriptor_leaf = script_proof_v1.output_descriptor_leaf_hash(
    0,
    descriptor_cbor,
  )
  let descriptor_frontier = validation_merkle_v1.build_frontier(
    [descriptor_leaf],
  )
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    output_count: 1,
    output_descriptor_peaks: descriptor_frontier.peaks,
  }
  let context_control = empty_cek_context_control_fixture(3)
  expect Some(next_output_items) = script_context_v1.prepend_output_descriptor_v1(
    1,
    descriptor_frontier.peaks,
    0,
    descriptor_cbor,
    [],
    False,
    context_control.output_items,
  )
  let next_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    output_items: next_output_items,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    43,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(next_control),
    0,
    44,
  )
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary: validation_machine_v1.CekOutputContextItemWitness {
      output_index: 0,
      descriptor_cbor,
      siblings: [],
    },
  }
  and {
    validation_machine_v1.verify_cek_output_context_step(
      pre,
      evidence.transition,
      evidence.auxiliary,
      control,
      context_control,
      0,
      0,
      0,
    ),
    script_context_v1.prepend_output_descriptor_v1(
      1,
      descriptor_frontier.peaks,
      0,
      exact_output_descriptor_fixture(
        0,
        encode_midgard_tx_output(
          MidgardTxOutput {
            ..output,
            value: MidgardValue { lovelace: 2, assets: [] },
          },
        ),
      ),
      [],
      False,
      context_control.output_items,
    ) == None,
  }
}

// ---------------------------------------------------------------------------
// The genuine-Cardano maximum-cardinality field terminal folds.
//
// Every constant below is GENERATED, and #592 is why they had to become
// constants at all.
//
//   producer: pnpm --dir demo/midgard-validation run fixtures:ordered-collection-boundary-aiken:sync
//
// Before the flat reversion these fixtures were struct literals inside `fn`s,
// carrying a counted `(ItemProofV1, ChunkProofV1)` pair the name-keyed rebinder
// (`demo/midgard-core/scripts/golden-channel.mjs`) could not reach — the hazard
// #590 exists to close. They stayed green while this package's producers moved
// to §4 at #585, because the transaction id they pinned was the id of the
// *stale* compact structure they pinned beside it: a self-consistent copy of a
// world that no longer existed. §8's carriage is the field's whole §5.1
// preimage, which no human is going to retype, so the fields are hoisted to
// named constants here and bound from the six boundary suites' published
// vectors.
// ---------------------------------------------------------------------------

// Derived by the genuine Cardano boundary fixture in
// demo/midgard-validation/tests/ordered-collection-spend-inputs-boundary-v1.test.ts
// (accepted 434 spend inputs at 16,379 signed bytes; adjacent 435 at 16,417).
const maximum_spend_input_terminal_transaction_id =
  #"1a24788b66a39ec3a980f70999a30cb532f8dbdd9e86d427666f1c710c821da2"

const maximum_spend_input_terminal_transaction_commitment =
  #"438e8bf2fa1a91166d32545aa48b202bfd6db6d518021dd441add0943e6a3b64"

const maximum_spend_input_terminal_compact_cbor =
  #"84018c5820c711b14605dab88657696940a8f08edb424dbf6d655398d0634b6ae04277a6aa582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820861703c5de65d91b0eb30c2749b083d9bb97a20e6dd52daebce7d43b9fc5ddeb1a000d5e192020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820b0fee96667c5707ce45fd0f9b4b06bbda781069e678ab53683e47cfa5e7299be00"

const maximum_spend_input_terminal_witness_set_compact_cbor =
  #"83582095902e309b4dab96b5f91c564b2e393cf88e259b5d0a8cd02a30acab22c02466582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const maximum_spend_input_terminal_field_preimage_lengths_cbor =
  #"891943d301183001010101186801"

const maximum_spend_input_terminal_field_preimage_cbor =
  #"9901b2582682582000000000000000000000000000000000000000000000000000000000000000001900005826825820000000000000000000000000000000000000000000000000000000000000000019000158268258200000000000000000000000000000000000000000000000000000000000000000190002582682582000000000000000000000000000000000000000000000000000000000000000001900035826825820000000000000000000000000000000000000000000000000000000000000000019000458268258200000000000000000000000000000000000000000000000000000000000000000190005582682582000000000000000000000000000000000000000000000000000000000000000001900065826825820000000000000000000000000000000000000000000000000000000000000000019000758268258200000000000000000000000000000000000000000000000000000000000000000190008582682582000000000000000000000000000000000000000000000000000000000000000001900095826825820000000000000000000000000000000000000000000000000000000000000000019000a5826825820000000000000000000000000000000000000000000000000000000000000000019000b5826825820000000000000000000000000000000000000000000000000000000000000000019000c5826825820000000000000000000000000000000000000000000000000000000000000000019000d5826825820000000000000000000000000000000000000000000000000000000000000000019000e5826825820000000000000000000000000000000000000000000000000000000000000000019000f582682582000000000000000000000000000000000000000000000000000000000000000001900105826825820000000000000000000000000000000000000000000000000000000000000000019001158268258200000000000000000000000000000000000000000000000000000000000000000190012582682582000000000000000000000000000000000000000000000000000000000000000001900135826825820000000000000000000000000000000000000000000000000000000000000000019001458268258200000000000000000000000000000000000000000000000000000000000000000190015582682582000000000000000000000000000000000000000000000000000000000000000001900165826825820000000000000000000000000000000000000000000000000000000000000000019001758268258200000000000000000000000000000000000000000000000000000000000000000190018582682582000000000000000000000000000000000000000000000000000000000000000001900195826825820000000000000000000000000000000000000000000000000000000000000000019001a5826825820000000000000000000000000000000000000000000000000000000000000000019001b5826825820000000000000000000000000000000000000000000000000000000000000000019001c5826825820000000000000000000000000000000000000000000000000000000000000000019001d5826825820000000000000000000000000000000000000000000000000000000000000000019001e5826825820000000000000000000000000000000000000000000000000000000000000000019001f582682582000000000000000000000000000000000000000000000000000000000000000001900205826825820000000000000000000000000000000000000000000000000000000000000000019002158268258200000000000000000000000000000000000000000000000000000000000000000190022582682582000000000000000000000000000000000000000000000000000000000000000001900235826825820000000000000000000000000000000000000000000000000000000000000000019002458268258200000000000000000000000000000000000000000000000000000000000000000190025582682582000000000000000000000000000000000000000000000000000000000000000001900265826825820000000000000000000000000000000000000000000000000000000000000000019002758268258200000000000000000000000000000000000000000000000000000000000000000190028582682582000000000000000000000000000000000000000000000000000000000000000001900295826825820000000000000000000000000000000000000000000000000000000000000000019002a5826825820000000000000000000000000000000000000000000000000000000000000000019002b5826825820000000000000000000000000000000000000000000000000000000000000000019002c5826825820000000000000000000000000000000000000000000000000000000000000000019002d5826825820000000000000000000000000000000000000000000000000000000000000000019002e5826825820000000000000000000000000000000000000000000000000000000000000000019002f582682582000000000000000000000000000000000000000000000000000000000000000001900305826825820000000000000000000000000000000000000000000000000000000000000000019003158268258200000000000000000000000000000000000000000000000000000000000000000190032582682582000000000000000000000000000000000000000000000000000000000000000001900335826825820000000000000000000000000000000000000000000000000000000000000000019003458268258200000000000000000000000000000000000000000000000000000000000000000190035582682582000000000000000000000000000000000000000000000000000000000000000001900365826825820000000000000000000000000000000000000000000000000000000000000000019003758268258200000000000000000000000000000000000000000000000000000000000000000190038582682582000000000000000000000000000000000000000000000000000000000000000001900395826825820000000000000000000000000000000000000000000000000000000000000000019003a5826825820000000000000000000000000000000000000000000000000000000000000000019003b5826825820000000000000000000000000000000000000000000000000000000000000000019003c5826825820000000000000000000000000000000000000000000000000000000000000000019003d5826825820000000000000000000000000000000000000000000000000000000000000000019003e5826825820000000000000000000000000000000000000000000000000000000000000000019003f582682582000000000000000000000000000000000000000000000000000000000000000001900405826825820000000000000000000000000000000000000000000000000000000000000000019004158268258200000000000000000000000000000000000000000000000000000000000000000190042582682582000000000000000000000000000000000000000000000000000000000000000001900435826825820000000000000000000000000000000000000000000000000000000000000000019004458268258200000000000000000000000000000000000000000000000000000000000000000190045582682582000000000000000000000000000000000000000000000000000000000000000001900465826825820000000000000000000000000000000000000000000000000000000000000000019004758268258200000000000000000000000000000000000000000000000000000000000000000190048582682582000000000000000000000000000000000000000000000000000000000000000001900495826825820000000000000000000000000000000000000000000000000000000000000000019004a5826825820000000000000000000000000000000000000000000000000000000000000000019004b5826825820000000000000000000000000000000000000000000000000000000000000000019004c5826825820000000000000000000000000000000000000000000000000000000000000000019004d5826825820000000000000000000000000000000000000000000000000000000000000000019004e5826825820000000000000000000000000000000000000000000000000000000000000000019004f582682582000000000000000000000000000000000000000000000000000000000000000001900505826825820000000000000000000000000000000000000000000000000000000000000000019005158268258200000000000000000000000000000000000000000000000000000000000000000190052582682582000000000000000000000000000000000000000000000000000000000000000001900535826825820000000000000000000000000000000000000000000000000000000000000000019005458268258200000000000000000000000000000000000000000000000000000000000000000190055582682582000000000000000000000000000000000000000000000000000000000000000001900565826825820000000000000000000000000000000000000000000000000000000000000000019005758268258200000000000000000000000000000000000000000000000000000000000000000190058582682582000000000000000000000000000000000000000000000000000000000000000001900595826825820000000000000000000000000000000000000000000000000000000000000000019005a5826825820000000000000000000000000000000000000000000000000000000000000000019005b5826825820000000000000000000000000000000000000000000000000000000000000000019005c5826825820000000000000000000000000000000000000000000000000000000000000000019005d5826825820000000000000000000000000000000000000000000000000000000000000000019005e5826825820000000000000000000000000000000000000000000000000000000000000000019005f582682582000000000000000000000000000000000000000000000000000000000000000001900605826825820000000000000000000000000000000000000000000000000000000000000000019006158268258200000000000000000000000000000000000000000000000000000000000000000190062582682582000000000000000000000000000000000000000000000000000000000000000001900635826825820000000000000000000000000000000000000000000000000000000000000000019006458268258200000000000000000000000000000000000000000000000000000000000000000190065582682582000000000000000000000000000000000000000000000000000000000000000001900665826825820000000000000000000000000000000000000000000000000000000000000000019006758268258200000000000000000000000000000000000000000000000000000000000000000190068582682582000000000000000000000000000000000000000000000000000000000000000001900695826825820000000000000000000000000000000000000000000000000000000000000000019006a5826825820000000000000000000000000000000000000000000000000000000000000000019006b5826825820000000000000000000000000000000000000000000000000000000000000000019006c5826825820000000000000000000000000000000000000000000000000000000000000000019006d5826825820000000000000000000000000000000000000000000000000000000000000000019006e5826825820000000000000000000000000000000000000000000000000000000000000000019006f582682582000000000000000000000000000000000000000000000000000000000000000001900705826825820000000000000000000000000000000000000000000000000000000000000000019007158268258200000000000000000000000000000000000000000000000000000000000000000190072582682582000000000000000000000000000000000000000000000000000000000000000001900735826825820000000000000000000000000000000000000000000000000000000000000000019007458268258200000000000000000000000000000000000000000000000000000000000000000190075582682582000000000000000000000000000000000000000000000000000000000000000001900765826825820000000000000000000000000000000000000000000000000000000000000000019007758268258200000000000000000000000000000000000000000000000000000000000000000190078582682582000000000000000000000000000000000000000000000000000000000000000001900795826825820000000000000000000000000000000000000000000000000000000000000000019007a5826825820000000000000000000000000000000000000000000000000000000000000000019007b5826825820000000000000000000000000000000000000000000000000000000000000000019007c5826825820000000000000000000000000000000000000000000000000000000000000000019007d5826825820000000000000000000000000000000000000000000000000000000000000000019007e5826825820000000000000000000000000000000000000000000000000000000000000000019007f582682582000000000000000000000000000000000000000000000000000000000000000001900805826825820000000000000000000000000000000000000000000000000000000000000000019008158268258200000000000000000000000000000000000000000000000000000000000000000190082582682582000000000000000000000000000000000000000000000000000000000000000001900835826825820000000000000000000000000000000000000000000000000000000000000000019008458268258200000000000000000000000000000000000000000000000000000000000000000190085582682582000000000000000000000000000000000000000000000000000000000000000001900865826825820000000000000000000000000000000000000000000000000000000000000000019008758268258200000000000000000000000000000000000000000000000000000000000000000190088582682582000000000000000000000000000000000000000000000000000000000000000001900895826825820000000000000000000000000000000000000000000000000000000000000000019008a5826825820000000000000000000000000000000000000000000000000000000000000000019008b5826825820000000000000000000000000000000000000000000000000000000000000000019008c5826825820000000000000000000000000000000000000000000000000000000000000000019008d5826825820000000000000000000000000000000000000000000000000000000000000000019008e5826825820000000000000000000000000000000000000000000000000000000000000000019008f582682582000000000000000000000000000000000000000000000000000000000000000001900905826825820000000000000000000000000000000000000000000000000000000000000000019009158268258200000000000000000000000000000000000000000000000000000000000000000190092582682582000000000000000000000000000000000000000000000000000000000000000001900935826825820000000000000000000000000000000000000000000000000000000000000000019009458268258200000000000000000000000000000000000000000000000000000000000000000190095582682582000000000000000000000000000000000000000000000000000000000000000001900965826825820000000000000000000000000000000000000000000000000000000000000000019009758268258200000000000000000000000000000000000000000000000000000000000000000190098582682582000000000000000000000000000000000000000000000000000000000000000001900995826825820000000000000000000000000000000000000000000000000000000000000000019009a5826825820000000000000000000000000000000000000000000000000000000000000000019009b5826825820000000000000000000000000000000000000000000000000000000000000000019009c5826825820000000000000000000000000000000000000000000000000000000000000000019009d5826825820000000000000000000000000000000000000000000000000000000000000000019009e5826825820000000000000000000000000000000000000000000000000000000000000000019009f582682582000000000000000000000000000000000000000000000000000000000000000001900a0582682582000000000000000000000000000000000000000000000000000000000000000001900a1582682582000000000000000000000000000000000000000000000000000000000000000001900a2582682582000000000000000000000000000000000000000000000000000000000000000001900a3582682582000000000000000000000000000000000000000000000000000000000000000001900a4582682582000000000000000000000000000000000000000000000000000000000000000001900a5582682582000000000000000000000000000000000000000000000000000000000000000001900a6582682582000000000000000000000000000000000000000000000000000000000000000001900a7582682582000000000000000000000000000000000000000000000000000000000000000001900a8582682582000000000000000000000000000000000000000000000000000000000000000001900a9582682582000000000000000000000000000000000000000000000000000000000000000001900aa582682582000000000000000000000000000000000000000000000000000000000000000001900ab582682582000000000000000000000000000000000000000000000000000000000000000001900ac582682582000000000000000000000000000000000000000000000000000000000000000001900ad582682582000000000000000000000000000000000000000000000000000000000000000001900ae582682582000000000000000000000000000000000000000000000000000000000000000001900af582682582000000000000000000000000000000000000000000000000000000000000000001900b0582682582000000000000000000000000000000000000000000000000000000000000000001900b1582682582000000000000000000000000000000000000000000000000000000000000000001900b2582682582000000000000000000000000000000000000000000000000000000000000000001900b3582682582000000000000000000000000000000000000000000000000000000000000000001900b4582682582000000000000000000000000000000000000000000000000000000000000000001900b5582682582000000000000000000000000000000000000000000000000000000000000000001900b6582682582000000000000000000000000000000000000000000000000000000000000000001900b7582682582000000000000000000000000000000000000000000000000000000000000000001900b8582682582000000000000000000000000000000000000000000000000000000000000000001900b9582682582000000000000000000000000000000000000000000000000000000000000000001900ba582682582000000000000000000000000000000000000000000000000000000000000000001900bb582682582000000000000000000000000000000000000000000000000000000000000000001900bc582682582000000000000000000000000000000000000000000000000000000000000000001900bd582682582000000000000000000000000000000000000000000000000000000000000000001900be582682582000000000000000000000000000000000000000000000000000000000000000001900bf582682582000000000000000000000000000000000000000000000000000000000000000001900c0582682582000000000000000000000000000000000000000000000000000000000000000001900c1582682582000000000000000000000000000000000000000000000000000000000000000001900c2582682582000000000000000000000000000000000000000000000000000000000000000001900c3582682582000000000000000000000000000000000000000000000000000000000000000001900c4582682582000000000000000000000000000000000000000000000000000000000000000001900c5582682582000000000000000000000000000000000000000000000000000000000000000001900c6582682582000000000000000000000000000000000000000000000000000000000000000001900c7582682582000000000000000000000000000000000000000000000000000000000000000001900c8582682582000000000000000000000000000000000000000000000000000000000000000001900c9582682582000000000000000000000000000000000000000000000000000000000000000001900ca582682582000000000000000000000000000000000000000000000000000000000000000001900cb582682582000000000000000000000000000000000000000000000000000000000000000001900cc582682582000000000000000000000000000000000000000000000000000000000000000001900cd582682582000000000000000000000000000000000000000000000000000000000000000001900ce582682582000000000000000000000000000000000000000000000000000000000000000001900cf582682582000000000000000000000000000000000000000000000000000000000000000001900d0582682582000000000000000000000000000000000000000000000000000000000000000001900d1582682582000000000000000000000000000000000000000000000000000000000000000001900d2582682582000000000000000000000000000000000000000000000000000000000000000001900d3582682582000000000000000000000000000000000000000000000000000000000000000001900d4582682582000000000000000000000000000000000000000000000000000000000000000001900d5582682582000000000000000000000000000000000000000000000000000000000000000001900d6582682582000000000000000000000000000000000000000000000000000000000000000001900d7582682582000000000000000000000000000000000000000000000000000000000000000001900d8582682582000000000000000000000000000000000000000000000000000000000000000001900d9582682582000000000000000000000000000000000000000000000000000000000000000001900da582682582000000000000000000000000000000000000000000000000000000000000000001900db582682582000000000000000000000000000000000000000000000000000000000000000001900dc582682582000000000000000000000000000000000000000000000000000000000000000001900dd582682582000000000000000000000000000000000000000000000000000000000000000001900de582682582000000000000000000000000000000000000000000000000000000000000000001900df582682582000000000000000000000000000000000000000000000000000000000000000001900e0582682582000000000000000000000000000000000000000000000000000000000000000001900e1582682582000000000000000000000000000000000000000000000000000000000000000001900e2582682582000000000000000000000000000000000000000000000000000000000000000001900e3582682582000000000000000000000000000000000000000000000000000000000000000001900e4582682582000000000000000000000000000000000000000000000000000000000000000001900e5582682582000000000000000000000000000000000000000000000000000000000000000001900e6582682582000000000000000000000000000000000000000000000000000000000000000001900e7582682582000000000000000000000000000000000000000000000000000000000000000001900e8582682582000000000000000000000000000000000000000000000000000000000000000001900e9582682582000000000000000000000000000000000000000000000000000000000000000001900ea582682582000000000000000000000000000000000000000000000000000000000000000001900eb582682582000000000000000000000000000000000000000000000000000000000000000001900ec582682582000000000000000000000000000000000000000000000000000000000000000001900ed582682582000000000000000000000000000000000000000000000000000000000000000001900ee582682582000000000000000000000000000000000000000000000000000000000000000001900ef582682582000000000000000000000000000000000000000000000000000000000000000001900f0582682582000000000000000000000000000000000000000000000000000000000000000001900f1582682582000000000000000000000000000000000000000000000000000000000000000001900f2582682582000000000000000000000000000000000000000000000000000000000000000001900f3582682582000000000000000000000000000000000000000000000000000000000000000001900f4582682582000000000000000000000000000000000000000000000000000000000000000001900f5582682582000000000000000000000000000000000000000000000000000000000000000001900f6582682582000000000000000000000000000000000000000000000000000000000000000001900f7582682582000000000000000000000000000000000000000000000000000000000000000001900f8582682582000000000000000000000000000000000000000000000000000000000000000001900f9582682582000000000000000000000000000000000000000000000000000000000000000001900fa582682582000000000000000000000000000000000000000000000000000000000000000001900fb582682582000000000000000000000000000000000000000000000000000000000000000001900fc582682582000000000000000000000000000000000000000000000000000000000000000001900fd582682582000000000000000000000000000000000000000000000000000000000000000001900fe582682582000000000000000000000000000000000000000000000000000000000000000001900ff582682582000000000000000000000000000000000000000000000000000000000000000001901005826825820000000000000000000000000000000000000000000000000000000000000000019010158268258200000000000000000000000000000000000000000000000000000000000000000190102582682582000000000000000000000000000000000000000000000000000000000000000001901035826825820000000000000000000000000000000000000000000000000000000000000000019010458268258200000000000000000000000000000000000000000000000000000000000000000190105582682582000000000000000000000000000000000000000000000000000000000000000001901065826825820000000000000000000000000000000000000000000000000000000000000000019010758268258200000000000000000000000000000000000000000000000000000000000000000190108582682582000000000000000000000000000000000000000000000000000000000000000001901095826825820000000000000000000000000000000000000000000000000000000000000000019010a5826825820000000000000000000000000000000000000000000000000000000000000000019010b5826825820000000000000000000000000000000000000000000000000000000000000000019010c5826825820000000000000000000000000000000000000000000000000000000000000000019010d5826825820000000000000000000000000000000000000000000000000000000000000000019010e5826825820000000000000000000000000000000000000000000000000000000000000000019010f582682582000000000000000000000000000000000000000000000000000000000000000001901105826825820000000000000000000000000000000000000000000000000000000000000000019011158268258200000000000000000000000000000000000000000000000000000000000000000190112582682582000000000000000000000000000000000000000000000000000000000000000001901135826825820000000000000000000000000000000000000000000000000000000000000000019011458268258200000000000000000000000000000000000000000000000000000000000000000190115582682582000000000000000000000000000000000000000000000000000000000000000001901165826825820000000000000000000000000000000000000000000000000000000000000000019011758268258200000000000000000000000000000000000000000000000000000000000000000190118582682582000000000000000000000000000000000000000000000000000000000000000001901195826825820000000000000000000000000000000000000000000000000000000000000000019011a5826825820000000000000000000000000000000000000000000000000000000000000000019011b5826825820000000000000000000000000000000000000000000000000000000000000000019011c5826825820000000000000000000000000000000000000000000000000000000000000000019011d5826825820000000000000000000000000000000000000000000000000000000000000000019011e5826825820000000000000000000000000000000000000000000000000000000000000000019011f582682582000000000000000000000000000000000000000000000000000000000000000001901205826825820000000000000000000000000000000000000000000000000000000000000000019012158268258200000000000000000000000000000000000000000000000000000000000000000190122582682582000000000000000000000000000000000000000000000000000000000000000001901235826825820000000000000000000000000000000000000000000000000000000000000000019012458268258200000000000000000000000000000000000000000000000000000000000000000190125582682582000000000000000000000000000000000000000000000000000000000000000001901265826825820000000000000000000000000000000000000000000000000000000000000000019012758268258200000000000000000000000000000000000000000000000000000000000000000190128582682582000000000000000000000000000000000000000000000000000000000000000001901295826825820000000000000000000000000000000000000000000000000000000000000000019012a5826825820000000000000000000000000000000000000000000000000000000000000000019012b5826825820000000000000000000000000000000000000000000000000000000000000000019012c5826825820000000000000000000000000000000000000000000000000000000000000000019012d5826825820000000000000000000000000000000000000000000000000000000000000000019012e5826825820000000000000000000000000000000000000000000000000000000000000000019012f582682582000000000000000000000000000000000000000000000000000000000000000001901305826825820000000000000000000000000000000000000000000000000000000000000000019013158268258200000000000000000000000000000000000000000000000000000000000000000190132582682582000000000000000000000000000000000000000000000000000000000000000001901335826825820000000000000000000000000000000000000000000000000000000000000000019013458268258200000000000000000000000000000000000000000000000000000000000000000190135582682582000000000000000000000000000000000000000000000000000000000000000001901365826825820000000000000000000000000000000000000000000000000000000000000000019013758268258200000000000000000000000000000000000000000000000000000000000000000190138582682582000000000000000000000000000000000000000000000000000000000000000001901395826825820000000000000000000000000000000000000000000000000000000000000000019013a5826825820000000000000000000000000000000000000000000000000000000000000000019013b5826825820000000000000000000000000000000000000000000000000000000000000000019013c5826825820000000000000000000000000000000000000000000000000000000000000000019013d5826825820000000000000000000000000000000000000000000000000000000000000000019013e5826825820000000000000000000000000000000000000000000000000000000000000000019013f582682582000000000000000000000000000000000000000000000000000000000000000001901405826825820000000000000000000000000000000000000000000000000000000000000000019014158268258200000000000000000000000000000000000000000000000000000000000000000190142582682582000000000000000000000000000000000000000000000000000000000000000001901435826825820000000000000000000000000000000000000000000000000000000000000000019014458268258200000000000000000000000000000000000000000000000000000000000000000190145582682582000000000000000000000000000000000000000000000000000000000000000001901465826825820000000000000000000000000000000000000000000000000000000000000000019014758268258200000000000000000000000000000000000000000000000000000000000000000190148582682582000000000000000000000000000000000000000000000000000000000000000001901495826825820000000000000000000000000000000000000000000000000000000000000000019014a5826825820000000000000000000000000000000000000000000000000000000000000000019014b5826825820000000000000000000000000000000000000000000000000000000000000000019014c5826825820000000000000000000000000000000000000000000000000000000000000000019014d5826825820000000000000000000000000000000000000000000000000000000000000000019014e5826825820000000000000000000000000000000000000000000000000000000000000000019014f582682582000000000000000000000000000000000000000000000000000000000000000001901505826825820000000000000000000000000000000000000000000000000000000000000000019015158268258200000000000000000000000000000000000000000000000000000000000000000190152582682582000000000000000000000000000000000000000000000000000000000000000001901535826825820000000000000000000000000000000000000000000000000000000000000000019015458268258200000000000000000000000000000000000000000000000000000000000000000190155582682582000000000000000000000000000000000000000000000000000000000000000001901565826825820000000000000000000000000000000000000000000000000000000000000000019015758268258200000000000000000000000000000000000000000000000000000000000000000190158582682582000000000000000000000000000000000000000000000000000000000000000001901595826825820000000000000000000000000000000000000000000000000000000000000000019015a5826825820000000000000000000000000000000000000000000000000000000000000000019015b5826825820000000000000000000000000000000000000000000000000000000000000000019015c5826825820000000000000000000000000000000000000000000000000000000000000000019015d5826825820000000000000000000000000000000000000000000000000000000000000000019015e5826825820000000000000000000000000000000000000000000000000000000000000000019015f582682582000000000000000000000000000000000000000000000000000000000000000001901605826825820000000000000000000000000000000000000000000000000000000000000000019016158268258200000000000000000000000000000000000000000000000000000000000000000190162582682582000000000000000000000000000000000000000000000000000000000000000001901635826825820000000000000000000000000000000000000000000000000000000000000000019016458268258200000000000000000000000000000000000000000000000000000000000000000190165582682582000000000000000000000000000000000000000000000000000000000000000001901665826825820000000000000000000000000000000000000000000000000000000000000000019016758268258200000000000000000000000000000000000000000000000000000000000000000190168582682582000000000000000000000000000000000000000000000000000000000000000001901695826825820000000000000000000000000000000000000000000000000000000000000000019016a5826825820000000000000000000000000000000000000000000000000000000000000000019016b5826825820000000000000000000000000000000000000000000000000000000000000000019016c5826825820000000000000000000000000000000000000000000000000000000000000000019016d5826825820000000000000000000000000000000000000000000000000000000000000000019016e5826825820000000000000000000000000000000000000000000000000000000000000000019016f582682582000000000000000000000000000000000000000000000000000000000000000001901705826825820000000000000000000000000000000000000000000000000000000000000000019017158268258200000000000000000000000000000000000000000000000000000000000000000190172582682582000000000000000000000000000000000000000000000000000000000000000001901735826825820000000000000000000000000000000000000000000000000000000000000000019017458268258200000000000000000000000000000000000000000000000000000000000000000190175582682582000000000000000000000000000000000000000000000000000000000000000001901765826825820000000000000000000000000000000000000000000000000000000000000000019017758268258200000000000000000000000000000000000000000000000000000000000000000190178582682582000000000000000000000000000000000000000000000000000000000000000001901795826825820000000000000000000000000000000000000000000000000000000000000000019017a5826825820000000000000000000000000000000000000000000000000000000000000000019017b5826825820000000000000000000000000000000000000000000000000000000000000000019017c5826825820000000000000000000000000000000000000000000000000000000000000000019017d5826825820000000000000000000000000000000000000000000000000000000000000000019017e5826825820000000000000000000000000000000000000000000000000000000000000000019017f582682582000000000000000000000000000000000000000000000000000000000000000001901805826825820000000000000000000000000000000000000000000000000000000000000000019018158268258200000000000000000000000000000000000000000000000000000000000000000190182582682582000000000000000000000000000000000000000000000000000000000000000001901835826825820000000000000000000000000000000000000000000000000000000000000000019018458268258200000000000000000000000000000000000000000000000000000000000000000190185582682582000000000000000000000000000000000000000000000000000000000000000001901865826825820000000000000000000000000000000000000000000000000000000000000000019018758268258200000000000000000000000000000000000000000000000000000000000000000190188582682582000000000000000000000000000000000000000000000000000000000000000001901895826825820000000000000000000000000000000000000000000000000000000000000000019018a5826825820000000000000000000000000000000000000000000000000000000000000000019018b5826825820000000000000000000000000000000000000000000000000000000000000000019018c5826825820000000000000000000000000000000000000000000000000000000000000000019018d5826825820000000000000000000000000000000000000000000000000000000000000000019018e5826825820000000000000000000000000000000000000000000000000000000000000000019018f582682582000000000000000000000000000000000000000000000000000000000000000001901905826825820000000000000000000000000000000000000000000000000000000000000000019019158268258200000000000000000000000000000000000000000000000000000000000000000190192582682582000000000000000000000000000000000000000000000000000000000000000001901935826825820000000000000000000000000000000000000000000000000000000000000000019019458268258200000000000000000000000000000000000000000000000000000000000000000190195582682582000000000000000000000000000000000000000000000000000000000000000001901965826825820000000000000000000000000000000000000000000000000000000000000000019019758268258200000000000000000000000000000000000000000000000000000000000000000190198582682582000000000000000000000000000000000000000000000000000000000000000001901995826825820000000000000000000000000000000000000000000000000000000000000000019019a5826825820000000000000000000000000000000000000000000000000000000000000000019019b5826825820000000000000000000000000000000000000000000000000000000000000000019019c5826825820000000000000000000000000000000000000000000000000000000000000000019019d5826825820000000000000000000000000000000000000000000000000000000000000000019019e5826825820000000000000000000000000000000000000000000000000000000000000000019019f582682582000000000000000000000000000000000000000000000000000000000000000001901a0582682582000000000000000000000000000000000000000000000000000000000000000001901a1582682582000000000000000000000000000000000000000000000000000000000000000001901a2582682582000000000000000000000000000000000000000000000000000000000000000001901a3582682582000000000000000000000000000000000000000000000000000000000000000001901a4582682582000000000000000000000000000000000000000000000000000000000000000001901a5582682582000000000000000000000000000000000000000000000000000000000000000001901a6582682582000000000000000000000000000000000000000000000000000000000000000001901a7582682582000000000000000000000000000000000000000000000000000000000000000001901a8582682582000000000000000000000000000000000000000000000000000000000000000001901a9582682582000000000000000000000000000000000000000000000000000000000000000001901aa582682582000000000000000000000000000000000000000000000000000000000000000001901ab582682582000000000000000000000000000000000000000000000000000000000000000001901ac582682582000000000000000000000000000000000000000000000000000000000000000001901ad582682582000000000000000000000000000000000000000000000000000000000000000001901ae582682582000000000000000000000000000000000000000000000000000000000000000001901af582682582000000000000000000000000000000000000000000000000000000000000000001901b0582682582000000000000000000000000000000000000000000000000000000000000000001901b1"

const maximum_spend_input_terminal_item_count = 434

const maximum_spend_input_terminal_item_index = 433

const maximum_spend_input_terminal_terminal_chunk_index = 0

const maximum_spend_input_terminal_encoded_length_before_item = 17323

const maximum_spend_input_terminal_pre_work_root =
  #"476250f69a5619d336a674f519abc6322f35504ad1b37de6e602086d3825fd42"

const maximum_spend_input_terminal_post_work_root =
  #"09dae6637ffa736e9bad178b60ad59a1572e6817d27d92b74a66602b423be606"

// Derived by the genuine Cardano boundary fixture in
// demo/midgard-validation/tests/ordered-collection-reference-inputs-boundary-v1.test.ts
// (accepted 433 reference inputs at 16,380 signed bytes; adjacent 434 at
// 16,418), sharing one spend input disjoint from every reference input.
const maximum_reference_input_terminal_transaction_id =
  #"a6bd2cb922f5d1052a1e1efd83bcdd11dd7721160080df149682cf417306b952"

const maximum_reference_input_terminal_transaction_commitment =
  #"40574dd42f376802bade6c4f6cd872b8a42ef2604bb0369eacde6945336fd387"

const maximum_reference_input_terminal_compact_cbor =
  #"84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e5820b6f6734af4f93c51274316e8954c613c86a0ecc5402b4b828ce31325900c817c58209ea027fa938e2c590b0b0352bd6f547b2ccd29b3938947830b3576943d7c0fb71a000d5e452020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff582073631c952fa8c19edcca913f23c451480f5be10695478f07a691ae19c7b1ccab00"

const maximum_reference_input_terminal_witness_set_compact_cbor =
  #"835820f675cfc328f48bfd88c4bc87b4a1e7ae69116c9c98d31b5d5166904637cc7c87582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const maximum_reference_input_terminal_field_preimage_lengths_cbor =
  #"8918291943ab182c01010101186801"

const maximum_reference_input_terminal_field_preimage_cbor =
  #"9901b15826825820000000000000000000000000000000000000000000000000000000000000000019000158268258200000000000000000000000000000000000000000000000000000000000000000190002582682582000000000000000000000000000000000000000000000000000000000000000001900035826825820000000000000000000000000000000000000000000000000000000000000000019000458268258200000000000000000000000000000000000000000000000000000000000000000190005582682582000000000000000000000000000000000000000000000000000000000000000001900065826825820000000000000000000000000000000000000000000000000000000000000000019000758268258200000000000000000000000000000000000000000000000000000000000000000190008582682582000000000000000000000000000000000000000000000000000000000000000001900095826825820000000000000000000000000000000000000000000000000000000000000000019000a5826825820000000000000000000000000000000000000000000000000000000000000000019000b5826825820000000000000000000000000000000000000000000000000000000000000000019000c5826825820000000000000000000000000000000000000000000000000000000000000000019000d5826825820000000000000000000000000000000000000000000000000000000000000000019000e5826825820000000000000000000000000000000000000000000000000000000000000000019000f582682582000000000000000000000000000000000000000000000000000000000000000001900105826825820000000000000000000000000000000000000000000000000000000000000000019001158268258200000000000000000000000000000000000000000000000000000000000000000190012582682582000000000000000000000000000000000000000000000000000000000000000001900135826825820000000000000000000000000000000000000000000000000000000000000000019001458268258200000000000000000000000000000000000000000000000000000000000000000190015582682582000000000000000000000000000000000000000000000000000000000000000001900165826825820000000000000000000000000000000000000000000000000000000000000000019001758268258200000000000000000000000000000000000000000000000000000000000000000190018582682582000000000000000000000000000000000000000000000000000000000000000001900195826825820000000000000000000000000000000000000000000000000000000000000000019001a5826825820000000000000000000000000000000000000000000000000000000000000000019001b5826825820000000000000000000000000000000000000000000000000000000000000000019001c5826825820000000000000000000000000000000000000000000000000000000000000000019001d5826825820000000000000000000000000000000000000000000000000000000000000000019001e5826825820000000000000000000000000000000000000000000000000000000000000000019001f582682582000000000000000000000000000000000000000000000000000000000000000001900205826825820000000000000000000000000000000000000000000000000000000000000000019002158268258200000000000000000000000000000000000000000000000000000000000000000190022582682582000000000000000000000000000000000000000000000000000000000000000001900235826825820000000000000000000000000000000000000000000000000000000000000000019002458268258200000000000000000000000000000000000000000000000000000000000000000190025582682582000000000000000000000000000000000000000000000000000000000000000001900265826825820000000000000000000000000000000000000000000000000000000000000000019002758268258200000000000000000000000000000000000000000000000000000000000000000190028582682582000000000000000000000000000000000000000000000000000000000000000001900295826825820000000000000000000000000000000000000000000000000000000000000000019002a5826825820000000000000000000000000000000000000000000000000000000000000000019002b5826825820000000000000000000000000000000000000000000000000000000000000000019002c5826825820000000000000000000000000000000000000000000000000000000000000000019002d5826825820000000000000000000000000000000000000000000000000000000000000000019002e5826825820000000000000000000000000000000000000000000000000000000000000000019002f582682582000000000000000000000000000000000000000000000000000000000000000001900305826825820000000000000000000000000000000000000000000000000000000000000000019003158268258200000000000000000000000000000000000000000000000000000000000000000190032582682582000000000000000000000000000000000000000000000000000000000000000001900335826825820000000000000000000000000000000000000000000000000000000000000000019003458268258200000000000000000000000000000000000000000000000000000000000000000190035582682582000000000000000000000000000000000000000000000000000000000000000001900365826825820000000000000000000000000000000000000000000000000000000000000000019003758268258200000000000000000000000000000000000000000000000000000000000000000190038582682582000000000000000000000000000000000000000000000000000000000000000001900395826825820000000000000000000000000000000000000000000000000000000000000000019003a5826825820000000000000000000000000000000000000000000000000000000000000000019003b5826825820000000000000000000000000000000000000000000000000000000000000000019003c5826825820000000000000000000000000000000000000000000000000000000000000000019003d5826825820000000000000000000000000000000000000000000000000000000000000000019003e5826825820000000000000000000000000000000000000000000000000000000000000000019003f582682582000000000000000000000000000000000000000000000000000000000000000001900405826825820000000000000000000000000000000000000000000000000000000000000000019004158268258200000000000000000000000000000000000000000000000000000000000000000190042582682582000000000000000000000000000000000000000000000000000000000000000001900435826825820000000000000000000000000000000000000000000000000000000000000000019004458268258200000000000000000000000000000000000000000000000000000000000000000190045582682582000000000000000000000000000000000000000000000000000000000000000001900465826825820000000000000000000000000000000000000000000000000000000000000000019004758268258200000000000000000000000000000000000000000000000000000000000000000190048582682582000000000000000000000000000000000000000000000000000000000000000001900495826825820000000000000000000000000000000000000000000000000000000000000000019004a5826825820000000000000000000000000000000000000000000000000000000000000000019004b5826825820000000000000000000000000000000000000000000000000000000000000000019004c5826825820000000000000000000000000000000000000000000000000000000000000000019004d5826825820000000000000000000000000000000000000000000000000000000000000000019004e5826825820000000000000000000000000000000000000000000000000000000000000000019004f582682582000000000000000000000000000000000000000000000000000000000000000001900505826825820000000000000000000000000000000000000000000000000000000000000000019005158268258200000000000000000000000000000000000000000000000000000000000000000190052582682582000000000000000000000000000000000000000000000000000000000000000001900535826825820000000000000000000000000000000000000000000000000000000000000000019005458268258200000000000000000000000000000000000000000000000000000000000000000190055582682582000000000000000000000000000000000000000000000000000000000000000001900565826825820000000000000000000000000000000000000000000000000000000000000000019005758268258200000000000000000000000000000000000000000000000000000000000000000190058582682582000000000000000000000000000000000000000000000000000000000000000001900595826825820000000000000000000000000000000000000000000000000000000000000000019005a5826825820000000000000000000000000000000000000000000000000000000000000000019005b5826825820000000000000000000000000000000000000000000000000000000000000000019005c5826825820000000000000000000000000000000000000000000000000000000000000000019005d5826825820000000000000000000000000000000000000000000000000000000000000000019005e5826825820000000000000000000000000000000000000000000000000000000000000000019005f582682582000000000000000000000000000000000000000000000000000000000000000001900605826825820000000000000000000000000000000000000000000000000000000000000000019006158268258200000000000000000000000000000000000000000000000000000000000000000190062582682582000000000000000000000000000000000000000000000000000000000000000001900635826825820000000000000000000000000000000000000000000000000000000000000000019006458268258200000000000000000000000000000000000000000000000000000000000000000190065582682582000000000000000000000000000000000000000000000000000000000000000001900665826825820000000000000000000000000000000000000000000000000000000000000000019006758268258200000000000000000000000000000000000000000000000000000000000000000190068582682582000000000000000000000000000000000000000000000000000000000000000001900695826825820000000000000000000000000000000000000000000000000000000000000000019006a5826825820000000000000000000000000000000000000000000000000000000000000000019006b5826825820000000000000000000000000000000000000000000000000000000000000000019006c5826825820000000000000000000000000000000000000000000000000000000000000000019006d5826825820000000000000000000000000000000000000000000000000000000000000000019006e5826825820000000000000000000000000000000000000000000000000000000000000000019006f582682582000000000000000000000000000000000000000000000000000000000000000001900705826825820000000000000000000000000000000000000000000000000000000000000000019007158268258200000000000000000000000000000000000000000000000000000000000000000190072582682582000000000000000000000000000000000000000000000000000000000000000001900735826825820000000000000000000000000000000000000000000000000000000000000000019007458268258200000000000000000000000000000000000000000000000000000000000000000190075582682582000000000000000000000000000000000000000000000000000000000000000001900765826825820000000000000000000000000000000000000000000000000000000000000000019007758268258200000000000000000000000000000000000000000000000000000000000000000190078582682582000000000000000000000000000000000000000000000000000000000000000001900795826825820000000000000000000000000000000000000000000000000000000000000000019007a5826825820000000000000000000000000000000000000000000000000000000000000000019007b5826825820000000000000000000000000000000000000000000000000000000000000000019007c5826825820000000000000000000000000000000000000000000000000000000000000000019007d5826825820000000000000000000000000000000000000000000000000000000000000000019007e5826825820000000000000000000000000000000000000000000000000000000000000000019007f582682582000000000000000000000000000000000000000000000000000000000000000001900805826825820000000000000000000000000000000000000000000000000000000000000000019008158268258200000000000000000000000000000000000000000000000000000000000000000190082582682582000000000000000000000000000000000000000000000000000000000000000001900835826825820000000000000000000000000000000000000000000000000000000000000000019008458268258200000000000000000000000000000000000000000000000000000000000000000190085582682582000000000000000000000000000000000000000000000000000000000000000001900865826825820000000000000000000000000000000000000000000000000000000000000000019008758268258200000000000000000000000000000000000000000000000000000000000000000190088582682582000000000000000000000000000000000000000000000000000000000000000001900895826825820000000000000000000000000000000000000000000000000000000000000000019008a5826825820000000000000000000000000000000000000000000000000000000000000000019008b5826825820000000000000000000000000000000000000000000000000000000000000000019008c5826825820000000000000000000000000000000000000000000000000000000000000000019008d5826825820000000000000000000000000000000000000000000000000000000000000000019008e5826825820000000000000000000000000000000000000000000000000000000000000000019008f582682582000000000000000000000000000000000000000000000000000000000000000001900905826825820000000000000000000000000000000000000000000000000000000000000000019009158268258200000000000000000000000000000000000000000000000000000000000000000190092582682582000000000000000000000000000000000000000000000000000000000000000001900935826825820000000000000000000000000000000000000000000000000000000000000000019009458268258200000000000000000000000000000000000000000000000000000000000000000190095582682582000000000000000000000000000000000000000000000000000000000000000001900965826825820000000000000000000000000000000000000000000000000000000000000000019009758268258200000000000000000000000000000000000000000000000000000000000000000190098582682582000000000000000000000000000000000000000000000000000000000000000001900995826825820000000000000000000000000000000000000000000000000000000000000000019009a5826825820000000000000000000000000000000000000000000000000000000000000000019009b5826825820000000000000000000000000000000000000000000000000000000000000000019009c5826825820000000000000000000000000000000000000000000000000000000000000000019009d5826825820000000000000000000000000000000000000000000000000000000000000000019009e5826825820000000000000000000000000000000000000000000000000000000000000000019009f582682582000000000000000000000000000000000000000000000000000000000000000001900a0582682582000000000000000000000000000000000000000000000000000000000000000001900a1582682582000000000000000000000000000000000000000000000000000000000000000001900a2582682582000000000000000000000000000000000000000000000000000000000000000001900a3582682582000000000000000000000000000000000000000000000000000000000000000001900a4582682582000000000000000000000000000000000000000000000000000000000000000001900a5582682582000000000000000000000000000000000000000000000000000000000000000001900a6582682582000000000000000000000000000000000000000000000000000000000000000001900a7582682582000000000000000000000000000000000000000000000000000000000000000001900a8582682582000000000000000000000000000000000000000000000000000000000000000001900a9582682582000000000000000000000000000000000000000000000000000000000000000001900aa582682582000000000000000000000000000000000000000000000000000000000000000001900ab582682582000000000000000000000000000000000000000000000000000000000000000001900ac582682582000000000000000000000000000000000000000000000000000000000000000001900ad582682582000000000000000000000000000000000000000000000000000000000000000001900ae582682582000000000000000000000000000000000000000000000000000000000000000001900af582682582000000000000000000000000000000000000000000000000000000000000000001900b0582682582000000000000000000000000000000000000000000000000000000000000000001900b1582682582000000000000000000000000000000000000000000000000000000000000000001900b2582682582000000000000000000000000000000000000000000000000000000000000000001900b3582682582000000000000000000000000000000000000000000000000000000000000000001900b4582682582000000000000000000000000000000000000000000000000000000000000000001900b5582682582000000000000000000000000000000000000000000000000000000000000000001900b6582682582000000000000000000000000000000000000000000000000000000000000000001900b7582682582000000000000000000000000000000000000000000000000000000000000000001900b8582682582000000000000000000000000000000000000000000000000000000000000000001900b9582682582000000000000000000000000000000000000000000000000000000000000000001900ba582682582000000000000000000000000000000000000000000000000000000000000000001900bb582682582000000000000000000000000000000000000000000000000000000000000000001900bc582682582000000000000000000000000000000000000000000000000000000000000000001900bd582682582000000000000000000000000000000000000000000000000000000000000000001900be582682582000000000000000000000000000000000000000000000000000000000000000001900bf582682582000000000000000000000000000000000000000000000000000000000000000001900c0582682582000000000000000000000000000000000000000000000000000000000000000001900c1582682582000000000000000000000000000000000000000000000000000000000000000001900c2582682582000000000000000000000000000000000000000000000000000000000000000001900c3582682582000000000000000000000000000000000000000000000000000000000000000001900c4582682582000000000000000000000000000000000000000000000000000000000000000001900c5582682582000000000000000000000000000000000000000000000000000000000000000001900c6582682582000000000000000000000000000000000000000000000000000000000000000001900c7582682582000000000000000000000000000000000000000000000000000000000000000001900c8582682582000000000000000000000000000000000000000000000000000000000000000001900c9582682582000000000000000000000000000000000000000000000000000000000000000001900ca582682582000000000000000000000000000000000000000000000000000000000000000001900cb582682582000000000000000000000000000000000000000000000000000000000000000001900cc582682582000000000000000000000000000000000000000000000000000000000000000001900cd582682582000000000000000000000000000000000000000000000000000000000000000001900ce582682582000000000000000000000000000000000000000000000000000000000000000001900cf582682582000000000000000000000000000000000000000000000000000000000000000001900d0582682582000000000000000000000000000000000000000000000000000000000000000001900d1582682582000000000000000000000000000000000000000000000000000000000000000001900d2582682582000000000000000000000000000000000000000000000000000000000000000001900d3582682582000000000000000000000000000000000000000000000000000000000000000001900d4582682582000000000000000000000000000000000000000000000000000000000000000001900d5582682582000000000000000000000000000000000000000000000000000000000000000001900d6582682582000000000000000000000000000000000000000000000000000000000000000001900d7582682582000000000000000000000000000000000000000000000000000000000000000001900d8582682582000000000000000000000000000000000000000000000000000000000000000001900d9582682582000000000000000000000000000000000000000000000000000000000000000001900da582682582000000000000000000000000000000000000000000000000000000000000000001900db582682582000000000000000000000000000000000000000000000000000000000000000001900dc582682582000000000000000000000000000000000000000000000000000000000000000001900dd582682582000000000000000000000000000000000000000000000000000000000000000001900de582682582000000000000000000000000000000000000000000000000000000000000000001900df582682582000000000000000000000000000000000000000000000000000000000000000001900e0582682582000000000000000000000000000000000000000000000000000000000000000001900e1582682582000000000000000000000000000000000000000000000000000000000000000001900e2582682582000000000000000000000000000000000000000000000000000000000000000001900e3582682582000000000000000000000000000000000000000000000000000000000000000001900e4582682582000000000000000000000000000000000000000000000000000000000000000001900e5582682582000000000000000000000000000000000000000000000000000000000000000001900e6582682582000000000000000000000000000000000000000000000000000000000000000001900e7582682582000000000000000000000000000000000000000000000000000000000000000001900e8582682582000000000000000000000000000000000000000000000000000000000000000001900e9582682582000000000000000000000000000000000000000000000000000000000000000001900ea582682582000000000000000000000000000000000000000000000000000000000000000001900eb582682582000000000000000000000000000000000000000000000000000000000000000001900ec582682582000000000000000000000000000000000000000000000000000000000000000001900ed582682582000000000000000000000000000000000000000000000000000000000000000001900ee582682582000000000000000000000000000000000000000000000000000000000000000001900ef582682582000000000000000000000000000000000000000000000000000000000000000001900f0582682582000000000000000000000000000000000000000000000000000000000000000001900f1582682582000000000000000000000000000000000000000000000000000000000000000001900f2582682582000000000000000000000000000000000000000000000000000000000000000001900f3582682582000000000000000000000000000000000000000000000000000000000000000001900f4582682582000000000000000000000000000000000000000000000000000000000000000001900f5582682582000000000000000000000000000000000000000000000000000000000000000001900f6582682582000000000000000000000000000000000000000000000000000000000000000001900f7582682582000000000000000000000000000000000000000000000000000000000000000001900f8582682582000000000000000000000000000000000000000000000000000000000000000001900f9582682582000000000000000000000000000000000000000000000000000000000000000001900fa582682582000000000000000000000000000000000000000000000000000000000000000001900fb582682582000000000000000000000000000000000000000000000000000000000000000001900fc582682582000000000000000000000000000000000000000000000000000000000000000001900fd582682582000000000000000000000000000000000000000000000000000000000000000001900fe582682582000000000000000000000000000000000000000000000000000000000000000001900ff582682582000000000000000000000000000000000000000000000000000000000000000001901005826825820000000000000000000000000000000000000000000000000000000000000000019010158268258200000000000000000000000000000000000000000000000000000000000000000190102582682582000000000000000000000000000000000000000000000000000000000000000001901035826825820000000000000000000000000000000000000000000000000000000000000000019010458268258200000000000000000000000000000000000000000000000000000000000000000190105582682582000000000000000000000000000000000000000000000000000000000000000001901065826825820000000000000000000000000000000000000000000000000000000000000000019010758268258200000000000000000000000000000000000000000000000000000000000000000190108582682582000000000000000000000000000000000000000000000000000000000000000001901095826825820000000000000000000000000000000000000000000000000000000000000000019010a5826825820000000000000000000000000000000000000000000000000000000000000000019010b5826825820000000000000000000000000000000000000000000000000000000000000000019010c5826825820000000000000000000000000000000000000000000000000000000000000000019010d5826825820000000000000000000000000000000000000000000000000000000000000000019010e5826825820000000000000000000000000000000000000000000000000000000000000000019010f582682582000000000000000000000000000000000000000000000000000000000000000001901105826825820000000000000000000000000000000000000000000000000000000000000000019011158268258200000000000000000000000000000000000000000000000000000000000000000190112582682582000000000000000000000000000000000000000000000000000000000000000001901135826825820000000000000000000000000000000000000000000000000000000000000000019011458268258200000000000000000000000000000000000000000000000000000000000000000190115582682582000000000000000000000000000000000000000000000000000000000000000001901165826825820000000000000000000000000000000000000000000000000000000000000000019011758268258200000000000000000000000000000000000000000000000000000000000000000190118582682582000000000000000000000000000000000000000000000000000000000000000001901195826825820000000000000000000000000000000000000000000000000000000000000000019011a5826825820000000000000000000000000000000000000000000000000000000000000000019011b5826825820000000000000000000000000000000000000000000000000000000000000000019011c5826825820000000000000000000000000000000000000000000000000000000000000000019011d5826825820000000000000000000000000000000000000000000000000000000000000000019011e5826825820000000000000000000000000000000000000000000000000000000000000000019011f582682582000000000000000000000000000000000000000000000000000000000000000001901205826825820000000000000000000000000000000000000000000000000000000000000000019012158268258200000000000000000000000000000000000000000000000000000000000000000190122582682582000000000000000000000000000000000000000000000000000000000000000001901235826825820000000000000000000000000000000000000000000000000000000000000000019012458268258200000000000000000000000000000000000000000000000000000000000000000190125582682582000000000000000000000000000000000000000000000000000000000000000001901265826825820000000000000000000000000000000000000000000000000000000000000000019012758268258200000000000000000000000000000000000000000000000000000000000000000190128582682582000000000000000000000000000000000000000000000000000000000000000001901295826825820000000000000000000000000000000000000000000000000000000000000000019012a5826825820000000000000000000000000000000000000000000000000000000000000000019012b5826825820000000000000000000000000000000000000000000000000000000000000000019012c5826825820000000000000000000000000000000000000000000000000000000000000000019012d5826825820000000000000000000000000000000000000000000000000000000000000000019012e5826825820000000000000000000000000000000000000000000000000000000000000000019012f582682582000000000000000000000000000000000000000000000000000000000000000001901305826825820000000000000000000000000000000000000000000000000000000000000000019013158268258200000000000000000000000000000000000000000000000000000000000000000190132582682582000000000000000000000000000000000000000000000000000000000000000001901335826825820000000000000000000000000000000000000000000000000000000000000000019013458268258200000000000000000000000000000000000000000000000000000000000000000190135582682582000000000000000000000000000000000000000000000000000000000000000001901365826825820000000000000000000000000000000000000000000000000000000000000000019013758268258200000000000000000000000000000000000000000000000000000000000000000190138582682582000000000000000000000000000000000000000000000000000000000000000001901395826825820000000000000000000000000000000000000000000000000000000000000000019013a5826825820000000000000000000000000000000000000000000000000000000000000000019013b5826825820000000000000000000000000000000000000000000000000000000000000000019013c5826825820000000000000000000000000000000000000000000000000000000000000000019013d5826825820000000000000000000000000000000000000000000000000000000000000000019013e5826825820000000000000000000000000000000000000000000000000000000000000000019013f582682582000000000000000000000000000000000000000000000000000000000000000001901405826825820000000000000000000000000000000000000000000000000000000000000000019014158268258200000000000000000000000000000000000000000000000000000000000000000190142582682582000000000000000000000000000000000000000000000000000000000000000001901435826825820000000000000000000000000000000000000000000000000000000000000000019014458268258200000000000000000000000000000000000000000000000000000000000000000190145582682582000000000000000000000000000000000000000000000000000000000000000001901465826825820000000000000000000000000000000000000000000000000000000000000000019014758268258200000000000000000000000000000000000000000000000000000000000000000190148582682582000000000000000000000000000000000000000000000000000000000000000001901495826825820000000000000000000000000000000000000000000000000000000000000000019014a5826825820000000000000000000000000000000000000000000000000000000000000000019014b5826825820000000000000000000000000000000000000000000000000000000000000000019014c5826825820000000000000000000000000000000000000000000000000000000000000000019014d5826825820000000000000000000000000000000000000000000000000000000000000000019014e5826825820000000000000000000000000000000000000000000000000000000000000000019014f582682582000000000000000000000000000000000000000000000000000000000000000001901505826825820000000000000000000000000000000000000000000000000000000000000000019015158268258200000000000000000000000000000000000000000000000000000000000000000190152582682582000000000000000000000000000000000000000000000000000000000000000001901535826825820000000000000000000000000000000000000000000000000000000000000000019015458268258200000000000000000000000000000000000000000000000000000000000000000190155582682582000000000000000000000000000000000000000000000000000000000000000001901565826825820000000000000000000000000000000000000000000000000000000000000000019015758268258200000000000000000000000000000000000000000000000000000000000000000190158582682582000000000000000000000000000000000000000000000000000000000000000001901595826825820000000000000000000000000000000000000000000000000000000000000000019015a5826825820000000000000000000000000000000000000000000000000000000000000000019015b5826825820000000000000000000000000000000000000000000000000000000000000000019015c5826825820000000000000000000000000000000000000000000000000000000000000000019015d5826825820000000000000000000000000000000000000000000000000000000000000000019015e5826825820000000000000000000000000000000000000000000000000000000000000000019015f582682582000000000000000000000000000000000000000000000000000000000000000001901605826825820000000000000000000000000000000000000000000000000000000000000000019016158268258200000000000000000000000000000000000000000000000000000000000000000190162582682582000000000000000000000000000000000000000000000000000000000000000001901635826825820000000000000000000000000000000000000000000000000000000000000000019016458268258200000000000000000000000000000000000000000000000000000000000000000190165582682582000000000000000000000000000000000000000000000000000000000000000001901665826825820000000000000000000000000000000000000000000000000000000000000000019016758268258200000000000000000000000000000000000000000000000000000000000000000190168582682582000000000000000000000000000000000000000000000000000000000000000001901695826825820000000000000000000000000000000000000000000000000000000000000000019016a5826825820000000000000000000000000000000000000000000000000000000000000000019016b5826825820000000000000000000000000000000000000000000000000000000000000000019016c5826825820000000000000000000000000000000000000000000000000000000000000000019016d5826825820000000000000000000000000000000000000000000000000000000000000000019016e5826825820000000000000000000000000000000000000000000000000000000000000000019016f582682582000000000000000000000000000000000000000000000000000000000000000001901705826825820000000000000000000000000000000000000000000000000000000000000000019017158268258200000000000000000000000000000000000000000000000000000000000000000190172582682582000000000000000000000000000000000000000000000000000000000000000001901735826825820000000000000000000000000000000000000000000000000000000000000000019017458268258200000000000000000000000000000000000000000000000000000000000000000190175582682582000000000000000000000000000000000000000000000000000000000000000001901765826825820000000000000000000000000000000000000000000000000000000000000000019017758268258200000000000000000000000000000000000000000000000000000000000000000190178582682582000000000000000000000000000000000000000000000000000000000000000001901795826825820000000000000000000000000000000000000000000000000000000000000000019017a5826825820000000000000000000000000000000000000000000000000000000000000000019017b5826825820000000000000000000000000000000000000000000000000000000000000000019017c5826825820000000000000000000000000000000000000000000000000000000000000000019017d5826825820000000000000000000000000000000000000000000000000000000000000000019017e5826825820000000000000000000000000000000000000000000000000000000000000000019017f582682582000000000000000000000000000000000000000000000000000000000000000001901805826825820000000000000000000000000000000000000000000000000000000000000000019018158268258200000000000000000000000000000000000000000000000000000000000000000190182582682582000000000000000000000000000000000000000000000000000000000000000001901835826825820000000000000000000000000000000000000000000000000000000000000000019018458268258200000000000000000000000000000000000000000000000000000000000000000190185582682582000000000000000000000000000000000000000000000000000000000000000001901865826825820000000000000000000000000000000000000000000000000000000000000000019018758268258200000000000000000000000000000000000000000000000000000000000000000190188582682582000000000000000000000000000000000000000000000000000000000000000001901895826825820000000000000000000000000000000000000000000000000000000000000000019018a5826825820000000000000000000000000000000000000000000000000000000000000000019018b5826825820000000000000000000000000000000000000000000000000000000000000000019018c5826825820000000000000000000000000000000000000000000000000000000000000000019018d5826825820000000000000000000000000000000000000000000000000000000000000000019018e5826825820000000000000000000000000000000000000000000000000000000000000000019018f582682582000000000000000000000000000000000000000000000000000000000000000001901905826825820000000000000000000000000000000000000000000000000000000000000000019019158268258200000000000000000000000000000000000000000000000000000000000000000190192582682582000000000000000000000000000000000000000000000000000000000000000001901935826825820000000000000000000000000000000000000000000000000000000000000000019019458268258200000000000000000000000000000000000000000000000000000000000000000190195582682582000000000000000000000000000000000000000000000000000000000000000001901965826825820000000000000000000000000000000000000000000000000000000000000000019019758268258200000000000000000000000000000000000000000000000000000000000000000190198582682582000000000000000000000000000000000000000000000000000000000000000001901995826825820000000000000000000000000000000000000000000000000000000000000000019019a5826825820000000000000000000000000000000000000000000000000000000000000000019019b5826825820000000000000000000000000000000000000000000000000000000000000000019019c5826825820000000000000000000000000000000000000000000000000000000000000000019019d5826825820000000000000000000000000000000000000000000000000000000000000000019019e5826825820000000000000000000000000000000000000000000000000000000000000000019019f582682582000000000000000000000000000000000000000000000000000000000000000001901a0582682582000000000000000000000000000000000000000000000000000000000000000001901a1582682582000000000000000000000000000000000000000000000000000000000000000001901a2582682582000000000000000000000000000000000000000000000000000000000000000001901a3582682582000000000000000000000000000000000000000000000000000000000000000001901a4582682582000000000000000000000000000000000000000000000000000000000000000001901a5582682582000000000000000000000000000000000000000000000000000000000000000001901a6582682582000000000000000000000000000000000000000000000000000000000000000001901a7582682582000000000000000000000000000000000000000000000000000000000000000001901a8582682582000000000000000000000000000000000000000000000000000000000000000001901a9582682582000000000000000000000000000000000000000000000000000000000000000001901aa582682582000000000000000000000000000000000000000000000000000000000000000001901ab582682582000000000000000000000000000000000000000000000000000000000000000001901ac582682582000000000000000000000000000000000000000000000000000000000000000001901ad582682582000000000000000000000000000000000000000000000000000000000000000001901ae582682582000000000000000000000000000000000000000000000000000000000000000001901af582682582000000000000000000000000000000000000000000000000000000000000000001901b0582682582000000000000000000000000000000000000000000000000000000000000000001901b1"

const maximum_reference_input_terminal_item_count = 433

const maximum_reference_input_terminal_item_index = 432

const maximum_reference_input_terminal_terminal_chunk_index = 0

const maximum_reference_input_terminal_encoded_length_before_item = 17283

const maximum_reference_input_terminal_pre_work_root =
  #"20c14233d70ec13b8bfdf301e0eb9c670c55e71fe37a27a3a93421ce00b393eb"

const maximum_reference_input_terminal_post_work_root =
  #"bdc0a9fd3ee84b6e458a9f3c61552e89c41ae213a78893a73679a0336c476305"

// Derived by the genuine Cardano boundary fixture in
// demo/midgard-validation/tests/ordered-collection-observer-native-script-boundary-v1.test.ts
// (accepted 224 observers at 16,338 signed bytes; adjacent 225 at 16,410). This
// is the same signed transaction replayed by
// cek_context_observer_cardano_maximum_224_first_item_and_terminal_agree, so
// the two selectors share one exact source.
const maximum_observer_terminal_transaction_id =
  #"b16b2f9ffdef875e489d13c518def5771131b15832e88cd0f79d6d3956fe168b"

const maximum_observer_terminal_transaction_commitment =
  #"5b79d540d0a51bb5e2ab678c63f5b8b88646d13e337c1d33f1e1ba6ca2db4d95"

const maximum_observer_terminal_compact_cbor =
  #"84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820b2b468552a6382aa0a02ece7767ee914a08a85a2527b117e048ca891ba560f351a000d570d2019271058206ba17d1becf6846cbc137424e22d01732cdefc5a2b14105f47babd83e6550668582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff582059778271799fb2f636923a2cb70dac0426ba8aa42ab730db4166b1968f01b29000"

const maximum_observer_terminal_witness_set_compact_cbor =
  #"83582091bef5c8f29aa474731c2ad6b30f1872403c692814a9bfd55e70687bfdc74810582039728233c559f2fdc7876f58aad839f313ea4af375a4e91d60e63e1a55adbe2e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const maximum_observer_terminal_field_preimage_lengths_cbor =
  #"89182901182c191a420101192842186801"

const maximum_observer_terminal_field_preimage_cbor =
  #"98e0581c008b91b576da13635f7150f248cd133c23ac0a697f1ccae85617ea85581c00e78218f4908ee6d6ca348130460e592fe826d744b7dd6a70c43ae6581c02658c662113c1448f31f0b4c35d7d523094a9d3650ecee81807d8cc581c0397f305417f1d7d891158a165a75fa0852c5166ed4f736d12f4a743581c039cff7866dce34998dc3baf5a0f2955c21e32e6b675f80b1de413bd581c058b872a842ec99a990a91381e66794938a29d9908117fb41ad684e2581c0779e8efdd918389fd6f4d19bf09e4912328b294d9f3aee37f1429c9581c078a4fdb67b1babe0d7f4a9fa0b1213921ee781e2429494e0ac3b4b6581c07dfe1e3d0bfec2013c1b6e5a562a76fc08f75d6b4a2477be2446032581c086a6f3a2246583f4d22a8f4ae1cbe8d51d8532d4375026fa0e4015c581c0938a4daca2c8519a71dc355331a81c734b313888b61fc98f028ff72581c0ad3858c89badde575ff065ba23cd25c1c42fcd987c04b607c49ae91581c0be47415893968ca1d757baaea03ecb16e1ff3c16f4abf08073cfd46581c0c388a0e34fc4a29119227f75b328a6abef44159bd2109eb5fb69433581c0c9bfb1f182630bac8c1b18c7e165bc41f82f2d8fe6b6a5c2186f595581c0de2347b2ddcf0880318c3b477c6bce5a481fe2662b1fc1caacb43a0581c0eb23262cbcd1d9d44238bf55695513427360be2832507997fff16f2581c1178658936533a03a976db1114baf8733e99ab19d9d4efe4562e5b46581c1266954073a1f9bc134200604cdbfd187defd025086fa9263c58ec23581c136720c58b54a91f99871f3d3d716862fd9dc15aa2f4d6577a96bee8581c139ab8b7e6d417bf9f283702da0eb82b91761529679c19ec87fafb13581c14362060bfaf0c331ad71ba5096c9f77e7123082dc16e055e23bbb91581c164bc8e36dd1cf749ad72174d339b6400e8044d9aba3c4fe13d5efa9581c169b6cc7be2690ce852347e77fc5187b9b2c247cb8a6e05c70fcc43b581c18c9ce8acba0fff5229acfdd1da43b9c49ff9a42a33fa8032f2b6d7e581c1dac4d767751a69e36c78a49744ca87f6d3158dd61f1c96342a54819581c1dde6411cbae71fc74121b40a485bc661432778c4909a269fca40b3a581c204c205f2407eff15294e38438e86a65a6acfa52502af513d1eabab5581c2069c5b685c2f16a30f413c320c48ad26a528150d99e6f51e5246aa2581c20f922ae2ac025432f87549debddb1b32cbc7e04317126e1aaeb8beb581c22f3713c89876eb7aa245e750a045cc012ee6c6934131137cd2b4357581c243796fa7fba7e02b23f5deb590889187c56bcf5c1625f36bef9b3d1581c244b4b778d0c9f01a119d7e823f6c67aac5a6faf9912be513efbf72a581c24aeb5a8614fc12df8d2f9174282c3683e588c5c6cfed3d267312e59581c26dcc43667b3d8309ce60894e35d460573588013b6727a60eff0de8c581c26f11d96f781916eceea351ff99c032904913f2b07b874ac0e542f11581c2ddf14044610560943fb6f233ec858ebdc82ed796b92adead4a18a42581c2ec83016b0f5cbdb300469ca2911ab35cc6e588b30689d45a058094f581c3131bc72cf45feb70995478337e001391a1aa64eafe215e00d758328581c314e9285b70b587051306f42302ef290f0a0511d77354df72dd00321581c315ba9090599711f7992186ea0c6a1166cb2a72606999eeb28e50ebb581c32379b36c2f84aa31829bcc8af3349932ebf668e1ed02f26d7cecba3581c32410feeae62047765d74024ee7e264c821c0a74895cab6fc4bafbc0581c326308a7ac90d8cf3aa56efae587914bd89628c2065512c47bcc63b2581c32a491048b68e1556df8c45b14be183a74f32968d54375c2740c0708581c33cee96d6e8da3cc0fabeb330a18819e2531a66acf48cebd5fd11c4c581c34b228b298904e5e36eb9ca00086cddab97f6b277394b680f0f677e5581c354f18e7c5ba118c9bfd750059720bac50064b9766e6b67407d4d569581c3777b6b6e97e771a2fc7b9ed1517488bc689564ccd283ad9ab317991581c377b478e6cc66ac4b89ee09ccf76184d95a2725763dde45741e5cb1b581c37f25e30eff19a0066881e022265e34a5c0018bb1451c1939d9202aa581c389f14839a37196f40fc115243f4efd983d40462773cbf9b1402bce9581c39d199e8b5be1c4de5d08934b4403a6e06798e222a52b8e888a454d6581c39f3b33948d77a6ee27b569b110227c385b39b9637dc024c593c5ca3581c3eb62accba770c2fea777b6e4fea6b3b5c8385e7ca1939055f777f17581c3fc5c54af3bd603ebe5c8a2700148f91a030d60c7505d2c17386c8bf581c4081a3f41e2c049d6566b9f9bc6467f03040162834c1a5e405511958581c43b85d7ee92c62ff5a7efb352cac03f5d66b153d7480f12de2e8f60a581c486baf7f7b63bffb98931a4c8f988b743d61609b07b8e881cccc20bb581c4ad128a30dcf2740e8e9c6c1e947ade61eb7384b0a4954c38915bc6f581c4c190383289d45d78a326e6b95933465dca10e43c0d5f298d39e4063581c4d1e1430a759b22b7aa8b50bca42aff18c448b3dfd4ac0a877bb9368581c4f952ef189fbbd5b7417a21b9c51a36e41ec7abbf18eea394dfa5637581c528b6102fbce47517daf8cf1fe50939aa50003532295a483c06f070a581c53adab38929bc7ced479c2ef40a04cc1710ffd4505613290abd6b749581c54183468df3c55e6557166b46910310216a40f971ccd86c9d6259be1581c54734b243175b2f323568e093b66126b4db133df7a3b98049cc4562e581c54b6fff46393da5f09ffb05c1ed937b0ca809322b513c478abef3aa1581c550e35621b7c9ee6cc1cba49b10918bf74bb4f53bd4c36092ac7448d581c5513c9c2cc436c86e8c1799d9f20e57cbfe1f62a35487bdb28d65de7581c5a8a5a81232619ae44f2afbc2587e57b445c645c7156f016114d6b11581c5b09c156f6a30879784b34119442223ab43f69d79275b6245b6a6fd3581c5e4d6947436248c75fc87132730c6b887f2e51fd8a66dba0e58068df581c5ec62b07dd460e246deddd9e0cc4856677ed1165dcc63b884dcb060e581c5f2ee4245ef16bfb65e419813ea972f8affd057580943c482f575af4581c5f5ec37c577200549d3c59dc5f3dd220d37aa65e7cb1f3d8774273b4581c60d0cab74c395abf0f4165ca6de6fbafd14789c5b0148c7fdbceda5c581c6142e9d9747d3b84901acd999528fbed99fc5ced1f3309b38cf70955581c6209af4ce1a79bb3bb6688d5d0bfa23f692236e843949a451ff44286581c6256d55614235100cc361006c7ee839a4e953396ba0e7a4e80b912ef581c629d740ccf8f7afb941dbd661578e699d827ca0b9b1d1ba2d701f8b2581c6409e42ece35379e1d18e682af9f6f68179f8c8fedff55cd6357ab7f581c64cd2ab23ea2a2210387d2f72abc5334090c3b87fb3e4c4311e90bd9581c65a1ff7d09c86b1cf2f19b8763454dd8b60b4f12da38baf56336eb54581c65c67ddc8a986abc5f9f44da1f3eb30033a0ec280b387dff30c2023e581c65c94c30d7c727db5a43f2ed02dd899288eb48305e228abd5fb1b67c581c660c23b4003b6dec623ee84fb5534622c91cd9233c3907f92e4c6e16581c6839715b591e76ca5558ea3b1173abe69e8a67772dba091c6d9b34ff581c68676952ff13772ebc3ecd4e1b841c149762ee9f79ceb2461d3dea33581c6893703e7912fc484b72ddcb79dc99384362a4f5c8e85e017119e880581c68c9a5212d950901e25e96c1511e47fa5da2698109551f01c82e79f7581c69f4663546754e3be3c8ea0cbb007a1d9dc722d7eec04cd3376be76d581c6a2547b23ad72199bfab589f489916b6103ce06cc7259f335f4af51a581c6a8973b4a7a92c9746225561facf4168f49f8dc4ed544913104651f7581c6cd0555025cff40d32aa8ecdc62045eb9377220fed0bff9f4c600404581c70ad604db3dc06dee4cab1226b8a08ce2b166907650acd76fb4d2b88581c72dba6d41ac78ecc2aaf3c89a26c9125d26d8cfda5393365155aeb7e581c73a318da0ad26351684745708de461e5bc373b0689010be0f244442b581c76a4032c66a16fd7c9f2776ff6a24e73f8271dbcc4f6bd9fc4ee45d2581c76ad1e45ad3177ddfc8fec9b004f3316ffaec5c98e005b5a236e859c581c76e37afa8155fe9dadf23ab2f708ac6862c5d59ebd022fe7b45176f5581c77f0213559ef4ab9bf318b812c66ad2e702fdd9baced9288eaaffb29581c792db96de13969809169ee374ca1536c601173135f9bb2f0d5bff920581c7975bd0d1ded66fa5825c823d94863b47dff7b4fbc077c10a8b2599a581c7a4926bb2619e5fc9b5bf3e5d4b8826efb8ef1acb5e637477c8c612c581c7b8316b2855abdbc3a30c97e041d2a9efd47897541b3faecf4438d52581c828fbf634181dbe19538bcd90d910915e6c25f801f633281e39ef053581c8387b8b28489b138a997ef6ea2d78528c21bfa554f977c7272a606c1581c83b5d59d35a1b4d2461d7e89fb45f56036234a46584edc2c99f33805581c83c9a87d7e9d950c4befdc3ba0df7295ffc71e6a406287516337f11e581c8460adb2114d204ea7e29524753fb16929b46ae88398613585919577581c85945a16fb7b15838031a5ff6bb6270b4128bcf93b9d75e64dddeecd581c8598db79cd6a8131f56e4abbc2d52c86ba7822cb8a25cb3442533bb3581c85d94364e775f16aee99b2a636d605cd2971a5985f00407391b19dbd581c86f958a624457c246261b5a58fbdfdc087ab2d4e2a76974b595e59f9581c876386e795f67132dafad946b6fe451ce15cc8e5a827419269eb47f4581c8793be784a5aa971e84ce08827fcbeb8f4b5df197e567aac5791f245581c87ce1d91022881c6af8adfe8d81754f01cfa08e2489393490dfb23c8581c882bf622ffd6208740788ec77e6ccedede0dfc407f2775a52eb357ea581c8a12ca25173e9c5b64558702ed953a80246ba124290b7812335ff6e7581c8ac43c6d88cd0a9dbc95724ff8f4b245daf6f257aef0daa41f8616b8581c8b903dc03ca9342f7fd748d31b66ffae87683c2501beca9b56cb7c66581c8cffaaf3a033d4f3478b544ff51ccad6f055d0765dd4fdb1ae16a009581c8d7add245a53f9dc4af82166ac801ff67bf42626c98da7c1036c6c98581c8e041fba7418a4f32969f9fb3a02f90db07bd46cc33519deee334a12581c8f4835e727e585b793f57b4aee0be79e1dad5ffba10c0fb14c974911581c8fd9394c7acb1309a50ac3a92d0b6945124d4abb3cd8eb2a7ca09d78581c91b95659396da242a5271816a0043585b7bdde9c2766b9653aa16bef581c92e0d88ccd2e5a211e2ebb746626aef78879f5317093a6c4217d4dcf581c935b2bc2682ca59a351a6e159b001161377f4f4b99ba509bfb1a2d04581c93bd9c9b0fd474cb90f011a38f417c9a0ecd2b4abd50d7592bbe9b28581c955a68d5022659cd509fc8c860e52c7a20cc1d9b9299eb0e41ee46ab581c96c1b791a79b74a6e10eb371ed452554c905b46ad14b167014b258ae581c96c3a369e9d3b9e7190cb994b524c4f5dab0a9ebd8c5afe0b44c7a29581c96c76fabefc1d790bc3285549017fe010f1861a8afc361721fbfc400581c96c914765cafc31a1381254fd64d59a3514fbc51e9cbe1be7dcd7d3b581c970574140f863f782711ec899dac4ebbe7dcf0e5ba1493fc3775461a581c9810aab19cb5b494af6940f6e6502175503fd722875c97335a762fb2581c98e6237591faa57f150d6d870dd910424bfbdeea033620df4c2c799d581c9947cc6e4299462e146bca358f4a630696e3ce001c7a2392487b33be581c99b32ce846695e74ace7454e53f06e5b153fae9ce1dd78850487f609581c9b5e4e7f12472336ef706ba4a34878f1a14feeee02cfd369196225dc581c9bcf9d63c6331323c60695b411ef4c8a862c6601ddba9b49e00181aa581c9d9acc84aeec67922a95efe84749e1403f5d2247832cbb52a382890f581c9fd919c6648f2c515a0615662c0fda46ddb2a9cdef313cc1a5589e4c581ca1a4be4b3e9ac95de819d2e167dfab5c5e9af1f86d24d2e37c730bff581ca1ba45d0c02e27400c6d427551de264c56d559e04f99ebb74052c53e581ca32ee44e7d066e4c09972b13babfb13d196984d7c4a34acfed7c5acd581ca3955d2a6dcc6ec27fe231955748a9d3b03398959fbe43b8d54f43b1581ca5109c241d5b9000fcf2f61f29aa9484cea5e4c3367aae20b7655d49581ca618dbce8336fa55fdc45d902e34fa86d531e2a156d26614fc5a978e581ca9e8c10b033a9c47ab47282eb50fea8013b9e76f1be8503a19f15697581caac7e637dd41420ffa6b23b0a40dce1ab5edae647e6163d95f888ce8581cae1575096ac76ba5d51198da0acdc08a336f54977b9f98c3d96500f7581cae6af5a56c3bad55c581d212954a79fa6611bb559eca2c6c636763eb581caf493d7581d1303485f6d6cf89a7c84f51821a2f9194960d05fa259a581cafdc760dc626434e958bfc9e0ec255bf75af738fbf0b35741a902a1f581cb10d19bf8769a636c72133227d3ea2f3a8c55da66c59807f80c83c7b581cb2701ff1fb114fad081d7cfb8a360b8c3b440222e3c1b012594a42ce581cb368fb9f48e4f1239378779ef91c16ba074c1e467250c81a541ab4a6581cb602484e2ee03651ecea1f7b3347562222be9e612d01539eabdfdfeb581cb9244c1dd27685e29129e1c257237f8d137abc0b25136e5813b94582581cb9b21866ff44e4fb17c3d6356074bb47661e8bbb50b0a30d9eddc0e5581cb9e31abd7cc30ab0450c6ed234d9acfe2c60eff17078aaef094070cc581cba17adde7d550871fc79f81c1eb122bb558fdf9e65bcd9092f27412c581cba306a6ccdfd15cd7d8935d09353f044212802a2516a4b5c38e2891f581cbb5e3a5fce2d1af029a2d912b82d91940290a1238fd28d629d2abdf9581cbb715800161b3d400fc5ffd94d5bfde8964fbd5f6e97923703ce7e26581cbdca9f6bcea09b91d5710000523813dc2e57304f117225e8ebcc84bb581cbf662d5e5030258406474df7944f2c3eb6c873e2dfd03efb7ddb329e581cbfa7d1172d7fbeedd3a20fe1bd84d0b3ad3824456380614b6c8e0a39581cbfecf8c192d75054b928290f606c813b0cad164d66f6b620883e3ed8581cc31f4a10a7b2424b9683d62dc8512fa1ccd456c5d5ecee37f026de40581cc3d8f9daf90ed98cf1daec0150e5b9d128d2061c0563239c9d8ee929581cc5733b5aa0339090225a03f2a14a10765607edcaefe016bf6f4c74fc581cc60a9768c649b192c3b6e883a51938c494a2f62b918176d13d2253c4581cc6a170d71a2af6d3ab7c3d8c9b4b57d9302951bbbf9313d06fb2aa94581cc99269152ab7d529bcc1938371d7a946c8eaf3dad3b4d93fd8f3f2fd581cca1039b7d5179e88e20b04df33911d845b5ad4dddcfa24dbdefbd02d581cca8ab5520de6578a213da045a82f4ea48102076427ff3cf255c74762581ccb1415b707ee02cf078e21682cf58451cc7d710b595843be4f33be4c581ccd4157817326c3e05e1480922d9b17776dfd0c6d879db51f10bed2a0581ccd45304c6030525bbbdf66d6beda8c16d7c7b03f81fd1fb53b8a1c96581cceb9f968ed01cb0a0bd6fc997bb815f0bd2e0899b319c9bc6126d52e581ccfdf56c28e92315b2def8302c081d1573525e29ec5b60a96e234c79a581cd019e835634b7225931b925a0329361c3fb52d910130b4ff7ec65c88581cd0c96e8ec6b5cf9c6599a28080634a83efd7915a30bfcaaefa4a406e581cd143dfdaf37552cd6f71d059a3f01adf8c561a9372ae0d15fe44c571581cd79b3518265df4c937af13e80375397a3c77963502f51dd5ca15e39e581cd93999721d9ed58c14253dd63f6808cf2eca9c65324db38eaea34b6b581cd9fc2834cb7511037d2f8a01994ca0e7f91a7369b0387caf420969d8581cdb02a8c0de91911575937eb559751e21391bfc1cffa85b3c256e980e581cdce139767d36bcedde2c02ba79e144276025ec2aba0be8164b35caa6581cdd0d734ee5951d3a49737b5de19764658a4f610b4fcc820e68149b23581cddfea2ac68f06e11467d7b97b36920a0761ad67ff26c0238e2f9da70581cde6f6a660299d680cf921b6d7ba51ed43b5ac7718d0174f15df0ddb7581ce00cfb82166efc8ec5878ff45abdf27255b523f4e00c7811e1f07463581ce0fafce906f822cd7ba0c835c9bdd48bc11450bb8a3daafdf0457796581ce219774a4456dc69def11d4210111d3c413bbf3ce68fa04b4d21f7c0581ce228a8430523c67943a1e7385e98346c5561802674c788d952aea091581ce4d4eac754c891ecac695f4024402ead64b3ce37eb3f84b627cb0598581ce527193aa6ae8a2f46f779e4017f29f86b30f44c920ac0b34c443f37581ceabf408a4f486bbd83cdc311af28c807cd38b10514210f05c29c304e581cec0e6a432de18cfb69446f97322a0486feee0a221cb8387cb97bb7f9581cec8014c1538497051e5b4054f83073973e61907df21042f7b5ca7c47581cecdc9f7b61021b3f39892fe83349c9145ad8389be7bcde94923fdf65581cedaa0e4a94eb2d607cde51e861c2ee31ca7410349e3520fbc5241364581cee11a5731eb29e7f82bd894510c93ecbecea15930ae7204ef2ef7c9e581cef9dbe211c6c43e2690e0644e1d52bb052123301876bcb5c07a3bf95581cf0955c058758b25357e0269024227071030aa5beddef59f690607355581cf196fa3edf039b47a38e273fab6f11f32ebf46090bf2fa4fbb46fc01581cf25fed298b1a0c50c17440da2cb3b09225e75a261eb0962b9ad43d0f581cf274518701175e2772bc50395ce9b0c37b8cce1d4e7607037e786e74581cf39e069fa5d3833f6893c4c908cd288ba5ebaf79e79993ecfbc30f6d581cf4335c4caf5a83d59c5137b0bd6645905956e11b7c9802af73bd6b13581cf479489276b01d8ce8d59224447b2076e13f1bb24b6bc5efa4845af2581cf4e452d116866918479c358f3df7d6589d5d75f592a386e09dc36b5d581cf65d1a9a1396775c3958b768a060245352dfb4eb21812dfed51091f9581cf8bdf8af1119b897285fe82286c53d9863b95acefb02cd4341eae719581cf941fcf7517151bf26db69122ad3d7e13a3c19a05629745e74a2dc8c581cfd3452414d0f1a97c2224438290268bf6c263151dc73e7c2fa0a573f581cfde025f9a3fd2cae96a31014efef5062c688fdc3fc7d4ecf638198fb581cfe6f33afe1208bf44feeb2a55cdab50873e904588266518c687a2fbb581cffab1dd64f82b6991818c1ecc5047d52ce5d00f6fdbc5023e2980167"

const maximum_observer_terminal_item_count = 224

const maximum_observer_terminal_item_index = 223

const maximum_observer_terminal_terminal_chunk_index = 0

const maximum_observer_terminal_encoded_length_before_item = 6692

const maximum_observer_terminal_pre_work_root =
  #"1078262a33daff30359c3e2b00ebc601b2058fd77e7587bdae0d67baf5478e19"

const maximum_observer_terminal_post_work_root =
  #"5e98b6a8a98432b486d068899b3dad627f8c41dd6ae8afef51976b88fa0d84d4"

// Derived by the genuine Cardano boundary fixture in
// demo/midgard-validation/tests/ordered-collection-boundary-v1.test.ts
// (accepted 438 outputs; the field-2 maximum without a Midgard count cap).
const maximum_output_terminal_transaction_id =
  #"1faba3bc592fac6c30165c2789e08a18357deb9b449f5d0ceea1b3e6aa40ee1e"

const maximum_output_terminal_transaction_commitment =
  #"89de7a79482046596d7a217e20889350a26e5cb168c9cb69fdca382b4a402c7a"

const maximum_output_terminal_compact_cbor =
  #"84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582029aa9d887ffc6c715298aca446ef28580bdefe11f0187542a1af885f4bc37d891a000d5ce52020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820eccbee583d6f236c74876ca59375b638367b0e69bf0e865cb61a78de2e09806d00"

const maximum_output_terminal_witness_set_compact_cbor =
  #"8358201eebef5772f4e0efc21590eb23d67c94de0f675e91ab78c78b89c4ab4b1e2be7582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const maximum_output_terminal_field_preimage_lengths_cbor =
  #"8918290119499901010101186801"

const maximum_output_terminal_field_preimage_cbor =
  #"9901b65829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a05829a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821a001e8480a0582da200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821b000000091c0a049ba0"

const maximum_output_terminal_item_count = 438

const maximum_output_terminal_item_index = 437

const maximum_output_terminal_terminal_chunk_index = 0

const maximum_output_terminal_encoded_length_before_item = 18794

const maximum_output_terminal_pre_work_root =
  #"c355c350925b94503664350b6b6d7f44c0776ef44caef0e40ebdf5d6936b29c2"

const maximum_output_terminal_post_work_root =
  #"b39febf97e07ab5bd85330521e7205f074b12e8e20e0dd2e39a40e5868fdfce1"

// Derived by the genuine Cardano boundary fixture in
// demo/midgard-validation/tests/ordered-collection-signer-witness-boundary-v1.test.ts
// (field 4 of the coupled signer/vkey-witness maximum, 124 witnesses).
const maximum_required_signer_terminal_transaction_id =
  #"7b4e4657e0083544359f4398fb092c482766220cd53ad99b598239297d1e9813"

const maximum_required_signer_terminal_transaction_commitment =
  #"56ba4f868c6e37a2eb67542110b3cf1b87007e6f88493b97faa1e276cd1943ea"

const maximum_required_signer_terminal_compact_cbor =
  #"84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820095c12f5790acc50dbbf52c0b47fe4ebd1dfd9ab308b14701543d6d4d78a06ae1a000d59492020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820e2d5bb3b4c4475d516516e5396ec041b553ada06379a53f665b47e1485e0451f582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820ad12ff89400f2f7975c77231241032e6a7bf49d0f2ab388425b3de0cefef003000"

const maximum_required_signer_terminal_witness_set_compact_cbor =
  #"835820689afcab7a4406fa8da9a4f97b325f34458bd2114d6b2ae9eb357e681acc0e97582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const maximum_required_signer_terminal_field_preimage_lengths_cbor =
  #"89182901183001190e8a01011931e601"

const maximum_required_signer_terminal_field_preimage_cbor =
  #"987c581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d581c99e2b94e1cd5c95c4c689797890d0d7cbb93c2137db2964b12a67dde581c745627a7fd9497525daaa102d19ec0fb5450810b792a21945f7e84ca581cb0157b4610a6a346ce628a2ca9c193563aeb506b4121ea084149f5a8581cbcf50cd3961e94dbdebcb556b280f2e6a7c4e9f0f00de05c71f3ad6b581c90a8e29e3b945beed1e63b54be4c4522375aaedea305ff714540550a581c485d024e2416185df4d0f194a1cc104032a2a6834fe23bf8887dd88c581ca2ce381556c98d547e1dead2940407f7f6c29ab8c4114a95a7fd4dfb581c013d89be7811865fd1dfa2c793d6274b403819dbb51c5249e4389147581ceaf6e220273c8ead5340cd18e3e846b0ea4dc59414a4802217ac8ead581cb36d074c6f96627fc85f1677965af4a8557401f0ca5456033a5ca312581cfde6e48a319ae9da76f3a640aab1b035b8e244cdd34143140642babf581caf15f2e5bd66f3b63f5b7b5f1aa7f9d04d199bfe83fe8d238e8a5f7b581c94b9ce6e951d6a63140b6889a985f84989426bb5e4cc84bbd9d2d296581caed55f6815ede8ca1653d48ee1aac7e44290ebed3b442e309c457369581c60ca8118b295721e58cd7052f6d357fea4a2f7f143738d591b6d0c18581c95b3ed784949ee55fd7fb2d3c523a765121654f43f8d06b5e32c25ca581c1a450d9b5d083fe4094205a7c238e1504c9fd1f7a71b95f0410e06f0581cd8adebabf5ce10be772b25b931bb9110fdfac67ef7588363d1b3e0b5581ce96f7a912733c5295945223a34ffa309a1796d0e316fbd6950ffda3c581cfca1e77e91fb60ea5a61c382f5783b43e0fbded0b7d60df81da7cd55581c0e67fbf3e49b6f59f507d2a2a7045c2fbdd5d95dfc4d42f32dd0f20d581ced91d4e69f83e8196247e2fef74f8e361fdad86d7bf5ea599a9e46e5581ce1e61bedb343510f945e044c80e7df0fd5e4cd5a42e9a353f7b798d0581c29f205f302af401f05149a140795bdf17069876b5f7a72cab991ea21581c6a5ebdda6ad6254531aa94f210f9ca2a6aea71fa83a0c6f85fc01829581c3e66c0b6ea79ca78258adc66fc128ad3b023486eb61c197ce983a42e581cc40adb9e682709d8f0603328a430b2428147c83ad962fdb9f25bfe7a581c678b0d628e692a7d5118e978b6bf75ad28be7b3065bf5a634792de4e581c7166a605e8915779c024ca341f5fa61f55c4f76d3c5ad8157f923c1f581c89b0f66d743a0002221ed2aa7add7e3d71c7ce14d93528afa3526dc5581c690cae0f7506557781f88dfee0e8f3acafb4d2867c8951700b4da309581c5267be7e20c54b87db434f7303c55f5e37dc1caea8ee9d4b54be675e581c37e5fe33eb86df4635bf087ad79f3c8408c9336d01ecda5fe3df22dc581c2129505dcd591b92e56415f51374ddf9c73fcbe6d0c1e272ca18007b581c59c7f01133e43710ff666845a395797729144255a9656b47d7ea3e42581cb500c8ae40ea1c55360d0f1fb9bf29d6704f18d0dca13f7005b2d710581c58f144987fe294b9021a3b4ed84e6cc7aef0ea2290c5bc22896fb9fd581c17ea7db65acb3747513f4aa557c5a7d8f281d6b782ced2f124a63df7581c83a10a91190be2f6ce77aec5247987304d7af3160c3c503f57cede94581cc8e03ba51a8789fcbdcd524bd07140bcb0e18a5afdec5295e9a54a21581c31cf9e93f399e11c42c6c38c915aca1e71954651d412c5685a5b908d581ccbb3883292f93d3d0cbe898edbee84a59bcd2d1cff3100ab6321fdc4581c45686df72883acd36ac5870e6a6f3363a86cfff6e1d2bd583bded2dd581c44c436db37c88110231ea1826735c78dae3c9545a56b2bf08b716100581c0152b4152a6a2c695128420086d2c4db87720d777f8c9c2592a25ca0581cdcd46d9b4d54793b8b1a5fcc497f95f530cf00c164b1f4abaf82de1e581cc2acb87c8240b07272dcf7afdd584cc18b6b29e66868684fd5f84634581ccfbc19df7a131535fa48e6617afca6d99ee8d1624cadf3dda101f3cb581c017ca3fd208c69433cd1ffe49ad6b1e3a3cb0087f24adc98885ec567581cf05b37e8fe339f3c872c789fe2f3e1a0b27a5e16d4878446b56d0a1e581c07640a700232d515e0ac789f033cd794c54e9a2f6bdea98adef2790a581cffe8a81c50c12dce96c3abb60a066690b245aaa63cd09e4ccee7a22c581c2bc1bb283dcfc9505d7246654deeca05f2be247e1057d11e7529fbe3581cfe5a450082b9cdec4db6fbbb09765131e4c9ef27171cfedc656c1c74581ce34d354e07cd0ac9574f04ed1842a40466ec8a3a57c58d9d7f072fe8581c83d1bd8872e2fb9c2465ed1ecdcc7e2cd1b42a3c28da763efd1ee098581cd001dc3623a8e52d0dcafd5244eb7abf9a25af8bb730ba912a9a84f1581c4164deddf803c8f1db1af43f364916c7e7284de97331d0322b72ebd4581c27d464f6b8f0c87d01bad1f8f528eadfe0941683e7a9a507623efb16581c9b0adf5a408b433351bd0f91483197a81513597ca99320d2f6ba297a581cf755de6cc952fa38fdb6e36d66c0267dc97b1194ab2f16973c1b1a92581c8bcf50cf83f3f432b34b8d4350631e5256b42ba6bee7ff8fb4aee898581c08335c6dba9969e2ba21ff068ede3afa96691f3bed8bb64764df1a6c581c6ab175efcc272ba8a70c28011fe62b1b6d6fe0ccbeae7ab93c7105b5581c76afb6d83ebdc618e406171b1aee674897863f76954ed75336c03f40581c4c82b242536b88431f81feb61cf5fa2e04c84f84760c1a5be0676149581c2f0ed105eee27afed011cbcf267bd5704ac1f8e50d50f8c20d7573b7581c63dfd1e19195152adec9a7f321408b7328bc786476d708e3b74c634b581c9a9ca6852ff9397679ca0dcc3021f66d0c1b35c09e97785b65451937581cc599968cf9aa7cc6a0b4e071cb224d14c4ca0c62b178c9a807bd0b05581cce1a462d42897483db55838fbeffae4cce629429f5fc2e6c248e41fb581c0f7ba014b8fa86e8a6e1c4d52f39f6400ab4289e609f2dc2fe16b7bb581cd5aee89caf14e29aea6b16397cf881f6a332f8396f9f241df5c787a0581c66db7803b959bb9b8d1baa3969f06f727b80357f7815677c8acf3fe9581c2522cfdf2b4f379a09d88c2e40edc5243de8bd494f32ff808063b9cc581c0a11ca6f7e07e1292f6bbab506c72250676e73e07c031befbea397f5581c1dca6db06ce3d33a9eaffee76fe8cf54278ea37b5627e0ad407af734581c9f87eed39cbfe00fad8d92ebb15f0f93ab16f1c4a2bd5d7f6a541dab581c9b6a3bdc830cfb0fd8068d050d28d248f6e2f6335f140140a460ed6a581ca1a31563dba2c4baba2fbbfb349fd47be5960b79b912b55c776ba9c3581c2c81a68d7d16529d7c38c063a889f04fc9988cca74b681eae577c5bc581cd3ed4588bc3f68b135113c0108ffc772fb7aebea985e80b931f6dccf581ca569e2ff8e2fb9ac896b0512ce588a7484f665b2399c9b658000d710581c2d7e3d725527f5a5b6609a73548848e3795f846c4e2762217265c64b581c64e1f7245ac1aa7f87b2855f720acc72d5fb8cbf5a163fc9e2b74459581cebfb2fb1d3b11a96bcaa683e302110896a1465e9c8d9c2431a322a8e581cfb83de521c32159849f917d8ab71fa50e444ee7a3a6c67468fdea73f581c708ba8011e315ea4d44ffa5e65bb6585248c4943b18e57bdaa4b4f76581ca60c290d6d9abe2b3e11d3d8586c3eb0e0f0db6e4d06bfb1b839131b581cb25990c4e46727d1c1e03b56c4d2e55a3cbc4b4d7d7288db5cd80d62581c015e5a48f66b2127901f247ca5adab9c0c0bd15df977d495b7cd0620581c7f95042a9d8aa74f51a21ab809a4876cd06a5de4c38a7cc3a205ad50581c014ecb3049ba3ca697dda24ad4bf9da8e4eacf0f8c4821c66e9df7af581c8a3e07d3849d5124b06c9fcee2d42f837d5259f6ee6a593fcb7b98a2581ca196802f4753d17247911f511e5e87e1be199b47a4b039648404442f581c40a54991356b05486791cbcd0055dfb6a0a1d537bdf487a7569526f3581c21b3af12565e2940b4d9ec016d3e87e7828c0337f03363d242d291f2581c0df457cbcc7c9344d478c80622cadf012e1b95057894a7d362636001581c4394ba0078ae96c412acd99a8b1ec5ce12acbd69d344d08785bc300d581c382f370262b469227551e723f305ca4be6d3b4b1eb5031d2c3f0df0e581c1e4291357e4704c9dd010e8e83311ab4359d3551761d585d77a5b7f6581ca5328a3ea032dd5261649b3500cc56615c7a335345c5ff33caf77383581ca96d1588135ad8733a7d95b7dfd00dc2fda413f0a06c04ec40a4e567581c0ec806a4e920cc5565fdbd4ef1c4862ae968c055552bec01d8d42c25581cd6508318ec0d5e2b5543937be3a6c5ad4b09355d3368eae6971e95ef581c3f7c0ceee05471404ae18e750b7e1a5bea2d2bc178e849f2e27d79e2581c1d879bb439d05390d73a9a45dfa7e599d35cb1640e4db18f5d2e4837581cfab38f493b44a3ac63df71c8d4bbe6b4966e26a8666762c5f55e4d85581c6dd8d10a606d83338a85236b87c30c167fd0c4509d197ea5ee77930e581cf2a1b5ee43d66e200bf1ca89c93e6d1749bf37de5d624304d3e4a273581c80798c60471603fd60b6bab953f5aca46907615e1d3376ea83337e51581c025dc46bef542719a048f9313ee3da316598ecff7ee0a3947f45177d581cb8b38e14e971490e8a549acf3dd07867c1c3b7ba1fe0659e7f164381581ccdf5a6563380cec5a0170a23647d5fc4d653de8021f7805dc4689882581cd4f53bf6a93c9f5fc51d12822d069a89d6f454e285473e3230229f9d581c36987e3b783597edc8a9e016ac99cede4e32ce0e86242c3096137988581cba345932c22ec96b13cfee45bf05550e4703e6127f053efa91cc7269581c99ca37991024058a6bd7b970c4926baafac1b0d6ad9478b7f4863b6c581c04c2c1d13f474c63592411c09fa814d9770af99532bb5ac953c06aa5581c2e963aa107eff00392c09a26020029c2c5b4dc98b408cc48a50af5a2581cd124484e34894a0c4617291ce512cc9f18615a17673911ec03715e00581c6645be8a3d3e838553d34de816c2bde2488bce745469db7cd984ad78581c38abf94805d076d7253d8386794096ec3d48fe233bc45d5edf97ab19"

const maximum_required_signer_terminal_item_count = 124

const maximum_required_signer_terminal_item_index = 123

const maximum_required_signer_terminal_terminal_chunk_index = 0

const maximum_required_signer_terminal_encoded_length_before_item = 3692

const maximum_required_signer_terminal_pre_work_root =
  #"c39fcbe1b9077c6a2242f2cec28b84ad23fadb5b4065248f6cd6ecaab49a2c03"

const maximum_required_signer_terminal_post_work_root =
  #"f34d2c3452c0168fc935a7a0c9de927311c8bfd372a53d83d7df8e0c5effec37"

// Derived by the genuine Cardano boundary fixture in
// demo/midgard-validation/tests/ordered-collection-mint-boundary-v1.test.ts
// (130 mint policies packed under maxValueSize, every policy authorized by a
// field-6 native script).
const maximum_mint_terminal_transaction_id =
  #"7d05811c269235d01486fb1b9f5d9f08ee27dfd67f5b2f5553e98f5f63d2904c"

const maximum_mint_terminal_transaction_commitment =
  #"53fab1e7ca307ed09192048fc8c4fe82b33e4cd9de40bd2e723207039d85b281"

const maximum_mint_terminal_compact_cbor =
  #"84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820743cefaed4ac50d862140f9018bd5a6723a941ac660315426da4e0912d60ed141a000d5d9520192710582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c058207ba153c420ecc2fc34570ff76ea54d8b892b9b2f21baff8ae9bbaf95b7e1eab7582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820878c89b8969e798009d6fae019bdec7f6e14b989b41d2ee7ae2d02bfa4df08c100"

const maximum_mint_terminal_witness_set_compact_cbor =
  #"8358204a7039dd2bf25a2eedc820e9786a94f14f36017f375092fa327fbe7a3d5796205820f5f20200888316fc6bd33ec7fc4d5d91ce372d0e68d954b491f5b225b1fab688582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const maximum_mint_terminal_field_preimage_lengths_cbor =
  #"891829011915b201011916dc19175e186801"

const maximum_mint_terminal_field_preimage_cbor =
  #"9882582b82581c02658c662113c1448f31f0b4c35d7d523094a9d3650ecee81807d8cca1494d696467617264563101582b82581c0397f305417f1d7d891158a165a75fa0852c5166ed4f736d12f4a743a1494d696467617264563101582b82581c039cff7866dce34998dc3baf5a0f2955c21e32e6b675f80b1de413bda1494d696467617264563101582b82581c058b872a842ec99a990a91381e66794938a29d9908117fb41ad684e2a1494d696467617264563101582b82581c0779e8efdd918389fd6f4d19bf09e4912328b294d9f3aee37f1429c9a1494d696467617264563101582b82581c078a4fdb67b1babe0d7f4a9fa0b1213921ee781e2429494e0ac3b4b6a1494d696467617264563101582b82581c07dfe1e3d0bfec2013c1b6e5a562a76fc08f75d6b4a2477be2446032a1494d696467617264563101582b82581c0938a4daca2c8519a71dc355331a81c734b313888b61fc98f028ff72a1494d696467617264563101582b82581c0ad3858c89badde575ff065ba23cd25c1c42fcd987c04b607c49ae91a1494d696467617264563101582b82581c0be47415893968ca1d757baaea03ecb16e1ff3c16f4abf08073cfd46a1494d696467617264563101582b82581c0c388a0e34fc4a29119227f75b328a6abef44159bd2109eb5fb69433a1494d696467617264563101582b82581c0c9bfb1f182630bac8c1b18c7e165bc41f82f2d8fe6b6a5c2186f595a1494d696467617264563101582b82581c0de2347b2ddcf0880318c3b477c6bce5a481fe2662b1fc1caacb43a0a1494d696467617264563101582b82581c0eb23262cbcd1d9d44238bf55695513427360be2832507997fff16f2a1494d696467617264563101582b82581c1266954073a1f9bc134200604cdbfd187defd025086fa9263c58ec23a1494d696467617264563101582b82581c139ab8b7e6d417bf9f283702da0eb82b91761529679c19ec87fafb13a1494d696467617264563101582b82581c14362060bfaf0c331ad71ba5096c9f77e7123082dc16e055e23bbb91a1494d696467617264563101582b82581c169b6cc7be2690ce852347e77fc5187b9b2c247cb8a6e05c70fcc43ba1494d696467617264563101582b82581c1dac4d767751a69e36c78a49744ca87f6d3158dd61f1c96342a54819a1494d696467617264563101582b82581c204c205f2407eff15294e38438e86a65a6acfa52502af513d1eabab5a1494d696467617264563101582b82581c2069c5b685c2f16a30f413c320c48ad26a528150d99e6f51e5246aa2a1494d696467617264563101582b82581c22f3713c89876eb7aa245e750a045cc012ee6c6934131137cd2b4357a1494d696467617264563101582b82581c243796fa7fba7e02b23f5deb590889187c56bcf5c1625f36bef9b3d1a1494d696467617264563101582b82581c244b4b778d0c9f01a119d7e823f6c67aac5a6faf9912be513efbf72aa1494d696467617264563101582b82581c26dcc43667b3d8309ce60894e35d460573588013b6727a60eff0de8ca1494d696467617264563101582b82581c2ddf14044610560943fb6f233ec858ebdc82ed796b92adead4a18a42a1494d696467617264563101582b82581c2ec83016b0f5cbdb300469ca2911ab35cc6e588b30689d45a058094fa1494d696467617264563101582b82581c3131bc72cf45feb70995478337e001391a1aa64eafe215e00d758328a1494d696467617264563101582b82581c314e9285b70b587051306f42302ef290f0a0511d77354df72dd00321a1494d696467617264563101582b82581c32410feeae62047765d74024ee7e264c821c0a74895cab6fc4bafbc0a1494d696467617264563101582b82581c32a491048b68e1556df8c45b14be183a74f32968d54375c2740c0708a1494d696467617264563101582b82581c34b228b298904e5e36eb9ca00086cddab97f6b277394b680f0f677e5a1494d696467617264563101582b82581c3777b6b6e97e771a2fc7b9ed1517488bc689564ccd283ad9ab317991a1494d696467617264563101582b82581c37f25e30eff19a0066881e022265e34a5c0018bb1451c1939d9202aaa1494d696467617264563101582b82581c389f14839a37196f40fc115243f4efd983d40462773cbf9b1402bce9a1494d696467617264563101582b82581c39d199e8b5be1c4de5d08934b4403a6e06798e222a52b8e888a454d6a1494d696467617264563101582b82581c3eb62accba770c2fea777b6e4fea6b3b5c8385e7ca1939055f777f17a1494d696467617264563101582b82581c3fc5c54af3bd603ebe5c8a2700148f91a030d60c7505d2c17386c8bfa1494d696467617264563101582b82581c4081a3f41e2c049d6566b9f9bc6467f03040162834c1a5e405511958a1494d696467617264563101582b82581c43b85d7ee92c62ff5a7efb352cac03f5d66b153d7480f12de2e8f60aa1494d696467617264563101582b82581c486baf7f7b63bffb98931a4c8f988b743d61609b07b8e881cccc20bba1494d696467617264563101582b82581c4ad128a30dcf2740e8e9c6c1e947ade61eb7384b0a4954c38915bc6fa1494d696467617264563101582b82581c4d1e1430a759b22b7aa8b50bca42aff18c448b3dfd4ac0a877bb9368a1494d696467617264563101582b82581c4f952ef189fbbd5b7417a21b9c51a36e41ec7abbf18eea394dfa5637a1494d696467617264563101582b82581c53adab38929bc7ced479c2ef40a04cc1710ffd4505613290abd6b749a1494d696467617264563101582b82581c54183468df3c55e6557166b46910310216a40f971ccd86c9d6259be1a1494d696467617264563101582b82581c54734b243175b2f323568e093b66126b4db133df7a3b98049cc4562ea1494d696467617264563101582b82581c5a8a5a81232619ae44f2afbc2587e57b445c645c7156f016114d6b11a1494d696467617264563101582b82581c5e4d6947436248c75fc87132730c6b887f2e51fd8a66dba0e58068dfa1494d696467617264563101582b82581c60d0cab74c395abf0f4165ca6de6fbafd14789c5b0148c7fdbceda5ca1494d696467617264563101582b82581c6409e42ece35379e1d18e682af9f6f68179f8c8fedff55cd6357ab7fa1494d696467617264563101582b82581c64cd2ab23ea2a2210387d2f72abc5334090c3b87fb3e4c4311e90bd9a1494d696467617264563101582b82581c65a1ff7d09c86b1cf2f19b8763454dd8b60b4f12da38baf56336eb54a1494d696467617264563101582b82581c65c67ddc8a986abc5f9f44da1f3eb30033a0ec280b387dff30c2023ea1494d696467617264563101582b82581c65c94c30d7c727db5a43f2ed02dd899288eb48305e228abd5fb1b67ca1494d696467617264563101582b82581c660c23b4003b6dec623ee84fb5534622c91cd9233c3907f92e4c6e16a1494d696467617264563101582b82581c68c9a5212d950901e25e96c1511e47fa5da2698109551f01c82e79f7a1494d696467617264563101582b82581c69f4663546754e3be3c8ea0cbb007a1d9dc722d7eec04cd3376be76da1494d696467617264563101582b82581c6a2547b23ad72199bfab589f489916b6103ce06cc7259f335f4af51aa1494d696467617264563101582b82581c6a8973b4a7a92c9746225561facf4168f49f8dc4ed544913104651f7a1494d696467617264563101582b82581c6cd0555025cff40d32aa8ecdc62045eb9377220fed0bff9f4c600404a1494d696467617264563101582b82581c70ad604db3dc06dee4cab1226b8a08ce2b166907650acd76fb4d2b88a1494d696467617264563101582b82581c72dba6d41ac78ecc2aaf3c89a26c9125d26d8cfda5393365155aeb7ea1494d696467617264563101582b82581c73a318da0ad26351684745708de461e5bc373b0689010be0f244442ba1494d696467617264563101582b82581c76a4032c66a16fd7c9f2776ff6a24e73f8271dbcc4f6bd9fc4ee45d2a1494d696467617264563101582b82581c76ad1e45ad3177ddfc8fec9b004f3316ffaec5c98e005b5a236e859ca1494d696467617264563101582b82581c76e37afa8155fe9dadf23ab2f708ac6862c5d59ebd022fe7b45176f5a1494d696467617264563101582b82581c792db96de13969809169ee374ca1536c601173135f9bb2f0d5bff920a1494d696467617264563101582b82581c7975bd0d1ded66fa5825c823d94863b47dff7b4fbc077c10a8b2599aa1494d696467617264563101582b82581c828fbf634181dbe19538bcd90d910915e6c25f801f633281e39ef053a1494d696467617264563101582b82581c83b5d59d35a1b4d2461d7e89fb45f56036234a46584edc2c99f33805a1494d696467617264563101582b82581c85945a16fb7b15838031a5ff6bb6270b4128bcf93b9d75e64dddeecda1494d696467617264563101582b82581c8598db79cd6a8131f56e4abbc2d52c86ba7822cb8a25cb3442533bb3a1494d696467617264563101582b82581c85d94364e775f16aee99b2a636d605cd2971a5985f00407391b19dbda1494d696467617264563101582b82581c8793be784a5aa971e84ce08827fcbeb8f4b5df197e567aac5791f245a1494d696467617264563101582b82581c87ce1d91022881c6af8adfe8d81754f01cfa08e2489393490dfb23c8a1494d696467617264563101582b82581c882bf622ffd6208740788ec77e6ccedede0dfc407f2775a52eb357eaa1494d696467617264563101582b82581c8a12ca25173e9c5b64558702ed953a80246ba124290b7812335ff6e7a1494d696467617264563101582b82581c8ac43c6d88cd0a9dbc95724ff8f4b245daf6f257aef0daa41f8616b8a1494d696467617264563101582b82581c8b903dc03ca9342f7fd748d31b66ffae87683c2501beca9b56cb7c66a1494d696467617264563101582b82581c8cffaaf3a033d4f3478b544ff51ccad6f055d0765dd4fdb1ae16a009a1494d696467617264563101582b82581c8d7add245a53f9dc4af82166ac801ff67bf42626c98da7c1036c6c98a1494d696467617264563101582b82581c8fd9394c7acb1309a50ac3a92d0b6945124d4abb3cd8eb2a7ca09d78a1494d696467617264563101582b82581c91b95659396da242a5271816a0043585b7bdde9c2766b9653aa16befa1494d696467617264563101582b82581c93bd9c9b0fd474cb90f011a38f417c9a0ecd2b4abd50d7592bbe9b28a1494d696467617264563101582b82581c96c3a369e9d3b9e7190cb994b524c4f5dab0a9ebd8c5afe0b44c7a29a1494d696467617264563101582b82581c98e6237591faa57f150d6d870dd910424bfbdeea033620df4c2c799da1494d696467617264563101582b82581c9bcf9d63c6331323c60695b411ef4c8a862c6601ddba9b49e00181aaa1494d696467617264563101582b82581c9d9acc84aeec67922a95efe84749e1403f5d2247832cbb52a382890fa1494d696467617264563101582b82581ca1a4be4b3e9ac95de819d2e167dfab5c5e9af1f86d24d2e37c730bffa1494d696467617264563101582b82581ca1ba45d0c02e27400c6d427551de264c56d559e04f99ebb74052c53ea1494d696467617264563101582b82581ca32ee44e7d066e4c09972b13babfb13d196984d7c4a34acfed7c5acda1494d696467617264563101582b82581ca5109c241d5b9000fcf2f61f29aa9484cea5e4c3367aae20b7655d49a1494d696467617264563101582b82581ca618dbce8336fa55fdc45d902e34fa86d531e2a156d26614fc5a978ea1494d696467617264563101582b82581caac7e637dd41420ffa6b23b0a40dce1ab5edae647e6163d95f888ce8a1494d696467617264563101582b82581cafdc760dc626434e958bfc9e0ec255bf75af738fbf0b35741a902a1fa1494d696467617264563101582b82581cb368fb9f48e4f1239378779ef91c16ba074c1e467250c81a541ab4a6a1494d696467617264563101582b82581cb602484e2ee03651ecea1f7b3347562222be9e612d01539eabdfdfeba1494d696467617264563101582b82581cb9b21866ff44e4fb17c3d6356074bb47661e8bbb50b0a30d9eddc0e5a1494d696467617264563101582b82581cba17adde7d550871fc79f81c1eb122bb558fdf9e65bcd9092f27412ca1494d696467617264563101582b82581cbb715800161b3d400fc5ffd94d5bfde8964fbd5f6e97923703ce7e26a1494d696467617264563101582b82581cbf662d5e5030258406474df7944f2c3eb6c873e2dfd03efb7ddb329ea1494d696467617264563101582b82581cbfecf8c192d75054b928290f606c813b0cad164d66f6b620883e3ed8a1494d696467617264563101582b82581cc5733b5aa0339090225a03f2a14a10765607edcaefe016bf6f4c74fca1494d696467617264563101582b82581cc60a9768c649b192c3b6e883a51938c494a2f62b918176d13d2253c4a1494d696467617264563101582b82581ccb1415b707ee02cf078e21682cf58451cc7d710b595843be4f33be4ca1494d696467617264563101582b82581ccd4157817326c3e05e1480922d9b17776dfd0c6d879db51f10bed2a0a1494d696467617264563101582b82581cceb9f968ed01cb0a0bd6fc997bb815f0bd2e0899b319c9bc6126d52ea1494d696467617264563101582b82581ccfdf56c28e92315b2def8302c081d1573525e29ec5b60a96e234c79aa1494d696467617264563101582b82581cd79b3518265df4c937af13e80375397a3c77963502f51dd5ca15e39ea1494d696467617264563101582b82581cdce139767d36bcedde2c02ba79e144276025ec2aba0be8164b35caa6a1494d696467617264563101582b82581cddfea2ac68f06e11467d7b97b36920a0761ad67ff26c0238e2f9da70a1494d696467617264563101582b82581ce00cfb82166efc8ec5878ff45abdf27255b523f4e00c7811e1f07463a1494d696467617264563101582b82581ce219774a4456dc69def11d4210111d3c413bbf3ce68fa04b4d21f7c0a1494d696467617264563101582b82581ce228a8430523c67943a1e7385e98346c5561802674c788d952aea091a1494d696467617264563101582b82581ce4d4eac754c891ecac695f4024402ead64b3ce37eb3f84b627cb0598a1494d696467617264563101582b82581ce527193aa6ae8a2f46f779e4017f29f86b30f44c920ac0b34c443f37a1494d696467617264563101582b82581cec0e6a432de18cfb69446f97322a0486feee0a221cb8387cb97bb7f9a1494d696467617264563101582b82581cec8014c1538497051e5b4054f83073973e61907df21042f7b5ca7c47a1494d696467617264563101582b82581cecdc9f7b61021b3f39892fe83349c9145ad8389be7bcde94923fdf65a1494d696467617264563101582b82581cee11a5731eb29e7f82bd894510c93ecbecea15930ae7204ef2ef7c9ea1494d696467617264563101582b82581cef9dbe211c6c43e2690e0644e1d52bb052123301876bcb5c07a3bf95a1494d696467617264563101582b82581cf0955c058758b25357e0269024227071030aa5beddef59f690607355a1494d696467617264563101582b82581cf196fa3edf039b47a38e273fab6f11f32ebf46090bf2fa4fbb46fc01a1494d696467617264563101582b82581cf274518701175e2772bc50395ce9b0c37b8cce1d4e7607037e786e74a1494d696467617264563101582b82581cf479489276b01d8ce8d59224447b2076e13f1bb24b6bc5efa4845af2a1494d696467617264563101582b82581cf4e452d116866918479c358f3df7d6589d5d75f592a386e09dc36b5da1494d696467617264563101582b82581cf65d1a9a1396775c3958b768a060245352dfb4eb21812dfed51091f9a1494d696467617264563101582b82581cfd3452414d0f1a97c2224438290268bf6c263151dc73e7c2fa0a573fa1494d696467617264563101582b82581cffab1dd64f82b6991818c1ecc5047d52ce5d00f6fdbc5023e2980167a1494d696467617264563101"

const maximum_mint_terminal_item_count = 130

const maximum_mint_terminal_item_index = 129

const maximum_mint_terminal_terminal_chunk_index = 0

const maximum_mint_terminal_encoded_length_before_item = 5807

const maximum_mint_terminal_pre_work_root =
  #"4a04c1b0d3ab0307fcbad5cda4985636ac30cbd52d11f229967bb21d6e027868"

const maximum_mint_terminal_post_work_root =
  #"6b18ca10e35cdec5bf29736c0a43aa76b4fa32600949fce9208dde1c9fdbd798"

/// One genuine-Cardano maximum-cardinality field, at the terminal step of its
/// `CanonicalDecode` fold, as §8's door reaches it.
///
/// **What left this record, and why it is not a weaker fixture.** It used to
/// carry a `bounded_collection_v1.ItemProofV1` and a
/// `bounded_item_v1.ChunkProofV1` — a per-item Merkle frontier with siblings,
/// and the item's chunk with its own frontier — and the step checked the first
/// against the field's §4 commitment. That is the unsatisfiable predicate #592
/// is about, and both proofs are gone: the fixture now carries the field's whole
/// §5.1 **preimage**, the door hashes it once against the flat commitment the
/// compact structure declares, and the item's length, chunk and
/// `bounded_item_v1` commitment are all derived from those authenticated bytes.
/// Nothing per-item is asserted by the fixture, so nothing per-item can drift.
type MaximumFieldTerminalFixtureV1 {
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  field_preimage_cbor: ByteArray,
  field_index: Int,
  item_count: Int,
  item_index: Int,
  terminal_chunk_index: Int,
  encoded_length_before_item: Int,
  expected_pre_work_root: ByteArray,
  expected_post_work_root: ByteArray,
}

/// The §4 slot a field index names, read out of the compact structures the
/// fixture pins.
///
/// §4 removed field-index domain separation, so which of the nine 32-byte hashes
/// a preimage is *supposed* to match is positional and nothing else. Spelling
/// the map here — rather than reaching for the machine's private reader — is what
/// lets these fixtures state "this preimage commits to *this* slot" as a
/// checkable fact.
fn terminal_fixture_field_slot_commitment(
  fixture: MaximumFieldTerminalFixtureV1,
) -> ByteArray {
  let compact = decode_native_tx_compact_v1(fixture.compact_cbor)
  let witness_set = decode_native_tx_witness_set_compact(
    fixture.witness_set_compact_cbor,
  )
  when fixture.field_index is {
    0 -> compact.body.spend_inputs_hash
    1 -> compact.body.reference_inputs_hash
    2 -> compact.body.outputs_hash
    3 -> compact.body.required_observers_hash
    4 -> compact.body.required_signers_hash
    5 -> compact.body.mint_hash
    6 -> witness_set.script_tx_wits_hash
    7 -> witness_set.addr_tx_wits_hash
    _ -> witness_set.redeemer_tx_wits_hash
  }
}

/// §5.1's items of the fixture's field preimage, through the one uniform reader
/// all nine fields share since #567.
fn terminal_fixture_items(
  fixture: MaximumFieldTerminalFixtureV1,
) -> List<ByteArray> {
  decode_midgard_tx_byte_list_preimage_cbor(fixture.field_preimage_cbor)
}

/// The terminal item's payload bytes — what the retired
/// `ChunkProofV1.chunk` carried at this boundary, now read out of the preimage.
fn terminal_fixture_item(fixture: MaximumFieldTerminalFixtureV1) -> ByteArray {
  expect Some(item) = list.last(terminal_fixture_items(fixture))
  item
}

fn terminal_fixture_carriage(
  fixture: MaximumFieldTerminalFixtureV1,
) -> native_tx_field_access_v1.FieldCarriageV1 {
  native_tx_field_access_v1.Inline { preimage: fixture.field_preimage_cbor }
}

fn verify_maximum_field_terminal_fixture_v1(
  fixture: MaximumFieldTerminalFixtureV1,
) -> Bool {
  let derived_source_commitment = native_tx_proof_commitment_v1(
    fixture.compact_cbor,
    fixture.witness_set_compact_cbor,
    fixture.field_preimage_lengths_cbor,
  )
  let terminal_item = terminal_fixture_item(fixture)
  let work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    fixture.compact_cbor,
    fixture.witness_set_compact_cbor,
    fixture.field_preimage_lengths_cbor,
    validation_context_cbor,
    fixture.field_index,
    fixture.item_index,
    fixture.terminal_chunk_index,
    fixture.item_count,
    fixture.encoded_length_before_item,
  )
  let successor_work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    fixture.compact_cbor,
    fixture.witness_set_compact_cbor,
    fixture.field_preimage_lengths_cbor,
    validation_context_cbor,
    fixture.field_index + 1,
    0,
    0,
    -1,
    0,
  )
  let pre = terminal_fixture_pre_state(fixture, work_witness_cbor)
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      41,
      successor_work_witness_cbor,
    ),
  }
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  // A *different* transaction body: the compact structure with one field's
  // declared preimage length bumped. Under the counted scheme this fed
  // `verify_midgard_transaction_field_chunk_v1`'s substitution controls; under §4
  // it is the door's own binding that refuses it, and the two statements below
  // say so without needing that retired reader.
  let alternate_length_pre = terminal_fixture_alternate_length_pre_state(
    fixture,
  )
  and {
    derived_source_commitment == fixture.transaction_commitment,
    // The published preimage is the committed one, for the slot this fixture
    // names. This is the whole authentication the door performs, stated once.
    native_tx_field_access_v1.field_commitment(fixture.field_preimage_cbor) == terminal_fixture_field_slot_commitment(
      fixture,
    ),
    pre.work_root == fixture.expected_pre_work_root,
    post.work_root == fixture.expected_post_work_root,
    fixture.item_index + 1 == fixture.item_count,
    list.length(terminal_fixture_items(fixture)) == fixture.item_count,
    fixture.terminal_chunk_index + 1 == bounded_item_v1.chunk_count(
      bytearray.length(terminal_item),
    ),
    // The terminal fold authenticates.
    validation_machine_v1.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      fixture.field_index,
      fixture.item_index,
      terminal_fixture_carriage(fixture),
    ),
    // Evidence naming a neighbouring slot does not authenticate this one: §4
    // removed domain separation and the machine re-establishes it by checking the
    // index against the control's.
    !validation_machine_v1.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      fixture.field_index + 1,
      fixture.item_index,
      terminal_fixture_carriage(fixture),
    ),
    // Nor does evidence naming a different item of the same field.
    !validation_machine_v1.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      fixture.field_index,
      fixture.item_index - 1,
      terminal_fixture_carriage(fixture),
    ),
    // A declared §5.1 length that disagrees with the authenticated preimage's own
    // length renders `reject_field_preimage_size` rather than authenticating —
    // one of the two `CanonicalDecode` verdicts the door leaves reachable (the
    // other being `reject_invalid_field_type`; committed bytes that are not a
    // §5.1 envelope at all are #596's).
    validation_machine_v1.verify_canonical_decode_chunk_semantics_v1(
      alternate_length_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: terminal_fixture_alternate_length_work_witness(
          fixture,
        ),
        claimed_successor: exact_rejection_post(
          alternate_length_pre,
          #"455f4649454c445f505245494d4147455f53495a45",
        ),
      },
      inline_field_door(),
      fixture.field_index,
      fixture.item_index,
      terminal_fixture_carriage(fixture),
    ),
  }
}

fn terminal_fixture_pre_state(
  fixture: MaximumFieldTerminalFixtureV1,
  work_witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: fixture.transaction_id,
    transaction_id: fixture.transaction_id,
    transaction_commitment: fixture.transaction_commitment,
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: fixture.transaction_commitment,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 40,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      40,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
}

/// The same transaction with the *declared* §5.1 length of this fixture's field
/// bumped by one, and the compact structures re-derived around it.
///
/// The two committed structures disagree, which §4's flat hash cannot collapse:
/// `NativeTxFieldPreimageLengthsV1` is its own commitment, so a declared length
/// that the authenticated preimage's own length contradicts is exactly the case
/// `reject_field_preimage_size` exists for.
fn terminal_fixture_alternate_length_cbor(
  fixture: MaximumFieldTerminalFixtureV1,
) -> ByteArray {
  let lengths = decode_native_tx_field_preimage_lengths_v1(
    fixture.field_preimage_lengths_cbor,
  )
  encode_native_tx_field_preimage_lengths_v1(
    when fixture.field_index is {
      0 ->
        NativeTxFieldPreimageLengthsV1 {
          ..lengths,
          spend_inputs: lengths.spend_inputs + 1,
        }
      1 ->
        NativeTxFieldPreimageLengthsV1 {
          ..lengths,
          reference_inputs: lengths.reference_inputs + 1,
        }
      2 ->
        NativeTxFieldPreimageLengthsV1 {
          ..lengths,
          outputs: lengths.outputs + 1,
        }
      3 ->
        NativeTxFieldPreimageLengthsV1 {
          ..lengths,
          required_observers: lengths.required_observers + 1,
        }
      4 ->
        NativeTxFieldPreimageLengthsV1 {
          ..lengths,
          required_signers: lengths.required_signers + 1,
        }
      _ -> NativeTxFieldPreimageLengthsV1 { ..lengths, mint: lengths.mint + 1 }
    },
  )
}

fn terminal_fixture_alternate_length_work_witness(
  fixture: MaximumFieldTerminalFixtureV1,
) -> ByteArray {
  validation_machine_v1.encode_transaction_field_scan_witness(
    fixture.compact_cbor,
    fixture.witness_set_compact_cbor,
    terminal_fixture_alternate_length_cbor(fixture),
    validation_context_cbor,
    fixture.field_index,
    fixture.item_index,
    fixture.terminal_chunk_index,
    fixture.item_count,
    fixture.encoded_length_before_item,
  )
}

fn terminal_fixture_alternate_length_pre_state(
  fixture: MaximumFieldTerminalFixtureV1,
) -> validation_trace_v1.ValidationMachineStateV1 {
  let alternate_length_cbor = terminal_fixture_alternate_length_cbor(fixture)
  let work_witness_cbor = terminal_fixture_alternate_length_work_witness(
    fixture,
  )
  validation_trace_v1.ValidationMachineStateV1 {
    ..terminal_fixture_pre_state(fixture, work_witness_cbor),
    transaction_commitment: native_tx_proof_commitment_v1(
      fixture.compact_cbor,
      fixture.witness_set_compact_cbor,
      alternate_length_cbor,
    ),
    prior_ledger_root: fixture.transaction_commitment,
  }
}

fn maximum_spend_input_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_spend_input_terminal_transaction_id,
    transaction_commitment: maximum_spend_input_terminal_transaction_commitment,
    compact_cbor: maximum_spend_input_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_spend_input_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_spend_input_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_spend_input_terminal_field_preimage_cbor,
    field_index: 0,
    item_count: maximum_spend_input_terminal_item_count,
    item_index: maximum_spend_input_terminal_item_index,
    terminal_chunk_index: maximum_spend_input_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_spend_input_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_spend_input_terminal_pre_work_root,
    expected_post_work_root: maximum_spend_input_terminal_post_work_root,
  }
}

fn maximum_reference_input_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_reference_input_terminal_transaction_id,
    transaction_commitment: maximum_reference_input_terminal_transaction_commitment,
    compact_cbor: maximum_reference_input_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_reference_input_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_reference_input_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_reference_input_terminal_field_preimage_cbor,
    field_index: 1,
    item_count: maximum_reference_input_terminal_item_count,
    item_index: maximum_reference_input_terminal_item_index,
    terminal_chunk_index: maximum_reference_input_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_reference_input_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_reference_input_terminal_pre_work_root,
    expected_post_work_root: maximum_reference_input_terminal_post_work_root,
  }
}

fn maximum_observer_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_observer_terminal_transaction_id,
    transaction_commitment: maximum_observer_terminal_transaction_commitment,
    compact_cbor: maximum_observer_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_observer_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_observer_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_observer_terminal_field_preimage_cbor,
    field_index: 3,
    item_count: maximum_observer_terminal_item_count,
    item_index: maximum_observer_terminal_item_index,
    terminal_chunk_index: maximum_observer_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_observer_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_observer_terminal_pre_work_root,
    expected_post_work_root: maximum_observer_terminal_post_work_root,
  }
}

fn maximum_output_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_output_terminal_transaction_id,
    transaction_commitment: maximum_output_terminal_transaction_commitment,
    compact_cbor: maximum_output_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_output_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_output_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_output_terminal_field_preimage_cbor,
    field_index: 2,
    item_count: maximum_output_terminal_item_count,
    item_index: maximum_output_terminal_item_index,
    terminal_chunk_index: maximum_output_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_output_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_output_terminal_pre_work_root,
    expected_post_work_root: maximum_output_terminal_post_work_root,
  }
}

fn maximum_required_signer_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_required_signer_terminal_transaction_id,
    transaction_commitment: maximum_required_signer_terminal_transaction_commitment,
    compact_cbor: maximum_required_signer_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_required_signer_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_required_signer_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_required_signer_terminal_field_preimage_cbor,
    field_index: 4,
    item_count: maximum_required_signer_terminal_item_count,
    item_index: maximum_required_signer_terminal_item_index,
    terminal_chunk_index: maximum_required_signer_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_required_signer_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_required_signer_terminal_pre_work_root,
    expected_post_work_root: maximum_required_signer_terminal_post_work_root,
  }
}

fn maximum_mint_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_mint_terminal_transaction_id,
    transaction_commitment: maximum_mint_terminal_transaction_commitment,
    compact_cbor: maximum_mint_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_mint_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_mint_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_mint_terminal_field_preimage_cbor,
    field_index: 5,
    item_count: maximum_mint_terminal_item_count,
    item_index: maximum_mint_terminal_item_index,
    terminal_chunk_index: maximum_mint_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_mint_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_mint_terminal_pre_work_root,
    expected_post_work_root: maximum_mint_terminal_post_work_root,
  }
}

test maximum_spend_input_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_spend_input_field_terminal_fixture_v1()),
) {
  verify_maximum_field_terminal_fixture_v1(fixture)
}

test maximum_reference_input_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_reference_input_field_terminal_fixture_v1()),
) {
  verify_maximum_field_terminal_fixture_v1(fixture)
}

test maximum_observer_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_observer_field_terminal_fixture_v1()),
) {
  verify_maximum_field_terminal_fixture_v1(fixture)
}

test maximum_output_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_output_field_terminal_fixture_v1()),
) {
  verify_maximum_field_terminal_fixture_v1(fixture)
}

test maximum_required_signer_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_required_signer_field_terminal_fixture_v1()),
) {
  verify_maximum_field_terminal_fixture_v1(fixture)
}

test cek_context_signer_item_uses_authenticated_signer_frontier() {
  let tx = empty_v1_transaction()
  let signer_hash = h_28_b
  let signer_leaf = script_proof_v1.signer_leaf_hash(signer_hash)
  let signer_frontier = validation_merkle_v1.build_frontier([signer_leaf])
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    signer_count: 1,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
  }
  let context_control = empty_cek_context_control_fixture(4)
  expect Some(next_signer_items) = script_context_v1.prepend_signer_v1(
    1,
    control.signer_frontier_commitment,
    signer_frontier.peaks,
    0,
    signer_hash,
    [],
    context_control.signer_items,
  )
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    43,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(
      validation_machine_v1.CekContextControlV1 {
        ..context_control,
        signer_items: next_signer_items,
      },
    ),
    0,
    44,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.CekSignerContextItemWitness {
        peaks: signer_frontier.peaks,
        signer_index: 0,
        signer_hash,
        siblings: [],
      },
    },
    inline_field_door(),
  )
}

test cek_context_observer_cardano_maximum_224_first_item_and_terminal_agree() {
  let base = empty_v1_transaction()
  // The same signed transaction the field-3 terminal fixture pins, through the
  // same generated constants — one exact source for both selectors, rather than
  // two hand-mirrored copies that could drift apart (#590).
  let transaction_id = maximum_observer_terminal_transaction_id
  let transaction_commitment = maximum_observer_terminal_transaction_commitment
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(base),
    compact_cbor: maximum_observer_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_observer_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_observer_terminal_field_preimage_lengths_cbor,
  }
  let context_control = empty_cek_context_control_fixture(5)
  let observers_carriage = native_tx_field_access_v1.Inline {
    preimage: maximum_observer_terminal_field_preimage_cbor,
  }
  // The CEK observer list is built back to front, so the first item step reads
  // the *last* observer. Derived from the authenticated preimage rather than
  // pinned: under §8 the item's bytes are not a prover claim any more.
  expect Some(observer_hash) = list.last(
    decode_midgard_tx_byte_list_preimage_cbor(
      maximum_observer_terminal_field_preimage_cbor,
    ),
  )
  let first_items = validation_machine_v1.prepend_cek_observer_item_v1(
    observer_hash,
    False,
    context_control.observer_items,
  )
  let first_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    observer_count: 224,
    observer_items: first_items,
    previous_observer: observer_hash,
  }
  let mismatched_count_control = validation_machine_v1.CekContextControlV1 {
    ..first_control,
    observer_count: 225,
  }
  let complete_items = cek_data_v1.DataSequenceSummaryV1 {
    root: #"354faff8c56e5c2db595d2b0682336623a4de555e8426762630783d26921ab5d",
    length: 224,
    payload_cbor_length: 7840,
    memory: 9184,
  }
  let complete_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    observer_count: 224,
    observer_items: complete_items,
    previous_observer: #"008b91b576da13635f7150f248cd133c23ac0a697f1ccae85617ea85",
  }
  let observer_summary = cek_data_v1.DataSummaryV1 {
    root: #"61eff6b0a693f2da6f3fd45ae1b3d402937e3ab0de94da2fcbd5ed67ab61a726",
    cbor_length: 7842,
    memory: 9188,
  }
  let final_control = validation_machine_v1.CekContextControlV1 {
    ..complete_control,
    stage: 6,
    observer_summary,
  }
  and {
    observer_summary == validation_machine_v1.finalize_cek_observer_items_v1(
      complete_items,
      False,
    ),
    verify_cek_context_source_transition_fixture(
      transaction_id,
      transaction_commitment,
      native_control,
      context_control,
      first_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        field_index: 3,
        item_index: 223,
        carriage: observers_carriage,
      },
      45,
    ),
    verify_cek_context_source_transition_fixture(
      transaction_id,
      transaction_commitment,
      native_control,
      complete_control,
      final_control,
      validation_machine_v1.NoAuxiliaryWitness,
      46,
    ),
    !verify_cek_context_source_transition_fixture(
      transaction_id,
      transaction_commitment,
      native_control,
      context_control,
      mismatched_count_control,
      // The count is no longer a prover field: it is §5.2's authenticated header
      // of the very bytes the door hashed. What a prover can still claim wrongly
      // is the successor's `observer_count`, and `mismatched_count_control` does
      // exactly that against the honest carriage — the same 225-against-224
      // overstatement the retired `ItemProofV1.item_count: 225` expressed.
      validation_machine_v1.TransactionFieldChunkWitness {
        field_index: 3,
        item_index: 223,
        carriage: observers_carriage,
      },
      47,
    ),
  }
}

test cek_context_observer_empty_field_has_exact_no_auxiliary_path() {
  let tx = empty_v1_transaction()
  let native_control = cek_context_native_control_fixture(tx)
  let context_control = empty_cek_context_control_fixture(5)
  let observer_summary = validation_machine_v1.finalize_cek_observer_items_v1(
    context_control.observer_items,
    False,
  )
  let successor_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    stage: 6,
    observer_summary,
  }
  let carriage = inline_field_carriage([h_28_a])
  and {
    verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      successor_control,
      validation_machine_v1.NoAuxiliaryWitness,
      48,
    ),
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      successor_control,
      // An empty field-3 slot admits no item step at all: the auxiliary must be
      // `NoAuxiliaryWitness`, so any carriage — even one that is a well-formed
      // §5.1 preimage — is refused before the door is consulted.
      validation_machine_v1.TransactionFieldChunkWitness {
        field_index: 3,
        item_index: 0,
        carriage,
      },
      49,
    ),
  }
}

/// The `CekContext` observer step's negative space, after §8's door left it
/// with only three prover-controlled inputs.
///
/// **What retired here, and what it became.** Eight rows used to perturb a
/// counted `ItemProofV1`: its `item_index`, its `field_index`, its `item_count`,
/// its `item_length`, and the paired `ChunkProofV1.total_length`. Under §8 the
/// item's bytes, its length and the field's §5.2 count are all *derived* from a
/// preimage the door hashed once against `required_observers_hash`, so five of
/// those five fields no longer exist to forge — a prover that changes any of them
/// has changed the preimage, and the door refuses it by aborting (§7.3), which is
/// fail-closed rather than a `False` a `!verify` row could observe.
///
/// The two that survive as evidence are the pair the machine still checks
/// against its own cursor (`field_index == 3`, `item_index == expected_index`),
/// and the count/length claims move to where they are now decidable: the
/// **claimed successor**. `observer_count` is claimed there, and understating it
/// is exactly the attack the retired `item_count` perturbation stood for. Each
/// row below is one falsifier, measured.
test cek_context_observer_rejects_malformed_proofs_order_and_successors() {
  let base = empty_v1_transaction()
  let observers = observer_hashes_from(0, 2)
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, required_observers: observers },
  }
  let native_control = cek_context_native_control_fixture(tx)
  let context_control = empty_cek_context_control_fixture(5)
  let observers_carriage = observer_field_carriage(2)
  let first_items = validation_machine_v1.prepend_cek_observer_item_v1(
    observer_hash_at(1),
    False,
    context_control.observer_items,
  )
  let first_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    observer_count: 2,
    observer_items: first_items,
    previous_observer: observer_hash_at(1),
  }
  let malformed_successor = validation_machine_v1.CekContextControlV1 {
    ..first_control,
    previous_observer: observer_hash_at(0),
  }
  // Replaces the retired `ItemProofV1.item_count: 1` perturbation: the field's
  // authenticated §5.2 count is 2, and a successor claiming 1 is the same
  // understatement, now made where a prover can actually make it.
  let understated_count_successor = validation_machine_v1.CekContextControlV1 {
    ..first_control,
    observer_count: 1,
  }
  let next_items = validation_machine_v1.prepend_cek_observer_item_v1(
    observer_hash_at(0),
    False,
    first_items,
  )
  let wrong_order_control = validation_machine_v1.CekContextControlV1 {
    ..first_control,
    previous_observer: observer_hash_at(0),
  }
  let complete_control = validation_machine_v1.CekContextControlV1 {
    ..wrong_order_control,
    observer_items: next_items,
  }
  let exact_summary = validation_machine_v1.finalize_cek_observer_items_v1(
    next_items,
    False,
  )
  let wrong_summary = cek_data_v1.DataSummaryV1 { ..exact_summary, root: h_a }
  let wrong_summary_control = validation_machine_v1.CekContextControlV1 {
    ..complete_control,
    stage: 6,
    observer_summary: wrong_summary,
  }
  // The list is built back to front, so the first item step must read index 1.
  let honest_auxiliary = validation_machine_v1.TransactionFieldChunkWitness {
    field_index: 3,
    item_index: 1,
    carriage: observers_carriage,
  }
  and {
    // The honest first step.
    verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      honest_auxiliary,
      50,
    ),
    // Descending-order violation in the claimed successor.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      malformed_successor,
      honest_auxiliary,
      51,
    ),
    // An `item_index` that is not the one the accumulated list derives.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        field_index: 3,
        item_index: 0,
        carriage: observers_carriage,
      },
      52,
    ),
    // A successor understating the field's authenticated item count.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      understated_count_successor,
      honest_auxiliary,
      53,
    ),
    // Evidence naming a different field slot. §4 removed domain separation, so
    // this is refused by the step's own `field_index == 3` and by nothing else.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        field_index: 2,
        item_index: 1,
        carriage: observers_carriage,
      },
      54,
    ),
    // The second step taken out of order: `previous_observer` is already the
    // lesser hash, so nothing remains that can precede it.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      wrong_order_control,
      complete_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        field_index: 3,
        item_index: 0,
        carriage: observers_carriage,
      },
      57,
    ),
    // And the finalization summary must be the exact fold of what was read.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      complete_control,
      wrong_summary_control,
      validation_machine_v1.NoAuxiliaryWitness,
      58,
    ),
  }
}

test cek_context_observer_complete_summary_relation_is_exact() {
  let tx = empty_v1_transaction()
  let native_control = cek_context_native_control_fixture(tx)
  let empty_items = cek_data_v1.empty_data_pair_summary_v1()
  let exact_summary = validation_machine_v1.finalize_cek_observer_items_v1(
    empty_items,
    False,
  )
  let malformed_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(6),
    observer_summary: cek_data_v1.DataSummaryV1 { ..exact_summary, root: h_a },
  }
  let successor_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(9),
    observer_summary: exact_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(malformed_control),
    0,
    59,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(successor_control),
    0,
    60,
  )
  !validation_machine_v1.verify_cek_one_step_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
}

test cek_context_mint_is_authenticated_and_folded_incrementally() {
  // §5.6 — the enveloped list of per-policy items, not the retired raw map.
  let mint_cbor = native_tx_field_access_v1.encode_field_preimage(
    [
      #"82581c"
        |> bytearray.concat(h_28_a)
        |> bytearray.concat(#"a1410124"),
    ],
  )
  let mint = decode_midgard_tx_mint_preimage_cbor(mint_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  let mint_leaf = validation_machine_v1.mint_asset_leaf_hash(h_28_a, #"01", -5)
  let mint_frontier = validation_merkle_v1.build_frontier([mint_leaf])
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let init_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(6),
    observer_summary,
  }
  let semantic_control = validation_machine_v1.CekContextControlV1 {
    ..init_control,
    stage: 8,
  }
  let Pair(init_pre, init_work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(init_control),
    0,
    47,
  )
  let Pair(init_post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(semantic_control),
    0,
    48,
  )
  let init_step = validation_machine_v1.verify_cek_one_step_v1(
    init_pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: init_work_witness_cbor,
        claimed_successor: init_post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
  let asset_items = cek_data_v1.prepend_data_pair_summary_v1(
    builtin.b_data(#"01") |> cek_data_v1.semantic_data_summary_v1,
    builtin.i_data(-5) |> cek_data_v1.semantic_data_summary_v1,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let item_control = validation_machine_v1.CekContextControlV1 {
    ..semantic_control,
    mint_cursor: 1,
    current_mint_policy: h_28_a,
    current_mint_assets: asset_items,
  }
  let Pair(item_pre, item_work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(semantic_control),
    0,
    48,
  )
  let Pair(item_post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(item_control),
    0,
    49,
  )
  let item_step = validation_machine_v1.verify_cek_one_step_v1(
    item_pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: item_work_witness_cbor,
        claimed_successor: item_post,
      },
      auxiliary: validation_machine_v1.CekMintContextItemWitness {
        mint_index: 0,
        policy_id: h_28_a,
        asset_name: #"01",
        quantity: -5,
        siblings: [],
      },
    },
    inline_field_door(),
  )
  let policy_items = cek_data_v1.prepend_data_pair_summary_v1(
    builtin.b_data(h_28_a) |> cek_data_v1.semantic_data_summary_v1,
    cek_data_v1.map_data_summary_v1(asset_items),
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let final_control = validation_machine_v1.CekContextControlV1 {
    ..item_control,
    stage: 9,
    current_mint_policy: #"",
    current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
    mint_policies: policy_items,
    mint_summary: cek_data_v1.map_data_summary_v1(policy_items),
  }
  let Pair(final_pre, final_work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(item_control),
    0,
    49,
  )
  let Pair(final_post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(final_control),
    0,
    50,
  )
  let final_step = validation_machine_v1.verify_cek_one_step_v1(
    final_pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: final_work_witness_cbor,
        claimed_successor: final_post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
  and {
    init_step,
    item_step,
    final_step,
  }
}

test cek_context_redeemer_scalar_scan_is_content_addressed() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: cbor.serialise(builtin.i_data(0)),
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let subject = encode_midgard_tx_input(
    MidgardTxInput { tx_id: h_b, output_index: 0 },
  )
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, subject)
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    redeemer_count: 1,
    redeemer_peaks: redeemer_frontier.peaks,
    purpose_peaks: purpose_frontier.peaks,
  }
  let raw = cbor.serialise(builtin.i_data(0))
  let scan = cek_data_scan_v1.initial_control_v1(
    blake2b_256(raw),
    bytearray.length(raw),
  )
  expect Some(completed_scan) = cek_data_scan_v1.reveal_leaf_control_step_v1(
    scan,
    raw,
    None,
    bytearray.length(raw),
  )
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let mint_summary = cek_data_v1.map_data_summary_v1(
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  expect Some(purpose_summary) = script_context_v1.script_purpose_summary_v1(
    0,
    h_28_a,
    subject,
    False,
  )
  let active_redeemers = validation_machine_v1.CekRedeemerContextControlV1 {
    ..validation_machine_v1.initial_cek_redeemer_context_control_v1(),
    active_scan_hash: cek_data_scan_v1.hash_control_v1(scan),
    active_redeemer_leaf: redeemer_leaf,
    active_purpose: purpose_summary,
  }
  let context_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(9),
    subject,
    redeemer_leaf,
    observer_summary,
    mint_summary,
    redeemer_context_control_hash: validation_machine_v1.hash_cek_redeemer_context_control_v1(
      active_redeemers,
    ),
  }
  let map_items = cek_data_v1.prepend_data_pair_summary_v1(
    purpose_summary,
    completed_scan.result,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let completed_redeemers = validation_machine_v1.CekRedeemerContextControlV1 {
    cursor: 1,
    map_items,
    active_scan_hash: #"",
    active_redeemer_leaf: #"",
    active_purpose: cek_data_scan_v1.empty_result_v1(),
    current_redeemer: completed_scan.result,
  }
  let next_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    stage: 10,
    redeemer_context_control_hash: validation_machine_v1.hash_cek_redeemer_context_control_v1(
      completed_redeemers,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    52,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(next_control),
    0,
    53,
  )
  and {
    pre.program_counter == 52,
    work_witness_cbor != #"",
    post.program_counter == 53,
    validation_machine_v1.hash_cek_redeemer_context_control_v1(active_redeemers) != #"",
  }
}

test cek_context_redeemer_map_selection_authenticates_purpose_and_redeemer() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: cbor.serialise(builtin.i_data(0)),
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let subject = encode_midgard_tx_input(
    MidgardTxInput { tx_id: h_b, output_index: 0 },
  )
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, subject)
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    redeemer_count: 1,
    redeemer_peaks: redeemer_frontier.peaks,
    purpose_peaks: purpose_frontier.peaks,
  }
  let initial_redeemers = validation_machine_v1.initial_cek_redeemer_context_control_v1()
  // The active redeemer scan is a bounded `redeemer_item_proof_v1` control, not
  // the superseded free-standing `cek_data_scan_v1` control:
  // `verify_cek_redeemer_data_step` seeds `active_scan_hash` with
  // `redeemer_item_proof_v1.hash_control_v1` of a fresh `mode_data` item control
  // (validation-machine-v1.ak:15818-15839) and the follow-on
  // `RedeemerItemStepWitness` branch re-derives exactly that hash (:15868).
  // `validation-machine-v1.ak` no longer references `cek_data_scan_v1` at all.
  let redeemer_item_cbor = encode_midgard_redeemer_witness(redeemer)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(
    8,
    0,
    redeemer_item_cbor,
  )
  expect Some(redeemer_tag) = validation_machine_v1.redeemer_tag_for_purpose_kind_v1(
    0,
  )
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_data,
    0,
    1,
    bytearray.length(redeemer_item_cbor),
    redeemer_item_commitment,
    redeemer_tag,
    0,
  )
  expect Some(purpose_summary) = script_context_v1.script_purpose_summary_v1(
    0,
    h_28_a,
    subject,
    False,
  )
  let active_redeemers = validation_machine_v1.CekRedeemerContextControlV1 {
    ..initial_redeemers,
    active_scan_hash: redeemer_item_proof_v1.hash_control_v1(item_control),
    active_redeemer_leaf: redeemer_leaf,
    active_purpose: purpose_summary,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let context_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(9),
    subject,
    redeemer_leaf,
    observer_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
    redeemer_context_control_hash: validation_machine_v1.hash_cek_redeemer_context_control_v1(
      initial_redeemers,
    ),
  }
  let next_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    redeemer_context_control_hash: validation_machine_v1.hash_cek_redeemer_context_control_v1(
      active_redeemers,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    53,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(next_control),
    0,
    54,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.CekRedeemerContextSelectWitness {
        control: initial_redeemers,
        item_index: 0,
        item_count: 1,
        total_length: bytearray.length(
          encode_midgard_redeemer_witness(redeemer),
        ),
        item_commitment: bounded_item_v1.from_bytes(
          8,
          0,
          encode_midgard_redeemer_witness(redeemer),
        ),
        redeemer_siblings: [],
        purpose_frontier_index: 0,
        purpose_kind: 0,
        purpose_index: 0,
        script_hash: h_28_a,
        subject,
        purpose_siblings: [],
      },
    },
    inline_field_door(),
  )
}

test cek_context_finalize_seeds_the_exact_semantic_context_application() {
  let redeemer = MidgardRedeemerWitness {
    purpose: RewardRedeemer,
    index: 0,
    redeemer_cbor: cbor.serialise(builtin.i_data(0)),
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, required_observers: [h_28_a] },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(2, 0, h_28_a, h_28_a)
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    redeemer_count: 1,
    redeemer_peaks: validation_merkle_v1.build_frontier([redeemer_leaf]).peaks,
    purpose_peaks: validation_merkle_v1.build_frontier([purpose_leaf]).peaks,
  }
  let redeemer_summary = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  expect Some(purpose_summary) = script_context_v1.script_purpose_summary_v1(
    2,
    h_28_a,
    h_28_a,
    False,
  )
  let redeemer_items = cek_data_v1.prepend_data_pair_summary_v1(
    purpose_summary,
    redeemer_summary,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let redeemer_control = validation_machine_v1.CekRedeemerContextControlV1 {
    cursor: 1,
    map_items: redeemer_items,
    active_scan_hash: #"",
    active_redeemer_leaf: #"",
    active_purpose: cek_data_scan_v1.empty_result_v1(),
    current_redeemer: redeemer_summary,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [h_28_a],
    False,
  )
  // Stage 10 is gated by `mint_context_is_final` (validation-machine-v1.ak:13631),
  // which requires `observer_context_is_complete` (:13602): the replayed
  // `observer_items`/`observer_count`/`previous_observer` must be exactly the
  // state that produced `observer_summary`. This transaction carries one
  // required observer, so the observer scan must be shown as completed over
  // that one observer -- leaving the pristine (count 0, empty items) fixture
  // in place while overriding `observer_summary` with a one-observer summary
  // is not a reachable control.
  let observer_items = validation_machine_v1.prepend_cek_observer_item_v1(
    h_28_a,
    False,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let context_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(10),
    purpose_kind: 2,
    script_hash: h_28_a,
    subject: h_28_a,
    redeemer_leaf,
    observer_count: 1,
    observer_items,
    previous_observer: h_28_a,
    observer_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
    redeemer_context_control_hash: validation_machine_v1.hash_cek_redeemer_context_control_v1(
      redeemer_control,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    54,
  )
  expect Some(script_info) = script_context_v1.cardano_script_info_summary_v1(
    2,
    h_28_a,
    h_28_a,
    None,
  )
  let parts = validation_machine_v1.CekContextPartsControlV1 {
    redeemer_items,
    redeemer: redeemer_summary,
    script_info,
  }
  let next_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    stage: 11,
    redeemer_context_control_hash: validation_machine_v1.hash_cek_context_parts_control_v1(
      parts,
    ),
  }
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(next_control),
    0,
    55,
  )
  and {
    observer_summary == validation_machine_v1.finalize_cek_observer_items_v1(
      observer_items,
      False,
    ),
    validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor,
          claimed_successor: post,
        },
        auxiliary: validation_machine_v1.CekContextFinalizeWitness {
          redeemer_control,
        },
      },
      inline_field_door(),
    ),
  }
}

test cek_spend_finalize_authenticates_descriptor_datum_summary() {
  let tx = empty_v1_transaction()
  let input = MidgardTxInput { tx_id: h_b, output_index: 2 }
  let subject = encode_midgard_tx_input(input)
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 1, assets: [] },
    datum_cbor: Some(cbor.serialise(builtin.i_data(7))),
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let descriptor_cbor = exact_output_descriptor_fixture(2, output_cbor)
  let resolved_leaf = script_proof_v1.resolved_context_item_leaf_hash(
    0,
    0,
    subject,
    descriptor_cbor,
  )
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    resolved_input_count: 1,
    spend_input_count: 1,
    resolved_item_peaks: validation_merkle_v1.build_frontier([resolved_leaf]).peaks,
  }
  let redeemer_summary = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  let redeemer_control = validation_machine_v1.CekRedeemerContextControlV1 {
    ..validation_machine_v1.initial_cek_redeemer_context_control_v1(),
    current_redeemer: redeemer_summary,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let context_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(10),
    purpose_kind: 0,
    purpose_index: 0,
    subject,
    observer_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
    redeemer_context_control_hash: validation_machine_v1.hash_cek_redeemer_context_control_v1(
      redeemer_control,
    ),
  }
  expect Some(script_info) = script_context_v1.cardano_spend_script_info_from_descriptor_v1(
    subject,
    descriptor_cbor,
  )
  let parts = validation_machine_v1.CekContextPartsControlV1 {
    redeemer_items: redeemer_control.map_items,
    redeemer: redeemer_summary,
    script_info,
  }
  let next_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    stage: 11,
    redeemer_context_control_hash: validation_machine_v1.hash_cek_context_parts_control_v1(
      parts,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    56,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(next_control),
    0,
    57,
  )
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary: validation_machine_v1.CekContextFinalizeSpendWitness {
      redeemer_control,
      item_index: 0,
      key: subject,
      descriptor_cbor,
      siblings: [],
    },
  }
  and {
    validation_machine_v1.verify_cek_context_finalize(
      pre,
      evidence.transition,
      evidence.auxiliary,
      native_control,
      context_control,
      0,
      0,
      0,
    ),
    !validation_merkle_v1.verify_membership(
      native_control.resolved_input_count,
      native_control.resolved_item_peaks,
      0,
      script_proof_v1.resolved_context_item_leaf_hash(
        0,
        0,
        subject,
        exact_output_descriptor_fixture(
          2,
          encode_midgard_tx_output(
            MidgardTxOutput {
              ..output,
              datum_cbor: Some(cbor.serialise(builtin.i_data(8))),
            },
          ),
        ),
      ),
      [],
    ),
  }
}

test cek_context_seed_fits_one_step() {
  let tx = empty_v1_transaction()
  let native_control = cek_context_native_control_fixture(tx)
  let empty_items = cek_data_v1.empty_data_list_summary_v1()
  let empty_pairs = cek_data_v1.empty_data_pair_summary_v1()
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let tx_info = script_context_v1.tx_info_summary_v1(
    False,
    empty_items,
    empty_items,
    empty_items,
    0,
    -1,
    -1,
    observer_summary,
    empty_items,
    cek_data_v1.map_data_summary_v1(empty_pairs),
    empty_pairs,
    native_tx_id_for_version(
      native_tx_version_v1,
      encode_native_tx_body_compact(midgard_transaction_to_compact(tx).body),
    ),
  )
  let redeemer = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  expect Some(script_info) = script_context_v1.script_purpose_summary_v1(
    1,
    h_28_a,
    #"",
    False,
  )
  let final_context = validation_machine_v1.CekFinalContextControlV1 {
    tx_info,
    redeemer,
    script_info,
  }
  let context_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(13),
    purpose_kind: 1,
    observer_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(empty_pairs),
    redeemer_context_control_hash: validation_machine_v1.hash_cek_final_context_control_v1(
      final_context,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    55,
  )
  let context_summary = script_context_v1.script_context_summary_v1(
    tx_info,
    redeemer,
    script_info,
  )
  let context_value = cek_constant_v1.semantic_data_constant_root_v1(
    context_summary,
  )
  let initial_state = cek_machine_v1.MachineStateV1 {
    mode: cek_machine_v1.mode_compute,
    execution_index: 0,
    // The seeded context constant is the runtime-only tag-10 term
    // (`cek_proof_v1.hash_context_constant_term_v1`), not the ordinary tag-4
    // constant a source program may contain. `cek_context_execution_successor_is_exact`
    // (validation-machine-v1.ak:15953) deliberately uses the tag-10 form so a
    // canonical source program can never forge the authenticated script
    // context as one of its own constants.
    focus_root: cek_proof_v1.hash_application_term_v1(
      context_control.program_term_root,
      cek_proof_v1.hash_context_constant_term_v1(context_value),
    ),
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root: cek_proof_v1.empty_continuation_root_v1,
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  let post_work = validation_machine_v1.encode_cek_witness_v1(
    native_control,
    #"",
    0,
    0,
    0,
    cek_machine_v1.hash_state_v1(initial_state),
    context_control.execution_cpu_limit,
    context_control.execution_memory_limit,
    context_control.program_envelope_hash,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 56,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      56,
      post_work,
    ),
  }
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.CekContextSeedWitness {
        control: final_context,
      },
    },
    inline_field_door(),
  )
}

test cek_context_assemble_fits_one_step() {
  let tx = empty_v1_transaction()
  let native_control = cek_context_native_control_fixture(tx)
  let empty_items = cek_data_v1.empty_data_list_summary_v1()
  let empty_pairs = cek_data_v1.empty_data_pair_summary_v1()
  let redeemer = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  expect Some(script_info) = script_context_v1.script_purpose_summary_v1(
    1,
    h_28_a,
    #"",
    False,
  )
  let parts = validation_machine_v1.CekContextPartsControlV1 {
    redeemer_items: empty_pairs,
    redeemer,
    script_info,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let context_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(11),
    purpose_kind: 1,
    observer_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(empty_pairs),
    redeemer_context_control_hash: validation_machine_v1.hash_cek_context_parts_control_v1(
      parts,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    55,
  )
  let tail_fields = script_context_v1.tx_info_tail_fields_summary_v1(
    False,
    observer_summary,
    empty_items,
    cek_data_v1.map_data_summary_v1(empty_pairs),
    empty_pairs,
    pre.transaction_id,
  )
  let assembly = validation_machine_v1.CekTxInfoAssemblyControlV1 {
    tail_fields,
    redeemer,
    script_info,
  }
  let next_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    stage: 12,
    redeemer_context_control_hash: validation_machine_v1.hash_cek_tx_info_assembly_control_v1(
      assembly,
    ),
  }
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(next_control),
    0,
    56,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.CekContextAssembleWitness {
        control: parts,
      },
    },
    inline_field_door(),
  )
}

test cek_context_tx_info_finalize_fits_one_step() {
  let tx = empty_v1_transaction()
  let native_control = cek_context_native_control_fixture(tx)
  let compact = midgard_transaction_to_compact(tx)
  let transaction_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let empty_items = cek_data_v1.empty_data_list_summary_v1()
  let empty_pairs = cek_data_v1.empty_data_pair_summary_v1()
  let redeemer = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  expect Some(script_info) = script_context_v1.script_purpose_summary_v1(
    1,
    h_28_a,
    #"",
    False,
  )
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let mint = cek_data_v1.map_data_summary_v1(empty_pairs)
  let assembly = validation_machine_v1.CekTxInfoAssemblyControlV1 {
    tail_fields: script_context_v1.tx_info_tail_fields_summary_v1(
      False,
      observer_summary,
      empty_items,
      mint,
      empty_pairs,
      transaction_id,
    ),
    redeemer,
    script_info,
  }
  let context_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(12),
    purpose_kind: 1,
    observer_summary,
    mint_summary: mint,
    redeemer_context_control_hash: validation_machine_v1.hash_cek_tx_info_assembly_control_v1(
      assembly,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    55,
  )
  let final_context = validation_machine_v1.CekFinalContextControlV1 {
    tx_info: script_context_v1.tx_info_from_tail_summary_v1(
      False,
      empty_items,
      empty_items,
      empty_items,
      compact.body.fee,
      compact.body.validity_interval_start,
      compact.body.validity_interval_end,
      observer_summary,
      mint,
      assembly.tail_fields,
    ),
    redeemer,
    script_info,
  }
  let next_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    stage: 13,
    redeemer_context_control_hash: validation_machine_v1.hash_cek_final_context_control_v1(
      final_context,
    ),
  }
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(next_control),
    0,
    56,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.CekTxInfoFinalizeWitness {
        control: assembly,
      },
    },
    inline_field_door(),
  )
}

/// The mint boundary's terminal fold, plus the four Merkle attack classes the
/// counted scheme needed per-item openings to refute.
///
/// **How the four controls changed shape, and why they are stronger.** Under the
/// counted scheme each control forged an `ItemProofV1` — a substituted item
/// commitment, a reordered pair of leaves, an omitted leaf, a duplicated one —
/// and asserted that `bounded_collection_v1.verify_item` refused it against the
/// field root. §4 replaced that root with a flat `blake2b_256` over the field's
/// whole §5.1 preimage, so each attack is now a claim about *bytes*: it produces
/// a different preimage, whose commitment is not the committed one, and §8's
/// door refuses it before any rule runs. The controls therefore construct the
/// perturbed preimage — from the authenticated one, through §5.1's own reader
/// and encoder, so no attack is hand-spelled — and measure that its commitment
/// differs from the field's. That is a statement about every one of the field's
/// 130 items at once, where the counted controls only ever bound the two leaves
/// their siblings reached.
fn verify_maximum_mint_field_terminal_fixture_v1(
  fixture: MaximumFieldTerminalFixtureV1,
) -> Bool {
  let terminal = terminal_fixture_item(fixture)
  let chunk_length = bytearray.length(terminal)
  let policy_mutation = replace_byte_v1(terminal, 3, #"fe")
  let asset_name_mutation = replace_byte_v1(terminal, 33, #"4e")
  let sign_mutation = replace_byte_v1(terminal, 42, #"20")

  and {
    verify_maximum_field_terminal_fixture_v1(fixture),
    chunk_length == 43,
    builtin.slice_bytearray(3, 1, terminal) == #"ff",
    builtin.slice_bytearray(33, 1, terminal) == #"4d",
    builtin.slice_bytearray(42, 1, terminal) == #"01",
    verify_terminal_substitution_control_v1(fixture, policy_mutation),
    verify_terminal_substitution_control_v1(fixture, asset_name_mutation),
    verify_terminal_substitution_control_v1(fixture, sign_mutation),
    verify_terminal_reorder_control_v1(fixture),
    verify_terminal_omission_control_v1(fixture),
    verify_terminal_duplicate_control_v1(fixture),
  }
}

fn replace_byte_v1(
  bytes: ByteArray,
  offset: Int,
  replacement: ByteArray,
) -> ByteArray {
  expect bytearray.length(replacement) == 1
  expect offset >= 0
  expect offset < bytearray.length(bytes)
  builtin.slice_bytearray(0, offset, bytes)
    |> bytearray.concat(replacement)
    |> bytearray.concat(
        builtin.slice_bytearray(
          offset + 1,
          bytearray.length(bytes) - offset - 1,
          bytes,
        ),
      )
}

/// `items` with its last element replaced by `replacement`.
fn with_last_item_replaced(
  items: List<ByteArray>,
  replacement: ByteArray,
) -> List<ByteArray> {
  let count = list.length(items)
  list.concat(list.take(items, count - 1), [replacement])
}

/// The commitment of a perturbed field preimage, re-encoded through §5.1 so the
/// header count and every wrapper follow from the item list rather than from a
/// hand-spelled byte string.
fn perturbed_field_commitment(items: List<ByteArray>) -> ByteArray {
  native_tx_field_access_v1.field_commitment(
    native_tx_field_access_v1.encode_field_preimage(items),
  )
}

/// Substitution: one item's bytes replaced. The committed preimage is not the
/// substituted one, so the door never opens it.
fn verify_terminal_substitution_control_v1(
  fixture: MaximumFieldTerminalFixtureV1,
  alternate_item: ByteArray,
) -> Bool {
  let items = terminal_fixture_items(fixture)
  and {
    alternate_item != terminal_fixture_item(fixture),
    perturbed_field_commitment(with_last_item_replaced(items, alternate_item)) != terminal_fixture_field_slot_commitment(
      fixture,
    ),
  }
}

/// Reorder: the last two items swapped. The counted scheme needed this because a
/// per-item opening says nothing about position beyond the leaf it hashes; a
/// flat preimage hash is order-sensitive by construction, and this measures it
/// on the field the fixture actually pins.
fn verify_terminal_reorder_control_v1(
  fixture: MaximumFieldTerminalFixtureV1,
) -> Bool {
  let items = terminal_fixture_items(fixture)
  let count = list.length(items)
  expect Some(terminal) = list.last(items)
  expect Some(penultimate) = list.at(items, count - 2)
  let reordered = list.concat(
    list.take(items, count - 2),
    [terminal, penultimate],
  )
  and {
    terminal != penultimate,
    perturbed_field_commitment(reordered) != terminal_fixture_field_slot_commitment(
      fixture,
    ),
  }
}

/// Omission: the terminal item dropped, with §5.1's header count following it
/// down. Both halves of the envelope move, which is why the encoder rather than
/// a byte edit builds it.
fn verify_terminal_omission_control_v1(
  fixture: MaximumFieldTerminalFixtureV1,
) -> Bool {
  let items = terminal_fixture_items(fixture)
  let count = list.length(items)
  let omitted = list.take(items, count - 1)
  and {
    list.length(omitted) + 1 == fixture.item_count,
    perturbed_field_commitment(omitted) != terminal_fixture_field_slot_commitment(
      fixture,
    ),
  }
}

/// Duplication: the terminal item appended a second time. The retired control
/// forged a leaf at `item_count`; the flat commitment refuses the whole preimage.
fn verify_terminal_duplicate_control_v1(
  fixture: MaximumFieldTerminalFixtureV1,
) -> Bool {
  let items = terminal_fixture_items(fixture)
  let duplicated = list.concat(items, [terminal_fixture_item(fixture)])
  and {
    list.length(duplicated) == fixture.item_count + 1,
    perturbed_field_commitment(duplicated) != terminal_fixture_field_slot_commitment(
      fixture,
    ),
  }
}

test maximum_mint_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_mint_field_terminal_fixture_v1()),
) {
  verify_maximum_mint_field_terminal_fixture_v1(fixture)
}

test cek_context_maximum_mint_initializes_from_authenticated_frontier() {
  let mint = decode_midgard_tx_mint_preimage_cbor(maximum_mint_preimage_cbor())
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  let mint_frontier = validation_merkle_v1.build_frontier(
    maximum_mint_asset_leaves(0, 128),
  )
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let init_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(6),
    observer_summary,
  }
  let semantic_control = validation_machine_v1.CekContextControlV1 {
    ..init_control,
    stage: 8,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(init_control),
    0,
    51,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(semantic_control),
    0,
    52,
  )
  validation_machine_v1.verify_cek_one_step_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
    inline_field_door(),
  )
}

test cek_context_maximum_mint_authenticates_last_asset_membership() {
  let mint = decode_midgard_tx_mint_preimage_cbor(maximum_mint_preimage_cbor())
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  let mint_frontier = validation_merkle_v1.build_frontier(
    maximum_mint_asset_leaves(0, 128),
  )
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let semantic_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(8),
    observer_summary,
  }
  let last_asset = bytearray.from_int_big_endian(127, 32)
  let asset_items = cek_data_v1.prepend_data_pair_summary_v1(
    builtin.b_data(last_asset) |> cek_data_v1.semantic_data_summary_v1,
    builtin.i_data(1) |> cek_data_v1.semantic_data_summary_v1,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let next_control = validation_machine_v1.CekContextControlV1 {
    ..semantic_control,
    mint_cursor: 1,
    current_mint_policy: h_28_a,
    current_mint_assets: asset_items,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(semantic_control),
    0,
    53,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(next_control),
    0,
    54,
  )
  validation_machine_v1.verify_cek_one_step_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.CekMintContextItemWitness {
        mint_index: 127,
        policy_id: h_28_a,
        asset_name: last_asset,
        quantity: 1,
        siblings: maximum_mint_last_asset_siblings(0),
      },
    },
    inline_field_door(),
  )
}

test cek_active_core_step_replays_exact_machine_transition() {
  let tx = empty_v1_transaction()
  let purpose_frontier = validation_merkle_v1.build_frontier([h_a])
  let execution_frontier = validation_merkle_v1.build_frontier([h_b])
  let control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  let application_root = cek_proof_v1.hash_application_term_v1(h_a, h_b)
  let machine_pre = cek_machine_v1.MachineStateV1 {
    mode: cek_machine_v1.mode_compute,
    execution_index: 0,
    focus_root: application_root,
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root: cek_proof_v1.empty_continuation_root_v1,
    auxiliary: 0,
    cpu: 10,
    memory: 11,
  }
  let continuation_root = cek_proof_v1.hash_apply_argument_continuation_v1(
    h_b,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
  )
  let machine_post = cek_machine_v1.MachineStateV1 {
    ..machine_pre,
    focus_root: h_a,
    continuation_root,
    cpu: 16010,
    memory: 111,
  }
  let active_pre_hash = cek_proof_v1.hash_machine_state_v1(
    machine_pre.mode,
    machine_pre.execution_index,
    machine_pre.focus_root,
    machine_pre.environment_root,
    machine_pre.continuation_root,
    machine_pre.auxiliary,
    machine_pre.cpu,
    machine_pre.memory,
  )
  let active_post_hash = cek_proof_v1.hash_machine_state_v1(
    machine_post.mode,
    machine_post.execution_index,
    machine_post.focus_root,
    machine_post.environment_root,
    machine_post.continuation_root,
    machine_post.auxiliary,
    machine_post.cpu,
    machine_post.memory,
  )
  let work_witness_cbor = validation_machine_v1.encode_cek_witness_v1(
    control,
    #"",
    0,
    0,
    0,
    active_pre_hash,
    1000000,
    1000000,
    h_b,
  )
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.Cek,
    program_counter: 40,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      40,
      work_witness_cbor,
    ),
    execution_cpu: 10,
    execution_memory: 11,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      41,
      validation_machine_v1.encode_cek_witness_v1(
        control,
        #"",
        0,
        0,
        0,
        active_post_hash,
        1000000,
        1000000,
        h_b,
      ),
    ),
    execution_cpu: 16010,
    execution_memory: 111,
  }
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary: validation_machine_v1.CekCoreStepWitness {
      step: cek_machine_v1.CoreStepEvidenceV1 {
        pre: machine_pre,
        post: machine_post,
        witness: cek_machine_v1.ComputeApplication {
          function: h_a,
          argument: h_b,
        },
      },
    },
  }
  and {
    validation_machine_v1.verify_one_step_evidence(
      pre,
      evidence,
      inline_field_door(),
    ),
    !validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        ..evidence,
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          ..evidence.transition,
          claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
            ..post,
            execution_cpu: 16009,
          },
        },
      },
      inline_field_door(),
    ),
  }
}

fn ledger_delta_operation_authentication_fixture(operation_kind: Int) -> Bool {
  let descriptor = empty_mpf_proof_descriptor()
  let output_cursor = 0
  let key = if operation_kind == 0 {
    #"0102"
  } else {
    encode_midgard_tx_input(
      MidgardTxInput { tx_id: h_a, output_index: output_cursor },
    )
  }
  let value = if operation_kind == 0 {
    #""
  } else {
    #"0304"
  }
  let operation_leaf = validation_machine_v1.ledger_delta_operation_leaf_hash(
    operation_kind,
    key,
    value,
    descriptor,
  )
  let operation_frontier = validation_merkle_v1.build_frontier([operation_leaf])
  let output_descriptor_frontier = validation_merkle_v1.build_frontier([h_b])
  let control = validation_machine_v1.LedgerDeltaControlV1 {
    resolved_input_count: 1,
    resolved_inputs_accumulator: h_b,
    output_count: 1,
    output_descriptor_peaks: output_descriptor_frontier.peaks,
    stage: operation_kind,
    replay_schedule_hash: h_c,
    replay_cursor: 0,
    replay_accumulator: h_b,
    replay_remaining_schedule_hash: h_c,
    current_ledger_root: h_c,
    output_cursor,
    operation_count: 0,
    operation_peaks: [],
    pending_mutation_cbor: #"",
  }
  let work_witness_cbor = validation_machine_v1.encode_ledger_delta_control_v1(
    control,
  )
  let pending_mutation_cbor = validation_machine_v1.encode_ledger_delta_pending_mutation_v1(
    validation_machine_v1.LedgerDeltaPendingMutationV1 {
      authorization_stage: 0,
      operation_kind,
      key,
      value,
      descriptor,
      fold_control: mpf_proof_fold_v1.ProofFoldControlV1 {
        next_frame_index: -1,
        expected_next_cursor: 0,
        including_root: #"",
        excluding_root: #"",
      },
    },
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: h_a,
    transaction_commitment: h_b,
    validation_context_hash: h_c,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.LedgerDelta,
    program_counter: 48,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      48,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_merkle_v1.frontier_commitment(
      operation_frontier.count,
      operation_frontier.peaks,
    ),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 49,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      49,
      validation_machine_v1.encode_ledger_delta_control_v1(
        validation_machine_v1.LedgerDeltaControlV1 {
          ..control,
          pending_mutation_cbor,
        },
      ),
    ),
  }
  validation_machine_v1.verify_ledger_delta_operation_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    operation_kind,
    key,
    value,
    validation_machine_v1.LedgerDeltaOperationProofV1 {
      descriptor,
      operation_count: operation_frontier.count,
      operation_peaks: operation_frontier.peaks,
      operation_index: 0,
      operation_siblings: [],
    },
  )
}

test ledger_delta_spend_step_authenticates_deletion_operation() {
  ledger_delta_operation_authentication_fixture(0)
}

test ledger_delta_output_step_authenticates_insertion_operation() {
  ledger_delta_operation_authentication_fixture(1)
}

fn compact_ledger_output_descriptor(output_index: Int) -> ByteArray {
  let summary = cek_data_v1.DataSummaryV1 {
    root: h_a,
    cbor_length: 0,
    memory: 0,
  }
  ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      version: ledger_output_commitment_v1.version,
      output_index,
      total_length: 0,
      item_commitment: h_b,
      address: #"70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      lovelace: 0,
      asset_count: 0,
      asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
      cardano_value_size: 0,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out: summary,
      midgard_tx_out: summary,
      cardano_spend_datum: summary,
    },
  )
}

fn compact_ledger_delta_control(
  stage: Int,
) -> validation_machine_v1.LedgerDeltaControlV1 {
  validation_machine_v1.LedgerDeltaControlV1 {
    resolved_input_count: 0,
    resolved_inputs_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    output_count: 0,
    output_descriptor_peaks: [],
    stage,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    current_ledger_root: h_c,
    output_cursor: 0,
    operation_count: 0,
    operation_peaks: [],
    pending_mutation_cbor: #"",
  }
}

fn compact_ledger_delta_state(
  control: validation_machine_v1.LedgerDeltaControlV1,
  program_counter: Int,
  ledger_delta_root: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: h_a,
    transaction_commitment: h_b,
    validation_context_hash: h_c,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.LedgerDelta,
    program_counter,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      program_counter,
      validation_machine_v1.encode_ledger_delta_control_v1(control),
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root,
  }
}

fn compact_ledger_delta_successor(
  pre: validation_trace_v1.ValidationMachineStateV1,
  control: validation_machine_v1.LedgerDeltaControlV1,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      pre.program_counter + 1,
      validation_machine_v1.encode_ledger_delta_control_v1(control),
    ),
  }
}

test ledger_delta_reference_replay_authenticates_descriptor() {
  let key = encode_midgard_tx_input(
    MidgardTxInput { tx_id: h_a, output_index: 0 },
  )
  let value = compact_ledger_output_descriptor(0)
  let next_schedule_hash = validation_machine_v1.empty_resolution_schedule_hash()
  let schedule_hash = validation_machine_v1.resolution_schedule_node_hash(
    1,
    key,
    next_schedule_hash,
  )
  let resolved_accumulator = validation_machine_v1.resolved_input_accumulator_successor(
    validation_machine_v1.initial_resolution_accumulator(),
    1,
    key,
    value,
  )
  let control = validation_machine_v1.LedgerDeltaControlV1 {
    ..compact_ledger_delta_control(0),
    resolved_input_count: 1,
    resolved_inputs_accumulator: resolved_accumulator,
    replay_schedule_hash: schedule_hash,
    replay_remaining_schedule_hash: schedule_hash,
  }
  let pre = compact_ledger_delta_state(
    control,
    50,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  let post = compact_ledger_delta_successor(
    pre,
    validation_machine_v1.LedgerDeltaControlV1 {
      ..control,
      replay_cursor: 1,
      replay_accumulator: resolved_accumulator,
      replay_remaining_schedule_hash: next_schedule_hash,
    },
  )
  validation_machine_v1.verify_ledger_delta_replay_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: validation_machine_v1.encode_ledger_delta_control_v1(
        control,
      ),
      claimed_successor: post,
    },
    1,
    key,
    next_schedule_hash,
    value,
  )
}

test ledger_delta_spend_replay_initializes_authenticated_fold() {
  let key = encode_midgard_tx_input(
    MidgardTxInput { tx_id: h_a, output_index: 0 },
  )
  let value = compact_ledger_output_descriptor(0)
  let descriptor = empty_mpf_proof_descriptor()
  let next_schedule_hash = validation_machine_v1.empty_resolution_schedule_hash()
  let schedule_hash = validation_machine_v1.resolution_schedule_node_hash(
    0,
    key,
    next_schedule_hash,
  )
  let resolved_accumulator = validation_machine_v1.resolved_input_accumulator_successor(
    validation_machine_v1.initial_resolution_accumulator(),
    0,
    key,
    value,
  )
  let authorized_pending = validation_machine_v1.encode_ledger_delta_pending_mutation_v1(
    validation_machine_v1.LedgerDeltaPendingMutationV1 {
      authorization_stage: 0,
      operation_kind: 0,
      key,
      value: #"",
      descriptor,
      fold_control: mpf_proof_fold_v1.ProofFoldControlV1 {
        next_frame_index: -1,
        expected_next_cursor: 0,
        including_root: #"",
        excluding_root: #"",
      },
    },
  )
  let control = validation_machine_v1.LedgerDeltaControlV1 {
    ..compact_ledger_delta_control(0),
    resolved_input_count: 1,
    resolved_inputs_accumulator: resolved_accumulator,
    replay_schedule_hash: schedule_hash,
    replay_remaining_schedule_hash: schedule_hash,
    current_ledger_root: singleton_root(key, value),
    pending_mutation_cbor: authorized_pending,
  }
  expect Some(fold_control) = mpf_proof_fold_v1.initial_fold_control_v1(
    key,
    value,
    descriptor,
  )
  let post = compact_ledger_delta_successor(
    compact_ledger_delta_state(
      control,
      51,
      validation_merkle_v1.frontier_commitment(0, []),
    ),
    validation_machine_v1.LedgerDeltaControlV1 {
      ..control,
      replay_cursor: 1,
      replay_accumulator: resolved_accumulator,
      replay_remaining_schedule_hash: next_schedule_hash,
      pending_mutation_cbor: validation_machine_v1.encode_ledger_delta_pending_mutation_v1(
        validation_machine_v1.LedgerDeltaPendingMutationV1 {
          authorization_stage: 1,
          operation_kind: 0,
          key,
          value,
          descriptor,
          fold_control,
        },
      ),
    },
  )
  let pre = compact_ledger_delta_state(
    control,
    51,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  validation_machine_v1.verify_ledger_delta_replay_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: validation_machine_v1.encode_ledger_delta_control_v1(
        control,
      ),
      claimed_successor: post,
    },
    0,
    key,
    next_schedule_hash,
    value,
  )
}

test ledger_delta_replay_finish_is_exact() {
  let control = compact_ledger_delta_control(0)
  let pre = compact_ledger_delta_state(
    control,
    52,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  validation_machine_v1.verify_ledger_delta_replay_finish_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: validation_machine_v1.encode_ledger_delta_control_v1(
        control,
      ),
      claimed_successor: compact_ledger_delta_successor(
        pre,
        validation_machine_v1.LedgerDeltaControlV1 { ..control, stage: 1 },
      ),
    },
  )
}

test ledger_delta_output_authenticates_descriptor_and_initializes_fold() {
  let output_index = 0
  let key = encode_midgard_tx_input(MidgardTxInput { tx_id: h_a, output_index })
  let value = compact_ledger_output_descriptor(output_index)
  let descriptor = empty_mpf_proof_descriptor()
  let descriptor_leaf = script_proof_v1.output_descriptor_leaf_hash(
    output_index,
    value,
  )
  let descriptor_frontier = validation_merkle_v1.build_frontier(
    [descriptor_leaf],
  )
  let authorized_pending = validation_machine_v1.encode_ledger_delta_pending_mutation_v1(
    validation_machine_v1.LedgerDeltaPendingMutationV1 {
      authorization_stage: 0,
      operation_kind: 1,
      key,
      value,
      descriptor,
      fold_control: mpf_proof_fold_v1.ProofFoldControlV1 {
        next_frame_index: -1,
        expected_next_cursor: 0,
        including_root: #"",
        excluding_root: #"",
      },
    },
  )
  let control = validation_machine_v1.LedgerDeltaControlV1 {
    ..compact_ledger_delta_control(1),
    output_count: 1,
    output_descriptor_peaks: descriptor_frontier.peaks,
    current_ledger_root: null_hash,
    pending_mutation_cbor: authorized_pending,
  }
  expect Some(fold_control) = mpf_proof_fold_v1.initial_fold_control_v1(
    key,
    value,
    descriptor,
  )
  let pre = compact_ledger_delta_state(
    control,
    53,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  validation_machine_v1.verify_ledger_delta_output_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: validation_machine_v1.encode_ledger_delta_control_v1(
        control,
      ),
      claimed_successor: compact_ledger_delta_successor(
        pre,
        validation_machine_v1.LedgerDeltaControlV1 {
          ..control,
          output_cursor: 1,
          pending_mutation_cbor: validation_machine_v1.encode_ledger_delta_pending_mutation_v1(
            validation_machine_v1.LedgerDeltaPendingMutationV1 {
              authorization_stage: 1,
              operation_kind: 1,
              key,
              value,
              descriptor,
              fold_control,
            },
          ),
        },
      ),
    },
    output_index,
    value,
    [],
  )
}

test ledger_delta_output_finish_is_exact() {
  let control = compact_ledger_delta_control(1)
  let pre = compact_ledger_delta_state(
    control,
    54,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  validation_machine_v1.verify_ledger_delta_output_finish_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: validation_machine_v1.encode_ledger_delta_control_v1(
        control,
      ),
      claimed_successor: compact_ledger_delta_successor(
        pre,
        validation_machine_v1.LedgerDeltaControlV1 { ..control, stage: 2 },
      ),
    },
  )
}

test ledger_delta_proof_frame_advances_only_authenticated_frame() {
  let key = #"616273656e742d6b6579"
  let value = #"696e7365727465642d76616c7565"
  let neighbors = h_a
    |> bytearray.concat(h_b)
    |> bytearray.concat(h_c)
    |> bytearray.concat(zero)
  let frame = mpf_proof_fold_v1.ProofFrameV1 {
    version: 1,
    frame_index: 0,
    cursor: 0,
    next_cursor: 1,
    step: mpf.Branch { skip: 0, neighbors },
  }
  let frame_frontier = validation_merkle_v1.build_frontier(
    [mpf_proof_fold_v1.proof_frame_leaf_hash_v1(frame)],
  )
  let descriptor = mpf_proof_fold_v1.ProofDescriptorV1 {
    version: 1,
    frame_count: 1,
    terminal_cursor: 1,
    peaks: frame_frontier.peaks,
  }
  expect Some(fold_control) = mpf_proof_fold_v1.initial_fold_control_v1(
    key,
    value,
    descriptor,
  )
  expect Some(next_fold_control) = mpf_proof_fold_v1.fold_proof_frame_v1(
    key,
    descriptor,
    fold_control,
    frame,
    [],
  )
  let pending = validation_machine_v1.LedgerDeltaPendingMutationV1 {
    authorization_stage: 1,
    operation_kind: 1,
    key,
    value,
    descriptor,
    fold_control,
  }
  let control = validation_machine_v1.LedgerDeltaControlV1 {
    ..compact_ledger_delta_control(1),
    pending_mutation_cbor: validation_machine_v1.encode_ledger_delta_pending_mutation_v1(
      pending,
    ),
  }
  let pre = compact_ledger_delta_state(
    control,
    55,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  validation_machine_v1.verify_ledger_delta_proof_frame_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: validation_machine_v1.encode_ledger_delta_control_v1(
        control,
      ),
      claimed_successor: compact_ledger_delta_successor(
        pre,
        validation_machine_v1.LedgerDeltaControlV1 {
          ..control,
          pending_mutation_cbor: validation_machine_v1.encode_ledger_delta_pending_mutation_v1(
            validation_machine_v1.LedgerDeltaPendingMutationV1 {
              ..pending,
              fold_control: next_fold_control,
            },
          ),
        },
      ),
    },
    frame,
    [],
  )
}

test ledger_delta_finalize_applies_only_authenticated_root_transition() {
  let key = #"696e736572742d6b6579"
  let value = #"696e736572742d76616c7565"
  let descriptor = empty_mpf_proof_descriptor()
  expect Some(fold_control) = mpf_proof_fold_v1.initial_fold_control_v1(
    key,
    value,
    descriptor,
  )
  let pending = validation_machine_v1.LedgerDeltaPendingMutationV1 {
    authorization_stage: 1,
    operation_kind: 1,
    key,
    value,
    descriptor,
    fold_control,
  }
  let operation_leaf = validation_machine_v1.ledger_delta_operation_leaf_hash(
    1,
    key,
    value,
    descriptor,
  )
  let operation_frontier = validation_merkle_v1.build_frontier([operation_leaf])
  let control = validation_machine_v1.LedgerDeltaControlV1 {
    ..compact_ledger_delta_control(1),
    current_ledger_root: fold_control.excluding_root,
    pending_mutation_cbor: validation_machine_v1.encode_ledger_delta_pending_mutation_v1(
      pending,
    ),
  }
  let pre = compact_ledger_delta_state(
    control,
    56,
    validation_merkle_v1.frontier_commitment(
      operation_frontier.count,
      operation_frontier.peaks,
    ),
  )
  validation_machine_v1.verify_ledger_delta_finalize_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: validation_machine_v1.encode_ledger_delta_control_v1(
        control,
      ),
      claimed_successor: compact_ledger_delta_successor(
        pre,
        validation_machine_v1.LedgerDeltaControlV1 {
          ..control,
          current_ledger_root: fold_control.including_root,
          operation_count: 1,
          operation_peaks: operation_frontier.peaks,
          pending_mutation_cbor: #"",
        },
      ),
    },
  )
}

test value_and_mint_final_equation_is_enforced_on_l1() {
  let tx = empty_v1_transaction()
  let native_control = empty_native_control_fixture(tx)
  let valid_control = validation_machine_v1.ValueAndMintControlV1 {
    native_control,
    stage: 5,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 0,
    mint_cursor: 0,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: null_hash,
      seen_asset_count: 0,
      nonzero_asset_count: 0,
    },
  }
  let valid_work_witness_cbor = validation_machine_v1.encode_value_and_mint_control_v1(
    valid_control,
  )
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let valid_pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 40,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      40,
      valid_work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let valid_post = validation_trace_v1.ValidationMachineStateV1 {
    ..valid_pre,
    phase: validation_trace_v1.LedgerDelta,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      41,
      validation_machine_v1.encode_ledger_delta_witness_v1(
        native_control,
        valid_pre.prior_ledger_root,
        valid_control.replay_schedule_hash,
      ),
    ),
  }
  let invalid_control = validation_machine_v1.ValueAndMintControlV1 {
    ..valid_control,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 1,
      asset_root: null_hash,
      seen_asset_count: 0,
      nonzero_asset_count: 0,
    },
  }
  let invalid_work_witness_cbor = validation_machine_v1.encode_value_and_mint_control_v1(
    invalid_control,
  )
  let invalid_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..valid_pre,
    program_counter: 42,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      42,
      invalid_work_witness_cbor,
    ),
    // Re-based off `frontier_commitment(0, [])`: this pre-state backs a
    // rejection proof, so it must carry the realistic non-empty claimed delta.
    ledger_delta_root: non_empty_claimed_delta_root(),
  }
  let rejection_code = #"455f56414c55455f4e4f545f505245534552564544"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..invalid_pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 43,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      43,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        invalid_pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  and {
    validation_machine_v1.verify_value_and_mint_one_step_v1(
      valid_pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: valid_work_witness_cbor,
          claimed_successor: valid_post,
        },
        auxiliary: validation_machine_v1.NoAuxiliaryWitness,
      },
    ),
    validation_machine_v1.verify_value_and_mint_one_step_v1(
      invalid_pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: invalid_work_witness_cbor,
          claimed_successor: terminal,
        },
        auxiliary: validation_machine_v1.NoAuxiliaryWitness,
      },
    ),
  }
}

test value_and_mint_uses_authenticated_native_mint_frontier() {
  // §5.6 — the enveloped list of per-policy items, not the retired raw map.
  let mint_cbor = native_tx_field_access_v1.encode_field_preimage(
    [
      #"82581c"
        |> bytearray.concat(h_28_a)
        |> bytearray.concat(#"a1410124"),
    ],
  )
  let mint = decode_midgard_tx_mint_preimage_cbor(mint_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  let mint_leaf = validation_machine_v1.mint_asset_leaf_hash(h_28_a, #"01", -5)
  let mint_frontier = validation_merkle_v1.build_frontier([mint_leaf])
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = validation_machine_v1.ValueAndMintControlV1 {
    native_control,
    stage: 1,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 0,
    mint_cursor: 0,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: null_hash,
      seen_asset_count: 0,
      nonzero_asset_count: 0,
    },
  }
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = validation_machine_v1.encode_value_and_mint_control_v1(
    control,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 40,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      40,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      41,
      validation_machine_v1.encode_value_and_mint_control_v1(
        validation_machine_v1.ValueAndMintControlV1 { ..control, stage: 2 },
      ),
    ),
  }
  let valid_step = validation_machine_v1.verify_value_and_mint_one_step_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
  )
  let malformed_native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..native_control,
    mint_count: 0,
  }
  let malformed_control = validation_machine_v1.ValueAndMintControlV1 {
    ..control,
    native_control: malformed_native_control,
  }
  let malformed_work_witness_cbor = validation_machine_v1.encode_value_and_mint_control_v1(
    malformed_control,
  )
  let malformed_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      pre.program_counter,
      malformed_work_witness_cbor,
    ),
  }
  let malformed_post = validation_trace_v1.ValidationMachineStateV1 {
    ..post,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      post.program_counter,
      validation_machine_v1.encode_value_and_mint_control_v1(
        validation_machine_v1.ValueAndMintControlV1 {
          ..malformed_control,
          stage: 2,
        },
      ),
    ),
  }
  and {
    valid_step,
    !validation_machine_v1.verify_value_and_mint_one_step_v1(
      malformed_pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: malformed_work_witness_cbor,
          claimed_successor: malformed_post,
        },
        auxiliary: validation_machine_v1.NoAuxiliaryWitness,
      },
    ),
  }
}

test value_and_mint_replays_signed_burn_leaf_on_l1() {
  let tx = empty_v1_transaction()
  let policy_id = #"11111111111111111111111111111111111111111111111111111111"
  let asset_name = #"beef"
  let unit = policy_id |> bytearray.concat(asset_name)
  let leaf = validation_machine_v1.mint_asset_leaf_hash(
    policy_id,
    asset_name,
    -5,
  )
  let mint_frontier = validation_merkle_v1.build_frontier([leaf])
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = validation_machine_v1.ValueAndMintControlV1 {
    native_control,
    stage: 4,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 0,
    mint_cursor: 0,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: singleton_root(unit, cbor.serialise(5)),
      seen_asset_count: 1,
      nonzero_asset_count: 1,
    },
  }
  let work_witness_cbor = validation_machine_v1.encode_value_and_mint_control_v1(
    control,
  )
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 44,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      44,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let next_control = validation_machine_v1.ValueAndMintControlV1 {
    ..control,
    mint_cursor: 1,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: singleton_root(unit, cbor.serialise(0)),
      seen_asset_count: 1,
      nonzero_asset_count: 0,
    },
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 45,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      45,
      validation_machine_v1.encode_value_and_mint_control_v1(next_control),
    ),
  }
  validation_machine_v1.verify_value_and_mint_one_step_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.ValueMintAssetWitness {
        mint_index: 0,
        policy_id,
        asset_name,
        quantity: -5,
        siblings: [],
        mutation: validation_machine_v1.ValueAssetMutationWitnessV1 {
          delta_was_present: True,
          old_delta: 5,
          delta_proof: [],
        },
      },
    },
  )
}

type ValueOutputDescriptorFixture {
  pre: validation_trace_v1.ValidationMachineStateV1,
  active_state: validation_trace_v1.ValidationMachineStateV1,
  final_state: validation_trace_v1.ValidationMachineStateV1,
  initial_work: ByteArray,
  active_work: ByteArray,
  descriptor_cbor: ByteArray,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  asset_peaks: List<validation_merkle_v1.FrontierPeak>,
}

fn value_output_descriptor_fixture() -> ValueOutputDescriptorFixture {
  let tx = empty_v1_transaction()
  let policy_id = h_28_a
  let asset_name = #"beef"
  let quantity = 7
  let unit = policy_id |> bytearray.concat(asset_name)
  let asset_frontier = validation_merkle_v1.build_frontier(
    [
      ledger_output_commitment_v1.asset_leaf_hash(
        policy_id,
        asset_name,
        quantity,
      ),
    ],
  )
  let empty_summary = cek_data_v1.DataSummaryV1 {
    root: h_a,
    cbor_length: 0,
    memory: 0,
  }
  let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: 0,
    total_length: 16384,
    item_commitment: h_b,
    address: #"60" |> bytearray.concat(h_28_b),
    lovelace: 3,
    asset_count: 1,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(
      asset_frontier.count,
      asset_frontier.peaks,
    ),
    cardano_value_size: 5000,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out: empty_summary,
    midgard_tx_out: empty_summary,
    cardano_spend_datum: empty_summary,
  }
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let descriptor_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_descriptor_leaf_hash(0, descriptor_cbor)],
  )
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    output_count: 1,
    output_peaks: validation_merkle_v1.build_frontier([h_c]).peaks,
    output_descriptor_peaks: descriptor_frontier.peaks,
  }
  let initial_control = validation_machine_v1.ValueAndMintControlV1 {
    native_control,
    stage: 3,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 0,
    mint_cursor: 0,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: null_hash,
      seen_asset_count: 0,
      nonzero_asset_count: 0,
    },
  }
  let active_control = validation_machine_v1.ValueAndMintControlV1 {
    ..initial_control,
    output_asset_cursor: 1,
    replay_value_hash: blake2b_256(descriptor_cbor),
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      ..initial_control.value_accumulator,
      lovelace_delta: -3,
    },
  }
  let final_control = validation_machine_v1.ValueAndMintControlV1 {
    ..active_control,
    output_cursor: 1,
    output_asset_cursor: 0,
    replay_value_hash: zero,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: -3,
      asset_root: singleton_root(unit, cbor.serialise(-7)),
      seen_asset_count: 1,
      nonzero_asset_count: 1,
    },
  }
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let initial_work = validation_machine_v1.encode_value_and_mint_control_v1(
    initial_control,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 46,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      46,
      initial_work,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let active_work = validation_machine_v1.encode_value_and_mint_control_v1(
    active_control,
  )
  let active_state = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 47,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      47,
      active_work,
    ),
  }
  let final_state = validation_trace_v1.ValidationMachineStateV1 {
    ..active_state,
    program_counter: 48,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      48,
      validation_machine_v1.encode_value_and_mint_control_v1(final_control),
    ),
  }
  ValueOutputDescriptorFixture {
    pre,
    active_state,
    final_state,
    initial_work,
    active_work,
    descriptor_cbor,
    policy_id,
    asset_name,
    quantity,
    asset_peaks: asset_frontier.peaks,
  }
}

test value_and_mint_authenticates_output_descriptor_on_l1() {
  let fixture = value_output_descriptor_fixture()
  validation_machine_v1.verify_one_step_evidence(
    fixture.pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: fixture.initial_work,
        claimed_successor: fixture.active_state,
      },
      auxiliary: validation_machine_v1.ValueOutputDescriptorWitness {
        output_index: 0,
        descriptor_cbor: fixture.descriptor_cbor,
        siblings: [],
      },
    },
    inline_field_door(),
  )
}

test value_and_mint_replays_output_descriptor_asset_on_l1() {
  let fixture = value_output_descriptor_fixture()
  validation_machine_v1.verify_one_step_evidence(
    fixture.active_state,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: fixture.active_work,
        claimed_successor: fixture.final_state,
      },
      auxiliary: validation_machine_v1.ValueOutputAssetWitness {
        output_index: 0,
        descriptor_cbor: fixture.descriptor_cbor,
        asset_index: 0,
        policy_id: fixture.policy_id,
        asset_name: fixture.asset_name,
        quantity: fixture.quantity,
        asset_peaks: fixture.asset_peaks,
        asset_siblings: [],
        mutation: validation_machine_v1.ValueAssetMutationWitnessV1 {
          delta_was_present: False,
          old_delta: 0,
          delta_proof: [],
        },
      },
    },
    inline_field_door(),
  )
}

// ---------------------------------------------------------------------------
// C49 (#540) closure -- the four prescribed value/fee/mint selectors.
//
// The six selectors above authenticate the ValueAndMint scan machinery. These
// four close the row's remaining semantics: the canonical parameterized
// minimum-fee formula and the minimum-Ada floor, both instantiated at the C70
// target snapshot with adjacent accept/reject; the exact signed multi-asset
// plus Ada conservation equation, pinned on the same numeric vector the
// TypeScript twin uses; and mint/burn authorization against the authenticated
// mint frontier.
// ---------------------------------------------------------------------------

// The exact canonical size the min-fee formula is evaluated against, for the
// empty V1 transaction carrying `fee`. Size depends on the fee's own CBOR
// width, so the boundary is a fixed point: the tests below assert that the
// probe fee, the boundary fee and the adjacent below-boundary fee all encode to
// the same width and therefore share one canonical size.
fn target_snapshot_canonical_size(fee: Int) -> Int {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, fee },
  }
  let compact = midgard_transaction_to_compact(tx)
  let proof_source = midgard_transaction_proof_source_v1(tx)
  native_tx_canonical_size_v1(
    compact,
    decode_native_tx_field_preimage_lengths_v1(
      proof_source.field_preimage_lengths_cbor,
    ),
  )
}

// One StaticLedgerRules step over the empty V1 transaction carrying `fee`,
// evaluated against the target-snapshot validation context. `claims_advance`
// selects which successor the prover claims: the exact InputSets advance, or
// the exact `E_MIN_FEE` terminal. Every other field is identical between the
// two legs, so the only thing that can move the result is the min-fee conjunct
// of `static_rules_rejection`.
fn target_snapshot_min_fee_step_is_provable(
  fee: Int,
  claims_advance: Bool,
) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, fee },
  }
  let compact = midgard_transaction_to_compact(tx)
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = validation_machine_v1.encode_static_rules_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    target_snapshot_validation_context_cbor,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      target_snapshot_validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.StaticLedgerRules,
    program_counter: 2,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.StaticLedgerRules,
      2,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    // A realistic non-empty claimed delta, per the rejection-fixture rule
    // recorded at `static_rules_prove_a_network_mismatch_is_an_exact_no_op`.
    ledger_delta_root: non_empty_claimed_delta_root(),
  }
  let claimed_successor = if claims_advance {
    validation_trace_v1.ValidationMachineStateV1 {
      ..pre,
      phase: validation_trace_v1.InputSets,
      program_counter: 3,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.InputSets,
        3,
        validation_machine_v1.encode_input_sets_scan_witness(
          proof_source.compact_cbor,
          proof_source.witness_set_compact_cbor,
          proof_source.field_preimage_lengths_cbor,
          target_snapshot_validation_context_cbor,
          0,
          0,
          0,
          0,
          #"",
          validation_machine_v1.empty_resolution_schedule_hash(),
        ),
      ),
    }
  } else {
    exact_rejection_post(pre, reject_min_fee_code)
  }
  validation_machine_v1.verify_one_step(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor,
    },
    inline_field_door(),
  )
}

// C49 (#540). The canonical parameterized minimum-fee rule is
// `body.fee >= min_fee_a * canonical_tx_size + min_fee_b`, evaluated by
// `static_rules_rejection` (validation-machine-v1.ak) over
// `native_tx_canonical_size_v1`. This selector instantiates it at the C70
// target snapshot (`minFeeA = 44`, `minFeeB = 155_381`) and pins the two
// ADJACENT transactions at the boundary: a fee exactly at the floor advances to
// InputSets, a fee one lovelace below it is an exact `E_MIN_FEE` rejection.
//
// The two negative legs are the attribution: neither disposition is provable
// against the other's transaction, so the min-fee comparison -- not some other
// conjunct of the StaticLedgerRules step -- is what separates them. The two
// transactions differ in exactly one lovelace of `body.fee`.
//
// NO ARBITRARY SUB-CAP: the accepted leg pays exactly `44 * size + 155_381`,
// not one lovelace more, so no Midgard-specific margin is stacked on top of the
// target-network floor.
test parameterized_min_fee_boundary_matches_target_snapshot() {
  let probe_size = target_snapshot_canonical_size(target_snapshot_min_fee_b)
  let boundary_fee = target_snapshot_min_fee_a * probe_size + target_snapshot_min_fee_b
  and {
    target_snapshot_min_fee_a == 44,
    target_snapshot_min_fee_b == 155_381,
    // The fixed point: the fee's CBOR width, and therefore the canonical size,
    // is the same for the probe, the boundary and the adjacent transaction.
    target_snapshot_canonical_size(boundary_fee) == probe_size,
    target_snapshot_canonical_size(boundary_fee - 1) == probe_size,
    target_snapshot_min_fee_step_is_provable(boundary_fee, True),
    target_snapshot_min_fee_step_is_provable(boundary_fee - 1, False),
    !target_snapshot_min_fee_step_is_provable(boundary_fee, False),
    !target_snapshot_min_fee_step_is_provable(boundary_fee - 1, True),
  }
}

// C49 (#540). The canonical parameterized minimum-Ada floor,
// `min_ada_lovelace_v1` (validation-machine-v1.ak), instantiated at the C70
// target snapshot `coinsPerUtxoByte = 4_310` over a concrete serialized ledger
// output. The boundary is pinned adjacently: an output funded at exactly the
// floor satisfies it, one lovelace below does not.
//
// NO ARBITRARY SUB-CAP: the last two conjuncts pin that the floor is exactly
// linear in the serialized size with slope `coins_per_utxo_byte` and intercept
// `coins_per_utxo_byte * 160` -- the target-network entry overhead -- with no
// additional Midgard margin at any size.
test parameterized_min_ada_boundary_matches_target_snapshot() {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 2_000_000, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_bytes = bytearray.length(encode_midgard_tx_output(output))
  let floor = validation_machine_v1.min_ada_lovelace_v1(
    target_snapshot_coins_per_utxo_byte,
    output_bytes,
  )
  and {
    target_snapshot_coins_per_utxo_byte == 4_310,
    validation_machine_v1.min_ada_output_overhead_bytes == 160,
    floor == target_snapshot_coins_per_utxo_byte * ( 160 + output_bytes ),
    validation_machine_v1.output_meets_min_ada_v1(
      target_snapshot_coins_per_utxo_byte,
      output_bytes,
      floor,
    ),
    !validation_machine_v1.output_meets_min_ada_v1(
      target_snapshot_coins_per_utxo_byte,
      output_bytes,
      floor - 1,
    ),
    validation_machine_v1.output_meets_min_ada_v1(
      target_snapshot_coins_per_utxo_byte,
      output_bytes,
      floor + 1,
    ),
    validation_machine_v1.min_ada_lovelace_v1(
      target_snapshot_coins_per_utxo_byte,
      output_bytes + 1,
    ) - floor == target_snapshot_coins_per_utxo_byte,
    validation_machine_v1.min_ada_lovelace_v1(
      target_snapshot_coins_per_utxo_byte,
      0,
    ) == 689_600,
  }
}

// The conservation vector's transaction. `fee = 1` is the TypeScript twin's
// fee, and stage five reads it straight off the authenticated body.
fn conservation_transaction() -> MidgardTransaction {
  let base = empty_v1_transaction()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, fee: 1 },
  }
}

fn conservation_unit() -> ByteArray {
  h_28_a |> bytearray.concat(#"beef")
}

// One ValueAndMint stage-four mint step that folds `mint_quantity` into an
// asset delta that already stands at `pre_asset_delta` (inputs minus outputs
// for that unit). This is where the signed multi-asset arithmetic actually
// happens: `apply_value_asset_mutation` updates the delta trie and maintains
// `nonzero_asset_count`.
fn conservation_mint_step_is_exact(
  mint_quantity: Int,
  pre_asset_delta: Int,
) -> Bool {
  let asset_name = #"beef"
  let tx = conservation_transaction()
  let unit = conservation_unit()
  let mint_frontier = validation_merkle_v1.build_frontier(
    [
      validation_machine_v1.mint_asset_leaf_hash(
        h_28_a,
        asset_name,
        mint_quantity,
      ),
    ],
  )
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = validation_machine_v1.ValueAndMintControlV1 {
    native_control,
    stage: 4,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 0,
    mint_cursor: 0,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 1,
      asset_root: singleton_root(unit, cbor.serialise(pre_asset_delta)),
      seen_asset_count: 1,
      nonzero_asset_count: 1,
    },
  }
  let next_asset_delta = pre_asset_delta + mint_quantity
  let next_control = validation_machine_v1.ValueAndMintControlV1 {
    ..control,
    mint_cursor: 1,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 1,
      asset_root: singleton_root(unit, cbor.serialise(next_asset_delta)),
      seen_asset_count: 1,
      nonzero_asset_count: if next_asset_delta == 0 {
        0
      } else {
        1
      },
    },
  }
  let work_witness_cbor = validation_machine_v1.encode_value_and_mint_control_v1(
    control,
  )
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 60,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      60,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: non_empty_claimed_delta_root(),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 61,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      61,
      validation_machine_v1.encode_value_and_mint_control_v1(next_control),
    ),
  }
  validation_machine_v1.verify_value_and_mint_one_step_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.ValueMintAssetWitness {
        mint_index: 0,
        policy_id: h_28_a,
        asset_name,
        quantity: mint_quantity,
        siblings: [],
        mutation: validation_machine_v1.ValueAssetMutationWitnessV1 {
          delta_was_present: True,
          old_delta: pre_asset_delta,
          delta_proof: [],
        },
      },
    },
  )
}

// The ValueAndMint stage-five final equation on the settled accumulator.
// `claims_advance` selects the claimed successor: the LedgerDelta advance, or
// the exact `E_VALUE_NOT_PRESERVED` terminal.
fn conservation_final_step_is_provable(
  lovelace_delta: Int,
  asset_delta: Int,
  claims_advance: Bool,
) -> Bool {
  let tx = conservation_transaction()
  let unit = conservation_unit()
  let mint_frontier = validation_merkle_v1.build_frontier(
    [validation_machine_v1.mint_asset_leaf_hash(h_28_a, #"beef", 3)],
  )
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = validation_machine_v1.ValueAndMintControlV1 {
    native_control,
    stage: 5,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 0,
    mint_cursor: 1,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta,
      asset_root: singleton_root(unit, cbor.serialise(asset_delta)),
      seen_asset_count: 1,
      nonzero_asset_count: if asset_delta == 0 {
        0
      } else {
        1
      },
    },
  }
  let work_witness_cbor = validation_machine_v1.encode_value_and_mint_control_v1(
    control,
  )
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 62,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      62,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: non_empty_claimed_delta_root(),
  }
  let claimed_successor = if claims_advance {
    validation_trace_v1.ValidationMachineStateV1 {
      ..pre,
      phase: validation_trace_v1.LedgerDelta,
      program_counter: 63,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.LedgerDelta,
        63,
        validation_machine_v1.encode_ledger_delta_witness_v1(
          native_control,
          pre.prior_ledger_root,
          control.replay_schedule_hash,
        ),
      ),
    }
  } else {
    exact_rejection_post(pre, reject_value_not_preserved_code)
  }
  validation_machine_v1.verify_value_and_mint_one_step_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
  )
}

// C49 (#540). Exact signed multi-asset conservation, including the Ada-specific
// rule, on the SAME numeric vector the TypeScript twin pins in
// demo/midgard-validation/tests/value-accounting.test.ts (`mint-aware value
// accounting`, over `valuePreservationDelta`):
//
//   mint vector  inputs {lovelace 5, asset 2}, fee 1, mint +3,
//                outputs {lovelace 4, asset 5}  ->  delta is zero
//   burn vector  inputs {lovelace 5, asset 5}, fee 1, mint -2,
//                outputs {lovelace 4, asset 3}  ->  delta is zero
//
// Aiken accumulates the same equation in two halves. The unit's delta reaches
// the mint step already carrying `inputs - outputs` (-3 for the mint vector,
// +2 for the burn vector), and the mint step folds the signed mint quantity in,
// which must land it on zero and drop `nonzero_asset_count` to zero. Ada is
// carried separately as `lovelace_delta = 5 - 4 = 1`, and stage five closes it
// against the authenticated `body.fee = 1`. Both languages therefore assert
// `inputs + mint - outputs - fee == 0` over the identical numbers; a change to
// either side's arithmetic breaks this selector.
//
// The negative legs are single-number perturbations of the same vector, each
// falsifying exactly one half of the equation: outputs of 5 lovelace instead of
// 4 leaves the Ada rule one lovelace short, and a +2 mint instead of +3 leaves
// the asset delta at -1. Each is an exact `E_VALUE_NOT_PRESERVED` rejection and
// is NOT provable as an advance, and the conserved vector is NOT provable as a
// rejection.
test parameterized_ada_and_multi_asset_conservation_matches_typescript() {
  and {
    // Mint vector: asset delta -3 (2 in, 5 out) plus a +3 mint settles to zero.
    conservation_mint_step_is_exact(3, -3),
    // Burn vector: asset delta +2 (5 in, 3 out) plus a -2 burn settles to zero.
    conservation_mint_step_is_exact(-2, 2),
    // Ada and every asset conserved: the step advances to LedgerDelta.
    conservation_final_step_is_provable(1, 0, True),
    !conservation_final_step_is_provable(1, 0, False),
    // Ada short by one lovelace (outputs 5 instead of 4).
    conservation_final_step_is_provable(0, 0, False),
    !conservation_final_step_is_provable(0, 0, True),
    // One asset unbalanced (a +2 mint instead of +3).
    conservation_final_step_is_provable(1, -1, False),
    !conservation_final_step_is_provable(1, -1, True),
  }
}

// One ValueAndMint stage-four step whose mint witness names
// `witnessed_policy_id`, against a mint frontier that authenticates
// `authenticated_policy_id` at the same asset name and quantity. Everything the
// prover claims -- the successor control, the accumulator mutation, the cursor
// -- is derived from the WITNESSED policy, so the two legs differ in exactly
// one field of the auxiliary witness.
fn mint_authorization_step_is_provable(
  authenticated_policy_id: ByteArray,
  witnessed_policy_id: ByteArray,
  quantity: Int,
) -> Bool {
  let asset_name = #"beef"
  let tx = empty_v1_transaction()
  let mint_frontier = validation_merkle_v1.build_frontier(
    [
      validation_machine_v1.mint_asset_leaf_hash(
        authenticated_policy_id,
        asset_name,
        quantity,
      ),
    ],
  )
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = validation_machine_v1.ValueAndMintControlV1 {
    native_control,
    stage: 4,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 0,
    mint_cursor: 0,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: null_hash,
      seen_asset_count: 0,
      nonzero_asset_count: 0,
    },
  }
  let unit = witnessed_policy_id |> bytearray.concat(asset_name)
  let next_control = validation_machine_v1.ValueAndMintControlV1 {
    ..control,
    mint_cursor: 1,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: singleton_root(unit, cbor.serialise(quantity)),
      seen_asset_count: 1,
      nonzero_asset_count: 1,
    },
  }
  let work_witness_cbor = validation_machine_v1.encode_value_and_mint_control_v1(
    control,
  )
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 64,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      64,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: non_empty_claimed_delta_root(),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 65,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      65,
      validation_machine_v1.encode_value_and_mint_control_v1(next_control),
    ),
  }
  validation_machine_v1.verify_value_and_mint_one_step_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.ValueMintAssetWitness {
        mint_index: 0,
        policy_id: witnessed_policy_id,
        asset_name,
        quantity,
        siblings: [],
        mutation: validation_machine_v1.ValueAssetMutationWitnessV1 {
          delta_was_present: False,
          old_delta: 0,
          delta_proof: [],
        },
      },
    },
  )
}

// C49 (#540). Mint and burn authorization. A ValueAndMint mint step may only
// move an asset delta for a policy the AUTHENTICATED native mint frontier
// commits to: `value_and_mint_stage_four` requires membership of
// `mint_asset_leaf_hash(policy_id, asset_name, quantity)` in
// `native_control.mint_peaks`. An unbacked policy -- one no native script
// authorized, so no leaf exists for it -- can therefore never move value, in
// either direction.
//
// ATTRIBUTION (post-#532). This is not a bare `test ... fail`: each leg is a
// full one-step evaluation and the fixture differs between the passing and
// failing legs in exactly one field, `policy_id` in the auxiliary witness.
// Every other conjunct of the stage-four arm survives that substitution --
// `mint_index == mint_cursor` (0 == 0), `length(policy_id) == 28` (h_28_b is
// also 28 bytes), `length(asset_name) <= 32`, `quantity != 0`, and the
// accumulator mutation, whose claimed successor is derived from the witnessed
// policy so it stays internally exact. The last two conjuncts below pin that
// directly: the authenticated leaf IS a member of the frontier and the unbacked
// one is NOT, which makes the membership check the sole falsifier.
test mint_and_burn_authorization_rejects_unbacked_policy() {
  let backed_mint_frontier = validation_merkle_v1.build_frontier(
    [validation_machine_v1.mint_asset_leaf_hash(h_28_a, #"beef", 3)],
  )
  and {
    // Positive controls: the authorized policy mints and burns.
    mint_authorization_step_is_provable(h_28_a, h_28_a, 3),
    mint_authorization_step_is_provable(h_28_a, h_28_a, -3),
    // The same steps under an unbacked policy are unprovable, mint and burn.
    !mint_authorization_step_is_provable(h_28_a, h_28_b, 3),
    !mint_authorization_step_is_provable(h_28_a, h_28_b, -3),
    // Attribution: the frontier membership conjunct is what moved.
    validation_merkle_v1.verify_membership(
      backed_mint_frontier.count,
      backed_mint_frontier.peaks,
      0,
      validation_machine_v1.mint_asset_leaf_hash(h_28_a, #"beef", 3),
      [],
    ),
    !validation_merkle_v1.verify_membership(
      backed_mint_frontier.count,
      backed_mint_frontier.peaks,
      0,
      validation_machine_v1.mint_asset_leaf_hash(h_28_b, #"beef", 3),
      [],
    ),
  }
}

test value_asset_output_instruction_evidence_is_bounded() {
  let tx = empty_v1_transaction()
  let dense_frontier_four = [
    validation_merkle_v1.FrontierPeak { height: 0, hash: zero },
    validation_merkle_v1.FrontierPeak { height: 1, hash: h_a },
    validation_merkle_v1.FrontierPeak { height: 2, hash: h_b },
    validation_merkle_v1.FrontierPeak { height: 3, hash: h_c },
  ]
  let dense_frontier_six = [
    validation_merkle_v1.FrontierPeak { height: 0, hash: zero },
    validation_merkle_v1.FrontierPeak { height: 1, hash: h_a },
    validation_merkle_v1.FrontierPeak { height: 2, hash: h_b },
    validation_merkle_v1.FrontierPeak { height: 3, hash: h_c },
    validation_merkle_v1.FrontierPeak { height: 4, hash: h_a },
    validation_merkle_v1.FrontierPeak { height: 5, hash: h_b },
  ]
  let dense_frontier_seven = [
    validation_merkle_v1.FrontierPeak { height: 0, hash: zero },
    validation_merkle_v1.FrontierPeak { height: 1, hash: h_a },
    validation_merkle_v1.FrontierPeak { height: 2, hash: h_b },
    validation_merkle_v1.FrontierPeak { height: 3, hash: h_c },
    validation_merkle_v1.FrontierPeak { height: 4, hash: h_a },
    validation_merkle_v1.FrontierPeak { height: 5, hash: h_b },
    validation_merkle_v1.FrontierPeak { height: 6, hash: h_c },
  ]
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    resolved_input_count: 127,
    spend_input_count: 63,
    resolved_item_peaks: dense_frontier_seven,
    signer_count: 63,
    source_count: 15,
    source_peaks: dense_frontier_four,
    redeemer_count: 15,
    redeemer_peaks: dense_frontier_four,
    purpose_count: 15,
    purpose_peaks: dense_frontier_four,
    output_count: 63,
    output_peaks: dense_frontier_six,
    output_descriptor_peaks: dense_frontier_six,
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
    execution_count: 15,
    execution_peaks: dense_frontier_four,
    execution_cursor: 15,
    language_bitmap: 3,
  }
  let control = validation_machine_v1.ValueAndMintControlV1 {
    native_control,
    stage: 3,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 1,
    mint_cursor: 0,
    value_accumulator: validation_machine_v1.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: h_a,
      seen_asset_count: 127,
      nonzero_asset_count: 127,
    },
  }
  let work_witness_cbor = validation_machine_v1.encode_value_and_mint_control_v1(
    control,
  )
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 50,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      50,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_b,
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 51,
    work_root: h_c,
  }
  let maximum_branch = mpf.Branch {
    skip: 0,
    neighbors: bytearray.from_int_big_endian(0, 128),
  }
  let maximum_map_proof = list.repeat(maximum_branch, 16)
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary: validation_machine_v1.ValueOutputAssetWitness {
      output_index: 63,
      descriptor_cbor: bytearray.from_int_big_endian(0, 512),
      asset_index: 127,
      policy_id: h_28_a,
      asset_name: bytearray.from_int_big_endian(0, 32),
      quantity: 9_223_372_036_854_775_807,
      asset_peaks: dense_frontier_seven,
      asset_siblings: list.repeat(zero, 7),
      mutation: validation_machine_v1.ValueAssetMutationWitnessV1 {
        delta_was_present: True,
        old_delta: -9_223_372_036_854_775_808,
        delta_proof: maximum_map_proof,
      },
    },
  }
  let evidence_size = bytearray.length(cbor.serialise(evidence))
  evidence_size < 12_288
}

/// `length` zero bytes.
///
/// `bytearray.from_int_big_endian` caps at 8,192 bytes, which is below §8.3's
/// tier-1 planning bound, so the filler is built in 4,096-byte blocks.
fn zero_filler_bytes(length: Int) -> ByteArray {
  if length <= 4096 {
    bytearray.from_int_big_endian(0, length)
  } else {
    bytearray.from_int_big_endian(0, 4096)
      |> bytearray.concat(zero_filler_bytes(length - 4096))
  }
}

/// The on-wire size of one `CanonicalDecode` chunk instruction, at both ends of
/// §8's carriage ladder.
///
/// The retired counted evidence was bounded by *proof* size — a 14-peak
/// collection frontier, 14 siblings, a chunk frontier and one
/// `bounded_item_v1.chunk_bytes` window — and this row pinned that worst case.
/// §8 replaced all of it with a `FieldCarriageV1`, so the bound now has two
/// shapes and both are pinned:
///
///   * **tier 1** is the only tier whose evidence grows with the field, and
///     §8.3 caps a tier-1 plan at `max_tier1_redeemer_preimage_bytes`;
///   * **tier 3** names one certificate reference input and at most
///     `max_tier3_chunk_count` chunk reference inputs, so it is O(1) in the
///     field's size — which is what makes the measured 16,225-byte field-2
///     maximum reachable by a machine step at all (#593 Ruling 1, option c).
test maximum_general_field_bounded_chunk_instruction_evidence_is_bounded() {
  let Pair(_pre, base_evidence) = canonical_step_fixture()
  let maximum_tier1_preimage = zero_filler_bytes(
    native_tx_field_access_v1.max_tier1_redeemer_preimage_bytes,
  )
  let maximum_tier3_chunk_indices = [1, 2, 3]
  let tier1_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: base_evidence.transition,
    auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
      field_index: 8,
      item_index: 16_382,
      carriage: native_tx_field_access_v1.Inline {
        preimage: maximum_tier1_preimage,
      },
    },
  }
  let tier3_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: base_evidence.transition,
    auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
      field_index: 8,
      item_index: 16_382,
      carriage: native_tx_field_access_v1.Certified {
        cert_ref_input_index: 0,
        chunk_ref_input_indices: maximum_tier3_chunk_indices,
      },
    },
  }
  and {
    bytearray.length(maximum_tier1_preimage) == native_tx_field_access_v1.max_tier1_redeemer_preimage_bytes,
    list.length(maximum_tier3_chunk_indices) == native_tx_field_access_v1.max_tier3_chunk_count,
    bytearray.length(cbor.serialise(tier1_evidence)) < 16_384,
    // The whole point of the ladder: tier-3 evidence does not grow with the
    // field, so it is smaller than the tier-1 form by three orders of magnitude.
    bytearray.length(cbor.serialise(tier3_evidence)) < 1_024,
  }
}

test maximum_script_program_instruction_evidence_is_bounded() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let Pair(base_pre, _base_work_witness_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    validation_machine_v1.empty_script_discovery_control(),
    52,
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let dense_frontier_four = validation_merkle_v1.build_frontier(
    list.repeat(h_a, 15),
  )
  let dense_frontier_six = validation_merkle_v1.build_frontier(
    list.repeat(h_b, 63),
  )
  let dense_frontier_seven = validation_merkle_v1.build_frontier(
    list.repeat(h_c, 127),
  )
  let discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    purpose_cursor: 15,
    source_cursor: 15,
    redeemer_cursor: 15,
    current_purpose_kind: 3,
    current_purpose_index: 15,
    current_script_hash: h_28_a,
    current_subject: bytearray.from_int_big_endian(0, 36),
    matched_source_index: 15,
    matched_language_tag: 128,
    matched_source_leaf: h_a,
    used_inline_bitmap: 65_535,
    used_redeemer_bitmap: 65_535,
    execution_count: 15,
    execution_peaks: dense_frontier_four.peaks,
    redeemer_item_control_hash: #"",
  }
  let control = validation_machine_v1.ScriptSourcesControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 127,
    resolved_inputs_accumulator: h_a,
    resolved_item_peaks: dense_frontier_seven.peaks,
    signer_count: 63,
    signer_frontier_commitment: h_b,
    stage: 8,
    source_count: 15,
    source_peaks: dense_frontier_four.peaks,
    redeemer_count: 15,
    redeemer_peaks: dense_frontier_four.peaks,
    replay_cursor: 127,
    replay_accumulator: h_b,
    replay_remaining_schedule_hash: h_c,
    spend_index: 63,
    purpose_count: 15,
    purpose_peaks: dense_frontier_four.peaks,
    output_cursor: 63,
    output_count: 63,
    output_peaks: dense_frontier_six.peaks,
    output_total_count: 63,
    receive_scan: validation_machine_v1.empty_receive_purpose_scan_control(),
    source_total_count: 15,
    redeemer_total_count: 15,
    observer_scan: validation_machine_v1.empty_observer_purpose_scan_control(),
    discovery,
    output_proof: None,
    pending_source_cbor: #"",
    mint_fold: validation_machine_v1.empty_mint_fold_control(),
    resolution_schedule_hash: h_c,
  }
  let work_witness_cbor = validation_machine_v1.encode_script_sources_discovery_witness(
    control,
    8,
    discovery,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..base_pre,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      base_pre.program_counter,
      work_witness_cbor,
    ),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 53,
    work_root: h_b,
  }
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary: validation_machine_v1.ScriptSourceScanWitness {
      source_index: 15,
      origin_kind: 1,
      source_key: bytearray.from_int_big_endian(0, 36),
      script_language_tag: 3,
      script_hash: h_28_a,
      script_total_length: 50,
      script_item_commitment: h_a,
      siblings: list.repeat(zero, 4),
    },
  }
  and {
    bytearray.length(cbor.serialise(evidence)) < 16_384,
    maximum_program_envelope_selection_is_exact(),
  }
}

fn stage_zero_script_sources_control(
  tx: MidgardTransaction,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  source_total_count: Int,
  pending_source_cbor: ByteArray,
) -> validation_machine_v1.ScriptSourcesControlV1 {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  validation_machine_v1.ScriptSourcesControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    stage: 0,
    source_count: source_frontier.count,
    source_peaks: source_frontier.peaks,
    redeemer_count: 0,
    redeemer_peaks: validation_merkle_v1.empty_frontier(),
    replay_cursor: 0,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    spend_index: 0,
    purpose_count: 0,
    purpose_peaks: validation_merkle_v1.empty_frontier(),
    output_cursor: 0,
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    output_total_count: 0,
    receive_scan: validation_machine_v1.empty_receive_purpose_scan_control(),
    source_total_count,
    redeemer_total_count: 0,
    observer_scan: validation_machine_v1.empty_observer_purpose_scan_control(),
    discovery: validation_machine_v1.empty_script_discovery_control(),
    output_proof: None,
    pending_source_cbor,
    mint_fold: validation_machine_v1.empty_mint_fold_control(),
    resolution_schedule_hash: validation_machine_v1.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

fn stage_zero_script_sources_work_witness(
  control: validation_machine_v1.ScriptSourcesControlV1,
) -> ByteArray {
  if control.pending_source_cbor == #"" {
    validation_machine_v1.encode_script_sources_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolved_input_count,
      control.resolved_inputs_accumulator,
      control.signer_count,
      control.signer_frontier_commitment,
      control.resolved_item_peaks,
      control.stage,
      control.source_count,
      control.source_peaks,
      control.redeemer_count,
      control.redeemer_peaks,
      control.replay_cursor,
      control.replay_accumulator,
      control.replay_remaining_schedule_hash,
      control.spend_index,
      control.purpose_count,
      control.purpose_peaks,
      control.output_cursor,
      control.output_count,
      control.output_peaks,
      control.output_total_count,
      control.receive_scan,
      control.source_total_count,
      control.redeemer_total_count,
      control.observer_scan,
      control.mint_fold,
      control.resolution_schedule_hash,
    )
  } else {
    validation_machine_v1.encode_script_sources_pending_source_witness(
      control,
      control.pending_source_cbor,
    )
  }
}

fn stage_zero_script_sources_state(
  tx: MidgardTransaction,
  control: validation_machine_v1.ScriptSourcesControlV1,
  program_counter: Int,
) -> validation_trace_v1.ValidationMachineStateV1 {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.ScriptSources,
    program_counter,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      program_counter,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
}

test script_sources_stage_zero_begin_initializes_an_authenticated_identity_trace() {
  let base = empty_v1_transaction()
  let payload = v1_program_envelope()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: payload,
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let item_cbor = encode_midgard_versioned_script(script)
  let carriage = inline_field_carriage([item_cbor])
  let pending = validation_machine_v1.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment: bounded_item_v1.from_bytes(6, 0, item_cbor),
    hash_control: blake2b_224_trace_v1.initial_control_v1(
      bytearray.length(payload) + 1,
    ),
  }
  let control = stage_zero_script_sources_control(
    tx,
    validation_merkle_v1.build_frontier([]),
    0,
    #"",
  )
  let successor_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..control,
    source_total_count: 1,
    pending_source_cbor: validation_machine_v1.encode_inline_source_hash_control_v1(
      pending,
    ),
  }
  let pre = stage_zero_script_sources_state(tx, control, 59)
  let post = stage_zero_script_sources_state(tx, successor_control, 60)
  validation_machine_v1.verify_script_sources_stage_zero_begin_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: stage_zero_script_sources_work_witness(control),
      claimed_successor: post,
    },
    inline_field_door(),
    6,
    0,
    carriage,
  )
}

test script_sources_stage_zero_finish_advances_a_complete_source_frontier() {
  let base = empty_v1_transaction()
  let payload = v1_program_envelope()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: payload,
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let item_cbor = encode_midgard_versioned_script(script)
  let item_commitment = bounded_item_v1.from_bytes(6, 0, item_cbor)
  let source_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.inline_source_leaf_hash(
        0,
        3,
        #"03" |> bytearray.concat(payload) |> blake2b_224,
        bytearray.length(item_cbor),
        item_commitment,
      ),
    ],
  )
  let control = stage_zero_script_sources_control(tx, source_frontier, 1, #"")
  let successor_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..control,
    stage: 1,
  }
  let pre = stage_zero_script_sources_state(tx, control, 62)
  let post = stage_zero_script_sources_state(tx, successor_control, 63)
  validation_machine_v1.verify_script_sources_stage_zero_finish_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: stage_zero_script_sources_work_witness(control),
      claimed_successor: post,
    },
  )
}

test script_sources_stage_zero_accepts_an_authenticated_hash_block() {
  let base = empty_v1_transaction()
  let payload = v1_program_envelope()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: payload,
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let item_cbor = encode_midgard_versioned_script(script)
  let chunk_proof = singleton_field_item_chunk_proof(6, item_cbor)
  let hash_control = blake2b_224_trace_v1.initial_control_v1(
    bytearray.length(payload) + 1,
  )
  let pending = validation_machine_v1.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment: bounded_item_v1.from_bytes(6, 0, item_cbor),
    hash_control,
  }
  let pending_cbor = validation_machine_v1.encode_inline_source_hash_control_v1(
    pending,
  )
  let control = stage_zero_script_sources_control(
    tx,
    validation_merkle_v1.build_frontier([]),
    1,
    pending_cbor,
  )
  let block = #"03" |> bytearray.concat(payload)
  expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
    hash_control,
    Some(block),
  )
  let next_pending_cbor = validation_machine_v1.encode_inline_source_hash_control_v1(
    validation_machine_v1.InlineSourceHashControlV1 {
      ..pending,
      hash_control: next_hash_control,
    },
  )
  let successor_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..control,
    pending_source_cbor: next_pending_cbor,
  }
  let pre = stage_zero_script_sources_state(tx, control, 60)
  let post = stage_zero_script_sources_state(tx, successor_control, 61)
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_zero_script_sources_work_witness(control),
    claimed_successor: post,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_zero_semantics_v1(
      pre,
      transition,
      validation_machine_v1.ScriptSourceHashBlockWitness {
        chunk_proof,
        next_chunk_proof: None,
      },
      inline_field_door(),
    ),
    validation_machine_v1.verify_script_sources_stage_zero_hash_block_semantics_v1(
      pre,
      transition,
      chunk_proof,
      None,
    ),
    !validation_machine_v1.verify_script_sources_stage_zero_hash_block_semantics_v1(
      validation_trace_v1.ValidationMachineStateV1 {
        ..pre,
        transaction_commitment: h_b,
      },
      transition,
      chunk_proof,
      None,
    ),
  }
}

test script_sources_stage_zero_advances_an_inline_hash_round() {
  let base = empty_v1_transaction()
  let payload = v1_program_envelope()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: payload,
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let item_cbor = encode_midgard_versioned_script(script)
  let hash_control = blake2b_224_trace_v1.initial_control_v1(
    bytearray.length(payload) + 1,
  )
  expect Some(round_control) = blake2b_224_trace_v1.step_v1(
    hash_control,
    Some(#"03" |> bytearray.concat(payload)),
  )
  expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
    round_control,
    None,
  )
  let pending = validation_machine_v1.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment: bounded_item_v1.from_bytes(6, 0, item_cbor),
    hash_control: round_control,
  }
  let control = stage_zero_script_sources_control(
    tx,
    validation_merkle_v1.build_frontier([]),
    1,
    validation_machine_v1.encode_inline_source_hash_control_v1(pending),
  )
  let successor_control = validation_machine_v1.ScriptSourcesControlV1 {
    ..control,
    pending_source_cbor: validation_machine_v1.encode_inline_source_hash_control_v1(
      validation_machine_v1.InlineSourceHashControlV1 {
        ..pending,
        hash_control: next_hash_control,
      },
    ),
  }
  let pre = stage_zero_script_sources_state(tx, control, 60)
  let post = stage_zero_script_sources_state(tx, successor_control, 61)
  validation_machine_v1.verify_script_sources_stage_zero_hash_advance_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: stage_zero_script_sources_work_witness(control),
      claimed_successor: post,
    },
  )
}

test script_sources_stage_zero_commits_the_terminal_script_identity() {
  let base = empty_v1_transaction()
  let payload = v1_program_envelope()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: payload,
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let item_cbor = encode_midgard_versioned_script(script)
  let item_commitment = bounded_item_v1.from_bytes(6, 0, item_cbor)
  let script_hash = #"03" |> bytearray.concat(payload) |> blake2b_224
  let terminal_hash_control = blake2b_224_trace_v1.Blake2b224TraceControlV1 {
    version: 1,
    stage: blake2b_224_trace_v1.stage_terminal,
    cursor: bytearray.length(payload) + 1,
    total_length: bytearray.length(payload) + 1,
    chaining_value: script_hash
      |> bytearray.concat(builtin.replicate_byte(36, 0)),
    active_block: #"",
    active_block_length: 0,
    working_value: #"",
    round: 0,
  }
  let pending = validation_machine_v1.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment,
    hash_control: terminal_hash_control,
  }
  let control = stage_zero_script_sources_control(
    tx,
    validation_merkle_v1.build_frontier([]),
    1,
    validation_machine_v1.encode_inline_source_hash_control_v1(pending),
  )
  let successor_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.inline_source_leaf_hash(
        0,
        3,
        script_hash,
        bytearray.length(item_cbor),
        item_commitment,
      ),
    ],
  )
  let successor_control = stage_zero_script_sources_control(
    tx,
    successor_frontier,
    1,
    #"",
  )
  let pre = stage_zero_script_sources_state(tx, control, 61)
  let post = stage_zero_script_sources_state(tx, successor_control, 62)
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_zero_script_sources_work_witness(control),
    claimed_successor: post,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_zero_semantics_v1(
      pre,
      transition,
      validation_machine_v1.NoAuxiliaryWitness,
      inline_field_door(),
    ),
    validation_machine_v1.verify_script_sources_stage_zero_hash_terminal_semantics_v1(
      pre,
      transition,
    ),
  }
}

fn crossing_inline_source_hash_fixture() -> Pair<
  validation_machine_v1.InlineSourceHashControlV1,
  Pair<bounded_item_v1.ChunkProofV1, bounded_item_v1.ChunkProofV1>,
> {
  let payload = builtin.replicate_byte(4_200, 171)
  let item_cbor = encode_midgard_versioned_script(
    MidgardVersionedScript { language: PlutusV3Script, script_bytes: payload },
  )
  let first_chunk = builtin.slice_bytearray(
    0,
    bounded_item_v1.chunk_bytes,
    item_cbor,
  )
  let second_chunk = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes,
    bytearray.length(item_cbor) - bounded_item_v1.chunk_bytes,
    item_cbor,
  )
  let first_hash = bounded_item_v1.hash_chunk(6, 0, 0, first_chunk)
  let second_hash = bounded_item_v1.hash_chunk(6, 0, 1, second_chunk)
  let frontier = validation_merkle_v1.build_frontier([first_hash, second_hash])
  let item_commitment = bounded_item_v1.commitment(
    6,
    0,
    bytearray.length(item_cbor),
    frontier.peaks,
  )
  let pending = validation_machine_v1.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment,
    hash_control: blake2b_224_trace_v1.Blake2b224TraceControlV1 {
      version: 1,
      stage: blake2b_224_trace_v1.stage_ready,
      cursor: 3_968,
      total_length: bytearray.length(payload) + 1,
      chaining_value: builtin.replicate_byte(64, 17),
      active_block: #"",
      active_block_length: 0,
      working_value: #"",
      round: 0,
    },
  }
  Pair(
    pending,
    Pair(
      bounded_item_v1.ChunkProofV1 {
        version: 1,
        field_index: 6,
        item_index: 0,
        total_length: bytearray.length(item_cbor),
        chunk_index: 0,
        chunk: first_chunk,
        frontier: frontier.peaks,
        siblings: [second_hash],
      },
      bounded_item_v1.ChunkProofV1 {
        version: 1,
        field_index: 6,
        item_index: 0,
        total_length: bytearray.length(item_cbor),
        chunk_index: 1,
        chunk: second_chunk,
        frontier: frontier.peaks,
        siblings: [first_hash],
      },
    ),
  )
}

test inline_source_hash_block_authenticates_a_chunk_boundary() {
  let Pair(pending, Pair(current, next)) = crossing_inline_source_hash_fixture()
  validation_machine_v1.inline_source_hash_block_v1(
    pending,
    current,
    Some(next),
  ) == Some(builtin.replicate_byte(128, 171))
}

test inline_source_hash_block_fails_closed_when_boundary_data_is_withheld() {
  let Pair(pending, Pair(current, _next)) = crossing_inline_source_hash_fixture()
  validation_machine_v1.inline_source_hash_block_v1(pending, current, None) == None
}

// ===========================================================================
// VM-STAGE-DIFFERENTIAL -- per-stage route agreement controls.
//
// Every ScriptSources stage is implemented twice.
//
//   * once as the narrow sub-case entry point a deployed validator calls,
//     `verify_script_sources_stage_<n>_<case>_semantics_v1` (for example
//     `validators/fraud-proofs/validation-trace/script-sources-stage-eight-finish-semantic-v1.ak`
//     calls `verify_script_sources_stage_eight_finish_semantics_v1`);
//   * once inside the generic dispatcher `script_sources_stage_<n>`, reached
//     through `verify_script_sources_stage_<n>_semantics_v1`,
//     `verify_script_sources_non_output_semantics_v1` and
//     `verify_one_step_evidence`.
//
// Two of the four confirmed production defects on this branch came from that
// single structural cause: the stage-7 deployed successor checks demanded the
// Plutus `serialiseData` successor encoding while the evidence path's
// `script_sources_control_successor_is_exact` demanded the canonical
// definite-length encoding, so the deployed stage-7 path was unsatisfiable for
// every transaction while the evidence path worked.
//
// The controls below pin the two routes together on ONE shared fixture per
// stage, for an accepting transition AND for a mutated transition. Agreement
// on rejection matters as much as agreement on acceptance: a deployed route
// that is *wider* than the canonical rule is a soundness break of the dispute
// game (a step `verify_one_step` rejects can be won on-chain), while a
// deployed route that is narrower is a liveness break. A happy-path-only guard
// would not have caught the mutated-successor half of defect 4.
//
// Each test is deliberately built from the smallest fixture that reaches the
// stage -- this module is under aiken#1389, so every extra fixture costs
// compile minutes.
// ===========================================================================

/// The deployed narrow route and the generic route must return the same
/// verdict, and that verdict must be `expected`. Both arguments are evaluated
/// before the comparison, so neither route can hide behind `and` short
/// circuiting.
fn stage_routes_agree(deployed: Bool, generic: Bool, expected: Bool) -> Bool {
  and {
    deployed == generic,
    deployed == expected,
  }
}

/// `E_MISSING_REQUIRED_WITNESS`.
const differential_missing_witness_code: ByteArray =
  #"455f4d495353494e475f52455155495245445f5749544e455353"

/// `E_INVALID_FIELD_TYPE` -- used only as a wrong-code mutation.
const differential_invalid_field_type_code: ByteArray =
  #"455f494e56414c49445f4649454c445f54595045"

// --- stage 0 --------------------------------------------------------------

test stage_differential_zero_begin_routes_agree() {
  let base = empty_v1_transaction()
  let payload = v1_program_envelope()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: payload,
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let item_cbor = encode_midgard_versioned_script(script)
  let carriage = inline_field_carriage([item_cbor])
  let pending = validation_machine_v1.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment: bounded_item_v1.from_bytes(6, 0, item_cbor),
    hash_control: blake2b_224_trace_v1.initial_control_v1(
      bytearray.length(payload) + 1,
    ),
  }
  let control = stage_zero_script_sources_control(
    tx,
    validation_merkle_v1.build_frontier([]),
    0,
    #"",
  )
  let pre = stage_zero_script_sources_state(tx, control, 59)
  let post = stage_zero_script_sources_state(
    tx,
    validation_machine_v1.ScriptSourcesControlV1 {
      ..control,
      source_total_count: 1,
      pending_source_cbor: validation_machine_v1.encode_inline_source_hash_control_v1(
        pending,
      ),
    },
    60,
  )
  // Mutated successor: the opened scan claims two sources where the
  // door-authenticated field-6 preimage carries one.
  let mutated_post = stage_zero_script_sources_state(
    tx,
    validation_machine_v1.ScriptSourcesControlV1 {
      ..control,
      source_total_count: 2,
      pending_source_cbor: validation_machine_v1.encode_inline_source_hash_control_v1(
        validation_machine_v1.InlineSourceHashControlV1 {
          ..pending,
          source_total_count: 2,
        },
      ),
    },
    60,
  )
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  let accepting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  let auxiliary = validation_machine_v1.TransactionFieldChunkWitness {
    field_index: 6,
    item_index: 0,
    carriage,
  }
  and {
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_zero_begin_semantics_v1(
        pre,
        accepting,
        inline_field_door(),
        6,
        0,
        carriage,
      ),
      validation_machine_v1.verify_script_sources_stage_zero_semantics_v1(
        pre,
        accepting,
        auxiliary,
        inline_field_door(),
      ),
      True,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_zero_begin_semantics_v1(
        pre,
        rejecting,
        inline_field_door(),
        6,
        0,
        carriage,
      ),
      validation_machine_v1.verify_script_sources_stage_zero_semantics_v1(
        pre,
        rejecting,
        auxiliary,
        inline_field_door(),
      ),
      False,
    ),
  }
}

test stage_differential_zero_finish_routes_agree() {
  let base = empty_v1_transaction()
  let payload = v1_program_envelope()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: payload,
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let item_cbor = encode_midgard_versioned_script(script)
  let item_commitment = bounded_item_v1.from_bytes(6, 0, item_cbor)
  let source_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.inline_source_leaf_hash(
        0,
        3,
        #"03" |> bytearray.concat(payload) |> blake2b_224,
        bytearray.length(item_cbor),
        item_commitment,
      ),
    ],
  )
  let control = stage_zero_script_sources_control(tx, source_frontier, 1, #"")
  let pre = stage_zero_script_sources_state(tx, control, 62)
  let post = stage_zero_script_sources_state(
    tx,
    validation_machine_v1.ScriptSourcesControlV1 { ..control, stage: 1 },
    63,
  )
  // Mutated successor: stage 2 instead of the mandated stage 1.
  let mutated_post = stage_zero_script_sources_state(
    tx,
    validation_machine_v1.ScriptSourcesControlV1 { ..control, stage: 2 },
    63,
  )
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  let accepting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  and {
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_zero_finish_semantics_v1(
        pre,
        accepting,
      ),
      validation_machine_v1.verify_script_sources_stage_zero_semantics_v1(
        pre,
        accepting,
        validation_machine_v1.NoAuxiliaryWitness,
        inline_field_door(),
      ),
      True,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_zero_finish_semantics_v1(
        pre,
        rejecting,
      ),
      validation_machine_v1.verify_script_sources_stage_zero_semantics_v1(
        pre,
        rejecting,
        validation_machine_v1.NoAuxiliaryWitness,
        inline_field_door(),
      ),
      False,
    ),
  }
}

// --- stage 7 --------------------------------------------------------------

test stage_differential_seven_observer_routes_agree() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, required_observers: [h_28_a] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let observer_purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(2, 0, h_28_a, h_28_a)],
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    18,
  )
  let observer_scan = validation_machine_v1.ObserverPurposeScanControlV1 {
    total_count: 1,
    seen: 1,
    previous_hash: h_28_a,
  }
  let Pair(post, _) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    empty_frontier,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    observer_purpose_frontier,
    0,
    empty_frontier,
    0,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    observer_scan,
    19,
  )
  // Mutated successor: the observer purpose was never appended to the purpose
  // frontier, but the scan claims it was seen.
  let Pair(mutated_post, _) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    empty_frontier,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    0,
    validation_machine_v1.empty_receive_purpose_scan_control(),
    observer_scan,
    19,
  )
  let carriage = inline_field_carriage([h_28_a])
  let accepting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  let auxiliary = validation_machine_v1.TransactionFieldChunkWitness {
    field_index: 3,
    item_index: 0,
    carriage,
  }
  and {
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_seven_observer_semantics_v1(
        pre,
        accepting,
        inline_field_door(),
        3,
        0,
        carriage,
      ),
      validation_machine_v1.verify_script_sources_stage_seven_semantics_v1(
        pre,
        accepting,
        auxiliary,
        inline_field_door(),
      ),
      True,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_seven_observer_semantics_v1(
        pre,
        rejecting,
        inline_field_door(),
        3,
        0,
        carriage,
      ),
      validation_machine_v1.verify_script_sources_stage_seven_semantics_v1(
        pre,
        rejecting,
        auxiliary,
        inline_field_door(),
      ),
      False,
    ),
  }
}

test stage_differential_seven_receive_routes_agree() {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let material = authenticated_output_material(output_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let receive_source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, h_28_a, h_28_a)],
  )
  let output_descriptor_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_descriptor_leaf_hash(0, material.descriptor_cbor)],
  )
  let empty_receive_scan = validation_machine_v1.ReceivePurposeScanControlV1 {
    source_count: receive_source_frontier.count,
    source_peaks: receive_source_frontier.peaks,
    receive_count: 0,
    previous_hash: #"",
    candidate_hash: #"",
    descriptor_peaks: output_descriptor_frontier.peaks,
  }
  let selected_receive_scan = validation_machine_v1.ReceivePurposeScanControlV1 {
    ..empty_receive_scan,
    candidate_hash: h_28_a,
  }
  let Pair(pre, work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    0,
    output_frontier,
    empty_receive_scan,
    15,
  )
  let Pair(post, _) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    1,
    output_frontier,
    selected_receive_scan,
    16,
  )
  // Mutated successor: the output cursor did not advance.
  let Pair(mutated_post, _) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    0,
    output_frontier,
    selected_receive_scan,
    16,
  )
  let accepting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  let auxiliary = validation_machine_v1.ScriptPurposeScanWitness {
    purpose_kind: 3,
    purpose_index: 0,
    script_hash: h_28_a,
    subject: h_28_a,
    siblings: [],
  }
  and {
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_seven_receive_semantics_v1(
        pre,
        accepting,
        3,
        0,
        h_28_a,
        h_28_a,
        [],
      ),
      validation_machine_v1.verify_script_sources_stage_seven_semantics_v1(
        pre,
        accepting,
        auxiliary,
        inline_field_door(),
      ),
      True,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_seven_receive_semantics_v1(
        pre,
        rejecting,
        3,
        0,
        h_28_a,
        h_28_a,
        [],
      ),
      validation_machine_v1.verify_script_sources_stage_seven_semantics_v1(
        pre,
        rejecting,
        auxiliary,
        inline_field_door(),
      ),
      False,
    ),
  }
}

// --- stage 8 --------------------------------------------------------------

test stage_differential_eight_purpose_routes_agree() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, #"0102")],
  )
  let empty_discovery = validation_machine_v1.empty_script_discovery_control()
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    empty_discovery,
    19,
  )
  let selected = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: h_28_a,
    current_subject: #"0102",
  }
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    selected,
    20,
  )
  // Mutated successor: the selected purpose subject does not match the
  // authenticated purpose leaf.
  let Pair(mutated_post, _) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..selected,
      current_subject: #"0103",
    },
    20,
  )
  let accepting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  let auxiliary = validation_machine_v1.ScriptPurposeScanWitness {
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: h_28_a,
    subject: #"0102",
    siblings: [],
  }
  and {
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_eight_purpose_semantics_v1(
        pre,
        accepting,
        0,
        0,
        h_28_a,
        #"0102",
        [],
      ),
      validation_machine_v1.verify_script_sources_stage_eight_semantics_v1(
        pre,
        accepting,
        auxiliary,
      ),
      True,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_eight_purpose_semantics_v1(
        pre,
        rejecting,
        0,
        0,
        h_28_a,
        #"0102",
        [],
      ),
      validation_machine_v1.verify_script_sources_stage_eight_semantics_v1(
        pre,
        rejecting,
        auxiliary,
      ),
      False,
    ),
  }
}

test stage_differential_eight_finish_routes_agree() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let empty_discovery = validation_machine_v1.empty_script_discovery_control()
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    19,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    11,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    20,
  )
  // Mutated successor: stage 12 instead of the mandated stage 11.
  let Pair(mutated_post, _) = script_discovery_step_fixture(
    tx,
    12,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    20,
  )
  let accepting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  and {
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_eight_finish_semantics_v1(
        pre,
        accepting,
      ),
      validation_machine_v1.verify_script_sources_stage_eight_semantics_v1(
        pre,
        accepting,
        validation_machine_v1.NoAuxiliaryWitness,
      ),
      True,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_eight_finish_semantics_v1(
        pre,
        rejecting,
      ),
      validation_machine_v1.verify_script_sources_stage_eight_semantics_v1(
        pre,
        rejecting,
        validation_machine_v1.NoAuxiliaryWitness,
      ),
      False,
    ),
  }
}

// --- stage 9 --------------------------------------------------------------

test stage_differential_nine_missing_routes_agree() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, h_28_a, h_28_a)],
  )
  let discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    ..validation_machine_v1.empty_script_discovery_control(),
    current_purpose_kind: 3,
    current_purpose_index: 0,
    current_script_hash: h_28_a,
    current_subject: h_28_a,
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    24,
  )
  let accepting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: exact_rejection_post(
      pre,
      differential_missing_witness_code,
    ),
  }
  // Mutated successor: a rejection carrying the wrong code.
  let rejecting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: exact_rejection_post(
      pre,
      differential_invalid_field_type_code,
    ),
  }
  and {
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_nine_missing_semantics_v1(
        pre,
        accepting,
      ),
      validation_machine_v1.verify_script_sources_stage_nine_semantics_v1(
        pre,
        accepting,
        validation_machine_v1.NoAuxiliaryWitness,
      ),
      True,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_nine_missing_semantics_v1(
        pre,
        rejecting,
      ),
      validation_machine_v1.verify_script_sources_stage_nine_semantics_v1(
        pre,
        rejecting,
        validation_machine_v1.NoAuxiliaryWitness,
      ),
      False,
    ),
  }
}

// --- stage 10 -------------------------------------------------------------

/// The stage-10 "missing redeemer" fixture: one redeemer in the witness set,
/// the discovery scan already past it, so `redeemer_cursor == redeemer_count`
/// and the only legal step is the `E_MISSING_REQUIRED_WITNESS` rejection.
fn stage_ten_missing_fixture(
  redeemer_item_control_hash: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let singleton_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 1, h_28_a, #"0102")],
  )
  script_discovery_step_fixture(
    tx,
    10,
    singleton_frontier,
    singleton_frontier,
    purpose_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..validation_machine_v1.empty_script_discovery_control(),
      source_cursor: 1,
      redeemer_cursor: 1,
      current_purpose_kind: 0,
      current_purpose_index: 1,
      current_script_hash: h_28_a,
      current_subject: #"0102",
      matched_source_index: 0,
      matched_language_tag: 3,
      matched_source_leaf: redeemer_leaf,
      redeemer_item_control_hash,
    },
    31,
  )
}

test stage_differential_ten_missing_routes_agree() {
  let Pair(pre, work_witness_cbor) = stage_ten_missing_fixture(#"")
  let accepting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: exact_rejection_post(
      pre,
      differential_missing_witness_code,
    ),
  }
  // Mutated successor: a rejection carrying the wrong code.
  let rejecting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: exact_rejection_post(
      pre,
      differential_invalid_field_type_code,
    ),
  }
  and {
    !validation_machine_v1.verify_script_sources_stage_ten_match_semantics_v1(
      pre,
      accepting,
      validation_machine_v1.NoAuxiliaryWitness,
    ),
    !validation_machine_v1.verify_script_sources_stage_ten_mismatch_semantics_v1(
      pre,
      accepting,
      validation_machine_v1.NoAuxiliaryWitness,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_ten_missing_semantics_v1(
        pre,
        accepting,
      ),
      validation_machine_v1.verify_script_sources_stage_ten_semantics_v1(
        pre,
        accepting,
        validation_machine_v1.NoAuxiliaryWitness,
      ),
      True,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_ten_missing_semantics_v1(
        pre,
        rejecting,
      ),
      validation_machine_v1.verify_script_sources_stage_ten_semantics_v1(
        pre,
        rejecting,
        validation_machine_v1.NoAuxiliaryWitness,
      ),
      False,
    ),
  }
}

// --- stage 11 / 12 --------------------------------------------------------

/// One inline PlutusV3 source that has already been discovered and executed;
/// the stage-11 audit and the stage-12 hand-off both run over this shape.
fn stage_eleven_audit_discovery(
  execution_peaks: List<validation_merkle_v1.FrontierPeak>,
  source_cursor: Int,
  redeemer_item_control_hash: ByteArray,
) -> validation_machine_v1.ScriptDiscoveryControlV1 {
  validation_machine_v1.ScriptDiscoveryControlV1 {
    ..validation_machine_v1.empty_script_discovery_control(),
    purpose_cursor: 1,
    source_cursor,
    used_inline_bitmap: 1,
    execution_count: 1,
    execution_peaks,
    redeemer_item_control_hash,
  }
}

test stage_differential_eleven_finish_routes_agree() {
  let tx = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, script_hash, #"0102")],
  )
  let execution_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let discovery = stage_eleven_audit_discovery(execution_frontier.peaks, 1, #"")
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    11,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    31,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    12,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    32,
  )
  // Mutated successor: the audit never leaves stage 11.
  let Pair(mutated_post, _) = script_discovery_step_fixture(
    tx,
    11,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    32,
  )
  let accepting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  and {
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_eleven_finish_semantics_v1(
        pre,
        accepting,
      ),
      validation_machine_v1.verify_script_sources_stage_eleven_semantics_v1(
        pre,
        accepting,
        validation_machine_v1.NoAuxiliaryWitness,
      ),
      True,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_eleven_finish_semantics_v1(
        pre,
        rejecting,
      ),
      validation_machine_v1.verify_script_sources_stage_eleven_semantics_v1(
        pre,
        rejecting,
        validation_machine_v1.NoAuxiliaryWitness,
      ),
      False,
    ),
  }
}

/// The stage-12 hand-off fixture, parameterised by the dangling redeemer item
/// control hash so the differential probe below can reuse it. Returns the
/// pre-state, its work witness, the canonical NativeScripts successor and a
/// mutated successor that starts one execution late.
fn stage_twelve_finish_fixture(
  redeemer_item_control_hash: ByteArray,
) -> Pair<
  validation_trace_v1.ValidationMachineStateV1,
  Pair<
    ByteArray,
    Pair<
      validation_trace_v1.ValidationMachineStateV1,
      validation_trace_v1.ValidationMachineStateV1,
    >,
  >,
> {
  let tx = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, script_hash, #"0102")],
  )
  let execution_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    12,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    stage_eleven_audit_discovery(
      execution_frontier.peaks,
      1,
      redeemer_item_control_hash,
    ),
    32,
  )
  let Pair(post, _) = native_scripts_step_fixture(
    tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    0,
    0,
    33,
  )
  // Mutated successor: the NativeScripts hand-off starts one execution late.
  let Pair(mutated_post, _) = native_scripts_step_fixture(
    tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    1,
    0,
    33,
  )
  Pair(pre, Pair(work_witness_cbor, Pair(post, mutated_post)))
}

test stage_differential_twelve_finish_routes_agree() {
  let Pair(pre, Pair(work_witness_cbor, Pair(post, mutated_post))) = stage_twelve_finish_fixture(
    #"",
  )
  let accepting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  and {
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_twelve_finish_semantics_v1(
        pre,
        accepting,
      ),
      validation_machine_v1.verify_script_sources_stage_twelve_semantics_v1(
        pre,
        accepting,
        validation_machine_v1.NoAuxiliaryWitness,
      ),
      True,
    ),
    stage_routes_agree(
      validation_machine_v1.verify_script_sources_stage_twelve_finish_semantics_v1(
        pre,
        rejecting,
      ),
      validation_machine_v1.verify_script_sources_stage_twelve_semantics_v1(
        pre,
        rejecting,
        validation_machine_v1.NoAuxiliaryWitness,
      ),
      False,
    ),
  }
}

// ---------------------------------------------------------------------------
// LATENT HAZARD -- dangling `redeemer_item_control_hash`.
//
// These three are NOT route-agreement controls and they are NOT a defect
// report. They are latent-hazard notes: they pin a real asymmetry between the
// two implementations of one rule, on states the machine provably cannot
// produce, so that the asymmetry is visible to whoever next touches the narrow
// stage wrappers.
//
// THE ASYMMETRY.
// The generic route binds a ScriptSources control with
// `script_sources_control_is_bound`, which calls
// `script_discovery_stage_is_well_formed` (validation-machine-v1.ak:11862).
// That predicate forbids a non-empty `discovery.redeemer_item_control_hash` at
// stages 8, 9 and 11 (:11881, :11892, :11922), and `script_sources_stage_ten`
// (:11094) / `script_sources_stage_twelve` (:11644) re-impose the same rule as
// an explicit conjunct in their terminal branches. None of the deployed
// per-stage binders
// (`script_sources_stage_{eight,ten,twelve}_control_is_bound`, :10457, :11267,
// :11788) carries that conjunct, and none of the narrow terminal wrappers
// restates it. So on a state carrying a pending hash the narrow route ACCEPTS
// where the generic route REJECTS -- the narrow route is strictly wider. Each
// test below pins exactly that: `narrow_route_is_wider_than_generic` asserts
// the deployed operand is True AND the generic operand is False, on a fixture
// that differs from its passing clean-hash sibling
// (`stage_differential_{eight_finish,ten_missing,twelve_finish}_routes_agree`)
// in that one field alone.
//
// WHY IT IS NOT EXPLOITABLE TODAY (reachability).
// `discovery.redeemer_item_control_hash` is set to a non-empty value in exactly
// four places -- :11142 and :11235 in `script_sources_stage_ten`, :11693 and
// :11756 in `script_sources_stage_twelve`. All four sit under the branch guard
// `discovery.redeemer_cursor != control.redeemer_count` and all four leave
// `redeemer_cursor` unchanged. It is cleared at :11218 and :11740 (the cursor
// advances past the item) and by `reset_script_discovery_current` (:10367),
// which is the only way stages 8 and 11 are ever entered (:10386, :10527,
// :10682, :11185); stage 7's hand-off into stage 8 and stage 11's hand-off into
// stage 12 (:11422) carry an already-empty hash because
// `script_discovery_stage_is_well_formed` forbids a pending hash at stages 8, 9
// and 11 on the pre-state. The deployed narrow validators that produce
// discovery successors (`script-sources-stage-{eight-purpose,nine-*,eleven-*}-semantic-v1`)
// only propagate `..discovery` and never write the field, and the two
// redeemer-scan validators that do write it
// (`script-sources-stage-ten-mismatch-semantic-v1`,
// `script-sources-stage-twelve-redeemer-semantic-v1`) call the GENERIC
// dispatcher, so they carry the guard. Hence, for every state the machine can
// produce:
//   * stage 8, 9 or 11  =>  `redeemer_item_control_hash == #""`;
//   * stage 10 or 12 with a pending hash  =>  `redeemer_cursor < redeemer_count`.
// Each narrow wrapper below only applies in the opposite regime -- stage-8
// finish is a stage-8 state; stage-10 missing and stage-12 finish both require
// `redeemer_cursor == redeemer_count` -- so no reachable pre-state can reach
// the divergence. There is no reachable-state variant of these probes to build:
// in the reachable pending-hash regime (stage 10/12, cursor < count) the rule is
// implemented only once, by the generic dispatcher, so there is no second
// implementation to differentially test.
//
// THE HAZARD, AND WHAT BREAKS THESE TESTS.
// The divergence becomes live the moment a narrow successor-splice wrapper is
// added for a stage-10/12 redeemer-scan sub-case, or any narrow wrapper is
// allowed to hand off a state that still carries a pending hash. At that point
// the narrow route accepts ScriptSources steps the canonical machine forbids,
// which is an acceptance-widening soundness break of the dispute game. These
// tests are the tripwire: they FAIL if the narrow binders gain the missing
// conjunct (the correct fix -- delete the note then), and they FAIL if the
// generic route loses it. Do not relax them to `deployed == generic`; that
// assertion is false today and asserts nothing about which side moved.
// ---------------------------------------------------------------------------

/// The deployed narrow route accepts and the generic route rejects. Both
/// operands are pinned, so neither side can drift without failing the test.
fn narrow_route_is_wider_than_generic(deployed: Bool, generic: Bool) -> Bool {
  and {
    deployed,
    !generic,
  }
}

/// Latent hazard, stage 8. Unreachable: `reset_script_discovery_current` is the
/// only door into stage 8 and it clears the pending hash, so no produced
/// stage-8 state carries one. See the section note above.
test stage_eight_finish_pending_redeemer_hash_divergence_is_unreachable() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let empty_discovery = validation_machine_v1.empty_script_discovery_control()
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    validation_machine_v1.ScriptDiscoveryControlV1 {
      ..empty_discovery,
      redeemer_item_control_hash: h_a,
    },
    19,
  )
  // `reset_script_discovery_current` clears the pending hash, so the stage-11
  // successor is the same one the clean fixture commits to.
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    11,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    20,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  narrow_route_is_wider_than_generic(
    validation_machine_v1.verify_script_sources_stage_eight_finish_semantics_v1(
      pre,
      transition,
    ),
    validation_machine_v1.verify_script_sources_stage_eight_semantics_v1(
      pre,
      transition,
      validation_machine_v1.NoAuxiliaryWitness,
    ),
  )
}

/// Latent hazard, stage 10. Unreachable: the only writers of the pending hash
/// (validation-machine-v1.ak:11142, :11235) are guarded by
/// `redeemer_cursor != redeemer_count` and leave the cursor alone, while this
/// wrapper's sub-case requires `redeemer_cursor == redeemer_count`. See the
/// section note above.
test stage_ten_missing_pending_redeemer_hash_divergence_is_unreachable() {
  let Pair(pre, work_witness_cbor) = stage_ten_missing_fixture(h_a)
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: exact_rejection_post(
      pre,
      differential_missing_witness_code,
    ),
  }
  narrow_route_is_wider_than_generic(
    validation_machine_v1.verify_script_sources_stage_ten_missing_semantics_v1(
      pre,
      transition,
    ),
    validation_machine_v1.verify_script_sources_stage_ten_semantics_v1(
      pre,
      transition,
      validation_machine_v1.NoAuxiliaryWitness,
    ),
  )
}

/// Latent hazard, stage 12. Unreachable for the same reason as stage 10
/// (writers at validation-machine-v1.ak:11693, :11756), plus stage 11 -- the
/// only door into stage 12 -- may not carry a pending hash at all.
/// See the section note above.
test stage_twelve_finish_pending_redeemer_hash_divergence_is_unreachable() {
  let Pair(pre, Pair(work_witness_cbor, Pair(post, _))) = stage_twelve_finish_fixture(
    h_a,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  narrow_route_is_wider_than_generic(
    validation_machine_v1.verify_script_sources_stage_twelve_finish_semantics_v1(
      pre,
      transition,
    ),
    validation_machine_v1.verify_script_sources_stage_twelve_semantics_v1(
      pre,
      transition,
      validation_machine_v1.NoAuxiliaryWitness,
    ),
  )
}
