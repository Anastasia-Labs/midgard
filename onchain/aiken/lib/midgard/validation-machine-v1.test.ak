use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_224, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, suffix}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/blake2b_224_trace_v1
use midgard/cek_constant_v1
use midgard/cek_data_scan_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{encode_native_byte_list}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_witness_set_compact, encode_native_tx_body_compact,
  native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_redeemer_witness,
  encode_midgard_tx_input, encode_midgard_tx_output,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{
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
  MidgardVersionedScript, NativeCardanoScript, PlutusV3Script, PubKeyCredential,
  RewardRedeemer, ScriptCredential, SpendRedeemer, TxIsValid,
  native_tx_version_v1,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_v1
use midgard/mpf_proof_fold_v1
use midgard/native_script_scan_v1
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const h_28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_28_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const maximum_mint_preimage_cbor =
  #"a1581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab88058200000000000000000000000000000000000000000000000000000000000000000015820000000000000000000000000000000000000000000000000000000000000000101582000000000000000000000000000000000000000000000000000000000000000020158200000000000000000000000000000000000000000000000000000000000000003015820000000000000000000000000000000000000000000000000000000000000000401582000000000000000000000000000000000000000000000000000000000000000050158200000000000000000000000000000000000000000000000000000000000000006015820000000000000000000000000000000000000000000000000000000000000000701582000000000000000000000000000000000000000000000000000000000000000080158200000000000000000000000000000000000000000000000000000000000000009015820000000000000000000000000000000000000000000000000000000000000000a015820000000000000000000000000000000000000000000000000000000000000000b015820000000000000000000000000000000000000000000000000000000000000000c015820000000000000000000000000000000000000000000000000000000000000000d015820000000000000000000000000000000000000000000000000000000000000000e015820000000000000000000000000000000000000000000000000000000000000000f0158200000000000000000000000000000000000000000000000000000000000000010015820000000000000000000000000000000000000000000000000000000000000001101582000000000000000000000000000000000000000000000000000000000000000120158200000000000000000000000000000000000000000000000000000000000000013015820000000000000000000000000000000000000000000000000000000000000001401582000000000000000000000000000000000000000000000000000000000000000150158200000000000000000000000000000000000000000000000000000000000000016015820000000000000000000000000000000000000000000000000000000000000001701582000000000000000000000000000000000000000000000000000000000000000180158200000000000000000000000000000000000000000000000000000000000000019015820000000000000000000000000000000000000000000000000000000000000001a015820000000000000000000000000000000000000000000000000000000000000001b015820000000000000000000000000000000000000000000000000000000000000001c015820000000000000000000000000000000000000000000000000000000000000001d015820000000000000000000000000000000000000000000000000000000000000001e015820000000000000000000000000000000000000000000000000000000000000001f0158200000000000000000000000000000000000000000000000000000000000000020015820000000000000000000000000000000000000000000000000000000000000002101582000000000000000000000000000000000000000000000000000000000000000220158200000000000000000000000000000000000000000000000000000000000000023015820000000000000000000000000000000000000000000000000000000000000002401582000000000000000000000000000000000000000000000000000000000000000250158200000000000000000000000000000000000000000000000000000000000000026015820000000000000000000000000000000000000000000000000000000000000002701582000000000000000000000000000000000000000000000000000000000000000280158200000000000000000000000000000000000000000000000000000000000000029015820000000000000000000000000000000000000000000000000000000000000002a015820000000000000000000000000000000000000000000000000000000000000002b015820000000000000000000000000000000000000000000000000000000000000002c015820000000000000000000000000000000000000000000000000000000000000002d015820000000000000000000000000000000000000000000000000000000000000002e015820000000000000000000000000000000000000000000000000000000000000002f0158200000000000000000000000000000000000000000000000000000000000000030015820000000000000000000000000000000000000000000000000000000000000003101582000000000000000000000000000000000000000000000000000000000000000320158200000000000000000000000000000000000000000000000000000000000000033015820000000000000000000000000000000000000000000000000000000000000003401582000000000000000000000000000000000000000000000000000000000000000350158200000000000000000000000000000000000000000000000000000000000000036015820000000000000000000000000000000000000000000000000000000000000003701582000000000000000000000000000000000000000000000000000000000000000380158200000000000000000000000000000000000000000000000000000000000000039015820000000000000000000000000000000000000000000000000000000000000003a015820000000000000000000000000000000000000000000000000000000000000003b015820000000000000000000000000000000000000000000000000000000000000003c015820000000000000000000000000000000000000000000000000000000000000003d015820000000000000000000000000000000000000000000000000000000000000003e015820000000000000000000000000000000000000000000000000000000000000003f0158200000000000000000000000000000000000000000000000000000000000000040015820000000000000000000000000000000000000000000000000000000000000004101582000000000000000000000000000000000000000000000000000000000000000420158200000000000000000000000000000000000000000000000000000000000000043015820000000000000000000000000000000000000000000000000000000000000004401582000000000000000000000000000000000000000000000000000000000000000450158200000000000000000000000000000000000000000000000000000000000000046015820000000000000000000000000000000000000000000000000000000000000004701582000000000000000000000000000000000000000000000000000000000000000480158200000000000000000000000000000000000000000000000000000000000000049015820000000000000000000000000000000000000000000000000000000000000004a015820000000000000000000000000000000000000000000000000000000000000004b015820000000000000000000000000000000000000000000000000000000000000004c015820000000000000000000000000000000000000000000000000000000000000004d015820000000000000000000000000000000000000000000000000000000000000004e015820000000000000000000000000000000000000000000000000000000000000004f0158200000000000000000000000000000000000000000000000000000000000000050015820000000000000000000000000000000000000000000000000000000000000005101582000000000000000000000000000000000000000000000000000000000000000520158200000000000000000000000000000000000000000000000000000000000000053015820000000000000000000000000000000000000000000000000000000000000005401582000000000000000000000000000000000000000000000000000000000000000550158200000000000000000000000000000000000000000000000000000000000000056015820000000000000000000000000000000000000000000000000000000000000005701582000000000000000000000000000000000000000000000000000000000000000580158200000000000000000000000000000000000000000000000000000000000000059015820000000000000000000000000000000000000000000000000000000000000005a015820000000000000000000000000000000000000000000000000000000000000005b015820000000000000000000000000000000000000000000000000000000000000005c015820000000000000000000000000000000000000000000000000000000000000005d015820000000000000000000000000000000000000000000000000000000000000005e015820000000000000000000000000000000000000000000000000000000000000005f0158200000000000000000000000000000000000000000000000000000000000000060015820000000000000000000000000000000000000000000000000000000000000006101582000000000000000000000000000000000000000000000000000000000000000620158200000000000000000000000000000000000000000000000000000000000000063015820000000000000000000000000000000000000000000000000000000000000006401582000000000000000000000000000000000000000000000000000000000000000650158200000000000000000000000000000000000000000000000000000000000000066015820000000000000000000000000000000000000000000000000000000000000006701582000000000000000000000000000000000000000000000000000000000000000680158200000000000000000000000000000000000000000000000000000000000000069015820000000000000000000000000000000000000000000000000000000000000006a015820000000000000000000000000000000000000000000000000000000000000006b015820000000000000000000000000000000000000000000000000000000000000006c015820000000000000000000000000000000000000000000000000000000000000006d015820000000000000000000000000000000000000000000000000000000000000006e015820000000000000000000000000000000000000000000000000000000000000006f0158200000000000000000000000000000000000000000000000000000000000000070015820000000000000000000000000000000000000000000000000000000000000007101582000000000000000000000000000000000000000000000000000000000000000720158200000000000000000000000000000000000000000000000000000000000000073015820000000000000000000000000000000000000000000000000000000000000007401582000000000000000000000000000000000000000000000000000000000000000750158200000000000000000000000000000000000000000000000000000000000000076015820000000000000000000000000000000000000000000000000000000000000007701582000000000000000000000000000000000000000000000000000000000000000780158200000000000000000000000000000000000000000000000000000000000000079015820000000000000000000000000000000000000000000000000000000000000007a015820000000000000000000000000000000000000000000000000000000000000007b015820000000000000000000000000000000000000000000000000000000000000007c015820000000000000000000000000000000000000000000000000000000000000007d015820000000000000000000000000000000000000000000000000000000000000007e015820000000000000000000000000000000000000000000000000000000000000007f01"

const validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

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

fn singleton_field_item_proofs(
  field_index: Int,
  key: ByteArray,
) -> Pair<bounded_collection_v1.ItemProofV1, bounded_item_v1.ChunkProofV1> {
  let item_commitment = bounded_item_v1.from_bytes(field_index, 0, key)
  let chunk_frontier = [
    validation_merkle_v1.FrontierPeak {
      height: 0,
      hash: bounded_item_v1.hash_chunk(field_index, 0, 0, key),
    },
  ]
  let collection_frontier = [
    validation_merkle_v1.FrontierPeak {
      height: 0,
      hash: bounded_collection_v1.hash_item(
        field_index,
        0,
        bytearray.length(key),
        item_commitment,
      ),
    },
  ]
  Pair(
    bounded_collection_v1.ItemProofV1 {
      version: bounded_collection_v1.version,
      field_index,
      item_count: 1,
      item_index: 0,
      item_length: bytearray.length(key),
      item_commitment,
      frontier: collection_frontier,
      siblings: [],
    },
    bounded_item_v1.ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index,
      item_index: 0,
      total_length: bytearray.length(key),
      chunk_index: 0,
      chunk: key,
      frontier: chunk_frontier,
      siblings: [],
    },
  )
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
    ledger_delta_root: validation_merkle_v1.frontier_commitment(0, []),
  }
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

fn duplicate_observer_field_item_proofs(
  item_index: Int,
) -> Pair<bounded_collection_v1.ItemProofV1, bounded_item_v1.ChunkProofV1> {
  expect item_index == 0 || item_index == 1
  let first_item_commitment = bounded_item_v1.from_bytes(3, 0, h_28_a)
  let second_item_commitment = bounded_item_v1.from_bytes(3, 1, h_28_a)
  let first_leaf = bounded_collection_v1.hash_item(
    3,
    0,
    28,
    first_item_commitment,
  )
  let second_leaf = bounded_collection_v1.hash_item(
    3,
    1,
    28,
    second_item_commitment,
  )
  let collection_frontier = validation_merkle_v1.build_frontier(
    [first_leaf, second_leaf],
  )
  let item_commitment = if item_index == 0 {
    first_item_commitment
  } else {
    second_item_commitment
  }
  let collection_sibling = if item_index == 0 {
    second_leaf
  } else {
    first_leaf
  }
  let chunk_frontier = validation_merkle_v1.build_frontier(
    [bounded_item_v1.hash_chunk(3, item_index, 0, h_28_a)],
  )
  Pair(
    bounded_collection_v1.ItemProofV1 {
      version: bounded_collection_v1.version,
      field_index: 3,
      item_count: 2,
      item_index,
      item_length: 28,
      item_commitment,
      frontier: collection_frontier.peaks,
      siblings: [collection_sibling],
    },
    bounded_item_v1.ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index: 3,
      item_index,
      total_length: 28,
      chunk_index: 0,
      chunk: h_28_a,
      frontier: chunk_frontier.peaks,
      siblings: [],
    },
  )
}

fn observer_hash_at(item_index: Int) -> ByteArray {
  bytearray.from_int_big_endian(item_index + 1, 28)
}

fn observer_hashes_from(
  item_index: Int,
  item_count: Int,
) -> List<ByteArray> {
  if item_index == item_count {
    []
  } else {
    [observer_hash_at(item_index), ..observer_hashes_from(item_index + 1, item_count)]
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
      bounded_collection_v1.hash_item(
        3,
        item_index,
        28,
        item_commitment,
      ),
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

fn last_observer_siblings_16() -> List<ByteArray> {
  [
    observer_collection_subtree_root(14, 1),
    observer_collection_subtree_root(12, 2),
    observer_collection_subtree_root(8, 4),
    observer_collection_subtree_root(0, 8),
  ]
}

fn observer_field_item_proofs(
  item_count: Int,
  item_index: Int,
  siblings: List<ByteArray>,
) -> Pair<bounded_collection_v1.ItemProofV1, bounded_item_v1.ChunkProofV1> {
  let observer_hash = observer_hash_at(item_index)
  let item_commitment = bounded_item_v1.from_bytes(
    3,
    item_index,
    observer_hash,
  )
  let collection_frontier = validation_merkle_v1.build_frontier(
    observer_collection_leaves_from(0, item_count),
  )
  let chunk_frontier = validation_merkle_v1.build_frontier(
    [bounded_item_v1.hash_chunk(3, item_index, 0, observer_hash)],
  )
  Pair(
    bounded_collection_v1.ItemProofV1 {
      version: bounded_collection_v1.version,
      field_index: 3,
      item_count,
      item_index,
      item_length: 28,
      item_commitment,
      frontier: collection_frontier.peaks,
      siblings,
    },
    bounded_item_v1.ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index: 3,
      item_index,
      total_length: 28,
      chunk_index: 0,
      chunk: observer_hash,
      frontier: chunk_frontier.peaks,
      siblings: [],
    },
  )
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
      ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
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
    validation_machine_v1.empty_mint_fold_control(),
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
  let Pair(_, chunk_proof) = singleton_field_item_proofs(2, output_cbor)
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
    mint_fold: validation_machine_v1.empty_mint_fold_control(),
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
  let work_witness_cbor = validation_machine_v1.encode_cek_witness_v1(
    control,
    context_control_cbor,
    execution_cursor,
    0,
    0,
    #"",
    0,
    0,
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

fn observer_items_summary_fixture(
  observers: List<ByteArray>,
  midgard_encoding: Bool,
) -> cek_data_v1.DataSequenceSummaryV1 {
  when observers is {
    [] ->
      if midgard_encoding {
        cek_data_v1.empty_data_list_summary_v1()
      } else {
        cek_data_v1.empty_data_pair_summary_v1()
      }
    [observer, ..rest] ->
      validation_machine_v1.prepend_cek_observer_item_v1(
        observer,
        midgard_encoding,
        observer_items_summary_fixture(rest, midgard_encoding),
      )
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
      maximum_mint_subtree_root(
        128 - 2 * sibling_size,
        sibling_size,
      ),
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
  let work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    context_cbor,
    0,
    0,
    0,
    0,
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
  validation_machine_v1.verify_canonical_decode_one_step_v1(pre, evidence)
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
  let chunk_hash = bounded_item_v1.hash_chunk(7, 0, 0, item_cbor)
  let chunk_frontier = [
    validation_merkle_v1.FrontierPeak { height: 0, hash: chunk_hash },
  ]
  let item_commitment = bounded_item_v1.commitment(
    7,
    0,
    bytearray.length(item_cbor),
    chunk_frontier,
  )
  let item_hash = bounded_collection_v1.hash_item(
    7,
    0,
    bytearray.length(item_cbor),
    item_commitment,
  )
  let collection_frontier = [
    validation_merkle_v1.FrontierPeak { height: 0, hash: item_hash },
  ]
  let work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    7,
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
        8,
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
      auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof: bounded_collection_v1.ItemProofV1 {
          version: bounded_collection_v1.version,
          field_index: 7,
          item_count: 1,
          item_index: 0,
          item_length: bytearray.length(item_cbor),
          item_commitment,
          frontier: collection_frontier,
          siblings: [],
        },
        chunk_proof: bounded_item_v1.ChunkProofV1 {
          version: bounded_item_v1.version,
          field_index: 7,
          item_index: 0,
          total_length: bytearray.length(item_cbor),
          chunk_index: 0,
          chunk: item_cbor,
          frontier: chunk_frontier,
          siblings: [],
        },
      },
    },
  )
}

test canonical_decode_rejects_a_wrong_claimed_successor() fail {
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
  let chunk_hash = bounded_item_v1.hash_chunk(7, 0, 0, item_cbor)
  let chunk_frontier = [
    validation_merkle_v1.FrontierPeak { height: 0, hash: chunk_hash },
  ]
  let item_commitment = bounded_item_v1.commitment(
    7,
    0,
    bytearray.length(item_cbor),
    chunk_frontier,
  )
  let item_hash = bounded_collection_v1.hash_item(
    7,
    0,
    bytearray.length(item_cbor),
    item_commitment,
  )
  let collection_frontier = [
    validation_merkle_v1.FrontierPeak { height: 0, hash: item_hash },
  ]
  let work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    7,
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
  validation_machine_v1.verify_canonical_decode_one_step_v1(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: terminal,
      },
      auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof: bounded_collection_v1.ItemProofV1 {
          version: bounded_collection_v1.version,
          field_index: 7,
          item_count: 1,
          item_index: 0,
          item_length: bytearray.length(item_cbor),
          item_commitment,
          frontier: collection_frontier,
          siblings: [],
        },
        chunk_proof: bounded_item_v1.ChunkProofV1 {
          version: bounded_item_v1.version,
          field_index: 7,
          item_index: 0,
          total_length: bytearray.length(item_cbor),
          chunk_index: 0,
          chunk: item_cbor,
          frontier: chunk_frontier,
          siblings: [],
        },
      },
    },
  )
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
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 3,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      3,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  validation_machine_v1.verify_one_step(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: terminal,
    },
  )
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
  let Pair(reference_collection_proof, reference_chunk_proof) = singleton_field_item_proofs(
    1,
    reference_key,
  )
  let first_is_valid = validation_machine_v1.verify_input_sets_item_semantics_v1(
    first_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: first_witness_cbor,
      claimed_successor: second_pre,
    },
    reference_collection_proof,
    reference_chunk_proof,
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
  let Pair(spend_collection_proof, spend_chunk_proof) = singleton_field_item_proofs(
    0,
    spend_key,
  )
  and {
    first_is_valid,
    validation_machine_v1.verify_input_sets_item_semantics_v1(
      second_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: second_witness_cbor,
        claimed_successor: signatures_post,
      },
      spend_collection_proof,
      spend_chunk_proof,
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
  let Pair(reference_collection_proof, reference_chunk_proof) = singleton_field_item_proofs(
    1,
    key,
  )
  let Pair(spend_collection_proof, spend_chunk_proof) = singleton_field_item_proofs(
    0,
    key,
  )
  let first_is_valid = validation_machine_v1.verify_input_sets_item_semantics_v1(
    first_pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: first_witness_cbor,
      claimed_successor: second_pre,
    },
    reference_collection_proof,
    reference_chunk_proof,
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
      spend_collection_proof,
      spend_chunk_proof,
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(0, key)
  let rejection_code = #"455f494e56414c49445f56414c49444954595f494e54455256414c5f464f524d4154"
  validation_machine_v1.verify_input_sets_item_semantics_v1(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(pre, rejection_code),
    },
    collection_proof,
    chunk_proof,
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    4,
    h_28_a,
  )
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
      collection_proof,
      chunk_proof,
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    6,
    address_witness_cbor,
  )
  let rejection_code = #"455f494e56414c49445f5349474e4154555245"
  and {
    validation_machine_v1.verify_signature_address_item_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: required_pre,
      },
      collection_proof,
      chunk_proof,
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    7,
    item_cbor,
  )
  let token_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    collection_proof.item_commitment,
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
    collection_proof.item_commitment,
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
    collection_proof.item_commitment,
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
      collection_proof,
      chunk_proof,
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    7,
    item_cbor,
  )
  let payload_offset = bytearray.length(item_cbor) - bytearray.length(
    script_bytes,
  )
  let child_offset = payload_offset + 3
  let item_length = bytearray.length(item_cbor)
  let item_commitment = collection_proof.item_commitment
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    7,
    item_cbor,
  )
  let token_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    collection_proof.item_commitment,
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
    collection_proof.item_commitment,
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
    collection_proof.item_commitment,
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
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..finalize_pre,
    phase: validation_trace_v1.Terminal,
    program_counter: finalize_pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      finalize_pre.program_counter + 1,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        finalize_pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
    ledger_delta_root: validation_merkle_v1.frontier_commitment(0, []),
  }
  and {
    validation_machine_v1.verify_phase_a_native_item_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: token_pre,
      },
      collection_proof,
      chunk_proof,
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
  let Pair(first_collection_proof, first_chunk_proof) = duplicate_observer_field_item_proofs(
    0,
  )
  let Pair(second_collection_proof, second_chunk_proof) = duplicate_observer_field_item_proofs(
    1,
  )
  let first_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: next,
    },
    auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
      collection_proof: first_collection_proof,
      chunk_proof: first_chunk_proof,
    },
  }
  let second_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor: next_work_witness_cbor,
      claimed_successor: terminal,
    },
    auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
      collection_proof: second_collection_proof,
      chunk_proof: second_chunk_proof,
    },
  }
  and {
    validation_machine_v1.verify_one_step_evidence(pre, first_evidence),
    validation_machine_v1.verify_one_step_evidence(next, second_evidence),
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
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let rejection_code = #"455f494e56414c49445f4649454c445f54595045"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      7,
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
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 9,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      9,
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
      auxiliary: validation_machine_v1.ScheduledLedgerNonMembershipWitness {
        source_kind: 0,
        key,
        next_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
        proof: [],
      },
    },
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
  )
}

test script_sources_commits_inline_reference_script_candidates() {
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
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.source_leaf_hash(0, #"00", script)],
  )
  let item_cbor = encode_midgard_versioned_script(script)
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    7,
    item_cbor,
  )
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    0,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    9,
  )
  let Pair(item_post, item_work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    0,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    10,
  )
  let Pair(final_post, _) = script_sources_step_fixture(
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
    11,
  )
  and {
    validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor,
          claimed_successor: item_post,
        },
        auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
          collection_proof,
          chunk_proof,
        },
      },
    ),
    validation_machine_v1.verify_one_step_evidence(
      item_post,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: item_work_witness_cbor,
          claimed_successor: final_post,
        },
        auxiliary: validation_machine_v1.NoAuxiliaryWitness,
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
  let Pair(collection_proof, _) = singleton_field_item_proofs(8, redeemer_cbor)
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
  let item_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: item_post,
  }
  let final_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: item_work_witness_cbor,
    claimed_successor: final_post,
  }
  and {
    validation_machine_v1.verify_script_sources_stage_one_redeemer_raw_semantics_v1(
      pre,
      item_transition,
      collection_proof,
      redeemer,
    ),
    validation_machine_v1.verify_script_sources_stage_one_finish_raw_semantics_v1(
      item_post,
      final_transition,
    ),
    validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: item_transition,
        auxiliary: validation_machine_v1.TransactionRedeemerItemWitness {
          collection_proof,
          redeemer,
        },
      },
    ),
    validation_machine_v1.verify_one_step_evidence(
      item_post,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: final_transition,
        auxiliary: validation_machine_v1.NoAuxiliaryWitness,
      },
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
    original_schedule_hash: #"3175ee405a7dab445d883a4e4912fa79c57399fbafd5fe8865e50ea2a4f900cc",
    terminal_accumulator: #"10d18e38469596142ba1fa63fd7c2bbdd310770d811bda870a32ba11fa0c2949",
    terminal_frontier_commitment: #"7d01f4857f245e957b13a07cdb54a66af980f29825235c7945fde902c2a5c8ee",
    penultimate_accumulator: #"e66484cfb1266c895a9dc2765e44542ecc0e63e8ef9c08173fb6c6dd91f954d5",
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
        hash: #"a51dc8d5735d7e5ab600dfff6020801e6ae064e76b0cf8f7a880d05c7bcb91de",
      },
    ],
    last_source_kind: 0,
    last_key: maximum_input_schedule_last_key,
    last_value: maximum_input_schedule_last_value,
  }
}

fn maximum_reference_input_schedule_terminal_vector() -> ResolutionScheduleTerminalVectorV1 {
  ResolutionScheduleTerminalVectorV1 {
    original_schedule_hash: #"95872510ce0c8c50c31d60b997049c0f7741916d7378a6cd264f7f6e60f26c75",
    terminal_accumulator: #"a8e7978a7b04c06c22e93f148e20c4f05d1e52e342086d99dce07ebcf22162cd",
    terminal_frontier_commitment: #"b3de0271b55374bf3aba6fed0bc45c7acbf89cf6cb94f28255f9642820e98e5c",
    penultimate_accumulator: #"a626f4411517e88a5d3cbd4f557eaa3016c097ee9faceb9f9778a122c3aa2085",
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
        hash: #"ef9e52876a7b2b2de2f7864af0bd5d3bcb8992826ea07065fb586085e8ac8f28",
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
  let value = encode_midgard_tx_output(output)
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
  let value = encode_midgard_tx_output(output)
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
  )
}

fn canonical_output_commitment_step(step_to_verify: Int) -> Bool {
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
  let Pair(after_item, after_item_work_witness_cbor) = script_sources_output_step_fixture(
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
    0,
    output_frontier,
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
  let Pair(collection_proof, _) = singleton_field_item_proofs(2, output_cbor)
  if step_to_verify == 0 {
    validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor,
          claimed_successor: after_item,
        },
        auxiliary: validation_machine_v1.TransactionFieldItemWitness {
          collection_proof,
        },
      },
    )
  } else {
    and {
      step_to_verify == 1,
      validation_machine_v1.verify_one_step_evidence(
        after_item,
        validation_machine_v1.ValidationOneStepEvidenceV1 {
          transition: validation_machine_v1.ValidationOneStepWitnessV1 {
            work_witness_cbor: after_item_work_witness_cbor,
            claimed_successor: post,
          },
          auxiliary: validation_machine_v1.NoAuxiliaryWitness,
        },
      ),
    }
  }
}

test script_sources_commits_an_authenticated_output_item() {
  canonical_output_commitment_step(0)
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
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 16,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      16,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
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

test script_sources_streams_mint_and_burn_into_authenticated_frontiers() {
  let mint_cbor = #"a1581c"
    |> bytearray.concat(h_28_a)
    |> bytearray.concat(#"a1410124")
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    5,
    policy_item_cbor,
  )
  let initial_fold = validation_machine_v1.empty_mint_fold_control()
  let active_fold = validation_machine_v1.MintFoldControlV1 {
    policy_count: 1,
    policy_cursor: 0,
    previous_policy: #"",
    active_policy: h_28_a,
    item_length: bytearray.length(policy_item_cbor),
    item_commitment: collection_proof.item_commitment,
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
      collection_proof,
      chunk_proof,
    },
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
  )
  and { begin_step, asset_step, finish_step }
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
  let mint = decode_midgard_tx_mint_preimage_cbor(maximum_mint_preimage_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  let item_cbor = #"82"
    |> bytearray.concat(
        builtin.slice_bytearray(
          1,
          bytearray.length(maximum_mint_preimage_cbor) - 1,
          maximum_mint_preimage_cbor,
        ),
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    3,
    h_28_a,
  )
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
        collection_proof,
        chunk_proof,
      ),
      validation_machine_v1.verify_one_step_evidence(
        pre,
        validation_machine_v1.ValidationOneStepEvidenceV1 {
          transition: observer_transition,
          auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
            collection_proof,
            chunk_proof,
          },
        },
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
  let script_item_commitment = bounded_item_v1.from_bytes(7, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
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
  let Pair(stage_eight_done, _) = script_discovery_step_fixture(
    tx,
    8,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    completed,
    23,
  )
  let stage_ten_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_ten_cbor,
    claimed_successor: stage_eight_done,
  }
  and {
    validation_machine_v1.verify_one_step_evidence(
      stage_eight,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: stage_eight_cbor,
          claimed_successor: stage_nine,
        },
        auxiliary: validation_machine_v1.ScriptPurposeScanWitness {
          purpose_kind: 0,
          purpose_index: 0,
          script_hash,
          subject: #"0102",
          siblings: [],
        },
      },
    ),
    validation_machine_v1.verify_one_step_evidence(
      stage_nine,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: stage_nine_cbor,
          claimed_successor: stage_ten,
        },
        auxiliary: validation_machine_v1.ScriptSourceScanWitness {
          source_index: 0,
          origin_kind: 0,
          source_key: #"00",
          script_language_tag: 3,
          script_hash,
          script_total_length: bytearray.length(script_cbor),
          script_item_commitment,
          siblings: [],
        },
      },
    ),
    validation_machine_v1.verify_one_step_evidence(
      stage_ten,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: stage_ten_transition,
        auxiliary: validation_machine_v1.RedeemerScanWitness {
          redeemer_index: 0,
          redeemer,
          siblings: [],
        },
      },
    ),
    validation_machine_v1.verify_script_sources_stage_ten_match_semantics_v1(
      stage_ten,
      stage_ten_transition,
      0,
      redeemer,
      [],
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
    validation_machine_v1.verify_script_sources_stage_ten_mismatch_semantics_v1(
      pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: after_mismatch,
      },
      0,
      redeemer,
      [],
    ),
    validation_machine_v1.verify_script_sources_stage_ten_missing_semantics_v1(
      after_mismatch,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: after_mismatch_work_witness_cbor,
        claimed_successor: exact_rejection_post(
          after_mismatch,
          #"455f4d495353494e475f52455155495245445f5749544e455353",
        ),
      },
    ),
  }
}

test script_sources_stage_nine_accepts_an_exact_mismatch() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(7, 0, script_cbor)
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
  let script_item_commitment = bounded_item_v1.from_bytes(7, 0, script_cbor)
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
  let script_item_commitment = bounded_item_v1.from_bytes(7, 0, script_cbor)
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
  let script_item_commitment = bounded_item_v1.from_bytes(7, 0, script_cbor)
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
  let script_item_commitment = bounded_item_v1.from_bytes(7, 0, script_cbor)
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
    validation_machine_v1.verify_script_sources_stage_twelve_redeemer_semantics_v1(
      used_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: used_work_witness_cbor,
        claimed_successor: used_post,
      },
      0,
      redeemer,
      [],
    ),
    validation_machine_v1.verify_script_sources_stage_twelve_redeemer_semantics_v1(
      unused_pre,
      validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: unused_work_witness_cbor,
        claimed_successor: exact_rejection_post(
          unused_pre,
          #"455f494e56414c49445f4649454c445f54595045",
        ),
      },
      0,
      redeemer,
      [],
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
    ledger_delta_root: validation_merkle_v1.frontier_commitment(0, []),
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    7,
    script_cbor,
  )
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    0,
    script_hash,
    bytearray.length(script_cbor),
    collection_proof.item_commitment,
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
    collection_proof.item_commitment,
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
    collection_proof.item_commitment,
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
    collection_proof.item_commitment,
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
      collection_proof.item_commitment,
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
    ledger_delta_root: validation_merkle_v1.frontier_commitment(0, []),
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

test cek_execution_selection_authenticates_program_and_context_subject() {
  let tx = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let subject = #"00"
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
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
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    41,
  )
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
        purpose_kind: 0,
        purpose_index: 0,
        script_hash,
        subject,
        purpose_siblings: [],
        source_index: 0,
        origin_kind: 0,
        source_key: #"00",
        script,
        source_siblings: [],
        redeemer_leaf: h_c,
        execution_siblings: [],
        signer_hashes: [],
      },
    },
  )
}

test cross_language_cek_context_control_vectors() {
  let context = validation_machine_v1.CekContextControlV1 {
    stage: 0,
    language_tag: 3,
    program_term_root: h_a,
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
    validation_machine_v1.encode_cek_context_control_v1(context) == #"981800035820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0000581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb41005820cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc4000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa5600000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba200000040834000000040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba200000083400000",
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
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
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
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      41,
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
        script,
        source_siblings: [],
        redeemer_leaf: h_c,
        execution_siblings: [],
        signer_hashes: [],
      },
    },
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
  let initial_context = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(0),
    redeemer_leaf,
    execution_memory_limit: 0,
    execution_cpu_limit: 0,
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
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    validation_machine_v1.encode_cek_context_control_v1(selected_context),
    0,
    42,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.RedeemerScanWitness {
        redeemer_index: 0,
        redeemer,
        siblings: [],
      },
    },
  )
}

test cek_context_spend_item_uses_authenticated_resolved_frontier() {
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
  let resolved_leaf = script_proof_v1.resolved_context_item_leaf_hash(
    0,
    0,
    key,
    value,
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
      value,
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
  expect Some(next_spend_items) = script_context_v1.prepend_resolved_tx_in_info_v1(
    1,
    resolved_frontier.peaks,
    1,
    0,
    0,
    key,
    value,
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
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.CekResolvedContextItemWitness {
        source_kind: 0,
        item_index: 0,
        key,
        value,
        siblings: [],
      },
    },
  )
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
  )
}

test cek_context_observer_maximum_16_replays_and_finalizes() {
  let base = empty_v1_transaction()
  let observers = observer_hashes_from(0, 16)
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      required_observers: observers,
    },
  }
  let native_control = cek_context_native_control_fixture(tx)
  let context_control = empty_cek_context_control_fixture(5)
  let Pair(collection_proof, chunk_proof) = observer_field_item_proofs(
    16,
    15,
    last_observer_siblings_16(),
  )
  let first_items = validation_machine_v1.prepend_cek_observer_item_v1(
    observer_hash_at(15),
    False,
    context_control.observer_items,
  )
  let first_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    observer_count: 16,
    observer_items: first_items,
    previous_observer: observer_hash_at(15),
  }
  let complete_items = observer_items_summary_fixture(observers, False)
  let complete_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    observer_count: 16,
    observer_items: complete_items,
    previous_observer: observer_hash_at(0),
  }
  let observer_summary = validation_machine_v1.finalize_cek_observer_items_v1(
    complete_items,
    False,
  )
  expect Some(established_summary) = script_context_v1.observer_collection_summary_v1(
    observers,
    False,
  )
  let final_control = validation_machine_v1.CekContextControlV1 {
    ..complete_control,
    stage: 6,
    observer_summary,
  }
  and {
    observer_summary == established_summary,
    verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof,
        chunk_proof,
      },
      45,
    ),
    verify_cek_context_transition_fixture(
      tx,
      native_control,
      complete_control,
      final_control,
      validation_machine_v1.NoAuxiliaryWitness,
      46,
    ),
  }
}

test cek_context_observer_adjacent_17_fails_closed() {
  let base = empty_v1_transaction()
  let observers = observer_hashes_from(0, 17)
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      required_observers: observers,
    },
  }
  let context_control = empty_cek_context_control_fixture(5)
  let Pair(collection_proof, chunk_proof) = observer_field_item_proofs(
    17,
    16,
    [],
  )
  let successor_control = validation_machine_v1.CekContextControlV1 {
    ..context_control,
    observer_count: 17,
    observer_items: validation_machine_v1.prepend_cek_observer_item_v1(
      observer_hash_at(16),
      False,
      context_control.observer_items,
    ),
    previous_observer: observer_hash_at(16),
  }
  !verify_cek_context_transition_fixture(
    tx,
    cek_context_native_control_fixture(tx),
    context_control,
    successor_control,
    validation_machine_v1.TransactionFieldChunkWitness {
      collection_proof,
      chunk_proof,
    },
    47,
  )
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    3,
    h_28_a,
  )
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
      validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof,
        chunk_proof,
      },
      49,
    ),
  }
}

test cek_context_observer_rejects_malformed_proofs_order_and_successors() {
  let base = empty_v1_transaction()
  let observers = observer_hashes_from(0, 2)
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      required_observers: observers,
    },
  }
  let native_control = cek_context_native_control_fixture(tx)
  let context_control = empty_cek_context_control_fixture(5)
  let first_leaf = observer_collection_subtree_root(0, 1)
  let second_leaf = observer_collection_subtree_root(1, 1)
  let Pair(collection_proof, chunk_proof) = observer_field_item_proofs(
    2,
    1,
    [first_leaf],
  )
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
  let Pair(next_collection_proof, next_chunk_proof) = observer_field_item_proofs(
    2,
    0,
    [second_leaf],
  )
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
  let wrong_summary = cek_data_v1.DataSummaryV1 {
    ..exact_summary,
    root: h_a,
  }
  let wrong_summary_control = validation_machine_v1.CekContextControlV1 {
    ..complete_control,
    stage: 6,
    observer_summary: wrong_summary,
  }
  and {
    verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof,
        chunk_proof,
      },
      50,
    ),
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      malformed_successor,
      validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof,
        chunk_proof,
      },
      51,
    ),
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof: bounded_collection_v1.ItemProofV1 {
          ..collection_proof,
          item_index: 0,
        },
        chunk_proof,
      },
      52,
    ),
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof: bounded_collection_v1.ItemProofV1 {
          ..collection_proof,
          item_count: 1,
        },
        chunk_proof,
      },
      53,
    ),
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof: bounded_collection_v1.ItemProofV1 {
          ..collection_proof,
          field_index: 2,
        },
        chunk_proof: bounded_item_v1.ChunkProofV1 {
          ..chunk_proof,
          field_index: 2,
        },
      },
      54,
    ),
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof: bounded_collection_v1.ItemProofV1 {
          ..collection_proof,
          item_length: 27,
        },
        chunk_proof,
      },
      55,
    ),
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof,
        chunk_proof: bounded_item_v1.ChunkProofV1 {
          ..chunk_proof,
          total_length: 27,
        },
      },
      56,
    ),
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      wrong_order_control,
      complete_control,
      validation_machine_v1.TransactionFieldChunkWitness {
        collection_proof: next_collection_proof,
        chunk_proof: next_chunk_proof,
      },
      57,
    ),
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
    observer_summary: cek_data_v1.DataSummaryV1 {
      ..exact_summary,
      root: h_a,
    },
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
  )
}

test cek_context_mint_is_authenticated_and_folded_incrementally() {
  let mint_cbor = #"a1581c"
    |> bytearray.concat(h_28_a)
    |> bytearray.concat(#"a1410124")
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
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.CekDataScanStepWitness {
        redeemer_control: active_redeemers,
        control: scan,
        step: validation_machine_v1.CekDataRevealLeaf {
          raw_cbor: raw,
          parent: None,
          item_length: bytearray.length(raw),
        },
      },
    },
  )
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
  let scan = cek_data_scan_v1.initial_control_v1(
    blake2b_256(redeemer.redeemer_cbor),
    bytearray.length(redeemer.redeemer_cbor),
  )
  expect Some(purpose_summary) = script_context_v1.script_purpose_summary_v1(
    0,
    h_28_a,
    subject,
    False,
  )
  let active_redeemers = validation_machine_v1.CekRedeemerContextControlV1 {
    ..initial_redeemers,
    active_scan_hash: cek_data_scan_v1.hash_control_v1(scan),
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
        redeemer_index: 0,
        redeemer,
        redeemer_siblings: [],
        purpose_frontier_index: 0,
        purpose_kind: 0,
        purpose_index: 0,
        script_hash: h_28_a,
        subject,
        purpose_siblings: [],
      },
    },
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
  let context_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(10),
    purpose_kind: 2,
    script_hash: h_28_a,
    subject: h_28_a,
    redeemer_leaf,
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
  )
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
    focus_root: cek_proof_v1.hash_application_term_v1(
      context_control.program_term_root,
      cek_proof_v1.hash_constant_term_v1(context_value),
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
  )
}

test cek_context_maximum_mint_initializes_from_authenticated_frontier() {
  let mint = decode_midgard_tx_mint_preimage_cbor(maximum_mint_preimage_cbor)
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
  )
}

test cek_context_maximum_mint_authenticates_last_asset_membership() {
  let mint = decode_midgard_tx_mint_preimage_cbor(maximum_mint_preimage_cbor)
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
    validation_machine_v1.verify_one_step_evidence(pre, evidence),
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
    ledger_delta_root: validation_merkle_v1.frontier_commitment(0, []),
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
  let mint_cbor = #"a1581c"
    |> bytearray.concat(h_28_a)
    |> bytearray.concat(#"a1410124")
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
        validation_machine_v1.ValueAndMintControlV1 {
          ..control,
          stage: 2,
        },
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
  let asset_frontier = validation_merkle_v1.build_frontier([
    ledger_output_commitment_v1.asset_leaf_hash(
      policy_id,
      asset_name,
      quantity,
    ),
  ])
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
  let descriptor_frontier = validation_merkle_v1.build_frontier([
    script_proof_v1.output_descriptor_leaf_hash(0, descriptor_cbor),
  ])
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
  )
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

test maximum_general_field_instruction_evidence_is_bounded() {
  let Pair(_pre, base_evidence) = canonical_step_fixture()
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: base_evidence.transition,
    auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
      preimage_cbor: bytearray.from_int_big_endian(0, 8_192)
        |> bytearray.concat(bytearray.from_int_big_endian(0, 1_023)),
    },
  }
  bytearray.length(cbor.serialise(evidence)) < 16_384
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
  bytearray.length(cbor.serialise(evidence)) < 16_384
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
  let Pair(collection_proof, chunk_proof) = singleton_field_item_proofs(
    7,
    item_cbor,
  )
  let pending = validation_machine_v1.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment: collection_proof.item_commitment,
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
    collection_proof,
    chunk_proof,
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
  let item_commitment = bounded_item_v1.from_bytes(7, 0, item_cbor)
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
  let control = stage_zero_script_sources_control(
    tx,
    source_frontier,
    1,
    #"",
  )
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
  let Pair(_collection_proof, chunk_proof) = singleton_field_item_proofs(
    7,
    item_cbor,
  )
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
    item_commitment: bounded_item_v1.from_bytes(7, 0, item_cbor),
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
    item_commitment: bounded_item_v1.from_bytes(7, 0, item_cbor),
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
  let item_commitment = bounded_item_v1.from_bytes(7, 0, item_cbor)
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
    ),
    validation_machine_v1.verify_script_sources_stage_zero_hash_terminal_semantics_v1(
      pre,
      transition,
    ),
  }
}

fn crossing_inline_source_hash_fixture() ->
  Pair<
    validation_machine_v1.InlineSourceHashControlV1,
    Pair<bounded_item_v1.ChunkProofV1, bounded_item_v1.ChunkProofV1>,
  > {
  let payload = builtin.replicate_byte(4_200, 171)
  let item_cbor = encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: PlutusV3Script,
      script_bytes: payload,
    },
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
  let first_hash = bounded_item_v1.hash_chunk(7, 0, 0, first_chunk)
  let second_hash = bounded_item_v1.hash_chunk(7, 0, 1, second_chunk)
  let frontier = validation_merkle_v1.build_frontier(
    [first_hash, second_hash],
  )
  let item_commitment = bounded_item_v1.commitment(
    7,
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
        field_index: 7,
        item_index: 0,
        total_length: bytearray.length(item_cbor),
        chunk_index: 0,
        chunk: first_chunk,
        frontier: frontier.peaks,
        siblings: [second_hash],
      },
      bounded_item_v1.ChunkProofV1 {
        version: 1,
        field_index: 7,
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
  validation_machine_v1.inline_source_hash_block_v1(
    pending,
    current,
    None,
  ) == None
}
