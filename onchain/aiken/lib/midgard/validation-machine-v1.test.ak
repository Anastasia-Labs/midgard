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
use midgard/cek_constant_v1
use midgard/cek_data_scan_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{encode_native_byte_list}
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_tx_input,
  encode_midgard_tx_output,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_mint_preimage_cbor, encode_input_preimage,
  encode_mint_preimage, encode_output_preimage,
  encode_redeemer_witness_preimage, encode_script_witness_preimage,
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

const maximum_mint_transaction_id =
  #"b69b7fdd5ec8fa6e66ff713db2dc03534fd8061f4a6dcace150ccd6c056df371"

const maximum_mint_transaction_commitment =
  #"6a55588e01211488fd676d8994674429bd8788fb3851c7326c7499952c2f8ecf"

const validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

fn v1_program_envelope() -> ByteArray {
  cek_proof_v1.encode_program_envelope_v1(1, 1, 0, h_a, 3, 144)
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

fn input_field_pair_auxiliary(
  tx: MidgardTransaction,
) -> validation_machine_v1.ValidationAuxiliaryWitnessV1 {
  validation_machine_v1.TransactionFieldPairPreimageWitness {
    first_field_index: 0,
    first_preimage_cbor: encode_input_preimage(tx.body.inputs),
    second_field_index: 1,
    second_preimage_cbor: encode_input_preimage(tx.body.reference_inputs),
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
  }
}

fn phase_a_script_preconditions_auxiliary(
  tx: MidgardTransaction,
) -> validation_machine_v1.ValidationAuxiliaryWitnessV1 {
  validation_machine_v1.TransactionFieldPairPreimageWitness {
    first_field_index: 3,
    first_preimage_cbor: encode_native_byte_list(tx.body.required_observers),
    second_field_index: 8,
    second_preimage_cbor: encode_redeemer_witness_preimage(
      tx.witness_set.redeemer_tx_wits,
    ),
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
  receive_hashes: List<ByteArray>,
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
    receive_hashes,
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
  receive_hashes: List<ByteArray>,
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
    receive_hashes,
    program_counter,
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
    [],
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

fn script_discovery_step_fixture(
  tx: MidgardTransaction,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  receive_hashes: List<ByteArray>,
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
    output_cursor: 0,
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    receive_hashes,
    discovery,
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
    execution_count: execution_frontier.count,
    execution_peaks: execution_frontier.peaks,
    execution_cursor,
    language_bitmap,
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
    execution_count: 0,
    execution_peaks: validation_merkle_v1.empty_frontier(),
    execution_cursor: 0,
    language_bitmap: 0,
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

fn cek_state_fixture_from_commitments(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  control: validation_machine_v1.NativeScriptsControlV1,
  context_control_cbor: ByteArray,
  execution_cursor: Int,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
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
      transaction_id,
      transaction_commitment,
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
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_preimage_hash: #"",
    mint_preimage_length: 0,
    mint_offset: 0,
    mint_policy_remaining: 0,
    mint_asset_remaining: 0,
    previous_mint_asset: #"",
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
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

fn maximum_mint_native_control_fixture() -> validation_machine_v1.NativeScriptsControlV1 {
  let singleton = validation_merkle_v1.build_frontier([h_a])
  validation_machine_v1.NativeScriptsControlV1 {
    compact_cbor: #"84028c582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0002020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c058202c64b6e8463997fc126bfce01ba8c2bd55f29c81733d8b72147865335753be67582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff582052408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c00",
    witness_set_compact_cbor: #"83582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
    field_preimage_lengths_cbor: #"8901010101011911a1010101",
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    spend_input_count: 0,
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    signer_count: 0,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
    source_count: 0,
    source_peaks: validation_merkle_v1.empty_frontier(),
    redeemer_count: 0,
    redeemer_peaks: validation_merkle_v1.empty_frontier(),
    purpose_count: 1,
    purpose_peaks: singleton.peaks,
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    execution_count: 1,
    execution_peaks: singleton.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
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
  let Pair(reference_collection_proof, reference_chunk_proof) =
    singleton_field_item_proofs(1, reference_key)
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
  let Pair(spend_collection_proof, spend_chunk_proof) =
    singleton_field_item_proofs(0, spend_key)
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
  let Pair(reference_collection_proof, reference_chunk_proof) =
    singleton_field_item_proofs(1, key)
  let Pair(spend_collection_proof, spend_chunk_proof) =
    singleton_field_item_proofs(0, key)
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
  let Pair(collection_proof, chunk_proof) =
    singleton_field_item_proofs(0, key)
  let rejection_code =
    #"455f494e56414c49445f56414c49444954595f494e54455256414c5f464f524d4154"
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
  let Pair(collection_proof, chunk_proof) =
    singleton_field_item_proofs(4, h_28_a)
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
        claimed_successor: exact_rejection_post(
          required_pre,
          rejection_code,
        ),
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
  let signer_frontier = validation_merkle_v1.build_frontier([
    script_proof_v1.signer_leaf_hash(signer_hash),
  ])
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
  let Pair(collection_proof, chunk_proof) =
    singleton_field_item_proofs(6, address_witness_cbor)
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
        claimed_successor: exact_rejection_post(
          required_pre,
          rejection_code,
        ),
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
  let Pair(collection_proof, chunk_proof) =
    singleton_field_item_proofs(7, item_cbor)
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
  let Pair(pre, _initial_work_witness_cbor) =
    phase_a_native_step_fixture(tx, h_a)
  let item_cbor = encode_midgard_versioned_script(script)
  let Pair(collection_proof, chunk_proof) =
    singleton_field_item_proofs(7, item_cbor)
  let payload_offset = bytearray.length(item_cbor) - bytearray.length(script_bytes)
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
  let Pair(collection_proof, chunk_proof) =
    singleton_field_item_proofs(7, item_cbor)
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
      auxiliary: phase_a_script_preconditions_auxiliary(tx),
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
      auxiliary: phase_a_script_preconditions_auxiliary(tx),
    },
  )
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
      auxiliary: phase_a_script_preconditions_auxiliary(tx),
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

test resolve_inputs_proves_one_membership_per_instruction() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let value = #"a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0"
  let root = singleton_root(key, value)
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
  let next_accumulator = validation_machine_v1.resolved_input_accumulator_successor(
    accumulator,
    0,
    key,
    value,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 9,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      9,
      validation_machine_v1.encode_resolve_inputs_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        2,
        next_accumulator,
        empty_schedule_hash,
        signer_frontier.count,
        validation_merkle_v1.frontier_commitment(
          signer_frontier.count,
          signer_frontier.peaks,
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
      auxiliary: validation_machine_v1.ScheduledLedgerMembershipWitness {
        source_kind: 0,
        key,
        next_schedule_hash: empty_schedule_hash,
        value,
        proof: [],
        signer_proof: validation_machine_v1.NoSignerSetProof,
      },
    },
  )
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

test resolve_inputs_rejects_a_malformed_ledger_output_exactly() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let malformed_value = #"8200"
  let root = singleton_root(key, malformed_value)
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
  let rejection_code = #"455f494e56414c49445f4f5554505554"
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
      auxiliary: validation_machine_v1.ScheduledLedgerMembershipWitness {
        source_kind: 0,
        key,
        next_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
        value: malformed_value,
        proof: [],
        signer_proof: validation_machine_v1.NoSignerSetProof,
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
        [],
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
  let Pair(post_base, _) = script_sources_step_fixture(
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
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post_base,
      },
      auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
        preimage_cbor: encode_script_witness_preimage(
          tx.witness_set.script_tx_wits,
        ),
      },
    },
  )
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
    [script_proof_v1.redeemer_leaf_hash(redeemer)],
  )
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
  let Pair(post_base, _) = script_sources_step_fixture(
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
    11,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post_base,
      },
      auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
        preimage_cbor: encode_redeemer_witness_preimage(
          tx.witness_set.redeemer_tx_wits,
        ),
      },
    },
  )
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
      auxiliary: input_field_pair_auxiliary(tx),
    },
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

test script_sources_commits_outputs_for_bounded_replay() {
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
    [],
    15,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
        preimage_cbor: encode_output_preimage(tx.body.outputs),
      },
    },
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
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
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
    [],
    15,
  )
  let Pair(after_output, _) = script_sources_output_step_fixture(
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
    1,
    output_frontier,
    [h_28_a],
    16,
  )
  let Pair(final_pre, final_work_witness_cbor) = script_sources_output_step_fixture(
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
    1,
    output_frontier,
    [h_28_a],
    16,
  )
  let Pair(stage_six, _) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    6,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    1,
    output_frontier,
    [h_28_a],
    17,
  )
  if step_to_verify == 0 {
    validation_machine_v1.verify_one_step_evidence(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor,
          claimed_successor: after_output,
        },
        auxiliary: validation_machine_v1.OutputReplayWitness {
          output_index: 0,
          output_cbor,
          siblings: [],
          signer_proof: validation_machine_v1.NoSignerSetProof,
        },
      },
    )
  } else {
    and {
      step_to_verify == 1,
      validation_machine_v1.verify_one_step_evidence(
        final_pre,
        validation_machine_v1.ValidationOneStepEvidenceV1 {
          transition: validation_machine_v1.ValidationOneStepWitnessV1 {
            work_witness_cbor: final_work_witness_cbor,
            claimed_successor: stage_six,
          },
          auxiliary: validation_machine_v1.NoAuxiliaryWitness,
        },
      ),
    }
  }
}

test script_sources_collects_protected_receive_purpose() {
  protected_receive_purpose_step(0)
}

test script_sources_finishes_protected_receive_purpose_replay() {
  protected_receive_purpose_step(1)
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
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
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
    [],
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
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: terminal,
      },
      auxiliary: validation_machine_v1.OutputReplayWitness {
        output_index: 0,
        output_cbor,
        siblings: [],
        signer_proof: validation_machine_v1.EmptySignerSetProof { peaks: [] },
      },
    },
  )
}

test script_sources_appends_mint_and_burn_policy_purposes() {
  let mint_cbor = #"a1581c"
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
  let accumulator = validation_machine_v1.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    6,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    validation_machine_v1.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    [],
    17,
  )
  let Pair(post, _) = script_sources_output_step_fixture(
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
    mint_purpose_frontier,
    0,
    empty_frontier,
    [],
    18,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
        preimage_cbor: encode_mint_preimage(tx.body.mint),
      },
    },
  )
}

test script_sources_appends_canonical_observer_purposes() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, required_observers: [h_28_a] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let final_purpose_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.purpose_leaf_hash(2, 0, h_28_a, h_28_a),
      script_proof_v1.purpose_leaf_hash(3, 0, h_28_b, h_28_b),
    ],
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
    [h_28_b],
    18,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    final_purpose_frontier,
    [h_28_b],
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
      auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
        preimage_cbor: encode_native_byte_list(tx.body.required_observers),
      },
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
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(redeemer)
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
    [],
    empty_discovery,
    20,
  )
  let Pair(stage_nine, stage_nine_cbor) = script_discovery_step_fixture(
    tx,
    9,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    [],
    selected_purpose,
    21,
  )
  let Pair(stage_ten, stage_ten_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    [],
    selected_source,
    22,
  )
  let Pair(stage_eight_done, _) = script_discovery_step_fixture(
    tx,
    8,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    [],
    completed,
    23,
  )
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
          script,
          siblings: [],
        },
      },
    ),
    validation_machine_v1.verify_one_step_evidence(
      stage_ten,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: stage_ten_cbor,
          claimed_successor: stage_eight_done,
        },
        auxiliary: validation_machine_v1.RedeemerScanWitness {
          redeemer_index: 0,
          redeemer,
          siblings: [],
        },
      },
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
    [],
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

test native_scripts_verify_each_authenticated_execution_on_l1() {
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
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let script_hash = script_proof_v1.versioned_script_hash(script)
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
  let Pair(valid_pre, valid_work_witness_cbor) = native_scripts_step_fixture(
    valid_tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    0,
    0,
    30,
  )
  let Pair(valid_post, _) = native_scripts_step_fixture(
    valid_tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    1,
    0,
    31,
  )
  let valid_step = validation_machine_v1.verify_one_step_evidence(
    valid_pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: valid_work_witness_cbor,
        claimed_successor: valid_post,
      },
      auxiliary: validation_machine_v1.NativeExecutionScanWitness {
        execution_index: 0,
        language_tag: 0,
        purpose_kind: 0,
        purpose_index: 0,
        script_hash,
        subject: #"0102",
        purpose_siblings: [],
        source_index: 0,
        origin_kind: 0,
        source_key: #"00",
        script,
        source_siblings: [],
        redeemer_leaf: #"",
        execution_siblings: [],
        signer_hashes: [],
      },
    },
  )
  let invalid_tx = MidgardTransaction {
    ..valid_tx,
    body: MidgardTransactionBody {
      ..valid_tx.body,
      validity_interval_start: -1,
    },
  }
  let Pair(invalid_pre, invalid_work_witness_cbor) = native_scripts_step_fixture(
    invalid_tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    0,
    0,
    32,
  )
  let rejection_code = #"455f4e41544956455f5343524950545f494e56414c4944"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..invalid_pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 33,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      33,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        invalid_pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  and {
    valid_step,
    validation_machine_v1.verify_one_step_evidence(
      invalid_pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: invalid_work_witness_cbor,
          claimed_successor: terminal,
        },
        auxiliary: validation_machine_v1.NativeExecutionScanWitness {
          execution_index: 0,
          language_tag: 0,
          purpose_kind: 0,
          purpose_index: 0,
          script_hash,
          subject: #"0102",
          purpose_siblings: [],
          source_index: 0,
          origin_kind: 0,
          source_key: #"00",
          script,
          source_siblings: [],
          redeemer_leaf: #"",
          execution_siblings: [],
          signer_hashes: [],
        },
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
  let valid_work_witness_cbor = validation_machine_v1.encode_script_integrity_witness_v1(
    valid_control,
  )
  let compact = midgard_transaction_to_compact(valid_tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let valid_pre = validation_trace_v1.ValidationMachineStateV1 {
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
    phase: validation_trace_v1.Cek,
    program_counter: 35,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      35,
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
  let valid_step = validation_machine_v1.verify_one_step_evidence(
    valid_pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: valid_work_witness_cbor,
        claimed_successor: valid_post,
      },
      auxiliary: validation_machine_v1.NoAuxiliaryWitness,
    },
  )
  let invalid_tx = base
  let invalid_control = empty_native_control_fixture(invalid_tx)
  let invalid_work_witness_cbor = validation_machine_v1.encode_script_integrity_witness_v1(
    invalid_control,
  )
  let invalid_compact = midgard_transaction_to_compact(invalid_tx)
  let invalid_tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(invalid_compact.body),
  )
  let invalid_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..valid_pre,
    transaction_id: invalid_tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(invalid_tx),
    program_counter: 36,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      36,
      invalid_work_witness_cbor,
    ),
  }
  let rejection_code = #"455f494e56414c49445f4649454c445f54595045"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..invalid_pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 37,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      37,
      validation_machine_v1.encode_terminal_rejection_witness(
        rejection_code,
        invalid_pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  and {
    valid_step,
    validation_machine_v1.verify_one_step_evidence(
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
  let deletion = validation_machine_v1.ledger_delta_operation_leaf_hash(
    0,
    #"010203",
    #"",
  )
  let insertion = validation_machine_v1.ledger_delta_operation_leaf_hash(
    1,
    #"0405",
    #"060708",
  )
  let frontier = validation_merkle_v1.build_frontier([deletion, insertion])
  and {
    deletion == #"51c42aeb84dd425e93bb0238422677a4d97e57702f3ad4e77fcd1aeb0037d107",
    insertion == #"4de61a76fc75a19efd570dc3666f9f8438d0f742c875c0837edaba8a593d3de7",
    validation_merkle_v1.frontier_commitment(frontier.count, frontier.peaks) == #"f0dfa1163ab84405f2ac41b4af371fdb19ec52bb67ce53a343e1b65abdd6c6e7",
  }
}

test ledger_delta_terminal_step_binds_post_root_and_operation_frontier() {
  let tx = empty_v1_transaction()
  let native_control = empty_native_control_fixture(tx)
  let control = validation_machine_v1.LedgerDeltaControlV1 {
    native_control,
    stage: 3,
    replay_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_accumulator: validation_machine_v1.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: validation_machine_v1.empty_resolution_schedule_hash(),
    current_ledger_root: h_b,
    output_cursor: 0,
    operation_count: 0,
    operation_peaks: validation_merkle_v1.empty_frontier(),
  }
  let work_witness_cbor = validation_machine_v1.encode_ledger_delta_control_v1(
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
    ledger_delta_root: validation_merkle_v1.frontier_commitment(0, []),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 47,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      47,
      validation_machine_v1.encode_terminal_acceptance_witness_v1(h_b, 0, []),
    ),
    verdict: validation_trace_v1.Accepted,
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
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_preimage_hash: #"",
    mint_preimage_length: 0,
    mint_offset: 0,
    mint_policy_remaining: 0,
    mint_asset_remaining: 0,
    previous_mint_asset: #"",
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
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
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_preimage_hash: #"",
    mint_preimage_length: 0,
    mint_offset: 0,
    mint_policy_remaining: 0,
    mint_asset_remaining: 0,
    previous_mint_asset: #"",
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
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
    validation_machine_v1.encode_cek_context_control_v1(context) == #"981d00035820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0000581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb41005820cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc4000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008340000040000000004000800040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba200000083400000",
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
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(redeemer)
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
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_preimage_hash: #"",
    mint_preimage_length: 0,
    mint_offset: 0,
    mint_policy_remaining: 0,
    mint_asset_remaining: 0,
    previous_mint_asset: #"",
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
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

test cek_context_observers_use_authenticated_field_preimage() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      required_observers: [h_28_a, h_28_b],
    },
  }
  let control = cek_context_native_control_fixture(tx)
  let context_control = empty_cek_context_control_fixture(5)
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    tx.body.required_observers,
    False,
  )
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(context_control),
    0,
    45,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(
      validation_machine_v1.CekContextControlV1 {
        ..context_control,
        stage: 6,
        observer_summary,
      },
    ),
    0,
    46,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
        preimage_cbor: encode_native_byte_list(tx.body.required_observers),
      },
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
  let control = cek_context_native_control_fixture(tx)
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let init_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(6),
    observer_summary,
  }
  let mint_leaf = validation_machine_v1.mint_asset_leaf_hash(h_28_a, #"01", -5)
  let mint_frontier = validation_merkle_v1.build_frontier([mint_leaf])
  let scan_control = validation_machine_v1.CekContextControlV1 {
    ..init_control,
    stage: 7,
    mint_preimage_hash: #"a6a34a174a22fa5cb2e8b3fd6b0c360f67601e0df8cdc5a101118958ac3afa65",
    mint_preimage_length: 35,
    mint_offset: 1,
    mint_policy_remaining: 1,
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
    validation_machine_v1.encode_cek_context_control_v1(scan_control),
    0,
    48,
  )
  let init_step = validation_machine_v1.verify_one_step_evidence(
    init_pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: init_work_witness_cbor,
        claimed_successor: init_post,
      },
      auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
        preimage_cbor: encode_mint_preimage(tx.body.mint),
      },
    },
  )
  let semantic_control = validation_machine_v1.CekContextControlV1 {
    ..scan_control,
    stage: 8,
    mint_offset: 35,
    mint_policy_remaining: 0,
    mint_count: 1,
    mint_peaks: mint_frontier.peaks,
  }
  let Pair(scan_pre, scan_work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(scan_control),
    0,
    48,
  )
  let Pair(scan_post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(semantic_control),
    0,
    49,
  )
  let scan_step = validation_machine_v1.verify_one_step_evidence(
    scan_pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor: scan_work_witness_cbor,
        claimed_successor: scan_post,
      },
      auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
        preimage_cbor: encode_mint_preimage(tx.body.mint),
      },
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
    49,
  )
  let Pair(item_post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(item_control),
    0,
    50,
  )
  let item_step = validation_machine_v1.verify_one_step_evidence(
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
    50,
  )
  let Pair(final_post, _) = cek_state_fixture(
    tx,
    control,
    validation_machine_v1.encode_cek_context_control_v1(final_control),
    0,
    51,
  )
  let final_step = validation_machine_v1.verify_one_step_evidence(
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
    scan_step,
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
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(redeemer)
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
    mint_preimage_hash: blake2b_256(#"80"),
    mint_preimage_length: 1,
    mint_offset: 1,
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
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(redeemer)
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
    mint_preimage_hash: blake2b_256(#"80"),
    mint_preimage_length: 1,
    mint_offset: 1,
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
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(redeemer)
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
    mint_preimage_hash: blake2b_256(#"80"),
    mint_preimage_length: 1,
    mint_offset: 1,
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
    mint_preimage_hash: blake2b_256(#"80"),
    mint_preimage_length: 1,
    mint_offset: 1,
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
    mint_preimage_hash: blake2b_256(#"80"),
    mint_preimage_length: 1,
    mint_offset: 1,
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
    mint_preimage_hash: blake2b_256(#"80"),
    mint_preimage_length: 1,
    mint_offset: 1,
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

test cek_context_maximum_mint_stream_initialization_fits_one_step() {
  let control = maximum_mint_native_control_fixture()
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let init_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(6),
    observer_summary,
  }
  let scan_control = validation_machine_v1.CekContextControlV1 {
    ..init_control,
    stage: 7,
    mint_preimage_hash: #"2c64b6e8463997fc126bfce01ba8c2bd55f29c81733d8b72147865335753be67",
    mint_preimage_length: 4513,
    mint_offset: 1,
    mint_policy_remaining: 1,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture_from_commitments(
    maximum_mint_transaction_id,
    maximum_mint_transaction_commitment,
    control,
    validation_machine_v1.encode_cek_context_control_v1(init_control),
    0,
    51,
  )
  let Pair(post, _) = cek_state_fixture_from_commitments(
    maximum_mint_transaction_id,
    maximum_mint_transaction_commitment,
    control,
    validation_machine_v1.encode_cek_context_control_v1(scan_control),
    0,
    52,
  )
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
        preimage_cbor: maximum_mint_preimage_cbor,
      },
    },
  )
}

test cek_context_maximum_mint_stream_item_fits_one_step() {
  let control = maximum_mint_native_control_fixture()
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let scan_control = validation_machine_v1.CekContextControlV1 {
    ..empty_cek_context_control_fixture(7),
    observer_summary,
    mint_preimage_hash: #"2c64b6e8463997fc126bfce01ba8c2bd55f29c81733d8b72147865335753be67",
    mint_preimage_length: 4513,
    mint_offset: 1,
    mint_policy_remaining: 1,
  }
  let first_asset = bytearray.from_int_big_endian(0, 32)
  let first_leaf = validation_machine_v1.mint_asset_leaf_hash(
    h_28_a,
    first_asset,
    1,
  )
  let next_control = validation_machine_v1.CekContextControlV1 {
    ..scan_control,
    mint_offset: 68,
    mint_policy_remaining: 0,
    mint_asset_remaining: 127,
    previous_mint_asset: first_asset,
    mint_count: 1,
    mint_peaks: [
      validation_merkle_v1.FrontierPeak { height: 0, hash: first_leaf },
    ],
    current_mint_policy: h_28_a,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture_from_commitments(
    maximum_mint_transaction_id,
    maximum_mint_transaction_commitment,
    control,
    validation_machine_v1.encode_cek_context_control_v1(scan_control),
    0,
    53,
  )
  let Pair(post, _) = cek_state_fixture_from_commitments(
    maximum_mint_transaction_id,
    maximum_mint_transaction_commitment,
    control,
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
      auxiliary: validation_machine_v1.TransactionFieldPreimageWitness {
        preimage_cbor: maximum_mint_preimage_cbor,
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

test ledger_delta_spend_step_deletes_authenticated_utxo_root() {
  let tx = empty_v1_transaction()
  let key = #"010203"
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
  let initial_accumulator = validation_machine_v1.initial_resolution_accumulator()
  let resolved_accumulator = validation_machine_v1.resolved_input_accumulator_successor(
    initial_accumulator,
    0,
    key,
    value,
  )
  let native_control = validation_machine_v1.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    resolved_input_count: 1,
    resolved_inputs_accumulator: resolved_accumulator,
    spend_input_count: 1,
    resolved_item_peaks: validation_merkle_v1.build_frontier(
      [script_proof_v1.resolved_context_item_leaf_hash(0, 0, key, value)],
    ).peaks,
  }
  let empty_schedule = validation_machine_v1.empty_resolution_schedule_hash()
  let schedule = validation_machine_v1.resolution_schedule_node_hash(
    0,
    key,
    empty_schedule,
  )
  let current_root = singleton_root(key, value)
  let control = validation_machine_v1.LedgerDeltaControlV1 {
    native_control,
    stage: 1,
    replay_schedule_hash: schedule,
    replay_cursor: 0,
    replay_accumulator: initial_accumulator,
    replay_remaining_schedule_hash: schedule,
    current_ledger_root: current_root,
    output_cursor: 0,
    operation_count: 0,
    operation_peaks: [],
  }
  let work_witness_cbor = validation_machine_v1.encode_ledger_delta_control_v1(
    control,
  )
  let operation_leaf = validation_machine_v1.ledger_delta_operation_leaf_hash(
    0,
    key,
    #"",
  )
  let operation_frontier = validation_merkle_v1.build_frontier([operation_leaf])
  let next_control = validation_machine_v1.LedgerDeltaControlV1 {
    ..control,
    replay_cursor: 1,
    replay_accumulator: resolved_accumulator,
    replay_remaining_schedule_hash: empty_schedule,
    current_ledger_root: zero,
    operation_count: 1,
    operation_peaks: operation_frontier.peaks,
  }
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
    prior_ledger_root: current_root,
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
      validation_machine_v1.encode_ledger_delta_control_v1(next_control),
    ),
  }
  validation_machine_v1.verify_one_step_evidence(
    pre,
    validation_machine_v1.ValidationOneStepEvidenceV1 {
      transition: validation_machine_v1.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      auxiliary: validation_machine_v1.LedgerDeltaReplayWitness {
        source_kind: 0,
        key,
        next_schedule_hash: empty_schedule,
        value,
        proof: [],
      },
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
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
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
    validation_machine_v1.verify_one_step_evidence(
      valid_pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor: valid_work_witness_cbor,
          claimed_successor: valid_post,
        },
        auxiliary: validation_machine_v1.NoAuxiliaryWitness,
      },
    ),
    validation_machine_v1.verify_one_step_evidence(
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

test value_and_mint_replays_signed_burn_leaf_on_l1() {
  let tx = empty_v1_transaction()
  let native_control = empty_native_control_fixture(tx)
  let policy_id = #"11111111111111111111111111111111111111111111111111111111"
  let asset_name = #"beef"
  let unit = policy_id |> bytearray.concat(asset_name)
  let leaf = validation_machine_v1.mint_asset_leaf_hash(
    policy_id,
    asset_name,
    -5,
  )
  let mint_frontier = validation_merkle_v1.build_frontier([leaf])
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
    mint_count: 1,
    mint_peaks: mint_frontier.peaks,
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
  validation_machine_v1.verify_one_step_evidence(
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
    mint_count: 0,
    mint_peaks: [],
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
      output_cbor: bytearray.from_int_big_endian(0, 4095),
      siblings: list.repeat(zero, 6),
      asset_index: 127,
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
    [],
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
    receive_hashes: [],
    discovery,
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
      script: MidgardVersionedScript {
        language: PlutusV3Script,
        script_bytes: v1_program_envelope(),
      },
      siblings: list.repeat(zero, 4),
    },
  }
  bytearray.length(cbor.serialise(evidence)) < 16_384
}
