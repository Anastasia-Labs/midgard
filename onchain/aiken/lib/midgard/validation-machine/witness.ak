use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/native_tx/compact.{
  native_tx_proof_commitment_v1, verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_input_cbor, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTxInput, NativeTxWitnessSetCompact, VerifiedMidgardNativeTxCompact,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_v1
use midgard/ledger_output_v1
use midgard/mpf_proof_fold_v1
use midgard/native_script_scan_v1
use midgard/native_tx_field_access_v1
use midgard/validation_machine/machine_types.{
  CanonicalDecodeControlV1, CekContextControlV1, CekContextPartsControlV1,
  CekFinalContextControlV1, CekRedeemerContextControlV1,
  CekTxInfoAssemblyControlV1, CekWitnessControlV1, InlineSourceHashControlV1,
  InputSetsControlV1, LedgerDeltaControlV1, LedgerDeltaPendingMutationV1,
  MintFoldControlV1, NativeScriptsControlV1, ObserverPurposeScanControlV1,
  PhaseANativeScriptsControlV1, PhaseAScriptPreconditionsControlV1,
  ReceivePurposeScanControlV1, ResolveInputOutputProofV1, ResolveInputsControlV1,
  ScheduledInputV1, ScriptDiscoveryControlV1, ScriptSourcesControlV1,
  SignaturesControlV1, ValidationOneStepWitnessV1, ValueAccumulatorV1,
  ValueAndMintControlV1,
}
use midgard/validation_machine/shared.{
  finalize_cek_observer_items_v1, max_aggregate_field_preimage_bytes,
  max_tx_size_derived_collection_item_count, no_rejection_code_hash,
  script_discovery_bit,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub fn encode_compact_binding_witness(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
) -> ByteArray {
  #"86"
    |> bytearray.concat(encode_definite_bytes(transaction_id))
    |> bytearray.concat(encode_definite_bytes(transaction_commitment))
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
}

pub fn encode_transaction_field_scan_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  field_index: Int,
  item_index: Int,
  chunk_index: Int,
  item_count: Int,
  encoded_length: Int,
) -> ByteArray {
  expect field_index >= 0
  expect field_index <= 8
  expect item_index >= 0
  expect chunk_index >= 0
  expect item_count >= -1
  expect encoded_length >= 0
  #"89"
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(cbor.serialise(field_index))
    |> bytearray.concat(cbor.serialise(item_index))
    |> bytearray.concat(cbor.serialise(chunk_index))
    |> bytearray.concat(cbor.serialise(item_count))
    |> bytearray.concat(cbor.serialise(encoded_length))
}

pub fn encode_static_rules_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
) -> ByteArray {
  #"84"
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
}

pub fn encode_input_sets_scan_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  spend_count: Int,
  reference_count: Int,
  spend_seen: Int,
  reference_seen: Int,
  previous_key: ByteArray,
  resolution_schedule_hash: ByteArray,
) -> ByteArray {
  expect spend_count >= -1
  expect reference_count >= -1
  expect spend_seen >= 0
  expect reference_seen >= 0
  // `previous_key` is the §5.3 spend-/reference-input item this scan last read
  // (`verify_input_sets_item` takes it straight off the item it decoded), so its
  // one admissible width is the item width the field grammar declares. Named off
  // `native_tx_field_access_v1` rather than restated as a literal: the retired
  // minimal-index spelling was 36 bytes, this domain assertion still said 36
  // after #586 moved the item to the fixed 3-byte output index, and a scan that
  // reads one input could then not encode its own successor witness at all.
  expect or {
      bytearray.length(previous_key) == 0,
      bytearray.length(previous_key) == native_tx_field_access_v1.spend_input_item_bytes,
    }
  expect bytearray.length(resolution_schedule_hash) == 32
  encode_definite_array_header(10)
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(cbor.serialise(spend_count))
    |> bytearray.concat(cbor.serialise(reference_count))
    |> bytearray.concat(cbor.serialise(spend_seen))
    |> bytearray.concat(cbor.serialise(reference_seen))
    |> bytearray.concat(encode_definite_bytes(previous_key))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
}

pub fn encode_phase_a_native_scripts_scan_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolution_schedule_hash: ByteArray,
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
  signer_count: Int,
  signer_peaks: List<validation_merkle_v1.FrontierPeak>,
  continuation_cbor: ByteArray,
) -> ByteArray {
  expect bytearray.length(resolution_schedule_hash) == 32
  expect stage >= 0
  expect stage <= 8
  expect script_count >= -1
  expect script_seen >= 0
  expect contains_non_native_script == 0 || contains_non_native_script == 1
  expect item_length >= 0
  expect
    bytearray.length(item_commitment) == 0 || bytearray.length(item_commitment) == 32
  expect cursor >= 0
  expect bytearray.length(stack_root) == 0 || bytearray.length(stack_root) == 32
  expect stack_depth >= 0
  expect node_count >= 0
  expect result >= -1
  expect result <= 1
  expect signer_count >= 0
  #"9f"
    |> bytearray.concat(cbor.serialise(builtin.b_data(compact_cbor)))
    |> bytearray.concat(
        cbor.serialise(builtin.b_data(witness_set_compact_cbor)),
      )
    |> bytearray.concat(
        cbor.serialise(builtin.b_data(field_preimage_lengths_cbor)),
      )
    |> bytearray.concat(cbor.serialise(builtin.b_data(context_cbor)))
    |> bytearray.concat(
        cbor.serialise(builtin.b_data(resolution_schedule_hash)),
      )
    |> bytearray.concat(cbor.serialise(stage))
    |> bytearray.concat(cbor.serialise(script_count))
    |> bytearray.concat(cbor.serialise(script_seen))
    |> bytearray.concat(cbor.serialise(contains_non_native_script))
    |> bytearray.concat(cbor.serialise(item_length))
    |> bytearray.concat(cbor.serialise(builtin.b_data(item_commitment)))
    |> bytearray.concat(cbor.serialise(cursor))
    |> bytearray.concat(cbor.serialise(builtin.b_data(stack_root)))
    |> bytearray.concat(cbor.serialise(stack_depth))
    |> bytearray.concat(cbor.serialise(node_count))
    |> bytearray.concat(cbor.serialise(result))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(encode_definite_array_header(2))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(signer_peaks))
    |> bytearray.concat(cbor.serialise(builtin.b_data(continuation_cbor)))
    |> bytearray.concat(#"ff")
}

pub fn encode_signatures_scan_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolution_schedule_hash: ByteArray,
  stage: Int,
  address_count: Int,
  required_count: Int,
  address_seen: Int,
  required_seen: Int,
  previous_order_key: ByteArray,
  previous_signer_hash: ByteArray,
  signer_count: Int,
  signer_peaks: List<validation_merkle_v1.FrontierPeak>,
  invalid_signature_seen: Int,
) -> ByteArray {
  expect bytearray.length(resolution_schedule_hash) == 32
  expect stage >= 0
  expect stage <= 2
  expect address_count >= -1
  expect required_count >= -1
  expect address_seen >= 0
  expect required_seen >= 0
  expect
    bytearray.length(previous_signer_hash) == 0 || bytearray.length(
      previous_signer_hash,
    ) == 28
  expect signer_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(signer_count, signer_peaks)
  expect invalid_signature_seen == 0 || invalid_signature_seen == 1
  encode_definite_array_header(15)
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
    |> bytearray.concat(cbor.serialise(stage))
    |> bytearray.concat(cbor.serialise(address_count))
    |> bytearray.concat(cbor.serialise(required_count))
    |> bytearray.concat(cbor.serialise(address_seen))
    |> bytearray.concat(cbor.serialise(required_seen))
    |> bytearray.concat(encode_definite_bytes(previous_order_key))
    |> bytearray.concat(encode_definite_bytes(previous_signer_hash))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(signer_peaks))
    |> bytearray.concat(cbor.serialise(invalid_signature_seen))
}

pub fn encode_phase_a_script_preconditions_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolution_schedule_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  has_non_native_script: Bool,
  observer_count: Int,
  observer_seen: Int,
  previous_observer: ByteArray,
) -> ByteArray {
  expect bytearray.length(resolution_schedule_hash) == 32
  expect signer_count >= 0
  expect bytearray.length(signer_frontier_commitment) == 32
  expect observer_count >= 0
  expect observer_seen >= 0
  expect
    bytearray.length(previous_observer) == 0 || bytearray.length(
      previous_observer,
    ) == 28
  encode_definite_array_header(11)
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(encode_definite_bytes(signer_frontier_commitment))
    |> bytearray.concat(
        cbor.serialise(
          if has_non_native_script {
            1
          } else {
            0
          },
        ),
      )
    |> bytearray.concat(encode_definite_bytes(previous_observer))
    |> bytearray.concat(cbor.serialise(observer_count))
    |> bytearray.concat(cbor.serialise(observer_seen))
}

const resolution_accumulator_domain =
  #"4d6964676172645265736f6c766564496e70757473416363756d756c61746f725631"

const resolution_schedule_domain =
  #"4d696467617264496e7075745265736f6c7574696f6e5363686564756c655631"

pub fn initial_resolution_accumulator() -> ByteArray {
  blake2b_256(resolution_accumulator_domain)
}

pub fn empty_resolution_schedule_hash() -> ByteArray {
  blake2b_256(resolution_schedule_domain)
}

pub fn resolution_schedule_node_hash(
  source_kind: Int,
  key: ByteArray,
  next_hash: ByteArray,
) -> ByteArray {
  expect source_kind == 0 || source_kind == 1
  expect bytearray.length(next_hash) == 32
  resolution_schedule_domain
    |> bytearray.concat(cbor.serialise(source_kind))
    |> bytearray.concat(encode_definite_bytes(key))
    |> bytearray.concat(next_hash)
    |> blake2b_256
}

fn scheduled_input_resolution_hash(inputs: List<ScheduledInputV1>) -> ByteArray {
  when inputs is {
    [] -> empty_resolution_schedule_hash()
    [input, ..rest] ->
      resolution_schedule_node_hash(
        input.source_kind,
        input.key,
        scheduled_input_resolution_hash(rest),
      )
  }
}

fn compare_scheduled_inputs(
  left: ScheduledInputV1,
  right: ScheduledInputV1,
) -> Ordering {
  bytearray.compare(left.key, right.key)
}

pub fn transaction_resolution_schedule_hash(
  spend_inputs: List<MidgardTxInput>,
  reference_inputs: List<MidgardTxInput>,
) -> ByteArray {
  list.concat(
    spend_inputs
      |> list.map(
          fn(input) {
            ScheduledInputV1 {
              source_kind: 0,
              key: encode_midgard_tx_input(input),
            }
          },
        ),
    reference_inputs
      |> list.map(
          fn(input) {
            ScheduledInputV1 {
              source_kind: 1,
              key: encode_midgard_tx_input(input),
            }
          },
        ),
  )
    |> list.sort(compare_scheduled_inputs)
    |> scheduled_input_resolution_hash
}

fn encode_resolve_input_output_proof(
  pending: ResolveInputOutputProofV1,
) -> ByteArray {
  #"85"
    |> bytearray.concat(cbor.serialise(pending.source_kind))
    |> bytearray.concat(encode_definite_bytes(pending.key))
    |> bytearray.concat(encode_definite_bytes(pending.next_schedule_hash))
    |> bytearray.concat(encode_definite_bytes(pending.descriptor_cbor))
    |> bytearray.concat(
        encode_definite_bytes(
          ledger_output_proof_v1.encode_control_v1(pending.output_proof),
        ),
      )
}

fn resolve_input_output_proof_from_cbor(
  pending_cbor: ByteArray,
) -> ResolveInputOutputProofV1 {
  expect Some(data) = cbor.deserialise(pending_cbor)
  expect
      [
        source_kind_data, key_data, next_schedule_hash_data,
        descriptor_cbor_data, output_proof_cbor_data,
      ]
    = builtin.un_list_data(data)
  ResolveInputOutputProofV1 {
    source_kind: builtin.un_i_data(source_kind_data),
    key: builtin.un_b_data(key_data),
    next_schedule_hash: builtin.un_b_data(next_schedule_hash_data),
    descriptor_cbor: builtin.un_b_data(descriptor_cbor_data),
    output_proof: output_proof_cbor_data
      |> builtin.un_b_data
      |> ledger_output_proof_v1.decode_control_v1,
  }
}

fn encode_optional_resolve_input_output_proof(
  pending: Option<ResolveInputOutputProofV1>,
) -> ByteArray {
  when pending is {
    None -> encode_definite_bytes(#"00")
    Some(active) ->
      encode_resolve_input_output_proof(active)
        |> encode_definite_bytes
  }
}

pub fn encode_resolve_inputs_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  cursor: Int,
  accumulator: ByteArray,
  remaining_schedule_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  pending: Option<ResolveInputOutputProofV1>,
  resolution_schedule_hash: ByteArray,
) -> ByteArray {
  expect cursor >= 0
  expect bytearray.length(accumulator) == 32
  expect bytearray.length(remaining_schedule_hash) == 32
  expect signer_count >= 0
  expect bytearray.length(signer_frontier_commitment) == 32
  expect bytearray.length(resolution_schedule_hash) == 32
  #"8b"
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(cbor.serialise(cursor))
    |> bytearray.concat(encode_definite_bytes(accumulator))
    |> bytearray.concat(encode_definite_bytes(remaining_schedule_hash))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(encode_definite_bytes(signer_frontier_commitment))
    |> bytearray.concat(encode_optional_resolve_input_output_proof(pending))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
}

pub fn empty_observer_purpose_scan_control() -> ObserverPurposeScanControlV1 {
  ObserverPurposeScanControlV1 { total_count: 0, seen: 0, previous_hash: #"" }
}

pub fn encode_observer_purpose_scan_control(
  control: ObserverPurposeScanControlV1,
) -> ByteArray {
  expect control.total_count >= 0
  expect control.seen >= 0
  expect control.seen <= control.total_count
  expect
    if control.seen == 0 {
      control.previous_hash == #""
    } else {
      bytearray.length(control.previous_hash) == 28
    }
  #"83"
    |> bytearray.concat(cbor.serialise(control.total_count))
    |> bytearray.concat(encode_definite_bytes(control.previous_hash))
    |> bytearray.concat(cbor.serialise(control.seen))
}

pub fn empty_receive_purpose_scan_control() -> ReceivePurposeScanControlV1 {
  ReceivePurposeScanControlV1 {
    source_count: 0,
    source_peaks: [],
    receive_count: 0,
    previous_hash: #"",
    candidate_hash: #"",
    descriptor_peaks: [],
  }
}

pub fn encode_receive_purpose_scan_control(
  control: ReceivePurposeScanControlV1,
) -> ByteArray {
  expect control.source_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    )
  expect control.receive_count >= 0
  expect
    if control.receive_count == 0 {
      control.previous_hash == #""
    } else {
      bytearray.length(control.previous_hash) == 28
    }
  expect or {
      control.candidate_hash == #"",
      and {
        bytearray.length(control.candidate_hash) == 28,
        or {
          control.previous_hash == #"",
          bytearray.compare(control.previous_hash, control.candidate_hash) == Less,
        },
      },
    }
  #"86"
    |> bytearray.concat(cbor.serialise(control.source_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.source_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.receive_count))
    |> bytearray.concat(encode_definite_bytes(control.previous_hash))
    |> bytearray.concat(encode_definite_bytes(control.candidate_hash))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.descriptor_peaks),
      )
}

pub fn encode_script_sources_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  resolved_item_peaks: List<validation_merkle_v1.FrontierPeak>,
  stage: Int,
  source_count: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
  redeemer_count: Int,
  redeemer_peaks: List<validation_merkle_v1.FrontierPeak>,
  replay_cursor: Int,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  spend_index: Int,
  purpose_count: Int,
  purpose_peaks: List<validation_merkle_v1.FrontierPeak>,
  output_cursor: Int,
  output_count: Int,
  output_peaks: List<validation_merkle_v1.FrontierPeak>,
  output_total_count: Int,
  receive_scan: ReceivePurposeScanControlV1,
  source_total_count: Int,
  redeemer_total_count: Int,
  observer_scan: ObserverPurposeScanControlV1,
  mint_fold: MintFoldControlV1,
  resolution_schedule_hash: ByteArray,
) -> ByteArray {
  expect resolved_input_count >= 0
  expect bytearray.length(resolved_inputs_accumulator) == 32
  expect signer_count >= 0
  expect bytearray.length(signer_frontier_commitment) == 32
  expect
    validation_merkle_v1.frontier_is_well_formed(
      replay_cursor,
      resolved_item_peaks,
    )
  expect stage >= 0
  expect source_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(source_count, source_peaks)
  expect redeemer_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(redeemer_count, redeemer_peaks)
  expect replay_cursor >= 0
  expect bytearray.length(replay_accumulator) == 32
  expect bytearray.length(replay_remaining_schedule_hash) == 32
  expect spend_index >= 0
  expect purpose_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(purpose_count, purpose_peaks)
  expect output_cursor >= 0
  expect output_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(output_count, output_peaks)
  expect output_cursor <= output_count
  expect output_total_count >= output_count
  expect source_total_count >= source_count
  expect redeemer_total_count >= redeemer_count
  expect bytearray.length(resolution_schedule_hash) == 32
  encode_definite_array_header(30)
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(cbor.serialise(resolved_input_count))
    |> bytearray.concat(encode_definite_bytes(resolved_inputs_accumulator))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(encode_definite_bytes(signer_frontier_commitment))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(resolved_item_peaks),
      )
    |> bytearray.concat(cbor.serialise(stage))
    |> bytearray.concat(cbor.serialise(source_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(source_peaks))
    |> bytearray.concat(cbor.serialise(redeemer_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(redeemer_peaks))
    |> bytearray.concat(cbor.serialise(replay_cursor))
    |> bytearray.concat(encode_definite_bytes(replay_accumulator))
    |> bytearray.concat(encode_definite_bytes(replay_remaining_schedule_hash))
    |> bytearray.concat(cbor.serialise(spend_index))
    |> bytearray.concat(cbor.serialise(purpose_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(purpose_peaks))
    |> bytearray.concat(cbor.serialise(output_cursor))
    |> bytearray.concat(cbor.serialise(output_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(output_peaks))
    |> bytearray.concat(cbor.serialise(output_total_count))
    |> bytearray.concat(encode_receive_purpose_scan_control(receive_scan))
    |> bytearray.concat(cbor.serialise(source_total_count))
    |> bytearray.concat(cbor.serialise(redeemer_total_count))
    |> bytearray.concat(encode_observer_purpose_scan_control(observer_scan))
    |> bytearray.concat(encode_mint_fold_control(mint_fold))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
}

pub fn resolved_input_accumulator_successor(
  accumulator: ByteArray,
  source_kind: Int,
  key: ByteArray,
  value: ByteArray,
) -> ByteArray {
  resolution_accumulator_domain
    |> bytearray.concat(accumulator)
    |> bytearray.concat(cbor.serialise(source_kind))
    |> bytearray.concat(encode_definite_bytes(key))
    |> bytearray.concat(encode_definite_bytes(value))
    |> blake2b_256
}

pub fn canonical_decode_control_from_witness(
  witness_cbor: ByteArray,
) -> CanonicalDecodeControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data, field_index_data,
        item_index_data, chunk_index_data, item_count_data, encoded_length_data,
      ]
    = builtin.un_list_data(data)
  CanonicalDecodeControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    field_index: builtin.un_i_data(field_index_data),
    item_index: builtin.un_i_data(item_index_data),
    chunk_index: builtin.un_i_data(chunk_index_data),
    item_count: builtin.un_i_data(item_count_data),
    encoded_length: builtin.un_i_data(encoded_length_data),
  }
}

pub fn canonical_decode_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: CanonicalDecodeControlV1,
  verified: VerifiedMidgardNativeTxCompact,
) -> Bool {
  let exact_witness = encode_transaction_field_scan_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.field_index,
    control.item_index,
    control.chunk_index,
    control.item_count,
    control.encoded_length,
  )
  and {
    verified.version == 1,
    control.field_index >= 0,
    control.field_index <= 8,
    control.item_index >= 0,
    control.chunk_index >= 0,
    control.item_count >= -1,
    control.encoded_length >= 0,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == exact_witness,
  }
}

pub fn input_sets_control_from_witness(
  witness_cbor: ByteArray,
) -> InputSetsControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data, spend_count_data,
        reference_count_data, spend_seen_data, reference_seen_data,
        previous_key_data, resolution_schedule_hash_data,
      ]
    = builtin.un_list_data(data)
  InputSetsControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    spend_count: builtin.un_i_data(spend_count_data),
    reference_count: builtin.un_i_data(reference_count_data),
    spend_seen: builtin.un_i_data(spend_seen_data),
    reference_seen: builtin.un_i_data(reference_seen_data),
    previous_key: builtin.un_b_data(previous_key_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
  }
}

pub fn input_sets_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: InputSetsControlV1,
  verified: VerifiedMidgardNativeTxCompact,
) -> Bool {
  let body = verified.tx_compact.body
  let spend_is_empty = body.spend_inputs_hash == native_tx_field_access_v1.empty_field_commitment
  let reference_is_empty = body.reference_inputs_hash == native_tx_field_access_v1.empty_field_commitment
  let seen = control.spend_seen + control.reference_seen
  and {
    verified.version == 1,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.spend_count >= -1,
    control.spend_count <= max_tx_size_derived_collection_item_count,
    control.reference_count >= -1,
    control.reference_count <= max_tx_size_derived_collection_item_count,
    control.spend_seen >= 0,
    control.reference_seen >= 0,
    if spend_is_empty {
      control.spend_count == 0 && control.spend_seen == 0
    } else if control.spend_count == -1 {
      control.spend_seen == 0
    } else {
      control.spend_count > 0 && control.spend_seen <= control.spend_count
    },
    if reference_is_empty {
      control.reference_count == 0 && control.reference_seen == 0
    } else if control.reference_count == -1 {
      control.reference_seen == 0
    } else {
      control.reference_count > 0 && control.reference_seen <= control.reference_count
    },
    if seen == 0 {
      and {
        control.previous_key == #"",
        control.resolution_schedule_hash == empty_resolution_schedule_hash(),
      }
    } else {
      // The same §5.3 item width `encode_input_sets_scan_witness` admits, and
      // named off the same constant so the two cannot drift apart again.
      bytearray.length(control.previous_key) == native_tx_field_access_v1.spend_input_item_bytes
    },
    bytearray.length(control.resolution_schedule_hash) == 32,
    witness.work_witness_cbor == encode_input_sets_scan_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.spend_count,
      control.reference_count,
      control.spend_seen,
      control.reference_seen,
      control.previous_key,
      control.resolution_schedule_hash,
    ),
  }
}

pub fn signatures_control_from_witness(
  witness_cbor: ByteArray,
) -> SignaturesControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolution_schedule_hash_data, stage_data, address_count_data,
        required_count_data, address_seen_data, required_seen_data,
        previous_order_key_data, previous_signer_hash_data, signer_count_data,
        signer_peaks_data, invalid_signature_seen_data,
      ]
    = builtin.un_list_data(data)
  SignaturesControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
    stage: builtin.un_i_data(stage_data),
    address_count: builtin.un_i_data(address_count_data),
    required_count: builtin.un_i_data(required_count_data),
    address_seen: builtin.un_i_data(address_seen_data),
    required_seen: builtin.un_i_data(required_seen_data),
    previous_order_key: builtin.un_b_data(previous_order_key_data),
    previous_signer_hash: builtin.un_b_data(previous_signer_hash_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_peaks: decode_frontier_peaks(signer_peaks_data),
    invalid_signature_seen: builtin.un_i_data(invalid_signature_seen_data),
  }
}

pub fn signatures_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: SignaturesControlV1,
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
) -> Bool {
  let address_is_empty = witness_set.addr_tx_wits_hash == native_tx_field_access_v1.empty_field_commitment
  let required_is_empty = verified.tx_compact.body.required_signers_hash == native_tx_field_access_v1.empty_field_commitment
  let address_complete = and {
    control.address_count >= 0,
    control.address_seen == control.address_count,
  }
  let required_complete = and {
    control.required_count >= 0,
    control.required_seen == control.required_count,
  }
  and {
    verified.version == 1,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.resolution_schedule_hash) == 32,
    control.stage >= 0,
    control.stage <= 2,
    control.address_count >= -1,
    control.address_count <= max_tx_size_derived_collection_item_count,
    control.required_count >= -1,
    control.required_count <= max_tx_size_derived_collection_item_count,
    control.address_seen >= 0,
    control.required_seen >= 0,
    if address_is_empty {
      control.address_count == 0 && control.address_seen == 0
    } else if control.address_count == -1 {
      control.address_seen == 0
    } else {
      control.address_count > 0 && control.address_seen <= control.address_count
    },
    if required_is_empty {
      control.required_count == 0 && control.required_seen == 0
    } else if control.required_count == -1 {
      control.required_seen == 0
    } else {
      control.required_count > 0 && control.required_seen <= control.required_count
    },
    control.signer_count >= 0,
    control.signer_count <= control.address_seen,
    validation_merkle_v1.frontier_is_well_formed(
      control.signer_count,
      control.signer_peaks,
    ),
    control.invalid_signature_seen == 0 || control.invalid_signature_seen == 1,
    control.invalid_signature_seen <= control.address_seen,
    if control.stage == 0 {
      and {
        control.required_seen == 0,
        if control.address_seen == 0 {
          and {
            control.previous_order_key == #"",
            control.previous_signer_hash == #"",
            control.signer_count == 0,
          }
        } else {
          and {
            bytearray.length(control.previous_order_key) > 0,
            bytearray.length(control.previous_signer_hash) == 28,
            control.signer_count > 0,
          }
        },
      }
    } else {
      and {
        address_complete,
        control.previous_order_key == #"",
        control.previous_signer_hash == #"",
        if control.stage == 2 {
          required_complete
        } else {
          True
        },
      }
    },
    witness.work_witness_cbor == encode_signatures_scan_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolution_schedule_hash,
      control.stage,
      control.address_count,
      control.required_count,
      control.address_seen,
      control.required_seen,
      control.previous_order_key,
      control.previous_signer_hash,
      control.signer_count,
      control.signer_peaks,
      control.invalid_signature_seen,
    ),
  }
}

pub fn phase_a_native_control_from_witness(
  witness_cbor: ByteArray,
) -> PhaseANativeScriptsControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolution_schedule_hash_data, stage_data, script_count_data,
        script_seen_data, contains_non_native_script_data, item_length_data,
        item_commitment_data, cursor_data, stack_root_data, stack_depth_data,
        node_count_data, result_data, signer_count_data,
        signer_continuation_data,
      ]
    = builtin.un_list_data(data)
  expect [signer_peaks_data,
        continuation_cbor_data] = builtin.un_list_data(signer_continuation_data)
  PhaseANativeScriptsControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
    stage: builtin.un_i_data(stage_data),
    script_count: builtin.un_i_data(script_count_data),
    script_seen: builtin.un_i_data(script_seen_data),
    contains_non_native_script: builtin.un_i_data(
      contains_non_native_script_data,
    ),
    item_length: builtin.un_i_data(item_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    cursor: builtin.un_i_data(cursor_data),
    stack_root: builtin.un_b_data(stack_root_data),
    stack_depth: builtin.un_i_data(stack_depth_data),
    node_count: builtin.un_i_data(node_count_data),
    result: builtin.un_i_data(result_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_peaks: decode_frontier_peaks(signer_peaks_data),
    continuation_cbor: builtin.un_b_data(continuation_cbor_data),
  }
}

pub fn phase_a_native_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
) -> Bool {
  let scripts_are_empty = witness_set.script_tx_wits_hash == native_tx_field_access_v1.empty_field_commitment
  let is_late_continuation = control.continuation_cbor != #""
  and {
    verified.version == 1,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.resolution_schedule_hash) == 32,
    control.stage >= 0,
    control.stage <= 8,
    control.script_count >= -1,
    control.script_count <= max_tx_size_derived_collection_item_count,
    control.script_seen >= 0,
    if is_late_continuation {
      let native_control = native_scripts_control_from_witness(
        control.continuation_cbor,
      )
      and {
        control.resolution_schedule_hash == blake2b_256(
          control.continuation_cbor,
        ),
        control.stage > 0,
        control.script_count == 1,
        control.script_seen == 0,
        control.contains_non_native_script == 0,
        native_scripts_control_is_well_formed(native_control),
        control.continuation_cbor == encode_native_scripts_control_v1(
          native_control,
        ),
        native_control.compact_cbor == control.compact_cbor,
        native_control.witness_set_compact_cbor == control.witness_set_compact_cbor,
        native_control.field_preimage_lengths_cbor == control.field_preimage_lengths_cbor,
        native_control.context_cbor == control.context_cbor,
        native_control.execution_cursor < native_control.execution_count,
        native_control.signer_count == control.signer_count,
        native_control.signer_frontier_commitment == validation_merkle_v1.frontier_commitment(
          control.signer_count,
          control.signer_peaks,
        ),
      }
    } else if scripts_are_empty {
      control.script_count == 0 && control.script_seen == 0
    } else if control.script_count == -1 {
      control.script_seen == 0
    } else {
      control.script_count > 0 && control.script_seen <= control.script_count
    },
    control.contains_non_native_script == 0 || control.contains_non_native_script == 1,
    control.signer_count >= 0,
    control.signer_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.signer_count,
      control.signer_peaks,
    ),
    control.item_length >= 0,
    control.item_length <= max_aggregate_field_preimage_bytes,
    control.cursor >= 0,
    control.cursor <= control.item_length,
    control.stack_depth >= 0,
    control.stack_depth <= native_script_scan_v1.max_native_script_depth,
    control.node_count >= 0,
    control.node_count <= native_script_scan_v1.max_native_script_nodes,
    control.result >= -1,
    control.result <= 1,
    if control.stage == 0 {
      and {
        !is_late_continuation,
        control.item_length == 0,
        control.item_commitment == #"",
        control.cursor == 0,
        control.stack_root == #"",
        control.stack_depth == 0,
        control.node_count == 0,
        control.result == -1,
      }
    } else {
      and {
        control.script_count > 0,
        control.script_seen < control.script_count,
        control.item_length > 0,
        bytearray.length(control.item_commitment) == 32,
        if control.stack_root == #"" {
          control.stack_depth == 0
        } else {
          and {
            bytearray.length(control.stack_root) == 32,
            control.stack_depth > 0,
          }
        },
        if control.stage == 2 {
          control.result >= 0
        } else {
          control.result == -1
        },
      }
    },
    witness.work_witness_cbor == encode_phase_a_native_scripts_scan_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolution_schedule_hash,
      control.stage,
      control.script_count,
      control.script_seen,
      control.contains_non_native_script,
      control.item_length,
      control.item_commitment,
      control.cursor,
      control.stack_root,
      control.stack_depth,
      control.node_count,
      control.result,
      control.signer_count,
      control.signer_peaks,
      control.continuation_cbor,
    ),
  }
}

pub fn reset_phase_a_native_control(
  control: PhaseANativeScriptsControlV1,
  script_count: Int,
  script_seen: Int,
  contains_non_native_script: Int,
) -> PhaseANativeScriptsControlV1 {
  PhaseANativeScriptsControlV1 {
    ..control,
    stage: 0,
    script_count,
    script_seen,
    contains_non_native_script,
    item_length: 0,
    item_commitment: #"",
    cursor: 0,
    stack_root: #"",
    stack_depth: 0,
    node_count: 0,
    result: -1,
  }
}

pub fn phase_a_script_preconditions_control_from_witness(
  witness_cbor: ByteArray,
) -> PhaseAScriptPreconditionsControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolution_schedule_hash_data, signer_count_data,
        signer_frontier_commitment_data, contains_non_native_script_data,
        previous_observer_data, observer_count_data, observer_seen_data,
      ]
    = builtin.un_list_data(data)
  PhaseAScriptPreconditionsControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_frontier_commitment: builtin.un_b_data(
      signer_frontier_commitment_data,
    ),
    contains_non_native_script: builtin.un_i_data(
      contains_non_native_script_data,
    ),
    observer_count: builtin.un_i_data(observer_count_data),
    observer_seen: builtin.un_i_data(observer_seen_data),
    previous_observer: builtin.un_b_data(previous_observer_data),
  }
}

pub fn phase_a_script_preconditions_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseAScriptPreconditionsControlV1,
  observer_commitment: ByteArray,
) -> Bool {
  let observers_are_empty = observer_commitment == native_tx_field_access_v1.empty_field_commitment
  and {
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.resolution_schedule_hash) == 32,
    control.signer_count >= 0,
    bytearray.length(control.signer_frontier_commitment) == 32,
    control.contains_non_native_script == 0 || control.contains_non_native_script == 1,
    control.observer_count >= 0,
    control.observer_count <= max_tx_size_derived_collection_item_count,
    control.observer_seen >= 0,
    if observers_are_empty {
      and {
        control.observer_count == 0,
        control.observer_seen == 0,
        control.previous_observer == #"",
      }
    } else if control.observer_count == 0 {
      and {
        control.observer_seen == 0,
        control.previous_observer == #"",
      }
    } else {
      and {
        control.observer_count > 0,
        control.observer_seen <= control.observer_count,
        if control.observer_seen == 0 {
          control.previous_observer == #""
        } else {
          bytearray.length(control.previous_observer) == 28
        },
      }
    },
    witness.work_witness_cbor == encode_phase_a_script_preconditions_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolution_schedule_hash,
      control.signer_count,
      control.signer_frontier_commitment,
      control.contains_non_native_script == 1,
      control.observer_count,
      control.observer_seen,
      control.previous_observer,
    ),
  }
}

pub fn resolve_inputs_control_from_witness(
  witness_cbor: ByteArray,
) -> ResolveInputsControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data, cursor_data,
        accumulator_data, remaining_schedule_hash_data, signer_count_data,
        signer_frontier_commitment_data, pending_cbor_data,
        resolution_schedule_hash_data,
      ]
    = builtin.un_list_data(data)
  ResolveInputsControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    cursor: builtin.un_i_data(cursor_data),
    accumulator: builtin.un_b_data(accumulator_data),
    remaining_schedule_hash: builtin.un_b_data(remaining_schedule_hash_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_frontier_commitment: builtin.un_b_data(
      signer_frontier_commitment_data,
    ),
    pending: if builtin.un_b_data(pending_cbor_data) == #"00" {
      None
    } else {
      Some(
        pending_cbor_data
          |> builtin.un_b_data
          |> resolve_input_output_proof_from_cbor,
      )
    },
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
  }
}

pub fn resolve_inputs_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
) -> Bool {
  let pending_is_well_formed = when control.pending is {
    None -> True
    Some(pending) -> {
      let descriptor = ledger_output_commitment_v1.decode(
        pending.descriptor_cbor,
      )
      let input = decode_midgard_tx_input_cbor(pending.key)
      and {
        control.cursor > 0,
        pending.source_kind == 0 || pending.source_kind == 1,
        resolution_schedule_node_hash(
          pending.source_kind,
          pending.key,
          pending.next_schedule_hash,
        ) == control.remaining_schedule_hash,
        input.output_index == descriptor.output_index,
        ledger_output_proof_v1.control_is_well_formed(pending.output_proof),
        pending.output_proof.output_index == descriptor.output_index,
        pending.output_proof.total_length == descriptor.total_length,
        pending.output_proof.item_commitment == descriptor.item_commitment,
      }
    }
  }
  and {
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.accumulator) == 32,
    bytearray.length(control.remaining_schedule_hash) == 32,
    bytearray.length(control.resolution_schedule_hash) == 32,
    if control.cursor <= 1 {
      control.remaining_schedule_hash == control.resolution_schedule_hash
    } else {
      True
    },
    control.signer_count >= 0,
    bytearray.length(control.signer_frontier_commitment) == 32,
    control.cursor >= 0,
    pending_is_well_formed,
    witness.work_witness_cbor == encode_resolve_inputs_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.cursor,
      control.accumulator,
      control.remaining_schedule_hash,
      control.signer_count,
      control.signer_frontier_commitment,
      control.pending,
      control.resolution_schedule_hash,
    ),
  }
}

pub fn empty_mint_fold_control() -> MintFoldControlV1 {
  MintFoldControlV1 {
    policy_count: -1,
    policy_cursor: 0,
    previous_policy: #"",
    active_policy: #"",
    item_length: 0,
    item_commitment: #"",
    item_cursor: 0,
    assets_remaining: 0,
    policy_asset_cursor: 0,
    previous_asset: #"",
    asset_count: 0,
    asset_peaks: validation_merkle_v1.empty_frontier(),
  }
}

pub fn encode_mint_fold_control(control: MintFoldControlV1) -> ByteArray {
  encode_definite_array_header(12)
    |> bytearray.concat(cbor.serialise(control.policy_count))
    |> bytearray.concat(cbor.serialise(control.policy_cursor))
    |> bytearray.concat(encode_definite_bytes(control.previous_policy))
    |> bytearray.concat(encode_definite_bytes(control.active_policy))
    |> bytearray.concat(cbor.serialise(control.item_length))
    |> bytearray.concat(encode_definite_bytes(control.item_commitment))
    |> bytearray.concat(cbor.serialise(control.item_cursor))
    |> bytearray.concat(cbor.serialise(control.assets_remaining))
    |> bytearray.concat(cbor.serialise(control.policy_asset_cursor))
    |> bytearray.concat(encode_definite_bytes(control.previous_asset))
    |> bytearray.concat(cbor.serialise(control.asset_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.asset_peaks),
      )
}

pub fn empty_script_discovery_control() -> ScriptDiscoveryControlV1 {
  ScriptDiscoveryControlV1 {
    purpose_cursor: 0,
    source_cursor: 0,
    redeemer_cursor: 0,
    current_purpose_kind: -1,
    current_purpose_index: -1,
    current_script_hash: #"",
    current_subject: #"",
    matched_source_index: -1,
    matched_language_tag: -1,
    matched_source_leaf: #"",
    used_inline_bitmap: 0,
    used_redeemer_bitmap: 0,
    redeemer_item_control_hash: #"",
    execution_count: 0,
    execution_peaks: validation_merkle_v1.empty_frontier(),
  }
}

pub fn encode_script_discovery_control(
  control: ScriptDiscoveryControlV1,
) -> ByteArray {
  encode_definite_array_header(15)
    |> bytearray.concat(cbor.serialise(control.purpose_cursor))
    |> bytearray.concat(cbor.serialise(control.source_cursor))
    |> bytearray.concat(cbor.serialise(control.redeemer_cursor))
    |> bytearray.concat(cbor.serialise(control.current_purpose_kind))
    |> bytearray.concat(cbor.serialise(control.current_purpose_index))
    |> bytearray.concat(encode_definite_bytes(control.current_script_hash))
    |> bytearray.concat(encode_definite_bytes(control.current_subject))
    |> bytearray.concat(cbor.serialise(control.matched_source_index))
    |> bytearray.concat(cbor.serialise(control.matched_language_tag))
    |> bytearray.concat(encode_definite_bytes(control.matched_source_leaf))
    |> bytearray.concat(cbor.serialise(control.used_inline_bitmap))
    |> bytearray.concat(cbor.serialise(control.used_redeemer_bitmap))
    |> bytearray.concat(
        encode_definite_bytes(control.redeemer_item_control_hash),
      )
    |> bytearray.concat(cbor.serialise(control.execution_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.execution_peaks),
      )
}

fn script_discovery_control_from_cbor(
  discovery_cbor: ByteArray,
) -> ScriptDiscoveryControlV1 {
  expect Some(data) = cbor.deserialise(discovery_cbor)
  expect
      [
        purpose_cursor_data, source_cursor_data, redeemer_cursor_data,
        current_purpose_kind_data, current_purpose_index_data,
        current_script_hash_data, current_subject_data,
        matched_source_index_data, matched_language_tag_data,
        matched_source_leaf_data, used_inline_bitmap_data,
        used_redeemer_bitmap_data, redeemer_item_control_hash_data,
        execution_count_data, execution_peaks_data,
      ]
    = builtin.un_list_data(data)
  ScriptDiscoveryControlV1 {
    purpose_cursor: builtin.un_i_data(purpose_cursor_data),
    source_cursor: builtin.un_i_data(source_cursor_data),
    redeemer_cursor: builtin.un_i_data(redeemer_cursor_data),
    current_purpose_kind: builtin.un_i_data(current_purpose_kind_data),
    current_purpose_index: builtin.un_i_data(current_purpose_index_data),
    current_script_hash: builtin.un_b_data(current_script_hash_data),
    current_subject: builtin.un_b_data(current_subject_data),
    matched_source_index: builtin.un_i_data(matched_source_index_data),
    matched_language_tag: builtin.un_i_data(matched_language_tag_data),
    matched_source_leaf: builtin.un_b_data(matched_source_leaf_data),
    used_inline_bitmap: builtin.un_i_data(used_inline_bitmap_data),
    used_redeemer_bitmap: builtin.un_i_data(used_redeemer_bitmap_data),
    redeemer_item_control_hash: builtin.un_b_data(
      redeemer_item_control_hash_data,
    ),
    execution_count: builtin.un_i_data(execution_count_data),
    execution_peaks: decode_frontier_peaks(execution_peaks_data),
  }
}

fn decode_frontier_peak_items(
  items: List<Data>,
) -> List<validation_merkle_v1.FrontierPeak> {
  when items is {
    [] -> []
    [item, ..rest] -> {
      expect [height_data, hash_data] = builtin.un_list_data(item)
      [
        validation_merkle_v1.FrontierPeak {
          height: builtin.un_i_data(height_data),
          hash: builtin.un_b_data(hash_data),
        },
        ..decode_frontier_peak_items(rest)
      ]
    }
  }
}

pub fn decode_frontier_peaks(
  data: Data,
) -> List<validation_merkle_v1.FrontierPeak> {
  data |> builtin.un_list_data |> decode_frontier_peak_items
}

fn split_last_data(items: List<Data>) -> Pair<List<Data>, Data> {
  expect [first, ..rest] = items
  when rest is {
    [last] -> Pair([first], last)
    _ -> {
      let Pair(prefix, last) = split_last_data(rest)
      Pair([first, ..prefix], last)
    }
  }
}

fn mint_fold_control_from_data(data: Data) -> MintFoldControlV1 {
  expect
      [
        policy_count_data, policy_cursor_data, previous_policy_data,
        active_policy_data, item_length_data, item_commitment_data,
        item_cursor_data, assets_remaining_data, policy_asset_cursor_data,
        previous_asset_data, asset_count_data, asset_peaks_data,
      ]
    = builtin.un_list_data(data)
  MintFoldControlV1 {
    policy_count: builtin.un_i_data(policy_count_data),
    policy_cursor: builtin.un_i_data(policy_cursor_data),
    previous_policy: builtin.un_b_data(previous_policy_data),
    active_policy: builtin.un_b_data(active_policy_data),
    item_length: builtin.un_i_data(item_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    item_cursor: builtin.un_i_data(item_cursor_data),
    assets_remaining: builtin.un_i_data(assets_remaining_data),
    policy_asset_cursor: builtin.un_i_data(policy_asset_cursor_data),
    previous_asset: builtin.un_b_data(previous_asset_data),
    asset_count: builtin.un_i_data(asset_count_data),
    asset_peaks: decode_frontier_peaks(asset_peaks_data),
  }
}

fn script_sources_control_from_data_items(
  items: List<Data>,
  discovery: ScriptDiscoveryControlV1,
  output_proof: Option<ledger_output_proof_v1.LedgerOutputProofControlV1>,
) -> ScriptSourcesControlV1 {
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolved_input_count_data, resolved_inputs_accumulator_data,
        signer_count_data, signer_frontier_commitment_data,
        resolved_item_peaks_data, stage_data, source_count_data,
        source_peaks_data, redeemer_count_data, redeemer_peaks_data,
        replay_cursor_data, replay_accumulator_data,
        replay_remaining_schedule_hash_data, spend_index_data,
        purpose_count_data, purpose_peaks_data, output_cursor_data,
        output_count_data, output_peaks_data, output_total_count_data,
        receive_scan_data, source_total_count_data, redeemer_total_count_data,
        observer_scan_data, mint_fold_data, resolution_schedule_hash_data,
      ]
    = items
  expect
      [
        observer_total_count_data, observer_previous_hash_data,
        observer_seen_data,
      ]
    = builtin.un_list_data(observer_scan_data)
  expect
      [
        receive_source_count_data, receive_source_peaks_data, receive_count_data,
        receive_previous_hash_data, receive_candidate_hash_data,
        receive_descriptor_peaks_data,
      ]
    = builtin.un_list_data(receive_scan_data)
  expect
    bytearray.length(builtin.un_b_data(resolution_schedule_hash_data)) == 32
  ScriptSourcesControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    resolved_input_count: builtin.un_i_data(resolved_input_count_data),
    resolved_inputs_accumulator: builtin.un_b_data(
      resolved_inputs_accumulator_data,
    ),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_frontier_commitment: builtin.un_b_data(
      signer_frontier_commitment_data,
    ),
    resolved_item_peaks: decode_frontier_peaks(resolved_item_peaks_data),
    stage: builtin.un_i_data(stage_data),
    source_count: builtin.un_i_data(source_count_data),
    source_peaks: decode_frontier_peaks(source_peaks_data),
    redeemer_count: builtin.un_i_data(redeemer_count_data),
    redeemer_peaks: decode_frontier_peaks(redeemer_peaks_data),
    replay_cursor: builtin.un_i_data(replay_cursor_data),
    replay_accumulator: builtin.un_b_data(replay_accumulator_data),
    replay_remaining_schedule_hash: builtin.un_b_data(
      replay_remaining_schedule_hash_data,
    ),
    spend_index: builtin.un_i_data(spend_index_data),
    purpose_count: builtin.un_i_data(purpose_count_data),
    purpose_peaks: decode_frontier_peaks(purpose_peaks_data),
    output_cursor: builtin.un_i_data(output_cursor_data),
    output_count: builtin.un_i_data(output_count_data),
    output_peaks: decode_frontier_peaks(output_peaks_data),
    output_total_count: builtin.un_i_data(output_total_count_data),
    receive_scan: ReceivePurposeScanControlV1 {
      source_count: builtin.un_i_data(receive_source_count_data),
      source_peaks: decode_frontier_peaks(receive_source_peaks_data),
      receive_count: builtin.un_i_data(receive_count_data),
      previous_hash: builtin.un_b_data(receive_previous_hash_data),
      candidate_hash: builtin.un_b_data(receive_candidate_hash_data),
      descriptor_peaks: decode_frontier_peaks(receive_descriptor_peaks_data),
    },
    source_total_count: builtin.un_i_data(source_total_count_data),
    redeemer_total_count: builtin.un_i_data(redeemer_total_count_data),
    observer_scan: ObserverPurposeScanControlV1 {
      total_count: builtin.un_i_data(observer_total_count_data),
      seen: builtin.un_i_data(observer_seen_data),
      previous_hash: builtin.un_b_data(observer_previous_hash_data),
    },
    discovery,
    output_proof,
    pending_source_cbor: #"",
    mint_fold: mint_fold_control_from_data(mint_fold_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
  }
}

pub fn script_sources_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  if list.length(items) == 30 {
    script_sources_control_from_data_items(
      items,
      empty_script_discovery_control(),
      None,
    )
  } else {
    expect list.length(items) == 31
    let Pair(base_items, extension_data) = split_last_data(items)
    let base_control = script_sources_control_from_data_items(
      base_items,
      empty_script_discovery_control(),
      None,
    )
    if base_control.stage == 0 {
      ScriptSourcesControlV1 {
        ..base_control,
        pending_source_cbor: builtin.un_b_data(extension_data),
      }
    } else if base_control.stage == 1 {
      ScriptSourcesControlV1 {
        ..base_control,
        pending_source_cbor: builtin.un_b_data(extension_data),
      }
    } else if base_control.stage == 5 {
      ScriptSourcesControlV1 {
        ..base_control,
        output_proof: Some(
          extension_data
            |> builtin.un_b_data
            |> ledger_output_proof_v1.decode_control_v1,
        ),
      }
    } else {
      expect base_control.stage >= 8
      ScriptSourcesControlV1 {
        ..base_control,
        discovery: extension_data
          |> builtin.un_b_data
          |> script_discovery_control_from_cbor,
      }
    }
  }
}

pub fn encode_inline_source_hash_control_v1(
  control: InlineSourceHashControlV1,
) -> ByteArray {
  expect control.version == 1
  expect control.source_index >= 0
  expect control.source_total_count > control.source_index
  expect or {
      control.language_tag == 0,
      control.language_tag == 3,
      control.language_tag == 128,
    }
  expect control.payload_offset > 0
  expect control.payload_length >= 0
  expect control.item_length == control.payload_offset + control.payload_length
  expect control.item_length > 0
  expect control.item_length <= max_aggregate_field_preimage_bytes
  expect bytearray.length(control.item_commitment) == 32
  expect blake2b_224_trace_v1.control_is_well_formed(control.hash_control)
  #"89"
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(cbor.serialise(control.source_index))
    |> bytearray.concat(cbor.serialise(control.source_total_count))
    |> bytearray.concat(cbor.serialise(control.language_tag))
    |> bytearray.concat(cbor.serialise(control.payload_offset))
    |> bytearray.concat(cbor.serialise(control.payload_length))
    |> bytearray.concat(cbor.serialise(control.item_length))
    |> bytearray.concat(encode_definite_bytes(control.item_commitment))
    |> bytearray.concat(
        encode_definite_bytes(
          blake2b_224_trace_v1.encode_control_v1(control.hash_control),
        ),
      )
}

pub fn inline_source_hash_control_from_cbor(
  control_cbor: ByteArray,
) -> InlineSourceHashControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  expect
      [
        version_data, source_index_data, source_total_count_data,
        language_tag_data, payload_offset_data, payload_length_data,
        item_length_data, item_commitment_data, hash_control_data,
      ]
    = builtin.un_list_data(data)
  let control = InlineSourceHashControlV1 {
    version: builtin.un_i_data(version_data),
    source_index: builtin.un_i_data(source_index_data),
    source_total_count: builtin.un_i_data(source_total_count_data),
    language_tag: builtin.un_i_data(language_tag_data),
    payload_offset: builtin.un_i_data(payload_offset_data),
    payload_length: builtin.un_i_data(payload_length_data),
    item_length: builtin.un_i_data(item_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    hash_control: builtin.un_b_data(hash_control_data)
      |> blake2b_224_trace_v1.decode_control_v1,
  }
  expect encode_inline_source_hash_control_v1(control) == control_cbor
  control
}

pub fn encode_script_sources_pending_source_witness(
  control: ScriptSourcesControlV1,
  pending_source_cbor: ByteArray,
) -> ByteArray {
  expect control.stage == 0
  expect pending_source_cbor != #""
  let base = encode_script_sources_witness(
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
  encode_definite_array_header(31)
    |> bytearray.concat(bytearray.slice(base, 2, bytearray.length(base) - 1))
    |> bytearray.concat(encode_definite_bytes(pending_source_cbor))
}

pub fn encode_script_sources_output_proof_witness(
  control: ScriptSourcesControlV1,
  output_proof: ledger_output_proof_v1.LedgerOutputProofControlV1,
) -> ByteArray {
  expect control.stage == 5
  let base = encode_script_sources_witness(
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
  encode_definite_array_header(31)
    |> bytearray.concat(bytearray.slice(base, 2, bytearray.length(base) - 1))
    |> bytearray.concat(
        encode_definite_bytes(
          ledger_output_proof_v1.encode_control_v1(output_proof),
        ),
      )
}

pub fn encode_script_sources_redeemer_item_witness(
  control: ScriptSourcesControlV1,
  redeemer_item_control_hash: ByteArray,
) -> ByteArray {
  expect control.stage == 1
  expect bytearray.length(redeemer_item_control_hash) == 32
  let base = encode_script_sources_witness(
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
  encode_definite_array_header(31)
    |> bytearray.concat(bytearray.slice(base, 2, bytearray.length(base) - 1))
    |> bytearray.concat(encode_definite_bytes(redeemer_item_control_hash))
}

pub fn encode_script_sources_discovery_witness(
  control: ScriptSourcesControlV1,
  stage: Int,
  discovery: ScriptDiscoveryControlV1,
) -> ByteArray {
  expect stage >= 8
  let base = encode_script_sources_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.resolved_input_count,
    control.resolved_inputs_accumulator,
    control.signer_count,
    control.signer_frontier_commitment,
    control.resolved_item_peaks,
    stage,
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
  encode_definite_array_header(31)
    |> bytearray.concat(bytearray.slice(base, 2, bytearray.length(base) - 1))
    |> bytearray.concat(
        encode_definite_bytes(encode_script_discovery_control(discovery)),
      )
}

fn script_discovery_control_is_well_formed(
  control: ScriptDiscoveryControlV1,
) -> Bool {
  and {
    control.purpose_cursor >= 0,
    control.source_cursor >= 0,
    control.redeemer_cursor >= 0,
    control.current_purpose_kind >= -1,
    control.current_purpose_kind <= 3,
    control.current_purpose_index >= -1,
    or {
      control.current_script_hash == #"",
      bytearray.length(control.current_script_hash) == 28,
    },
    control.matched_source_index >= -1,
    or {
      control.matched_language_tag == -1,
      control.matched_language_tag == 0,
      control.matched_language_tag == 3,
      control.matched_language_tag == 128,
    },
    or {
      control.matched_source_leaf == #"",
      bytearray.length(control.matched_source_leaf) == 32,
    },
    control.used_inline_bitmap >= 0,
    control.used_redeemer_bitmap >= 0,
    validation_merkle_v1.frontier_is_well_formed(
      control.execution_count,
      control.execution_peaks,
    ),
    bytearray.length(control.redeemer_item_control_hash) == 0 || bytearray.length(
      control.redeemer_item_control_hash,
    ) == 32,
  }
}

pub fn exact_script_sources_control(
  control: ScriptSourcesControlV1,
) -> ByteArray {
  if control.stage == 0 && control.pending_source_cbor != #"" {
    encode_script_sources_pending_source_witness(
      control,
      control.pending_source_cbor,
    )
  } else if control.stage == 1 && control.pending_source_cbor != #"" {
    encode_script_sources_redeemer_item_witness(
      control,
      control.pending_source_cbor,
    )
  } else if control.stage == 5 {
    when control.output_proof is {
      None ->
        encode_script_sources_witness(
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
      Some(output_proof) ->
        encode_script_sources_output_proof_witness(control, output_proof)
    }
  } else if control.stage < 8 {
    expect control.output_proof == None
    encode_script_sources_witness(
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
    expect control.output_proof == None
    encode_script_sources_discovery_witness(
      control,
      control.stage,
      control.discovery,
    )
  }
}

/// Canonical wire encoding for an already-decoded ScriptSources control.
/// Fraud-proof families use this read-only seam to bind a retained control to
/// the committed work root without copying the consensus encoder.
pub fn encode_script_sources_control_v1(
  control: ScriptSourcesControlV1,
) -> ByteArray {
  exact_script_sources_control(control)
}

pub fn script_sources_stage_zero_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  if list.length(items) == 30 {
    script_sources_control_from_data_items(
      items,
      empty_script_discovery_control(),
      None,
    )
  } else {
    expect list.length(items) == 31
    let Pair(base_items, extension_data) = split_last_data(items)
    let base_control = script_sources_control_from_data_items(
      base_items,
      empty_script_discovery_control(),
      None,
    )
    expect base_control.stage == 0
    ScriptSourcesControlV1 {
      ..base_control,
      pending_source_cbor: builtin.un_b_data(extension_data),
    }
  }
}

fn observer_purpose_scan_is_well_formed(
  stage: Int,
  scan: ObserverPurposeScanControlV1,
) -> Bool {
  and {
    scan.total_count >= 0,
    scan.seen >= 0,
    scan.seen <= scan.total_count,
    if scan.seen == 0 {
      scan.previous_hash == #""
    } else {
      bytearray.length(scan.previous_hash) == 28
    },
    if stage == 7 {
      True
    } else {
      and {
        scan.total_count == 0,
        scan.seen == 0,
        scan.previous_hash == #"",
      }
    },
  }
}

fn receive_purpose_scan_is_well_formed(
  stage: Int,
  output_cursor: Int,
  output_count: Int,
  scan: ReceivePurposeScanControlV1,
) -> Bool {
  let descriptor_count = if stage < 5 {
    0
  } else if stage == 5 {
    output_cursor
  } else {
    output_count
  }
  and {
    validation_merkle_v1.frontier_is_well_formed(
      descriptor_count,
      scan.descriptor_peaks,
    ),
    scan.source_count >= 0,
    scan.source_count <= output_count,
    validation_merkle_v1.frontier_is_well_formed(
      scan.source_count,
      scan.source_peaks,
    ),
    scan.receive_count >= 0,
    scan.receive_count <= scan.source_count,
    if scan.receive_count == 0 {
      scan.previous_hash == #""
    } else {
      bytearray.length(scan.previous_hash) == 28
    },
    or {
      scan.candidate_hash == #"",
      and {
        bytearray.length(scan.candidate_hash) == 28,
        or {
          scan.previous_hash == #"",
          bytearray.compare(scan.previous_hash, scan.candidate_hash) == Less,
        },
      },
    },
    if stage < 5 {
      scan == empty_receive_purpose_scan_control()
    } else if stage < 7 {
      and {
        scan.receive_count == 0,
        scan.previous_hash == #"",
        scan.candidate_hash == #"",
      }
    } else if stage == 7 {
      True
    } else {
      and {
        scan.source_count == 0,
        scan.source_peaks == [],
        scan.receive_count == 0,
        scan.previous_hash == #"",
        scan.candidate_hash == #"",
      }
    },
  }
}

pub fn script_sources_stage_seven_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 30
  let control = script_sources_control_from_data_items(
    items,
    empty_script_discovery_control(),
    None,
  )
  expect control.stage == 7
  control
}

pub fn reset_script_discovery_current(
  control: ScriptDiscoveryControlV1,
) -> ScriptDiscoveryControlV1 {
  ScriptDiscoveryControlV1 {
    ..control,
    source_cursor: 0,
    redeemer_cursor: 0,
    current_purpose_kind: -1,
    current_purpose_index: -1,
    current_script_hash: #"",
    current_subject: #"",
    matched_source_index: -1,
    matched_language_tag: -1,
    matched_source_leaf: #"",
    redeemer_item_control_hash: #"",
  }
}

pub fn script_sources_stage_eight_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 31
  let Pair(base_items, extension_data) = split_last_data(items)
  let base_control = script_sources_control_from_data_items(
    base_items,
    empty_script_discovery_control(),
    None,
  )
  expect base_control.stage == 8
  ScriptSourcesControlV1 {
    ..base_control,
    discovery: extension_data
      |> builtin.un_b_data
      |> script_discovery_control_from_cbor,
  }
}

pub fn script_sources_stage_nine_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 31
  let Pair(base_items, extension_data) = split_last_data(items)
  let base_control = script_sources_control_from_data_items(
    base_items,
    empty_script_discovery_control(),
    None,
  )
  expect base_control.stage == 9
  ScriptSourcesControlV1 {
    ..base_control,
    discovery: extension_data
      |> builtin.un_b_data
      |> script_discovery_control_from_cbor,
  }
}

pub fn script_sources_stage_ten_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 31
  let Pair(base_items, extension_data) = split_last_data(items)
  let base_control = script_sources_control_from_data_items(
    base_items,
    empty_script_discovery_control(),
    None,
  )
  expect base_control.stage == 10
  ScriptSourcesControlV1 {
    ..base_control,
    discovery: extension_data
      |> builtin.un_b_data
      |> script_discovery_control_from_cbor,
  }
}

pub fn encode_native_scripts_witness_v1(
  control: ScriptSourcesControlV1,
  execution_cursor: Int,
  language_bitmap: Int,
) -> ByteArray {
  expect execution_cursor >= 0
  expect execution_cursor <= control.discovery.execution_count
  expect language_bitmap >= 0
  expect language_bitmap <= 3
  encode_definite_array_header(26)
    |> bytearray.concat(encode_definite_bytes(control.compact_cbor))
    |> bytearray.concat(encode_definite_bytes(control.witness_set_compact_cbor))
    |> bytearray.concat(
        encode_definite_bytes(control.field_preimage_lengths_cbor),
      )
    |> bytearray.concat(encode_definite_bytes(control.context_cbor))
    |> bytearray.concat(cbor.serialise(control.resolved_input_count))
    |> bytearray.concat(
        encode_definite_bytes(control.resolved_inputs_accumulator),
      )
    |> bytearray.concat(cbor.serialise(control.spend_index))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.resolved_item_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.signer_count))
    |> bytearray.concat(
        encode_definite_bytes(control.signer_frontier_commitment),
      )
    |> bytearray.concat(cbor.serialise(control.source_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.source_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.redeemer_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.redeemer_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.purpose_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.purpose_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.output_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.output_peaks),
      )
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(
          control.receive_scan.descriptor_peaks,
        ),
      )
    |> bytearray.concat(cbor.serialise(control.mint_fold.asset_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.mint_fold.asset_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.discovery.execution_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.discovery.execution_peaks),
      )
    |> bytearray.concat(cbor.serialise(execution_cursor))
    |> bytearray.concat(cbor.serialise(language_bitmap))
    |> bytearray.concat(encode_definite_bytes(control.resolution_schedule_hash))
}

pub fn script_sources_stage_eleven_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 31
  let Pair(base_items, extension_data) = split_last_data(items)
  let base_control = script_sources_control_from_data_items(
    base_items,
    empty_script_discovery_control(),
    None,
  )
  expect base_control.stage == 11
  ScriptSourcesControlV1 {
    ..base_control,
    discovery: extension_data
      |> builtin.un_b_data
      |> script_discovery_control_from_cbor,
  }
}

pub fn script_sources_stage_twelve_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 31
  let Pair(base_items, extension_data) = split_last_data(items)
  let base_control = script_sources_control_from_data_items(
    base_items,
    empty_script_discovery_control(),
    None,
  )
  expect base_control.stage == 12
  ScriptSourcesControlV1 {
    ..base_control,
    discovery: extension_data
      |> builtin.un_b_data
      |> script_discovery_control_from_cbor,
  }
}

fn script_discovery_stage_is_well_formed(
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if control.stage < 8 {
    discovery == empty_script_discovery_control()
  } else {
    and {
      control.stage <= 12,
      discovery.purpose_cursor <= control.purpose_count,
      discovery.source_cursor <= control.source_count,
      discovery.redeemer_cursor <= control.redeemer_count,
      discovery.execution_count == discovery.purpose_cursor,
      discovery.used_inline_bitmap < script_discovery_bit(control.source_count),
      discovery.used_redeemer_bitmap < script_discovery_bit(
        control.redeemer_count,
      ),
      if control.stage == 8 {
        and {
          discovery.redeemer_item_control_hash == #"",
          discovery.current_purpose_kind == -1,
          discovery.current_purpose_index == -1,
          discovery.current_script_hash == #"",
          discovery.current_subject == #"",
          discovery.matched_source_index == -1,
          discovery.matched_language_tag == -1,
          discovery.matched_source_leaf == #"",
        }
      } else if control.stage == 9 {
        and {
          discovery.redeemer_item_control_hash == #"",
          discovery.purpose_cursor < control.purpose_count,
          discovery.current_purpose_kind >= 0,
          discovery.current_purpose_kind <= 3,
          discovery.current_purpose_index >= 0,
          bytearray.length(discovery.current_script_hash) == 28,
          discovery.current_subject != #"",
          discovery.matched_source_index == -1,
          discovery.matched_language_tag == -1,
          discovery.matched_source_leaf == #"",
        }
      } else if control.stage == 10 {
        and {
          discovery.purpose_cursor < control.purpose_count,
          discovery.current_purpose_kind >= 0,
          discovery.current_purpose_kind <= 3,
          discovery.current_purpose_index >= 0,
          bytearray.length(discovery.current_script_hash) == 28,
          discovery.current_subject != #"",
          discovery.matched_source_index >= 0,
          discovery.matched_source_index < control.source_count,
          or {
            discovery.matched_language_tag == 3,
            discovery.matched_language_tag == 128,
          },
          bytearray.length(discovery.matched_source_leaf) == 32,
        }
      } else {
        and {
          if control.stage == 11 {
            discovery.redeemer_item_control_hash == #""
          } else {
            True
          },
          discovery.purpose_cursor == control.purpose_count,
          discovery.current_purpose_kind == -1,
          discovery.current_purpose_index == -1,
          discovery.current_script_hash == #"",
          discovery.current_subject == #"",
          discovery.matched_source_index == -1,
          discovery.matched_language_tag == -1,
          discovery.matched_source_leaf == #"",
        }
      },
    }
  }
}

pub fn mint_fold_active_state_is_empty(control: MintFoldControlV1) -> Bool {
  and {
    control.active_policy == #"",
    control.item_length == 0,
    control.item_commitment == #"",
    control.item_cursor == 0,
    control.assets_remaining == 0,
    control.policy_asset_cursor == 0,
    control.previous_asset == #"",
  }
}

fn mint_fold_control_is_well_formed(
  stage: Int,
  control: MintFoldControlV1,
) -> Bool {
  let common_is_valid = and {
    control.policy_count >= -1,
    control.policy_count <= max_tx_size_derived_collection_item_count,
    control.policy_cursor >= 0,
    control.asset_count >= 0,
    control.asset_count <= ledger_output_v1.max_distinct_asset_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.asset_count,
      control.asset_peaks,
    ),
    if control.policy_cursor == 0 {
      control.previous_policy == #""
    } else {
      bytearray.length(control.previous_policy) == 28
    },
  }
  if stage < 6 {
    control == empty_mint_fold_control()
  } else if stage == 6 {
    and {
      common_is_valid,
      if control.policy_count == -1 {
        control == empty_mint_fold_control()
      } else if control.active_policy == #"" {
        and {
          control.policy_cursor <= control.policy_count,
          mint_fold_active_state_is_empty(control),
        }
      } else {
        and {
          control.policy_cursor < control.policy_count,
          bytearray.length(control.active_policy) == 28,
          control.item_length > 0,
          control.item_length <= max_aggregate_field_preimage_bytes,
          bytearray.length(control.item_commitment) == 32,
          control.item_cursor > 0,
          control.item_cursor < control.item_length,
          control.assets_remaining > 0,
          control.policy_asset_cursor >= 0,
          if control.policy_asset_cursor == 0 {
            control.previous_asset == #""
          } else {
            bytearray.length(control.previous_asset) <= 32
          },
          control.asset_count + control.assets_remaining <= ledger_output_v1.max_distinct_asset_count,
        }
      },
    }
  } else {
    and {
      common_is_valid,
      control.policy_count >= 0,
      control.policy_cursor == control.policy_count,
      mint_fold_active_state_is_empty(control),
    }
  }
}

pub fn script_sources_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  and {
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
    bytearray.length(control.signer_frontier_commitment) == 32,
    control.resolved_input_count >= 0,
    control.signer_count >= 0,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.source_total_count >= control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    control.redeemer_total_count >= control.redeemer_count,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    control.replay_cursor >= 0,
    validation_merkle_v1.frontier_is_well_formed(
      control.replay_cursor,
      control.resolved_item_peaks,
    ),
    control.spend_index >= 0,
    control.spend_index <= control.replay_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    control.output_cursor >= 0,
    control.output_cursor <= control.output_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_peaks,
    ),
    control.output_total_count >= control.output_count,
    control.output_total_count <= max_tx_size_derived_collection_item_count,
    if control.stage == 0 {
      if control.pending_source_cbor == #"" {
        True
      } else {
        let pending = inline_source_hash_control_from_cbor(
          control.pending_source_cbor,
        )
        and {
          pending.source_index == control.source_count,
          control.source_total_count == 0 || pending.source_total_count == control.source_total_count,
        }
      }
    } else if control.stage == 1 {
      bytearray.length(control.pending_source_cbor) == 0 || bytearray.length(
        control.pending_source_cbor,
      ) == 32
    } else {
      control.pending_source_cbor == #""
    },
    if control.stage == 5 {
      when control.output_proof is {
        None -> True
        Some(output_proof) -> and {
            ledger_output_proof_v1.control_is_well_formed(output_proof),
            output_proof.output_index == control.output_cursor,
            output_proof.output_index < control.output_count,
          }
      }
    } else {
      control.output_proof == None
    },
    if control.stage < 4 {
      and {
        control.output_count == 0,
        control.output_total_count == 0,
      }
    } else if control.stage == 4 {
      control.output_cursor == 0
    } else if control.stage == 5 {
      True
    } else if control.stage == 6 {
      control.output_cursor == control.output_count
    } else if control.stage == 7 {
      True
    } else {
      control.output_cursor == control.output_count
    },
    if control.stage == 4 {
      True
    } else {
      control.output_total_count == control.output_count
    },
    receive_purpose_scan_is_well_formed(
      control.stage,
      control.output_cursor,
      control.output_count,
      control.receive_scan,
    ),
    observer_purpose_scan_is_well_formed(control.stage, control.observer_scan),
    mint_fold_control_is_well_formed(control.stage, control.mint_fold),
    script_discovery_control_is_well_formed(control.discovery),
    script_discovery_stage_is_well_formed(control),
    witness.work_witness_cbor == exact_script_sources_control(control),
  }
}

pub fn native_scripts_control_from_witness(
  work_witness_cbor: ByteArray,
) -> NativeScriptsControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolved_input_count_data, resolved_inputs_accumulator_data,
        spend_input_count_data, resolved_item_peaks_data, signer_count_data,
        signer_frontier_commitment_data, source_count_data, source_peaks_data,
        redeemer_count_data, redeemer_peaks_data, purpose_count_data,
        purpose_peaks_data, output_count_data, output_peaks_data,
        output_descriptor_peaks_data, mint_count_data, mint_peaks_data,
        execution_count_data, execution_peaks_data, execution_cursor_data,
        language_bitmap_data, resolution_schedule_hash_data,
      ]
    = builtin.un_list_data(data)
  NativeScriptsControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    resolved_input_count: builtin.un_i_data(resolved_input_count_data),
    resolved_inputs_accumulator: builtin.un_b_data(
      resolved_inputs_accumulator_data,
    ),
    spend_input_count: builtin.un_i_data(spend_input_count_data),
    resolved_item_peaks: decode_frontier_peaks(resolved_item_peaks_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_frontier_commitment: builtin.un_b_data(
      signer_frontier_commitment_data,
    ),
    source_count: builtin.un_i_data(source_count_data),
    source_peaks: decode_frontier_peaks(source_peaks_data),
    redeemer_count: builtin.un_i_data(redeemer_count_data),
    redeemer_peaks: decode_frontier_peaks(redeemer_peaks_data),
    purpose_count: builtin.un_i_data(purpose_count_data),
    purpose_peaks: decode_frontier_peaks(purpose_peaks_data),
    output_count: builtin.un_i_data(output_count_data),
    output_peaks: decode_frontier_peaks(output_peaks_data),
    output_descriptor_peaks: decode_frontier_peaks(output_descriptor_peaks_data),
    mint_count: builtin.un_i_data(mint_count_data),
    mint_peaks: decode_frontier_peaks(mint_peaks_data),
    execution_count: builtin.un_i_data(execution_count_data),
    execution_peaks: decode_frontier_peaks(execution_peaks_data),
    execution_cursor: builtin.un_i_data(execution_cursor_data),
    language_bitmap: builtin.un_i_data(language_bitmap_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
  }
}

pub fn encode_native_scripts_control_v1(
  control: NativeScriptsControlV1,
) -> ByteArray {
  encode_definite_array_header(26)
    |> bytearray.concat(encode_definite_bytes(control.compact_cbor))
    |> bytearray.concat(encode_definite_bytes(control.witness_set_compact_cbor))
    |> bytearray.concat(
        encode_definite_bytes(control.field_preimage_lengths_cbor),
      )
    |> bytearray.concat(encode_definite_bytes(control.context_cbor))
    |> bytearray.concat(cbor.serialise(control.resolved_input_count))
    |> bytearray.concat(
        encode_definite_bytes(control.resolved_inputs_accumulator),
      )
    |> bytearray.concat(cbor.serialise(control.spend_input_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.resolved_item_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.signer_count))
    |> bytearray.concat(
        encode_definite_bytes(control.signer_frontier_commitment),
      )
    |> bytearray.concat(cbor.serialise(control.source_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.source_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.redeemer_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.redeemer_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.purpose_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.purpose_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.output_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.output_peaks),
      )
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.output_descriptor_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.mint_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.mint_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.execution_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.execution_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.execution_cursor))
    |> bytearray.concat(cbor.serialise(control.language_bitmap))
    |> bytearray.concat(encode_definite_bytes(control.resolution_schedule_hash))
}

pub fn native_resolved_context_is_well_formed(
  control: NativeScriptsControlV1,
) -> Bool {
  and {
    control.spend_input_count >= 0,
    control.spend_input_count <= control.resolved_input_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.resolved_input_count,
      control.resolved_item_peaks,
    ),
  }
}

pub fn native_scripts_control_is_well_formed(
  control: NativeScriptsControlV1,
) -> Bool {
  and {
    control.resolved_input_count >= 0,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
    bytearray.length(control.resolution_schedule_hash) == 32,
    native_resolved_context_is_well_formed(control),
    control.signer_count >= 0,
    bytearray.length(control.signer_frontier_commitment) == 32,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_descriptor_peaks,
    ),
    control.mint_count >= 0,
    control.mint_count <= ledger_output_v1.max_distinct_asset_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.mint_count,
      control.mint_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.execution_count,
      control.execution_peaks,
    ),
    control.execution_count == control.purpose_count,
    control.execution_cursor >= 0,
    control.execution_cursor <= control.execution_count,
    control.language_bitmap >= 0,
    control.language_bitmap <= 3,
  }
}

pub fn encode_script_integrity_witness_v1(
  control: NativeScriptsControlV1,
) -> ByteArray {
  #"82"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(control)),
      )
    |> bytearray.concat(cbor.serialise(0))
}

pub fn encode_script_integrity_compact_witness_v1(
  control: NativeScriptsControlV1,
) -> ByteArray {
  #"82"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(control)),
      )
    |> bytearray.concat(cbor.serialise(1))
}

pub fn encode_script_integrity_witness_set_witness_v1(
  control: NativeScriptsControlV1,
  script_integrity_hash: ByteArray,
  witness_set_hash: ByteArray,
) -> ByteArray {
  expect bytearray.length(script_integrity_hash) == 32
  expect bytearray.length(witness_set_hash) == 32
  #"84"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(control)),
      )
    |> bytearray.concat(cbor.serialise(2))
    |> bytearray.concat(encode_definite_bytes(script_integrity_hash))
    |> bytearray.concat(encode_definite_bytes(witness_set_hash))
}

pub fn encode_script_integrity_finalize_witness_v1(
  control: NativeScriptsControlV1,
  script_integrity_hash: ByteArray,
  redeemer_tx_wits_hash: ByteArray,
) -> ByteArray {
  expect bytearray.length(script_integrity_hash) == 32
  expect bytearray.length(redeemer_tx_wits_hash) == 32
  #"84"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(control)),
      )
    |> bytearray.concat(cbor.serialise(3))
    |> bytearray.concat(encode_definite_bytes(script_integrity_hash))
    |> bytearray.concat(encode_definite_bytes(redeemer_tx_wits_hash))
}

fn encode_data_sequence_summary_v1(
  summary: cek_data_v1.DataSequenceSummaryV1,
) -> ByteArray {
  #"84"
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.length))
    |> bytearray.concat(cbor.serialise(summary.payload_cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

fn data_sequence_summary_from_data(
  data: Data,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect [root_data,
        length_data, payload_data, memory_data] = builtin.un_list_data(data)
  cek_data_v1.DataSequenceSummaryV1 {
    root: builtin.un_b_data(root_data),
    length: builtin.un_i_data(length_data),
    payload_cbor_length: builtin.un_i_data(payload_data),
    memory: builtin.un_i_data(memory_data),
  }
}

fn encode_data_summary_v1(summary: cek_data_v1.DataSummaryV1) -> ByteArray {
  #"83"
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

fn data_summary_from_data(data: Data) -> cek_data_v1.DataSummaryV1 {
  expect [root_data, cbor_length_data, memory_data] = builtin.un_list_data(data)
  cek_data_v1.DataSummaryV1 {
    root: builtin.un_b_data(root_data),
    cbor_length: builtin.un_i_data(cbor_length_data),
    memory: builtin.un_i_data(memory_data),
  }
}

pub fn empty_data_summary_v1() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 { root: #"", cbor_length: 0, memory: 0 }
}

pub fn encode_cek_context_control_v1(control: CekContextControlV1) -> ByteArray {
  encode_definite_array_header(25)
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.language_tag))
    |> bytearray.concat(encode_definite_bytes(control.program_term_root))
    |> bytearray.concat(encode_definite_bytes(control.program_envelope_hash))
    |> bytearray.concat(cbor.serialise(control.purpose_kind))
    |> bytearray.concat(cbor.serialise(control.purpose_index))
    |> bytearray.concat(encode_definite_bytes(control.script_hash))
    |> bytearray.concat(encode_definite_bytes(control.subject))
    |> bytearray.concat(encode_definite_bytes(control.redeemer_leaf))
    |> bytearray.concat(
        encode_definite_bytes(control.redeemer_context_control_hash),
      )
    |> bytearray.concat(cbor.serialise(control.execution_memory_limit))
    |> bytearray.concat(cbor.serialise(control.execution_cpu_limit))
    |> bytearray.concat(
        encode_data_sequence_summary_v1(control.reference_items),
      )
    |> bytearray.concat(encode_data_sequence_summary_v1(control.spend_items))
    |> bytearray.concat(encode_data_sequence_summary_v1(control.output_items))
    |> bytearray.concat(encode_data_sequence_summary_v1(control.signer_items))
    |> bytearray.concat(cbor.serialise(control.observer_count))
    |> bytearray.concat(encode_data_sequence_summary_v1(control.observer_items))
    |> bytearray.concat(encode_definite_bytes(control.previous_observer))
    |> bytearray.concat(encode_data_summary_v1(control.observer_summary))
    |> bytearray.concat(cbor.serialise(control.mint_cursor))
    |> bytearray.concat(encode_definite_bytes(control.current_mint_policy))
    |> bytearray.concat(
        encode_data_sequence_summary_v1(control.current_mint_assets),
      )
    |> bytearray.concat(encode_data_sequence_summary_v1(control.mint_policies))
    |> bytearray.concat(encode_data_summary_v1(control.mint_summary))
}

pub fn cek_context_control_from_cbor(
  control_cbor: ByteArray,
) -> CekContextControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  expect
      [
        stage_data, language_tag_data, program_term_root_data,
        program_envelope_hash_data, purpose_kind_data, purpose_index_data,
        script_hash_data, subject_data, redeemer_leaf_data,
        redeemer_context_control_hash_data, execution_memory_limit_data,
        execution_cpu_limit_data, reference_items_data, spend_items_data,
        output_items_data, signer_items_data, observer_count_data,
        observer_items_data, previous_observer_data, observer_summary_data,
        mint_cursor_data, current_mint_policy_data, current_mint_assets_data,
        mint_policies_data, mint_summary_data,
      ]
    = builtin.un_list_data(data)
  CekContextControlV1 {
    stage: builtin.un_i_data(stage_data),
    language_tag: builtin.un_i_data(language_tag_data),
    program_term_root: builtin.un_b_data(program_term_root_data),
    program_envelope_hash: builtin.un_b_data(program_envelope_hash_data),
    purpose_kind: builtin.un_i_data(purpose_kind_data),
    purpose_index: builtin.un_i_data(purpose_index_data),
    script_hash: builtin.un_b_data(script_hash_data),
    subject: builtin.un_b_data(subject_data),
    redeemer_leaf: builtin.un_b_data(redeemer_leaf_data),
    redeemer_context_control_hash: builtin.un_b_data(
      redeemer_context_control_hash_data,
    ),
    execution_memory_limit: builtin.un_i_data(execution_memory_limit_data),
    execution_cpu_limit: builtin.un_i_data(execution_cpu_limit_data),
    reference_items: data_sequence_summary_from_data(reference_items_data),
    spend_items: data_sequence_summary_from_data(spend_items_data),
    output_items: data_sequence_summary_from_data(output_items_data),
    signer_items: data_sequence_summary_from_data(signer_items_data),
    observer_count: builtin.un_i_data(observer_count_data),
    observer_items: data_sequence_summary_from_data(observer_items_data),
    previous_observer: builtin.un_b_data(previous_observer_data),
    observer_summary: data_summary_from_data(observer_summary_data),
    mint_cursor: builtin.un_i_data(mint_cursor_data),
    current_mint_policy: builtin.un_b_data(current_mint_policy_data),
    current_mint_assets: data_sequence_summary_from_data(
      current_mint_assets_data,
    ),
    mint_policies: data_sequence_summary_from_data(mint_policies_data),
    mint_summary: data_summary_from_data(mint_summary_data),
  }
}

fn data_sequence_summary_is_well_formed(
  summary: cek_data_v1.DataSequenceSummaryV1,
) -> Bool {
  and {
    bytearray.length(summary.root) == 32,
    summary.length >= 0,
    summary.payload_cbor_length >= 0,
    summary.memory >= 0,
  }
}

fn optional_data_summary_is_well_formed(
  summary: cek_data_v1.DataSummaryV1,
) -> Bool {
  if summary.root == #"" {
    and {
      summary.cbor_length == 0,
      summary.memory == 0,
    }
  } else {
    and {
      bytearray.length(summary.root) == 32,
      summary.cbor_length >= 0,
      summary.memory >= 0,
    }
  }
}

const cek_redeemer_context_domain =
  #"4d69646761726443656b52656465656d6572436f6e74657874436f6e74726f6c5631"

const cek_final_context_domain =
  #"4d69646761726443656b46696e616c436f6e74657874436f6e74726f6c5631"

const cek_context_parts_domain =
  #"4d69646761726443656b436f6e746578745061727473436f6e74726f6c5631"

const cek_tx_info_assembly_domain =
  #"4d69646761726443656b5478496e666f417373656d626c79436f6e74726f6c5631"

pub fn encode_cek_redeemer_context_control_v1(
  control: CekRedeemerContextControlV1,
) -> ByteArray {
  encode_definite_array_header(6)
    |> bytearray.concat(cbor.serialise(control.cursor))
    |> bytearray.concat(encode_data_sequence_summary_v1(control.map_items))
    |> bytearray.concat(encode_definite_bytes(control.active_scan_hash))
    |> bytearray.concat(encode_definite_bytes(control.active_redeemer_leaf))
    |> bytearray.concat(encode_data_summary_v1(control.active_purpose))
    |> bytearray.concat(encode_data_summary_v1(control.current_redeemer))
}

pub fn hash_cek_redeemer_context_control_v1(
  control: CekRedeemerContextControlV1,
) -> ByteArray {
  cek_redeemer_context_domain
    |> bytearray.concat(encode_cek_redeemer_context_control_v1(control))
    |> blake2b_256
}

pub fn cek_redeemer_context_control_is_well_formed(
  redeemer_count: Int,
  control: CekRedeemerContextControlV1,
) -> Bool {
  and {
    control.cursor >= 0,
    control.cursor <= redeemer_count,
    data_sequence_summary_is_well_formed(control.map_items),
    control.map_items.length <= control.cursor,
    optional_data_summary_is_well_formed(control.active_purpose),
    optional_data_summary_is_well_formed(control.current_redeemer),
    if control.active_scan_hash == #"" {
      and {
        control.active_redeemer_leaf == #"",
        control.active_purpose == empty_data_summary_v1(),
      }
    } else {
      and {
        control.cursor < redeemer_count,
        bytearray.length(control.active_scan_hash) == 32,
        bytearray.length(control.active_redeemer_leaf) == 32,
        control.active_purpose == empty_data_summary_v1() || bytearray.length(
          control.active_purpose.root,
        ) == 32,
      }
    },
  }
}

pub fn initial_cek_redeemer_context_control_v1() -> CekRedeemerContextControlV1 {
  CekRedeemerContextControlV1 {
    cursor: 0,
    map_items: cek_data_v1.empty_data_pair_summary_v1(),
    active_scan_hash: #"",
    active_redeemer_leaf: #"",
    active_purpose: empty_data_summary_v1(),
    current_redeemer: empty_data_summary_v1(),
  }
}

pub fn encode_cek_final_context_control_v1(
  control: CekFinalContextControlV1,
) -> ByteArray {
  encode_definite_array_header(3)
    |> bytearray.concat(encode_data_summary_v1(control.tx_info))
    |> bytearray.concat(encode_data_summary_v1(control.redeemer))
    |> bytearray.concat(encode_data_summary_v1(control.script_info))
}

pub fn hash_cek_final_context_control_v1(
  control: CekFinalContextControlV1,
) -> ByteArray {
  expect optional_data_summary_is_well_formed(control.tx_info)
  expect optional_data_summary_is_well_formed(control.redeemer)
  expect optional_data_summary_is_well_formed(control.script_info)
  expect control.tx_info.root != #""
  expect control.redeemer.root != #""
  expect control.script_info.root != #""
  cek_final_context_domain
    |> bytearray.concat(encode_cek_final_context_control_v1(control))
    |> blake2b_256
}

pub fn encode_cek_context_parts_control_v1(
  control: CekContextPartsControlV1,
) -> ByteArray {
  encode_definite_array_header(3)
    |> bytearray.concat(encode_data_sequence_summary_v1(control.redeemer_items))
    |> bytearray.concat(encode_data_summary_v1(control.redeemer))
    |> bytearray.concat(encode_data_summary_v1(control.script_info))
}

pub fn hash_cek_context_parts_control_v1(
  control: CekContextPartsControlV1,
) -> ByteArray {
  expect data_sequence_summary_is_well_formed(control.redeemer_items)
  expect optional_data_summary_is_well_formed(control.redeemer)
  expect optional_data_summary_is_well_formed(control.script_info)
  expect control.redeemer.root != #""
  expect control.script_info.root != #""
  cek_context_parts_domain
    |> bytearray.concat(encode_cek_context_parts_control_v1(control))
    |> blake2b_256
}

pub fn encode_cek_tx_info_assembly_control_v1(
  control: CekTxInfoAssemblyControlV1,
) -> ByteArray {
  encode_definite_array_header(3)
    |> bytearray.concat(encode_data_sequence_summary_v1(control.tail_fields))
    |> bytearray.concat(encode_data_summary_v1(control.redeemer))
    |> bytearray.concat(encode_data_summary_v1(control.script_info))
}

pub fn hash_cek_tx_info_assembly_control_v1(
  control: CekTxInfoAssemblyControlV1,
) -> ByteArray {
  expect data_sequence_summary_is_well_formed(control.tail_fields)
  expect optional_data_summary_is_well_formed(control.redeemer)
  expect optional_data_summary_is_well_formed(control.script_info)
  expect control.tail_fields.length > 0
  expect control.redeemer.root != #""
  expect control.script_info.root != #""
  cek_tx_info_assembly_domain
    |> bytearray.concat(encode_cek_tx_info_assembly_control_v1(control))
    |> blake2b_256
}

fn mint_context_is_pristine(control: CekContextControlV1) -> Bool {
  and {
    control.mint_cursor == 0,
    control.current_mint_policy == #"",
    control.current_mint_assets == cek_data_v1.empty_data_pair_summary_v1(),
    control.mint_policies == cek_data_v1.empty_data_pair_summary_v1(),
    control.mint_summary == empty_data_summary_v1(),
  }
}

pub fn empty_observer_items_v1(
  language_tag: Int,
) -> cek_data_v1.DataSequenceSummaryV1 {
  if language_tag == 128 {
    cek_data_v1.empty_data_list_summary_v1()
  } else {
    cek_data_v1.empty_data_pair_summary_v1()
  }
}

pub fn observer_context_is_pristine(control: CekContextControlV1) -> Bool {
  and {
    control.observer_count == 0,
    control.observer_items == empty_observer_items_v1(control.language_tag),
    control.previous_observer == #"",
    control.observer_summary == empty_data_summary_v1(),
  }
}

fn observer_context_is_partial(control: CekContextControlV1) -> Bool {
  and {
    control.observer_count >= 0,
    control.observer_count <= max_tx_size_derived_collection_item_count,
    control.observer_items.length <= control.observer_count,
    control.observer_summary == empty_data_summary_v1(),
    if control.observer_items.length == 0 {
      and {
        control.observer_count == 0,
        control.observer_items == empty_observer_items_v1(control.language_tag),
        control.previous_observer == #"",
      }
    } else {
      and {
        control.observer_count > 0,
        bytearray.length(control.previous_observer) == 28,
      }
    },
  }
}

fn observer_context_is_complete(control: CekContextControlV1) -> Bool {
  and {
    control.observer_count >= 0,
    control.observer_count <= max_tx_size_derived_collection_item_count,
    control.observer_items.length == control.observer_count,
    bytearray.length(control.observer_summary.root) == 32,
    control.observer_summary == finalize_cek_observer_items_v1(
      control.observer_items,
      control.language_tag == 128,
    ),
    if control.observer_count == 0 {
      and {
        control.observer_items == empty_observer_items_v1(control.language_tag),
        control.previous_observer == #"",
      }
    } else {
      bytearray.length(control.previous_observer) == 28
    },
  }
}

fn redeemer_data_scan_is_initial(control: CekContextControlV1) -> Bool {
  bytearray.length(control.redeemer_context_control_hash) == 32
}

fn redeemer_data_scan_is_complete(control: CekContextControlV1) -> Bool {
  bytearray.length(control.redeemer_context_control_hash) == 32
}

fn mint_context_is_final(
  native_control: NativeScriptsControlV1,
  control: CekContextControlV1,
) -> Bool {
  and {
    observer_context_is_complete(control),
    control.mint_cursor == native_control.mint_count,
    control.current_mint_policy == #"",
    control.current_mint_assets == cek_data_v1.empty_data_pair_summary_v1(),
    bytearray.length(control.mint_summary.root) == 32,
  }
}

pub fn cek_context_control_is_well_formed(
  native_control: NativeScriptsControlV1,
  control: CekContextControlV1,
) -> Bool {
  let reference_count = native_control.resolved_input_count - native_control.spend_input_count
  and {
    control.stage >= 0,
    control.stage <= 13,
    or {
      control.language_tag == 3,
      control.language_tag == 128,
    },
    bytearray.length(control.program_term_root) == 32,
    bytearray.length(control.program_envelope_hash) == 32,
    control.purpose_kind >= 0,
    control.purpose_kind <= 3,
    control.purpose_index >= 0,
    bytearray.length(control.script_hash) == 28,
    bytearray.length(control.redeemer_leaf) == 32,
    or {
      control.redeemer_context_control_hash == #"",
      bytearray.length(control.redeemer_context_control_hash) == 32,
    },
    control.execution_memory_limit >= 0,
    control.execution_cpu_limit >= 0,
    data_sequence_summary_is_well_formed(control.reference_items),
    data_sequence_summary_is_well_formed(control.spend_items),
    data_sequence_summary_is_well_formed(control.output_items),
    data_sequence_summary_is_well_formed(control.signer_items),
    data_sequence_summary_is_well_formed(control.observer_items),
    optional_data_summary_is_well_formed(control.observer_summary),
    control.mint_cursor >= 0,
    control.mint_cursor <= native_control.mint_count,
    or {
      control.current_mint_policy == #"",
      bytearray.length(control.current_mint_policy) == 28,
    },
    data_sequence_summary_is_well_formed(control.current_mint_assets),
    data_sequence_summary_is_well_formed(control.mint_policies),
    optional_data_summary_is_well_formed(control.mint_summary),
    control.reference_items.length <= reference_count,
    control.spend_items.length <= native_control.spend_input_count,
    control.output_items.length <= native_control.output_count,
    if control.stage == 0 {
      and {
        control.execution_memory_limit == 0,
        control.execution_cpu_limit == 0,
        control.reference_items.length == 0,
        control.spend_items.length == 0,
        control.output_items.length == 0,
        control.signer_items.length == 0,
        observer_context_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 1 {
      and {
        redeemer_data_scan_is_initial(control),
        control.spend_items.length == 0,
        control.output_items.length == 0,
        control.signer_items.length == 0,
        observer_context_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 2 {
      and {
        redeemer_data_scan_is_initial(control),
        control.reference_items.length == reference_count,
        control.output_items.length == 0,
        control.signer_items.length == 0,
        observer_context_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 3 {
      and {
        redeemer_data_scan_is_initial(control),
        control.reference_items.length == reference_count,
        control.spend_items.length == native_control.spend_input_count,
        control.signer_items.length == 0,
        observer_context_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 4 {
      and {
        redeemer_data_scan_is_initial(control),
        control.reference_items.length == reference_count,
        control.spend_items.length == native_control.spend_input_count,
        control.output_items.length == native_control.output_count,
        control.signer_items.length <= native_control.signer_count,
        observer_context_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 5 {
      and {
        redeemer_data_scan_is_initial(control),
        control.signer_items.length == native_control.signer_count,
        observer_context_is_partial(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 6 {
      and {
        redeemer_data_scan_is_initial(control),
        control.signer_items.length == native_control.signer_count,
        observer_context_is_complete(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 7 {
      False
    } else if control.stage == 8 {
      and {
        redeemer_data_scan_is_initial(control),
        control.signer_items.length == native_control.signer_count,
        observer_context_is_complete(control),
        native_control.mint_count > 0,
        if control.mint_cursor == 0 {
          and {
            control.current_mint_policy == #"",
            control.current_mint_assets == cek_data_v1.empty_data_pair_summary_v1(),
            control.mint_policies == cek_data_v1.empty_data_pair_summary_v1(),
          }
        } else {
          and {
            bytearray.length(control.current_mint_policy) == 28,
            control.current_mint_assets.length > 0,
          }
        },
        control.mint_summary.root == #"",
      }
    } else if control.stage == 9 {
      and {
        control.signer_items.length == native_control.signer_count,
        mint_context_is_final(native_control, control),
        redeemer_data_scan_is_initial(control),
      }
    } else {
      and {
        control.signer_items.length == native_control.signer_count,
        mint_context_is_final(native_control, control),
        redeemer_data_scan_is_complete(control),
      }
    },
  }
}

pub fn encode_cek_witness_v1(
  native_control: NativeScriptsControlV1,
  context_control_cbor: ByteArray,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
  active_state_hash: ByteArray,
  execution_cpu_limit: Int,
  execution_memory_limit: Int,
  program_envelope_hash: ByteArray,
) -> ByteArray {
  expect execution_cursor >= 0
  expect execution_cursor <= native_control.execution_count
  expect completed_cpu >= 0
  expect completed_memory >= 0
  expect or {
      active_state_hash == #"",
      bytearray.length(active_state_hash) == 32,
    }
  expect execution_cpu_limit >= 0
  expect execution_memory_limit >= 0
  expect or {
      program_envelope_hash == #"",
      bytearray.length(program_envelope_hash) == 32,
    }
  // The witness must never end with a possibly-empty bytestring:
  // `aiken/cbor.deserialise` rejects a zero-length final item at an
  // exhausted cursor, so `program_envelope_hash` (empty before program
  // selection) sits before the two integer limits.
  #"89"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(native_control)),
      )
    |> bytearray.concat(encode_definite_bytes(context_control_cbor))
    |> bytearray.concat(cbor.serialise(execution_cursor))
    |> bytearray.concat(cbor.serialise(completed_cpu))
    |> bytearray.concat(cbor.serialise(completed_memory))
    |> bytearray.concat(encode_definite_bytes(active_state_hash))
    |> bytearray.concat(encode_definite_bytes(program_envelope_hash))
    |> bytearray.concat(cbor.serialise(execution_cpu_limit))
    |> bytearray.concat(cbor.serialise(execution_memory_limit))
}

pub fn encode_value_accumulator_v1(accumulator: ValueAccumulatorV1) -> ByteArray {
  #"84"
    |> bytearray.concat(cbor.serialise(accumulator.lovelace_delta))
    |> bytearray.concat(encode_definite_bytes(accumulator.asset_root))
    |> bytearray.concat(cbor.serialise(accumulator.seen_asset_count))
    |> bytearray.concat(cbor.serialise(accumulator.nonzero_asset_count))
}

fn value_accumulator_from_cbor(
  accumulator_cbor: ByteArray,
) -> ValueAccumulatorV1 {
  expect Some(data) = cbor.deserialise(accumulator_cbor)
  expect
      [
        lovelace_data, asset_root_data, seen_asset_count_data,
        nonzero_asset_count_data,
      ]
    = builtin.un_list_data(data)
  let accumulator = ValueAccumulatorV1 {
    lovelace_delta: builtin.un_i_data(lovelace_data),
    asset_root: builtin.un_b_data(asset_root_data),
    seen_asset_count: builtin.un_i_data(seen_asset_count_data),
    nonzero_asset_count: builtin.un_i_data(nonzero_asset_count_data),
  }
  expect bytearray.length(accumulator.asset_root) == 32
  expect accumulator.seen_asset_count >= 0
  expect
    accumulator.seen_asset_count <= ledger_output_v1.max_distinct_asset_count
  expect accumulator.nonzero_asset_count >= 0
  expect accumulator.nonzero_asset_count <= accumulator.seen_asset_count
  expect encode_value_accumulator_v1(accumulator) == accumulator_cbor
  accumulator
}

pub fn encode_value_and_mint_control_v1(
  control: ValueAndMintControlV1,
) -> ByteArray {
  encode_definite_array_header(12)
    |> bytearray.concat(
        encode_definite_bytes(
          encode_native_scripts_control_v1(control.native_control),
        ),
      )
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(encode_definite_bytes(control.replay_schedule_hash))
    |> bytearray.concat(cbor.serialise(control.replay_cursor))
    |> bytearray.concat(cbor.serialise(control.replay_asset_cursor))
    |> bytearray.concat(encode_definite_bytes(control.replay_value_hash))
    |> bytearray.concat(encode_definite_bytes(control.replay_accumulator))
    |> bytearray.concat(
        encode_definite_bytes(control.replay_remaining_schedule_hash),
      )
    |> bytearray.concat(cbor.serialise(control.output_cursor))
    |> bytearray.concat(cbor.serialise(control.output_asset_cursor))
    |> bytearray.concat(cbor.serialise(control.mint_cursor))
    |> bytearray.concat(
        encode_definite_bytes(
          encode_value_accumulator_v1(control.value_accumulator),
        ),
      )
}

pub fn value_and_mint_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ValueAndMintControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  expect
      [
        native_control_cbor_data, stage_data, replay_schedule_hash_data,
        replay_cursor_data, replay_asset_cursor_data, replay_value_hash_data,
        replay_accumulator_data, replay_remaining_schedule_hash_data,
        output_cursor_data, output_asset_cursor_data, mint_cursor_data,
        value_accumulator_cbor_data,
      ]
    = builtin.un_list_data(data)
  ValueAndMintControlV1 {
    native_control: native_scripts_control_from_witness(
      builtin.un_b_data(native_control_cbor_data),
    ),
    stage: builtin.un_i_data(stage_data),
    replay_schedule_hash: builtin.un_b_data(replay_schedule_hash_data),
    replay_cursor: builtin.un_i_data(replay_cursor_data),
    replay_asset_cursor: builtin.un_i_data(replay_asset_cursor_data),
    replay_value_hash: builtin.un_b_data(replay_value_hash_data),
    replay_accumulator: builtin.un_b_data(replay_accumulator_data),
    replay_remaining_schedule_hash: builtin.un_b_data(
      replay_remaining_schedule_hash_data,
    ),
    output_cursor: builtin.un_i_data(output_cursor_data),
    output_asset_cursor: builtin.un_i_data(output_asset_cursor_data),
    mint_cursor: builtin.un_i_data(mint_cursor_data),
    value_accumulator: value_accumulator_from_cbor(
      builtin.un_b_data(value_accumulator_cbor_data),
    ),
  }
}

pub fn initial_value_accumulator() -> ValueAccumulatorV1 {
  ValueAccumulatorV1 {
    lovelace_delta: 0,
    asset_root: null_hash,
    seen_asset_count: 0,
    nonzero_asset_count: 0,
  }
}

pub fn encode_value_and_mint_witness_v1(
  native_control: NativeScriptsControlV1,
) -> ByteArray {
  encode_value_and_mint_control_v1(
    ValueAndMintControlV1 {
      native_control,
      stage: 0,
      replay_schedule_hash: empty_resolution_schedule_hash(),
      replay_cursor: 0,
      replay_asset_cursor: 0,
      replay_value_hash: no_rejection_code_hash,
      replay_accumulator: initial_resolution_accumulator(),
      replay_remaining_schedule_hash: empty_resolution_schedule_hash(),
      output_cursor: 0,
      output_asset_cursor: 0,
      mint_cursor: 0,
      value_accumulator: initial_value_accumulator(),
    },
  )
}

fn ledger_delta_proof_descriptor_from_cbor(
  descriptor_cbor: ByteArray,
) -> mpf_proof_fold_v1.ProofDescriptorV1 {
  expect Some(data) = cbor.deserialise(descriptor_cbor)
  expect
      [
        version_data, frame_count_data, terminal_cursor_data,
        operation_peaks_data,
      ]
    = builtin.un_list_data(data)
  let descriptor = mpf_proof_fold_v1.ProofDescriptorV1 {
    version: builtin.un_i_data(version_data),
    frame_count: builtin.un_i_data(frame_count_data),
    terminal_cursor: builtin.un_i_data(terminal_cursor_data),
    peaks: decode_frontier_peaks(operation_peaks_data),
  }
  expect
    descriptor_cbor == mpf_proof_fold_v1.encode_proof_descriptor_v1(descriptor)
  descriptor
}

pub fn encode_ledger_delta_pending_mutation_v1(
  pending: LedgerDeltaPendingMutationV1,
) -> ByteArray {
  encode_definite_array_header(10)
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(cbor.serialise(pending.authorization_stage))
    |> bytearray.concat(cbor.serialise(pending.operation_kind))
    |> bytearray.concat(encode_definite_bytes(pending.key))
    |> bytearray.concat(encode_definite_bytes(pending.value))
    |> bytearray.concat(
        encode_definite_bytes(
          mpf_proof_fold_v1.encode_proof_descriptor_v1(pending.descriptor),
        ),
      )
    |> bytearray.concat(cbor.serialise(pending.fold_control.next_frame_index))
    |> bytearray.concat(
        encode_definite_bytes(pending.fold_control.including_root),
      )
    |> bytearray.concat(
        encode_definite_bytes(pending.fold_control.excluding_root),
      )
    |> bytearray.concat(
        cbor.serialise(pending.fold_control.expected_next_cursor),
      )
}

pub fn ledger_delta_pending_mutation_from_cbor(
  pending_cbor: ByteArray,
) -> LedgerDeltaPendingMutationV1 {
  expect Some(data) = cbor.deserialise(pending_cbor)
  expect
      [
        version_data, authorization_stage_data, operation_kind_data, key_data,
        value_data, descriptor_cbor_data, next_frame_index_data,
        including_root_data, excluding_root_data, expected_next_cursor_data,
      ]
    = builtin.un_list_data(data)
  expect builtin.un_i_data(version_data) == 1
  let pending = LedgerDeltaPendingMutationV1 {
    authorization_stage: builtin.un_i_data(authorization_stage_data),
    operation_kind: builtin.un_i_data(operation_kind_data),
    key: builtin.un_b_data(key_data),
    value: builtin.un_b_data(value_data),
    descriptor: ledger_delta_proof_descriptor_from_cbor(
      builtin.un_b_data(descriptor_cbor_data),
    ),
    fold_control: mpf_proof_fold_v1.ProofFoldControlV1 {
      next_frame_index: builtin.un_i_data(next_frame_index_data),
      expected_next_cursor: builtin.un_i_data(expected_next_cursor_data),
      including_root: builtin.un_b_data(including_root_data),
      excluding_root: builtin.un_b_data(excluding_root_data),
    },
  }
  expect
    if pending.authorization_stage == 0 {
      and {
        pending.fold_control.next_frame_index == -1,
        pending.fold_control.expected_next_cursor == 0,
        pending.fold_control.including_root == #"",
        pending.fold_control.excluding_root == #"",
      }
    } else {
      pending.authorization_stage == 1
    }
  expect pending.operation_kind == 0 || pending.operation_kind == 1
  expect pending_cbor == encode_ledger_delta_pending_mutation_v1(pending)
  pending
}

pub fn encode_ledger_delta_control_v1(
  control: LedgerDeltaControlV1,
) -> ByteArray {
  encode_definite_array_header(14)
    |> bytearray.concat(cbor.serialise(control.resolved_input_count))
    |> bytearray.concat(
        encode_definite_bytes(control.resolved_inputs_accumulator),
      )
    |> bytearray.concat(cbor.serialise(control.output_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.output_descriptor_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(encode_definite_bytes(control.replay_schedule_hash))
    |> bytearray.concat(cbor.serialise(control.replay_cursor))
    |> bytearray.concat(encode_definite_bytes(control.replay_accumulator))
    |> bytearray.concat(
        encode_definite_bytes(control.replay_remaining_schedule_hash),
      )
    |> bytearray.concat(encode_definite_bytes(control.current_ledger_root))
    |> bytearray.concat(cbor.serialise(control.output_cursor))
    |> bytearray.concat(cbor.serialise(control.operation_count))
    |> bytearray.concat(encode_definite_bytes(control.pending_mutation_cbor))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.operation_peaks),
      )
}

pub fn ledger_delta_control_from_witness(
  work_witness_cbor: ByteArray,
) -> LedgerDeltaControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  expect
      [
        resolved_input_count_data, resolved_inputs_accumulator_data,
        output_count_data, output_descriptor_peaks_data, stage_data,
        replay_schedule_hash_data, replay_cursor_data, replay_accumulator_data,
        replay_remaining_schedule_hash_data, current_ledger_root_data,
        output_cursor_data, operation_count_data, pending_mutation_cbor_data,
        operation_peaks_data,
      ]
    = builtin.un_list_data(data)
  LedgerDeltaControlV1 {
    resolved_input_count: builtin.un_i_data(resolved_input_count_data),
    resolved_inputs_accumulator: builtin.un_b_data(
      resolved_inputs_accumulator_data,
    ),
    output_count: builtin.un_i_data(output_count_data),
    output_descriptor_peaks: decode_frontier_peaks(output_descriptor_peaks_data),
    stage: builtin.un_i_data(stage_data),
    replay_schedule_hash: builtin.un_b_data(replay_schedule_hash_data),
    replay_cursor: builtin.un_i_data(replay_cursor_data),
    replay_accumulator: builtin.un_b_data(replay_accumulator_data),
    replay_remaining_schedule_hash: builtin.un_b_data(
      replay_remaining_schedule_hash_data,
    ),
    current_ledger_root: builtin.un_b_data(current_ledger_root_data),
    output_cursor: builtin.un_i_data(output_cursor_data),
    operation_count: builtin.un_i_data(operation_count_data),
    operation_peaks: decode_frontier_peaks(operation_peaks_data),
    pending_mutation_cbor: builtin.un_b_data(pending_mutation_cbor_data),
  }
}

pub fn encode_ledger_delta_witness_v1(
  native_control: NativeScriptsControlV1,
  prior_ledger_root: ByteArray,
  replay_schedule_hash: ByteArray,
) -> ByteArray {
  expect bytearray.length(prior_ledger_root) == 32
  expect bytearray.length(replay_schedule_hash) == 32
  encode_ledger_delta_control_v1(
    LedgerDeltaControlV1 {
      resolved_input_count: native_control.resolved_input_count,
      resolved_inputs_accumulator: native_control.resolved_inputs_accumulator,
      output_count: native_control.output_count,
      output_descriptor_peaks: native_control.output_descriptor_peaks,
      stage: 0,
      replay_schedule_hash,
      replay_cursor: 0,
      replay_accumulator: initial_resolution_accumulator(),
      replay_remaining_schedule_hash: replay_schedule_hash,
      current_ledger_root: prior_ledger_root,
      output_cursor: 0,
      operation_count: 0,
      operation_peaks: [],
      pending_mutation_cbor: #"",
    },
  )
}

pub fn encode_terminal_acceptance_witness_v1(
  post_ledger_root: ByteArray,
  operation_count: Int,
  operation_peaks: List<validation_merkle_v1.FrontierPeak>,
) -> ByteArray {
  expect bytearray.length(post_ledger_root) == 32
  expect
    validation_merkle_v1.frontier_is_well_formed(
      operation_count,
      operation_peaks,
    )
  let frontier_cbor = #"82"
    |> bytearray.concat(cbor.serialise(operation_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(operation_peaks))
  #"84"
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(encode_definite_bytes(#""))
    |> bytearray.concat(encode_definite_bytes(post_ledger_root))
    |> bytearray.concat(encode_definite_bytes(frontier_cbor))
}

pub fn cek_witness_control_v1(
  witness: ValidationOneStepWitnessV1,
) -> CekWitnessControlV1 {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        native_control_cbor_data, context_control_cbor_data,
        execution_cursor_data, completed_cpu_data, completed_memory_data,
        active_state_hash_data, program_envelope_hash_data,
        execution_cpu_limit_data, execution_memory_limit_data,
      ]
    = builtin.un_list_data(data)
  CekWitnessControlV1 {
    control: native_scripts_control_from_witness(
      builtin.un_b_data(native_control_cbor_data),
    ),
    context_control_cbor: builtin.un_b_data(context_control_cbor_data),
    execution_cursor: builtin.un_i_data(execution_cursor_data),
    completed_cpu: builtin.un_i_data(completed_cpu_data),
    completed_memory: builtin.un_i_data(completed_memory_data),
    active_state_hash: builtin.un_b_data(active_state_hash_data),
    program_envelope_hash: builtin.un_b_data(program_envelope_hash_data),
    execution_cpu_limit: builtin.un_i_data(execution_cpu_limit_data),
    execution_memory_limit: builtin.un_i_data(execution_memory_limit_data),
  }
}

pub fn cek_witness_is_well_formed_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  cek: CekWitnessControlV1,
) -> Bool {
  let control = cek.control
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    verified_source.version == 1,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    native_scripts_control_is_well_formed(control),
    control.execution_cursor == control.execution_count,
    cek.execution_cursor >= 0,
    cek.execution_cursor <= control.execution_count,
    cek.completed_cpu >= 0,
    cek.completed_memory >= 0,
    witness.work_witness_cbor == encode_cek_witness_v1(
      control,
      cek.context_control_cbor,
      cek.execution_cursor,
      cek.completed_cpu,
      cek.completed_memory,
      cek.active_state_hash,
      cek.execution_cpu_limit,
      cek.execution_memory_limit,
      cek.program_envelope_hash,
    ),
  }
}

/// A cek step whose control carries no active state and no context control
/// is either the hand-off to ValueAndMint (`cek_control_is_finish_v1`) or an
/// execution selection; the two are told apart by the execution cursor
/// alone, so they stay total and disjoint. The language bitmap takes no part:
/// a transaction whose executions are all native carries `language_bitmap
/// == 0` and still has every one of those executions to select, one selection
/// step each, exactly as the machine's trace emits them (#629).
fn cek_control_is_selecting_v1(cek: CekWitnessControlV1) -> Bool {
  and {
    cek.active_state_hash == #"",
    cek.context_control_cbor == #"",
  }
}

pub fn cek_control_is_finish_v1(cek: CekWitnessControlV1) -> Bool {
  and {
    cek_control_is_selecting_v1(cek),
    cek.execution_cursor == cek.control.execution_count,
  }
}

pub fn cek_control_is_execution_selection_v1(cek: CekWitnessControlV1) -> Bool {
  and {
    cek_control_is_selecting_v1(cek),
    cek.execution_cursor != cek.control.execution_count,
  }
}

pub fn cek_control_is_context_step_v1(cek: CekWitnessControlV1) -> Bool {
  and {
    cek.active_state_hash == #"",
    cek.context_control_cbor != #"",
  }
}

pub fn cek_control_is_core_step_v1(cek: CekWitnessControlV1) -> Bool {
  cek.active_state_hash != #""
}

pub fn ledger_delta_control_is_well_formed(
  witness: ValidationOneStepWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  and {
    control.resolved_input_count >= 0,
    control.resolved_input_count <= max_tx_size_derived_collection_item_count,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
    control.output_count >= 0,
    control.output_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_descriptor_peaks,
    ),
    bytearray.length(control.replay_schedule_hash) == 32,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    bytearray.length(control.current_ledger_root) == 32,
    control.replay_cursor >= 0,
    control.replay_cursor <= control.resolved_input_count,
    control.output_cursor >= 0,
    control.output_cursor <= control.output_count,
    control.operation_count >= 0,
    control.operation_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.operation_count,
      control.operation_peaks,
    ),
    control.stage >= 0,
    control.stage <= 2,
    witness.work_witness_cbor == encode_ledger_delta_control_v1(control),
  }
}
