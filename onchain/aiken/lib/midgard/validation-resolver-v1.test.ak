use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId, from_lovelace}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/cek_proof_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardTxInput, TxIsValid, native_tx_version_v1,
}
use midgard/ledger_state.{CekProgramMaterialDatumV1}
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_resolution_v1
use midgard/validation_resolver_v1
use midgard/validation_semantic_v1
use midgard/validation_trace_v1

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const material_script_hash: ScriptHash =
  #"77777777777777777777777777777777777777777777777777777777"

const computation_thread_policy: PolicyId =
  #"22222222222222222222222222222222222222222222222222222222"

const prepare_script_hash: ScriptHash =
  #"33333333333333333333333333333333333333333333333333333333"

const award_script_hash: ScriptHash =
  #"66666666666666666666666666666666666666666666666666666666"

const fraud_prover = #"44444444444444444444444444444444444444444444444444444444"

const computation_thread_asset_name =
  #"00000006aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const own_out_ref =
  OutputReference {
    transaction_id: #"5555555555555555555555555555555555555555555555555555555555555555",
    output_index: 0,
  }

const validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

test cek_material_reference_indices_are_unique_reference_selectors() {
  and {
    validation_resolver_v1.cek_reference_indices_unique_nonnegative_v1([]),
    validation_resolver_v1.cek_reference_indices_unique_nonnegative_v1([0]),
    validation_resolver_v1.cek_reference_indices_unique_nonnegative_v1(
      [2, 0, 1],
    ),
    !validation_resolver_v1.cek_reference_indices_unique_nonnegative_v1([-1]),
    !validation_resolver_v1.cek_reference_indices_unique_nonnegative_v1([1, 1]),
  }
}

test cek_direct_and_incremental_routes_bind_the_selected_envelope() {
  let preimage = #"8106"
  let root = cek_proof_v1.hash_error_term_v1()
  let envelope = cek_proof_v1.encode_program_envelope_v1(1, 1, 0, root, 1, 2)
  let sidecar = #"82"
    |> bytearray.concat(#"01")
    |> bytearray.concat(#"81")
    |> bytearray.concat(#"82")
    |> bytearray.concat(#"5820")
    |> bytearray.concat(root)
    |> bytearray.concat(#"83")
    |> bytearray.concat(#"01")
    |> bytearray.concat(#"00")
    |> bytearray.concat(#"42")
    |> bytearray.concat(preimage)
  let envelope_hash = cek_proof_v1.hash_program_envelope_v1(1, 1, 0, root, 1, 2)
  and {
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.DirectCekMaterial {
        envelope_cbor: envelope,
        sidecar_cbor: sidecar,
      },
      [],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.DirectCekMaterial {
        envelope_cbor: #"8106",
        sidecar_cbor: sidecar,
      },
      [],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.DirectCekMaterial {
        envelope_cbor: envelope,
        sidecar_cbor: sidecar |> bytearray.concat(#"00"),
      },
      [],
      material_script_hash,
    ),
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.IncrementalCekMaterial {
        program_envelope_hash: envelope_hash,
      },
      [],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.IncrementalCekMaterial {
        program_envelope_hash: h_a,
      },
      [],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.NoCekMaterial,
      [],
      material_script_hash,
    ),
  }
}

fn material_reference_input(datum: Data, hash: ScriptHash) -> Input {
  Input {
    output_reference: own_out_ref,
    output: Output {
      address: script_address(hash),
      value: from_lovelace(2_000_000),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

test cek_single_and_multi_reference_routes_verify_typed_material() {
  let preimage = #"8106"
  let root = cek_proof_v1.hash_error_term_v1()
  let envelope = cek_proof_v1.encode_program_envelope_v1(1, 1, 0, root, 1, 2)
  let sidecar = #"820181825820"
    |> bytearray.concat(root)
    |> bytearray.concat(#"830100428106")
  let envelope_hash = cek_proof_v1.hash_program_envelope_v1(1, 1, 0, root, 1, 2)
  let single: Data = validation_resolver_v1.CekSinglePublicationDatumV1 {
    version: 1,
    program_envelope_hash: envelope_hash,
    sidecar_cbor: sidecar,
  }
  let bad_version: Data = validation_resolver_v1.CekSinglePublicationDatumV1 {
    version: 2,
    program_envelope_hash: envelope_hash,
    sidecar_cbor: sidecar,
  }
  let bad_hash: Data = validation_resolver_v1.CekSinglePublicationDatumV1 {
    version: 1,
    program_envelope_hash: h_a,
    sidecar_cbor: sidecar,
  }
  let entry: Data = CekProgramMaterialDatumV1 { kind: 0, root, preimage }
  and {
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(single, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 1,
      },
      [material_reference_input(single, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(bad_version, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(bad_hash, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(single, prepare_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [0, 0],
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [1],
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [0],
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [],
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
  }
}

// Bound by the SDK cross-language test to buildFaultProofContracts over a
// freshly generated, compiler-pinned testnet plutus.json and deterministic
// fixture policy IDs. The same test independently applies the Phase-A and
// ScriptSources prepare validators to these exact semantic groups. These are
// parameterized production-builder fixture identities, not a claim about a
// live deployment.
fn phase_a_script_precondition_resolvers() -> List<ScriptHash> {
  [
    #"e2bc97da9ec252d996282fb9715a5c76df04107357605f9f58875ef8",
    #"6f7267cf1da8c83c5a3f5135ee1bb15a6bf5a62184ebab67d9f5c254",
  ]
}

fn input_sets_resolvers() -> List<ScriptHash> {
  [prepare_script_hash, award_script_hash]
}

fn script_source_resolvers() -> List<ScriptHash> {
  [
    #"26fcba0b1e76a4d60c35c084c5ceb71f15cdc05d27d191a286be9f2b",
    #"dbc5185f10bfd71ba6d0b0ae3198164fbbfbf9c8a7689710967dfbf5",
    #"a9df8e702624b87db5198a8520b02388faeed615979719678eaa249e",
    #"d69732eda4f267c3a37f7f58e99418877031f57fa98386520aa911c1",
    #"ebdcae5094b7733b6f535782fcb8a61912312b0cb25ac02151b5d921",
    #"d97b8f23efb31e32538c89a0d97009dcbbaffacbc73e097a86355ba2",
    #"37618d1d07d266924ac5f0631a1791c27d9bd76d3bacad893466f6ca",
    #"ba604acdd10a0101a6a293f6237a411f03a1013d984f279156657d5d",
    #"8cc9c85b11695228483e63d0a59684a989d8236ca2ec654f8e8ec0c1",
    #"be5e382cd992ec856b427e1dac37aad24d07765a09aefb7f8509fae7",
    #"01a692a30d8775625c42ff0fbe088b84610bd1aa0c9f6a7a01bf9a58",
    #"3c2d3271c5354eba7c4696e1f4a3dd89c30620637637ddc66accb8c3",
    #"ac35e730352b2ce5f579a6b757037014e745fa8a7561c40cacb58aa4",
    #"1e48d46d39a29e6158d40da127654959b1b64fdb88700969db0b1f94",
    #"7319aa9013feca18760134d3b9929d5b135829216fe34a6f05a16508",
    #"48137906ab01e0301b898e4eb3e33ef465790325d7ba46bc37689f9e",
    #"3d0e2e530d0781721e5d1b8efab92eadf6100c36ce69fc61b3ce794c",
    #"fa448dd1d98c443842fb50e1f22ce3c71b7bcbb8f1bec3e83f66a637",
    #"cd74112e0e53e710bd27b5f0e3c0ca6b60bbf65b609ccec69c36a153",
    #"04986f3203ca2e208b02165819ccd6fd21e3580fa05b75b2d5829379",
    #"10b4c378158f236c02fd8a4abe3d9ecca63b48e65f5830e2726081f0",
    #"4882dd77a217aa71ecc8dc3dada0ea710775e03ab18c598a548f490f",
    #"942a8d522d5468146cad13eb7958d418e9da53aa266c65ded404723a",
    #"00d2cf824ca845f19880af8f9a32a0a1e6b93c74907735af96c93e50",
    #"b350f95cfaaf97c5bad1b1d82e5df8592782a12f7cdbbbf5b0285cf2",
    #"dc6561fe389cf36a1b520934cf508eafd2428e53e1208b77b53d545c",
    #"8b2be118846589dc7584d74cbf8a35b17ca14b9ef49a0a3187ba1fe3",
    #"594f9a0e996ff5857847edee440f9fe63cb297e05bbbf196cff650c7",
    #"9e0e7ddbfa728c2b6b27f084e31220f3c68025ecb249d2d0eaa9ae8a",
  ]
}

fn script_address(script_hash: ScriptHash) -> Address {
  Address {
    payment_credential: address.Script(script_hash),
    stake_credential: None,
  }
}

fn empty_v1_transaction() -> MidgardTransaction {
  let empty_mint: Data = []
  MidgardTransaction {
    version: native_tx_version_v1,
    validity: TxIsValid,
    body: MidgardTransactionBody {
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
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

fn phase_a_semantic_fixture() -> Pair<
  validation_trace_v1.ValidationMachineStateV1,
  validation_machine_v1.ValidationOneStepWitnessV1,
> {
  let tx = empty_v1_transaction()
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  let schedule_hash = validation_machine_v1.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let signer_frontier_commitment = validation_merkle_v1.frontier_commitment(
    0,
    [],
  )
  let work_witness_cbor = validation_machine_v1.encode_phase_a_script_preconditions_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    schedule_hash,
    0,
    signer_frontier_commitment,
    False,
    0,
    0,
    #"",
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: native_tx_id_for_version(
      native_tx_version_v1,
      encode_native_tx_body_compact(compact.body),
    ),
    transaction_commitment: native_transaction.midgard_transaction_proof_commitment_v1(
      tx,
    ),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_b,
    phase: validation_trace_v1.PhaseAScriptPreconditions,
    program_counter: 6,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseAScriptPreconditions,
      6,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_c,
  }
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
        schedule_hash,
        0,
        signer_frontier_commitment,
        None,
        schedule_hash,
      ),
    ),
  }
  Pair(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
  )
}

fn pre_state(
  phase: validation_trace_v1.ValidationPhase,
  work_witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: h_b,
    transaction_commitment: h_c,
    validation_context_hash: h_a,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_b,
    phase,
    program_counter: 6,
    work_root: validation_trace_v1.hash_work_witness(
      phase,
      6,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_c,
  }
}

fn successor_state(
  pre: validation_trace_v1.ValidationMachineStateV1,
  work_root: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: pre.program_counter + 1,
    work_root,
  }
}

fn resolution_state(
  pre: validation_trace_v1.ValidationMachineStateV1,
  transition: validation_machine_v1.ValidationOneStepWitnessV1,
) -> validation_resolution_v1.ValidationResolutionStateV1 {
  validation_resolution_v1.ValidationResolutionStateV1 {
    version: validation_resolution_v1.resolution_version,
    pre_state: pre,
    operator_successor_hash: h_a,
    challenger_successor_hash: validation_trace_v1.hash_machine_state(
      transition.claimed_successor,
    ),
  }
}

fn step_datum(
  state: validation_resolution_v1.ValidationResolutionStateV1,
) -> validation_resolver_v1.Datum {
  ct.StepDatum { fraud_prover, data: Some(state) }
}

fn no_auxiliary_data() -> Data {
  let auxiliary_data: Data = validation_machine_v1.NoAuxiliaryWitness
  auxiliary_data
}

fn script_sources_auxiliary() -> validation_machine_v1.ValidationAuxiliaryWitnessV1 {
  validation_machine_v1.ScriptPurposeScanWitness {
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: prepare_script_hash,
    subject: #"80",
    siblings: [],
  }
}

fn script_sources_auxiliary_data() -> Data {
  let auxiliary_data: Data = script_sources_auxiliary()
  auxiliary_data
}

fn prepare_redeemer(
  semantic_resolver_index: Int,
  transition: validation_machine_v1.ValidationOneStepWitnessV1,
  auxiliary: Data,
) -> validation_resolver_v1.PrepareSelectedSpendRedeemer {
  ct.Continue(
    validation_resolver_v1.PrepareSelected {
      input_index: 0,
      output_index: 0,
      semantic_resolver_index,
      transition,
      auxiliary,
    },
  )
}

fn prepare_transaction(
  state: validation_resolution_v1.ValidationResolutionStateV1,
  transition: validation_machine_v1.ValidationOneStepWitnessV1,
  auxiliary: Data,
  output_script_hash: ScriptHash,
) -> Transaction {
  let input_datum_data: Data = step_datum(state)
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    transition,
    auxiliary,
  )
  let output_datum_data: Data = ct.StepDatum {
    fraud_prover,
    data: Some(prepared_state),
  }
  Transaction {
    ..transaction.placeholder,
    inputs: [
      transaction.Input {
        output_reference: own_out_ref,
        output: transaction.Output {
          address: script_address(prepare_script_hash),
          value: assets.from_lovelace(2_000_000)
            |> assets.add(
                computation_thread_policy,
                computation_thread_asset_name,
                1,
              ),
          datum: transaction.InlineDatum(input_datum_data),
          reference_script: None,
        },
      },
    ],
    outputs: [
      transaction.Output {
        address: script_address(output_script_hash),
        value: assets.from_lovelace(2_000_000)
          |> assets.add(
              computation_thread_policy,
              computation_thread_asset_name,
              1,
            ),
        datum: transaction.InlineDatum(output_datum_data),
        reference_script: None,
      },
    ],
  }
}

fn prepared_step_datum(
  prepared_state: validation_resolution_v1.PreparedValidationResolutionStateV1,
) -> validation_semantic_v1.Datum {
  ct.StepDatum { fraud_prover, data: Some(prepared_state) }
}

fn semantic_transaction(
  prepared_state: validation_resolution_v1.PreparedValidationResolutionStateV1,
  input_script_hash: ScriptHash,
) -> Transaction {
  let input_datum_data: Data = prepared_step_datum(prepared_state)
  let output_datum_data: Data = ct.StepDatum {
    fraud_prover,
    data: Some(validation_resolution_v1.winning_resolution()),
  }
  Transaction {
    ..transaction.placeholder,
    inputs: [
      transaction.Input {
        output_reference: own_out_ref,
        output: transaction.Output {
          address: script_address(input_script_hash),
          value: assets.from_lovelace(2_000_000)
            |> assets.add(
                computation_thread_policy,
                computation_thread_asset_name,
                1,
              ),
          datum: transaction.InlineDatum(input_datum_data),
          reference_script: None,
        },
      },
    ],
    outputs: [
      transaction.Output {
        address: script_address(award_script_hash),
        value: assets.from_lovelace(2_000_000)
          |> assets.add(
              computation_thread_policy,
              computation_thread_asset_name,
              1,
            ),
        datum: transaction.InlineDatum(output_datum_data),
        reference_script: None,
      },
    ],
  }
}

fn resolver_at(group: List<ScriptHash>, count: Int, index: Int) -> ScriptHash {
  expect Some(script_hash) = validation_resolver_v1.select_semantic_resolver(
    group,
    count,
    index,
  )
  script_hash
}

fn every_route_uses_the_deployed_hash(
  expected_phase: validation_trace_v1.ValidationPhase,
  group: List<ScriptHash>,
  count: Int,
  state: validation_resolution_v1.ValidationResolutionStateV1,
  transition: validation_machine_v1.ValidationOneStepWitnessV1,
  auxiliary: Data,
  index: Int,
) -> Bool {
  if index == count {
    True
  } else {
    let output_script_hash = resolver_at(group, count, index)
    and {
      validation_resolver_v1.prepare_selected(
        expected_phase,
        group,
        count,
        computation_thread_policy,
        Some(step_datum(state)),
        prepare_redeemer(index, transition, auxiliary),
        own_out_ref,
        prepare_transaction(state, transition, auxiliary, output_script_hash),
      ),
      every_route_uses_the_deployed_hash(
        expected_phase,
        group,
        count,
        state,
        transition,
        auxiliary,
        index + 1,
      ),
    }
  }
}

test phase_a_applied_group_routes_through_the_production_prepare_path() {
  let work_witness_cbor = #"80"
  let pre = pre_state(
    validation_trace_v1.PhaseAScriptPreconditions,
    work_witness_cbor,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let state = resolution_state(pre, transition)
  let auxiliary = no_auxiliary_data()
  let group = phase_a_script_precondition_resolvers()
  every_route_uses_the_deployed_hash(
    validation_trace_v1.PhaseAScriptPreconditions,
    group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    state,
    transition,
    auxiliary,
    0,
  )
}

test script_sources_applied_group_routes_through_the_production_prepare_path() {
  let work_witness_cbor = #"81"
  let pre = pre_state(validation_trace_v1.ScriptSources, work_witness_cbor)
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let state = resolution_state(pre, transition)
  let auxiliary = script_sources_auxiliary_data()
  let group = script_source_resolvers()
  every_route_uses_the_deployed_hash(
    validation_trace_v1.ScriptSources,
    group,
    validation_resolver_v1.script_sources_semantic_resolver_count,
    state,
    transition,
    auxiliary,
    0,
  )
}

test production_prepare_rejects_a_wrong_semantic_resolver_index() fail {
  let work_witness_cbor = #"80"
  let pre = pre_state(
    validation_trace_v1.PhaseAScriptPreconditions,
    work_witness_cbor,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let state = resolution_state(pre, transition)
  let auxiliary = no_auxiliary_data()
  let group = phase_a_script_precondition_resolvers()
  let index_one_output = resolver_at(
    group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    1,
  )
  validation_resolver_v1.prepare_selected(
    validation_trace_v1.PhaseAScriptPreconditions,
    group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    computation_thread_policy,
    Some(step_datum(state)),
    prepare_redeemer(0, transition, auxiliary),
    own_out_ref,
    prepare_transaction(state, transition, auxiliary, index_one_output),
  )
}

test production_prepare_rejects_a_wrong_output_script_hash() {
  let work_witness_cbor = #"80"
  let pre = pre_state(
    validation_trace_v1.PhaseAScriptPreconditions,
    work_witness_cbor,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let state = resolution_state(pre, transition)
  let auxiliary = no_auxiliary_data()
  let wrong_output_script_hash = resolver_at(
    input_sets_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  !validation_resolver_v1.prepare_selected(
    validation_trace_v1.PhaseAScriptPreconditions,
    phase_a_script_precondition_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    computation_thread_policy,
    Some(step_datum(state)),
    prepare_redeemer(1, transition, auxiliary),
    own_out_ref,
    prepare_transaction(state, transition, auxiliary, wrong_output_script_hash),
  )
}

test production_prepare_rejects_same_cardinality_group_substitution() {
  let work_witness_cbor = #"80"
  let pre = pre_state(
    validation_trace_v1.PhaseAScriptPreconditions,
    work_witness_cbor,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let state = resolution_state(pre, transition)
  let auxiliary = no_auxiliary_data()
  let deployed_group = phase_a_script_precondition_resolvers()
  let deployed_output = resolver_at(
    deployed_group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    1,
  )
  !validation_resolver_v1.prepare_selected(
    validation_trace_v1.PhaseAScriptPreconditions,
    input_sets_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    computation_thread_policy,
    Some(step_datum(state)),
    prepare_redeemer(1, transition, auxiliary),
    own_out_ref,
    prepare_transaction(state, transition, auxiliary, deployed_output),
  )
}

test semantic_handoff_accepts_exact_prepared_phase_a_evidence() {
  let Pair(pre, transition) = phase_a_semantic_fixture()
  let state = resolution_state(pre, transition)
  let auxiliary = validation_machine_v1.NoAuxiliaryWitness
  let auxiliary_data: Data = auxiliary
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    transition,
    auxiliary_data,
  )
  let group = phase_a_script_precondition_resolvers()
  let input_script_hash = resolver_at(
    group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition,
    auxiliary,
  }
  and {
    validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
      pre,
      evidence,
    ),
    validation_semantic_v1.continue_winning(
      validation_trace_v1.PhaseAScriptPreconditions,
      award_script_hash,
      computation_thread_policy,
      Some(prepared_step_datum(prepared_state)),
      0,
      0,
      transition,
      auxiliary_data,
      validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
        pre,
        evidence,
      ),
      own_out_ref,
      semantic_transaction(prepared_state, input_script_hash),
    ),
  }
}

test semantic_handoff_rejects_script_sources_auxiliary_substitution() {
  let Pair(pre, transition) = phase_a_semantic_fixture()
  let state = resolution_state(pre, transition)
  let committed_auxiliary = validation_machine_v1.NoAuxiliaryWitness
  let committed_auxiliary_data: Data = committed_auxiliary
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    transition,
    committed_auxiliary_data,
  )
  let input_script_hash = resolver_at(
    phase_a_script_precondition_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  let committed_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition,
    auxiliary: committed_auxiliary,
  }
  let cross_family_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition,
    auxiliary: script_sources_auxiliary(),
  }
  and {
    validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
      pre,
      committed_evidence,
    ),
    !validation_semantic_v1.continue_winning(
      validation_trace_v1.PhaseAScriptPreconditions,
      award_script_hash,
      computation_thread_policy,
      Some(prepared_step_datum(prepared_state)),
      0,
      0,
      transition,
      committed_auxiliary_data,
      validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
        pre,
        cross_family_evidence,
      ),
      own_out_ref,
      semantic_transaction(prepared_state, input_script_hash),
    ),
  }
}

test semantic_handoff_rejects_cross_family_action_substitution() {
  let Pair(pre, committed_transition) = phase_a_semantic_fixture()
  let committed_auxiliary = validation_machine_v1.NoAuxiliaryWitness
  let committed_auxiliary_data: Data = committed_auxiliary
  let cross_family_post = validation_trace_v1.ValidationMachineStateV1 {
    ..committed_transition.claimed_successor,
    phase: validation_trace_v1.ScriptSources,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      committed_transition.claimed_successor.program_counter,
      committed_transition.work_witness_cbor,
    ),
  }
  let cross_family_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    ..committed_transition,
    claimed_successor: cross_family_post,
  }
  let state = resolution_state(pre, committed_transition)
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    committed_transition,
    committed_auxiliary_data,
  )
  let input_script_hash = resolver_at(
    phase_a_script_precondition_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  let committed_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: committed_transition,
    auxiliary: committed_auxiliary,
  }
  let cross_family_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: cross_family_transition,
    auxiliary: committed_auxiliary,
  }
  and {
    validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
      pre,
      committed_evidence,
    ),
    !validation_semantic_v1.continue_winning(
      validation_trace_v1.PhaseAScriptPreconditions,
      award_script_hash,
      computation_thread_policy,
      Some(prepared_step_datum(prepared_state)),
      0,
      0,
      committed_transition,
      committed_auxiliary_data,
      validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
        pre,
        cross_family_evidence,
      ),
      own_out_ref,
      semantic_transaction(prepared_state, input_script_hash),
    ),
  }
}
