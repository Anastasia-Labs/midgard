use aiken/crypto.{ScriptHash, blake2b_224}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/validation_machine_v1
use midgard/validation_resolution_v1
use midgard/validation_resolver_v1
use midgard/validation_semantic_v1
use midgard/validation_trace_v1

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

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

fn semantic_script_hash_fixture(title: ByteArray) -> ScriptHash {
  blake2b_224(title)
}

fn phase_a_script_precondition_resolvers() -> List<ScriptHash> {
  [
    semantic_script_hash_fixture(
      #"70686173655f615f7363726970745f707265636f6e646974696f6e735f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"70686173655f615f7363726970745f707265636f6e646974696f6e735f6974656d5f73656d616e7469635f7631",
    ),
  ]
}

fn input_sets_resolvers() -> List<ScriptHash> {
  [
    semantic_script_hash_fixture(
      #"696e7075745f736574735f656d7074795f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"696e7075745f736574735f6974656d5f73656d616e7469635f7631",
    ),
  ]
}

fn script_source_resolvers() -> List<ScriptHash> {
  [
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f6e6f6e5f6f75747075745f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f6f75747075745f70726f6f665f626567696e5f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f6f75747075745f70726f6f665f737465705f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f6f75747075745f70726f6f665f66696e616c697a655f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f6f75747075745f70726f6f665f66696e6973685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f7a65726f5f626567696e5f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f7a65726f5f66696e6973685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f7a65726f5f686173685f626c6f636b5f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f7a65726f5f686173685f616476616e63655f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f7a65726f5f686173685f7465726d696e616c5f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f6e696e655f6d69736d617463685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f6e696e655f6e61746976655f6d617463685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f6e696e655f65666665637466756c5f6d617463685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f6e696e655f6d697373696e675f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f6f6e655f66696e6973685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f6f6e655f72656465656d65725f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f656c6576656e5f66696e6973685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f656c6576656e5f736f757263655f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f7477656c76655f66696e6973685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f7477656c76655f72656465656d65725f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f74656e5f6d697373696e675f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f74656e5f6d69736d617463685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f74656e5f6d617463685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f65696768745f66696e6973685f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f65696768745f707572706f73655f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f736576656e5f6f627365727665725f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f736576656e5f726563656976655f73656d616e7469635f7631",
    ),
    semantic_script_hash_fixture(
      #"7363726970745f736f75726365735f73746167655f736576656e5f66696e6973685f73656d616e7469635f7631",
    ),
  ]
}

fn script_address(script_hash: ScriptHash) -> Address {
  Address {
    payment_credential: address.Script(script_hash),
    stake_credential: None,
  }
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

fn script_sources_auxiliary_data() -> Data {
  let auxiliary_data: Data = validation_machine_v1.ScriptPurposeScanWitness {
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: semantic_script_hash_fixture(
      #"7363726970745f736f757263655f617578696c69617279",
    ),
    subject: #"80",
    siblings: [],
  }
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
  let output_script_hash = resolver_at(
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
    prepare_redeemer(1, transition, auxiliary),
    own_out_ref,
    prepare_transaction(state, transition, auxiliary, output_script_hash),
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
  let output_script_hash = resolver_at(
    group,
    validation_resolver_v1.script_sources_semantic_resolver_count,
    27,
  )
  validation_resolver_v1.prepare_selected(
    validation_trace_v1.ScriptSources,
    group,
    validation_resolver_v1.script_sources_semantic_resolver_count,
    computation_thread_policy,
    Some(step_datum(state)),
    prepare_redeemer(27, transition, auxiliary),
    own_out_ref,
    prepare_transaction(state, transition, auxiliary, output_script_hash),
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
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    transition,
    auxiliary,
  )
  let group = phase_a_script_precondition_resolvers()
  let input_script_hash = resolver_at(
    group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  validation_semantic_v1.continue_winning(
    validation_trace_v1.PhaseAScriptPreconditions,
    award_script_hash,
    computation_thread_policy,
    Some(prepared_step_datum(prepared_state)),
    0,
    0,
    transition,
    auxiliary,
    True,
    own_out_ref,
    semantic_transaction(prepared_state, input_script_hash),
  )
}

test semantic_handoff_rejects_script_sources_auxiliary_substitution() {
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
  let committed_auxiliary = no_auxiliary_data()
  let substituted_auxiliary = script_sources_auxiliary_data()
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    transition,
    committed_auxiliary,
  )
  let input_script_hash = resolver_at(
    phase_a_script_precondition_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  !validation_semantic_v1.continue_winning(
    validation_trace_v1.PhaseAScriptPreconditions,
    award_script_hash,
    computation_thread_policy,
    Some(prepared_step_datum(prepared_state)),
    0,
    0,
    transition,
    substituted_auxiliary,
    True,
    own_out_ref,
    semantic_transaction(prepared_state, input_script_hash),
  )
}

test semantic_handoff_rejects_cross_family_action_substitution() {
  let work_witness_cbor = #"80"
  let pre = pre_state(
    validation_trace_v1.PhaseAScriptPreconditions,
    work_witness_cbor,
  )
  let committed_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let substituted_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_c),
  }
  let state = resolution_state(pre, committed_transition)
  let auxiliary = no_auxiliary_data()
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    committed_transition,
    auxiliary,
  )
  let input_script_hash = resolver_at(
    phase_a_script_precondition_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  !validation_semantic_v1.continue_winning(
    validation_trace_v1.PhaseAScriptPreconditions,
    award_script_hash,
    computation_thread_policy,
    Some(prepared_step_datum(prepared_state)),
    0,
    0,
    substituted_transition,
    auxiliary,
    True,
    own_out_ref,
    semantic_transaction(prepared_state, input_script_hash),
  )
}
