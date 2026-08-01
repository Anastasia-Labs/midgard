use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardTxInput, TxIsValid, native_tx_version_v1,
}
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

// Bound by the SDK cross-language test to buildFaultProofContracts over a
// freshly generated, compiler-pinned testnet plutus.json and deterministic
// fixture policy IDs. The same test independently applies the Phase-A and
// ScriptSources prepare validators to these exact semantic groups. These are
// parameterized production-builder fixture identities, not a claim about a
// live deployment.
fn phase_a_script_precondition_resolvers() -> List<ScriptHash> {
  [
    #"1a1f32b85daf623a14c33ef8caea42e52bce7593d80dccad3d23a822",
    #"2dec6c872e3cb1a59eb9084eb0d38671664e0af294dce185e89b286c",
  ]
}

fn input_sets_resolvers() -> List<ScriptHash> {
  [prepare_script_hash, award_script_hash]
}

fn script_source_resolvers() -> List<ScriptHash> {
  [
    #"4035db5caebf2cfe08c0e0c81362686b80ff56fed66f53798f86ed66",
    #"ddb3db4589769ec81966177deeb30856f5352bd860695ca0becd067e",
    #"c3dbab8b7c5f7731bea163be779b5be79bd3f27acd80cbfeeadfd180",
    #"b6ca49f39cea3aa7de25fd9c1dccb9d2846e20b824bd81f4990ee777",
    #"f890e913e3085154ffe503397f2e8520ffc872650626df4436ebc2db",
    #"d97b8f23efb31e32538c89a0d97009dcbbaffacbc73e097a86355ba2",
    #"37618d1d07d266924ac5f0631a1791c27d9bd76d3bacad893466f6ca",
    #"ba604acdd10a0101a6a293f6237a411f03a1013d984f279156657d5d",
    #"8cc9c85b11695228483e63d0a59684a989d8236ca2ec654f8e8ec0c1",
    #"be5e382cd992ec856b427e1dac37aad24d07765a09aefb7f8509fae7",
    #"01a692a30d8775625c42ff0fbe088b84610bd1aa0c9f6a7a01bf9a58",
    #"3c2d3271c5354eba7c4696e1f4a3dd89c30620637637ddc66accb8c3",
    #"ac35e730352b2ce5f579a6b757037014e745fa8a7561c40cacb58aa4",
    #"1e48d46d39a29e6158d40da127654959b1b64fdb88700969db0b1f94",
    #"854b55e73fab7ee4c3ddb27936eb6fdaa8cedac4eb6a7473c0941d4d",
    #"dabe082d6aec42decbb77360398bbb3bc45dbd83d91540d43b687917",
    #"83fdc76a2fd08bbe98de3cfc9c4887fdacc9569471732537a466d2f4",
    #"043dc4924e2e0dc55839fc6cace8110fc7c8aa280bc4788fbbb0026f",
    #"0258383a99b515a2eca2574a3d25a97d6f5006ffca02792393a8f582",
    #"0fdc2f9b584ce02df41e72a82d6330af28bbec210a8e2e5c7f3420aa",
    #"33dc97130cf917181b315531a5c08768dbea089465cc54b7f7ae28a2",
    #"bb64f40e7d07acd5b4602f683db4627f3e546d93eb4620b3fe25628b",
    #"bb64f40e7d07acd5b4602f683db4627f3e546d93eb4620b3fe25628b",
    #"d7ee04abf30408af3d7c5193414036d0f158d6eb3e4068a0a0c3c9d7",
    #"ab944248e01cdf2717f898cf164b172427f0b021435bbe1cc9692271",
    #"4ff19553c3331c3f181d9f8bcfc6368b9798008536be42c8dea49bcc",
    #"7b83d0ecf20121cfd74171b722ed15165316656b35c31377fcd0e7df",
    #"8db17bdc9ff6d976bce850777b8ded022d4f1944c2a67e006e093e60",
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
