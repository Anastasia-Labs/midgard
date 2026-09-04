use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/receive_purpose_language/rule
use midgard/ledger_state
use midgard/rejection_reason_v1.{
  OutputNonCanonical, ReceivePurposePlutusV3Forbidden,
}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/witness as vm_witness
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const h32 = #"1111111111111111111111111111111111111111111111111111111111111111"

const h28 = #"22222222222222222222222222222222222222222222222222222222"

fn subject(direction: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction,
    source_kind: if direction == substrate.direction_wrongful_acceptance {
      substrate.source_kind_accepted
    } else {
      substrate.source_kind_forced
    },
    transaction_id: tx_id,
    source_key: if direction == substrate.direction_wrongful_acceptance {
      #""
    } else {
      #"01"
    },
    rejection_reason: if direction == substrate.direction_wrongful_acceptance {
      None
    } else {
      Some(ReceivePurposePlutusV3Forbidden { execution_index: 2 })
    },
  }
}

fn authenticated(
  direction: Int,
  language_tag: Int,
) -> rule.AuthenticatedReceiveLanguageV1 {
  rule.AuthenticatedReceiveLanguageV1 {
    bound: rule.bind_execution_v1(
      subject(direction),
      #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
      1,
      2,
    ),
    prior_ledger_root: #"101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f",
    purpose_kind: 3,
    purpose_index: 2,
    source_index: 0,
    origin_kind: 0,
    source_key: #"00",
    language_tag,
    script_hash: #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b",
  }
}

test receive_plutus_v3_convicts_wrongful_acceptance() {
  rule.terminal_contradiction_v1(
    authenticated(substrate.direction_wrongful_acceptance, 3),
  )
}

test receive_native_contradicts_wrongful_rejection() {
  rule.terminal_contradiction_v1(
    authenticated(substrate.direction_wrongful_rejection, 0),
  )
}

test receive_midgard_v1_contradicts_wrongful_rejection() {
  rule.terminal_contradiction_v1(
    authenticated(substrate.direction_wrongful_rejection, 128),
  )
}

test receive_plutus_v3_refuses_wrongful_rejection() fail {
  rule.terminal_contradiction_v1(
    authenticated(substrate.direction_wrongful_rejection, 3),
  )
}

test receive_native_refuses_wrongful_acceptance() fail {
  rule.terminal_contradiction_v1(
    authenticated(substrate.direction_wrongful_acceptance, 0),
  )
}

test receive_reason_coordinate_substitution_refuses() fail {
  rule.bind_execution_v1(
    subject(substrate.direction_wrongful_rejection),
    #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
    1,
    1,
  ).execution_index == 1
}

test receive_reason_constructor_substitution_refuses() fail {
  let bad = substrate.VerdictSubjectV1 {
    ..subject(substrate.direction_wrongful_rejection),
    rejection_reason: Some(OutputNonCanonical { output_index: 2 }),
  }
  rule.bind_execution_v1(
    bad,
    #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
    1,
    2,
  ).execution_index == 2
}

test receive_invalid_language_tag_is_not_forbidden() {
  !rule.forbidden_receive_language_holds_v1(3, 128)
}

fn authenticate_exact_receive(
  committed_source_key: ByteArray,
  supplied_source_key: ByteArray,
  committed_purpose_kind: Int,
  language_tag: Int,
) -> rule.AuthenticatedReceiveLanguageV1 {
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    committed_purpose_kind,
    0,
    h28,
    #"aa",
  )
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    committed_source_key,
    language_tag,
    h28,
    1,
    h32,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    language_tag,
    purpose_leaf,
    source_leaf,
    h32,
  )
  let purpose = validation_merkle_v1.build_frontier([purpose_leaf])
  let source = validation_merkle_v1.build_frontier([source_leaf])
  let execution = validation_merkle_v1.build_frontier([execution_leaf])
  let control = vm_machine_types.NativeScriptsControlV1 {
    compact_cbor: #"",
    witness_set_compact_cbor: #"",
    field_preimage_lengths_cbor: #"",
    context_cbor: #"",
    resolved_input_count: 0,
    resolved_inputs_accumulator: h32,
    spend_input_count: 0,
    resolved_item_peaks: [],
    signer_count: 0,
    signer_frontier_commitment: h32,
    source_count: 1,
    source_peaks: source.peaks,
    redeemer_count: 0,
    redeemer_peaks: [],
    purpose_count: 1,
    purpose_peaks: purpose.peaks,
    output_count: 0,
    output_peaks: [],
    output_descriptor_peaks: [],
    mint_count: 0,
    mint_peaks: [],
    execution_count: 1,
    execution_peaks: execution.peaks,
    execution_cursor: 0,
    language_bitmap: 0,
    resolution_schedule_hash: h32,
  }
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let state = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: blake2b_256(cbor.serialise(event_key)),
    transaction_id: tx_id,
    transaction_commitment: h32,
    validation_context_hash: h32,
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: h32,
    phase: validation_trace_v1.NativeScripts,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.NativeScripts,
      7,
      vm_witness.encode_native_scripts_control_v1(control),
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
    ledger_delta_root: h32,
  }
  let state_hash = validation_trace_v1.hash_machine_state(state)
  let descriptor = validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_leaf_hash(state_hash),
    step_count: 0,
    initial_state_hash: state_hash,
    terminal_state_hash: state_hash,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
  }
  let phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      cbor.serialise(event_key),
      cbor.serialise(descriptor),
      [],
    ),
  )
  let membership = transition_trace.RootMembershipProof {
    domain: transition_trace.ValidationTracesRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.ValidationTracesRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: event_key,
    value: descriptor,
    proof: [],
  }
  rule.authenticate_receive_language_v1(
    rule.bind_execution_v1(
      subject(substrate.direction_wrongful_acceptance),
      membership.root,
      1,
      0,
    ),
    membership,
    state,
    validation_trace_v1.ValidationTraceProof {
      state_index: 0,
      state_hash,
      siblings: [],
    },
    control,
    0,
    h28,
    #"aa",
    [],
    0,
    0,
    supplied_source_key,
    language_tag,
    1,
    h32,
    [],
    h32,
    [],
  )
}

test receive_authenticates_exact_purpose_source_language_and_execution() {
  let exact = authenticate_exact_receive(#"00", #"00", 3, 3)
  and {
    exact.purpose_kind == 3,
    exact.language_tag == 3,
    exact.bound.execution_index == 0,
  }
}

test receive_source_substitution_refuses() fail {
  authenticate_exact_receive(#"00", #"01", 3, 3).source_key == #"01"
}

test receive_purpose_substitution_refuses() fail {
  authenticate_exact_receive(#"00", #"00", 2, 3).purpose_kind == 3
}
