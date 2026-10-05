use midgard/validation_direct_step_v1
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const h_d = #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const h_e = #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const h_f = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn descriptor2(
  initial: ByteArray,
  terminal: ByteArray,
) -> validation_trace_v1.ValidationTraceDescriptorV1 {
  validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_branch_hash(
      validation_trace_v1.trace_leaf_hash(initial),
      validation_trace_v1.trace_leaf_hash(terminal),
    ),
    step_count: 1,
    initial_state_hash: initial,
    terminal_state_hash: terminal,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: zero,
  }
}

fn fixture_pre_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: h_b,
    transaction_commitment: h_c,
    validation_context_hash: h_d,
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: h_e,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 0,
    work_root: h_f,
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_a,
  }
}

fn evidence(
  phase: validation_trace_v1.ValidationPhase,
) -> (
  validation_trace_v1.ValidationTraceDescriptorV1,
  validation_direct_step_v1.CommittedStepEvidenceV1,
) {
  let pre_state = validation_trace_v1.ValidationMachineStateV1 {
    ..fixture_pre_state(),
    phase,
  }
  let pre_hash = validation_trace_v1.hash_machine_state(pre_state)
  (
    descriptor2(pre_hash, h_b),
    validation_direct_step_v1.CommittedStepEvidenceV1 {
      pre_state,
      pre_proof: validation_trace_v1.ValidationTraceProof {
        state_index: 0,
        state_hash: pre_hash,
        siblings: [validation_trace_v1.trace_leaf_hash(h_b)],
      },
      post_proof: validation_trace_v1.ValidationTraceProof {
        state_index: 1,
        state_hash: h_b,
        siblings: [validation_trace_v1.trace_leaf_hash(pre_hash)],
      },
      challenger_successor_hash: h_c,
    },
  )
}

test authenticated_committed_step_routes_without_challenger_trace() {
  let (descriptor, witness) = evidence(validation_trace_v1.CanonicalDecode)
  let result = validation_direct_step_v1.prepare(descriptor, witness)
  and {
    result.pre_state == witness.pre_state,
    result.operator_successor_hash == h_b,
    result.challenger_successor_hash == h_c,
    validation_resolution_v1.state_is_well_formed(result),
  }
}

test honest_successor_cannot_be_convicted() fail {
  let (descriptor, witness) = evidence(validation_trace_v1.CanonicalDecode)
  expect _ = validation_direct_step_v1.prepare(
    descriptor,
    validation_direct_step_v1.CommittedStepEvidenceV1 {
      ..witness,
      challenger_successor_hash: h_b,
    },
  )
}

test substituted_pre_state_is_refused() fail {
  let (descriptor, witness) = evidence(validation_trace_v1.CanonicalDecode)
  expect _ = validation_direct_step_v1.prepare(
    descriptor,
    validation_direct_step_v1.CommittedStepEvidenceV1 {
      ..witness,
      pre_state: validation_trace_v1.ValidationMachineStateV1 {
        ..witness.pre_state,
        program_counter: 1,
      },
    },
  )
}

test substituted_post_proof_is_refused() fail {
  let (descriptor, witness) = evidence(validation_trace_v1.CanonicalDecode)
  expect _ = validation_direct_step_v1.prepare(
    descriptor,
    validation_direct_step_v1.CommittedStepEvidenceV1 {
      ..witness,
      post_proof: validation_trace_v1.ValidationTraceProof {
        ..witness.post_proof,
        state_hash: h_d,
      },
    },
  )
}

test non_adjacent_indices_are_refused() fail {
  let (descriptor, witness) = evidence(validation_trace_v1.CanonicalDecode)
  expect _ = validation_direct_step_v1.prepare(
    descriptor,
    validation_direct_step_v1.CommittedStepEvidenceV1 {
      ..witness,
      post_proof: validation_trace_v1.ValidationTraceProof {
        ..witness.post_proof,
        state_index: 0,
      },
    },
  )
}

test cek_remains_on_existing_dispute_path() fail {
  let (descriptor, witness) = evidence(validation_trace_v1.Cek)
  expect _ = validation_direct_step_v1.prepare(descriptor, witness)
}

test signatures_remains_unresolved_until_successor_is_unique() fail {
  let (descriptor, witness) = evidence(validation_trace_v1.Signatures)
  expect _ = validation_direct_step_v1.prepare(descriptor, witness)
}

test terminal_remains_on_terminal_padding_path() fail {
  let (descriptor, witness) = evidence(validation_trace_v1.Terminal)
  expect _ = validation_direct_step_v1.prepare(descriptor, witness)
}
