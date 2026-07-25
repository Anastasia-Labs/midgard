use midgard/validation_dispute_v1
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const h_d = #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const h_e = #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const h_f = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn descriptor4(
  a: ByteArray,
  b: ByteArray,
  c: ByteArray,
  d: ByteArray,
) -> validation_trace_v1.ValidationTraceDescriptorV1 {
  let left = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(a),
    validation_trace_v1.trace_leaf_hash(b),
  )
  let right = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(c),
    validation_trace_v1.trace_leaf_hash(d),
  )
  validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_branch_hash(left, right),
    step_count: 3,
    initial_state_hash: a,
    terminal_state_hash: d,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: zero,
  }
}

fn proof4(
  index: Int,
  state: ByteArray,
  sibling: ByteArray,
  other_pair_root: ByteArray,
) -> validation_trace_v1.ValidationTraceProof {
  validation_trace_v1.ValidationTraceProof {
    state_index: index,
    state_hash: state,
    siblings: [validation_trace_v1.trace_leaf_hash(sibling), other_pair_root],
  }
}

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

test bisection_keeps_the_equal_left_boundary_and_first_different_right_boundary() {
  let operator = descriptor4(h_a, h_b, h_c, h_d)
  let challenger = descriptor4(h_a, h_b, h_e, h_f)
  let opened = validation_dispute_v1.open(operator, challenger, 100)
  let operator_right = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(h_c),
    validation_trace_v1.trace_leaf_hash(h_d),
  )
  let challenger_right = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(h_e),
    validation_trace_v1.trace_leaf_hash(h_f),
  )
  let with_operator_1 = validation_dispute_v1.reveal_operator_midpoint(
    opened,
    proof4(1, h_b, h_a, operator_right),
    101,
  )
  let after_equal_1 = validation_dispute_v1.reveal_challenger_midpoint(
    with_operator_1,
    proof4(1, h_b, h_a, challenger_right),
    102,
  )
  let operator_left = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(h_a),
    validation_trace_v1.trace_leaf_hash(h_b),
  )
  let with_operator_2 = validation_dispute_v1.reveal_operator_midpoint(
    after_equal_1,
    proof4(2, h_c, h_d, operator_left),
    103,
  )
  let ready = validation_dispute_v1.reveal_challenger_midpoint(
    with_operator_2,
    proof4(2, h_e, h_f, operator_left),
    104,
  )
  and {
    ready.low_index == 1,
    ready.high_index == 2,
    ready.agreed_low_hash == h_b,
    ready.operator_high_hash == h_c,
    ready.challenger_high_hash == h_e,
    ready.turn == validation_dispute_v1.ReadyForOneStep,
    ready.round == 2,
  }
}

test timeout_assigns_loss_to_the_party_that_owes_the_move() {
  let operator = descriptor4(h_a, h_b, h_c, h_d)
  let challenger = descriptor4(h_a, h_b, h_e, h_f)
  let opened = validation_dispute_v1.open(operator, challenger, 100)
  let operator_right = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(h_c),
    validation_trace_v1.trace_leaf_hash(h_d),
  )
  let awaiting_challenger = validation_dispute_v1.reveal_operator_midpoint(
    opened,
    proof4(1, h_b, h_a, operator_right),
    101,
  )
  and {
    validation_dispute_v1.timeout_winner(opened, 300_101) == validation_dispute_v1.ChallengerWins,
    validation_dispute_v1.timeout_winner(awaiting_challenger, 300_102) == validation_dispute_v1.OperatorWins,
  }
}

test dispute_open_maturity_boundary_is_exact_and_fails_closed() {
  let challenged_block_end_time = 1_000_000
  let maturity_duration = 604_800_000
  let final_open_time = challenged_block_end_time + maturity_duration - validation_dispute_v1.max_dispute_duration_milliseconds
  and {
    validation_dispute_v1.can_open_before_maturity(
      final_open_time,
      challenged_block_end_time,
      maturity_duration,
    ),
    !validation_dispute_v1.can_open_before_maturity(
      final_open_time + 1,
      challenged_block_end_time,
      maturity_duration,
    ),
    !validation_dispute_v1.can_open_before_maturity(
      final_open_time,
      challenged_block_end_time,
      validation_dispute_v1.max_dispute_duration_milliseconds - 1,
    ),
    !validation_dispute_v1.can_open_before_maturity(
      -1,
      challenged_block_end_time,
      maturity_duration,
    ),
  }
}

test one_step_boundary_is_authenticated_before_resolver_handoff() {
  let base = fixture_pre_state()
  let work_witness_cbor = #"80"
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..base,
    phase: validation_trace_v1.CompactBinding,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CompactBinding,
      0,
      work_witness_cbor,
    ),
  }
  let operator_successor = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.InputSets,
    program_counter: 1,
    work_root: h_c,
  }
  let challenger_successor = validation_trace_v1.ValidationMachineStateV1 {
    ..operator_successor,
    work_root: h_d,
  }
  let pre_hash = validation_trace_v1.hash_machine_state(pre)
  let operator_post_hash = validation_trace_v1.hash_machine_state(
    operator_successor,
  )
  let challenger_post_hash = validation_trace_v1.hash_machine_state(
    challenger_successor,
  )
  let operator = descriptor2(pre_hash, operator_post_hash)
  let challenger = descriptor2(pre_hash, challenger_post_hash)
  let ready = validation_dispute_v1.open(operator, challenger, 100)
  let operator_post = validation_trace_v1.ValidationTraceProof {
    state_index: 1,
    state_hash: operator_post_hash,
    siblings: [validation_trace_v1.trace_leaf_hash(pre_hash)],
  }
  let challenger_post = validation_trace_v1.ValidationTraceProof {
    state_index: 1,
    state_hash: challenger_post_hash,
    siblings: [validation_trace_v1.trace_leaf_hash(pre_hash)],
  }
  let resolution = validation_resolution_v1.prepare(
    ready,
    pre,
    operator_post,
    challenger_post,
  )
  and {
    resolution.version == validation_resolution_v1.resolution_version,
    resolution.pre_state == pre,
    resolution.operator_successor_hash == operator_post_hash,
    resolution.challenger_successor_hash == challenger_post_hash,
    validation_resolution_v1.resolver_index(pre.phase) == 1,
    validation_resolution_v1.state_is_well_formed(resolution),
  }
}

test resolver_routing_is_total_for_nonterminal_phases_and_rejects_terminal() {
  and {
    validation_resolution_v1.resolver_index(validation_trace_v1.CanonicalDecode) == 0,
    validation_resolution_v1.resolver_index(validation_trace_v1.CompactBinding) == 1,
    validation_resolution_v1.resolver_index(
      validation_trace_v1.StaticLedgerRules,
    ) == 2,
    validation_resolution_v1.resolver_index(validation_trace_v1.InputSets) == 3,
    validation_resolution_v1.resolver_index(validation_trace_v1.Signatures) == 4,
    validation_resolution_v1.resolver_index(
      validation_trace_v1.PhaseANativeScripts,
    ) == 5,
    validation_resolution_v1.resolver_index(
      validation_trace_v1.PhaseAScriptPreconditions,
    ) == 6,
    validation_resolution_v1.resolver_index(validation_trace_v1.ResolveInputs) == 7,
    validation_resolution_v1.resolver_index(validation_trace_v1.ScriptSources) == 8,
    validation_resolution_v1.resolver_index(validation_trace_v1.NativeScripts) == 9,
    validation_resolution_v1.resolver_index(validation_trace_v1.ScriptIntegrity) == 10,
    validation_resolution_v1.resolver_index(validation_trace_v1.Cek) == 11,
    validation_resolution_v1.resolver_index(validation_trace_v1.ValueAndMint) == 12,
    validation_resolution_v1.resolver_index(validation_trace_v1.LedgerDelta) == 13,
    validation_resolution_v1.resolver_index(validation_trace_v1.Terminal) == -1,
    validation_resolution_v1.resolver_count == 14,
  }
}

test malformed_resolution_states_fail_closed() {
  let pre = fixture_pre_state()
  let valid = validation_resolution_v1.ValidationResolutionStateV1 {
    version: validation_resolution_v1.resolution_version,
    pre_state: pre,
    operator_successor_hash: h_b,
    challenger_successor_hash: h_c,
  }
  and {
    validation_resolution_v1.state_is_well_formed(valid),
    !validation_resolution_v1.state_is_well_formed(
      validation_resolution_v1.ValidationResolutionStateV1 {
        ..valid,
        version: 2,
      },
    ),
    !validation_resolution_v1.state_is_well_formed(
      validation_resolution_v1.ValidationResolutionStateV1 {
        ..valid,
        operator_successor_hash: h_c,
      },
    ),
    !validation_resolution_v1.state_is_well_formed(
      validation_resolution_v1.ValidationResolutionStateV1 {
        ..valid,
        challenger_successor_hash: #"00",
      },
    ),
    !validation_resolution_v1.state_is_well_formed(
      validation_resolution_v1.ValidationResolutionStateV1 {
        ..valid,
        pre_state: validation_trace_v1.ValidationMachineStateV1 {
          ..pre,
          phase: validation_trace_v1.Terminal,
        },
      },
    ),
  }
}

test challenger_resolution_requires_exact_hashes_and_a_unique_valid_successor() {
  let pre = fixture_pre_state()
  let operator = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 1,
    work_root: h_b,
  }
  let challenger = validation_trace_v1.ValidationMachineStateV1 {
    ..operator,
    work_root: h_c,
  }
  let state = validation_resolution_v1.ValidationResolutionStateV1 {
    version: validation_resolution_v1.resolution_version,
    pre_state: pre,
    operator_successor_hash: validation_trace_v1.hash_machine_state(operator),
    challenger_successor_hash: validation_trace_v1.hash_machine_state(
      challenger,
    ),
  }
  and {
    validation_resolution_v1.challenger_uniquely_wins(
      state,
      operator,
      challenger,
      False,
      True,
    ),
    !validation_resolution_v1.challenger_uniquely_wins(
      state,
      operator,
      challenger,
      True,
      True,
    ),
    !validation_resolution_v1.challenger_uniquely_wins(
      state,
      operator,
      challenger,
      False,
      False,
    ),
    !validation_resolution_v1.challenger_uniquely_wins(
      state,
      challenger,
      operator,
      False,
      True,
    ),
  }
}
