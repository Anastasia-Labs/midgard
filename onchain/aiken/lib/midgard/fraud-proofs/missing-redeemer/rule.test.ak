use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{OutputReference}
use midgard/fraud_proofs/missing_redeemer/rule
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{RedeemerMissing, rejection_code_of}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

const h28 = #"33333333333333333333333333333333333333333333333333333333"

const h32 = #"4444444444444444444444444444444444444444444444444444444444444444"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn subject(direction: Int, kind: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction,
    source_kind: if direction == 0 {
      0
    } else {
      1
    },
    transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
    source_key: if direction == 0 {
      #""
    } else {
      #"820058200000000000000000000000000000000000000000000000000000000000000000"
    },
    rejection_reason: if direction == 0 {
      None
    } else {
      Some(RedeemerMissing { purpose_kind: kind, purpose_index: 7 })
    },
  }
}

fn authenticated(direction: Int, kind: Int) -> rule.AuthenticatedPurposeV1 {
  let bound = rule.bind_purpose_v1(
    subject(direction, kind),
    #"1111111111111111111111111111111111111111111111111111111111111111",
    #"2222222222222222222222222222222222222222222222222222222222222222",
    1,
    kind,
    7,
  )
  expect Some(tag) = rule.redeemer_tag_for_purpose_kind_v1(kind)
  rule.AuthenticatedPurposeV1 {
    bound,
    purpose_count: 4,
    redeemer_tag: tag,
    required_script_hash: #"33333333333333333333333333333333333333333333333333333333",
    source_index: 0,
    source_language_tag: 3,
    source_leaf: #"4444444444444444444444444444444444444444444444444444444444444444",
  }
}

test purpose_kind_mapping_is_consensus_exact() {
  and {
    rule.redeemer_tag_for_purpose_kind_v1(0) == Some(0),
    rule.redeemer_tag_for_purpose_kind_v1(1) == Some(1),
    rule.redeemer_tag_for_purpose_kind_v1(2) == Some(3),
    rule.redeemer_tag_for_purpose_kind_v1(3) == Some(6),
    rule.redeemer_tag_for_purpose_kind_v1(4) == None,
  }
}

test only_plutus_languages_can_require_a_redeemer() {
  and {
    rule.is_redeemer_bearing_language_v1(3),
    rule.is_redeemer_bearing_language_v1(128),
    !rule.is_redeemer_bearing_language_v1(0),
    !rule.is_redeemer_bearing_language_v1(2),
  }
}

test complete_absence_and_present_terminals_are_opposite() {
  let auth = authenticated(0, 0)
  let absent = rule.decision_v1(
    rule.ScanStateV1 {
      authenticated: auth,
      checkpoint_hash: #"5555555555555555555555555555555555555555555555555555555555555555",
      cursor: 2,
      item_count: 2,
      found: False,
    },
  )
  let present = rule.decision_v1(
    rule.ScanStateV1 {
      authenticated: auth,
      checkpoint_hash: #"5555555555555555555555555555555555555555555555555555555555555555",
      cursor: 1,
      item_count: 2,
      found: True,
    },
  )
  and {
    rule.terminal_contradiction_v1(absent),
    !rule.terminal_contradiction_v1(present),
  }
}

test forced_presence_contradicts_for_every_purpose_kind() {
  let check = fn(kind) {
    rule.terminal_contradiction_v1(
      rule.decision_v1(
        rule.ScanStateV1 {
          authenticated: authenticated(1, kind),
          checkpoint_hash: #"5555555555555555555555555555555555555555555555555555555555555555",
          cursor: 1,
          item_count: 8,
          found: True,
        },
      ),
    )
  }
  and {
    check(0),
    check(1),
    check(2),
    check(3),
  }
}

test alternate_pointer_does_not_satisfy_target() {
  let scanned = rule.scan_item_v1(
    rule.initial_scan_v1(
      authenticated(0, 0),
      #"5555555555555555555555555555555555555555555555555555555555555555",
      1,
    ),
    0,
    #"8400084100820102",
  )
  and {
    !scanned.found,
    rule.terminal_contradiction_v1(rule.decision_v1(scanned)),
  }
}

fn stage_ten_authentication(
  direction: Int,
  language_tag: Int,
  stage: Int,
  substitute_source_key: Bool,
  malformed_source_frontier: Bool,
  wrong_machine_source_kind: Bool,
  wrong_descriptor_verdict: Bool,
  wrong_trace_state: Bool,
  wrong_work_root: Bool,
  wrong_transaction_id: Bool,
  wrong_purpose_coordinate: Bool,
  wrong_rejection_hash: Bool,
  prior_root: ByteArray,
) -> rule.AuthenticatedPurposeV1 {
  let out_ref = OutputReference { transaction_id: zero, output_index: 0 }
  let event_key = if direction == substrate.direction_wrongful_acceptance {
    ledger_state.L2TransactionEventKey { tx_id: zero }
  } else {
    ledger_state.ForcedTransactionEventKey { tx_order_id: out_ref }
  }
  let exact_subject = substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction,
    source_kind: if direction == substrate.direction_wrongful_acceptance {
      substrate.source_kind_accepted
    } else {
      substrate.source_kind_forced
    },
    transaction_id: if wrong_transaction_id {
      h32
    } else {
      zero
    },
    source_key: if direction == substrate.direction_wrongful_acceptance {
      #""
    } else {
      cbor.serialise(out_ref)
    },
    rejection_reason: if direction == substrate.direction_wrongful_acceptance {
      None
    } else {
      Some(RedeemerMissing { purpose_kind: 0, purpose_index: 7 })
    },
  }
  let source_key = #"00"
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    source_key,
    language_tag,
    h28,
    1,
    h32,
  )
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 7, h28, #"aa")
  let sources = validation_merkle_v1.build_frontier([source_leaf])
  let purposes = validation_merkle_v1.build_frontier([purpose_leaf])
  let discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    purpose_cursor: 0,
    source_cursor: 1,
    redeemer_cursor: 0,
    current_purpose_kind: 0,
    current_purpose_index: 7,
    current_script_hash: h28,
    current_subject: #"aa",
    matched_source_index: 0,
    matched_language_tag: language_tag,
    matched_source_leaf: source_leaf,
    used_inline_bitmap: 0,
    used_redeemer_bitmap: 0,
    redeemer_item_control_hash: #"",
    execution_count: 0,
    execution_peaks: [],
  }
  let committed_control = validation_machine_v1.ScriptSourcesControlV1 {
    compact_cbor: #"",
    witness_set_compact_cbor: #"",
    field_preimage_lengths_cbor: #"",
    context_cbor: #"",
    resolved_input_count: 0,
    resolved_inputs_accumulator: h32,
    signer_count: 0,
    signer_frontier_commitment: h32,
    resolved_item_peaks: [],
    stage,
    source_count: 1,
    source_peaks: sources.peaks,
    redeemer_count: 0,
    redeemer_peaks: [],
    replay_cursor: 0,
    replay_accumulator: h32,
    replay_remaining_schedule_hash: h32,
    spend_index: 0,
    purpose_count: 1,
    purpose_peaks: purposes.peaks,
    output_cursor: 0,
    output_count: 0,
    output_peaks: [],
    output_total_count: 0,
    receive_scan: validation_machine_v1.empty_receive_purpose_scan_control(),
    source_total_count: 1,
    redeemer_total_count: 0,
    observer_scan: validation_machine_v1.empty_observer_purpose_scan_control(),
    discovery,
    output_proof: None,
    pending_source_cbor: #"",
    mint_fold: validation_machine_v1.empty_mint_fold_control(),
    resolution_schedule_hash: h32,
  }
  let supplied_control = if malformed_source_frontier {
    validation_machine_v1.ScriptSourcesControlV1 {
      ..committed_control,
      source_peaks: [],
    }
  } else {
    committed_control
  }
  let expected_source_kind = if direction == substrate.direction_wrongful_acceptance {
    validation_trace_v1.Normal
  } else {
    validation_trace_v1.Forced
  }
  let machine_state = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: blake2b_256(cbor.serialise(event_key)),
    transaction_id: zero,
    transaction_commitment: native_tx_proof_commitment_v1(#"", #"", #""),
    validation_context_hash: validation_trace_v1.hash_validation_context(#""),
    source_kind: if wrong_machine_source_kind {
      validation_trace_v1.Forced
    } else {
      expected_source_kind
    },
    prior_ledger_root: prior_root,
    phase: validation_trace_v1.ScriptSources,
    program_counter: 17,
    work_root: if wrong_work_root {
      h32
    } else {
      validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        17,
        validation_machine_v1.encode_script_sources_discovery_witness(
          committed_control,
          10,
          discovery,
        ),
      )
    },
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h32,
  }
  let state_hash = validation_trace_v1.hash_machine_state(machine_state)
  let correct_verdict = if direction == substrate.direction_wrongful_acceptance {
    validation_trace_v1.Accepted
  } else {
    validation_trace_v1.Rejected
  }
  let descriptor = validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_leaf_hash(state_hash),
    step_count: 0,
    initial_state_hash: state_hash,
    terminal_state_hash: state_hash,
    verdict: if wrong_descriptor_verdict {
      validation_trace_v1.Rejected
    } else {
      correct_verdict
    },
    rejection_code_hash: if wrong_rejection_hash {
      h32
    } else if direction == substrate.direction_wrongful_acceptance {
      zero
    } else {
      validation_trace_v1.hash_rejection_code(
        rejection_code_of(RedeemerMissing { purpose_kind: 0, purpose_index: 7 }),
      )
    },
  }
  let phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      cbor.serialise(event_key),
      cbor.serialise(descriptor),
      [],
    ),
  )
  let root = transition_trace.commit_counted_root(
    transition_trace.ValidationTracesRootDomain,
    phas_root,
    1,
  )
  rule.authenticate_stage_ten_purpose_v1(
    rule.bind_purpose_v1(exact_subject, blake2b_256(#""), root, 1, 0, 7),
    transition_trace.RootMembershipProof {
      domain: transition_trace.ValidationTracesRootDomain,
      root,
      phas_root,
      count: 1,
      key: event_key,
      value: descriptor,
      proof: [],
    },
    machine_state,
    validation_trace_v1.ValidationTraceProof {
      state_index: 0,
      state_hash: if wrong_trace_state {
        h32
      } else {
        state_hash
      },
      siblings: [],
    },
    supplied_control,
    if wrong_purpose_coordinate {
      1
    } else {
      0
    },
    [],
    0,
    if substitute_source_key {
      #"01"
    } else {
      source_key
    },
    language_tag,
    h28,
    1,
    h32,
    [],
  )
}

test accepted_stage_ten_authenticates_exact_purpose_and_source() {
  let authenticated = stage_ten_authentication(
    0,
    3,
    10,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    h32,
  )
  and {
    authenticated.source_language_tag == 3,
    authenticated.required_script_hash == h28,
  }
}

test forced_stage_ten_authenticates_exact_reason_and_later_event_root() {
  let later_event_prior_root = #"5555555555555555555555555555555555555555555555555555555555555555"
  let authenticated = stage_ten_authentication(
    1,
    128,
    10,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    later_event_prior_root,
  )
  and {
    authenticated.source_language_tag == 128,
    authenticated.bound.subject.direction == 1,
  }
}

test native_language_stage_ten_refuses() fail {
  stage_ten_authentication(
    0,
    0,
    10,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    h32,
  ).source_language_tag == 0
}

test wrong_stage_refuses() fail {
  stage_ten_authentication(
    0,
    3,
    9,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    h32,
  ).source_language_tag == 3
}

test alternate_source_key_refuses() fail {
  stage_ten_authentication(
    0,
    3,
    10,
    True,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    h32,
  ).source_language_tag == 3
}

test malformed_source_frontier_refuses() fail {
  stage_ten_authentication(
    0,
    3,
    10,
    False,
    True,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    h32,
  ).source_language_tag == 3
}

test wrong_machine_source_kind_refuses() fail {
  stage_ten_authentication(
    0,
    3,
    10,
    False,
    False,
    True,
    False,
    False,
    False,
    False,
    False,
    False,
    h32,
  ).source_language_tag == 3
}

test wrong_descriptor_verdict_refuses() fail {
  stage_ten_authentication(
    0,
    3,
    10,
    False,
    False,
    False,
    True,
    False,
    False,
    False,
    False,
    False,
    h32,
  ).source_language_tag == 3
}

test wrong_trace_state_refuses() fail {
  stage_ten_authentication(
    0,
    3,
    10,
    False,
    False,
    False,
    False,
    True,
    False,
    False,
    False,
    False,
    h32,
  ).source_language_tag == 3
}

test wrong_work_root_refuses() fail {
  stage_ten_authentication(
    0,
    3,
    10,
    False,
    False,
    False,
    False,
    False,
    True,
    False,
    False,
    False,
    h32,
  ).source_language_tag == 3
}

test wrong_machine_transaction_refuses() fail {
  stage_ten_authentication(
    0,
    3,
    10,
    False,
    False,
    False,
    False,
    False,
    False,
    True,
    False,
    False,
    h32,
  ).source_language_tag == 3
}

test wrong_purpose_coordinate_refuses() fail {
  stage_ten_authentication(
    0,
    3,
    10,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    True,
    False,
    h32,
  ).source_language_tag == 3
}

test wrong_forced_rejection_hash_refuses() fail {
  stage_ten_authentication(
    1,
    3,
    10,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    False,
    True,
    h32,
  ).source_language_tag == 3
}
