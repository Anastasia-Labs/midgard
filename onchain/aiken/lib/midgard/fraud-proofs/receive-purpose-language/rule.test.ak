use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{OutputReference}
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
use midgard/validation_merkle_v1.{FrontierPeak}
use midgard/validation_trace_v1

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const h32 = #"1111111111111111111111111111111111111111111111111111111111111111"

const h28 = #"22222222222222222222222222222222222222222222222222222222"

const zero32 =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const forced_order_id =
  OutputReference {
    transaction_id: #"4444444444444444444444444444444444444444444444444444444444444444",
    output_index: 0,
  }

fn reason(execution_index: Int) {
  ReceivePurposePlutusV3Forbidden { execution_index }
}

fn subject_at(
  direction: Int,
  execution_index: Int,
) -> substrate.VerdictSubjectV1 {
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
      cbor.serialise(forced_order_id)
    },
    rejection_reason: if direction == substrate.direction_wrongful_acceptance {
      None
    } else {
      Some(reason(execution_index))
    },
  }
}

fn subject(direction: Int) -> substrate.VerdictSubjectV1 {
  subject_at(direction, 2)
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

test receive_midgard_v1_refuses_wrongful_acceptance() fail {
  rule.terminal_contradiction_v1(
    authenticated(substrate.direction_wrongful_acceptance, 128),
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

test receive_bind_refuses_empty_trace_root() fail {
  rule.bind_execution_v1(
    subject(substrate.direction_wrongful_acceptance),
    #"0001",
    1,
    2,
  ).execution_index == 2
}

test receive_bind_refuses_zero_trace_count() fail {
  rule.bind_execution_v1(
    subject(substrate.direction_wrongful_acceptance),
    h32,
    0,
    2,
  ).execution_index == 2
}

test receive_bind_refuses_negative_execution_index() fail {
  rule.bind_execution_v1(
    subject(substrate.direction_wrongful_acceptance),
    h32,
    1,
    -1,
  ).execution_index == -1
}

test receive_invalid_language_tag_is_not_forbidden() {
  !rule.forbidden_receive_language_holds_v1(3, 128)
}

// ---------------------------------------------------------------------------
// Step-02 authentication cases
// ---------------------------------------------------------------------------

/// Every knob a substitution test needs. `committed_*` values are the ones the
/// machine control and descriptor commit to; `supplied_*` values are what the
/// prover hands step 02.
type Case {
  direction: Int,
  committed_source_key: ByteArray,
  supplied_source_key: ByteArray,
  committed_purpose_kind: Int,
  committed_language: Int,
  supplied_language: Int,
  /// The execution index the bound state carries (and the leaf index of the
  /// committed purpose/execution leaves is always 0).
  bound_execution_index: Int,
  /// Number of committed purpose/execution leaves; the accused leaf sits at 0.
  leaf_count: Int,
  claimed_verdict: validation_trace_v1.ValidationVerdict,
  claimed_code_hash: ByteArray,
  bound_root_override: Option<ByteArray>,
  descriptor_verdict_override: Option<validation_trace_v1.ValidationVerdict>,
}

fn accepted_case() -> Case {
  Case {
    direction: substrate.direction_wrongful_acceptance,
    committed_source_key: #"00",
    supplied_source_key: #"00",
    committed_purpose_kind: 3,
    committed_language: 3,
    supplied_language: 3,
    bound_execution_index: 0,
    leaf_count: 1,
    claimed_verdict: validation_trace_v1.Accepted,
    claimed_code_hash: zero32,
    bound_root_override: None,
    descriptor_verdict_override: None,
  }
}

fn forced_case(language: Int) -> Case {
  Case {
    ..accepted_case(),
    direction: substrate.direction_wrongful_rejection,
    committed_language: language,
    supplied_language: language,
    claimed_verdict: validation_trace_v1.Rejected,
    claimed_code_hash: validation_trace_v1.hash_rejection_code(
      rejection_reason_v1.rejection_code_of(reason(0)),
    ),
  }
}

fn filler_leaves(count: Int) -> List<ByteArray> {
  if count <= 0 {
    []
  } else {
    [blake2b_256(cbor.serialise(count)), ..filler_leaves(count - 1)]
  }
}

fn authenticate_case(case: Case) -> rule.AuthenticatedReceiveLanguageV1 {
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    case.committed_purpose_kind,
    0,
    h28,
    #"aa",
  )
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    case.committed_source_key,
    case.committed_language,
    h28,
    1,
    h32,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    case.committed_language,
    purpose_leaf,
    source_leaf,
    h32,
  )
  let fillers = filler_leaves(case.leaf_count - 1)
  let purpose = validation_merkle_v1.build_frontier([purpose_leaf, ..fillers])
  let source = validation_merkle_v1.build_frontier([source_leaf])
  let execution = validation_merkle_v1.build_frontier(
    [execution_leaf, ..fillers],
  )
  let purpose_siblings = sibling_path(fillers, purpose_leaf)
  let execution_siblings = sibling_path(fillers, execution_leaf)
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
    purpose_count: case.leaf_count,
    purpose_peaks: purpose.peaks,
    output_count: 0,
    output_peaks: [],
    output_descriptor_peaks: [],
    mint_count: 0,
    mint_peaks: [],
    execution_count: case.leaf_count,
    execution_peaks: execution.peaks,
    execution_cursor: 0,
    language_bitmap: 0,
    resolution_schedule_hash: h32,
  }
  let accepted = case.direction == substrate.direction_wrongful_acceptance
  let event_key = if accepted {
    ledger_state.L2TransactionEventKey { tx_id }
  } else {
    ledger_state.ForcedTransactionEventKey { tx_order_id: forced_order_id }
  }
  let state = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: blake2b_256(cbor.serialise(event_key)),
    transaction_id: tx_id,
    transaction_commitment: h32,
    validation_context_hash: h32,
    source_kind: if accepted {
      validation_trace_v1.Normal
    } else {
      validation_trace_v1.Forced
    },
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
    rejection_code_hash: zero32,
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
    verdict: case.claimed_verdict,
    rejection_code_hash: case.claimed_code_hash,
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
    value: when case.descriptor_verdict_override is {
      None -> descriptor
      Some(verdict) ->
        validation_trace_v1.ValidationTraceDescriptorV1 {
          ..descriptor,
          verdict,
        }
    },
    proof: [],
  }
  let bound_root = when case.bound_root_override is {
    None -> membership.root
    Some(root) -> root
  }
  rule.authenticate_receive_language_v1(
    rule.bind_execution_v1(
      subject_at(case.direction, case.bound_execution_index),
      bound_root,
      1,
      case.bound_execution_index,
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
    purpose_siblings,
    0,
    0,
    case.supplied_source_key,
    case.supplied_language,
    1,
    h32,
    [],
    h32,
    execution_siblings,
  )
}

/// Sibling path for leaf 0 of `[leaf, ..fillers]` under the compact frontier:
/// leaf 0 lives in the highest peak, whose subtree is the first
/// `2^height` leaves, so its siblings are the right-hand subtree roots.
fn sibling_path(fillers: List<ByteArray>, leaf: ByteArray) -> List<ByteArray> {
  let count = 1 + count_of(fillers)
  let height = highest_bit_of(count, 0)
  siblings_at(height, [leaf, ..fillers])
}

fn count_of(items: List<ByteArray>) -> Int {
  when items is {
    [] -> 0
    [_, ..rest] -> 1 + count_of(rest)
  }
}

fn highest_bit_of(value: Int, height: Int) -> Int {
  if value < 2 {
    height
  } else {
    highest_bit_of(value / 2, height + 1)
  }
}

fn take(items: List<ByteArray>, n: Int) -> List<ByteArray> {
  if n <= 0 {
    []
  } else {
    when items is {
      [] -> []
      [head, ..rest] -> [head, ..take(rest, n - 1)]
    }
  }
}

fn drop(items: List<ByteArray>, n: Int) -> List<ByteArray> {
  if n <= 0 {
    items
  } else {
    when items is {
      [] -> []
      [_, ..rest] -> drop(rest, n - 1)
    }
  }
}

fn power(exponent: Int) -> Int {
  if exponent <= 0 {
    1
  } else {
    2 * power(exponent - 1)
  }
}

fn subtree_root(leaves: List<ByteArray>, height: Int) -> ByteArray {
  if height == 0 {
    expect [leaf] = leaves
    leaf
  } else {
    let half = power(height - 1)
    validation_merkle_v1.hash_branch(
      subtree_root(take(leaves, half), height - 1),
      subtree_root(drop(leaves, half), height - 1),
    )
  }
}

/// Bottom-up sibling list for leaf 0 of a full subtree of `2^height` leaves.
fn siblings_at(height: Int, leaves: List<ByteArray>) -> List<ByteArray> {
  if height == 0 {
    []
  } else {
    let half = power(height - 1)
    let right = subtree_root(take(drop(leaves, half), half), height - 1)
    list.concat(siblings_at(height - 1, take(leaves, half)), [right])
  }
}

test receive_authenticates_exact_purpose_source_language_and_execution() {
  let exact = authenticate_case(accepted_case())
  and {
    exact.purpose_kind == 3,
    exact.language_tag == 3,
    exact.bound.execution_index == 0,
    exact.prior_ledger_root == h32,
  }
}

test receive_authenticates_forced_rejected_descriptor() {
  let exact = authenticate_case(forced_case(0))
  and {
    exact.language_tag == 0,
    exact.bound.subject.direction == substrate.direction_wrongful_rejection,
    rule.terminal_contradiction_v1(exact),
  }
}

test receive_authenticates_forced_midgard_v1_descriptor() {
  let exact = authenticate_case(forced_case(128))
  and {
    exact.language_tag == 128,
    rule.terminal_contradiction_v1(exact),
  }
}

test receive_authenticates_accused_leaf_among_eight() {
  let exact = authenticate_case(Case { ..accepted_case(), leaf_count: 8 })
  exact.bound.execution_index == 0
}

test receive_source_substitution_refuses() fail {
  authenticate_case(Case { ..accepted_case(), supplied_source_key: #"01" }).source_key == #"01"
}

test receive_purpose_substitution_refuses() fail {
  authenticate_case(Case { ..accepted_case(), committed_purpose_kind: 2 }).purpose_kind == 3
}

test receive_language_substitution_refuses() fail {
  authenticate_case(Case { ..accepted_case(), supplied_language: 0 }).language_tag == 0
}

test receive_unknown_language_tag_refuses() fail {
  authenticate_case(
    Case { ..accepted_case(), committed_language: 2, supplied_language: 2 },
  ).language_tag == 2
}

test receive_validation_root_substitution_refuses() fail {
  authenticate_case(Case { ..accepted_case(), bound_root_override: Some(h32) }).language_tag == 3
}

test receive_descriptor_substitution_refuses() fail {
  authenticate_case(
    Case {
      ..accepted_case(),
      descriptor_verdict_override: Some(validation_trace_v1.Rejected),
    },
  ).language_tag == 3
}

test receive_accepted_direction_refuses_rejected_descriptor() fail {
  authenticate_case(
    Case {
      ..accepted_case(),
      claimed_verdict: validation_trace_v1.Rejected,
      claimed_code_hash: validation_trace_v1.hash_rejection_code(
        rejection_reason_v1.rejection_code_of(reason(0)),
      ),
    },
  ).language_tag == 3
}

test receive_forced_direction_refuses_accepted_descriptor() fail {
  authenticate_case(
    Case {
      ..forced_case(0),
      claimed_verdict: validation_trace_v1.Accepted,
      claimed_code_hash: zero32,
    },
  ).language_tag == 0
}

test receive_forced_rejection_code_substitution_refuses() fail {
  authenticate_case(
    Case {
      ..forced_case(0),
      claimed_code_hash: validation_trace_v1.hash_rejection_code(
        rejection_reason_v1.rejection_code_of(
          OutputNonCanonical { output_index: 0 },
        ),
      ),
    },
  ).language_tag == 0
}

test receive_execution_index_at_count_refuses() fail {
  // Adjacent over the bound: the accused index equals `execution_count`.
  authenticate_case(Case { ..accepted_case(), bound_execution_index: 1 }).language_tag == 3
}

test receive_forced_execution_index_at_count_refuses() fail {
  authenticate_case(Case { ..forced_case(0), bound_execution_index: 1 }).language_tag == 0
}

// ---------------------------------------------------------------------------
// Consensus-envelope maximum shape
// ---------------------------------------------------------------------------

fn fold_left(current: ByteArray, remaining: Int) -> ByteArray {
  if remaining == 0 {
    current
  } else {
    fold_left(validation_merkle_v1.hash_branch(current, h32), remaining - 1)
  }
}

fn fold_trace_left(current: ByteArray, remaining: Int) -> ByteArray {
  if remaining == 0 {
    current
  } else {
    fold_trace_left(
      validation_trace_v1.trace_branch_hash(current, h32),
      remaining - 1,
    )
  }
}

fn repeat(item: ByteArray, n: Int) -> List<ByteArray> {
  if n <= 0 {
    []
  } else {
    [item, ..repeat(item, n - 1)]
  }
}

/// Ascending peaks for count `2^(top + 1) - 1`: every height 0..top is
/// occupied, and leaf 0 lives in the height-`top` peak, whose hash is the leaf
/// folded with `top` right-hand siblings.
fn full_peaks(top: Int, leaf: ByteArray) -> List<FrontierPeak> {
  peaks_from(0, top, leaf)
}

fn peaks_from(height: Int, top: Int, leaf: ByteArray) -> List<FrontierPeak> {
  if height == top {
    [FrontierPeak { height, hash: fold_left(leaf, top) }]
  } else {
    [FrontierPeak { height, hash: h32 }, ..peaks_from(height + 1, top, leaf)]
  }
}

/// Authenticates the accused leaf with every frontier of the native-scripts
/// control at `2^(top + 1) - 1` leaves (`top + 1` peaks, `top` siblings) and
/// the trace proof at `max_machine_step_count` (32 siblings).
fn authenticate_wide(top: Int) -> rule.AuthenticatedReceiveLanguageV1 {
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(3, 0, h28, #"aa")
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    3,
    h28,
    1,
    h32,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    3,
    purpose_leaf,
    source_leaf,
    h32,
  )
  let count = power(top + 1) - 1
  let control = vm_machine_types.NativeScriptsControlV1 {
    compact_cbor: #"",
    witness_set_compact_cbor: #"",
    field_preimage_lengths_cbor: #"",
    context_cbor: #"",
    resolved_input_count: count,
    resolved_inputs_accumulator: h32,
    spend_input_count: count,
    resolved_item_peaks: full_peaks(top, h32),
    signer_count: 0,
    signer_frontier_commitment: h32,
    source_count: count,
    source_peaks: full_peaks(top, source_leaf),
    redeemer_count: count,
    redeemer_peaks: full_peaks(top, h32),
    purpose_count: count,
    purpose_peaks: full_peaks(top, purpose_leaf),
    output_count: count,
    output_peaks: full_peaks(top, h32),
    output_descriptor_peaks: full_peaks(top, h32),
    mint_count: count,
    mint_peaks: full_peaks(top, h32),
    execution_count: count,
    execution_peaks: full_peaks(top, execution_leaf),
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
    program_counter: validation_trace_v1.max_machine_step_count,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.NativeScripts,
      validation_trace_v1.max_machine_step_count,
      vm_witness.encode_native_scripts_control_v1(control),
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero32,
    ledger_delta_root: h32,
  }
  let state_hash = validation_trace_v1.hash_machine_state(state)
  let descriptor = validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: fold_trace_left(
      validation_trace_v1.trace_leaf_hash(state_hash),
      32,
    ),
    step_count: validation_trace_v1.max_machine_step_count,
    initial_state_hash: state_hash,
    terminal_state_hash: h32,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: zero32,
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
      subject_at(substrate.direction_wrongful_acceptance, 0),
      membership.root,
      1,
      0,
    ),
    membership,
    state,
    validation_trace_v1.ValidationTraceProof {
      state_index: 0,
      state_hash,
      siblings: repeat(h32, 32),
    },
    control,
    0,
    h28,
    #"aa",
    repeat(h32, top),
    0,
    0,
    #"00",
    3,
    1,
    h32,
    repeat(h32, top),
    h32,
    repeat(h32, top),
  )
}

/// The consensus-bounded maximum: no Midgard transaction field exceeds
/// 32,768 preimage bytes, so no frontier of the native-scripts control can
/// reach 4,096 leaves; 4,095 leaves (12 peaks, 11 siblings) covers every
/// realisable purpose, source, execution, input, output, redeemer and mint
/// frontier at once. This is the step-02 ExUnit row the fit ledger records.
test receive_authenticates_consensus_bounded_frontiers() {
  let authenticated = authenticate_wide(11)
  and {
    authenticated.language_tag == 3,
    rule.terminal_contradiction_v1(authenticated),
  }
}

/// The on-chain merkle envelope itself (`maximum_leaf_count`, 32 peaks and 31
/// siblings per frontier): unreachable by any admissible transaction, kept as
/// the validator's absolute cost ceiling.
test receive_authenticates_maximum_depth_memberships() {
  let authenticated = authenticate_wide(31)
  and {
    authenticated.language_tag == 3,
    rule.terminal_contradiction_v1(authenticated),
  }
}

// ---------------------------------------------------------------------------
// Cross-language golden vectors (twin: receive-purpose-language.test.ts)
// ---------------------------------------------------------------------------

test receive_bound_execution_golden_vector() {
  let bound = rule.bind_execution_v1(
    subject(substrate.direction_wrongful_acceptance),
    #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
    1,
    2,
  )
  cbor.serialise(bound) == #"d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff5820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f0102ff"
}

test receive_authenticated_accepted_golden_vector() {
  cbor.serialise(authenticated(substrate.direction_wrongful_acceptance, 3)) == #"d8799fd8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff5820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f0102ff5820101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f03020000410003581c000102030405060708090a0b0c0d0e0f101112131415161718191a1bff"
}

test receive_authenticated_forced_golden_vector() {
  cbor.serialise(authenticated(substrate.direction_wrongful_rejection, 0)) == #"d8799fd8799fd8799f0101015820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f5827d8799f5820444444444444444444444444444444444444444444444444444444444444444400ffd8799fd905219f02ffffff5820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f0102ff5820101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f03020000410000581c000102030405060708090a0b0c0d0e0f101112131415161718191a1bff"
}
