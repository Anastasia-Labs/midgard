use aiken/interval.{Finite, Interval, IntervalBound, between}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, ValidityRange,
}
use env
use midgard/availability_challenge as availability
use midgard/common/utils
use midgard/ledger_state
use midgard/script_context_v1
use midgard/state_queue

const non_empty_root =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h28 = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

fn empty_header() -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    prev_utxos_root: env.empty_merkle_tree_root,
    utxos_root: env.empty_merkle_tree_root,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 1_000,
    end_time: 2_000,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: h28,
    operator_vkey: h28,
    protocol_version: ledger_state.protocol_version_v1,
  }
}

fn header_hash(header: ledger_state.HeaderV1) -> ByteArray {
  let header_data: Data = header
  utils.serialise_and_hash_28(header_data)
}

test state_queue_header_decoder_accepts_exactly_protocol_v1() {
  let header = empty_header()
  state_queue.decode_header_view(header) == header
}

test state_queue_header_decoder_rejects_the_genesis_protocol_sentinel() fail {
  let header = ledger_state.HeaderV1 {
    ..empty_header(),
    protocol_version: ledger_state.genesis_protocol_version,
  }
  state_queue.decode_header_view(header) == header
}

test state_queue_header_decoder_rejects_an_unknown_future_protocol() fail {
  let header = ledger_state.HeaderV1 {
    ..empty_header(),
    protocol_version: ledger_state.protocol_version_v1 + 1,
  }
  state_queue.decode_header_view(header) == header
}

test committed_header_identity_binds_each_admission_count_and_root() {
  let header = empty_header()
  let expected_hash = header_hash(header)
  and {
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, withdrawal_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, forced_transaction_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, l2_transaction_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, deposit_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, total_event_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, transition_step_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, validation_trace_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, withdrawals_root: non_empty_root },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 {
        ..header,
        forced_transactions_root: non_empty_root,
      },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, transactions_root: non_empty_root },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, deposits_root: non_empty_root },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, transition_trace_root: non_empty_root },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, event_to_step_root: non_empty_root },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, validation_traces_root: non_empty_root },
    ),
  }
}

// ---------------------------------------------------------------------------
// The commit time rules (C2): `state_queue.commit_bound_header_time_is_valid`.
//
// - R1: `block_time` equals the commit transaction's inclusive validity upper
//   bound, and `start_time < end_time <= block_time`.
// - R2: a header with L2 transactions has `end_time == block_time`.
// - R3: `slot_to_posix_time_v1(block_slot) + slot_length - 1 == block_time`.
//
// Every fixture uses the Preprod slot config and one commit window whose
// ledger-side exclusive upper bound is the start of slot `commit_slot + 1`, so
// its inclusive upper bound is the last millisecond of `commit_slot`.
// ---------------------------------------------------------------------------

const preprod_slot_config =
  script_context_v1.L1SlotConfigV1 {
    zero_time: 1_655_769_600_000,
    zero_slot: 86_400,
    slot_length: 1_000,
  }

/// The slot the commit window ends in, and its last millisecond.
const commit_slot = 44_317_279

const commit_block_time = 1_700_000_479_999

/// The commit transaction's validity window, as the ledger presents it: an
/// inclusive lower bound and an exclusive upper bound at a slot start. Its
/// inclusive upper bound is `commit_block_time`.
fn commit_window() -> Interval {
  Interval {
    lower_bound: IntervalBound {
      bound_type: Finite(1_700_000_000_000),
      is_inclusive: True,
    },
    upper_bound: IntervalBound {
      bound_type: Finite(commit_block_time + 1),
      is_inclusive: False,
    },
  }
}

/// A header covering `(start_time, end_time]` with `l2_transaction_count`
/// L2 transactions, committed in `block_slot`. The rule reads no other field.
fn timed_header(
  start_time: Int,
  end_time: Int,
  l2_transaction_count: Int,
  block_slot: Int,
) -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    ..empty_header(),
    start_time,
    end_time,
    l2_transaction_count,
    block_slot,
  }
}

fn commit_time_is_valid(header: ledger_state.HeaderV1, block_time: Int) -> Bool {
  state_queue.commit_bound_header_time_is_valid(
    header,
    block_time,
    preprod_slot_config,
    commit_window(),
  )
}

/// The fixture constants agree: `commit_slot` ends at `commit_block_time`.
test state_queue_commit_time_fixture_slot_ends_at_the_block_time() {
  and {
    script_context_v1.slot_to_posix_time_v1(preprod_slot_config, commit_slot) + 999 == commit_block_time,
    script_context_v1.slot_to_posix_time_v1(
      preprod_slot_config,
      commit_slot + 1,
    ) == commit_block_time + 1,
  }
}

/// Normal mode: the block covers up to its commit time.
test state_queue_commit_time_accepts_a_normal_block() {
  commit_time_is_valid(
    timed_header(1_700_000_000_000, commit_block_time, 1, commit_slot),
    commit_block_time,
  )
}

/// Catch-up mode: an event-only block may end anywhere before its commit
/// time, here six hours earlier and one millisecond earlier.
test state_queue_commit_time_accepts_an_event_only_catch_up_block() {
  and {
    commit_time_is_valid(
      timed_header(
        1_699_978_000_000,
        commit_block_time - 21_600_000,
        0,
        commit_slot,
      ),
      commit_block_time,
    ),
    commit_time_is_valid(
      timed_header(1_699_978_000_000, commit_block_time - 1, 0, commit_slot),
      commit_block_time,
    ),
  }
}

/// R1: a `block_time` one slot before the commit's upper bound is refused,
/// even with `block_slot` and `end_time` consistent with it.
test state_queue_commit_time_rejects_a_backdated_block_time() {
  !commit_time_is_valid(
    timed_header(
      1_700_000_000_000,
      commit_block_time - 1_000,
      0,
      commit_slot - 1,
    ),
    commit_block_time - 1_000,
  )
}

/// R1: a `block_time` one slot after the commit's upper bound is refused.
test state_queue_commit_time_rejects_a_future_block_time() {
  !commit_time_is_valid(
    timed_header(1_700_000_000_000, commit_block_time, 0, commit_slot + 1),
    commit_block_time + 1_000,
  )
}

/// R1: the covered interval may not end after the commit time, by one
/// millisecond or by ten years.
test state_queue_commit_time_rejects_an_end_after_the_block_time() {
  and {
    !commit_time_is_valid(
      timed_header(1_700_000_000_000, commit_block_time + 1, 0, commit_slot),
      commit_block_time,
    ),
    !commit_time_is_valid(
      timed_header(
        1_700_000_000_000,
        commit_block_time + 315_360_000_000,
        0,
        commit_slot,
      ),
      commit_block_time,
    ),
  }
}

/// R1: the covered interval must advance; one millisecond is enough.
test state_queue_commit_time_rejects_a_non_advancing_interval() {
  and {
    !commit_time_is_valid(
      timed_header(commit_block_time, commit_block_time, 0, commit_slot),
      commit_block_time,
    ),
    commit_time_is_valid(
      timed_header(commit_block_time - 1, commit_block_time, 0, commit_slot),
      commit_block_time,
    ),
  }
}

/// R1 reads a short validity range only.
test state_queue_commit_time_rejects_an_overlong_validity_range() fail {
  state_queue.commit_bound_header_time_is_valid(
    timed_header(1, env.max_validity_range_length + 1, 0, 0),
    env.max_validity_range_length + 1,
    preprod_slot_config,
    between(0, env.max_validity_range_length + 1),
  )
}

/// R2: a block carrying L2 transactions must cover up to its commit time; one
/// millisecond short is refused.
test state_queue_commit_time_rejects_an_l2_block_ending_before_its_block_time() {
  !commit_time_is_valid(
    timed_header(1_700_000_000_000, commit_block_time - 1, 1, commit_slot),
    commit_block_time,
  )
}

/// R3: `block_slot` must be exactly the commit time's slot.
test state_queue_commit_time_rejects_a_block_slot_that_disagrees() {
  and {
    !commit_time_is_valid(
      timed_header(1_700_000_000_000, commit_block_time, 0, commit_slot - 1),
      commit_block_time,
    ),
    !commit_time_is_valid(
      timed_header(1_700_000_000_000, commit_block_time, 0, commit_slot + 1),
      commit_block_time,
    ),
    !commit_time_is_valid(
      timed_header(1_700_000_000_000, commit_block_time, 0, -1),
      commit_block_time,
    ),
  }
}

/// R3: a commit time that is not the last millisecond of a slot has no
/// `block_slot`. The window is otherwise honest.
test state_queue_commit_time_rejects_a_block_time_inside_a_slot() {
  let mid_slot_window = Interval {
    lower_bound: IntervalBound {
      bound_type: Finite(1_700_000_000_000),
      is_inclusive: True,
    },
    upper_bound: IntervalBound {
      bound_type: Finite(commit_block_time),
      is_inclusive: False,
    },
  }
  and {
    !state_queue.commit_bound_header_time_is_valid(
      timed_header(1_700_000_000_000, commit_block_time - 1, 0, commit_slot),
      commit_block_time - 1,
      preprod_slot_config,
      mid_slot_window,
    ),
    !state_queue.commit_bound_header_time_is_valid(
      timed_header(1_700_000_000_000, commit_block_time - 1, 0, commit_slot - 1),
      commit_block_time - 1,
      preprod_slot_config,
      mid_slot_window,
    ),
  }
}

// ---------------------------------------------------------------------------
// Q60 due-ness adjudication.
//
// D-S12's concrete harm is that a far-future `end_time` widens the window in
// which an L1 event counts as "due" for the block being committed, so an
// operator could sweep events that have not happened yet into their block — or
// be convicted for omitting them. Due-ness is the rule
// `start_time < inclusion_time <= end_time`: `horizon.eligible` in
// `lib/midgard/event-history/horizon.ak` applies it to deposits and
// withdrawals, and `validate_forced_l1_event_fault` in
// `lib/midgard/fraud-proofs/transition-trace/proof.ak` applies it inline to
// forced transactions.
//
// Read the two tests below for exactly what they are. They run a *local
// restatement* of that rule, `inclusion_time_is_in_due_window`, a hand
// transcription that lives in this file. They document the harm model. They do
// NOT execute horizon.ak or proof.ak, and nothing here can detect the
// restatement drifting from the rule those modules apply.
//
// The other half of the argument — that the far-future `end_time` which would
// make these events due is refused at commit time — is proved against the real
// predicate by `state_queue_commit_time_rejects_an_end_after_the_block_time`
// and `state_queue_commit_time_accepts_a_normal_block`.
// ---------------------------------------------------------------------------

/// The due window `start_time < inclusion_time <= end_time`, restated locally.
fn inclusion_time_is_in_due_window(
  header_start_time: Int,
  header_end_time: Int,
  inclusion_time: Int,
) -> Bool {
  and {
    header_start_time < inclusion_time,
    inclusion_time <= header_end_time,
  }
}

/// Due-event class 1 of 2 — deposit, over the local restatement.
/// A forced transaction order is due on the same inclusion window alone, so
/// this case covers it too: a deposit that becomes includable one millisecond after the maximum accepted
/// `end_time` is not due for this block, and would be due under a far-future
/// `end_time`. Documents the harm; does not execute `horizon.ak`.
test state_queue_commit_end_time_bound_keeps_a_future_deposit_not_due() {
  and {
    !inclusion_time_is_in_due_window(
      1_700_000_000_000,
      1_700_000_480_000,
      1_700_000_480_001,
    ),
    inclusion_time_is_in_due_window(
      1_700_000_000_000,
      2_015_360_480_000,
      1_700_000_480_001,
    ),
  }
}

/// Due-event class 2 of 2 — withdrawal, over the local restatement, taken a
/// full year past the commit window to show the gap the anchor closes is
/// unbounded rather than an off-by-one. Documents the harm; does not execute
/// `horizon.ak`.
test state_queue_commit_end_time_bound_keeps_a_future_withdrawal_not_due() {
  and {
    !inclusion_time_is_in_due_window(
      1_700_000_000_000,
      1_700_000_480_000,
      1_731_536_480_000,
    ),
    inclusion_time_is_in_due_window(
      1_700_000_000_000,
      2_015_360_480_000,
      1_731_536_480_000,
    ),
  }
}

// ## State-queue element invariant: the DA core pins `reference_script`
//
// `da_availability_status_transition` is the one node-continuation core behind
// Apply (`validate_da_attestation_attachment`), Open
// (`validate_da_challenge_open_transition_v1`) and Close
// (`validate_da_availability_status_transition`). It requires
// `element_output_has_no_reference_script` of the node output, because
// `validate_completed_fraud_record` can only mark a node without one.

const da_core_policy =
  #"c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7"

const da_core_commitment_hash =
  #"c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8"

const da_core_challenge_asset_name =
  #"44414348c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9"

const da_core_terminal_commitment =
  #"cacacacacacacacacacacacacacacacacacacacacacacacacacacacacacacaca"

const da_core_probe_reference_script =
  #"f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3"

/// The DA core node's commit time: a catch-up commit, three seconds after
/// `empty_header().end_time`, so a clock read from the header's end and one
/// read from `block_time` disagree.
const da_core_block_time = 5_000

/// The node for `empty_header()` at the state-queue address, carrying `status`.
fn da_core_node(status: availability.StateQueueStatusV1) -> Output {
  let header = empty_header()
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Node(
      state_queue.StateQueueNode {
        header,
        block_time: da_core_block_time,
        da_attestation: status,
        proven_fraud: None,
      },
    ),
    link: None,
  }
  Output {
    address: address.from_script(da_core_policy),
    value: assets.from_lovelace(2_000_000)
      |> assets.add(
          da_core_policy,
          bytearray.concat(
            state_queue.block_asset_name_prefix,
            header_hash(header),
          ),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn da_core_inputs(status: availability.StateQueueStatusV1) -> List<Input> {
  [
    Input {
      output_reference: OutputReference {
        transaction_id: #"0303030303030303030303030303030303030303030303030303030303030303",
        output_index: 0,
      },
      output: da_core_node(status),
    },
  ]
}

fn with_probe_reference_script(output: Output) -> Output {
  Output { ..output, reference_script: Some(da_core_probe_reference_script) }
}

fn da_core_close(rewrite: fn(Output) -> Output) -> Bool {
  let challenged = availability.Challenged {
    commitment_hash: da_core_commitment_hash,
    challenge_asset_name: da_core_challenge_asset_name,
  }
  let published = availability.Published {
    terminal_commitment: da_core_terminal_commitment,
  }
  state_queue.validate_da_availability_status_transition(
    da_core_inputs(challenged),
    [rewrite(da_core_node(published))],
    da_core_policy,
    0,
    0,
    header_hash(empty_header()),
    challenged,
    published,
  )
}

fn da_core_open(rewrite: fn(Output) -> Output) -> Int {
  state_queue.validate_da_challenge_open_transition_v1(
    da_core_inputs(
      availability.Attested { commitment_hash: da_core_commitment_hash },
    ),
    [
      rewrite(
        da_core_node(
          availability.Challenged {
            commitment_hash: da_core_commitment_hash,
            challenge_asset_name: da_core_challenge_asset_name,
          },
        ),
      ),
    ],
    da_core_policy,
    0,
    0,
    header_hash(empty_header()),
    da_core_commitment_hash,
    da_core_challenge_asset_name,
  )
}

fn da_core_apply(rewrite: fn(Output) -> Output) -> Bool {
  da_core_apply_within(
    rewrite,
    between(da_core_block_time, da_core_block_time + 1_000),
  )
}

fn da_core_apply_within(
  rewrite: fn(Output) -> Output,
  validity_range: ValidityRange,
) -> Bool {
  state_queue.validate_da_attestation_attachment(
    da_core_inputs(state_queue.no_da_attestation),
    [
      rewrite(
        da_core_node(
          availability.Attested { commitment_hash: da_core_commitment_hash },
        ),
      ),
    ],
    da_core_policy,
    0,
    0,
    header_hash(empty_header()),
    da_core_commitment_hash,
    validity_range,
  )
}

test state_queue_element_reference_script_predicate_is_exact() {
  let node = da_core_node(state_queue.no_da_attestation)
  and {
    state_queue.element_output_has_no_reference_script(node),
    !state_queue.element_output_has_no_reference_script(
      with_probe_reference_script(node),
    ),
  }
}

/// Controls for the three DA core users: each accepts the honest continuation,
/// whose node output carries no reference script.
test da_core_controls_accept_node_output_without_reference_script() {
  and {
    da_core_apply(fn(output) { output }),
    da_core_open(fn(output) { output }) == da_core_block_time,
    da_core_close(fn(output) { output }),
  }
}

/// Control: `da_core_controls_accept_node_output_without_reference_script`.
/// Refused by `expect element_output_has_no_reference_script(output)` in
/// `da_availability_status_transition`, reached through
/// `validate_da_attestation_attachment` (Apply).
test da_core_apply_rejects_node_output_reference_script() fail {
  da_core_apply(with_probe_reference_script)
}

/// Control: `da_core_controls_accept_node_output_without_reference_script`.
/// Refused by `expect element_output_has_no_reference_script(output)` in
/// `da_availability_status_transition`, reached through
/// `validate_da_challenge_open_transition_v1` (Open).
test da_core_open_rejects_node_output_reference_script() fail {
  da_core_open(with_probe_reference_script) == da_core_block_time
}

/// Control: `da_core_controls_accept_node_output_without_reference_script`.
/// Refused by `expect element_output_has_no_reference_script(output)` in
/// `da_availability_status_transition`, reached through
/// `validate_da_availability_status_transition` (Close).
test da_core_close_rejects_node_output_reference_script() fail {
  da_core_close(with_probe_reference_script)
}

// ## The DA core pins `block_time` (C2)
//
// Apply, Open and Close rebuild the node datum field by field. `block_time`
// is the clock of the attestation deadline, the challenge window, merge
// maturity and fraud-record maturity, so a transition that rewrote it could
// move every one of them. Each negative below is the honest control
// `da_core_controls_accept_node_output_without_reference_script` with the
// output node's `block_time` moved by one millisecond, refused by
// `expect output_block.block_time == input_block.block_time` in
// `da_availability_status_transition`.

/// Move the output node's `block_time` by `delta`, leaving the rest intact.
fn with_block_time_shifted(delta: Int) -> fn(Output) -> Output {
  fn(output: Output) {
    expect InlineDatum(datum_data) = output.datum
    expect
        linked_list.Element { data: linked_list.Node(node), link }: state_queue.Datum
      = datum_data
    let shifted: state_queue.Datum = linked_list.Element {
      data: linked_list.Node(
        state_queue.StateQueueNode {
          ..node,
          block_time: node.block_time + delta,
        },
      ),
      link,
    }
    Output { ..output, datum: InlineDatum(shifted) }
  }
}

test da_core_apply_rejects_a_later_block_time() fail {
  da_core_apply(with_block_time_shifted(1))
}

test da_core_apply_rejects_an_earlier_block_time() fail {
  da_core_apply(with_block_time_shifted(-1))
}

test da_core_open_rejects_a_later_block_time() fail {
  da_core_open(with_block_time_shifted(1)) == da_core_block_time
}

test da_core_open_rejects_an_earlier_block_time() fail {
  da_core_open(with_block_time_shifted(-1)) == da_core_block_time
}

test da_core_close_rejects_a_later_block_time() fail {
  da_core_close(with_block_time_shifted(1))
}

test da_core_close_rejects_an_earlier_block_time() fail {
  da_core_close(with_block_time_shifted(-1))
}

/// The attestation deadline runs from `block_time`: an Apply whose upper
/// bound is exactly `block_time + da_attestation_timeout_v1` is accepted,
/// although it is past `end_time + da_attestation_timeout_v1`.
test da_core_apply_accepts_the_deadline_measured_from_block_time() {
  let deadline = da_core_block_time + state_queue.da_attestation_timeout_v1
  and {
    deadline > empty_header().end_time + state_queue.da_attestation_timeout_v1,
    da_core_apply_within(
      fn(output) { output },
      between(deadline - 1_000, deadline),
    ),
  }
}

/// One millisecond past the deadline measured from `block_time` is refused by
/// `inclusive_tx_upper_bound <= block_time + da_attestation_timeout_v1` in
/// `validate_da_attestation_attachment`.
test da_core_apply_rejects_one_millisecond_past_the_deadline() {
  let deadline = da_core_block_time + state_queue.da_attestation_timeout_v1
  !da_core_apply_within(
    fn(output) { output },
    between(deadline - 999, deadline + 1),
  )
}
