use aiken/interval.{Finite, Interval, IntervalBound, between}
use env
use midgard/common/utils
use midgard/ledger_state
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

test commit_header_time_accepts_the_normalized_commit_upper_bound() {
  state_queue.commit_bound_header_time_is_valid(
    1_000,
    2_000,
    Interval {
      lower_bound: IntervalBound {
        bound_type: Finite(1_900),
        is_inclusive: True,
      },
      upper_bound: IntervalBound {
        bound_type: Finite(2_001),
        is_inclusive: False,
      },
    },
  )
}

test commit_header_time_rejects_a_backdated_end() {
  !state_queue.commit_bound_header_time_is_valid(
    1_000,
    1_500,
    between(1_900, 2_000),
  )
}

test commit_header_time_rejects_an_overlong_validity_range() fail {
  state_queue.commit_bound_header_time_is_valid(
    1,
    env.max_validity_range_length + 1,
    between(0, env.max_validity_range_length + 1),
  )
}

test commit_header_time_rejects_a_future_end() {
  !state_queue.commit_bound_header_time_is_valid(
    1_000,
    2_500,
    between(1_900, 2_000),
  )
}

test commit_header_time_rejects_a_non_advancing_event_interval() {
  !state_queue.commit_bound_header_time_is_valid(
    2_000,
    2_000,
    between(1_900, 2_000),
  )
}
