use aiken/builtin
use aiken/crypto
use aiken/interval.{between}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use env
use midgard/availability_challenge as availability
use midgard/correction_lock
use midgard/ledger_state
use midgard/settlement
use midgard/state_queue
use state_queue as state_queue_validator

const canonical_root =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const substituted_root =
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const canonical_header_key =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const substituted_header_key =
  #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const settlement_policy =
  #"11111111111111111111111111111111111111111111111111111111"

const other_mint_policy =
  #"22222222222222222222222222222222222222222222222222222222"

fn canonical_header() -> state_queue.HeaderView {
  ledger_state.HeaderV1 {
    prev_utxos_root: canonical_root,
    utxos_root: canonical_root,
    withdrawals_root: canonical_root,
    forced_transactions_root: canonical_root,
    transactions_root: canonical_root,
    deposits_root: canonical_root,
    transition_trace_root: canonical_root,
    event_to_step_root: canonical_root,
    validation_traces_root: canonical_root,
    withdrawal_count: 1,
    forced_transaction_count: 1,
    l2_transaction_count: 1,
    deposit_count: 1,
    total_event_count: 4,
    transition_step_count: 4,
    validation_trace_count: 2,
    start_time: 1_000,
    end_time: 2_000,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: canonical_header_key,
    operator_vkey: canonical_header_key,
    protocol_version: ledger_state.protocol_version_v1,
  }
}

fn empty_material_header() -> state_queue.HeaderView {
  ledger_state.HeaderV1 {
    ..canonical_header(),
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
  }
}

fn single_withdrawal_header() -> state_queue.HeaderView {
  ledger_state.HeaderV1 {
    ..empty_material_header(),
    withdrawals_root: canonical_root,
    transition_trace_root: canonical_root,
    event_to_step_root: canonical_root,
    withdrawal_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }
}

fn single_forced_transaction_header() -> state_queue.HeaderView {
  ledger_state.HeaderV1 {
    ..empty_material_header(),
    forced_transactions_root: canonical_root,
    transition_trace_root: canonical_root,
    event_to_step_root: canonical_root,
    validation_traces_root: canonical_root,
    forced_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
    validation_trace_count: 1,
  }
}

fn single_l2_transaction_header() -> state_queue.HeaderView {
  ledger_state.HeaderV1 {
    ..empty_material_header(),
    transactions_root: canonical_root,
    transition_trace_root: canonical_root,
    event_to_step_root: canonical_root,
    validation_traces_root: canonical_root,
    l2_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
    validation_trace_count: 1,
  }
}

fn single_deposit_header() -> state_queue.HeaderView {
  ledger_state.HeaderV1 {
    ..empty_material_header(),
    deposits_root: canonical_root,
    transition_trace_root: canonical_root,
    event_to_step_root: canonical_root,
    deposit_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }
}

fn canonical_commitments() -> state_queue_validator.MergeCommitments {
  let header = canonical_header()
  state_queue_validator.MergeCommitments {
    withdrawals_root: header.withdrawals_root,
    forced_transactions_root: header.forced_transactions_root,
    transactions_root: header.transactions_root,
    deposits_root: header.deposits_root,
    transition_trace_root: header.transition_trace_root,
    event_to_step_root: header.event_to_step_root,
    validation_traces_root: header.validation_traces_root,
    withdrawal_count: header.withdrawal_count,
    forced_transaction_count: header.forced_transaction_count,
    l2_transaction_count: header.l2_transaction_count,
    deposit_count: header.deposit_count,
    total_event_count: header.total_event_count,
    transition_step_count: header.transition_step_count,
    validation_trace_count: header.validation_trace_count,
  }
}

test merge_commitments_production_guard_accepts_exact_header() {
  state_queue_validator.merge_commitments_match_header(
    canonical_commitments(),
    canonical_header(),
  )
}

test merge_commitments_production_guard_rejects_each_independent_field_substitution() {
  let header = canonical_header()
  let commitments = canonical_commitments()
  and {
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        withdrawals_root: substituted_root,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        forced_transactions_root: substituted_root,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        transactions_root: substituted_root,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        deposits_root: substituted_root,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        transition_trace_root: substituted_root,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        event_to_step_root: substituted_root,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        validation_traces_root: substituted_root,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        withdrawal_count: commitments.withdrawal_count + 1,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        forced_transaction_count: commitments.forced_transaction_count + 1,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        l2_transaction_count: commitments.l2_transaction_count + 1,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        deposit_count: commitments.deposit_count + 1,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        total_event_count: commitments.total_event_count + 1,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        transition_step_count: commitments.transition_step_count + 1,
      },
      header,
    ),
    !state_queue_validator.merge_commitments_match_header(
      state_queue_validator.MergeCommitments {
        ..commitments,
        validation_trace_count: commitments.validation_trace_count + 1,
      },
      header,
    ),
  }
}

test merge_settlement_production_guard_binds_presence_and_identity() {
  let header = canonical_header()
  let empty_header = empty_material_header()
  let withdrawal_header = single_withdrawal_header()
  let forced_transaction_header = single_forced_transaction_header()
  let l2_transaction_header = single_l2_transaction_header()
  let deposit_header = single_deposit_header()
  and {
    state_queue_validator.merge_settlement_binding_matches_header(
      header,
      canonical_header_key,
      Some(canonical_header_key),
    ),
    !state_queue_validator.merge_settlement_binding_matches_header(
      header,
      canonical_header_key,
      None,
    ),
    !state_queue_validator.merge_settlement_binding_matches_header(
      header,
      canonical_header_key,
      Some(substituted_header_key),
    ),
    state_queue_validator.merge_settlement_binding_matches_header(
      empty_header,
      canonical_header_key,
      None,
    ),
    !state_queue_validator.merge_settlement_binding_matches_header(
      empty_header,
      canonical_header_key,
      Some(canonical_header_key),
    ),
    state_queue_validator.merge_settlement_binding_matches_header(
      withdrawal_header,
      canonical_header_key,
      Some(canonical_header_key),
    ),
    !state_queue_validator.merge_settlement_binding_matches_header(
      withdrawal_header,
      canonical_header_key,
      None,
    ),
    state_queue_validator.merge_settlement_binding_matches_header(
      forced_transaction_header,
      canonical_header_key,
      Some(canonical_header_key),
    ),
    !state_queue_validator.merge_settlement_binding_matches_header(
      forced_transaction_header,
      canonical_header_key,
      None,
    ),
    state_queue_validator.merge_settlement_binding_matches_header(
      l2_transaction_header,
      canonical_header_key,
      Some(canonical_header_key),
    ),
    !state_queue_validator.merge_settlement_binding_matches_header(
      l2_transaction_header,
      canonical_header_key,
      None,
    ),
    state_queue_validator.merge_settlement_binding_matches_header(
      deposit_header,
      canonical_header_key,
      Some(canonical_header_key),
    ),
    !state_queue_validator.merge_settlement_binding_matches_header(
      deposit_header,
      canonical_header_key,
      None,
    ),
  }
}

test merge_settlement_production_guard_binds_the_exact_redeemer_route() {
  let spawn_redeemer: settlement.MintRedeemer = settlement.Spawn {
    settlement_id: canonical_header_key,
    output_index: 0,
    state_queue_merge_redeemer_index: 0,
    hub_ref_input_index: 0,
  }
  let spawn_redeemer_data: Data = spawn_redeemer
  let redeemers = [Pair(Mint(settlement_policy), spawn_redeemer_data)]
  and {
    state_queue_validator.merge_settlement_id_at_route(
      redeemers,
      settlement_policy,
      Some(0),
    ) == Some(canonical_header_key),
    state_queue_validator.merge_settlement_id_at_route(
      redeemers,
      settlement_policy,
      None,
    ) == None,
  }
}

test merge_settlement_production_guard_rejects_the_wrong_redeemer_route() fail {
  let spawn_redeemer: settlement.MintRedeemer = settlement.Spawn {
    settlement_id: canonical_header_key,
    output_index: 0,
    state_queue_merge_redeemer_index: 0,
    hub_ref_input_index: 0,
  }
  let spawn_redeemer_data: Data = spawn_redeemer
  state_queue_validator.merge_settlement_id_at_route(
    [
      Pair(Mint(settlement_policy), spawn_redeemer_data),
      Pair(Mint(other_mint_policy), spawn_redeemer_data),
    ],
    settlement_policy,
    Some(1),
  ) == Some(canonical_header_key)
}

// ---------------------------------------------------------------------------
// Merge DA gate: a block merges only once its data is attested (`Attested`)
// or proven available by a closed challenge (`Published`). Each test runs the
// production merge check over the same transaction; only the head node's DA
// status differs.
// ---------------------------------------------------------------------------

const merge_state_queue_policy =
  #"33333333333333333333333333333333333333333333333333333333"

const merge_hub_oracle_policy =
  #"44444444444444444444444444444444444444444444444444444444"

const merge_correction_lock_script =
  #"55555555555555555555555555555555555555555555555555555555"

const merge_da_attestation_policy =
  #"66666666666666666666666666666666666666666666666666666666"

const merge_confirmed_header_hash =
  #"77777777777777777777777777777777777777777777777777777777"

const merge_commitment_hash =
  #"c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0"

const merge_challenge_asset_name =
  #"44414348eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const merge_terminal_commitment =
  #"d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0"

fn merge_out_ref(output_index: Int) -> OutputReference {
  OutputReference {
    transaction_id: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    output_index,
  }
}

fn merge_confirmed_state() -> ledger_state.ConfirmedState {
  ledger_state.ConfirmedState {
    header_hash: merge_confirmed_header_hash,
    prev_header_hash: canonical_header_key,
    utxo_root: canonical_root,
    start_time: 0,
    end_time: 1_000,
    protocol_version: ledger_state.protocol_version_v1,
  }
}

fn merge_root_output(
  confirmed_state: ledger_state.ConfirmedState,
  link: Option<ByteArray>,
) -> Output {
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Root(confirmed_state),
    link,
  }
  Output {
    address: address.from_script(merge_state_queue_policy),
    value: assets.from_lovelace(2_000_000)
      |> assets.add(
          merge_state_queue_policy,
          state_queue.confirmed_state_asset_name,
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn merge_head_key() -> ByteArray {
  crypto.blake2b_224(builtin.serialise_data(empty_material_header()))
}

fn merge_head_input(da_attestation: availability.StateQueueStatusV1) -> Input {
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Node(
      state_queue.StateQueueNode {
        header: empty_material_header(),
        da_attestation,
        proven_fraud: None,
      },
    ),
    link: None,
  }
  Input {
    output_reference: merge_out_ref(1),
    output: Output {
      address: address.from_script(merge_state_queue_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(
            merge_state_queue_policy,
            bytearray.concat(
              state_queue.block_asset_name_prefix,
              merge_head_key(),
            ),
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn merge_idle_correction_lock() -> Input {
  Input {
    output_reference: merge_out_ref(2),
    output: Output {
      address: address.from_script(merge_correction_lock_script),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(merge_hub_oracle_policy, correction_lock.asset_name, 1),
      datum: InlineDatum(correction_lock.Idle),
      reference_script: None,
    },
  }
}

/// Merge the head block (no L2 material, so no settlement) into the confirmed
/// state once its maturity has elapsed, with the head carrying `da_attestation`.
fn run_merge_with_head_status(
  da_attestation: availability.StateQueueStatusV1,
) -> Bool {
  let header = empty_material_header()
  let head_key = merge_head_key()
  let confirmed_state = merge_confirmed_state()
  let maturity = header.end_time + ledger_state.block_maturity_duration_v1
  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [
      Input {
        output_reference: merge_out_ref(0),
        output: merge_root_output(confirmed_state, Some(head_key)),
      },
      merge_head_input(da_attestation),
    ],
    outputs: [
      merge_root_output(
        ledger_state.ConfirmedState {
          header_hash: head_key,
          prev_header_hash: confirmed_state.header_hash,
          utxo_root: header.utxos_root,
          start_time: confirmed_state.start_time,
          end_time: header.end_time,
          protocol_version: header.protocol_version,
        },
        None,
      ),
    ],
    reference_inputs: [merge_idle_correction_lock()],
    mint: assets.from_asset(
      merge_state_queue_policy,
      bytearray.concat(state_queue.block_asset_name_prefix, head_key),
      -1,
    ),
    validity_range: between(maturity, maturity + 1_000),
  }
  state_queue_validator.validate_merge_v1(
    merge_hub_oracle_policy,
    merge_correction_lock_script,
    settlement_policy,
    merge_da_attestation_policy,
    merge_state_queue_policy,
    tx,
    head_key,
    merge_out_ref(0),
    0,
    None,
    header.withdrawals_root,
    header.forced_transactions_root,
    header.transactions_root,
    header.deposits_root,
    header.transition_trace_root,
    header.event_to_step_root,
    header.validation_traces_root,
    header.withdrawal_count,
    header.forced_transaction_count,
    header.l2_transaction_count,
    header.deposit_count,
    header.total_event_count,
    header.transition_step_count,
    header.validation_trace_count,
  )
}

test merge_da_gate_accepts_attested_head() {
  run_merge_with_head_status(
    availability.Attested { commitment_hash: merge_commitment_hash },
  )
}

test merge_da_gate_accepts_published_head() {
  run_merge_with_head_status(
    availability.Published { terminal_commitment: merge_terminal_commitment },
  )
}

/// Refused by the merge DA gate: a live challenge blocks the merge.
test merge_da_gate_rejects_challenged_head() fail {
  run_merge_with_head_status(
    availability.Challenged {
      commitment_hash: merge_commitment_hash,
      challenge_asset_name: merge_challenge_asset_name,
    },
  )
}

/// Refused by the merge DA gate: an unattested block never merges.
test merge_da_gate_rejects_unattested_head() fail {
  run_merge_with_head_status(availability.Unattested)
}
