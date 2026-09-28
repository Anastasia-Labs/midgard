use aiken/builtin
use aiken/collection/list
use aiken/crypto
use aiken/interval.{Finite, Interval, IntervalBound, between}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use availability_challenge as availability_validator
use availability_challenge_yields as availability_yields
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Spend, Transaction,
  Withdraw,
}
use env
use midgard/availability_challenge as availability
use midgard/availability_challenge_yield as yielding
use midgard/da_attestation_types as da_attestation
use midgard/da_bond_pool as pool
use midgard/hub_oracle as hub
use midgard/ledger_state.{ConfirmedState, HeaderV1, protocol_version_v1}
use midgard/state_queue
use midgard/validation_merkle_v1

const hub_policy = #"11111111111111111111111111111111111111111111111111111111"

const state_queue_policy =
  #"22222222222222222222222222222222222222222222222222222222"

const availability_policy =
  #"33333333333333333333333333333333333333333333333333333333"

const da_policy = #"34343434343434343434343434343434343434343434343434343434"

const da_bond_pool_policy =
  #"35353535353535353535353535353535353535353535353535353535"

const miscellaneous_policy =
  #"44444444444444444444444444444444444444444444444444444444"

/// The block operator, and the non-challenger key the negatives use.
const operator = #"55555555555555555555555555555555555555555555555555555555"

const challenger = #"66666666666666666666666666666666666666666666666666666666"

const terminal =
  #"7777777777777777777777777777777777777777777777777777777777777777"

const empty_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

/// A 28-byte script hash no fixture uses, attached as a state-queue node
/// output's reference script by the DA-core reference-script negatives.
const node_reference_script_probe =
  #"f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3"

const close_fee = 100

/// Inclusive upper bound of the `between(0, 1_000)` range the default open
/// fixtures use. An open anchors `opened_at` there, not at the lower bound.
const open_inclusive_upper = 1_000

fn parameters() -> availability.ParametersV1 {
  availability.ParametersV1 {
    response_geometry: availability.ResponseGeometryV1 {
      chunk_byte_length: 14_020,
      tranche_byte_length: 4 * 1024 * 1024,
      max_tranche_count: 16,
    },
    da_bond_lovelace: 10_000_000_000,
    challenger_bond_lovelace: 10_000_000_000,
    max_open_fee_lovelace: 500_000,
    max_publication_fee_lovelace: 500_000,
    max_settlement_fee_lovelace: 500_000,
    max_close_fee_lovelace: 1_000_000,
    max_timeout_fee_lovelace: 1_200_000,
    da_slash_penalty_lovelace: 2_500_000_000,
    da_bond_min_top_up_lovelace: 100_000_000,
    da_bond_pool_floor_lovelace: 5_000_000,
    challenge_record_lovelace: 27_000_000,
  }
}

fn outref(output_index: Int) -> OutputReference {
  OutputReference {
    transaction_id: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    output_index,
  }
}

fn header() -> HeaderV1 {
  HeaderV1 {
    prev_utxos_root: empty_root,
    utxos_root: empty_root,
    withdrawals_root: empty_root,
    forced_transactions_root: empty_root,
    transactions_root: empty_root,
    deposits_root: empty_root,
    transition_trace_root: empty_root,
    event_to_step_root: empty_root,
    validation_traces_root: empty_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 0,
    end_time: 1,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: miscellaneous_policy,
    operator_vkey: operator,
    protocol_version: protocol_version_v1,
  }
}

fn header_hash() -> ByteArray {
  let header_data: Data = header()
  crypto.blake2b_224(builtin.serialise_data(header_data))
}

fn descriptor() -> availability.TrancheDescriptorV1 {
  let leaf = availability.chunk_leaf_hash_v1(
    0,
    0,
    0,
    1,
    crypto.blake2b_256(#"01"),
  )
  let frontier = validation_merkle_v1.build_frontier([leaf])
  availability.TrancheDescriptorV1 {
    tranche_index: 0,
    start_offset: 0,
    byte_length: 1,
    chunk_count: frontier.count,
    chunk_commitment: validation_merkle_v1.frontier_commitment(
      frontier.count,
      frontier.peaks,
    ),
    terminal_accumulator: terminal,
  }
}

fn commitment_for(deployment_identity: ByteArray) -> availability.CommitmentV1 {
  availability.CommitmentV1 {
    version: availability.commitment_version_v1,
    deployment_identity,
    header_hash: header_hash(),
    payload_byte_length: 1,
    response_geometry: parameters().response_geometry,
    tranche_descriptors: [descriptor()],
  }
}

fn commitment() -> availability.CommitmentV1 {
  commitment_for(hub_policy)
}

fn maximum_descriptor(index: Int) -> availability.TrancheDescriptorV1 {
  let index_data: Data = index
  availability.TrancheDescriptorV1 {
    tranche_index: index,
    start_offset: index * parameters().response_geometry.tranche_byte_length,
    byte_length: parameters().response_geometry.tranche_byte_length,
    chunk_count: (
      parameters().response_geometry.tranche_byte_length + parameters().response_geometry.chunk_byte_length - 1
    ) / parameters().response_geometry.chunk_byte_length,
    chunk_commitment: terminal,
    terminal_accumulator: crypto.blake2b_256(builtin.serialise_data(index_data)),
  }
}

fn maximum_descriptors() -> List<availability.TrancheDescriptorV1> {
  list.map(list.range(0, 15), maximum_descriptor)
}

fn maximum_commitment() -> availability.CommitmentV1 {
  availability.CommitmentV1 {
    version: availability.commitment_version_v1,
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    payload_byte_length: availability.full_payload_max_bytes_v1,
    response_geometry: parameters().response_geometry,
    tranche_descriptors: maximum_descriptors(),
  }
}

/// The challenger input the open consumes; its outref names the challenge.
fn challenger_input_outref() -> OutputReference {
  outref(2)
}

fn challenge_asset_name() -> ByteArray {
  availability.challenge_asset_name_v1(challenger_input_outref())
}

fn tranche_asset_name() -> ByteArray {
  availability.tranche_asset_name_v1(challenge_asset_name(), 0)
}

fn terminal_accumulator_asset_name() -> ByteArray {
  availability.terminal_accumulator_asset_name_v1(challenge_asset_name())
}

fn node_output(status: availability.StateQueueStatusV1) -> Output {
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Node(
      state_queue.StateQueueNode {
        header: header(),
        da_attestation: status,
        proven_fraud: None,
      },
    ),
    link: None,
  }
  Output {
    address: address.from_script(state_queue_policy),
    value: assets.from_lovelace(2_000_000)
      |> assets.add(
          state_queue_policy,
          state_queue.block_asset_name_prefix
            |> bytearray.concat(header_hash()),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

/// A challenged node for `committed`, whose status binds the commitment hash
/// and the challenge name.
fn challenged_status(
  committed: availability.CommitmentV1,
) -> availability.StateQueueStatusV1 {
  availability.Challenged {
    commitment_hash: availability.commitment_hash_v1(committed),
    challenge_asset_name: challenge_asset_name(),
  }
}

fn attested_status(
  committed: availability.CommitmentV1,
) -> availability.StateQueueStatusV1 {
  availability.Attested {
    commitment_hash: availability.commitment_hash_v1(committed),
  }
}

fn state_queue_input_for(committed: availability.CommitmentV1) -> Input {
  Input {
    output_reference: outref(1),
    output: node_output(challenged_status(committed)),
  }
}

fn state_queue_input() -> Input {
  state_queue_input_for(commitment())
}

fn state_queue_input_with_status(
  status: availability.StateQueueStatusV1,
) -> Input {
  Input { output_reference: outref(1), output: node_output(status) }
}

/// The challenge record as the open writes it: availability address, exactly
/// `challenge_record_lovelace` and the one challenge token, inline datum.
fn challenge_record_output(
  committed: availability.CommitmentV1,
  claimant: ByteArray,
  opened_at: Int,
  response_deadline: Int,
) -> Output {
  let datum: availability.ChallengeRecordV1 = availability.ChallengeRecordV1 {
    commitment: committed,
    challenge_asset_name: challenge_asset_name(),
    challenger: claimant,
    opened_at,
    response_deadline,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(parameters().challenge_record_lovelace)
      |> assets.add(availability_policy, challenge_asset_name(), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

/// The small-commitment record an open at `opened_at` writes.
fn record_output(claimant: ByteArray, opened_at: Int) -> Output {
  challenge_record_output(
    commitment(),
    claimant,
    opened_at,
    opened_at + availability.small_response_window_ms_v1,
  )
}

/// The small-commitment record, opened at 0, as a later transaction spends or
/// references it.
fn record_input(claimant: ByteArray) -> Input {
  Input { output_reference: outref(0), output: record_output(claimant, 0) }
}

/// The maximum-commitment record, opened at 0.
fn maximum_record_input(claimant: ByteArray) -> Input {
  Input {
    output_reference: outref(0),
    output: challenge_record_output(
      maximum_commitment(),
      claimant,
      0,
      availability.full_response_window_ms_v1,
    ),
  }
}

fn initial_tranche_output(claimant: ByteArray, opened_at: Int) -> Output {
  expect Some(lovelace) = availability.tranche_initial_lovelace_v1(
    commitment(),
    parameters(),
    0,
  )
  let datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor: descriptor(),
    next_offset: 0,
    accumulator: availability.tranche_start_accumulator_v1(
      hub_policy,
      header_hash(),
      descriptor(),
    ),
    latest_carrier_output_index: None,
    response_deadline: opened_at + availability.small_response_window_ms_v1,
    challenger: claimant,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(lovelace)
      |> assets.add(availability_policy, tranche_asset_name(), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn initial_terminal_accumulator_output(
  claimant: ByteArray,
  opened_at: Int,
) -> Output {
  expect Some(lovelace) = availability.terminal_accumulator_initial_lovelace_v1(
    commitment(),
    parameters(),
  )
  let datum: availability.TerminalAccumulatorDatumV1 = availability.TerminalAccumulatorDatumV1 {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    next_tranche_index: 0,
    folded_terminal_accumulator: availability.terminal_accumulator_start_v1(
      commitment(),
      challenge_asset_name(),
    ),
    has_timed_out_tranche: False,
    response_deadline: opened_at + availability.small_response_window_ms_v1,
    challenger: claimant,
    remaining_challenger_lovelace: lovelace,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(lovelace)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

/// Funds exactly the challenger bond, the record's lovelace and the open fee.
fn challenger_input(claimant: ByteArray) -> Input {
  Input {
    output_reference: challenger_input_outref(),
    output: Output {
      address: address.from_verification_key(claimant),
      value: assets.from_lovelace(
        parameters().challenger_bond_lovelace + parameters().challenge_record_lovelace + close_fee,
      ),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn open_redeemer(
  record_output_index: Int,
  state_queue_output_index: Int,
  claimant: ByteArray,
) -> availability.MintRedeemerV1 {
  availability.OpenChallenge {
    yield_to_ref_input_index: 0,
    hub_oracle_ref_input_index: 0,
    record_output_index,
    challenger_input_index: 1,
    state_queue_input_index: 0,
    state_queue_output_index,
    first_tranche_output_index: 2,
    terminal_accumulator_output_index: 3,
    challenger: claimant,
  }
}

fn open_transaction(
  record_output_index: Int,
  state_queue_output_index: Int,
  claimant: ByteArray,
  signatory: ByteArray,
) -> Transaction {
  open_transaction_anchored(
    record_output_index,
    state_queue_output_index,
    claimant,
    signatory,
    between(0, open_inclusive_upper),
    open_inclusive_upper,
  )
}

/// An open whose outputs carry `opened_at` and the response deadlines derived
/// from it, under the given validity range.
fn open_transaction_anchored(
  record_output_index: Int,
  state_queue_output_index: Int,
  claimant: ByteArray,
  signatory: ByteArray,
  validity_range: Interval,
  opened_at: Int,
) -> Transaction {
  let redeemer = open_redeemer(
    record_output_index,
    state_queue_output_index,
    claimant,
  )
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      state_queue_input_with_status(attested_status(commitment())),
      challenger_input(claimant),
    ],
    outputs: [
      record_output(claimant, opened_at),
      node_output(challenged_status(commitment())),
      initial_tranche_output(claimant, opened_at),
      initial_terminal_accumulator_output(claimant, opened_at),
    ],
    reference_inputs: [hub_reference_input()],
    mint: assets.from_asset(availability_policy, challenge_asset_name(), 1)
      |> assets.add(availability_policy, tranche_asset_name(), 1)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), 1),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    extra_signatories: [signatory],
    fee: close_fee,
    validity_range,
  }
}

fn receipt_input(claimant: ByteArray, lovelace: Int) -> Input {
  let datum: availability.TrancheDatumV1 = availability.Receipt {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor: descriptor(),
    terminal_accumulator: terminal,
    terminal_carrier_output_index: 3,
    challenger: claimant,
  }
  Input {
    output_reference: outref(2),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace)
        |> assets.add(availability_policy, tranche_asset_name(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn carrier_input(lovelace: Int) -> Input {
  let datum: availability.PublicationDatumV1 = availability.PublicationDatumV1 {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    tranche_index: 0,
    chunk_index: 0,
    chunk_offset: 0,
    chunk_byte_length: 1,
    chunk_hash: crypto.blake2b_256(#"01"),
    chunk_frontier: validation_merkle_v1.build_frontier(
      [availability.chunk_leaf_hash_v1(0, 0, 0, 1, crypto.blake2b_256(#"01"))],
    ).peaks,
    chunk_siblings: [],
    previous_accumulator: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    next_accumulator: terminal,
    chunk: #"01",
  }
  Input {
    output_reference: outref(3),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn terminal_accumulator_input(
  committed: availability.CommitmentV1,
  next_tranche_index: Int,
  has_timed_out_tranche: Bool,
  remaining_lovelace: Int,
  response_deadline: Int,
  output_reference_index: Int,
) -> Input {
  let datum: availability.TerminalAccumulatorDatumV1 = availability.TerminalAccumulatorDatumV1 {
    deployment_identity: committed.deployment_identity,
    header_hash: committed.header_hash,
    challenge_asset_name: challenge_asset_name(),
    next_tranche_index,
    folded_terminal_accumulator: terminal,
    has_timed_out_tranche,
    response_deadline,
    challenger,
    remaining_challenger_lovelace: remaining_lovelace,
  }
  Input {
    output_reference: outref(output_reference_index),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(remaining_lovelace)
        |> assets.add(availability_policy, terminal_accumulator_asset_name(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn terminal_accumulator_output(
  committed: availability.CommitmentV1,
  next_tranche_index: Int,
  folded_terminal_accumulator: ByteArray,
  has_timed_out_tranche: Bool,
  remaining_lovelace: Int,
  response_deadline: Int,
) -> Output {
  let datum: availability.TerminalAccumulatorDatumV1 = availability.TerminalAccumulatorDatumV1 {
    deployment_identity: committed.deployment_identity,
    header_hash: committed.header_hash,
    challenge_asset_name: challenge_asset_name(),
    next_tranche_index,
    folded_terminal_accumulator,
    has_timed_out_tranche,
    response_deadline,
    challenger,
    remaining_challenger_lovelace: remaining_lovelace,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(remaining_lovelace)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn active_tranche_input(lovelace: Int) -> Input {
  let datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor: descriptor(),
    next_offset: 0,
    accumulator: availability.tranche_start_accumulator_v1(
      hub_policy,
      header_hash(),
      descriptor(),
    ),
    latest_carrier_output_index: None,
    response_deadline: availability.small_response_window_ms_v1,
    challenger,
  }
  Input {
    output_reference: outref(2),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace)
        |> assets.add(availability_policy, tranche_asset_name(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn settlement_redeemer(
  carrier_input_index: Option<Int>,
) -> availability.MintRedeemerV1 {
  availability.SettleTranche {
    yield_to_ref_input_index: 0,
    record_ref_input_index: 0,
    terminal_accumulator_input_index: 0,
    terminal_accumulator_output_index: 0,
    tranche_input_index: 1,
    carrier_input_index,
  }
}

fn receipt_settlement_transaction(fee: Int) -> Transaction {
  expect Some(thread_lovelace) = availability.tranche_initial_lovelace_v1(
    commitment(),
    parameters(),
    0,
  )
  expect Some(terminal_lovelace) = availability.terminal_accumulator_initial_lovelace_v1(
    commitment(),
    parameters(),
  )
  let carrier_lovelace = 2_000_000
  let receipt_lovelace = thread_lovelace - carrier_lovelace
  let output_lovelace = terminal_lovelace + thread_lovelace - fee
  let next_fold = availability.fold_terminal_accumulator_v1(
    terminal,
    0,
    availability.PublishedTranche { terminal_accumulator: terminal },
  )
  let redeemer = settlement_redeemer(Some(2))
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      terminal_accumulator_input(
        commitment(),
        0,
        False,
        terminal_lovelace,
        availability.small_response_window_ms_v1,
        4,
      ),
      receipt_input(challenger, receipt_lovelace),
      carrier_input(carrier_lovelace),
    ],
    outputs: [
      terminal_accumulator_output(
        commitment(),
        1,
        next_fold,
        False,
        output_lovelace,
        availability.small_response_window_ms_v1,
      ),
    ],
    reference_inputs: [record_input(challenger)],
    mint: assets.from_asset(availability_policy, tranche_asset_name(), -1),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee,
    validity_range: between(0, 1_000),
  }
}

fn active_timeout_settlement_transaction(
  fee: Int,
  validity_lower_bound: Int,
) -> Transaction {
  expect Some(thread_lovelace) = availability.tranche_initial_lovelace_v1(
    commitment(),
    parameters(),
    0,
  )
  expect Some(terminal_lovelace) = availability.terminal_accumulator_initial_lovelace_v1(
    commitment(),
    parameters(),
  )
  let start = availability.tranche_start_accumulator_v1(
    hub_policy,
    header_hash(),
    descriptor(),
  )
  let output_lovelace = terminal_lovelace + thread_lovelace - fee
  let next_fold = availability.fold_terminal_accumulator_v1(
    terminal,
    0,
    availability.TimedOutTranche { next_offset: 0, partial_accumulator: start },
  )
  let redeemer = settlement_redeemer(None)
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      terminal_accumulator_input(
        commitment(),
        0,
        False,
        terminal_lovelace,
        availability.small_response_window_ms_v1,
        4,
      ),
      active_tranche_input(thread_lovelace),
    ],
    outputs: [
      terminal_accumulator_output(
        commitment(),
        1,
        next_fold,
        True,
        output_lovelace,
        availability.small_response_window_ms_v1,
      ),
    ],
    reference_inputs: [record_input(challenger)],
    mint: assets.from_asset(availability_policy, tranche_asset_name(), -1),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee,
    validity_range: between(validity_lower_bound, validity_lower_bound + 1_000),
  }
}

fn maximum_record_output(committed: availability.CommitmentV1) -> Output {
  challenge_record_output(
    committed,
    challenger,
    open_inclusive_upper,
    open_inclusive_upper + availability.full_response_window_ms_v1,
  )
}

fn maximum_initial_tranche_output(
  committed: availability.CommitmentV1,
  descriptor: availability.TrancheDescriptorV1,
  lovelace: Int,
) -> Output {
  let datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: committed.deployment_identity,
    header_hash: committed.header_hash,
    challenge_asset_name: challenge_asset_name(),
    descriptor,
    next_offset: descriptor.start_offset,
    accumulator: availability.tranche_start_accumulator_v1(
      committed.deployment_identity,
      committed.header_hash,
      descriptor,
    ),
    latest_carrier_output_index: None,
    response_deadline: open_inclusive_upper + availability.full_response_window_ms_v1,
    challenger,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(lovelace)
      |> assets.add(
          availability_policy,
          availability.tranche_asset_name_v1(
            challenge_asset_name(),
            descriptor.tranche_index,
          ),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn maximum_initial_tranche_outputs(
  committed: availability.CommitmentV1,
  descriptors: List<availability.TrancheDescriptorV1>,
  lovelaces: List<Int>,
) -> List<Output> {
  when Pair(descriptors, lovelaces) is {
    Pair([], []) -> []
    Pair([descriptor, ..rest], [lovelace, ..rest_lovelaces]) ->
      [
        maximum_initial_tranche_output(committed, descriptor, lovelace),
        ..maximum_initial_tranche_outputs(committed, rest, rest_lovelaces)
      ]
    _ -> fail
  }
}

fn maximum_initial_terminal_accumulator_output(
  commitment: availability.CommitmentV1,
) -> Output {
  let lovelace = availability.terminal_accumulator_reserve_lovelace_v1(
    parameters(),
  )
  terminal_accumulator_output(
    commitment,
    0,
    availability.terminal_accumulator_start_v1(
      commitment,
      challenge_asset_name(),
    ),
    False,
    lovelace,
    open_inclusive_upper + availability.full_response_window_ms_v1,
  )
}

fn maximum_open_mint_value() -> assets.Value {
  list.range(0, 15)
    |> list.foldl(
        assets.from_asset(availability_policy, challenge_asset_name(), 1)
          |> assets.add(
              availability_policy,
              terminal_accumulator_asset_name(),
              1,
            ),
        fn(index, value) {
          value
            |> assets.add(
                availability_policy,
                availability.tranche_asset_name_v1(
                  challenge_asset_name(),
                  index,
                ),
                1,
              )
        },
      )
}

fn maximum_open_redeemer() -> availability.MintRedeemerV1 {
  availability.OpenChallenge {
    yield_to_ref_input_index: 0,
    hub_oracle_ref_input_index: 0,
    record_output_index: 0,
    challenger_input_index: 1,
    state_queue_input_index: 0,
    state_queue_output_index: 1,
    first_tranche_output_index: 2,
    terminal_accumulator_output_index: 18,
    challenger,
  }
}

fn maximum_open_transaction() -> Transaction {
  let redeemer = maximum_open_redeemer()
  let redeemer_data: Data = redeemer
  let commitment = maximum_commitment()
  expect Some(initial_lovelaces) = availability.tranche_initial_lovelaces_v1(
    commitment,
    parameters(),
  )
  Transaction {
    ..transaction.placeholder,
    inputs: [
      state_queue_input_with_status(attested_status(commitment)),
      challenger_input(challenger),
    ],
    outputs: [
      maximum_record_output(commitment),
      node_output(challenged_status(commitment)),
      ..list.concat(
        maximum_initial_tranche_outputs(
          commitment,
          commitment.tranche_descriptors,
          initial_lovelaces,
        ),
        [maximum_initial_terminal_accumulator_output(commitment)],
      )
    ],
    reference_inputs: [hub_reference_input()],
    mint: maximum_open_mint_value(),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    extra_signatories: [challenger],
    fee: close_fee,
    validity_range: between(0, open_inclusive_upper),
  }
}

fn maximum_receipt_input(
  descriptor_index: Int,
  output_reference_index: Int,
  lovelace: Int,
) -> Input {
  let descriptor = maximum_descriptor(descriptor_index)
  let datum: availability.TrancheDatumV1 = availability.Receipt {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor,
    terminal_accumulator: descriptor.terminal_accumulator,
    terminal_carrier_output_index: output_reference_index + 1,
    challenger,
  }
  Input {
    output_reference: outref(output_reference_index),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace)
        |> assets.add(
            availability_policy,
            availability.tranche_asset_name_v1(
              challenge_asset_name(),
              descriptor_index,
            ),
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn maximum_carrier_input(
  descriptor_index: Int,
  output_reference_index: Int,
  lovelace: Int,
) -> Input {
  let descriptor = maximum_descriptor(descriptor_index)
  let chunk = maximum_sized_chunk(descriptor_index)
  let datum: availability.PublicationDatumV1 = availability.PublicationDatumV1 {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    tranche_index: descriptor_index,
    chunk_index: descriptor.chunk_count - 1,
    chunk_offset: descriptor.start_offset + descriptor.byte_length - bytearray.length(
      chunk,
    ),
    chunk_byte_length: bytearray.length(chunk),
    chunk_hash: crypto.blake2b_256(chunk),
    chunk_frontier: [],
    chunk_siblings: [],
    previous_accumulator: crypto.blake2b_256(#"70726576696f7573"),
    next_accumulator: descriptor.terminal_accumulator,
    chunk,
  }
  Input {
    output_reference: outref(output_reference_index),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn maximum_sized_chunk(byte: Int) -> ByteArray {
  builtin.replicate_byte(8_192, byte)
    |> bytearray.concat(
        builtin.replicate_byte(
          parameters().response_geometry.chunk_byte_length - 8_192,
          byte,
        ),
      )
}

fn maximum_receipt_inputs(first_input_index: Int, lovelace: Int) -> List<Input> {
  list.map(
    list.range(0, 15),
    fn(index) {
      maximum_receipt_input(index, first_input_index + index, lovelace)
    },
  )
}

fn maximum_carrier_inputs(first_input_index: Int, lovelace: Int) -> List<Input> {
  list.map(
    list.range(0, 15),
    fn(index) {
      maximum_carrier_input(index, first_input_index + index, lovelace)
    },
  )
}

fn maximum_last_tranche_lovelace() -> Int {
  expect Some(lovelace) = availability.tranche_initial_lovelace_v1(
    maximum_commitment(),
    parameters(),
    15,
  )
  lovelace
}

fn maximum_terminal_before_last_lovelace() -> Int {
  parameters().challenger_bond_lovelace - maximum_last_tranche_lovelace() - 15 * close_fee
}

fn maximum_receipt_settlement_transaction() -> Transaction {
  let terminal_lovelace = maximum_terminal_before_last_lovelace()
  let carrier_lovelace = maximum_carrier_lovelace
  let receipt_lovelace = maximum_last_tranche_lovelace() - carrier_lovelace
  let descriptor = maximum_descriptor(15)
  let next_fold = availability.fold_terminal_accumulator_v1(
    terminal,
    15,
    availability.PublishedTranche {
      terminal_accumulator: descriptor.terminal_accumulator,
    },
  )
  let redeemer = settlement_redeemer(Some(2))
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      terminal_accumulator_input(
        maximum_commitment(),
        15,
        False,
        terminal_lovelace,
        availability.full_response_window_ms_v1,
        40,
      ),
      maximum_receipt_input(15, 41, receipt_lovelace),
      maximum_carrier_input(15, 42, carrier_lovelace),
    ],
    outputs: [
      terminal_accumulator_output(
        maximum_commitment(),
        16,
        next_fold,
        False,
        terminal_lovelace + maximum_last_tranche_lovelace() - close_fee,
        availability.full_response_window_ms_v1,
      ),
    ],
    reference_inputs: [maximum_record_input(challenger)],
    mint: assets.from_asset(
      availability_policy,
      availability.tranche_asset_name_v1(challenge_asset_name(), 15),
      -1,
    ),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee: close_fee,
    validity_range: between(0, 1_000),
  }
}

fn maximum_partial_chunk() -> ByteArray {
  maximum_sized_chunk(15)
}

fn maximum_partial_accumulator() -> ByteArray {
  let descriptor = maximum_descriptor(15)
  availability.tranche_step_accumulator_v1(
    hub_policy,
    header_hash(),
    15,
    descriptor.start_offset,
    maximum_partial_chunk(),
    availability.tranche_start_accumulator_v1(
      hub_policy,
      header_hash(),
      descriptor,
    ),
  )
}

fn maximum_partial_active_input(lovelace: Int) -> Input {
  let descriptor = maximum_descriptor(15)
  let datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor,
    next_offset: descriptor.start_offset + bytearray.length(
      maximum_partial_chunk(),
    ),
    accumulator: maximum_partial_accumulator(),
    latest_carrier_output_index: Some(42),
    response_deadline: availability.full_response_window_ms_v1,
    challenger,
  }
  Input {
    output_reference: outref(41),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace)
        |> assets.add(
            availability_policy,
            availability.tranche_asset_name_v1(challenge_asset_name(), 15),
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn maximum_partial_carrier_input(lovelace: Int) -> Input {
  let descriptor = maximum_descriptor(15)
  let chunk = maximum_partial_chunk()
  let start = availability.tranche_start_accumulator_v1(
    hub_policy,
    header_hash(),
    descriptor,
  )
  let datum: availability.PublicationDatumV1 = availability.PublicationDatumV1 {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    tranche_index: 15,
    chunk_index: 0,
    chunk_offset: descriptor.start_offset,
    chunk_byte_length: bytearray.length(chunk),
    chunk_hash: crypto.blake2b_256(chunk),
    chunk_frontier: [],
    chunk_siblings: [],
    previous_accumulator: start,
    next_accumulator: maximum_partial_accumulator(),
    chunk,
  }
  Input {
    output_reference: outref(42),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn maximum_partial_timeout_settlement_transaction() -> Transaction {
  let terminal_lovelace = maximum_terminal_before_last_lovelace()
  let carrier_lovelace = maximum_carrier_lovelace
  let publication_fee = close_fee
  let thread_lovelace = maximum_last_tranche_lovelace() - carrier_lovelace - publication_fee
  let descriptor = maximum_descriptor(15)
  let next_fold = availability.fold_terminal_accumulator_v1(
    terminal,
    15,
    availability.TimedOutTranche {
      next_offset: descriptor.start_offset + bytearray.length(
        maximum_partial_chunk(),
      ),
      partial_accumulator: maximum_partial_accumulator(),
    },
  )
  let redeemer = settlement_redeemer(Some(2))
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      terminal_accumulator_input(
        maximum_commitment(),
        15,
        False,
        terminal_lovelace,
        availability.full_response_window_ms_v1,
        40,
      ),
      maximum_partial_active_input(thread_lovelace),
      maximum_partial_carrier_input(carrier_lovelace),
    ],
    outputs: [
      terminal_accumulator_output(
        maximum_commitment(),
        16,
        next_fold,
        True,
        terminal_lovelace + maximum_last_tranche_lovelace() - publication_fee - close_fee,
        availability.full_response_window_ms_v1,
      ),
    ],
    reference_inputs: [maximum_record_input(challenger)],
    mint: assets.from_asset(
      availability_policy,
      availability.tranche_asset_name_v1(challenge_asset_name(), 15),
      -1,
    ),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee: close_fee,
    validity_range: between(
      availability.full_response_window_ms_v1,
      availability.full_response_window_ms_v1 + 1_000,
    ),
  }
}

fn maximum_publication_parameters() -> availability.ParametersV1 {
  availability.ParametersV1 {
    ..parameters(),
    response_geometry: availability.ResponseGeometryV1 {
      chunk_byte_length: 14_020,
      tranche_byte_length: 4 * 1024 * 1024,
      max_tranche_count: 16,
    },
  }
}

fn maximum_publication_frontier() -> List<validation_merkle_v1.FrontierPeak> {
  [
    validation_merkle_v1.FrontierPeak {
      height: 2,
      hash: #"1a916a0649ad791a11e9e723fea7da14b128fee860cd129d772f09272465e9ad",
    },
    validation_merkle_v1.FrontierPeak {
      height: 3,
      hash: #"02cf6052bcd6b453f2b27afa65bd4545cc81fb73d3b452ef4c21fa3400fa58e5",
    },
    validation_merkle_v1.FrontierPeak {
      height: 5,
      hash: #"e0c5aceb218e4482322bc4e2d2164261295f65cfd31d61469cf1f08fc3ae66b7",
    },
    validation_merkle_v1.FrontierPeak {
      height: 8,
      hash: #"90019f795c3b49fb01f60fd89a3b82c54aa1f939d442bbd3c3ea8b425019fefa",
    },
  ]
}

fn maximum_publication_siblings() -> List<ByteArray> {
  [
    #"cd89e524959690a742777d6fcfee00b67f49680d7db88f11f0b50d544685e8c3",
    #"b13af5471d8dd476547a5db0d45e60e9d7adfb8e3d92129b3b7173173480f5a2",
    #"356b52f863d490ab79e3f11c9c09f2945dc137b25ee3b1098c78ebd48181753d",
    #"6561a840018eec9cfb52705a20982d5f501ef5531c883ba21c9ba1f625a7ed81",
    #"0337d34252a47edf2ff04623473692a90255a79662d460de9f8be66524249450",
    #"b08429687e352fa2c76576b98d1a27fe8c4e6e28ed8eec75fe284dc5e843a054",
    #"88a59d200f5edb186dd53d8d60cdbcb170f8a99774dfb2a198f48d5ede33dd35",
    #"a6adec45f394267b46affeb0180a47a33ca2b7618304d98d61c7d71eaaf0b732",
  ]
}

fn maximum_publication_descriptor() -> availability.TrancheDescriptorV1 {
  availability.TrancheDescriptorV1 {
    tranche_index: 0,
    start_offset: 0,
    byte_length: 4 * 1024 * 1024,
    chunk_count: ( 4 * 1024 * 1024 + 14_020 - 1 ) / 14_020,
    chunk_commitment: #"406476e3fcfcc50f07f11bb09b9d59bf4101c258327cb8b7ab116757338edba5",
    terminal_accumulator: terminal,
  }
}

fn maximum_publication_commitment() -> availability.CommitmentV1 {
  let descriptor = maximum_publication_descriptor()
  availability.CommitmentV1 {
    version: availability.commitment_version_v1,
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    payload_byte_length: descriptor.byte_length,
    response_geometry: maximum_publication_parameters().response_geometry,
    tranche_descriptors: [descriptor],
  }
}

fn maximum_publication_chunk() -> ByteArray {
  builtin.replicate_byte(8_192, 42)
    |> bytearray.concat(builtin.replicate_byte(14_020 - 8_192, 42))
}

fn maximum_publication_transaction() -> Transaction {
  let descriptor = maximum_publication_descriptor()
  let committed = maximum_publication_commitment()
  let chunk = maximum_publication_chunk()
  let start = availability.tranche_start_accumulator_v1(
    hub_policy,
    header_hash(),
    descriptor,
  )
  let next = availability.tranche_step_accumulator_v1(
    hub_policy,
    header_hash(),
    0,
    0,
    chunk,
    start,
  )
  let current_datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor,
    next_offset: 0,
    accumulator: start,
    latest_carrier_output_index: None,
    response_deadline: availability.full_response_window_ms_v1,
    challenger,
  }
  let publication: availability.PublicationDatumV1 = availability.PublicationDatumV1 {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    tranche_index: 0,
    chunk_index: 0,
    chunk_offset: 0,
    chunk_byte_length: bytearray.length(chunk),
    chunk_hash: crypto.blake2b_256(chunk),
    chunk_frontier: maximum_publication_frontier(),
    chunk_siblings: maximum_publication_siblings(),
    previous_accumulator: start,
    next_accumulator: next,
    chunk,
  }
  let next_datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor,
    next_offset: bytearray.length(chunk),
    accumulator: next,
    latest_carrier_output_index: Some(1),
    response_deadline: availability.full_response_window_ms_v1,
    challenger,
  }
  let thread_lovelace = 20_000_000
  let carrier_lovelace = 2_000_000
  let input = Input {
    output_reference: outref(0),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(thread_lovelace)
        |> assets.add(availability_policy, tranche_asset_name(), 1),
      datum: InlineDatum(current_datum),
      reference_script: None,
    },
  }
  let redeemer: availability.SpendRedeemerV1 = availability.AdvanceTranche {
    thread_output_index: 0,
    carrier_output_index: 1,
    m_previous_carrier_input_index: None,
  }
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [input],
    outputs: [
      Output {
        address: address.from_script(availability_policy),
        value: assets.from_lovelace(
          thread_lovelace - carrier_lovelace - close_fee,
        )
          |> assets.add(availability_policy, tranche_asset_name(), 1),
        datum: InlineDatum(next_datum),
        reference_script: None,
      },
      Output {
        address: address.from_script(availability_policy),
        value: assets.from_lovelace(carrier_lovelace),
        datum: InlineDatum(publication),
        reference_script: None,
      },
    ],
    redeemers: [Pair(Spend(input.output_reference), redeemer_data)],
    fee: close_fee,
    validity_range: between(0, 1_000),
  }
}

fn validates_maximum_publication(tx: Transaction) -> Bool {
  expect [input] = tx.inputs
  expect InlineDatum(datum) = input.output.datum
  availability_validator.availability_challenge.spend(
    hub_policy,
    miscellaneous_policy,
    maximum_publication_parameters(),
    Some(datum),
    availability.AdvanceTranche {
      thread_output_index: 0,
      carrier_output_index: 1,
      m_previous_carrier_input_index: None,
    },
    input.output_reference,
    tx,
  )
}

const maximum_thread_lovelace = 10_000_000

const maximum_carrier_lovelace = 2_000_000

fn maximum_challenger_component_lovelace() -> Int {
  16 * ( maximum_thread_lovelace + maximum_carrier_lovelace )
}

fn hub_reference_input() -> Input {
  let script_address = address.from_script(miscellaneous_policy)
  let datum: hub.Datum = hub.Datum {
    registered_operators: miscellaneous_policy,
    active_operators: miscellaneous_policy,
    retired_operators: miscellaneous_policy,
    scheduler: miscellaneous_policy,
    state_queue: state_queue_policy,
    fraud_proof_catalogue: miscellaneous_policy,
    fraud_proof: miscellaneous_policy,
    deposit: miscellaneous_policy,
    withdrawal: miscellaneous_policy,
    tx_order: miscellaneous_policy,
    settlement: miscellaneous_policy,
    payout: miscellaneous_policy,
    registered_operators_addr: script_address,
    active_operators_addr: script_address,
    retired_operators_addr: script_address,
    scheduler_addr: script_address,
    state_queue_addr: address.from_script(state_queue_policy),
    fraud_proof_catalogue_addr: script_address,
    fraud_proof_addr: script_address,
    deposit_addr: script_address,
    withdrawal_addr: script_address,
    tx_order_addr: script_address,
    settlement_addr: script_address,
    reserve_addr: script_address,
    payout_addr: script_address,
    reserve_observer: miscellaneous_policy,
  }
  Input {
    output_reference: outref(9),
    output: Output {
      address: address.from_script(hub_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(hub_policy, hub.asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn enterprise_output(owner: ByteArray, lovelace: Int) -> Output {
  Output {
    address: address.from_verification_key(owner),
    value: assets.from_lovelace(lovelace),
    datum: NoDatum,
    reference_script: None,
  }
}

fn close_redeemer(
  challenger_refund_output_index: Int,
) -> availability.MintRedeemerV1 {
  availability.CloseChallenge {
    yield_to_ref_input_index: 0,
    hub_oracle_ref_input_index: 0,
    record_input_index: 0,
    terminal_accumulator_input_index: 2,
    state_queue_input_index: 1,
    state_queue_output_index: 0,
    challenger_refund_output_index,
  }
}

/// The close burns the record's challenge token and the terminal accumulator
/// token, publishes the node, and returns the unspent bond plus the record's
/// lovelace to the challenger in one output.
fn close_transaction_for(
  committed: availability.CommitmentV1,
  record: Input,
  claimant: ByteArray,
  remaining: Int,
  terminal_next_tranche_index: Int,
  terminal_response_deadline: Int,
  terminal_output_reference_index: Int,
  challenger_refund_output_index: Int,
) -> Transaction {
  let redeemer = close_redeemer(challenger_refund_output_index)
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      record,
      state_queue_input_for(committed),
      terminal_accumulator_input(
        committed,
        terminal_next_tranche_index,
        False,
        remaining,
        terminal_response_deadline,
        terminal_output_reference_index,
      ),
    ],
    outputs: [
      node_output(
        availability.Published {
          terminal_commitment: availability.published_terminal_commitment_v1(
            committed,
          ),
        },
      ),
      enterprise_output(
        claimant,
        remaining - close_fee + parameters().challenge_record_lovelace,
      ),
    ],
    reference_inputs: [hub_reference_input()],
    mint: assets.from_asset(availability_policy, challenge_asset_name(), -1)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), -1),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee: close_fee,
    validity_range: between(0, 1_000),
  }
}

fn close_transaction(
  claimant: ByteArray,
  receipt_lovelace: Int,
  carrier_lovelace: Int,
  challenger_refund_output_index: Int,
) -> Transaction {
  close_transaction_for(
    commitment(),
    record_input(claimant),
    claimant,
    receipt_lovelace + carrier_lovelace,
    1,
    availability.small_response_window_ms_v1,
    3,
    challenger_refund_output_index,
  )
}

fn maximum_close_redeemer() -> availability.MintRedeemerV1 {
  close_redeemer(1)
}

fn maximum_close_transaction() -> Transaction {
  close_transaction_for(
    maximum_commitment(),
    maximum_record_input(challenger),
    challenger,
    maximum_challenger_component_lovelace(),
    16,
    availability.full_response_window_ms_v1,
    40,
    1,
  )
}

fn with_yield_index(
  redeemer: availability.MintRedeemerV1,
  yield_to_ref_input_index: Int,
) -> availability.MintRedeemerV1 {
  when redeemer is {
    availability.OpenChallenge {
      hub_oracle_ref_input_index,
      record_output_index,
      challenger_input_index,
      state_queue_input_index,
      state_queue_output_index,
      first_tranche_output_index,
      terminal_accumulator_output_index,
      challenger,
      ..
    } ->
      availability.OpenChallenge {
        yield_to_ref_input_index,
        hub_oracle_ref_input_index,
        record_output_index,
        challenger_input_index,
        state_queue_input_index,
        state_queue_output_index,
        first_tranche_output_index,
        terminal_accumulator_output_index,
        challenger,
      }
    availability.SettleTranche {
      record_ref_input_index,
      terminal_accumulator_input_index,
      terminal_accumulator_output_index,
      tranche_input_index,
      carrier_input_index,
      ..
    } ->
      availability.SettleTranche {
        yield_to_ref_input_index,
        record_ref_input_index,
        terminal_accumulator_input_index,
        terminal_accumulator_output_index,
        tranche_input_index,
        carrier_input_index,
      }
    availability.CloseChallenge {
      hub_oracle_ref_input_index,
      record_input_index,
      terminal_accumulator_input_index,
      state_queue_input_index,
      state_queue_output_index,
      challenger_refund_output_index,
      ..
    } ->
      availability.CloseChallenge {
        yield_to_ref_input_index,
        hub_oracle_ref_input_index,
        record_input_index,
        terminal_accumulator_input_index,
        state_queue_input_index,
        state_queue_output_index,
        challenger_refund_output_index,
      }
    availability.TimeoutChallenge {
      hub_oracle_ref_input_index,
      record_input_index,
      terminal_accumulator_input_index,
      state_queue_mint_redeemer_index,
      pool_input_index,
      pool_output_index,
      challenger_refund_output_index,
      ..
    } ->
      availability.TimeoutChallenge {
        yield_to_ref_input_index,
        hub_oracle_ref_input_index,
        record_input_index,
        terminal_accumulator_input_index,
        state_queue_mint_redeemer_index,
        pool_input_index,
        pool_output_index,
        challenger_refund_output_index,
      }
  }
}

fn yield_role(redeemer: availability.MintRedeemerV1) -> assets.AssetName {
  when redeemer is {
    availability.OpenChallenge { .. } -> yielding.open_role
    availability.SettleTranche { .. } -> yielding.settle_role
    availability.CloseChallenge { .. } -> yielding.close_role
    availability.TimeoutChallenge { .. } -> yielding.timeout_role
  }
}

fn invokes_availability_yield(
  tx: Transaction,
  redeemer: availability.MintRedeemerV1,
) -> Bool {
  when redeemer is {
    availability.OpenChallenge { .. } ->
      availability_yields.open.withdraw(
        availability_policy,
        hub_policy,
        parameters(),
        availability.Yield,
        address.Script(da_policy),
        tx,
      )
    availability.SettleTranche { .. } ->
      availability_yields.settle.withdraw(
        availability_policy,
        hub_policy,
        parameters(),
        availability.Yield,
        address.Script(da_policy),
        tx,
      )
    availability.CloseChallenge { .. } ->
      availability_yields.close.withdraw(
        availability_policy,
        hub_policy,
        parameters(),
        availability.Yield,
        address.Script(da_policy),
        tx,
      )
    availability.TimeoutChallenge { .. } ->
      availability_yields.timeout.withdraw(
        availability_policy,
        hub_policy,
        da_bond_pool_policy,
        parameters(),
        availability.Yield,
        address.Script(da_policy),
        tx,
      )
  }
}

fn authenticated_yield_transaction(
  tx: Transaction,
  redeemer: availability.MintRedeemerV1,
) -> Transaction {
  let mint_redeemer: Data = redeemer
  let yield_redeemer: Data = availability.Yield
  let reference_input = Input {
    output_reference: outref(90),
    output: Output {
      address: address.from_script(miscellaneous_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(miscellaneous_policy, yield_role(redeemer), 1),
      datum: NoDatum,
      reference_script: Some(da_policy),
    },
  }
  Transaction {
    ..tx,
    reference_inputs: list.concat(tx.reference_inputs, [reference_input]),
    withdrawals: [Pair(address.Script(da_policy), 0)],
    redeemers: list.concat(
      list.map(
        tx.redeemers,
        fn(entry) {
          if entry.1st == Mint(availability_policy) {
            Pair(entry.1st, mint_redeemer)
          } else {
            entry
          }
        },
      ),
      [Pair(Withdraw(address.Script(da_policy)), yield_redeemer)],
    ),
  }
}

fn validates_close(
  tx: Transaction,
  redeemer: availability.MintRedeemerV1,
) -> Bool {
  let bound_redeemer = with_yield_index(
    redeemer,
    list.length(tx.reference_inputs),
  )
  let yielded_tx = authenticated_yield_transaction(tx, bound_redeemer)
  and {
    availability_validator.availability_challenge.mint(
      hub_policy,
      miscellaneous_policy,
      parameters(),
      bound_redeemer,
      availability_policy,
      yielded_tx,
    ),
    invokes_availability_yield(yielded_tx, bound_redeemer),
  }
}

fn validates_coordinate_input(
  tx: Transaction,
  input_index: Int,
  mint_redeemer_index: Int,
) -> Bool {
  expect Some(input) = list.at(tx.inputs, input_index)
  expect InlineDatum(datum) = input.output.datum
  availability_validator.availability_challenge.spend(
    hub_policy,
    miscellaneous_policy,
    parameters(),
    Some(datum),
    availability.Coordinate { mint_redeemer_index },
    input.output_reference,
    tx,
  )
}

fn validates_coordinate_inputs(
  tx: Transaction,
  indices: List<Int>,
  mint_redeemer_index: Int,
) -> Bool {
  when indices is {
    [] -> True
    [index, ..rest] ->
      validates_coordinate_input(tx, index, mint_redeemer_index) && validates_coordinate_inputs(
        tx,
        rest,
        mint_redeemer_index,
      )
  }
}

fn timeout_redeemer(
  state_queue_mint_redeemer_index: Int,
  pool_input_index: Int,
  pool_output_index: Int,
  challenger_refund_output_index: Int,
) -> availability.MintRedeemerV1 {
  availability.TimeoutChallenge {
    yield_to_ref_input_index: 0,
    hub_oracle_ref_input_index: 0,
    record_input_index: 0,
    terminal_accumulator_input_index: 1,
    state_queue_mint_redeemer_index,
    pool_input_index,
    pool_output_index,
    challenger_refund_output_index,
  }
}

/// The unavailable node the state-queue removal spends, still challenged.
fn timed_out_node_input(committed: availability.CommitmentV1) -> Input {
  Input {
    output_reference: outref(4),
    output: node_output(challenged_status(committed)),
  }
}

fn timeout_state_queue_redeemer(
  bound_challenge_asset_name: ByteArray,
) -> state_queue.MintRedeemer {
  state_queue.RemoveUnavailableBlockAfterTimeout {
    yield_to_ref_input_index: 0,
    unavailable_header_hash: header_hash(),
    challenge_asset_name: bound_challenge_asset_name,
    removal_approach: state_queue.PruneTimedOutBlockDescendant {
      confirmed_state_ref_input_index: 0,
      timed_out_node_input_outref: outref(4),
      timed_out_node_output_index: 0,
    },
  }
}

/// The challenger reserve the timeout fixtures leave on the terminal
/// accumulator.
const timeout_remaining_lovelace = 10_000_000_000

/// A pool holding exactly one bond above the floor: `backing == da_bond`.
const full_pool_lovelace = 10_005_000_000

fn bonded_pool_datum() -> Data {
  let datum: Data = pool.Bonded
  datum
}

fn withdrawing_pool_datum(unlock_at: Int) -> Data {
  let datum: Data = pool.Withdrawing { unlock_at }
  datum
}

fn pool_value(lovelace: Int) -> assets.Value {
  assets.from_lovelace(lovelace)
    |> assets.add(da_bond_pool_policy, pool.da_bond_pool_asset_name, 1)
}

/// The pool UTxO as the pool validator keeps it: `Script(pool policy)` with no
/// stake credential, lovelace plus the NFT, inline datum, no reference script.
fn pool_output(datum: Data, lovelace: Int) -> Output {
  Output {
    address: address.from_script(da_bond_pool_policy),
    value: pool_value(lovelace),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn pool_input(datum: Data, lovelace: Int) -> Input {
  Input { output_reference: outref(6), output: pool_output(datum, lovelace) }
}

/// The slash split, computed here independently of the validator:
/// `(taken, fee_part, payout)` with `taken = min(da_bond, max(0, lovelace -
/// floor))`, `fee_part = min(penalty, taken)` and `payout = taken - fee_part`.
fn slash_split(pool_lovelace: Int) -> (Int, Int, Int) {
  let params = parameters()
  let backing = if pool_lovelace > params.da_bond_pool_floor_lovelace {
    pool_lovelace - params.da_bond_pool_floor_lovelace
  } else {
    0
  }
  let taken = if backing < params.da_bond_lovelace {
    backing
  } else {
    params.da_bond_lovelace
  }
  let fee_part = if taken < params.da_slash_penalty_lovelace {
    taken
  } else {
    params.da_slash_penalty_lovelace
  }
  (taken, fee_part, taken - fee_part)
}

/// An honest Timeout: the expired record, its terminal accumulator, the
/// challenged node and the pool are spent. Inputs are the record (0), the
/// terminal accumulator (1), the node (2) and the pool (3); outputs are the
/// one merged challenger output (0) and the slashed pool (1), as
/// `timeout_redeemer(0, 3, 1, 0)` names them. `challenger_fee` is `c`: the
/// fee is `fee_part + c` and the challenger output
/// `remaining - c + challenge_record_lovelace + payout`.
fn timeout_transaction(
  bound_challenge_asset_name: ByteArray,
  pool_datum: Data,
  pool_lovelace: Int,
  challenger_fee: Int,
) -> Transaction {
  let (taken, fee_part, payout) = slash_split(pool_lovelace)
  let redeemer_data: Data = timeout_redeemer(0, 3, 1, 0)
  let state_queue_redeemer_data: Data = timeout_state_queue_redeemer(
    bound_challenge_asset_name,
  )
  Transaction {
    ..transaction.placeholder,
    inputs: [
      record_input(challenger),
      terminal_accumulator_input(
        commitment(),
        1,
        True,
        timeout_remaining_lovelace,
        availability.small_response_window_ms_v1,
        3,
      ),
      timed_out_node_input(commitment()),
      pool_input(pool_datum, pool_lovelace),
    ],
    outputs: [
      enterprise_output(
        challenger,
        timeout_remaining_lovelace - challenger_fee + parameters().challenge_record_lovelace + payout,
      ),
      pool_output(pool_datum, pool_lovelace - taken),
    ],
    reference_inputs: [hub_reference_input()],
    mint: assets.from_asset(availability_policy, challenge_asset_name(), -1)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), -1),
    redeemers: [
      Pair(Mint(state_queue_policy), state_queue_redeemer_data),
      Pair(Mint(availability_policy), redeemer_data),
    ],
    fee: fee_part + challenger_fee,
    validity_range: between(
      availability.small_response_window_ms_v1,
      availability.small_response_window_ms_v1 + 1_000,
    ),
  }
}

fn maximum_timeout_redeemer() -> availability.MintRedeemerV1 {
  timeout_redeemer(0, 3, 1, 0)
}

/// The 16-tranche Timeout with a full `Bonded` pool: the penalty pays the
/// whole fee (`c = 0`) and the challenger output carries the payout.
fn maximum_timeout_transaction() -> Transaction {
  let redeemer = maximum_timeout_redeemer()
  let redeemer_data: Data = redeemer
  let state_queue_redeemer_data: Data = timeout_state_queue_redeemer(
    challenge_asset_name(),
  )
  let remaining = maximum_challenger_component_lovelace()
  let (taken, fee_part, payout) = slash_split(full_pool_lovelace)
  Transaction {
    ..transaction.placeholder,
    inputs: [
      maximum_record_input(challenger),
      terminal_accumulator_input(
        maximum_commitment(),
        16,
        True,
        remaining,
        availability.full_response_window_ms_v1,
        40,
      ),
      timed_out_node_input(maximum_commitment()),
      pool_input(bonded_pool_datum(), full_pool_lovelace),
    ],
    outputs: [
      enterprise_output(
        challenger,
        remaining + parameters().challenge_record_lovelace + payout,
      ),
      pool_output(bonded_pool_datum(), full_pool_lovelace - taken),
    ],
    reference_inputs: [hub_reference_input()],
    mint: assets.from_asset(availability_policy, challenge_asset_name(), -1)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), -1),
    redeemers: [
      Pair(Mint(state_queue_policy), state_queue_redeemer_data),
      Pair(Mint(availability_policy), redeemer_data),
    ],
    fee: fee_part,
    validity_range: between(
      availability.full_response_window_ms_v1,
      availability.full_response_window_ms_v1 + 1_000,
    ),
  }
}

/// Positive control: the close spends the record, publishes the node and pays
/// the challenger the unspent bond plus the record's lovelace.
test q58_close_accepts_exact_distinct_terminal_outputs() {
  let tx = close_transaction(challenger, 6_000_000_000, 4_000_000_000, 1)
  and {
    validates_close(tx, close_redeemer(1)),
    validates_coordinate_inputs(tx, [0, 2], 0),
  }
}

/// The refund index names the published node output. Refused first by
/// `challenger_refund_output_index != state_queue_output_index`, but that check
/// is defence in depth, not the sole guard: the refund must be an exact
/// enterprise ADA output to the challenger, and the node output sits at the
/// state-queue script with its block NFT, so no output can fill both roles.
/// With the index check removed the transaction is still refused.
test q58_close_rejects_challenger_refund_aliased_with_state_queue_output() fail {
  validates_close(
    close_transaction(challenger, 6_000_000_000, 4_000_000_000, 0),
    close_redeemer(0),
  )
}

/// Positive control: the open writes the record and moves the node from
/// attested to challenged under the same commitment hash.
test q58_open_accepts_exact_distinct_inputs_outputs_and_signer() {
  let redeemer = open_redeemer(0, 1, challenger)
  validates_close(open_transaction(0, 1, challenger, challenger), redeemer)
}

test q58_open_rejects_missing_challenger_signature() fail {
  let redeemer = open_redeemer(0, 1, challenger)
  validates_close(open_transaction(0, 1, challenger, operator), redeemer)
}

/// The open must land strictly before the node's challenge window closes:
/// the last accepted inclusive upper bound is `end_time + window - 1`.
fn open_at_window_edge(inclusive_upper: Int) -> Transaction {
  open_transaction_anchored(
    0,
    1,
    challenger,
    challenger,
    between(inclusive_upper - 1_000, inclusive_upper),
    inclusive_upper,
  )
}

test availability_open_accepts_last_millisecond_of_challenge_window() {
  validates_close(
    open_at_window_edge(
      header().end_time + availability.da_challenge_window_ms_v1 - 1,
    ),
    open_redeemer(0, 1, challenger),
  )
}

test availability_open_rejects_upper_bound_at_challenge_window_end() fail {
  validates_close(
    open_at_window_edge(
      header().end_time + availability.da_challenge_window_ms_v1,
    ),
    open_redeemer(0, 1, challenger),
  )
}

/// HAZ-13: only the status changes; a node output that gains lovelace is
/// refused.
test availability_open_rejects_node_value_change() fail {
  let redeemer = open_redeemer(0, 1, challenger)
  let base = open_transaction(0, 1, challenger, challenger)
  expect [record, node, ..rest] = base.outputs
  let grown = Output { ..node, value: assets.add(node.value, "", "", 1) }
  validates_close(
    Transaction { ..base, outputs: [record, grown, ..rest] },
    redeemer,
  )
}

/// Only the node input changes: it attests another commitment while the
/// continuation still carries the record's. Refused by the node input-status
/// match `Attested{commitment_hash_v1(record.commitment)}`. The preimage
/// binding with a consistent node pair is pinned by
/// `availability_open_rejects_record_preimage_of_another_attested_hash`.
test availability_open_rejects_record_commitment_not_attested_by_node() fail {
  let redeemer = open_redeemer(0, 1, challenger)
  let base = open_transaction(0, 1, challenger, challenger)
  expect [_, challenger_funds] = base.inputs
  let foreign_node = state_queue_input_with_status(
    attested_status(maximum_commitment()),
  )
  validates_close(
    Transaction { ..base, inputs: [foreign_node, challenger_funds] },
    redeemer,
  )
}

/// The record output index names the node output. The index inequality
/// refuses it first, as defence in depth: the record must sit at the
/// availability address holding the minted DACH, and the node output at the
/// state-queue script with its block NFT, so the alias is refused without it.
test q58_open_rejects_aliased_record_and_state_queue_outputs() fail {
  let redeemer = open_redeemer(0, 0, challenger)
  validates_close(open_transaction(0, 0, challenger, challenger), redeemer)
}

/// The widest range the open accepts, with the ledger's exclusive upper end:
/// a challenger who backdates the lower bound by the whole maximum validity
/// range. Normalization makes the inclusive upper bound `upper - 1`.
fn backdated_open_range() -> Interval {
  Interval {
    lower_bound: IntervalBound { bound_type: Finite(0), is_inclusive: True },
    upper_bound: IntervalBound {
      bound_type: Finite(env.max_validity_range_length + 1),
      is_inclusive: False,
    },
  }
}

test availability_open_anchors_response_deadline_at_inclusive_upper_bound() {
  let redeemer = open_redeemer(0, 1, challenger)
  validates_close(
    open_transaction_anchored(
      0,
      1,
      challenger,
      challenger,
      backdated_open_range(),
      env.max_validity_range_length,
    ),
    redeemer,
  )
}

/// Differential twin of the test above: only the anchor differs. A deadline
/// counted from the backdated lower bound would reach the responder already
/// shortened by the whole backdate, so the open must refuse it.
test availability_open_rejects_response_deadline_anchored_at_lower_bound() fail {
  let redeemer = open_redeemer(0, 1, challenger)
  validates_close(
    open_transaction_anchored(
      0,
      1,
      challenger,
      challenger,
      backdated_open_range(),
      0,
    ),
    redeemer,
  )
}

/// The ledger's upper end is exclusive. Anchoring at it instead of at
/// `upper - 1` would disagree with the off-chain builders, which anchor at
/// `validTo - 1`.
test availability_open_rejects_response_deadline_anchored_at_exclusive_upper_end() fail {
  let redeemer = open_redeemer(0, 1, challenger)
  validates_close(
    open_transaction_anchored(
      0,
      1,
      challenger,
      challenger,
      backdated_open_range(),
      env.max_validity_range_length + 1,
    ),
    redeemer,
  )
}

/// Upper-bound anchoring leaves the open's maximum-validity-range check as the
/// only bound on how far a challenger can push the response deadline. The
/// ledger admits ranges of any length, so this Aiken test is the only place
/// that pins it. Differential twin of
/// `availability_open_anchors_response_deadline_at_inclusive_upper_bound`: the
/// inclusive range is one millisecond longer than the maximum, with every datum
/// anchored consistently at its inclusive upper bound.
test availability_open_rejects_validity_range_longer_than_maximum() fail {
  let redeemer = open_redeemer(0, 1, challenger)
  validates_close(
    open_transaction_anchored(
      0,
      1,
      challenger,
      challenger,
      Interval {
        lower_bound: IntervalBound { bound_type: Finite(0), is_inclusive: True },
        upper_bound: IntervalBound {
          bound_type: Finite(env.max_validity_range_length + 2),
          is_inclusive: False,
        },
      },
      env.max_validity_range_length + 1,
    ),
    redeemer,
  )
}

/// Positive control: the settlement authenticates the challenge record it
/// references and advances the terminal accumulator by one published tranche.
test q58_settle_published_tranche_accepts_exact_thread_and_carrier() {
  let tx = receipt_settlement_transaction(close_fee)
  and {
    validates_close(tx, settlement_redeemer(Some(2))),
    validates_coordinate_inputs(tx, [0, 1, 2], 0),
  }
}

test q58_settle_timed_out_unpublished_tranche_accepts_without_carrier() {
  let tx = active_timeout_settlement_transaction(
    close_fee,
    availability.small_response_window_ms_v1,
  )
  and {
    validates_close(tx, settlement_redeemer(None)),
    validates_coordinate_inputs(tx, [0, 1], 0),
  }
}

test q58_settle_rejects_substituted_terminal_carrier_outref() fail {
  let base = receipt_settlement_transaction(close_fee)
  expect [terminal_input, receipt, carrier] = base.inputs
  let substituted_carrier = Input { ..carrier, output_reference: outref(8) }
  let tx = Transaction {
    ..base,
    inputs: [terminal_input, receipt, substituted_carrier],
  }
  validates_close(tx, settlement_redeemer(Some(2)))
}

/// A record is authenticated by its challenge token: an identical datum at
/// the availability address without the token is refused.
test q58_settle_rejects_record_without_challenge_token() fail {
  let base = receipt_settlement_transaction(close_fee)
  let genuine = record_input(challenger)
  let forged = Input {
    ..genuine,
    output: Output {
      ..genuine.output,
      value: assets.from_lovelace(parameters().challenge_record_lovelace),
    },
  }
  validates_close(
    Transaction { ..base, reference_inputs: [forged] },
    settlement_redeemer(Some(2)),
  )
}

test q58_settle_rejects_premature_active_timeout() fail {
  let tx = active_timeout_settlement_transaction(
    close_fee,
    availability.small_response_window_ms_v1 - 1_000,
  )
  validates_close(tx, settlement_redeemer(None))
}

test q58_settle_rejects_excessive_fee() fail {
  let fee = parameters().max_settlement_fee_lovelace + 1
  validates_close(
    receipt_settlement_transaction(fee),
    settlement_redeemer(Some(2)),
  )
}

test q58_settle_rejects_aliased_terminal_and_tranche_roles() fail {
  let redeemer: availability.MintRedeemerV1 = availability.SettleTranche {
    yield_to_ref_input_index: 0,
    record_ref_input_index: 0,
    terminal_accumulator_input_index: 0,
    terminal_accumulator_output_index: 0,
    tranche_input_index: 0,
    carrier_input_index: Some(2),
  }
  validates_close(receipt_settlement_transaction(close_fee), redeemer)
}

test q58_settle_rejects_batched_second_tranche_fee_charge() fail {
  let base = receipt_settlement_transaction(close_fee)
  let extra = Input {
    ..receipt_input(challenger, 2_000_000),
    output_reference: outref(8),
  }
  let tx = Transaction { ..base, inputs: list.concat(base.inputs, [extra]) }
  validates_close(tx, settlement_redeemer(Some(2)))
}

test q58_maximum_16_tranche_published_settlement_applied_topology_fits() {
  let tx = maximum_receipt_settlement_transaction()
  and {
    validates_close(tx, settlement_redeemer(Some(2))),
    validates_coordinate_inputs(tx, [0, 1, 2], 0),
  }
}

test q58_maximum_16_tranche_partial_timeout_settlement_applied_topology_fits() {
  let tx = maximum_partial_timeout_settlement_transaction()
  and {
    validates_close(tx, settlement_redeemer(Some(2))),
    validates_coordinate_inputs(tx, [0, 1, 2], 0),
  }
}

test q58_maximum_14020_byte_publication_with_merkle_proof_applied_fits() {
  validates_maximum_publication(maximum_publication_transaction())
}

test q58_maximum_16_tranche_open_applied_topology_fits() {
  let tx = maximum_open_transaction()
  and {
    list.length(tx.outputs) == 19,
    validates_close(tx, maximum_open_redeemer()),
  }
}

test q58_maximum_16_tranche_open_mint_policy_measurement() {
  validates_close(maximum_open_transaction(), maximum_open_redeemer())
}

test q58_maximum_publication_first_leaf_cross_language_vector_matches() {
  let chunk = maximum_publication_chunk()
  availability.chunk_leaf_hash_v1(
    0,
    0,
    0,
    bytearray.length(chunk),
    crypto.blake2b_256(chunk),
  ) == #"2b89672abc40b0ba8c2d4db8cd236b2ca6bda31ab8db93517f88e88dff114675"
}

/// Positive control: a full `Bonded` pool (`backing == da_bond`). The
/// penalty pays the whole fee (`c = 0`), the challenger output carries the
/// reserve, the record's lovelace and `da_bond - penalty`, and the pool keeps
/// its floor. The record and terminal accumulator Coordinate spends accept.
test q58_timeout_accepts_full_pool_slash_with_the_penalty_as_the_fee() {
  let tx = honest_timeout()
  expect [challenger_output, slashed_pool] = tx.outputs
  and {
    tx.fee == 2_500_000_000,
    assets.lovelace_of(challenger_output.value) == 10_000_000_000 + 27_000_000 + 7_500_000_000,
    slashed_pool.value == pool_value(5_000_000),
    validates_honest_timeout_redeemer(tx),
    validates_coordinate_inputs(tx, [0, 1], 1),
  }
}

test q58_coordinate_rejects_undeclared_availability_input() fail {
  let base = close_transaction(challenger, 6_000_000_000, 4_000_000_000, 1)
  let unrelated = Input {
    ..receipt_input(challenger, 2_000_000),
    output_reference: outref(8),
  }
  let tx = Transaction { ..base, inputs: list.concat(base.inputs, [unrelated]) }
  validates_coordinate_input(tx, 3, 0)
}

test q58_timeout_rejects_premature_validity() fail {
  let base = honest_timeout()
  let tx = Transaction {
    ..base,
    validity_range: between(0, availability.small_response_window_ms_v1 - 1),
  }
  validates_honest_timeout_redeemer(tx)
}

test q58_timeout_rejects_wrong_state_queue_challenge_identity() fail {
  validates_honest_timeout_redeemer(
    timeout_transaction(terminal, bonded_pool_datum(), full_pool_lovelace, 0),
  )
}

/// The record and terminal accumulator indices name one input. The index
/// inequality refuses it first, as defence in depth: each role is
/// authenticated by an exact value holding its own token (the DACH, the
/// terminal token), so no input can fill both.
test q58_timeout_rejects_aliased_record_and_terminal_input_index() fail {
  let redeemer: availability.MintRedeemerV1 = availability.TimeoutChallenge {
    yield_to_ref_input_index: 0,
    hub_oracle_ref_input_index: 0,
    record_input_index: 0,
    terminal_accumulator_input_index: 0,
    state_queue_mint_redeemer_index: 0,
    pool_input_index: 3,
    pool_output_index: 1,
    challenger_refund_output_index: 0,
  }
  validates_close(honest_timeout(), redeemer)
}

/// G6: the node the removal spends must carry the record's commitment hash
/// as well as its challenge name. Refused by the node status match
/// `Challenged{commitment_hash_v1(record.commitment), record.challenge_asset_name}`.
test q58_timeout_rejects_node_challenged_under_another_commitment() fail {
  let base = honest_timeout()
  expect [record, terminal_input, _, pool_in] = base.inputs
  let foreign_node = timed_out_node_input(maximum_commitment())
  validates_honest_timeout_redeemer(
    Transaction {
      ..base,
      inputs: [record, terminal_input, foreign_node, pool_in],
    },
  )
}

/// A one-lovelace second output to the challenger. Refused by the challenger
/// output count (`challenger_output_count == 1`).
test q58_timeout_rejects_second_challenger_output() fail {
  let base = honest_timeout()
  validates_honest_timeout_redeemer(
    Transaction {
      ..base,
      outputs: list.concat(base.outputs, [enterprise_output(challenger, 1)]),
    },
  )
}

test q58_maximum_16_tranche_final_close_applied_topology_fits() {
  let tx = maximum_close_transaction()
  and {
    list.length(tx.inputs) == 3,
    validates_close(tx, maximum_close_redeemer()),
    validates_coordinate_inputs(tx, [0, 2], 0),
  }
}

test q58_maximum_16_tranche_final_close_mint_policy_measurement() {
  let tx = maximum_close_transaction()
  validates_close(tx, maximum_close_redeemer())
}

test q58_maximum_16_tranche_final_timeout_applied_topology_fits() {
  let tx = maximum_timeout_transaction()
  and {
    list.length(tx.inputs) == 4,
    validates_close(tx, maximum_timeout_redeemer()),
    validates_coordinate_inputs(tx, [0, 1], 1),
  }
}

test q58_maximum_16_tranche_final_timeout_mint_policy_measurement() {
  let tx = maximum_timeout_transaction()
  validates_close(tx, maximum_timeout_redeemer())
}

fn open_yield_fixture() -> Pair<Transaction, availability.MintRedeemerV1> {
  let tx = open_transaction(0, 1, challenger, challenger)
  let redeemer = with_yield_index(
    open_redeemer(0, 1, challenger),
    list.length(tx.reference_inputs),
  )
  Pair(authenticated_yield_transaction(tx, redeemer), redeemer)
}

fn validates_yield_dispatch(
  tx: Transaction,
  redeemer: availability.MintRedeemerV1,
) -> Bool {
  availability_validator.availability_challenge.mint(
    hub_policy,
    miscellaneous_policy,
    parameters(),
    redeemer,
    availability_policy,
    tx,
  )
}

test availability_yield_accepts_authenticated_open_conjunction() {
  let Pair(tx, redeemer) = open_yield_fixture()
  validates_yield_dispatch(tx, redeemer) && invokes_availability_yield(
    tx,
    redeemer,
  )
}

test availability_yield_rejects_missing_withdrawal() fail {
  let Pair(tx, redeemer) = open_yield_fixture()
  validates_yield_dispatch(Transaction { ..tx, withdrawals: [] }, redeemer)
}

test availability_yield_rejects_duplicate_withdrawal() fail {
  let Pair(tx, redeemer) = open_yield_fixture()
  validates_yield_dispatch(
    Transaction {
      ..tx,
      withdrawals: list.concat(tx.withdrawals, tx.withdrawals),
    },
    redeemer,
  )
}

test availability_yield_rejects_nonzero_withdrawal() fail {
  let Pair(tx, redeemer) = open_yield_fixture()
  validates_yield_dispatch(
    Transaction { ..tx, withdrawals: [Pair(address.Script(da_policy), 1)] },
    redeemer,
  )
}

test availability_yield_rejects_missing_withdrawal_redeemer() fail {
  let Pair(tx, redeemer) = open_yield_fixture()
  validates_yield_dispatch(
    Transaction {
      ..tx,
      redeemers: list.filter(
        tx.redeemers,
        fn(entry) { entry.1st != Withdraw(address.Script(da_policy)) },
      ),
    },
    redeemer,
  )
}

test availability_yield_rejects_cross_arm_reference_role() fail {
  let Pair(tx, redeemer) = open_yield_fixture()
  let references = list.map(
    tx.reference_inputs,
    fn(input) {
      if input.output_reference == outref(90) {
        Input {
          ..input,
          output: Output {
            ..input.output,
            value: assets.from_lovelace(2_000_000)
              |> assets.add(miscellaneous_policy, yielding.settle_role, 1),
          },
        }
      } else {
        input
      }
    },
  )
  validates_yield_dispatch(
    Transaction { ..tx, reference_inputs: references },
    redeemer,
  )
}

test availability_yield_rejects_substituted_reference_script() fail {
  let Pair(tx, redeemer) = open_yield_fixture()
  let references = list.map(
    tx.reference_inputs,
    fn(input) {
      if input.output_reference == outref(90) {
        Input {
          ..input,
          output: Output {
            ..input.output,
            reference_script: Some(state_queue_policy),
          },
        }
      } else {
        input
      }
    },
  )
  validates_yield_dispatch(
    Transaction { ..tx, reference_inputs: references },
    redeemer,
  )
}

test availability_yield_rejects_out_of_range_reference_index() fail {
  let Pair(tx, redeemer) = open_yield_fixture()
  validates_yield_dispatch(
    tx,
    with_yield_index(redeemer, list.length(tx.reference_inputs)),
  )
}

test availability_yield_rejects_wrong_mint_constructor() fail {
  let Pair(tx, _) = open_yield_fixture()
  availability_yields.settle.withdraw(
    availability_policy,
    hub_policy,
    parameters(),
    availability.Yield,
    address.Script(da_policy),
    tx,
  )
}

// -- #688 negative matrix: Open, Settle, Close, Timeout, Coordinate ---------
//
// Every negative below is one mutation of a named honest control, and its
// comment names the check that refuses it.

fn validates_open(tx: Transaction) -> Bool {
  validates_close(tx, open_redeemer(0, 1, challenger))
}

/// The transaction of the honest Open control
/// `q58_open_accepts_exact_distinct_inputs_outputs_and_signer`.
fn honest_open() -> Transaction {
  open_transaction(0, 1, challenger, challenger)
}

/// The record datum the honest open writes.
fn honest_open_record() -> availability.ChallengeRecordV1 {
  availability.ChallengeRecordV1 {
    commitment: commitment(),
    challenge_asset_name: challenge_asset_name(),
    challenger,
    opened_at: open_inclusive_upper,
    response_deadline: open_inclusive_upper + availability.small_response_window_ms_v1,
  }
}

/// The record output the honest open writes.
fn honest_open_record_output() -> Output {
  record_output(challenger, open_inclusive_upper)
}

fn open_with_record_output(record: Output) -> Transaction {
  let base = honest_open()
  expect [_, ..rest] = base.outputs
  Transaction { ..base, outputs: [record, ..rest] }
}

fn open_with_record_datum(record: availability.ChallengeRecordV1) -> Transaction {
  open_with_record_output(
    Output { ..honest_open_record_output(), datum: InlineDatum(record) },
  )
}

fn honest_challenger_input_lovelace() -> Int {
  parameters().challenger_bond_lovelace + parameters().challenge_record_lovelace + close_fee
}

fn open_with_challenger_input_lovelace(lovelace: Int) -> Transaction {
  let base = honest_open()
  expect [node, funds] = base.inputs
  let adjusted = Input {
    ..funds,
    output: Output { ..funds.output, value: assets.from_lovelace(lovelace) },
  }
  Transaction { ..base, inputs: [node, adjusted] }
}

/// Positive control for the Open matrix helpers: rebuilding the record output
/// from the honest datum and the challenger input from the honest amount
/// reproduces an accepted open.
test availability_open_matrix_helpers_rebuild_the_honest_open() {
  and {
    validates_open(open_with_record_datum(honest_open_record())),
    validates_open(
      open_with_challenger_input_lovelace(honest_challenger_input_lovelace()),
    ),
  }
}

/// HAZ-6. The node attests `maximum_commitment()` and keeps that hash when
/// challenged, while the record, and every output derived from it, carries
/// `commitment()`. Refused by the open's node input-status match
/// `Attested{commitment_hash_v1(record.commitment)}`.
test availability_open_rejects_record_preimage_of_another_attested_hash() fail {
  let base = honest_open()
  expect [_, funds] = base.inputs
  expect [record, _, ..rest] = base.outputs
  let attested = maximum_commitment()
  validates_open(
    Transaction {
      ..base,
      inputs: [state_queue_input_with_status(attested_status(attested)), funds],
      outputs: [record, node_output(challenged_status(attested)), ..rest],
    },
  )
}

/// Refused by the challenger input value check
/// (`challenger_bond + challenge_record_lovelace + fee`).
test availability_open_rejects_challenger_input_one_lovelace_short() fail {
  validates_open(
    open_with_challenger_input_lovelace(honest_challenger_input_lovelace() - 1),
  )
}

/// Refused by the challenger input value check
/// (`challenger_bond + challenge_record_lovelace + fee`).
test availability_open_rejects_challenger_input_one_lovelace_over() fail {
  validates_open(
    open_with_challenger_input_lovelace(honest_challenger_input_lovelace() + 1),
  )
}

/// A wallet input next to the node and the challenger input. Refused by the
/// input count (`list.length(tx.inputs) == 2`).
test availability_open_rejects_third_input() fail {
  let base = honest_open()
  let extra = Input {
    output_reference: outref(5),
    output: enterprise_output(challenger, 2_000_000),
  }
  validates_open(
    Transaction { ..base, inputs: list.concat(base.inputs, [extra]) },
  )
}

/// Refused by the record output's address pin (the availability script).
test availability_open_rejects_record_at_foreign_script_address() fail {
  validates_open(
    open_with_record_output(
      Output {
        ..honest_open_record_output(),
        address: address.from_script(miscellaneous_policy),
      },
    ),
  )
}

/// The availability payment credential with a stake credential attached.
/// Refused by the record output's address pin (no stake credential).
test availability_open_rejects_record_at_staked_availability_address() fail {
  validates_open(
    open_with_record_output(
      Output {
        ..honest_open_record_output(),
        address: address.from_script(availability_policy)
          |> address.with_delegation_key(operator),
      },
    ),
  )
}

/// Refused by the record output's value pin (`challenge_record_lovelace` plus
/// exactly the minted challenge token).
test availability_open_rejects_record_with_extra_token() fail {
  let honest = honest_open_record_output()
  validates_open(
    open_with_record_output(
      Output {
        ..honest,
        value: assets.add(honest.value, miscellaneous_policy, "extra", 1),
      },
    ),
  )
}

/// Refused by the record output's value pin (`challenge_record_lovelace`).
test availability_open_rejects_record_one_lovelace_short() fail {
  let honest = honest_open_record_output()
  validates_open(
    open_with_record_output(
      Output { ..honest, value: assets.add(honest.value, "", "", -1) },
    ),
  )
}

/// Refused by the record output's value pin (`challenge_record_lovelace`).
test availability_open_rejects_record_one_lovelace_over() fail {
  let honest = honest_open_record_output()
  validates_open(
    open_with_record_output(
      Output { ..honest, value: assets.add(honest.value, "", "", 1) },
    ),
  )
}

/// The datum names the challenge the node outref would derive. Refused by
/// the record datum pin (`challenge_asset_name` = the minted name).
test availability_open_rejects_record_naming_another_challenge() fail {
  validates_open(
    open_with_record_datum(
      availability.ChallengeRecordV1 {
        ..honest_open_record(),
        challenge_asset_name: availability.challenge_asset_name_v1(outref(1)),
      },
    ),
  )
}

/// Refused by the record datum pin (`challenger` = the redeemer's signing
/// challenger).
test availability_open_rejects_record_naming_another_challenger() fail {
  validates_open(
    open_with_record_datum(
      availability.ChallengeRecordV1 {
        ..honest_open_record(),
        challenger: operator,
      },
    ),
  )
}

/// Refused by the record datum pin (`opened_at` = the inclusive upper bound).
test availability_open_rejects_record_with_earlier_opened_at() fail {
  validates_open(
    open_with_record_datum(
      availability.ChallengeRecordV1 {
        ..honest_open_record(),
        opened_at: open_inclusive_upper - 1,
      },
    ),
  )
}

/// Refused by the record datum pin
/// (`response_deadline = opened_at + response window`).
test availability_open_rejects_record_with_later_response_deadline() fail {
  validates_open(
    open_with_record_datum(
      availability.ChallengeRecordV1 {
        ..honest_open_record(),
        response_deadline: open_inclusive_upper + availability.small_response_window_ms_v1 + 1,
      },
    ),
  )
}

/// Refused by the record output's reference-script pin (`None`).
test availability_open_rejects_record_with_reference_script() fail {
  validates_open(
    open_with_record_output(
      Output {
        ..honest_open_record_output(),
        reference_script: Some(miscellaneous_policy),
      },
    ),
  )
}

/// Control: `q58_open_accepts_exact_distinct_inputs_outputs_and_signer`. The
/// only change is a reference script on the challenged node output
/// (`state_queue_output_index` 1). Refused by
/// `expect element_output_has_no_reference_script(output)` in the state-queue
/// DA core `da_availability_status_transition`, reached through
/// `validate_da_challenge_open_transition_v1`. A node carrying a reference
/// script could never be marked by `validate_completed_fraud_record`.
test availability_open_rejects_state_queue_output_with_reference_script() fail {
  let base = honest_open()
  expect [record, node, ..rest] = base.outputs
  validates_open(
    Transaction {
      ..base,
      outputs: [
        record,
        Output { ..node, reference_script: Some(node_reference_script_probe) },
        ..rest
      ],
    },
  )
}

/// The record's datum by hash only. Refused by the inline-datum read of the
/// record: the open takes the commitment preimage from the inline datum.
test availability_open_rejects_record_datum_by_hash() fail {
  let record_data: Data = honest_open_record()
  validates_open(
    open_with_record_output(
      Output {
        ..honest_open_record_output(),
        datum: transaction.DatumHash(
          crypto.blake2b_256(builtin.serialise_data(record_data)),
        ),
      },
    ),
  )
}

/// HAZ-8. The node and the challenger input swap outrefs, so every name in
/// the transaction derives from the node's outref. Refused by the challenge
/// name derivation from the challenger input's outref.
test availability_open_rejects_challenge_name_derived_from_node_outref() fail {
  let base = honest_open()
  expect [node, funds] = base.inputs
  validates_open(
    Transaction {
      ..base,
      inputs: [
        Input { ..node, output_reference: challenger_input_outref() },
        Input { ..funds, output_reference: outref(1) },
      ],
    },
  )
}

/// A genuine record moved to another script address, token included.
fn record_at_foreign_address() -> Input {
  let genuine = record_input(challenger)
  Input {
    ..genuine,
    output: Output {
      ..genuine.output,
      address: address.from_script(miscellaneous_policy),
    },
  }
}

/// A genuine record's datum at the availability address without the token.
fn record_without_challenge_token() -> Input {
  let genuine = record_input(challenger)
  Input {
    ..genuine,
    output: Output {
      ..genuine.output,
      value: assets.from_lovelace(parameters().challenge_record_lovelace),
    },
  }
}

/// A genuine record holding one lovelace more than `challenge_record_lovelace`.
fn record_with_extra_lovelace() -> Input {
  let genuine = record_input(challenger)
  Input {
    ..genuine,
    output: Output {
      ..genuine.output,
      value: assets.add(genuine.output.value, "", "", 1),
    },
  }
}

/// Mutations of the honest Settle control
/// `q58_settle_published_tranche_accepts_exact_thread_and_carrier`.
fn settle_with_record(record: Input) -> Transaction {
  Transaction {
    ..receipt_settlement_transaction(close_fee),
    reference_inputs: [record],
  }
}

/// Refused by the record authentication's address pin.
test q58_settle_rejects_record_at_foreign_address() fail {
  validates_close(
    settle_with_record(record_at_foreign_address()),
    settlement_redeemer(Some(2)),
  )
}

/// Refused by the record authentication's value pin.
test q58_settle_rejects_record_with_extra_lovelace() fail {
  validates_close(
    settle_with_record(record_with_extra_lovelace()),
    settlement_redeemer(Some(2)),
  )
}

/// The transaction of the honest Close control
/// `q58_close_accepts_exact_distinct_terminal_outputs`.
fn honest_close() -> Transaction {
  close_transaction(challenger, 6_000_000_000, 4_000_000_000, 1)
}

fn validates_honest_close_redeemer(tx: Transaction) -> Bool {
  validates_close(tx, close_redeemer(1))
}

fn close_with_record(record: Input) -> Transaction {
  let base = honest_close()
  expect [_, node, terminal_input] = base.inputs
  Transaction { ..base, inputs: [record, node, terminal_input] }
}

fn close_with_node_status(
  status: availability.StateQueueStatusV1,
) -> Transaction {
  let base = honest_close()
  expect [record, _, terminal_input] = base.inputs
  Transaction {
    ..base,
    inputs: [record, state_queue_input_with_status(status), terminal_input],
  }
}

fn close_with_refund_delta(delta: Int) -> Transaction {
  let base = honest_close()
  expect [node, refund] = base.outputs
  Transaction {
    ..base,
    outputs: [
      node,
      Output { ..refund, value: assets.add(refund.value, "", "", delta) },
    ],
  }
}

/// Another challenge's name, as the node outref would derive it.
fn other_challenge_asset_name() -> ByteArray {
  availability.challenge_asset_name_v1(outref(7))
}

/// Refused by the record authentication's address pin.
test q58_close_rejects_record_at_foreign_address() fail {
  validates_honest_close_redeemer(
    close_with_record(record_at_foreign_address()),
  )
}

/// Refused by the record authentication's value pin (no challenge token).
test q58_close_rejects_record_without_challenge_token() fail {
  validates_honest_close_redeemer(
    close_with_record(record_without_challenge_token()),
  )
}

/// Refused by the record authentication's value pin.
test q58_close_rejects_record_with_extra_lovelace() fail {
  validates_honest_close_redeemer(
    close_with_record(record_with_extra_lovelace()),
  )
}

/// G6. The node is challenged under the record's name but another
/// commitment's hash. Refused by the exact input status
/// `Challenged{commitment_hash_v1(record.commitment), record.challenge_asset_name}`.
test q58_close_rejects_node_challenged_under_another_commitment() fail {
  validates_honest_close_redeemer(
    close_with_node_status(
      availability.Challenged {
        commitment_hash: availability.commitment_hash_v1(maximum_commitment()),
        challenge_asset_name: challenge_asset_name(),
      },
    ),
  )
}

/// G6. The node carries the record's commitment hash but another challenge's
/// name. Refused by the exact input status
/// `Challenged{commitment_hash_v1(record.commitment), record.challenge_asset_name}`.
test q58_close_rejects_node_challenged_under_another_challenge() fail {
  validates_honest_close_redeemer(
    close_with_node_status(
      availability.Challenged {
        commitment_hash: availability.commitment_hash_v1(commitment()),
        challenge_asset_name: other_challenge_asset_name(),
      },
    ),
  )
}

/// Refused by the challenger refund amount
/// (`remaining - fee + challenge_record_lovelace`).
test q58_close_rejects_challenger_refund_one_lovelace_short() fail {
  validates_honest_close_redeemer(close_with_refund_delta(-1))
}

/// Refused by the challenger refund amount
/// (`remaining - fee + challenge_record_lovelace`).
test q58_close_rejects_challenger_refund_one_lovelace_over() fail {
  validates_honest_close_redeemer(close_with_refund_delta(1))
}

/// A second output to the challenger. Refused by the output count
/// (`list.length(tx.outputs) == 2`: the node and the one refund).
test q58_close_rejects_second_challenger_output() fail {
  let base = honest_close()
  validates_honest_close_redeemer(
    Transaction {
      ..base,
      outputs: list.concat(base.outputs, [enterprise_output(challenger, 1)]),
    },
  )
}

/// Control: `q58_close_accepts_exact_distinct_terminal_outputs`. The only
/// change is a reference script on the published node output
/// (`state_queue_output_index` 0). Refused by
/// `expect element_output_has_no_reference_script(output)` in the state-queue
/// DA core `da_availability_status_transition`, reached through
/// `validate_da_availability_status_transition`. A node carrying a reference
/// script could never be marked by `validate_completed_fraud_record`.
test q58_close_rejects_state_queue_output_with_reference_script() fail {
  let base = honest_close()
  expect [node, refund] = base.outputs
  validates_honest_close_redeemer(
    Transaction {
      ..base,
      outputs: [
        Output { ..node, reference_script: Some(node_reference_script_probe) },
        refund,
      ],
    },
  )
}

// -- #693 Timeout slashes the DA bond pool ---------------------------------
//
// Test parameters: da_bond 10,000,000,000, penalty 2,500,000,000, floor
// 5,000,000, max_timeout_fee 1,200,000, challenge record 27,000,000; the
// challenger reserve on the terminal accumulator is 10,000,000,000.

/// The transaction of the honest Timeout control
/// `q58_timeout_accepts_full_pool_slash_with_the_penalty_as_the_fee`: a full
/// `Bonded` pool and `c = 0`.
fn honest_timeout() -> Transaction {
  timeout_transaction(
    challenge_asset_name(),
    bonded_pool_datum(),
    full_pool_lovelace,
    0,
  )
}

fn validates_honest_timeout_redeemer(tx: Transaction) -> Bool {
  validates_close(tx, timeout_redeemer(0, 3, 1, 0))
}

/// An honest Timeout accepted by the timeout yield and by the record and
/// terminal accumulator Coordinate spends.
fn timeout_is_accepted(tx: Transaction) -> Bool {
  and {
    validates_honest_timeout_redeemer(tx),
    validates_coordinate_inputs(tx, [0, 1], 1),
  }
}

fn timeout_with_record(record: Input) -> Transaction {
  let base = honest_timeout()
  expect [_, terminal_input, node, pool_in] = base.inputs
  Transaction { ..base, inputs: [record, terminal_input, node, pool_in] }
}

fn timeout_with_pool_input(tx: Transaction, pool_in: Input) -> Transaction {
  expect [record, terminal_input, node, _] = tx.inputs
  Transaction { ..tx, inputs: [record, terminal_input, node, pool_in] }
}

fn timeout_with_pool_output(tx: Transaction, pool_out: Output) -> Transaction {
  expect [challenger_output, _] = tx.outputs
  Transaction { ..tx, outputs: [challenger_output, pool_out] }
}

fn timeout_with_refund_delta(delta: Int) -> Transaction {
  let base = honest_timeout()
  expect [refund, slashed_pool] = base.outputs
  Transaction {
    ..base,
    outputs: [
      Output { ..refund, value: assets.add(refund.value, "", "", delta) },
      slashed_pool,
    ],
  }
}

/// The honest full-pool Timeout with its fee lowered by `delta` and the same
/// lovelace moved into the merged challenger output: the transaction still
/// balances, and the challenger keeps part of the penalty share.
fn timeout_with_fee_moved_to_challenger(
  base: Transaction,
  delta: Int,
) -> Transaction {
  expect [refund, slashed_pool] = base.outputs
  Transaction {
    ..base,
    fee: base.fee - delta,
    outputs: [
      Output { ..refund, value: assets.add(refund.value, "", "", delta) },
      slashed_pool,
    ],
  }
}

fn honest_pool_output() -> Output {
  expect [_, slashed_pool] = honest_timeout().outputs
  slashed_pool
}

// -- Accepted: the exact split ----------------------------------------------

/// Backing above one bond: the pool gives up exactly `da_bond` and keeps the
/// rest. fee = penalty (c = 0), challenger = reserve + record + 7,500,000,000.
test q58_timeout_accepts_pool_above_one_bond_taking_exactly_the_bond() {
  let tx = timeout_transaction(
    challenge_asset_name(),
    bonded_pool_datum(),
    13_005_000_000,
    0,
  )
  expect [challenger_output, slashed_pool] = tx.outputs
  and {
    tx.fee == 2_500_000_000,
    assets.lovelace_of(challenger_output.value) == 10_000_000_000 + 27_000_000 + 7_500_000_000,
    slashed_pool.value == pool_value(3_005_000_000),
    timeout_is_accepted(tx),
  }
}

/// Partial pool (penalty < backing < da_bond): taken = 6,000,000,000, the
/// penalty is burned in full, payout = 3,500,000,000, the pool is left at its
/// floor. Here the challenger also adds c = 150,000 to the fee.
test q58_timeout_accepts_partial_pool_penalty_first_split() {
  let tx = timeout_transaction(
    challenge_asset_name(),
    bonded_pool_datum(),
    6_005_000_000,
    150_000,
  )
  expect [challenger_output, slashed_pool] = tx.outputs
  and {
    tx.fee == 2_500_000_000 + 150_000,
    assets.lovelace_of(challenger_output.value) == 10_000_000_000 - 150_000 + 27_000_000 + 3_500_000_000,
    slashed_pool.value == pool_value(5_000_000),
    timeout_is_accepted(tx),
  }
}

/// Backing below the penalty: everything taken goes to the fee, payout 0,
/// with c = 0 and with c = 100.
test q58_timeout_accepts_pool_below_the_penalty_all_taken_to_the_fee() {
  let no_contribution = timeout_transaction(
    challenge_asset_name(),
    bonded_pool_datum(),
    1_005_000_000,
    0,
  )
  let contribution = timeout_transaction(
    challenge_asset_name(),
    bonded_pool_datum(),
    1_005_000_000,
    100,
  )
  expect [challenger_output, slashed_pool] = contribution.outputs
  and {
    no_contribution.fee == 1_000_000_000,
    contribution.fee == 1_000_000_100,
    assets.lovelace_of(challenger_output.value) == 10_000_000_000 - 100 + 27_000_000,
    slashed_pool.value == pool_value(5_000_000),
    timeout_is_accepted(no_contribution),
    timeout_is_accepted(contribution),
  }
}

/// Empty pool, at the floor and below it (backing clamps to 0): the pool is
/// still spent and passes through unchanged, and c pays the whole fee.
test q58_timeout_accepts_empty_pool_passing_through_with_c_as_the_fee() {
  let at_floor = timeout_transaction(
    challenge_asset_name(),
    bonded_pool_datum(),
    5_000_000,
    close_fee,
  )
  let below_floor = timeout_transaction(
    challenge_asset_name(),
    bonded_pool_datum(),
    2_000_000,
    close_fee,
  )
  expect [challenger_output, slashed_pool] = at_floor.outputs
  expect [_, below_floor_pool] = below_floor.outputs
  and {
    at_floor.fee == close_fee,
    below_floor.fee == close_fee,
    assets.lovelace_of(challenger_output.value) == 10_000_000_000 - close_fee + 27_000_000,
    slashed_pool.value == pool_value(5_000_000),
    below_floor_pool.value == pool_value(2_000_000),
    timeout_is_accepted(at_floor),
    timeout_is_accepted(below_floor),
  }
}

/// Dust payout (backing = penalty + 1): the merged challenger output carries
/// the one lovelace of payout, so the Timeout stays buildable (D3).
test q58_timeout_accepts_dust_payout_in_the_merged_challenger_output() {
  let tx = timeout_transaction(
    challenge_asset_name(),
    bonded_pool_datum(),
    2_505_000_001,
    0,
  )
  expect [challenger_output, slashed_pool] = tx.outputs
  and {
    tx.fee == 2_500_000_000,
    assets.lovelace_of(challenger_output.value) == 10_000_000_000 + 27_000_000 + 1,
    slashed_pool.value == pool_value(5_000_000),
    timeout_is_accepted(tx),
  }
}

/// G2: a `Withdrawing` pool is slashed like a `Bonded` one and keeps its
/// `unlock_at`.
test q58_timeout_accepts_withdrawing_pool_and_keeps_unlock_at() {
  let tx = timeout_transaction(
    challenge_asset_name(),
    withdrawing_pool_datum(123_456),
    full_pool_lovelace,
    0,
  )
  expect [_, slashed_pool] = tx.outputs
  and {
    slashed_pool.datum == InlineDatum(withdrawing_pool_datum(123_456)),
    slashed_pool.value == pool_value(5_000_000),
    timeout_is_accepted(tx),
  }
}

/// Control for the fee cap: c at exactly `max_timeout_fee_lovelace` on top of
/// the full penalty. The cap binds c, not the whole fee.
test q58_timeout_accepts_challenger_fee_at_the_cap_on_top_of_the_penalty() {
  let tx = timeout_transaction(
    challenge_asset_name(),
    bonded_pool_datum(),
    full_pool_lovelace,
    1_200_000,
  )
  and {
    tx.fee == 2_500_000_000 + 1_200_000,
    timeout_is_accepted(tx),
  }
}

/// Control for the fee cap on an empty pool: c = the cap pays the whole fee.
test q58_timeout_accepts_challenger_fee_at_the_cap_on_an_empty_pool() {
  timeout_is_accepted(
    timeout_transaction(
      challenge_asset_name(),
      bonded_pool_datum(),
      5_000_000,
      1_200_000,
    ),
  )
}

// -- Refused: the pool input ------------------------------------------------

/// `pool_input_index` names the challenged node instead of the pool. Refused
/// by `get_authentic_pool_input`'s credential pin
/// (`Script(da_bond_pool_policy_id)`); its NFT pin would refuse it next.
test q58_timeout_rejects_pool_index_naming_a_non_pool_input() fail {
  validates_close(honest_timeout(), timeout_redeemer(0, 2, 1, 0))
}

/// The pool left out altogether, in the shape a committee that challenged its
/// own withheld block would want: no pool input or output, c pays the fee and
/// the challenger gets back only the reserve and the record's lovelace.
/// Refused by `get_authentic_pool_input`: there is no input at
/// `pool_input_index`.
test q58_timeout_rejects_without_pool_input() fail {
  let base = honest_timeout()
  expect [record, terminal_input, node, _] = base.inputs
  timeout_is_accepted(
    Transaction {
      ..base,
      inputs: [record, terminal_input, node],
      outputs: [
        enterprise_output(
          challenger,
          timeout_remaining_lovelace - close_fee + parameters().challenge_record_lovelace,
        ),
      ],
      fee: close_fee,
    },
  )
}

/// A counterfeit pool at the pool address without the NFT. Refused by
/// `get_authentic_pool_input`'s NFT quantity pin.
test q58_timeout_rejects_counterfeit_pool_without_nft() fail {
  let counterfeit = Input {
    output_reference: outref(6),
    output: Output {
      ..pool_output(bonded_pool_datum(), full_pool_lovelace),
      value: assets.from_lovelace(full_pool_lovelace),
    },
  }
  validates_honest_timeout_redeemer(
    timeout_with_pool_input(honest_timeout(), counterfeit),
  )
}

/// The NFT at another script credential, spent and continued there, so the
/// continuation's address still equals the input's. Refused by
/// `get_authentic_pool_input`'s credential pin
/// (`Script(da_bond_pool_policy_id)`).
test q58_timeout_rejects_pool_nft_at_another_credential() fail {
  let elsewhere = address.from_script(miscellaneous_policy)
  let pool_in = pool_input(bonded_pool_datum(), full_pool_lovelace)
  validates_honest_timeout_redeemer(
    timeout_with_pool_output(
      timeout_with_pool_input(
        honest_timeout(),
        Input {
          ..pool_in,
          output: Output { ..pool_in.output, address: elsewhere },
        },
      ),
      Output { ..honest_pool_output(), address: elsewhere },
    ),
  )
}

// -- Refused: the fee -------------------------------------------------------

/// fee = fee_part - 1 + c with c = 0, the lovelace moved into the challenger
/// output: the penalty share is not burned in full. Refused by
/// `challenger_fee >= 0` (`c = tx.fee - fee_part`).
test q58_timeout_rejects_fee_one_lovelace_below_the_penalty_share() fail {
  timeout_is_accepted(timeout_with_fee_moved_to_challenger(honest_timeout(), 1))
}

/// c < 0 on a pool below the penalty (fee_part = taken): the fee is 1,000
/// lovelace short of what the pool gives up, moved to the challenger. Refused
/// by `challenger_fee >= 0`.
test q58_timeout_rejects_negative_challenger_fee_on_a_short_pool() fail {
  timeout_is_accepted(
    timeout_with_fee_moved_to_challenger(
      timeout_transaction(
        challenge_asset_name(),
        bonded_pool_datum(),
        1_005_000_000,
        0,
      ),
      1_000,
    ),
  )
}

/// c = max_timeout_fee + 1 on top of the full penalty. Refused by
/// `challenger_fee <= max_timeout_fee_lovelace`.
test q58_timeout_rejects_challenger_fee_one_above_the_cap() fail {
  timeout_is_accepted(
    timeout_transaction(
      challenge_asset_name(),
      bonded_pool_datum(),
      full_pool_lovelace,
      1_200_001,
    ),
  )
}

/// c = max_timeout_fee + 1 paying the whole fee of an empty pool. Refused by
/// `challenger_fee <= max_timeout_fee_lovelace`.
test q58_timeout_rejects_challenger_fee_one_above_the_cap_on_an_empty_pool() fail {
  timeout_is_accepted(
    timeout_transaction(
      challenge_asset_name(),
      bonded_pool_datum(),
      5_000_000,
      1_200_001,
    ),
  )
}

/// A zero fee on an empty pool. Refused by `tx.fee > 0`.
test q58_timeout_rejects_zero_fee_on_an_empty_pool() fail {
  timeout_is_accepted(
    timeout_transaction(
      challenge_asset_name(),
      bonded_pool_datum(),
      5_000_000,
      0,
    ),
  )
}

// -- Refused: the challenger output -----------------------------------------

/// Refused by the merged challenger output amount
/// (`remaining - c + challenge_record_lovelace + payout`).
test q58_timeout_rejects_merged_challenger_output_one_lovelace_short() fail {
  validates_honest_timeout_redeemer(timeout_with_refund_delta(-1))
}

/// Refused by the merged challenger output amount
/// (`remaining - c + challenge_record_lovelace + payout`).
test q58_timeout_rejects_merged_challenger_output_one_lovelace_over() fail {
  validates_honest_timeout_redeemer(timeout_with_refund_delta(1))
}

/// The refund and the payout split across two challenger outputs, together
/// exactly the merged amount. Refused by the merged amount at
/// `challenger_refund_output_index` and by the challenger output count
/// (`challenger_output_count == 1`).
test q58_timeout_rejects_refund_and_payout_split_across_two_outputs() fail {
  let base = honest_timeout()
  expect [merged, slashed_pool] = base.outputs
  let payout = 7_500_000_000
  validates_honest_timeout_redeemer(
    Transaction {
      ..base,
      outputs: [
        Output { ..merged, value: assets.add(merged.value, "", "", -payout) },
        slashed_pool,
        enterprise_output(challenger, payout),
      ],
    },
  )
}

// -- Refused: the pool output -----------------------------------------------

/// The pool gives up one lovelace more than `taken`. Refused by the pool
/// output value (`pool_in - taken` lovelace plus the NFT, exactly).
test q58_timeout_rejects_pool_output_one_lovelace_short() fail {
  validates_honest_timeout_redeemer(
    timeout_with_pool_output(
      honest_timeout(),
      pool_output(bonded_pool_datum(), 4_999_999),
    ),
  )
}

/// The pool gives up one lovelace less than `taken`. Refused by the pool
/// output value.
test q58_timeout_rejects_pool_output_one_lovelace_over() fail {
  validates_honest_timeout_redeemer(
    timeout_with_pool_output(
      honest_timeout(),
      pool_output(bonded_pool_datum(), 5_000_001),
    ),
  )
}

/// The pool output without its NFT. Refused by the pool output value.
test q58_timeout_rejects_pool_output_without_nft() fail {
  validates_honest_timeout_redeemer(
    timeout_with_pool_output(
      honest_timeout(),
      Output { ..honest_pool_output(), value: assets.from_lovelace(5_000_000) },
    ),
  )
}

/// The pool output with an extra token. Refused by the pool output value.
test q58_timeout_rejects_pool_output_with_extra_token() fail {
  validates_honest_timeout_redeemer(
    timeout_with_pool_output(
      honest_timeout(),
      Output {
        ..honest_pool_output(),
        value: pool_value(5_000_000)
          |> assets.add(miscellaneous_policy, "extra", 1),
      },
    ),
  )
}

/// `Bonded` continued as `Withdrawing`. Refused by the pool output datum
/// (`pool_output.datum == pool_input.datum`).
test q58_timeout_rejects_pool_output_bonded_to_withdrawing() fail {
  validates_honest_timeout_redeemer(
    timeout_with_pool_output(
      honest_timeout(),
      pool_output(withdrawing_pool_datum(0), 5_000_000),
    ),
  )
}

/// `Withdrawing` continued as `Bonded`. Refused by the pool output datum.
test q58_timeout_rejects_pool_output_withdrawing_to_bonded() fail {
  validates_honest_timeout_redeemer(
    timeout_with_pool_output(
      timeout_transaction(
        challenge_asset_name(),
        withdrawing_pool_datum(123_456),
        full_pool_lovelace,
        0,
      ),
      pool_output(bonded_pool_datum(), 5_000_000),
    ),
  )
}

/// `Withdrawing` continued with an earlier `unlock_at`. Refused by the pool
/// output datum.
test q58_timeout_rejects_pool_output_with_another_unlock_at() fail {
  validates_honest_timeout_redeemer(
    timeout_with_pool_output(
      timeout_transaction(
        challenge_asset_name(),
        withdrawing_pool_datum(123_456),
        full_pool_lovelace,
        0,
      ),
      pool_output(withdrawing_pool_datum(0), 5_000_000),
    ),
  )
}

/// The pool output with a stake credential attached. Refused by the pool
/// output address (`pool_output.address == pool_input.address`).
test q58_timeout_rejects_pool_output_with_changed_stake_credential() fail {
  validates_honest_timeout_redeemer(
    timeout_with_pool_output(
      honest_timeout(),
      Output {
        ..honest_pool_output(),
        address: address.from_script(da_bond_pool_policy)
          |> address.with_delegation_key(operator),
      },
    ),
  )
}

/// The pool output with a reference script. Refused by
/// `pool_output.reference_script == None`.
test q58_timeout_rejects_pool_output_with_reference_script() fail {
  validates_honest_timeout_redeemer(
    timeout_with_pool_output(
      honest_timeout(),
      Output {
        ..honest_pool_output(),
        reference_script: Some(miscellaneous_policy),
      },
    ),
  )
}

/// The pool output index names the challenger output. Refused by
/// `pool_output_index != challenger_refund_output_index` (the pool address pin
/// would refuse it next).
test q58_timeout_rejects_pool_output_index_aliased_with_challenger_output() fail {
  validates_close(honest_timeout(), timeout_redeemer(0, 3, 0, 0))
}

// -- Refused: the record and the node ---------------------------------------

/// Refused by the record authentication's address pin.
test q58_timeout_rejects_record_at_foreign_address() fail {
  validates_honest_timeout_redeemer(
    timeout_with_record(record_at_foreign_address()),
  )
}

/// Refused by the record authentication's value pin (no challenge token).
test q58_timeout_rejects_record_without_challenge_token() fail {
  validates_honest_timeout_redeemer(
    timeout_with_record(record_without_challenge_token()),
  )
}

/// Refused by the record authentication's value pin.
test q58_timeout_rejects_record_with_extra_lovelace() fail {
  validates_honest_timeout_redeemer(
    timeout_with_record(record_with_extra_lovelace()),
  )
}

/// G6. The removed node carries the record's commitment hash but another
/// challenge's name. Refused by the node status match
/// `Challenged{commitment_hash_v1(record.commitment), record.challenge_asset_name}`.
test q58_timeout_rejects_node_challenged_under_another_challenge() fail {
  let base = honest_timeout()
  expect [record, terminal_input, _, pool_in] = base.inputs
  let node = Input {
    output_reference: outref(4),
    output: node_output(
      availability.Challenged {
        commitment_hash: availability.commitment_hash_v1(commitment()),
        challenge_asset_name: other_challenge_asset_name(),
      },
    ),
  }
  validates_honest_timeout_redeemer(
    Transaction { ..base, inputs: [record, terminal_input, node, pool_in] },
  )
}

/// The Close control's coordinate spend of the record, with the mint
/// redeemer swapped for an open. Refused by the Coordinate spend's
/// `OpenChallenge` arm: an open spends nothing at the availability address.
test q58_coordinate_rejects_record_spend_under_open_challenge() fail {
  let open_data: Data = open_redeemer(0, 1, challenger)
  validates_coordinate_input(
    Transaction {
      ..honest_close(),
      redeemers: [Pair(Mint(availability_policy), open_data)],
    },
    0,
    0,
  )
}

/// The Timeout control with an unrelated availability UTxO spent under the
/// same coordinate. Refused by the Coordinate spend's `TimeoutChallenge` arm,
/// which admits only the record and the terminal accumulator.
test q58_coordinate_rejects_undeclared_input_under_timeout() fail {
  let base = honest_timeout()
  let unrelated = Input {
    ..receipt_input(challenger, 2_000_000),
    output_reference: outref(8),
  }
  validates_coordinate_input(
    Transaction { ..base, inputs: list.concat(base.inputs, [unrelated]) },
    4,
    1,
  )
}
