//// End-to-end coverage of the availability timeout with the pooled DA bond
//// slash (spec #685 §5; tickets #687, #693).
////
//// Each scenario is ONE Transaction: the first step of an unavailable-block
//// removal, taken on the Idle correction lock. It is built as the ledger
//// would present it (inputs in output-reference order; redeemers ordered
//// Spend by input, then Mint by policy, then Withdraw by credential; one
//// redeemer per script input, minting policy and script withdrawal, and no
//// other), and every one of its redeemers is run through the real handler:
////
////   Spend(record), Spend(terminal) -> availability_challenge.spend Coordinate
////   Spend(element A), Spend(element B) -> state_queue spend LinkedListMutation
////   Spend(pool)                       -> da_bond_pool.spend Slash
////   Spend(correction lock)            -> correction_lock spend Correct
////   Mint(state queue)                 -> state_queue mint, constructor 5
////   Mint(availability)                -> availability_challenge.mint Timeout
////   Withdraw(availability timeout)    -> availability_challenge_yields.timeout
////   Withdraw(state-queue unavailable) -> state_queue_yields.remove_unavailable
////
//// Both removal approaches are covered. `PruneTimedOutBlockDescendant` keeps
//// the challenged head (A) as the anchor, burns its descendant (B) and moves
//// the lock Idle -> Locked. `RemoveTimedOutHead` spends the confirmed-state
//// root (A) as the anchor, burns the challenged head (B) and keeps the lock
//// Idle -> Idle.
////
//// The negatives keep every other script accepting (a non-`fail` control
//// runs every redeemer except the refusing one) and show the refusing script
//// in a `fail` test that runs only it.

use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto
use aiken/interval.{between}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use availability_challenge as availability_validator
use availability_challenge_yields as availability_yields
use cardano/address.{Script}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, ScriptPurpose,
  Spend, Transaction, Withdraw,
}
use correction_lock as correction_lock_validator
use da_bond_pool as pool_validator
use midgard/availability_challenge as availability
use midgard/availability_challenge_yield as yielding
use midgard/correction_lock
use midgard/da_bond_pool as pool
use midgard/hub_oracle as hub
use midgard/ledger_state.{ConfirmedState, HeaderV1, protocol_version_v1}
use midgard/state_queue
use midgard/state_queue_yield
use midgard/validation_merkle_v1
use state_queue as state_queue_validator
use state_queue_yields

// ---------------------------------------------------------------------------
// Script hashes and keys (all distinct)
// ---------------------------------------------------------------------------

/// The hub oracle NFT policy; it also mints the correction-lock singleton.
const hub_policy = #"11111111111111111111111111111111111111111111111111111111"

/// The state-queue minting policy (hub datum field 4).
const state_queue_policy =
  #"22222222222222222222222222222222222222222222222222222222"

/// The state-queue spending script: the list elements sit here.
const state_queue_script =
  #"23232323232323232323232323232323232323232323232323232323"

/// The availability policy, which is also its spending script.
const availability_policy =
  #"33333333333333333333333333333333333333333333333333333333"

/// The pool NFT policy, which is also the pool's spending script.
const pool_policy = #"35353535353535353535353535353535353535353535353535353535"

const correction_lock_script =
  #"44444444444444444444444444444444444444444444444444444444"

const reference_script_auth_policy =
  #"55555555555555555555555555555555555555555555555555555555"

const availability_timeout_yield =
  #"77777777777777777777777777777777777777777777777777777777"

const availability_close_yield =
  #"78787878787878787878787878787878787878787878787878787878"

const state_queue_unavailable_yield =
  #"79797979797979797979797979797979797979797979797979797979"

const da_params_policy =
  #"e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1"

/// Every parameter no handler on this path reads.
const foreign_policy =
  #"f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0"

const faulty_operator =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const rotated_operator =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const challenger = #"c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0"

/// The submitter, who funds a resume step and receives the change.
const submitter = #"c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1"

const parent_header_hash =
  #"5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a"

const grandparent_header_hash =
  #"5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b"

const empty_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

const terminal =
  #"7777777777777777777777777777777777777777777777777777777777777777"

// ---------------------------------------------------------------------------
// Amounts
// ---------------------------------------------------------------------------

/// The challenger reserve left on the terminal accumulator.
const remaining = 10_000_000_000

/// The lovelace every list element and the correction lock carry.
const element_lovelace = 2_000_000

/// Backing exactly one bond: `taken = da_bond`, the penalty pays the fee.
const full_pool = 10_005_000_000

/// Backing 6,000 ADA: under one bond, over the penalty.
const partial_pool_over_penalty = 6_005_000_000

/// Backing 1,000 ADA: under the penalty, so all of it is burned as fee.
const partial_pool_under_penalty = 1_005_000_000

/// Exactly the floor: backing 0, nothing is taken.
const empty_pool = 5_000_000

/// The attacker-chosen fee of the optimised Close substitution.
const close_fee = 100

/// The fee a resume step's submitter pays from its wallet.
const resume_fee = 200_000

/// Canonical parameters, as `availability-challenge.test.ak` uses them.
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

// ---------------------------------------------------------------------------
// Block, commitment and challenge identity
// ---------------------------------------------------------------------------

fn header(operator: ByteArray, previous_header_hash: ByteArray) -> HeaderV1 {
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
    start_time: 1_000,
    end_time: 2_000,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: previous_header_hash,
    operator_vkey: operator,
    protocol_version: protocol_version_v1,
  }
}

fn hash_header(node_header: HeaderV1) -> ByteArray {
  let header_data: Data = node_header
  crypto.blake2b_224(builtin.serialise_data(header_data))
}

/// The challenged, unavailable block.
fn target_header() -> HeaderV1 {
  header(faulty_operator, parent_header_hash)
}

fn target_hash() -> ByteArray {
  hash_header(target_header())
}

/// Its descendant, built on it by another operator.
fn descendant_header() -> HeaderV1 {
  header(rotated_operator, target_hash())
}

fn descendant_hash() -> ByteArray {
  hash_header(descendant_header())
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

fn commitment() -> availability.CommitmentV1 {
  availability.CommitmentV1 {
    version: availability.commitment_version_v1,
    deployment_identity: hub_policy,
    header_hash: target_hash(),
    payload_byte_length: 1,
    response_geometry: parameters().response_geometry,
    tranche_descriptors: [descriptor()],
  }
}

/// The name the open minted: DACH-prefixed, 32 bytes.
fn challenge() -> ByteArray {
  availability.challenge_asset_name_v1(
    OutputReference {
      transaction_id: #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      output_index: 7,
    },
  )
}

fn terminal_name() -> ByteArray {
  availability.terminal_accumulator_asset_name_v1(challenge())
}

fn challenged_status() -> availability.StateQueueStatusV1 {
  availability.Challenged {
    commitment_hash: availability.commitment_hash_v1(commitment()),
    challenge_asset_name: challenge(),
  }
}

const opened_at = 2_500

fn response_deadline() -> Int {
  opened_at + availability.small_response_window_ms_v1
}

// ---------------------------------------------------------------------------
// UTxOs
// ---------------------------------------------------------------------------

fn outref(output_index: Int) -> OutputReference {
  OutputReference {
    transaction_id: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    output_index,
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

fn record_input() -> Input {
  let datum: availability.ChallengeRecordV1 = availability.ChallengeRecordV1 {
    commitment: commitment(),
    challenge_asset_name: challenge(),
    challenger,
    opened_at,
    response_deadline: response_deadline(),
  }
  Input {
    output_reference: outref(0),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(parameters().challenge_record_lovelace)
        |> assets.add(availability_policy, challenge(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn terminal_input(has_timed_out_tranche: Bool) -> Input {
  let datum: availability.TerminalAccumulatorDatumV1 = availability.TerminalAccumulatorDatumV1 {
    deployment_identity: hub_policy,
    header_hash: target_hash(),
    challenge_asset_name: challenge(),
    next_tranche_index: 1,
    folded_terminal_accumulator: terminal,
    has_timed_out_tranche,
    response_deadline: response_deadline(),
    challenger,
    remaining_challenger_lovelace: remaining,
  }
  Input {
    output_reference: outref(1),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(remaining)
        |> assets.add(availability_policy, terminal_name(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn node_input(
  output_index: Int,
  node_header: HeaderV1,
  status: availability.StateQueueStatusV1,
  link: Option<ByteArray>,
) -> Input {
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Node(
      state_queue.StateQueueNode {
        header: node_header,
        da_attestation: status,
        proven_fraud: None,
      },
    ),
    link,
  }
  Input {
    output_reference: outref(output_index),
    output: Output {
      address: address.from_script(state_queue_script),
      value: assets.from_lovelace(element_lovelace)
        |> assets.add(
            state_queue_policy,
            state_queue.block_asset_name_prefix
              |> bytearray.concat(hash_header(node_header)),
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn root_input(output_index: Int) -> Input {
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Root(
      ConfirmedState {
        header_hash: parent_header_hash,
        prev_header_hash: grandparent_header_hash,
        utxo_root: empty_root,
        start_time: 0,
        end_time: 1_000,
        protocol_version: protocol_version_v1,
      },
    ),
    link: Some(target_hash()),
  }
  Input {
    output_reference: outref(output_index),
    output: Output {
      address: address.from_script(state_queue_script),
      value: assets.from_lovelace(element_lovelace)
        |> assets.add(
            state_queue_policy,
            state_queue.confirmed_state_asset_name,
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

/// The challenged head, still `Challenged`, linking to its descendant (prune)
/// or terminal (head removal).
fn target_node_input(output_index: Int, link: Option<ByteArray>) -> Input {
  node_input(output_index, target_header(), challenged_status(), link)
}

fn descendant_node_input(output_index: Int) -> Input {
  node_input(
    output_index,
    descendant_header(),
    state_queue.no_da_attestation,
    None,
  )
}

/// The anchor continues with its value and data unchanged and skips the
/// removed element.
fn continue_element(input: Input, link: Option<ByteArray>) -> Output {
  expect InlineDatum(input_datum_data) = input.output.datum
  expect linked_list.Element { data, .. }: state_queue.Datum = input_datum_data
  let continued_datum: state_queue.Datum = linked_list.Element { data, link }
  Output { ..input.output, datum: InlineDatum(continued_datum) }
}

fn element_token(input: Input) -> ByteArray {
  expect [Pair(name, 1)] = input.output.value
    |> assets.tokens(state_queue_policy)
    |> dict.to_pairs
  name
}

fn lock_input(datum: correction_lock.Datum) -> Input {
  Input { output_reference: outref(5), output: lock_output(datum) }
}

fn lock_output(datum: correction_lock.Datum) -> Output {
  Output {
    address: address.from_script(correction_lock_script),
    value: assets.from_lovelace(element_lovelace)
      |> assets.add(hub_policy, correction_lock.asset_name, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn locked_on_target() -> correction_lock.Datum {
  correction_lock.Locked {
    target_header_hash: target_hash(),
    correction_identity: correction_lock.AvailabilityChallenge {
      challenge_asset_name: challenge(),
    },
  }
}

fn bonded() -> Data {
  let datum: Data = pool.Bonded
  datum
}

/// A pending withdrawal, far from unlocking.
fn withdrawing() -> Data {
  let datum: Data = pool.Withdrawing { unlock_at: 9_000_000_000_000 }
  datum
}

fn pool_output(datum: Data, lovelace: Int) -> Output {
  Output {
    address: address.from_script(pool_policy),
    value: assets.from_lovelace(lovelace)
      |> assets.add(pool_policy, pool.da_bond_pool_asset_name, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn pool_input(datum: Data, lovelace: Int) -> Input {
  Input { output_reference: outref(4), output: pool_output(datum, lovelace) }
}

/// Funds a resume step's fee: a resume step spends no challenger reserve.
fn wallet_input() -> Input {
  Input {
    output_reference: outref(6),
    output: enterprise_output(submitter, 5_000_000),
  }
}

fn hub_reference_input() -> Input {
  let other = address.from_script(foreign_policy)
  let datum: hub.Datum = hub.Datum {
    registered_operators: foreign_policy,
    active_operators: foreign_policy,
    retired_operators: foreign_policy,
    scheduler: foreign_policy,
    state_queue: state_queue_policy,
    fraud_proof_catalogue: foreign_policy,
    fraud_proof: foreign_policy,
    deposit: foreign_policy,
    withdrawal: foreign_policy,
    tx_order: foreign_policy,
    settlement: foreign_policy,
    payout: foreign_policy,
    registered_operators_addr: other,
    active_operators_addr: other,
    retired_operators_addr: other,
    scheduler_addr: other,
    state_queue_addr: address.from_script(state_queue_script),
    fraud_proof_catalogue_addr: other,
    fraud_proof_addr: other,
    deposit_addr: other,
    withdrawal_addr: other,
    tx_order_addr: other,
    settlement_addr: other,
    reserve_addr: other,
    payout_addr: other,
    reserve_observer: foreign_policy,
  }
  Input {
    output_reference: outref(10),
    output: Output {
      address: address.from_script(hub_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(hub_policy, hub.asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

/// A deployment reference input carrying one role NFT and the yield script.
fn yield_reference_input(
  output_index: Int,
  role: ByteArray,
  script_hash: ByteArray,
) -> Input {
  Input {
    output_reference: outref(output_index),
    output: Output {
      address: address.from_script(foreign_policy),
      value: assets.from_lovelace(20_000_000)
        |> assets.add(reference_script_auth_policy, role, 1),
      datum: NoDatum,
      reference_script: Some(script_hash),
    },
  }
}

// ---------------------------------------------------------------------------
// The removal transaction
// ---------------------------------------------------------------------------

type Approach {
  Prune
  Head
}

/// Which availability redeemer, if any, the transaction carries.
type Arm {
  /// The honest first step: `TimeoutChallenge` with its timeout yield.
  TimeoutArm
  /// The same transaction with the availability redeemer swapped to
  /// `CloseChallenge` and the close yield authenticated in place of the
  /// timeout yield.
  LiteralCloseArm
  /// A Close substitution tuned to pass Close's own value rules: a close-sized
  /// fee, a terminal with no timed-out tranche, and the Close refund.
  OptimisedCloseArm
  /// A resume step: no record, no terminal, no availability redeemer.
  NoArm
}

fn slash_split(pool_lovelace: Int) -> (Int, Int, Int) {
  let params = parameters()
  let backing = pool.lovelace_backing(pool_lovelace, params)
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

fn lovelace_sum(values: List<assets.Value>) -> Int {
  list.foldl(values, 0, fn(value, total) { total + assets.lovelace_of(value) })
}

fn script_hash_of(input: Input) -> Option<ByteArray> {
  when input.output.address.payment_credential is {
    Script(hash) -> Some(hash)
    _ -> None
  }
}

/// One step of an unavailable-block removal, as the ledger would present it.
///
/// - `approach`: which state-queue removal runs (see the module header).
/// - `lock`: the correction lock's current datum; `Idle` for the first step,
///   `locked_on_target()` for a resume step.
/// - `arm`: which availability redeemer, if any, rides along.
/// - `pool`: the pool's datum and lovelace; `None` omits the pool, and the
///   Timeout redeemer's pool indices then name the correction lock.
/// - `challenger_fee`: `c`, the challenger's share of the fee.
fn removal_tx(
  approach: Approach,
  lock: correction_lock.Datum,
  arm: Arm,
  pool: Option<(Data, Int)>,
  challenger_fee: Int,
) -> Transaction {
  let with_record = arm != NoArm
  // -- Inputs, in output-reference order.
  let record_inputs = if with_record {
    [record_input(), terminal_input(arm != OptimisedCloseArm)]
  } else {
    []
  }
  let base = list.length(record_inputs)
  let (element_a, element_b) = when approach is {
    Prune ->
      (target_node_input(2, Some(descendant_hash())), descendant_node_input(3))
    Head -> (root_input(2), target_node_input(3, None))
  }
  let pool_inputs = when pool is {
    Some((datum, lovelace)) -> [pool_input(datum, lovelace)]
    None -> []
  }
  let wallet_inputs = if with_record {
    []
  } else {
    [wallet_input()]
  }
  let inputs = list.concat(
    record_inputs,
    list.concat(
      [element_a, element_b],
      list.concat(pool_inputs, [lock_input(lock), ..wallet_inputs]),
    ),
  )
  let target_input_index = when approach is {
    Prune -> base
    Head -> base + 1
  }
  let pool_input_index = base + 2
  let lock_input_index = base + 2 + list.length(pool_inputs)
  // -- The slash split and the fee.
  let (taken, fee_part, payout) = when pool is {
    Some((_, lovelace)) -> slash_split(lovelace)
    None -> (0, 0, 0)
  }
  let fee = when arm is {
    TimeoutArm | LiteralCloseArm -> fee_part + challenger_fee
    OptimisedCloseArm -> close_fee
    NoArm -> resume_fee
  }
  // -- Outputs.
  let challenger_outputs = when arm is {
    TimeoutArm | LiteralCloseArm ->
      [
        enterprise_output(
          challenger,
          remaining - challenger_fee + parameters().challenge_record_lovelace + payout,
        ),
      ]
    OptimisedCloseArm ->
      [
        enterprise_output(
          challenger,
          remaining - close_fee + parameters().challenge_record_lovelace,
        ),
      ]
    NoArm -> []
  }
  let pool_outputs = when pool is {
    Some((datum, lovelace)) -> [pool_output(datum, lovelace - taken)]
    None -> []
  }
  let pool_output_index = list.length(challenger_outputs)
  let anchor_output_index = list.length(challenger_outputs) + list.length(
    pool_outputs,
  )
  let lock_output_index = anchor_output_index + 1
  let next_lock = when approach is {
    Prune -> locked_on_target()
    Head -> correction_lock.Idle
  }
  let body_outputs = list.concat(
    challenger_outputs,
    list.concat(
      pool_outputs,
      [continue_element(element_a, None), lock_output(next_lock)],
    ),
  )
  // Whatever is left (the burned element's lovelace, a resume step's wallet
  // change, the taken bond under a Close substitution) goes to the submitter.
  let change = lovelace_sum(list.map(inputs, fn(input) { input.output.value })) - lovelace_sum(
    list.map(body_outputs, fn(output) { output.value }),
  ) - fee
  let outputs = list.concat(
    body_outputs,
    [enterprise_output(submitter, change)],
  )
  // -- Reference inputs.
  let arm_yield_inputs = when arm is {
    TimeoutArm ->
      [
        yield_reference_input(
          11,
          yielding.timeout_role,
          availability_timeout_yield,
        ),
      ]
    LiteralCloseArm | OptimisedCloseArm ->
      [yield_reference_input(11, yielding.close_role, availability_close_yield)]
    NoArm -> []
  }
  let state_queue_yield_index = 1 + list.length(arm_yield_inputs)
  let root_reference_inputs = when approach is {
    Prune -> [root_input(13)]
    Head -> []
  }
  let reference_inputs = list.concat(
    [hub_reference_input(), ..arm_yield_inputs],
    [
      yield_reference_input(
        12,
        state_queue_yield.unavailable_timeout_role,
        state_queue_unavailable_yield,
      ),
      ..root_reference_inputs
    ],
  )
  // -- Mint.
  let element_burn = assets.from_asset(
    state_queue_policy,
    element_token(element_b),
    -1,
  )
  let mint = if with_record {
    element_burn
      |> assets.add(availability_policy, challenge(), -1)
      |> assets.add(availability_policy, terminal_name(), -1)
  } else {
    element_burn
  }
  // -- Withdrawals.
  let arm_yield_hashes = when arm is {
    TimeoutArm -> [availability_timeout_yield]
    LiteralCloseArm | OptimisedCloseArm -> [availability_close_yield]
    NoArm -> []
  }
  let withdrawals = list.map(
    list.concat(arm_yield_hashes, [state_queue_unavailable_yield]),
    fn(hash) { Pair(Script(hash), 0) },
  )
  // -- Redeemers: Spend by input, then Mint by policy, then Withdraw.
  let spend_count = list.length(list.filter_map(inputs, script_hash_of))
  let state_queue_mint_redeemer_index = spend_count
  let availability_mint_redeemer_index = spend_count + 1
  let spend_redeemers = list.filter_map(
    inputs,
    fn(input) {
      when script_hash_of(input) is {
        None -> None
        Some(hash) -> {
          let redeemer_data: Data = if hash == availability_policy {
            let coordinate: Data = availability.Coordinate {
              mint_redeemer_index: availability_mint_redeemer_index,
            }
            coordinate
          } else if hash == state_queue_script {
            let mutation: Data = state_queue.LinkedListMutation
            mutation
          } else if hash == pool_policy {
            let slash: Data = pool.Slash {
              hub_oracle_ref_input_index: 0,
              state_queue_mint_redeemer_index,
              correction_lock_input_index: lock_input_index,
              output_index: pool_output_index,
            }
            slash
          } else if hash == correction_lock_script {
            let correct: Data = correction_lock.Correct {
              hub_oracle_ref_input_index: 0,
            }
            correct
          } else {
            fail
          }
          Some(Pair(Spend(input.output_reference), redeemer_data))
        }
      }
    },
  )
  let removal_approach = when approach is {
    Prune ->
      state_queue.PruneTimedOutBlockDescendant {
        confirmed_state_ref_input_index: state_queue_yield_index + 1,
        timed_out_node_input_outref: element_a.output_reference,
        timed_out_node_output_index: anchor_output_index,
      }
    Head ->
      state_queue.RemoveTimedOutHead {
        confirmed_state_input_outref: element_a.output_reference,
        confirmed_state_output_index: anchor_output_index,
      }
  }
  let state_queue_redeemer: Data = state_queue.RemoveUnavailableBlockAfterTimeout {
    yield_to_ref_input_index: state_queue_yield_index,
    unavailable_header_hash: target_hash(),
    challenge_asset_name: challenge(),
    removal_approach,
  }
  let (pool_redeemer_input_index, pool_redeemer_output_index) = when pool is {
    Some(_) -> (pool_input_index, pool_output_index)
    None -> (lock_input_index, lock_output_index)
  }
  let availability_mint_redeemers = when arm is {
    TimeoutArm -> {
      let timeout: Data = availability.TimeoutChallenge {
        yield_to_ref_input_index: 1,
        hub_oracle_ref_input_index: 0,
        record_input_index: 0,
        terminal_accumulator_input_index: 1,
        state_queue_mint_redeemer_index,
        pool_input_index: pool_redeemer_input_index,
        pool_output_index: pool_redeemer_output_index,
        challenger_refund_output_index: 0,
      }
      [Pair(Mint(availability_policy), timeout)]
    }
    LiteralCloseArm | OptimisedCloseArm -> {
      let close: Data = availability.CloseChallenge {
        yield_to_ref_input_index: 1,
        hub_oracle_ref_input_index: 0,
        record_input_index: 0,
        terminal_accumulator_input_index: 1,
        state_queue_input_index: target_input_index,
        state_queue_output_index: anchor_output_index,
        challenger_refund_output_index: 0,
      }
      [Pair(Mint(availability_policy), close)]
    }
    NoArm -> []
  }
  let yield_redeemer: Data = availability.Yield
  let state_queue_yield_redeemer: Data = state_queue.YieldStateQueueV1
  let withdraw_redeemers = list.concat(
    list.map(
      arm_yield_hashes,
      fn(hash) { Pair(Withdraw(Script(hash)), yield_redeemer) },
    ),
    [
      Pair(
        Withdraw(Script(state_queue_unavailable_yield)),
        state_queue_yield_redeemer,
      ),
    ],
  )
  let redeemers = list.concat(
    spend_redeemers,
    list.concat(
      [
        Pair(Mint(state_queue_policy), state_queue_redeemer),
        ..availability_mint_redeemers
      ],
      withdraw_redeemers,
    ),
  )
  Transaction {
    ..transaction.placeholder,
    inputs,
    reference_inputs,
    outputs,
    fee,
    mint,
    withdrawals,
    validity_range: between(response_deadline(), response_deadline() + 1_000),
    redeemers,
  }
}

fn first_step(
  approach: Approach,
  pool_datum: Data,
  pool_lovelace: Int,
  challenger_fee: Int,
) -> Transaction {
  removal_tx(
    approach,
    correction_lock.Idle,
    TimeoutArm,
    Some((pool_datum, pool_lovelace)),
    challenger_fee,
  )
}

/// Replaces the redeemer of `purpose`, keeping its position.
fn with_redeemer(
  tx: Transaction,
  purpose: ScriptPurpose,
  redeemer: Data,
) -> Transaction {
  Transaction {
    ..tx,
    redeemers: list.map(
      tx.redeemers,
      fn(entry) {
        if entry.1st == purpose {
          Pair(purpose, redeemer)
        } else {
          entry
        }
      },
    ),
  }
}

// ---------------------------------------------------------------------------
// The ledger's view: which script runs for which redeemer
// ---------------------------------------------------------------------------

/// Runs the real handler the ledger would run for one redeemer entry.
fn run_redeemer(tx: Transaction, entry: Pair<ScriptPurpose, Data>) -> Bool {
  let redeemer = entry.2nd
  when entry.1st is {
    Spend(own_ref) -> {
      expect Some(input) = transaction.find_input(tx.inputs, own_ref)
      expect InlineDatum(datum) = input.output.datum
      expect Script(hash) = input.output.address.payment_credential
      if hash == availability_policy {
        expect spend_redeemer: availability.SpendRedeemerV1 = redeemer
        availability_validator.availability_challenge.spend(
          hub_policy,
          reference_script_auth_policy,
          parameters(),
          Some(datum),
          spend_redeemer,
          own_ref,
          tx,
        )
      } else if hash == state_queue_script {
        expect spend_redeemer: state_queue.SpendRedeemer = redeemer
        state_queue_validator.spend.spend(
          state_queue_policy,
          foreign_policy,
          availability_policy,
          foreign_policy,
          Some(datum),
          spend_redeemer,
          own_ref,
          tx,
        )
      } else if hash == pool_policy {
        expect spend_redeemer: pool.SpendRedeemer = redeemer
        pool_validator.da_bond_pool.spend(
          outref(99),
          hub_policy,
          da_params_policy,
          parameters(),
          Some(datum),
          spend_redeemer,
          own_ref,
          tx,
        )
      } else if hash == correction_lock_script {
        expect lock_datum: correction_lock.Datum = datum
        expect spend_redeemer: correction_lock.Redeemer = redeemer
        correction_lock_validator.spend.spend(
          hub_policy,
          availability_policy,
          Some(lock_datum),
          spend_redeemer,
          own_ref,
          tx,
        )
      } else {
        fail @"no spending script at this credential"
      }
    }
    Mint(policy) ->
      if policy == state_queue_policy {
        expect mint_redeemer: state_queue.MintRedeemer = redeemer
        state_queue_validator.mint.mint(
          hub_policy,
          correction_lock_script,
          foreign_policy,
          address.from_script(foreign_policy),
          foreign_policy,
          foreign_policy,
          foreign_policy,
          foreign_policy,
          foreign_policy,
          availability_policy,
          reference_script_auth_policy,
          mint_redeemer,
          policy,
          tx,
        )
      } else if policy == availability_policy {
        expect mint_redeemer: availability.MintRedeemerV1 = redeemer
        availability_validator.availability_challenge.mint(
          hub_policy,
          reference_script_auth_policy,
          parameters(),
          mint_redeemer,
          policy,
          tx,
        )
      } else {
        fail @"no minting policy"
      }
    Withdraw(Script(hash)) ->
      if hash == availability_timeout_yield {
        expect yield_redeemer: availability.YieldRedeemerV1 = redeemer
        availability_yields.timeout.withdraw(
          availability_policy,
          hub_policy,
          pool_policy,
          parameters(),
          yield_redeemer,
          Script(hash),
          tx,
        )
      } else if hash == availability_close_yield {
        expect yield_redeemer: availability.YieldRedeemerV1 = redeemer
        availability_yields.close.withdraw(
          availability_policy,
          hub_policy,
          parameters(),
          yield_redeemer,
          Script(hash),
          tx,
        )
      } else if hash == state_queue_unavailable_yield {
        expect yield_redeemer: state_queue.YieldRedeemerV1 = redeemer
        state_queue_yields.remove_unavailable.withdraw(
          state_queue_policy,
          hub_policy,
          correction_lock_script,
          availability_policy,
          yield_redeemer,
          Script(hash),
          tx,
        )
      } else {
        fail @"no withdrawal script"
      }
    _ -> fail @"no script for this purpose"
  }
}

fn every_redeemer_accepts(tx: Transaction) -> Bool {
  list.all(tx.redeemers, fn(entry) { run_redeemer(tx, entry) })
}

/// Every script except the one for `purpose` accepts, and that script is
/// part of the transaction.
fn every_redeemer_accepts_except(
  tx: Transaction,
  purpose: ScriptPurpose,
) -> Bool {
  and {
    list.any(tx.redeemers, fn(entry) { entry.1st == purpose }),
    list.all(
      tx.redeemers,
      fn(entry) { entry.1st == purpose || run_redeemer(tx, entry) },
    ),
  }
}

fn run_purpose(tx: Transaction, purpose: ScriptPurpose) -> Bool {
  expect Some(entry) = list.find(
    tx.redeemers,
    fn(entry) { entry.1st == purpose },
  )
  run_redeemer(tx, entry)
}

/// The redeemers are exactly the ledger's script purposes, in the ledger's
/// order: every script input, then every minting policy, then every script
/// withdrawal, and nothing else (Conway refuses extraneous redeemers).
fn redeemers_cover_every_script(tx: Transaction) -> Bool {
  let spends = list.filter_map(
    tx.inputs,
    fn(input) {
      when script_hash_of(input) is {
        Some(_) -> Some(Spend(input.output_reference))
        None -> None
      }
    },
  )
  let mints = list.map(assets.policies(tx.mint), fn(policy) { Mint(policy) })
  let withdrawals = list.filter_map(
    tx.withdrawals,
    fn(entry) {
      when entry.1st is {
        Script(_) -> Some(Withdraw(entry.1st))
        _ -> None
      }
    },
  )
  list.map(tx.redeemers, fn(entry) { entry.1st }) == list.concat(
    spends,
    list.concat(mints, withdrawals),
  )
}

/// Inputs plus mint equal outputs plus fee.
fn value_is_conserved(tx: Transaction) -> Bool {
  let consumed = list.foldl(
    tx.inputs,
    tx.mint,
    fn(input, total) { assets.merge(total, input.output.value) },
  )
  let produced = list.foldl(
    tx.outputs,
    assets.from_lovelace(tx.fee),
    fn(output, total) { assets.merge(total, output.value) },
  )
  consumed == produced
}

/// The whole transaction is valid for every script the ledger runs, and it
/// runs `script_count` of them.
fn ledger_accepts(tx: Transaction, script_count: Int) -> Bool {
  and {
    list.length(tx.redeemers) == script_count,
    redeemers_cover_every_script(tx),
    value_is_conserved(tx),
    every_redeemer_accepts(tx),
  }
}

fn output_lovelace(tx: Transaction, index: Int) -> Int {
  expect Some(output) = list.at(tx.outputs, index)
  assets.lovelace_of(output.value)
}

fn output_at(tx: Transaction, index: Int) -> Output {
  expect Some(output) = list.at(tx.outputs, index)
  output
}

fn pool_purpose() -> ScriptPurpose {
  Spend(outref(4))
}

fn lock_output_datum(tx: Transaction, index: Int) -> correction_lock.Datum {
  correction_lock.decode_datum(output_at(tx, index))
}

/// Six spends (record, terminal, two elements, pool, lock), two mints, two
/// withdrawals.
const first_step_script_count = 10

// ---------------------------------------------------------------------------
// Accepted scenarios
// ---------------------------------------------------------------------------

/// The exact settlement of one first step: the challenger output, the pool
/// continuation, the fee, and the lock's next datum.
fn settles_exactly(
  tx: Transaction,
  approach: Approach,
  pool_datum: Data,
  challenger_lovelace: Int,
  pool_lovelace: Int,
  fee: Int,
) -> Bool {
  and {
    ledger_accepts(tx, first_step_script_count),
    output_at(tx, 0) == enterprise_output(challenger, challenger_lovelace),
    output_at(tx, 1) == pool_output(pool_datum, pool_lovelace),
    tx.fee == fee,
    lock_output_datum(tx, 3) == when approach is {
      Prune -> locked_on_target()
      Head -> correction_lock.Idle
    },
  }
}

/// Full pool: `taken = da_bond`, the penalty pays the whole fee (`c = 0`) and
/// the challenger receives the reserve, the record and `da_bond - penalty`.
test pool_timeout_e2e_full_pool_prune() {
  settles_exactly(
    first_step(Prune, bonded(), full_pool, 0),
    Prune,
    bonded(),
    17_527_000_000,
    5_000_000,
    2_500_000_000,
  )
}

test pool_timeout_e2e_full_pool_head() {
  settles_exactly(
    first_step(Head, bonded(), full_pool, 0),
    Head,
    bonded(),
    17_527_000_000,
    5_000_000,
    2_500_000_000,
  )
}

/// Partial pool over the penalty: all 6,000 ADA of backing is taken, the
/// penalty is burned and the rest is paid out; the challenger adds `c`.
test pool_timeout_e2e_partial_pool_over_penalty_prune() {
  settles_exactly(
    first_step(Prune, bonded(), partial_pool_over_penalty, 150_000),
    Prune,
    bonded(),
    13_526_850_000,
    5_000_000,
    2_500_150_000,
  )
}

test pool_timeout_e2e_partial_pool_over_penalty_head() {
  settles_exactly(
    first_step(Head, bonded(), partial_pool_over_penalty, 150_000),
    Head,
    bonded(),
    13_526_850_000,
    5_000_000,
    2_500_150_000,
  )
}

/// Partial pool under the penalty: the whole 1,000 ADA backing is burned as
/// fee, nothing is paid out.
test pool_timeout_e2e_partial_pool_under_penalty_prune() {
  settles_exactly(
    first_step(Prune, bonded(), partial_pool_under_penalty, 150_000),
    Prune,
    bonded(),
    10_026_850_000,
    5_000_000,
    1_000_150_000,
  )
}

test pool_timeout_e2e_partial_pool_under_penalty_head() {
  settles_exactly(
    first_step(Head, bonded(), partial_pool_under_penalty, 150_000),
    Head,
    bonded(),
    10_026_850_000,
    5_000_000,
    1_000_150_000,
  )
}

/// Empty pool: nothing is taken, the pool passes through unchanged and `c` is
/// the whole fee.
test pool_timeout_e2e_empty_pool_prune() {
  settles_exactly(
    first_step(Prune, bonded(), empty_pool, 200_000),
    Prune,
    bonded(),
    10_026_800_000,
    empty_pool,
    200_000,
  )
}

test pool_timeout_e2e_empty_pool_head() {
  settles_exactly(
    first_step(Head, bonded(), empty_pool, 200_000),
    Head,
    bonded(),
    10_026_800_000,
    empty_pool,
    200_000,
  )
}

/// A Withdrawing pool stays slashable and keeps its `unlock_at`.
test pool_timeout_e2e_withdrawing_pool_prune() {
  settles_exactly(
    first_step(Prune, withdrawing(), full_pool, 0),
    Prune,
    withdrawing(),
    17_527_000_000,
    5_000_000,
    2_500_000_000,
  )
}

test pool_timeout_e2e_withdrawing_pool_head() {
  settles_exactly(
    first_step(Head, withdrawing(), full_pool, 0),
    Head,
    withdrawing(),
    17_527_000_000,
    5_000_000,
    2_500_000_000,
  )
}

// ---------------------------------------------------------------------------
// Negative: the resume-step shape with a pool Slash
// ---------------------------------------------------------------------------

/// A resume step: the lock is already Locked on this challenge, nothing is
/// minted under the availability policy, and no availability redeemer rides
/// along. A pool spent here under Slash gives its `taken` to the submitter.
fn resume_step(approach: Approach, pool: Option<(Data, Int)>) -> Transaction {
  removal_tx(approach, locked_on_target(), NoArm, pool, 0)
}

/// Control: the honest resume step (no pool) is accepted by every script, for
/// both approaches: element spends, lock, state-queue mint and yield.
test pool_timeout_e2e_resume_step_without_pool_is_accepted() {
  and {
    ledger_accepts(resume_step(Prune, None), 5),
    ledger_accepts(resume_step(Head, None), 5),
  }
}

/// Control: with a pool Slash added, every other script still accepts.
test pool_timeout_e2e_resume_step_slash_other_scripts_accept() {
  and {
    every_redeemer_accepts_except(
      resume_step(Prune, Some((bonded(), full_pool))),
      pool_purpose(),
    ),
    every_redeemer_accepts_except(
      resume_step(Head, Some((bonded(), full_pool))),
      pool_purpose(),
    ),
    redeemers_cover_every_script(
      resume_step(Prune, Some((bonded(), full_pool))),
    ),
    value_is_conserved(resume_step(Prune, Some((bonded(), full_pool)))),
  }
}

/// The pool refuses: `decode_datum(lock) == Idle` fails on the Locked lock.
test pool_timeout_e2e_resume_step_prune_pool_refuses_slash() fail {
  run_purpose(resume_step(Prune, Some((bonded(), full_pool))), pool_purpose())
}

test pool_timeout_e2e_resume_step_head_pool_refuses_slash() fail {
  run_purpose(resume_step(Head, Some((bonded(), full_pool))), pool_purpose())
}

// ---------------------------------------------------------------------------
// Negative: the availability redeemer swapped to CloseChallenge
// ---------------------------------------------------------------------------

fn close_yield_purpose() -> ScriptPurpose {
  Withdraw(Script(availability_close_yield))
}

fn literal_close(approach: Approach) -> Transaction {
  removal_tx(
    approach,
    correction_lock.Idle,
    LiteralCloseArm,
    Some((bonded(), full_pool)),
    0,
  )
}

fn optimised_close(approach: Approach) -> Transaction {
  removal_tx(
    approach,
    correction_lock.Idle,
    OptimisedCloseArm,
    Some((bonded(), full_pool)),
    0,
  )
}

/// Control: with Close in place of Timeout (and the close yield authenticated
/// in place of the timeout yield), every script but the close yield accepts:
/// the availability mint (close role), both Coordinate spends (Close names
/// the record and terminal), the element spends, the pool Slash, the lock and
/// the state-queue mint and yield. The pool's safety here rests on the close
/// yield alone.
test pool_timeout_e2e_close_swap_other_scripts_accept() {
  and {
    every_redeemer_accepts_except(literal_close(Prune), close_yield_purpose()),
    every_redeemer_accepts_except(literal_close(Head), close_yield_purpose()),
    every_redeemer_accepts_except(optimised_close(Prune), close_yield_purpose()),
    every_redeemer_accepts_except(optimised_close(Head), close_yield_purpose()),
    redeemers_cover_every_script(literal_close(Prune)),
    value_is_conserved(literal_close(Prune)),
    redeemers_cover_every_script(optimised_close(Head)),
    value_is_conserved(optimised_close(Head)),
  }
}

/// The literal swap: the close yield refuses. Its first false conjunct is
/// `tx.fee <= max_close_fee_lovelace` (the penalty-sized fee).
test pool_timeout_e2e_close_swap_prune_close_yield_refuses() fail {
  run_purpose(literal_close(Prune), close_yield_purpose())
}

test pool_timeout_e2e_close_swap_head_close_yield_refuses() fail {
  run_purpose(literal_close(Head), close_yield_purpose())
}

/// The attacker-optimised swap passes Close's fee, terminal and refund rules;
/// the close yield still refuses at `list.length(tx.inputs) == 3`. Behind it
/// stand `list.length(tx.outputs) == 2` and the Challenged -> Published node
/// continuation, which the removal forbids (prune keeps the anchor's data;
/// head removal burns the node).
test pool_timeout_e2e_optimised_close_prune_close_yield_refuses() fail {
  run_purpose(optimised_close(Prune), close_yield_purpose())
}

test pool_timeout_e2e_optimised_close_head_close_yield_refuses() fail {
  run_purpose(optimised_close(Head), close_yield_purpose())
}

// ---------------------------------------------------------------------------
// Negative: a Timeout that omits the pool
// ---------------------------------------------------------------------------

fn timeout_yield_purpose() -> ScriptPurpose {
  Withdraw(Script(availability_timeout_yield))
}

fn without_pool(approach: Approach) -> Transaction {
  removal_tx(approach, correction_lock.Idle, TimeoutArm, None, 150_000)
}

/// Control: with no pool, every other script accepts, including the
/// state-queue removal and the lock, which do not look for the pool.
test pool_timeout_e2e_without_pool_other_scripts_accept() {
  and {
    every_redeemer_accepts_except(without_pool(Prune), timeout_yield_purpose()),
    every_redeemer_accepts_except(without_pool(Head), timeout_yield_purpose()),
    redeemers_cover_every_script(without_pool(Prune)),
    value_is_conserved(without_pool(Prune)),
  }
}

/// The timeout yield refuses in `get_authentic_pool_input`: the named input is
/// the correction lock, not at `Script(pool policy)`.
test pool_timeout_e2e_without_pool_prune_timeout_yield_refuses() fail {
  run_purpose(without_pool(Prune), timeout_yield_purpose())
}

test pool_timeout_e2e_without_pool_head_timeout_yield_refuses() fail {
  run_purpose(without_pool(Head), timeout_yield_purpose())
}

// ---------------------------------------------------------------------------
// Negative: another pool arm in place of Slash
// ---------------------------------------------------------------------------

fn with_top_up(approach: Approach) -> Transaction {
  let top_up: Data = pool.TopUp { output_index: 1 }
  with_redeemer(
    first_step(approach, bonded(), full_pool, 0),
    pool_purpose(),
    top_up,
  )
}

/// Control: TopUp changes nothing the other scripts read.
test pool_timeout_e2e_top_up_in_place_of_slash_other_scripts_accept() {
  and {
    every_redeemer_accepts_except(with_top_up(Prune), pool_purpose()),
    every_redeemer_accepts_except(with_top_up(Head), pool_purpose()),
  }
}

/// The pool refuses: TopUp needs the pool to grow by the minimum top-up, and
/// the timeout yield's exact `input - taken` value makes it shrink.
test pool_timeout_e2e_top_up_in_place_of_slash_pool_refuses() fail {
  run_purpose(with_top_up(Prune), pool_purpose())
}
