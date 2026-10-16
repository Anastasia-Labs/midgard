//// The midpoint game, driven through the deployed validator: reveals are open
//// to anyone (the trace root pins every midpoint), EnterResolution is the
//// prover's alone, and each move accepts exactly one successor.

use aiken/interval
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use fraud_proofs/validation_trace/game_v1 as game_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/thread_census_fixture_v1 as census
use midgard/validation_case_thread_v1
use midgard/validation_case_v1.{CaseThreadV1}
use midgard/validation_dispute_v1
use midgard/validation_trace_v1

const game_hash = #"01010101010101010101010101010101010101010101010101010101"

const boundary_hash =
  #"02020202020202020202020202020202020202020202020202020202"

const timeout_hash = #"03030303030303030303030303030303030303030303030303030303"

const expiry_hash = #"04040404040404040404040404040404040404040404040404040404"

const thread_policy =
  #"05050505050505050505050505050505050505050505050505050505"

const thread_name = #"0606"

const fraud_prover = #"07070707070707070707070707070707070707070707070707070707"

const defender = #"08080808080808080808080808080808080808080808080808080808"

const third_party = #"09090909090909090909090909090909090909090909090909090909"

const thread_lovelace = 5_000_000

const fee = 400_000

const opened_at = 1_000

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const h_d = #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const h_e = #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const h_f = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn pair_root(a: ByteArray, b: ByteArray) -> ByteArray {
  validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(a),
    validation_trace_v1.trace_leaf_hash(b),
  )
}

fn root4(a: ByteArray, b: ByteArray, c: ByteArray, d: ByteArray) -> ByteArray {
  validation_trace_v1.trace_branch_hash(pair_root(a, b), pair_root(c, d))
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

/// The defender's trace is `a, b, c, d`; the challenger's is `a, b, e, f`.
fn opened() -> CaseThreadV1 {
  CaseThreadV1 {
    header: validation_case_v1.ThreadHeaderV1 {
      started_at: 0,
      defender_vkh: defender,
    },
    version: validation_case_v1.thread_version,
    case_id: h_a,
    challenger_root: root4(h_a, h_b, h_e, h_f),
    defender_root: root4(h_a, h_b, h_c, h_d),
    step_count: 3,
    low_index: 0,
    high_index: 3,
    agreed_low_hash: h_a,
    defender_high_hash: h_d,
    challenger_high_hash: h_f,
    round: 0,
    turn_started_at: opened_at,
    defender_clock_ms: validation_dispute_v1.dispute_side_clock_milliseconds,
    challenger_clock_ms: validation_dispute_v1.dispute_side_clock_milliseconds,
    turn: validation_dispute_v1.AwaitingOperator { midpoint: 1 },
  }
}

fn operator_first_proof() -> validation_trace_v1.ValidationTraceProof {
  proof4(1, h_b, h_a, pair_root(h_c, h_d))
}

fn challenger_first_proof() -> validation_trace_v1.ValidationTraceProof {
  proof4(1, h_b, h_a, pair_root(h_e, h_f))
}

fn operator_second_proof() -> validation_trace_v1.ValidationTraceProof {
  proof4(2, h_c, h_d, pair_root(h_a, h_b))
}

fn challenger_second_proof() -> validation_trace_v1.ValidationTraceProof {
  proof4(2, h_e, h_f, pair_root(h_a, h_b))
}

fn after_operator_first() -> CaseThreadV1 {
  validation_case_thread_v1.reveal_defender_midpoint(
    opened(),
    operator_first_proof(),
    1_100,
    1_200,
  )
}

/// Both rounds played: the game is ready for its one step.
fn ready() -> CaseThreadV1 {
  let after_first = validation_case_thread_v1.reveal_challenger_midpoint(
    after_operator_first(),
    challenger_first_proof(),
    1_300,
    1_400,
  )
  validation_case_thread_v1.reveal_challenger_midpoint(
    validation_case_thread_v1.reveal_defender_midpoint(
      after_first,
      operator_second_proof(),
      1_500,
      1_600,
    ),
    challenger_second_proof(),
    1_700,
    1_800,
  )
}

fn game(thread: CaseThreadV1) -> Data {
  let state: Data = thread
  state
}

fn own_out_ref() -> OutputReference {
  OutputReference { transaction_id: zero, output_index: 0 }
}

fn thread_output(script_hash: ByteArray, state: Data) -> Output {
  let datum: Data = ct.StepDatum { fraud_prover, data: Some(state) }
  Output {
    address: address.from_script(script_hash),
    value: assets.from_lovelace(thread_lovelace)
      |> assets.add(thread_policy, thread_name, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

/// The game at `from` moves to `to_hash`/`to_state` in a transaction valid in
/// `[lower, upper]` and signed by `signers`.
fn move(
  from: CaseThreadV1,
  action: game_validator.GameActionV1,
  lower: Int,
  upper: Int,
  signers: List<ByteArray>,
  to_hash: ByteArray,
  to_state: Data,
) -> Bool {
  shaped_move(
    census.honest,
    from,
    action,
    lower,
    upper,
    signers,
    to_hash,
    to_state,
  )
}

/// `move`, its transaction reshaped by `shape` (see `thread_census_fixture_v1`).
fn shaped_move(
  shape: fn(Transaction) -> Transaction,
  from: CaseThreadV1,
  action: game_validator.GameActionV1,
  lower: Int,
  upper: Int,
  signers: List<ByteArray>,
  to_hash: ByteArray,
  to_state: Data,
) -> Bool {
  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [
      Input {
        output_reference: own_out_ref(),
        output: thread_output(game_hash, game(from)),
      },
    ],
    outputs: [thread_output(to_hash, to_state)],
    fee,
    validity_range: interval.between(lower, upper),
    extra_signatories: signers,
  }
    |> shape
  game_validator.main.spend(
    boundary_hash,
    timeout_hash,
    thread_policy,
    expiry_hash,
    Some(ct.StepDatum { fraud_prover, data: Some(from) }),
    ct.Continue(action),
    own_out_ref(),
    tx,
  )
}

fn reveal_operator(proof: validation_trace_v1.ValidationTraceProof) {
  game_validator.RevealOperator { input_index: 0, output_index: 0, proof }
}

fn reveal_challenger(proof: validation_trace_v1.ValidationTraceProof) {
  game_validator.RevealChallenger { input_index: 0, output_index: 0, proof }
}

fn enter_resolution() {
  game_validator.EnterResolution { input_index: 0, output_index: 0 }
}

// ## Reveals are open to anyone

test a_third_party_reveal_operator_advances_the_game() {
  move(
    opened(),
    reveal_operator(operator_first_proof()),
    1_100,
    1_200,
    [],
    game_hash,
    game(after_operator_first()),
  )
}

test a_third_party_reveal_challenger_advances_the_game() {
  move(
    after_operator_first(),
    reveal_challenger(challenger_first_proof()),
    1_300,
    1_400,
    [third_party],
    game_hash,
    game(
      validation_case_thread_v1.reveal_challenger_midpoint(
        after_operator_first(),
        challenger_first_proof(),
        1_300,
        1_400,
      ),
    ),
  )
}

/// One pre-state, two successors: the reveal's own successor is accepted
/// (above); the same reveal claiming a different clock charge is refused.
test a_reveal_with_any_other_successor_is_refused() fail {
  move(
    opened(),
    reveal_operator(operator_first_proof()),
    1_100,
    1_200,
    [],
    game_hash,
    game(
      CaseThreadV1 {
        ..after_operator_first(),
        defender_clock_ms: after_operator_first().defender_clock_ms + 1,
      },
    ),
  )
}

test a_reveal_that_leaves_the_game_script_is_refused() fail {
  move(
    opened(),
    reveal_operator(operator_first_proof()),
    1_100,
    1_200,
    [],
    boundary_hash,
    game(after_operator_first()),
  )
}

test a_reveal_off_the_movers_trace_root_is_refused() fail {
  move(
    after_operator_first(),
    reveal_challenger(operator_first_proof()),
    1_300,
    1_400,
    [third_party],
    game_hash,
    game(after_operator_first()),
  )
}

// ## EnterResolution is the prover's

fn resolution_successor() -> Data {
  game(validation_case_thread_v1.enter_resolution(ready(), 1_900, 2_000))
}

test the_prover_enters_resolution() {
  move(
    ready(),
    enter_resolution(),
    1_900,
    2_000,
    [fraud_prover],
    boundary_hash,
    resolution_successor(),
  )
}

test a_third_party_cannot_enter_resolution() fail {
  move(
    ready(),
    enter_resolution(),
    1_900,
    2_000,
    [third_party],
    boundary_hash,
    resolution_successor(),
  )
}

// ## Census: who funds and who signs each move
//
// A lone thread input pays up to the cap and no more; beside another input the
// thread keeps its value; reveals and the challenger timeout are open to
// anyone, and EnterResolution needs the prover.

fn reveal_shaped(shape: fn(Transaction) -> Transaction) -> Bool {
  shaped_move(
    shape,
    opened(),
    reveal_operator(operator_first_proof()),
    1_100,
    1_200,
    [],
    game_hash,
    game(after_operator_first()),
  )
}

test census_a_reveal_pays_the_cap_from_the_thread() {
  reveal_shaped(census.lone_input_paying(census.cap))
}

test census_a_reveal_paying_over_the_cap_is_refused() fail {
  reveal_shaped(census.lone_input_paying(census.cap + 1))
}

test census_a_reveal_beside_a_wallet_input_keeps_the_thread_value() {
  reveal_shaped(census.beside_wallet_input(0))
}

test census_a_reveal_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  reveal_shaped(census.beside_wallet_input(1))
}

fn resolution_shaped(
  shape: fn(Transaction) -> Transaction,
  signers: List<ByteArray>,
) -> Bool {
  shaped_move(
    shape,
    ready(),
    enter_resolution(),
    1_900,
    2_000,
    signers,
    boundary_hash,
    resolution_successor(),
  )
}

test census_the_prover_enters_resolution_paying_the_cap_from_the_thread() {
  resolution_shaped(census.lone_input_paying(census.cap), [fraud_prover])
}

test census_entering_resolution_paying_over_the_cap_is_refused() fail {
  resolution_shaped(census.lone_input_paying(census.cap + 1), [fraud_prover])
}

test census_entering_resolution_beside_a_wallet_input_keeps_the_thread_value() {
  resolution_shaped(census.beside_wallet_input(0), [fraud_prover])
}

test census_entering_resolution_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  resolution_shaped(census.beside_wallet_input(1), [fraud_prover])
}

test census_entering_resolution_unsigned_is_refused() fail {
  resolution_shaped(census.lone_input_paying(census.cap), [])
}

/// The opened game awaits the operator; one millisecond past its clock the
/// challenger may take the thread to the timeout validator, unchanged.
fn challenger_timeout_shaped(
  shape: fn(Transaction) -> Transaction,
  lower: Int,
) -> Bool {
  shaped_move(
    shape,
    opened(),
    game_validator.EnterChallengerTimeout { input_index: 0, output_index: 0 },
    lower,
    lower + 1_000,
    [],
    timeout_hash,
    game(opened()),
  )
}

const operator_lapsed_at =
  opened_at + validation_dispute_v1.dispute_side_clock_milliseconds + 1

test census_anyone_enters_the_challenger_timeout_paying_the_cap() {
  challenger_timeout_shaped(
    census.lone_input_paying(census.cap),
    operator_lapsed_at,
  )
}

test census_the_challenger_timeout_before_the_operator_clock_lapses_is_refused() fail {
  challenger_timeout_shaped(
    census.lone_input_paying(census.cap),
    operator_lapsed_at - 1,
  )
}

test census_the_challenger_timeout_paying_over_the_cap_is_refused() fail {
  challenger_timeout_shaped(
    census.lone_input_paying(census.cap + 1),
    operator_lapsed_at,
  )
}

test census_the_challenger_timeout_beside_a_wallet_input_keeps_the_thread_value() {
  challenger_timeout_shaped(census.beside_wallet_input(0), operator_lapsed_at)
}

test census_the_challenger_timeout_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  challenger_timeout_shaped(census.beside_wallet_input(1), operator_lapsed_at)
}

// ## Timeouts on the challenger's turns go to the defender

/// The game after both rounds, at `ReadyForOneStep`: a timeout there is the
/// challenger's, and moves the thread to the timeout validator unchanged.
fn ready_timeout(lower: Int) -> Bool {
  shaped_move(
    census.honest,
    ready(),
    game_validator.EnterChallengerTimeout { input_index: 0, output_index: 0 },
    lower,
    lower + 1_000,
    [],
    timeout_hash,
    game(ready()),
  )
}

fn ready_lapsed_at() -> Int {
  ready().turn_started_at + ready().challenger_clock_ms + 1
}

test anyone_times_out_the_challenger_at_ready_for_one_step() {
  ready_timeout(ready_lapsed_at())
}

test a_ready_for_one_step_timeout_before_the_challenger_clock_lapses_is_refused() fail {
  ready_timeout(ready_lapsed_at() - 1)
}

test a_timed_out_thread_with_a_changed_state_is_refused() fail {
  shaped_move(
    census.honest,
    ready(),
    game_validator.EnterChallengerTimeout { input_index: 0, output_index: 0 },
    ready_lapsed_at(),
    ready_lapsed_at() + 1_000,
    [],
    timeout_hash,
    game(CaseThreadV1 { ..ready(), challenger_clock_ms: 0 }),
  )
}

test the_game_has_no_cancel() fail {
  game_validator.main.spend(
    boundary_hash,
    timeout_hash,
    thread_policy,
    expiry_hash,
    Some(ct.StepDatum { fraud_prover, data: Some(opened()) }),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    own_out_ref(),
    Transaction {
      ..transaction.placeholder,
      inputs: [
        Input {
          output_reference: own_out_ref(),
          output: thread_output(game_hash, game(opened())),
        },
      ],
      mint: assets.from_asset(thread_policy, thread_name, -1),
      extra_signatories: [fraud_prover],
    },
  )
}

// ## Every move keeps the thread header

fn with_header(
  thread: CaseThreadV1,
  header: validation_case_v1.ThreadHeaderV1,
) -> Data {
  game(CaseThreadV1 { ..thread, header })
}

fn other_header() -> validation_case_v1.ThreadHeaderV1 {
  validation_case_v1.ThreadHeaderV1 {
    started_at: opened().header.started_at + 1,
    defender_vkh: third_party,
  }
}

test a_reveal_operator_keeps_the_header() {
  move(
    opened(),
    reveal_operator(operator_first_proof()),
    1_100,
    1_200,
    [],
    game_hash,
    with_header(after_operator_first(), opened().header),
  )
}

test a_reveal_challenger_keeps_the_header() {
  move(
    after_operator_first(),
    reveal_challenger(challenger_first_proof()),
    1_300,
    1_400,
    [],
    game_hash,
    with_header(
      validation_case_thread_v1.reveal_challenger_midpoint(
        after_operator_first(),
        challenger_first_proof(),
        1_300,
        1_400,
      ),
      opened().header,
    ),
  )
}

test entering_resolution_keeps_the_header() {
  move(
    ready(),
    enter_resolution(),
    1_900,
    2_000,
    [fraud_prover],
    boundary_hash,
    with_header(
      validation_case_thread_v1.enter_resolution(ready(), 1_900, 2_000),
      opened().header,
    ),
  )
}

test entering_the_timeout_keeps_the_header() {
  move(
    ready(),
    game_validator.EnterChallengerTimeout { input_index: 0, output_index: 0 },
    ready_lapsed_at(),
    ready_lapsed_at() + 1_000,
    [],
    timeout_hash,
    with_header(ready(), opened().header),
  )
}

test a_reveal_changing_the_header_is_refused() fail {
  move(
    opened(),
    reveal_operator(operator_first_proof()),
    1_100,
    1_200,
    [],
    game_hash,
    with_header(after_operator_first(), other_header()),
  )
}

test entering_resolution_changing_the_header_is_refused() fail {
  move(
    ready(),
    enter_resolution(),
    1_900,
    2_000,
    [fraud_prover],
    boundary_hash,
    with_header(
      validation_case_thread_v1.enter_resolution(ready(), 1_900, 2_000),
      other_header(),
    ),
  )
}

test entering_the_timeout_changing_the_header_is_refused() fail {
  move(
    ready(),
    game_validator.EnterChallengerTimeout { input_index: 0, output_index: 0 },
    ready_lapsed_at(),
    ready_lapsed_at() + 1_000,
    [],
    timeout_hash,
    with_header(ready(), other_header()),
  )
}

// ## The game releases a thread to the expiry script

fn released(input: Output, withdrawals: Pairs<address.Credential, Int>) -> Bool {
  game_validator.main.spend(
    boundary_hash,
    timeout_hash,
    thread_policy,
    expiry_hash,
    Some(ct.StepDatum { fraud_prover, data: Some(opened()) }),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    own_out_ref(),
    Transaction {
      ..transaction.placeholder,
      inputs: [Input { output_reference: own_out_ref(), output: input }],
      withdrawals,
    },
  )
}

test the_game_releases_its_thread_to_the_expiry_withdrawal() {
  released(
    thread_output(game_hash, game(opened())),
    [Pair(address.Script(expiry_hash), 0)],
  )
}

test the_game_does_not_release_its_thread_without_the_expiry_withdrawal() fail {
  released(thread_output(game_hash, game(opened())), [])
}

test the_game_does_not_release_its_thread_to_another_withdrawal() fail {
  released(
    thread_output(game_hash, game(opened())),
    [Pair(address.Script(timeout_hash), 0)],
  )
}

test the_game_does_not_release_a_utxo_without_a_thread_token() fail {
  let thread = thread_output(game_hash, game(opened()))
  released(
    Output { ..thread, value: assets.from_lovelace(thread_lovelace) },
    [Pair(address.Script(expiry_hash), 0)],
  )
}
