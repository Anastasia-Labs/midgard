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
use midgard/validation_dispute_v1
use midgard/validation_game_v1
use midgard/validation_trace_v1

const game_hash = #"01010101010101010101010101010101010101010101010101010101"

const boundary_hash =
  #"02020202020202020202020202020202020202020202020202020202"

const timeout_hash = #"03030303030303030303030303030303030303030303030303030303"

const thread_policy =
  #"05050505050505050505050505050505050505050505050505050505"

const thread_name = #"0606"

const fraud_prover = #"07070707070707070707070707070707070707070707070707070707"

const header_hash = #"08080808080808080808080808080808080808080808080808080808"

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

/// A four-state trace `a, b, c, d`.
fn descriptor4(
  a: ByteArray,
  b: ByteArray,
  c: ByteArray,
  d: ByteArray,
) -> validation_trace_v1.ValidationTraceDescriptorV1 {
  validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_branch_hash(
      pair_root(a, b),
      pair_root(c, d),
    ),
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

/// The operator's trace is `a, b, c, d`; the challenger's is `a, b, e, f`.
fn opened() -> validation_dispute_v1.ValidationDisputeV1 {
  validation_dispute_v1.open(
    descriptor4(h_a, h_b, h_c, h_d),
    descriptor4(h_a, h_b, h_e, h_f),
    h_a,
    opened_at,
  )
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

fn after_operator_first() -> validation_dispute_v1.ValidationDisputeV1 {
  validation_dispute_v1.reveal_operator_midpoint(
    opened(),
    operator_first_proof(),
    1_100,
    1_200,
  )
}

/// Both rounds played: the game is ready for its one step.
fn ready() -> validation_dispute_v1.ValidationDisputeV1 {
  let after_first = validation_dispute_v1.reveal_challenger_midpoint(
    after_operator_first(),
    challenger_first_proof(),
    1_300,
    1_400,
  )
  validation_dispute_v1.reveal_challenger_midpoint(
    validation_dispute_v1.reveal_operator_midpoint(
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

fn game(dispute: validation_dispute_v1.ValidationDisputeV1) -> Data {
  let state: Data = validation_game_v1.ValidationGameStateV1 {
    challenged_header_hash: header_hash,
    dispute,
  }
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
  from: validation_dispute_v1.ValidationDisputeV1,
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
  from: validation_dispute_v1.ValidationDisputeV1,
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
    Some(
      ct.StepDatum {
        fraud_prover,
        data: Some(
          validation_game_v1.ValidationGameStateV1 {
            challenged_header_hash: header_hash,
            dispute: from,
          },
        ),
      },
    ),
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
      validation_dispute_v1.reveal_challenger_midpoint(
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
      validation_dispute_v1.ValidationDisputeV1 {
        ..after_operator_first(),
        operator_clock_ms: after_operator_first().operator_clock_ms + 1,
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
  game(validation_dispute_v1.enter_resolution(ready(), 1_900, 2_000))
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
