//// Handler coverage for `validators/script-decode-v1.ak`.
////
//// The decoder's classes and roots are covered in
//// `lib/midgard/script-decode-v1.test.ak`. These tests drive the handlers
//// over whole transactions: an honest bind, walk and finish, and one `fail`
//// test per guard where the honest transaction is changed so that one check
//// flips. A Walk transaction runs the spend and the withdraw handler of each
//// class staker it withdraws from, as the ledger does.

use aiken/builtin
use aiken/collection/list
use aiken/collection/pairs
use aiken/primitive/bytearray
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Redeemer, ScriptPurpose,
  Spend, Transaction, Withdraw,
}
use midgard/cek_proof_v1
use midgard/script_decode_v1.{
  Control, DecodeEnv, Decoded, Frame, Machine, Refused, ScriptDecodeRecordV1,
  ScriptDecodeRunV1, decode_env_v1, empty_stack_root, initial_run_v1,
  is_bytes_control_v1, is_data_control_v1, is_fold_control_v1,
  is_list_control_v1, is_term_control_v1, is_terminal, is_value_control_v1,
  machine_hash_v1, push_stack_root, record_of_v1, step_bytes_v1, step_data_v1,
  step_fold_v1, step_list_v1, step_revealing_v1, step_term_v1, step_v1,
  step_value_v1,
}
use script_decode_v1.{
  Bind, Close, Finish, ScriptDecodeLinkV1, ScriptDecodeMintRedeemerV1,
  ScriptDecodeSegmentV1, ScriptDecodeSpendRedeemerV1,
  ScriptDecodeStakeRedeemerV1, Walk, run_token_name, script_hash_v1,
} as family

// ## Fixtures

const policy = #"5c7d0a1f2e3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f8091"

const stager = #"11111111111111111111111111111111111111111111111111111111"

/// The class stakers' hashes, main's parameter: term, value, list, fold,
/// bytes and Data, in that order.
const stakers =
  [
    #"a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0",
    #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1",
    #"a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2",
    #"a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3",
    #"a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4",
    #"a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5",
  ]

/// A withdraw-zero script outside the family; it accepts anything.
const outsider = #"b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0"

fn own_address() -> Address {
  Address { payment_credential: Script(policy), stake_credential: None }
}

/// A CBOR byte string around `flat`.
fn wrap(flat: ByteArray) -> ByteArray {
  let length = bytearray.length(flat)
  let head = if length < 24 {
    builtin.cons_bytearray(64 + length, #"")
  } else if length < 256 {
    #"58" |> bytearray.concat(builtin.cons_bytearray(length, #""))
  } else {
    #"59" |> bytearray.concat(builtin.integer_to_bytearray(True, 2, length))
  }
  bytearray.concat(head, flat)
}

/// `n` lambda-delay pairs, one byte each. `replicate_byte` stops at 8,192.
fn lambda_delays(n: Int) -> ByteArray {
  if n <= 8192 {
    builtin.replicate_byte(n, 0x21)
  } else {
    bytearray.concat(
      builtin.replicate_byte(8192, 0x21),
      lambda_delays(n - 8192),
    )
  }
}

/// `(program 1.1.0 (lam (delay ... (lam (delay (var 1))))))` with `n`
/// lambda-delay pairs.
fn nested_script(n: Int) -> ByteArray {
  wrap(
    #"010100"
      |> bytearray.concat(lambda_delays(n))
      |> bytearray.concat(#"0011"),
  )
}

fn nested_root(n: Int) -> ByteArray {
  if n == 0 {
    cek_proof_v1.hash_variable_term_v1(1)
  } else {
    cek_proof_v1.hash_lambda_term_v1(
      cek_proof_v1.hash_delay_term_v1(nested_root(n - 1)),
    )
  }
}

/// Splits `bytes` into chunks of `size`.
fn chunks_of(bytes: ByteArray, size: Int) -> List<ByteArray> {
  if bytearray.length(bytes) <= size {
    [bytes]
  } else {
    [
      bytearray.take(bytes, size),
      ..chunks_of(bytearray.drop(bytes, size), size)
    ]
  }
}

/// One staging output per run of up to three chunks, as one byte string.
fn staging_inputs(chunks: List<ByteArray>, index: Int) -> List<Input> {
  if chunks == [] {
    []
  } else {
    let datum = builtin.constr_data(
      0,
      [builtin.b_data(list.foldr(list.take(chunks, 3), #"", bytearray.concat))],
    )
    [
      Input {
        output_reference: OutputReference {
          transaction_id: #"5151515151515151515151515151515151515151515151515151515151515151",
          output_index: index,
        },
        output: Output {
          address: Address {
            payment_credential: VerificationKey(stager),
            stake_credential: None,
          },
          value: assets.from_lovelace(60_000_000),
          datum: InlineDatum(datum),
          reference_script: None,
        },
      },
      ..staging_inputs(list.drop(chunks, 3), index + 1)
    ]
  }
}

fn indices(inputs: List<Input>) -> List<Int> {
  list.indexed_map(inputs, fn(i, _) { i })
}

fn fee_input() -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"fefefefefefefefefefefefefefefefefefefefefefefefefefefefefefefefe",
      output_index: 0,
    },
    output: Output {
      address: Address {
        payment_credential: VerificationKey(stager),
        stake_credential: None,
      },
      value: assets.from_lovelace(100_000_000),
      datum: transaction.NoDatum,
      reference_script: None,
    },
  }
}

fn own_output(name: ByteArray, datum: Data) -> Output {
  Output {
    address: own_address(),
    value: assets.from_lovelace(5_000_000)
      |> assets.add(policy, name, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn run_ref() -> OutputReference {
  OutputReference {
    transaction_id: #"7272727272727272727272727272727272727272727272727272727272727272",
    output_index: 0,
  }
}

fn run_input(run: ScriptDecodeRunV1) -> Input {
  Input {
    output_reference: run_ref(),
    output: own_output(run_token_name(run.script_hash), run),
  }
}

fn mint(redeemer: ScriptDecodeMintRedeemerV1, tx: Transaction) -> Bool {
  family.script_decode_v1.mint(stakers, redeemer, policy, tx)
}

fn spend(
  redeemer: ScriptDecodeSpendRedeemerV1,
  input: Input,
  tx: Transaction,
) -> Bool {
  family.script_decode_v1.spend(
    stakers,
    None,
    redeemer,
    input.output_reference,
    tx,
  )
}

// ## Bind

fn bind_tx(script: ByteArray, chunk_size: Int) -> (Transaction, Int) {
  bind_tx_in(3, script, chunk_size)
}

/// An honest bind of `script` in `language`: its token and run datum are
/// those of `language`'s script hash.
fn bind_tx_in(
  language: Int,
  script: ByteArray,
  chunk_size: Int,
) -> (Transaction, Int) {
  let staged = staging_inputs(chunks_of(script, chunk_size), 0)
  let hash = script_hash_v1(language, script)
  let run: Data = initial_run_v1(hash, language, script)
  (
    Transaction {
      ..transaction.placeholder,
      inputs: [fee_input()],
      reference_inputs: staged,
      outputs: [own_output(run_token_name(hash), run)],
      mint: assets.from_asset(policy, run_token_name(hash), 1),
    },
    list.length(staged),
  )
}

fn bind_redeemer(staged: Int) -> ScriptDecodeMintRedeemerV1 {
  Bind { language: 3, staging: list.range(0, staged - 1), run_output: 0 }
}

test script_decode_family_bind_opens_a_run() {
  let (tx, staged) = bind_tx(nested_script(3), 2)
  mint(bind_redeemer(staged), tx)
}

/// Two scripts from the Aiken compiler, with the hashes its blueprint
/// reports for them.
const compiled_small = #"5001010023259800b452689b2b20025735"

const compiled_small_hash =
  #"22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab"

const compiled_medium =
  #"58c3010100229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264646644b30013370e900118039baa00189919914c005660026464660020026eacc03cc040c040c040c040c034dd51807804912cc004006007132325980099b9100d0018acc004cdc7806800c4dd59808001401500e44cc010010c04c00d00e1bae300e0013010001403c297adef6c608a518a5040314a14a28048c02c004c02cc030004c020dd5000c59006180400098041804800980400098021baa0088a4d13656400801"

const compiled_medium_hash =
  #"72f05b6a9f1d7ba25460687a553a0afc150f34df970cb884290bfd1c"

test script_decode_family_bind_hashes_like_cardano() {
  and {
    script_hash_v1(3, compiled_small) == compiled_small_hash,
    script_hash_v1(3, compiled_medium) == compiled_medium_hash,
  }
}

/// A 16,384-byte script: a three-byte CBOR head and 16,381 bytes of Flat.
fn max_script() -> ByteArray {
  nested_script(16_381 - 3 - 2)
}

test script_decode_family_bind_at_max_script_size() {
  let (tx, staged) = bind_tx(max_script(), 4095)
  and {
    bytearray.length(max_script()) == 16_384,
    staged == 2,
    mint(bind_redeemer(staged), tx),
  }
}

test script_decode_family_bind_opens_a_midgard_v1_run() {
  let (tx, staged) = bind_tx_in(128, nested_script(3), 2)
  mint(
    Bind { language: 128, staging: list.range(0, staged - 1), run_output: 0 },
    tx,
  )
}

test script_decode_family_bind_rejects_another_language() fail {
  let (tx, staged) = bind_tx_in(2, nested_script(3), 2)
  mint(
    Bind { language: 2, staging: list.range(0, staged - 1), run_output: 0 },
    tx,
  )
}

test script_decode_family_bind_rejects_a_different_token_name() fail {
  let (tx, staged) = bind_tx(nested_script(3), 2)
  let other = script_hash_v1(3, nested_script(4))
  let run: Data = initial_run_v1(other, 3, nested_script(3))
  mint(
    bind_redeemer(staged),
    Transaction {
      ..tx,
      outputs: [own_output(run_token_name(other), run)],
      mint: assets.from_asset(policy, run_token_name(other), 1),
    },
  )
}

test script_decode_family_bind_rejects_a_reordered_staging() fail {
  let (tx, staged) = bind_tx(nested_script(3), 2)
  expect staged == 2
  mint(Bind { language: 3, staging: [1, 0], run_output: 0 }, tx)
}

test script_decode_family_bind_rejects_two_tokens() fail {
  let (tx, staged) = bind_tx(nested_script(3), 2)
  let hash = script_hash_v1(3, nested_script(3))
  mint(
    bind_redeemer(staged),
    Transaction {
      ..tx,
      mint: assets.from_asset(policy, run_token_name(hash), 2),
      outputs: [
        own_output(
          run_token_name(hash),
          initial_run_v1(hash, 3, nested_script(3)),
        ),
        own_output(
          run_token_name(hash),
          initial_run_v1(hash, 3, nested_script(3)),
        ),
      ],
    },
  )
}

test script_decode_family_bind_rejects_a_progressed_datum() fail {
  let script = nested_script(3)
  let (tx, staged) = bind_tx(script, 2)
  let hash = script_hash_v1(3, script)
  let run = initial_run_v1(hash, 3, script)
  let machine = step_v1(
    decode_env_v1(script, run),
    run.machine,
    [],
    empty_stack_root(),
    3,
  )
  mint(
    bind_redeemer(staged),
    Transaction {
      ..tx,
      outputs: [
        own_output(run_token_name(hash), ScriptDecodeRunV1 { ..run, machine }),
      ],
    },
  )
}

test script_decode_family_bind_rejects_a_staked_run_address() fail {
  let (tx, staged) = bind_tx(nested_script(3), 2)
  expect [output] = tx.outputs
  mint(
    bind_redeemer(staged),
    Transaction {
      ..tx,
      outputs: [
        Output {
          ..output,
          address: Address {
            payment_credential: Script(policy),
            stake_credential: Some(address.Inline(VerificationKey(stager))),
          },
        },
      ],
    },
  )
}

test script_decode_family_bind_rejects_a_spent_run() fail {
  let script = nested_script(3)
  let (tx, staged) = bind_tx(script, 2)
  let hash = script_hash_v1(3, script)
  let run = initial_run_v1(hash, 3, script)
  mint(bind_redeemer(staged), Transaction { ..tx, inputs: [run_input(run)] })
}

// ## Segments
//
// A Walk's ticks are cut into segments, each a maximal run of ticks of one
// class, stepped by that class's staker.

/// The class of `control`: its staker's index in `stakers`, or -1 for a
/// terminal control.
fn class_of(control: Control) -> Int {
  if is_term_control_v1(control) {
    0
  } else if is_value_control_v1(control) {
    1
  } else if is_list_control_v1(control) {
    2
  } else if is_fold_control_v1(control) {
    3
  } else if is_bytes_control_v1(control) {
    4
  } else if is_data_control_v1(control) {
    5
  } else {
    -1
  }
}

fn staker_of(class: Int) -> ByteArray {
  expect Some(hash) = list.at(stakers, class)
  hash
}

fn step_class(
  class: Int,
  env: DecodeEnv,
  machine: Machine,
  reveals: List<Frame>,
  base: ByteArray,
  fuel: Int,
) -> Machine {
  when class is {
    0 -> step_term_v1(env, machine, reveals, base, fuel)
    1 -> step_value_v1(env, machine, reveals, base, fuel)
    2 -> step_list_v1(env, machine, reveals, base, fuel)
    3 -> step_fold_v1(env, machine, reveals, base, fuel)
    4 -> step_bytes_v1(env, machine, reveals, base, fuel)
    _ -> step_data_v1(env, machine, reveals, base, fuel)
  }
}

/// The withdraw handler of the staker of `class`.
fn withdraw(class: Int, redeemer: Data, tx: Transaction) -> Bool {
  let credential = Script(staker_of(class))
  when class is {
    0 -> family.script_decode_term_v1.withdraw(redeemer, credential, tx)
    1 -> family.script_decode_value_v1.withdraw(redeemer, credential, tx)
    2 -> family.script_decode_list_v1.withdraw(redeemer, credential, tx)
    3 -> family.script_decode_fold_v1.withdraw(redeemer, credential, tx)
    4 -> family.script_decode_bytes_v1.withdraw(redeemer, credential, tx)
    _ -> family.script_decode_data_v1.withdraw(redeemer, credential, tx)
  }
}

/// One segment of a Walk with its class, its position in the chain and the
/// hashes of the machines it starts from and reaches.
type Piece {
  class: Int,
  position: Int,
  segment: ScriptDecodeSegmentV1,
  from: ByteArray,
  to: ByteArray,
}

/// A stack with the root at each of its frames, top first.
type Stack =
  List<(Frame, ByteArray)>

fn root(stack: Stack) -> ByteArray {
  when stack is {
    [] -> empty_stack_root()
    [(_, at), ..] -> at
  }
}

/// `frames`, top first, pushed over `below`.
fn over(frames: List<Frame>, below: Stack) -> Stack {
  list.foldr(
    frames,
    below,
    fn(frame, stack) { [(frame, push_stack_root(frame, root(stack))), ..stack] },
  )
}

fn frames_of(stack: Stack) -> List<Frame> {
  list.map(stack, fn(entry) { entry.1st })
}

/// Up to `fuel` ticks from `machine` revealing the top `visible` frames of
/// `stack`, with the stack they leave.
fn step_stack(
  env: DecodeEnv,
  machine: Machine,
  stack: Stack,
  visible: Int,
  fuel: Int,
) -> (Machine, Stack) {
  let hidden = list.drop(stack, visible)
  let (reached, top) = step_revealing_v1(
    env,
    machine,
    frames_of(list.take(stack, visible)),
    root(hidden),
    fuel,
  )
  (reached, over(top, hidden))
}

/// One tick from `machine`, revealing the top frames of `stack`, or all of
/// them when the top ones are not enough.
fn tick(env: DecodeEnv, machine: Machine, stack: Stack) -> (Machine, Stack) {
  let (reached, after) = step_stack(env, machine, stack, 8, 1)
  if reached == machine {
    step_stack(env, machine, stack, list.length(stack), 1)
  } else {
    (reached, after)
  }
}

/// The ticks of `class` from `machine`, up to `ticks` of them: how many, and
/// the machine and stack they reach.
fn class_run(
  env: DecodeEnv,
  machine: Machine,
  stack: Stack,
  class: Int,
  ticks: Int,
  taken: Int,
) -> (Int, Machine, Stack) {
  if ticks <= 0 || class_of(machine.control) != class {
    (taken, machine, stack)
  } else {
    let (reached, after) = tick(env, machine, stack)
    class_run(env, reached, after, class, ticks - 1, taken + 1)
  }
}

/// The fewest top frames a segment of `class` must reveal to reach
/// `reached` from `machine` in `fuel` ticks, trying 0, 1, 2, 4, ...
fn reveal_count(
  env: DecodeEnv,
  class: Int,
  machine: Machine,
  stack: Stack,
  fuel: Int,
  reached: Machine,
  visible: Int,
) -> Int {
  let top = list.take(stack, visible)
  if list.length(top) < visible {
    list.length(top)
  } else if step_class(
    class,
    env,
    machine,
    frames_of(top),
    root(list.drop(stack, visible)),
    fuel,
  ) == reached {
    visible
  } else {
    reveal_count(
      env,
      class,
      machine,
      stack,
      fuel,
      reached,
      if visible == 0 {
        1
      } else {
        2 * visible
      },
    )
  }
}

/// The honest segments of a Walk of at most `ticks` ticks from `machine`
/// over `stack`, numbered from `position`. Each reveals only the frames it
/// pops. Returns them with the machine and stack the Walk reaches.
fn plan(
  env: DecodeEnv,
  machine: Machine,
  stack: Stack,
  ticks: Int,
  position: Int,
) -> (List<Piece>, Machine, Stack) {
  let class = class_of(machine.control)
  if ticks <= 0 || class < 0 {
    ([], machine, stack)
  } else {
    let (fuel, reached, after) = class_run(env, machine, stack, class, ticks, 0)
    let visible = reveal_count(env, class, machine, stack, fuel, reached, 0)
    let piece = Piece {
      class,
      position,
      segment: ScriptDecodeSegmentV1 {
        machine,
        reveals: frames_of(list.take(stack, visible)),
        base: root(list.drop(stack, visible)),
        fuel,
      },
      from: machine_hash_v1(machine),
      to: machine_hash_v1(reached),
    }
    let (rest, end, end_stack) = plan(
      env,
      reached,
      after,
      ticks - fuel,
      position + 1,
    )
    ([piece, ..rest], end, end_stack)
  }
}

/// The redeemer of a staker holding `pieces` for the run at `run`.
fn stake_redeemer(
  pieces: List<Piece>,
  run: OutputReference,
  staging: List<Int>,
) -> Data {
  let redeemer: Data = ScriptDecodeStakeRedeemerV1 {
    run,
    links: list.map(
      pieces,
      fn(piece) {
        ScriptDecodeLinkV1 {
          position: piece.position,
          from: piece.from,
          to: piece.to,
        }
      },
    ),
    staging,
    segments: list.map(pieces, fn(piece) { piece.segment }),
  }
  redeemer
}

/// One redeemer per class with pieces, under its staker's withdrawal.
fn stake_redeemers(
  pieces: List<Piece>,
  run: OutputReference,
  staging: List<Int>,
) -> Pairs<ScriptPurpose, Redeemer> {
  list.foldr(
    list.range(0, 5),
    [],
    fn(class, rest) {
      let own = list.filter(pieces, fn(piece) { piece.class == class })
      if own == [] {
        rest
      } else {
        [
          Pair(
            Withdraw(Script(staker_of(class))),
            stake_redeemer(own, run, staging),
          ),
          ..rest
        ]
      }
    },
  )
}

fn chunk_size(script: ByteArray) -> Int {
  if bytearray.length(script) > 4095 {
    4095
  } else {
    64
  }
}

/// A Walk of `run` to `reached` along `pieces`, withdrawing zero from each
/// staker with pieces.
fn walk_tx(
  script: ByteArray,
  run: ScriptDecodeRunV1,
  reached: Machine,
  pieces: List<Piece>,
) -> Transaction {
  let staged = staging_inputs(chunks_of(script, chunk_size(script)), 0)
  let staked = stake_redeemers(pieces, run_ref(), indices(staged))
  Transaction {
    ..transaction.placeholder,
    inputs: [run_input(run), fee_input()],
    reference_inputs: staged,
    outputs: [
      own_output(
        run_token_name(run.script_hash),
        ScriptDecodeRunV1 { ..run, machine: reached },
      ),
    ],
    withdrawals: list.map(
      staked,
      fn(pair) {
        expect Withdraw(credential) = pair.1st
        Pair(credential, 0)
      },
    ),
    redeemers: [Pair(Spend(run_ref()), walk_data()), ..staked],
  }
}

fn walk_data() -> Data {
  let walk: Data = Walk { run_output: 0 }
  walk
}

/// The spend of the Walk's run.
fn main(run: ScriptDecodeRunV1, tx: Transaction) -> Bool {
  spend(Walk { run_output: 0 }, run_input(run), tx)
}

/// Every family staker the transaction withdraws from, run on its redeemer.
/// Other withdrawals' scripts accept.
fn stakers_run(tx: Transaction) -> Bool {
  list.all(
    tx.withdrawals,
    fn(pair) {
      when pair.1st is {
        Script(hash) ->
          when list.index_of(stakers, hash) is {
            None -> True
            Some(class) -> staker_runs(class, tx)
          }
        _ -> True
      }
    },
  )
}

/// The staker of `class`, run on its redeemer in `tx`.
fn staker_runs(class: Int, tx: Transaction) -> Bool {
  expect Some(redeemer) = pairs.get_first(
    tx.redeemers,
    Withdraw(Script(staker_of(class))),
  )
  withdraw(class, redeemer, tx)
}

/// Everything the ledger runs for the Walk of `run`.
fn walk_runs(run: ScriptDecodeRunV1, tx: Transaction) -> Bool {
  main(run, tx) && stakers_run(tx)
}

fn opened(script: ByteArray) -> ScriptDecodeRunV1 {
  initial_run_v1(script_hash_v1(3, script), 3, script)
}

/// An honest Walk of `script` of at most `ticks` ticks, after `skip` ticks.
type Fixture {
  script: ByteArray,
  run: ScriptDecodeRunV1,
  stack: Stack,
  pieces: List<Piece>,
  reached: Machine,
}

fn fixture(script: ByteArray, skip: Int, ticks: Int) -> Fixture {
  let run = opened(script)
  let env = decode_env_v1(script, run)
  let (_, start, stack) = plan(env, run.machine, [], skip, 0)
  let (pieces, reached, _) = plan(env, start, stack, ticks, 0)
  Fixture {
    script,
    run: ScriptDecodeRunV1 { ..run, machine: start },
    stack,
    pieces,
    reached,
  }
}

fn tx_of(f: Fixture) -> Transaction {
  walk_tx(f.script, f.run, f.reached, f.pieces)
}

/// The same Walk along other pieces.
fn tx_along(f: Fixture, pieces: List<Piece>) -> Transaction {
  walk_tx(f.script, f.run, f.reached, pieces)
}

fn classes(pieces: List<Piece>) -> List<Int> {
  list.foldr(
    pieces,
    [],
    fn(piece, seen) {
      if list.has(seen, piece.class) {
        seen
      } else {
        [piece.class, ..seen]
      }
    },
  )
}

/// Walks `script` from its bind to its end in Walks of at most `ticks`
/// ticks, checking each Walk transaction as the ledger runs it. Returns the
/// finished run, the number of Walks, the most classes one Walk crossed and
/// whether a segment kept frames hidden.
fn walk_all(
  script: ByteArray,
  ticks: Int,
) -> (ScriptDecodeRunV1, Int, Int, Bool) {
  let run = opened(script)
  walk_from(script, decode_env_v1(script, run), run, [], ticks, 0, 0, False)
}

fn walk_from(
  script: ByteArray,
  env: DecodeEnv,
  run: ScriptDecodeRunV1,
  stack: Stack,
  ticks: Int,
  walks: Int,
  widest: Int,
  hidden: Bool,
) -> (ScriptDecodeRunV1, Int, Int, Bool) {
  if is_terminal(run.machine.control) {
    (run, walks, widest, hidden)
  } else {
    let (pieces, reached, after) = plan(env, run.machine, stack, ticks, 0)
    expect walk_runs(run, walk_tx(script, run, reached, pieces))
    let width = list.length(classes(pieces))
    walk_from(
      script,
      env,
      ScriptDecodeRunV1 { ..run, machine: reached },
      after,
      ticks,
      walks + 1,
      if width > widest {
        width
      } else {
        widest
      },
      hidden || list.any(
        pieces,
        fn(piece) { piece.segment.base != empty_stack_root() },
      ),
    )
  }
}

/// The record a single unbounded step reaches.
fn one_shot(script: ByteArray) -> ScriptDecodeRecordV1 {
  expect Some(record) = record_of_v1(finished(script))
  record
}

// ## Walk

test script_decode_family_walk_advances_a_run() {
  let f = fixture(nested_script(3), 0, 4)
  walk_runs(f.run, tx_of(f))
}

test script_decode_family_walk_reaches_the_decoded_record() {
  let script = nested_script(12)
  let (run, walks, _, _) = walk_all(script, 3)
  expect Some(record) = record_of_v1(run)
  and {
    walks > 5,
    record == Decoded {
      script_hash: script_hash_v1(3, script),
      language: 3,
      program_version: 1,
      term_root: nested_root(12),
      phase2_class: 0,
    },
  }
}

test script_decode_family_walk_decodes_compiled_scripts() {
  let decoded = fn(script) {
    let (run, _, _, _) = walk_all(script, 40)
    expect Some(Decoded { program_version, phase2_class, script_hash, .. }) = record_of_v1(
      run,
    )
    and {
      program_version == 1,
      phase2_class == 0,
      script_hash == script_hash_v1(3, script),
      record_of_v1(run) == Some(one_shot(script)),
    }
  }
  and {
    decoded(compiled_small),
    decoded(compiled_medium),
  }
}

/// `(program 1.1.0 (con data (List [I 0, ...])))` with `n` items, as an
/// indefinite CBOR list.
fn data_script(n: Int) -> ByteArray {
  wrap(
    #"0101004c01"
      |> bytearray.concat(
          flat_chunks(
            #"9f"
              |> bytearray.concat(builtin.replicate_byte(n, 0))
              |> bytearray.concat(#"ff"),
          ),
        )
      |> bytearray.concat(#"01"),
  )
}

/// A whole decode of a Data constant in one Walk: term, value, byte-string
/// and Data segments in one transaction.
fn crossing() -> Fixture {
  fixture(data_script(2), 0, 10_000)
}

test script_decode_family_walk_crosses_classes_in_one_transaction() {
  let f = crossing()
  expect Some(record) = record_of_v1(
    ScriptDecodeRunV1 { ..f.run, machine: f.reached },
  )
  and {
    list.length(classes(f.pieces)) >= 4,
    list.length(f.pieces) >= 5,
    record == one_shot(data_script(2)),
    walk_runs(f.run, tx_of(f)),
  }
}

test script_decode_family_walk_at_max_script_size() {
  let f = fixture(max_script(), 0, 60)
  walk_runs(f.run, tx_of(f))
}

/// Other bytes whose Flat prefix the Walk decodes are the same.
test script_decode_family_walk_main_does_not_read_the_staged_bytes() {
  let f = fixture(nested_script(3), 0, 4)
  main(
    f.run,
    Transaction {
      ..tx_of(f),
      reference_inputs: staging_inputs(chunks_of(nested_script(4), 64), 0),
    },
  )
}

test script_decode_family_walk_rejects_other_staged_bytes() fail {
  let f = fixture(nested_script(3), 0, 4)
  staker_runs(
    0,
    Transaction {
      ..tx_of(f),
      reference_inputs: staging_inputs(chunks_of(nested_script(4), 64), 0),
    },
  )
}

test script_decode_family_walk_rejects_no_progress() fail {
  let f = fixture(nested_script(3), 0, 4)
  main(f.run, walk_tx(f.script, f.run, f.run.machine, []))
}

test script_decode_family_walk_rejects_a_finished_run() fail {
  let run = finished(nested_script(3))
  main(run, walk_tx(nested_script(3), run, run.machine, []))
}

/// A Walk of `script`'s opened run in a transaction that also mints under
/// this policy runs both handlers, as the ledger does: the Walk's spend, and
/// the mint with the redeemer `mint_redeemer` builds from the staging.
/// `extra` is appended to the outputs.
fn walk_and_mint(
  script: ByteArray,
  minted: assets.Value,
  extra: List<Output>,
  mint_redeemer: fn(List<Int>) -> ScriptDecodeMintRedeemerV1,
) -> Bool {
  let f = fixture(script, 0, 4)
  let tx = tx_of(f)
  let tx = Transaction {
    ..tx,
    mint: minted,
    outputs: list.concat(tx.outputs, extra),
  }
  walk_runs(f.run, tx) && mint(mint_redeemer(indices(tx.reference_inputs)), tx)
}

/// A Walk beside a bind of its own script, with the bound run's output.
test script_decode_family_walk_rejects_a_bind_mint() fail {
  let script = nested_script(3)
  let hash = script_hash_v1(3, script)
  walk_and_mint(
    script,
    assets.from_asset(policy, run_token_name(hash), 1),
    [own_output(run_token_name(hash), initial_run_v1(hash, 3, script))],
    fn(staging) { Bind { language: 3, staging, run_output: 1 } },
  )
}

test script_decode_family_walk_rejects_a_finish_mint() fail {
  let script = nested_script(3)
  let hash = script_hash_v1(3, script)
  walk_and_mint(
    script,
    assets.from_asset(policy, run_token_name(hash), -1)
      |> assets.add(policy, hash, 1),
    [
      own_output(
        hash,
        Refused { script_hash: hash, language: 3, class: 3, bit_offset: 0 },
      ),
    ],
    fn(_) { Finish { record_output: 1 } },
  )
}

/// A finished run spent by a Walk beside its own Finish: the run's successor
/// at output 0 and its record at output 1.
fn finished_walk_tx(script: ByteArray) -> (Transaction, ScriptDecodeRunV1) {
  let run = finished(script)
  expect Some(record) = record_of_v1(run)
  let tx = finish_tx(run, record)
  (
    Transaction {
      ..tx,
      reference_inputs: staging_inputs(chunks_of(script, 64), 0),
      outputs: [own_output(run_token_name(run.script_hash), run), ..tx.outputs],
    },
    run,
  )
}

test script_decode_family_finish_holds_beside_a_walk_output() {
  let (tx, _) = finished_walk_tx(nested_script(3))
  mint(Finish { record_output: 1 }, tx)
}

test script_decode_family_walk_rejects_a_finished_run_with_its_finish() fail {
  let (tx, run) = finished_walk_tx(nested_script(3))
  mint(Finish { record_output: 1 }, tx) && main(run, tx)
}

/// A second run, of another script, at another out_ref.
fn other_run_input() -> Input {
  Input {
    ..run_input(opened(nested_script(4))),
    output_reference: OutputReference {
      transaction_id: #"7373737373737373737373737373737373737373737373737373737373737373",
      output_index: 0,
    },
  }
}

test script_decode_family_walk_rejects_two_runs() fail {
  let f = fixture(nested_script(3), 0, 4)
  main(
    f.run,
    Transaction {
      ..tx_of(f),
      inputs: [run_input(f.run), other_run_input(), fee_input()],
    },
  )
}

fn successor_with(f: Fixture, change: fn(Output) -> Output) -> Transaction {
  let tx = tx_of(f)
  expect [output] = tx.outputs
  Transaction { ..tx, outputs: [change(output)] }
}

test script_decode_family_walk_rejects_a_successor_elsewhere() fail {
  let f = fixture(nested_script(3), 0, 4)
  main(
    f.run,
    successor_with(
      f,
      fn(output) {
        Output {
          ..output,
          address: Address {
            payment_credential: VerificationKey(stager),
            stake_credential: None,
          },
        }
      },
    ),
  )
}

test script_decode_family_walk_rejects_a_staked_successor() fail {
  let f = fixture(nested_script(3), 0, 4)
  main(
    f.run,
    successor_with(
      f,
      fn(output) {
        Output {
          ..output,
          address: Address {
            payment_credential: Script(policy),
            stake_credential: Some(address.Inline(VerificationKey(stager))),
          },
        }
      },
    ),
  )
}

test script_decode_family_walk_rejects_a_successor_without_the_token() fail {
  let f = fixture(nested_script(3), 0, 4)
  main(
    f.run,
    successor_with(
      f,
      fn(output) { Output { ..output, value: assets.from_lovelace(5_000_000) } },
    ),
  )
}

test script_decode_family_walk_rejects_a_successor_with_another_token() fail {
  let f = fixture(nested_script(3), 0, 4)
  main(
    f.run,
    successor_with(
      f,
      fn(output) {
        Output { ..output, value: assets.add(output.value, policy, #"00", 1) }
      },
    ),
  )
}

test script_decode_family_walk_rejects_a_successor_with_a_reference_script() fail {
  let f = fixture(nested_script(3), 0, 4)
  main(
    f.run,
    successor_with(
      f,
      fn(output) { Output { ..output, reference_script: Some(policy) } },
    ),
  )
}

test script_decode_family_walk_rejects_a_successor_of_another_header() fail {
  let f = fixture(nested_script(3), 0, 4)
  main(
    f.run,
    successor_with(
      f,
      fn(output) {
        let next: Data = ScriptDecodeRunV1 {
          ..f.run,
          flat_length: f.run.flat_length - 1,
          machine: f.reached,
        }
        Output { ..output, datum: InlineDatum(next) }
      },
    ),
  )
}

test script_decode_family_walk_rejects_a_run_without_its_token() fail {
  let f = fixture(nested_script(3), 0, 4)
  let bare = Input {
    ..run_input(f.run),
    output: Output {
      ..run_input(f.run).output,
      value: assets.from_lovelace(5_000_000),
    },
  }
  spend(
    Walk { run_output: 0 },
    bare,
    Transaction { ..tx_of(f), inputs: [bare, fee_input()] },
  )
}

// ## Chained segments
//
// Each row changes the honest Walk that decodes a Data constant in one
// transaction, along term, value, byte-string and Data segments.

fn renumber(pieces: List<Piece>) -> List<Piece> {
  list.indexed_map(pieces, fn(i, piece) { Piece { ..piece, position: i } })
}

fn at(pieces: List<Piece>, index: Int) -> Piece {
  expect Some(piece) = list.at(pieces, index)
  piece
}

/// The index of the first piece whose class differs from the next one's.
fn first_boundary(pieces: List<Piece>, index: Int) -> Int {
  expect [piece, next, ..] = list.drop(pieces, index)
  if piece.class != next.class {
    index
  } else {
    first_boundary(pieces, index + 1)
  }
}

fn with_redeemers(
  tx: Transaction,
  change: fn(Pair<ScriptPurpose, Redeemer>) -> Pair<ScriptPurpose, Redeemer>,
) -> Transaction {
  Transaction { ..tx, redeemers: list.map(tx.redeemers, change) }
}

/// Mutant 1: links passed by a staker outside the family are not read. The
/// whole chain sits in one outside staker's redeemer, so main sees no link.
test script_decode_family_chain_rejects_an_outside_staker() fail {
  let f = crossing()
  let tx = tx_of(f)
  let outside = Pair(
    Withdraw(Script(outsider)),
    stake_redeemer(f.pieces, run_ref(), indices(tx.reference_inputs)),
  )
  main(
    f.run,
    Transaction {
      ..tx,
      withdrawals: [Pair(Script(outsider), 0)],
      redeemers: [Pair(Spend(run_ref()), walk_data()), outside],
    },
  )
}

/// Mutant 2 (and 16): main reads links from whichever family staker passes
/// them; a segment handed to another class's staker is refused by it.
test script_decode_family_chain_main_reads_links_of_any_family_staker() {
  let f = crossing()
  let k = first_boundary(f.pieces, 0) + 1
  let moved = list.indexed_map(
    f.pieces,
    fn(i, piece) {
      if i == k {
        Piece { ..piece, class: 0 }
      } else {
        piece
      }
    },
  )
  and {
    at(f.pieces, k).class != 0,
    main(f.run, tx_along(f, moved)),
  }
}

test script_decode_family_chain_rejects_a_segment_at_another_class_staker() fail {
  let f = crossing()
  let k = first_boundary(f.pieces, 0) + 1
  let moved = list.indexed_map(
    f.pieces,
    fn(i, piece) {
      if i == k {
        Piece { ..piece, class: 0 }
      } else {
        piece
      }
    },
  )
  staker_runs(0, tx_along(f, moved))
}

/// Mutant 16: a staker refuses an in-state of another class even when it
/// claims no progress.
fn foreign_no_op(f: Fixture) -> List<Piece> {
  let k = first_boundary(f.pieces, 0) + 1
  let next = at(f.pieces, k)
  let machine = next.segment.machine
  let no_op = Piece {
    class: 0,
    position: k,
    segment: ScriptDecodeSegmentV1 {
      machine,
      reveals: [],
      base: machine.stack_root,
      fuel: 1,
    },
    from: next.from,
    to: next.from,
  }
  renumber(
    list.concat(list.take(f.pieces, k), [no_op, ..list.drop(f.pieces, k)]),
  )
}

test script_decode_family_chain_main_accepts_a_no_op_link() {
  let f = crossing()
  main(f.run, tx_along(f, foreign_no_op(f)))
}

test script_decode_family_chain_rejects_an_in_state_of_another_class() fail {
  let f = crossing()
  staker_runs(0, tx_along(f, foreign_no_op(f)))
}

/// Mutant 3: the stakers' links name another input than the spent run.
test script_decode_family_chain_rejects_links_for_another_input() fail {
  let f = crossing()
  let staged = staging_inputs(chunks_of(f.script, 64), 0)
  let tx = tx_of(f)
  main(
    f.run,
    Transaction {
      ..tx,
      redeemers: [
        Pair(Spend(run_ref()), walk_data()),
        ..stake_redeemers(
          f.pieces,
          fee_input().output_reference,
          indices(staged),
        )
      ],
    },
  )
}

/// Mutant 4: links under another purpose than the staker's withdrawal.
test script_decode_family_chain_rejects_links_under_another_purpose() fail {
  let f = crossing()
  main(
    f.run,
    with_redeemers(
      tx_of(f),
      fn(pair) {
        when pair.1st is {
          Withdraw(Script(hash)) -> Pair(Mint(hash), pair.2nd)
          _ -> pair
        }
      },
    ),
  )
}

test script_decode_family_chain_rejects_links_under_a_key_withdrawal() fail {
  let f = crossing()
  main(
    f.run,
    with_redeemers(
      tx_of(f),
      fn(pair) {
        when pair.1st is {
          Withdraw(Script(hash)) ->
            Pair(Withdraw(VerificationKey(hash)), pair.2nd)
          _ -> pair
        }
      },
    ),
  )
}

/// Mutant 5: a forged `to` that the successor matches. Main accepts the
/// chain; the staker that passed it refuses.
fn forged_end(f: Fixture) -> (Machine, List<Piece>) {
  let forged = Machine { ..f.reached, binders: f.reached.binders + 1 }
  let last = list.length(f.pieces) - 1
  (
    forged,
    list.indexed_map(
      f.pieces,
      fn(i, piece) {
        if i == last {
          Piece { ..piece, to: machine_hash_v1(forged) }
        } else {
          piece
        }
      },
    ),
  )
}

test script_decode_family_chain_main_accepts_a_forged_end() {
  let f = crossing()
  let (forged, pieces) = forged_end(f)
  main(f.run, walk_tx(f.script, f.run, forged, pieces))
}

test script_decode_family_chain_rejects_a_forged_end() fail {
  let f = crossing()
  let (forged, pieces) = forged_end(f)
  let last = at(pieces, list.length(pieces) - 1)
  staker_runs(last.class, walk_tx(f.script, f.run, forged, pieces))
}

/// Mutant 5: a forged `from` on an honest in-state.
test script_decode_family_chain_rejects_a_from_of_another_machine() fail {
  let f = crossing()
  let k = first_boundary(f.pieces, 0) + 1
  let tampered = list.indexed_map(
    f.pieces,
    fn(i, piece) {
      if i == k {
        Piece { ..piece, from: machine_hash_v1(f.run.machine) }
      } else {
        piece
      }
    },
  )
  staker_runs(at(f.pieces, k).class, tx_along(f, tampered))
}

/// Mutant 9: one segment across a class boundary.
fn merged(f: Fixture) -> (Int, List<Piece>) {
  let k = first_boundary(f.pieces, 0)
  let first = at(f.pieces, k)
  let second = at(f.pieces, k + 1)
  let joined = Piece {
    ..first,
    segment: ScriptDecodeSegmentV1 {
      ..first.segment,
      fuel: first.segment.fuel + second.segment.fuel,
    },
    to: second.to,
  }
  (
    first.class,
    renumber(
      list.concat(
        list.take(f.pieces, k),
        [joined, ..list.drop(f.pieces, k + 2)],
      ),
    ),
  )
}

test script_decode_family_chain_main_accepts_a_merged_segment() {
  let f = crossing()
  let (_, pieces) = merged(f)
  main(f.run, tx_along(f, pieces))
}

test script_decode_family_chain_rejects_a_step_outside_the_class() fail {
  let f = crossing()
  let (class, pieces) = merged(f)
  staker_runs(class, tx_along(f, pieces))
}

/// Mutant 11: a segment left out, the others keeping their positions.
test script_decode_family_chain_rejects_a_gap() fail {
  let f = crossing()
  let pieces = list.concat(list.take(f.pieces, 2), list.drop(f.pieces, 3))
  main(f.run, tx_along(f, pieces))
}

/// Mutant 12: two segments swapped.
test script_decode_family_chain_rejects_swapped_segments() fail {
  let f = crossing()
  let pieces = list.indexed_map(
    f.pieces,
    fn(i, piece) {
      if i == 1 {
        Piece { ..piece, position: 2 }
      } else if i == 2 {
        Piece { ..piece, position: 1 }
      } else {
        piece
      }
    },
  )
  main(f.run, tx_along(f, pieces))
}

/// Mutant 13: a segment left out and the rest renumbered. Every staker
/// accepts its own segments; the link that no longer meets is refused.
fn broken(f: Fixture) -> List<Piece> {
  renumber(list.concat(list.take(f.pieces, 2), list.drop(f.pieces, 3)))
}

test script_decode_family_chain_stakers_accept_a_broken_chain() {
  let f = crossing()
  stakers_run(tx_along(f, broken(f)))
}

test script_decode_family_chain_rejects_a_broken_link() fail {
  let f = crossing()
  main(f.run, tx_along(f, broken(f)))
}

/// Mutant 14: an honest chain from another state than the spent run's.
fn unanchored() -> (ScriptDecodeRunV1, Fixture) {
  let f = fixture(data_script(2), 1, 10_000)
  (opened(data_script(2)), f)
}

test script_decode_family_chain_stakers_accept_an_unanchored_chain() {
  let (run, f) = unanchored()
  stakers_run(walk_tx(f.script, run, f.reached, f.pieces))
}

test script_decode_family_chain_rejects_a_chain_from_another_state() fail {
  let (run, f) = unanchored()
  main(run, walk_tx(f.script, run, f.reached, f.pieces))
}

/// Mutant 15: a successor other than the chain's end.
test script_decode_family_chain_rejects_a_successor_off_the_chain_end() fail {
  let f = crossing()
  let last = at(f.pieces, list.length(f.pieces) - 1)
  main(f.run, walk_tx(f.script, f.run, last.segment.machine, f.pieces))
}

test script_decode_family_chain_rejects_a_successor_after_fewer_ticks() fail {
  let f = fixture(nested_script(3), 0, 4)
  let g = fixture(nested_script(3), 0, 3)
  main(f.run, walk_tx(f.script, f.run, g.reached, f.pieces))
}

/// Mutant 17: a staker's links for one run cannot advance another.
test script_decode_family_chain_rejects_links_of_another_run() fail {
  let f = crossing()
  let other = other_run_input()
  let tx = tx_of(f)
  let staged = indices(tx.reference_inputs)
  main(
    f.run,
    Transaction {
      ..tx,
      inputs: [run_input(f.run), other, fee_input()],
      redeemers: [
        Pair(Spend(run_ref()), walk_data()),
        ..stake_redeemers(f.pieces, other.output_reference, staged)
      ],
    },
  )
}

/// Mutant 18: a staker whose segment lies past the chain's end.
test script_decode_family_chain_rejects_a_link_left_over() fail {
  let f = fixture(nested_script(3), 0, 4)
  let extra = Piece {
    class: 1,
    position: list.length(f.pieces) + 1,
    segment: at(f.pieces, 0).segment,
    from: machine_hash_v1(f.reached),
    to: machine_hash_v1(f.reached),
  }
  main(f.run, tx_along(f, list.concat(f.pieces, [extra])))
}

test script_decode_family_chain_rejects_a_link_after_the_end() fail {
  let f = fixture(nested_script(3), 0, 4)
  let extra = Piece {
    class: 1,
    position: list.length(f.pieces),
    segment: at(f.pieces, 0).segment,
    from: machine_hash_v1(f.reached),
    to: machine_hash_v1(Machine { ..f.reached, binders: f.reached.binders + 1 }),
  }
  main(f.run, tx_along(f, list.concat(f.pieces, [extra])))
}

/// A forged in-state. At the head of the chain main refuses it, since its
/// hash is not the spent run's.
fn forged_at(f: Fixture, k: Int) -> List<Piece> {
  let forged = forged_at_head(f.pieces, k)
  let hash = at(forged, k).from
  list.indexed_map(
    forged,
    fn(i, piece) {
      if i == k - 1 {
        Piece { ..piece, to: hash }
      } else {
        piece
      }
    },
  )
}

fn forged_at_head(pieces: List<Piece>, k: Int) -> List<Piece> {
  list.indexed_map(
    pieces,
    fn(i, piece) {
      if i == k {
        let machine = Machine {
          ..piece.segment.machine,
          binders: piece.segment.machine.binders + 1,
        }
        Piece {
          ..piece,
          segment: ScriptDecodeSegmentV1 { ..piece.segment, machine },
          from: machine_hash_v1(machine),
        }
      } else {
        piece
      }
    },
  )
}

test script_decode_family_chain_rejects_a_forged_first_in_state() fail {
  let f = crossing()
  main(f.run, tx_along(f, forged_at_head(f.pieces, 0)))
}

/// Inside the chain, a forged in-state that the links meet is refused by
/// the staker whose segment should have reached it.
test script_decode_family_chain_main_accepts_a_linked_forged_in_state() {
  let f = crossing()
  main(f.run, tx_along(f, forged_at(f, 2)))
}

test script_decode_family_chain_rejects_a_linked_forged_in_state() fail {
  let f = crossing()
  staker_runs(at(f.pieces, 1).class, tx_along(f, forged_at(f, 2)))
}

test script_decode_family_chain_rejects_a_forged_in_state_under_its_hash() fail {
  let f = crossing()
  let pieces = list.indexed_map(
    forged_at_head(f.pieces, 2),
    fn(i, piece) {
      if i == 2 {
        Piece { ..piece, from: at(f.pieces, 2).from }
      } else {
        piece
      }
    },
  )
  staker_runs(at(f.pieces, 2).class, tx_along(f, pieces))
}

/// Revealed frames are checked against the in-state's stack root.
test script_decode_family_chain_rejects_a_wrong_reveal() fail {
  let f = crossing()
  let pieces = list.indexed_map(
    f.pieces,
    fn(i, piece) {
      if i == 0 {
        Piece {
          ..piece,
          segment: ScriptDecodeSegmentV1 {
            ..piece.segment,
            reveals: [script_decode_v1.FDelay],
          },
        }
      } else {
        piece
      }
    },
  )
  staker_runs(at(f.pieces, 0).class, tx_along(f, pieces))
}

/// A redeemer of another shape is refused by the staker and by main.
fn malformed(f: Fixture, redeemer: Data) -> Transaction {
  with_redeemers(
    tx_of(f),
    fn(pair) {
      when pair.1st is {
        Withdraw(_) -> Pair(pair.1st, redeemer)
        _ -> pair
      }
    },
  )
}

test script_decode_family_chain_staker_rejects_a_malformed_redeemer() fail {
  let f = fixture(nested_script(3), 0, 4)
  staker_runs(0, malformed(f, builtin.i_data(0)))
}

test script_decode_family_chain_main_rejects_malformed_links() fail {
  let f = fixture(nested_script(3), 0, 4)
  let run: Data = run_ref()
  main(f.run, malformed(f, builtin.constr_data(0, [run, builtin.i_data(0)])))
}

// ## Walks over several transactions

/// `pattern` repeated `n` times.
fn repeat_bytes(pattern: ByteArray, n: Int) -> ByteArray {
  if n == 0 {
    #""
  } else if n % 2 == 0 {
    let half = repeat_bytes(pattern, n / 2)
    bytearray.concat(half, half)
  } else {
    bytearray.concat(pattern, repeat_bytes(pattern, n - 1))
  }
}

/// `(program 1.1.0 (constr 0 [(builtin 1), ...]))` with `n` fields, `n` even
/// and at least 2. Each field is 12 bits, so two fields are three bytes.
fn dense_script(n: Int) -> ByteArray {
  wrap(
    #"010100800b81b8"
      |> bytearray.concat(repeat_bytes(#"1b81b8", n / 2 - 1))
      |> bytearray.concat(#"11"),
  )
}

/// `bytes` as Flat byte-string chunks of up to 255 bytes, then the
/// terminating empty chunk.
fn flat_chunks(bytes: ByteArray) -> ByteArray {
  let length = bytearray.length(bytes)
  if length == 0 {
    #"00"
  } else if length <= 255 {
    bytearray.push(bytes, length) |> bytearray.concat(#"00")
  } else {
    bytearray.push(bytearray.take(bytes, 255), 255)
      |> bytearray.concat(flat_chunks(bytearray.drop(bytes, 255)))
  }
}

test script_decode_family_walk_dense_shape_over_several_txs() {
  let script = dense_script(1024)
  let (run, walks, _, _) = walk_all(script, 80)
  expect Some(record) = record_of_v1(run)
  expect Decoded { phase2_class: 0, program_version: 1, .. } = record
  and {
    walks > 10,
    record == one_shot(script),
  }
}

test script_decode_family_walk_dense_refusal_over_several_txs() {
  let script = dense_script(1026)
  let (run, walks, _, _) = walk_all(script, 80)
  expect Some(record) = record_of_v1(run)
  expect Refused { class: 9, .. } = record
  and {
    walks > 10,
    record == one_shot(script),
  }
}

test script_decode_family_walk_deep_shape_over_several_txs() {
  let script = nested_script(300)
  let (run, walks, _, hidden) = walk_all(script, 80)
  expect Some(record) = record_of_v1(run)
  and {
    walks > 5,
    hidden,
    record == Decoded {
      script_hash: script_hash_v1(3, script),
      language: 3,
      program_version: 1,
      term_root: nested_root(300),
      phase2_class: 0,
    },
  }
}

test script_decode_family_walk_data_shape_over_several_txs() {
  let script = data_script(600)
  let (run, walks, widest, _) = walk_all(script, 80)
  expect Some(record) = record_of_v1(run)
  expect Decoded { phase2_class: 0, .. } = record
  and {
    walks > 5,
    widest >= 2,
    record == one_shot(script),
  }
}

/// A Walk deep inside the nested shape, whose segments keep the stack below
/// their reveals hidden.
fn deep_mid_walk() -> Fixture {
  fixture(nested_script(40), 40, 6)
}

test script_decode_family_walk_mid_walk_step_is_accepted() {
  let f = deep_mid_walk()
  and {
    list.any(f.pieces, fn(piece) { piece.segment.base != empty_stack_root() }),
    walk_runs(f.run, tx_of(f)),
  }
}

test script_decode_family_walk_mid_walk_rejects_a_hidden_stack_dropped() fail {
  let f = deep_mid_walk()
  let pieces = list.map(
    f.pieces,
    fn(piece) {
      Piece {
        ..piece,
        segment: ScriptDecodeSegmentV1 {
          ..piece.segment,
          base: empty_stack_root(),
        },
      }
    },
  )
  stakers_run(tx_along(f, pieces))
}

// ## Finish

fn finish_tx(
  run: ScriptDecodeRunV1,
  record: ScriptDecodeRecordV1,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [run_input(run), fee_input()],
    outputs: [own_output(run.script_hash, record)],
    mint: assets.from_asset(policy, run_token_name(run.script_hash), -1)
      |> assets.add(policy, run.script_hash, 1),
  }
}

fn finished(script: ByteArray) -> ScriptDecodeRunV1 {
  let run = opened(script)
  let machine = step_v1(
    decode_env_v1(script, run),
    run.machine,
    [],
    empty_stack_root(),
    100_000,
  )
  ScriptDecodeRunV1 { ..run, machine }
}

test script_decode_family_finish_records_a_decoded_script() {
  let run = finished(nested_script(3))
  expect Some(record) = record_of_v1(run)
  let tx = finish_tx(run, record)
  and {
    mint(Finish { record_output: 0 }, tx),
    spend(Close, run_input(run), tx),
  }
}

test script_decode_family_finish_records_a_refusal_at_bind() {
  // A CBOR unsigned integer, not a byte string.
  let run = opened(#"00")
  let tx = finish_tx(
    run,
    Refused {
      script_hash: script_hash_v1(3, #"00"),
      language: 3,
      class: 1,
      bit_offset: 0,
    },
  )
  and {
    mint(Finish { record_output: 0 }, tx),
    spend(Close, run_input(run), tx),
  }
}

test script_decode_family_finish_rejects_an_unfinished_run() fail {
  let run = opened(nested_script(3))
  mint(
    Finish { record_output: 0 },
    finish_tx(
      run,
      Refused {
        script_hash: run.script_hash,
        language: 3,
        class: 3,
        bit_offset: 0,
      },
    ),
  )
}

test script_decode_family_finish_rejects_another_verdict() fail {
  let run = finished(nested_script(3))
  mint(
    Finish { record_output: 0 },
    finish_tx(
      run,
      Refused {
        script_hash: run.script_hash,
        language: 3,
        class: 3,
        bit_offset: 0,
      },
    ),
  )
}

test script_decode_family_finish_rejects_a_kept_run_token() fail {
  let run = finished(nested_script(3))
  expect Some(record) = record_of_v1(run)
  mint(
    Finish { record_output: 0 },
    Transaction {
      ..finish_tx(run, record),
      mint: assets.from_asset(policy, run.script_hash, 1),
    },
  )
}

test script_decode_family_finish_rejects_a_record_with_extra_tokens() fail {
  let run = finished(nested_script(3))
  expect Some(record) = record_of_v1(run)
  let tx = finish_tx(run, record)
  expect [output] = tx.outputs
  // The extra token arrives from an input, so the mint itself is unchanged.
  let carrier = Input {
    ..fee_input(),
    output: Output {
      ..fee_input().output,
      value: assets.add(fee_input().output.value, policy, #"00", 1),
    },
  }
  mint(
    Finish { record_output: 0 },
    Transaction {
      ..tx,
      inputs: [run_input(run), carrier],
      outputs: [
        Output { ..output, value: assets.add(output.value, policy, #"00", 1) },
      ],
    },
  )
}

test script_decode_family_close_rejects_a_spend_without_the_burn() fail {
  let run = finished(nested_script(3))
  spend(
    Close,
    run_input(run),
    Transaction {
      ..transaction.placeholder,
      inputs: [run_input(run), fee_input()],
      outputs: [own_output(run_token_name(run.script_hash), run)],
    },
  )
}
