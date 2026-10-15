//// Handler coverage for `validators/script-decode-v1.ak`.
////
//// The decoder's classes and roots are covered in
//// `lib/midgard/script-decode-v1.test.ak`. These tests drive the three
//// handlers over whole transactions: an honest bind, walk and finish, and one
//// `fail` test per guard where the honest transaction is changed so that one
//// check flips.

use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/cek_proof_v1
use midgard/script_decode_v1.{
  Decoded, Frame, Refused, ScriptDecodeRecordV1, ScriptDecodeRunV1,
  decode_env_v1, empty_stack_root, initial_run_v1, is_terminal, record_of_v1,
  stack_root_of, step_v1, step_with_frames_v1,
}
use script_decode_v1.{
  Bind, Close, Finish, ScriptDecodeMintRedeemerV1, ScriptDecodeSpendRedeemerV1,
  Walk, run_token_name, script_hash_v1,
} as family

// ## Fixtures

const policy = #"5c7d0a1f2e3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f8091"

const stager = #"11111111111111111111111111111111111111111111111111111111"

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

/// One staging output per run of up to three chunks.
fn staging_inputs(chunks: List<ByteArray>, index: Int) -> List<Input> {
  if chunks == [] {
    []
  } else {
    let datum = builtin.constr_data(
      0,
      [builtin.list_data(list.map(list.take(chunks, 3), builtin.b_data))],
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

fn run_input(run: ScriptDecodeRunV1) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"7272727272727272727272727272727272727272727272727272727272727272",
      output_index: 0,
    },
    output: own_output(run_token_name(run.script_hash), run),
  }
}

fn mint(redeemer: ScriptDecodeMintRedeemerV1, tx: Transaction) -> Bool {
  family.script_decode_v1.mint(redeemer, policy, tx)
}

fn spend(
  redeemer: ScriptDecodeSpendRedeemerV1,
  input: Input,
  tx: Transaction,
) -> Bool {
  family.script_decode_v1.spend(None, redeemer, input.output_reference, tx)
}

// ## Bind

fn bind_tx(script: ByteArray, chunk_size: Int) -> (Transaction, Int) {
  let staged = staging_inputs(chunks_of(script, chunk_size), 0)
  let hash = script_hash_v1(3, script)
  let run: Data = initial_run_v1(hash, 3, script)
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

test script_decode_family_bind_rejects_another_language() fail {
  let (tx, staged) = bind_tx(nested_script(3), 2)
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

// ## Walk

fn walk_tx(
  script: ByteArray,
  run: ScriptDecodeRunV1,
  next: ScriptDecodeRunV1,
  chunk_size: Int,
) -> (Transaction, List<Int>) {
  let staged = staging_inputs(chunks_of(script, chunk_size), 0)
  (
    Transaction {
      ..transaction.placeholder,
      inputs: [run_input(run), fee_input()],
      reference_inputs: staged,
      outputs: [own_output(run_token_name(run.script_hash), next)],
    },
    indices(staged),
  )
}

/// Walks `run` to its end, checking each step's transaction against the
/// spend handler. `frames` is the whole stack, top first.
fn walk_all(
  script: ByteArray,
  run: ScriptDecodeRunV1,
  frames: List<Frame>,
  fuel: Int,
  visible: Int,
  steps: Int,
) -> (ScriptDecodeRunV1, Int) {
  if is_terminal(run.machine.control) {
    (run, steps)
  } else {
    let (machine, after) = step_with_frames_v1(
      decode_env_v1(script, run),
      run.machine,
      frames,
      visible,
      fuel,
    )
    let next = ScriptDecodeRunV1 { ..run, machine }
    let (tx, staging) = walk_tx(script, run, next, 5)
    expect
      spend(
        Walk {
          staging,
          reveals: list.take(frames, visible),
          base: stack_root_of(list.drop(frames, visible), empty_stack_root()),
          fuel,
          run_output: 0,
        },
        run_input(run),
        tx,
      )
    walk_all(script, next, after, fuel, visible, steps + 1)
  }
}

fn opened(script: ByteArray) -> ScriptDecodeRunV1 {
  initial_run_v1(script_hash_v1(3, script), 3, script)
}

/// The honest first step of `script` and its walk redeemer.
fn first_walk(
  script: ByteArray,
  fuel: Int,
) -> (Transaction, ScriptDecodeSpendRedeemerV1, ScriptDecodeRunV1) {
  let run = opened(script)
  let machine = step_v1(
    decode_env_v1(script, run),
    run.machine,
    [],
    empty_stack_root(),
    fuel,
  )
  let (tx, staging) = walk_tx(
    script,
    run,
    ScriptDecodeRunV1 { ..run, machine },
    if bytearray.length(script) > 4095 {
      4095
    } else {
      5
    },
  )
  (
    tx,
    Walk { staging, reveals: [], base: empty_stack_root(), fuel, run_output: 0 },
    run,
  )
}

test script_decode_family_walk_advances_a_run() {
  let (tx, redeemer, run) = first_walk(nested_script(3), 4)
  spend(redeemer, run_input(run), tx)
}

test script_decode_family_walk_reaches_the_decoded_record() {
  let script = nested_script(12)
  let (run, steps) = walk_all(script, opened(script), [], 3, 2, 0)
  expect Some(record) = record_of_v1(run)
  and {
    steps > 5,
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
    let (run, _) = walk_all(script, opened(script), [], 40, 4, 0)
    expect Some(Decoded { program_version, phase2_class, script_hash, .. }) = record_of_v1(
      run,
    )
    and {
      program_version == 1,
      phase2_class == 0,
      script_hash == script_hash_v1(3, script),
    }
  }
  and {
    decoded(compiled_small),
    decoded(compiled_medium),
  }
}

test script_decode_family_walk_at_max_script_size() {
  let (tx, redeemer, run) = first_walk(max_script(), 60)
  spend(redeemer, run_input(run), tx)
}

test script_decode_family_walk_rejects_other_staged_bytes() fail {
  let (tx, redeemer, run) = first_walk(nested_script(3), 4)
  spend(
    redeemer,
    run_input(run),
    Transaction {
      ..tx,
      reference_inputs: staging_inputs(chunks_of(nested_script(4), 5), 0),
    },
  )
}

test script_decode_family_walk_rejects_a_wrong_successor() fail {
  let (tx, redeemer, run) = first_walk(nested_script(3), 4)
  expect Walk { staging, reveals, base, run_output, .. } = redeemer
  spend(
    Walk { staging, reveals, base, fuel: 3, run_output },
    run_input(run),
    tx,
  )
}

test script_decode_family_walk_rejects_no_progress() fail {
  let script = nested_script(3)
  let run = opened(script)
  let (tx, staging) = walk_tx(script, run, run, 5)
  spend(
    Walk {
      staging,
      reveals: [],
      base: empty_stack_root(),
      fuel: 0,
      run_output: 0,
    },
    run_input(run),
    tx,
  )
}

test script_decode_family_walk_rejects_a_wrong_reveal() fail {
  let (tx, redeemer, run) = first_walk(nested_script(3), 4)
  expect Walk { staging, base, fuel, run_output, .. } = redeemer
  spend(
    Walk { staging, reveals: [script_decode_v1.FDelay], base, fuel, run_output },
    run_input(run),
    tx,
  )
}

test script_decode_family_walk_rejects_a_mint() fail {
  let (tx, redeemer, run) = first_walk(nested_script(3), 4)
  spend(
    redeemer,
    run_input(run),
    Transaction { ..tx, mint: assets.from_asset(policy, run.script_hash, 1) },
  )
}

test script_decode_family_walk_rejects_two_runs() fail {
  let (tx, redeemer, run) = first_walk(nested_script(3), 4)
  let other = opened(nested_script(4))
  spend(
    redeemer,
    run_input(run),
    Transaction {
      ..tx,
      inputs: [
        run_input(run),
        Input {
          ..run_input(other),
          output_reference: OutputReference {
            transaction_id: #"7373737373737373737373737373737373737373737373737373737373737373",
            output_index: 0,
          },
        },
        fee_input(),
      ],
    },
  )
}

test script_decode_family_walk_rejects_a_successor_elsewhere() fail {
  let (tx, redeemer, run) = first_walk(nested_script(3), 4)
  expect [output] = tx.outputs
  spend(
    redeemer,
    run_input(run),
    Transaction {
      ..tx,
      outputs: [
        Output {
          ..output,
          address: Address {
            payment_credential: VerificationKey(stager),
            stake_credential: None,
          },
        },
      ],
    },
  )
}

test script_decode_family_walk_rejects_a_run_without_its_token() fail {
  let (tx, redeemer, run) = first_walk(nested_script(3), 4)
  let bare = Input {
    ..run_input(run),
    output: Output {
      ..run_input(run).output,
      value: assets.from_lovelace(5_000_000),
    },
  }
  spend(redeemer, bare, Transaction { ..tx, inputs: [bare, fee_input()] })
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
