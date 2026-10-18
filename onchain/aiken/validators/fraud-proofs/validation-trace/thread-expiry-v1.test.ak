//// The thread expiry script, driven through the deployed validators: a thread
//// its case can no longer take is released by its holder (game, timeout,
//// boundary or award) to the expiry withdrawal, which returns the thread's
//// value to the defender named by the thread header.

use aiken/collection/list
use aiken/interval
use cardano/address.{Address, Credential, Script}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
}
use env
use fraud_proofs/validation_trace/award_v1 as award_validator
use fraud_proofs/validation_trace/boundary_v1 as boundary_validator
use fraud_proofs/validation_trace/game_v1 as game_validator
use fraud_proofs/validation_trace/thread_expiry_v1 as expiry_validator
use fraud_proofs/validation_trace/timeout_v1 as timeout_validator
use midgard/computation_thread as ct
use midgard/validation_award_v1.{SpendReceipt}
use midgard/validation_case_v1.{CaseThreadV1, ThreadHeaderV1}
use midgard/validation_dispute_v1
use midgard/validation_resolution_v1

/// The case validator: the thread token policy.
const thread_policy =
  #"c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1"

const expiry_hash = #"d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7"

const game_hash = #"d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2"

const boundary_hash =
  #"d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3"

const award_hash = #"d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4"

const timeout_hash = #"d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5"

const resolver_hash =
  #"d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8"

const hub_policy = #"c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0"

const prover = #"e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1"

const defender = #"e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0"

const other_key = #"e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2"

const thread_name =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const other_thread_name =
  #"5555555555555555555555555555555555555555555555555555555555555555"

const h32_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const h32_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const started = 1_000_000

const case_duration = validation_dispute_v1.max_dispute_duration_milliseconds

const thread_lovelace = 5_000_000

const fee = 300_000

fn expired_at() -> Int {
  started + case_duration + 1
}

// ## Fixtures

fn thread_out_ref() -> OutputReference {
  OutputReference { transaction_id: h32_a, output_index: 0 }
}

fn other_out_ref() -> OutputReference {
  OutputReference { transaction_id: h32_b, output_index: 0 }
}

fn header() -> ThreadHeaderV1 {
  ThreadHeaderV1 { started_at: started, defender_vkh: defender }
}

fn thread() -> CaseThreadV1 {
  CaseThreadV1 {
    header: header(),
    version: validation_case_v1.thread_version,
    case_id: h32_a,
    challenger_root: h32_a,
    defender_root: h32_b,
    step_count: 3,
    low_index: 0,
    high_index: 3,
    agreed_low_hash: h32_a,
    defender_high_hash: h32_b,
    challenger_high_hash: h32_a,
    round: 0,
    turn_started_at: started,
    defender_clock_ms: validation_dispute_v1.dispute_side_clock_milliseconds,
    challenger_clock_ms: validation_dispute_v1.dispute_side_clock_milliseconds,
    turn: validation_dispute_v1.AwaitingOperator { midpoint: 1 },
  }
}

fn thread_datum() -> Data {
  let datum: Data = ct.StepDatum { fraud_prover: prover, data: Some(thread()) }
  datum
}

fn thread_value() -> assets.Value {
  assets.from_lovelace(thread_lovelace)
    |> assets.add(thread_policy, thread_name, 1)
}

fn held_at(script_hash: ByteArray, datum: Data, value: assets.Value) -> Input {
  Input {
    output_reference: thread_out_ref(),
    output: Output {
      address: address.from_script(script_hash),
      value,
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn thread_at(script_hash: ByteArray) -> Input {
  held_at(script_hash, thread_datum(), thread_value())
}

fn wallet_input() -> Input {
  Input {
    output_reference: other_out_ref(),
    output: Output {
      address: address.from_verification_key(other_key),
      value: assets.from_lovelace(2_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn payout(to: Address, lovelace: Int, tag: OutputReference) -> Output {
  let tag_data: Data = tag
  Output {
    address: to,
    value: assets.from_lovelace(lovelace),
    datum: InlineDatum(tag_data),
    reference_script: None,
  }
}

fn defender_payout(lovelace: Int) -> Output {
  payout(address.from_verification_key(defender), lovelace, thread_out_ref())
}

fn expiry_withdrawal() -> Pairs<Credential, Int> {
  [Pair(Script(expiry_hash), 0)]
}

/// The honest expiry of the thread held by `thread_input`, alone in the
/// transaction, which pays the fee from the thread.
fn expiry_tx(thread_input: Input) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [thread_input],
    outputs: [defender_payout(thread_lovelace - fee)],
    mint: assets.from_asset(thread_policy, thread_name, -1),
    fee,
    validity_range: interval.after(expired_at()),
    withdrawals: expiry_withdrawal(),
  }
}

fn expires(tx: Transaction) -> Bool {
  expiry_validator.main.withdraw(
    thread_policy,
    case_duration,
    Void,
    Script(expiry_hash),
    tx,
  )
}

fn with_outputs(tx: Transaction, outputs: List<Output>) -> Transaction {
  Transaction { ..tx, outputs }
}

// ### The holders

fn game_releases(datum: Data, tx: Transaction) -> Bool {
  expect datum: game_validator.Datum = datum
  game_validator.main.spend(
    boundary_hash,
    timeout_hash,
    thread_policy,
    expiry_hash,
    Some(datum),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    thread_out_ref(),
    tx,
  )
}

fn timeout_releases(datum: Data, tx: Transaction) -> Bool {
  expect datum: timeout_validator.Datum = datum
  timeout_validator.main.spend(
    thread_policy,
    expiry_hash,
    Some(datum),
    timeout_validator.CloseTimedOutThread {
      input_index: 0,
      case_input_index: 0,
    },
    thread_out_ref(),
    tx,
  )
}

fn boundary_releases(datum: Data, tx: Transaction) -> Bool {
  expect datum: boundary_validator.Datum = datum
  boundary_validator.main.spend(
    [resolver_hash],
    award_hash,
    thread_policy,
    expiry_hash,
    Some(datum),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    thread_out_ref(),
    tx,
  )
}

fn award_releases(datum: Data, tx: Transaction) -> Bool {
  award_validator.main.spend(
    thread_policy,
    hub_policy,
    expiry_hash,
    Some(datum),
    SpendReceipt,
    thread_out_ref(),
    tx,
  )
}

// ## Honest expiry, from each holder

test a_thread_at_the_game_expires_to_its_defender() {
  let tx = expiry_tx(thread_at(game_hash))
  and {
    game_releases(thread_datum(), tx),
    expires(tx),
  }
}

test a_thread_at_the_timeout_expires_to_its_defender() {
  let tx = expiry_tx(thread_at(timeout_hash))
  and {
    timeout_releases(thread_datum(), tx),
    expires(tx),
  }
}

test a_thread_at_the_boundary_expires_to_its_defender() {
  let tx = expiry_tx(thread_at(boundary_hash))
  and {
    boundary_releases(thread_datum(), tx),
    expires(tx),
  }
}

test the_award_releases_a_thread_to_the_expiry_withdrawal() {
  let awarded: Data = ct.StepDatum {
    fraud_prover: prover,
    data: Some(validation_resolution_v1.winning_resolution()),
  }
  award_releases(
    awarded,
    expiry_tx(held_at(award_hash, awarded, thread_value())),
  )
}

/// Beside a wallet input, the thread pays its whole value to the defender.
test an_expiry_beside_a_wallet_input_returns_the_whole_thread() {
  let tx = expiry_tx(thread_at(game_hash))
  expires(
    Transaction {
      ..tx,
      inputs: [wallet_input(), ..tx.inputs],
      outputs: [defender_payout(thread_lovelace)],
    },
  )
}

// ## A holder releases only a thread, and only to the expiry withdrawal

test the_game_does_not_release_a_thread_without_the_expiry_withdrawal() fail {
  game_releases(
    thread_datum(),
    Transaction { ..expiry_tx(thread_at(game_hash)), withdrawals: [] },
  )
}

test the_timeout_does_not_release_a_thread_without_the_expiry_withdrawal() fail {
  timeout_releases(
    thread_datum(),
    Transaction { ..expiry_tx(thread_at(timeout_hash)), withdrawals: [] },
  )
}

test the_boundary_does_not_release_a_thread_without_the_expiry_withdrawal() fail {
  boundary_releases(
    thread_datum(),
    Transaction { ..expiry_tx(thread_at(boundary_hash)), withdrawals: [] },
  )
}

test the_award_does_not_release_a_thread_without_the_expiry_withdrawal() fail {
  award_releases(
    thread_datum(),
    Transaction { ..expiry_tx(thread_at(award_hash)), withdrawals: [] },
  )
}

test the_timeout_does_not_release_a_utxo_without_a_thread_token() fail {
  timeout_releases(
    thread_datum(),
    expiry_tx(
      held_at(
        timeout_hash,
        thread_datum(),
        assets.from_lovelace(thread_lovelace),
      ),
    ),
  )
}

test the_boundary_does_not_release_a_utxo_without_a_thread_token() fail {
  boundary_releases(
    thread_datum(),
    expiry_tx(
      held_at(
        boundary_hash,
        thread_datum(),
        assets.from_lovelace(thread_lovelace),
      ),
    ),
  )
}

/// A lost-defense receipt holds a token of the award policy, not of the thread
/// policy, so the expiry withdrawal never releases it.
test the_award_does_not_release_a_receipt() fail {
  let receipt: Data = validation_case_v1.LostDefenseReceiptV1 {
    case_id: h32_a,
    defender_vkh: defender,
    outcome: validation_dispute_v1.ChallengerWins,
    bounty_claimant: prover,
  }
  award_releases(
    receipt,
    expiry_tx(
      held_at(
        award_hash,
        receipt,
        assets.from_lovelace(thread_lovelace)
          |> assets.add(award_hash, thread_name, 1),
      ),
    ),
  )
}

// ## The expiry checks

/// A second thread, released beside the first by its holder, would leave
/// without its own expiry.
test an_expiry_with_two_thread_inputs_is_refused() fail {
  let tx = expiry_tx(thread_at(game_hash))
  let second = Input {
    output_reference: other_out_ref(),
    output: Output {
      ..thread_at(timeout_hash).output,
      value: assets.from_lovelace(thread_lovelace)
        |> assets.add(thread_policy, other_thread_name, 1),
    },
  }
  expires(
    Transaction {
      ..tx,
      inputs: list.concat(tx.inputs, [second]),
      outputs: [defender_payout(thread_lovelace)],
    },
  )
}

test an_expiry_beside_a_case_input_is_refused() fail {
  let tx = expiry_tx(thread_at(game_hash))
  expires(
    Transaction {
      ..tx,
      inputs: [
        Input {
          ..wallet_input(),
          output: Output {
            ..wallet_input().output,
            address: address.from_script(thread_policy),
          },
        },
        ..tx.inputs
      ],
      outputs: [defender_payout(thread_lovelace)],
    },
  )
}

test an_expiry_at_d_case_is_refused() fail {
  expires(
    Transaction {
      ..expiry_tx(thread_at(game_hash)),
      validity_range: interval.after(started + case_duration),
    },
  )
}

test an_expiry_without_the_burn_is_refused() fail {
  expires(Transaction { ..expiry_tx(thread_at(game_hash)), mint: assets.zero })
}

test an_expiry_of_a_thread_holding_two_tokens_is_refused() fail {
  let tx = expiry_tx(
    held_at(
      game_hash,
      thread_datum(),
      thread_value() |> assets.add(thread_policy, other_thread_name, 1),
    ),
  )
  expires(tx)
}

test an_expiry_paying_another_key_is_refused() fail {
  let tx = expiry_tx(thread_at(game_hash))
  expires(
    with_outputs(
      tx,
      [
        payout(
          address.from_verification_key(prover),
          thread_lovelace - fee,
          thread_out_ref(),
        ),
      ],
    ),
  )
}

test an_expiry_paying_the_defender_less_is_refused() fail {
  let tx = expiry_tx(thread_at(game_hash))
  expires(with_outputs(tx, [defender_payout(thread_lovelace - fee - 1)]))
}

test an_expiry_paying_under_another_tag_is_refused() fail {
  let tx = expiry_tx(thread_at(game_hash))
  expires(
    with_outputs(
      tx,
      [
        payout(
          address.from_verification_key(defender),
          thread_lovelace - fee,
          other_out_ref(),
        ),
      ],
    ),
  )
}

test an_expiry_paying_the_fee_beside_another_input_is_refused() fail {
  let tx = expiry_tx(thread_at(game_hash))
  expires(Transaction { ..tx, inputs: [wallet_input(), ..tx.inputs] })
}

test an_expiry_paying_over_the_fee_cap_is_refused() fail {
  let over = env.max_dispute_move_fee_lovelace + 1
  let tx = expiry_tx(thread_at(game_hash))
  expires(
    Transaction {
      ..tx,
      fee: over,
      outputs: [defender_payout(thread_lovelace - over)],
    },
  )
}

test an_expiry_of_a_thread_without_a_state_is_refused() fail {
  let bare: Data = ct.StepDatum { fraud_prover: prover, data: None }
  expires(expiry_tx(held_at(game_hash, bare, thread_value())))
}
