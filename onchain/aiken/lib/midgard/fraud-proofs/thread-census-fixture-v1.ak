//// Census reshapes of an honest validation-trace move. Each takes the honest
//// L1 transaction of a fixture, in which the thread is the lone input and its
//// successor is output 0 with the thread's lovelace, and changes only how the
//// move is funded or signed:
////
//// - `lone_input_paying(fee)`: the thread alone pays `fee`, so its successor
////   carries `fee` less lovelace. Accepted up to the cap and refused above it.
//// - `beside_wallet_input(skim)`: a wallet input pays the fee, and the
////   successor carries `skim` less lovelace than the thread. Accepted only at
////   zero.
//// - `without_signers`: no required signers. Refused on a prover-only move and
////   accepted on a move open to anyone.

use cardano/address
use cardano/assets
use cardano/transaction.{Input, NoDatum, Output, OutputReference, Transaction}
use env

/// The most a lone thread input may pay for its own move.
pub const cap: Int = env.max_dispute_move_fee_lovelace

const wallet_key_hash =
  #"f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0"

fn wallet_input() -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1",
      output_index: 0,
    },
    output: Output {
      address: address.from_verification_key(wallet_key_hash),
      value: assets.from_lovelace(100_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn with_lovelace(output: Output, lovelace: Int) -> Output {
  Output {
    ..output,
    value: assets.merge(
      assets.without_lovelace(output.value),
      assets.from_lovelace(lovelace),
    ),
  }
}

/// The thread alone pays `fee` out of its own lovelace.
pub fn lone_input_paying(fee: Int) -> fn(Transaction) -> Transaction {
  fn(tx: Transaction) {
    expect [thread] = tx.inputs
    expect [successor, ..rest] = tx.outputs
    Transaction {
      ..tx,
      fee,
      outputs: [
        with_lovelace(successor, assets.lovelace_of(thread.output.value) - fee),
        ..rest
      ],
    }
  }
}

/// A wallet input after the thread pays the capped fee; the successor carries
/// `skim` less lovelace than the thread.
pub fn beside_wallet_input(skim: Int) -> fn(Transaction) -> Transaction {
  fn(tx: Transaction) {
    expect [thread] = tx.inputs
    expect [successor, ..rest] = tx.outputs
    Transaction {
      ..tx,
      fee: cap,
      inputs: [thread, wallet_input()],
      outputs: [
        with_lovelace(successor, assets.lovelace_of(thread.output.value) - skim),
        ..rest
      ],
    }
  }
}

/// The move carries no required signers.
pub fn without_signers(tx: Transaction) -> Transaction {
  Transaction { ..tx, extra_signatories: [] }
}

/// The honest transaction, unchanged.
pub fn honest(tx: Transaction) -> Transaction {
  tx
}
