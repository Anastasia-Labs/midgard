//// The script-decode family: a permissionless, one-sided record of how
//// Cardano decodes one serialised Plutus script.
////
//// A run is opened for the script hash `H = blake2b_224(language ‖ bytes)`
//// and walked to a verdict over as many transactions as the decode needs.
//// The verdict is minted as a permanent record token named `H`, carrying
//// `Decoded` or `Refused`. Decoding is deterministic, so anyone may open a run
//// and any two records for one `H` agree.
////
//// - **Bind** (mint): concatenates the staged chunks, hashes them, and opens a
////   run at this script's address under the run token `"R" ‖ H`.
//// - **Walk** (spend): re-binds the staged bytes to `H`, advances the decoder
////   and writes the successor run.
//// - **Finish** (mint, with the run spent): burns the run token and mints the
////   record token with the run's verdict.
////
//// Staged bytes ride in reference inputs whose inline datum is
//// `Constr 0 [List [B chunk, ..]]`. Their split is free: only the hash binds
//// them.

use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{blake2b_224}
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/script_decode_v1.{
  Frame, ScriptDecodeRunV1, decode_env_v1, initial_run_v1, language_midgard_v1,
  language_plutus_v3, record_of_v1, step_v1,
} as decoder

pub type ScriptDecodeMintRedeemerV1 {
  /// Opens a run. `staging` names reference inputs, in order, whose chunks
  /// concatenate to the script bytes.
  Bind { language: Int, staging: List<Int>, run_output: Int }
  /// Burns the spent run's token and records its verdict at `record_output`.
  Finish { record_output: Int }
}

pub type ScriptDecodeSpendRedeemerV1 {
  /// Advances the run by up to `fuel` ticks. `reveals` are the stack's top
  /// frames (top first) over the root `base`.
  Walk {
    staging: List<Int>,
    reveals: List<Frame>,
    base: ByteArray,
    fuel: Int,
    run_output: Int,
  }
  /// The run is consumed by a `Finish` mint in the same transaction.
  Close
}

/// The run token's asset name: `"R" ‖ H`.
pub fn run_token_name(script_hash: ByteArray) -> AssetName {
  builtin.cons_bytearray(0x52, script_hash)
}

/// The script hash Cardano computes for `script_bytes` in `language`.
pub fn script_hash_v1(language: Int, script_bytes: ByteArray) -> ByteArray {
  blake2b_224(builtin.cons_bytearray(language, script_bytes))
}

/// The bytes staged in the named reference inputs, in order.
pub fn staged_bytes_v1(
  reference_inputs: List<Input>,
  staging: List<Int>,
) -> ByteArray {
  list.foldr(
    staging,
    #"",
    fn(index, rest) {
      expect Some(input) = list.at(reference_inputs, index)
      expect InlineDatum(datum) = input.output.datum
      expect Pair(0, [chunks]) = builtin.un_constr_data(datum)
      list.foldr(
        builtin.un_list_data(chunks),
        rest,
        fn(chunk, tail) { bytearray.concat(builtin.un_b_data(chunk), tail) },
      )
    },
  )
}

fn own_address(policy: PolicyId) -> Address {
  Address { payment_credential: Script(policy), stake_credential: None }
}

/// The output at `index` sits at this script's address, holds exactly the one
/// token `name` of this policy, carries no reference script, and has `datum`
/// inline.
fn is_own_output(
  outputs: List<Output>,
  index: Int,
  policy: PolicyId,
  name: AssetName,
  datum: Data,
) -> Bool {
  expect Some(Output { address, value, datum: output_datum, reference_script })
    = list.at(outputs, index)
  and {
    address == own_address(policy),
    reference_script == None,
    output_datum == InlineDatum(datum),
    dict.to_pairs(assets.tokens(value, policy)) == [Pair(name, 1)],
  }
}

/// The transaction's inputs at this script's address.
fn own_inputs(inputs: List<Input>, policy: PolicyId) -> List<Input> {
  list.filter(
    inputs,
    fn(input) { input.output.address.payment_credential == Script(policy) },
  )
}

/// The run held by an input: its datum, under exactly its own run token.
fn run_of(input: Input, policy: PolicyId) -> ScriptDecodeRunV1 {
  expect InlineDatum(datum) = input.output.datum
  expect run: ScriptDecodeRunV1 = datum
  expect
    dict.to_pairs(assets.tokens(input.output.value, policy)) == [
      Pair(run_token_name(run.script_hash), 1),
    ]
  run
}

validator script_decode_v1 {
  mint(redeemer: ScriptDecodeMintRedeemerV1, policy: PolicyId, tx: Transaction) {
    when redeemer is {
      Bind { language, staging, run_output } -> {
        expect language == language_plutus_v3 || language == language_midgard_v1
        // A bind spends no run, so every token of this policy it touches is
        // the one it mints.
        expect [] = own_inputs(tx.inputs, policy)
        let script_bytes = staged_bytes_v1(tx.reference_inputs, staging)
        let script_hash = script_hash_v1(language, script_bytes)
        let name = run_token_name(script_hash)
        expect dict.to_pairs(assets.tokens(tx.mint, policy)) == [Pair(name, 1)]
        let run: Data = initial_run_v1(script_hash, language, script_bytes)
        is_own_output(tx.outputs, run_output, policy, name, run)
      }
      Finish { record_output } -> {
        expect [run_input] = own_inputs(tx.inputs, policy)
        let run = run_of(run_input, policy)
        expect Some(record) = record_of_v1(run)
        let run_name = run_token_name(run.script_hash)
        let minted = dict.to_pairs(assets.tokens(tx.mint, policy))
        expect or {
            minted == [Pair(run.script_hash, 1), Pair(run_name, -1)],
            minted == [Pair(run_name, -1), Pair(run.script_hash, 1)],
          }
        let record: Data = record
        is_own_output(
          tx.outputs,
          record_output,
          policy,
          run.script_hash,
          record,
        )
      }
    }
  }

  spend(
    _datum: Option<Data>,
    redeemer: ScriptDecodeSpendRedeemerV1,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(own_input) = transaction.find_input(tx.inputs, own_ref)
    expect Script(policy) = own_input.output.address.payment_credential
    let run = run_of(own_input, policy)
    when redeemer is {
      Walk { staging, reveals, base, fuel, run_output } -> {
        // One run per transaction, and no token of this policy is minted or
        // burnt, so the run token can only move into the successor. A mint
        // runs this policy's mint handler: Bind refuses a transaction that
        // spends a run (`expect [] = own_inputs`), and Finish needs its one
        // run to hold a record (`record_of_v1`), which leaves a machine that
        // no Walk changes (`machine != run.machine` below).
        expect [_] = own_inputs(tx.inputs, policy)
        let script_bytes = staged_bytes_v1(tx.reference_inputs, staging)
        expect script_hash_v1(run.language, script_bytes) == run.script_hash
        let machine = step_v1(
          decode_env_v1(script_bytes, run),
          run.machine,
          reveals,
          base,
          fuel,
        )
        expect machine != run.machine
        let successor: Data = ScriptDecodeRunV1 { ..run, machine }
        is_own_output(
          tx.outputs,
          run_output,
          policy,
          run_token_name(run.script_hash),
          successor,
        )
      }
      Close ->
        assets.quantity_of(tx.mint, policy, run_token_name(run.script_hash)) == -1
    }
  }

  else(_) {
    fail
  }
}
