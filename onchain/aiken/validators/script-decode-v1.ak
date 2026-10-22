//// The script-decode family: a permissionless, one-sided record of how
//// Cardano decodes one serialised Plutus script.
////
//// A run is opened for the script hash `H = blake2b_224(language ‖ bytes)`
//// and walked to a verdict over as many transactions as the decode needs.
//// The verdict is minted as a permanent record token named `H`, carrying
//// `Decoded` or `Refused`. Decoding is deterministic, so anyone may open a run
//// and any two records for one `H` agree.
////
//// - **Bind** (mint): concatenates the staged bytes, hashes them, and opens a
////   run at this script's address under the run token `"R" ‖ H`.
//// - **Walk** (spend): advances the decoder and writes the successor run. The
////   decoding itself runs in withdraw-zero stakers, one per class of decoder
////   controls; the spend checks that their segments chain the spent run's
////   machine to the successor's.
//// - **Finish** (mint, with the run spent): burns the run token and mints the
////   record token with the run's verdict.
////
//// Staged bytes ride in reference inputs whose inline datum is
//// `Constr 0 [B bytes]`. Their split is free: only the hash binds them.

use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash, blake2b_224, blake2b_256}
use aiken/primitive/bytearray
use cardano/address.{Address, Credential, Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, ScriptPurpose,
  Transaction, Withdraw,
}
use midgard/script_decode_v1.{
  Control, DecodeEnv, Frame, Machine, ScriptDecodeRunV1, decode_env_at_v1,
  initial_run_v1, language_midgard_v1, language_plutus_v3, machine_hash_v1,
  record_of_v1,
} as decoder

pub type ScriptDecodeMintRedeemerV1 {
  /// Opens a run. `staging` names reference inputs, in order, whose bytes
  /// concatenate to the script bytes.
  Bind { language: Int, staging: List<Int>, run_output: Int }
  /// Burns the spent run's token and records its verdict at `record_output`.
  Finish { record_output: Int }
}

pub type ScriptDecodeSpendRedeemerV1 {
  /// Advances the run along the segments of the stakers' redeemers and
  /// writes the successor at `run_output`.
  Walk { run_output: Int }
  /// The run is consumed by a `Finish` mint in the same transaction.
  Close
}

/// One segment a staker steps: from `machine`, revealing `reveals` (top
/// first) over the root `base`, for up to `fuel` ticks.
pub type ScriptDecodeSegmentV1 {
  machine: Machine,
  reveals: List<Frame>,
  base: ByteArray,
  fuel: Int,
}

/// What a staker passes for one segment: its `position` in the Walk's chain
/// and the hashes of the machine it starts `from` and the machine it reaches
/// (`to`).
pub type ScriptDecodeLinkV1 {
  position: Int,
  from: ByteArray,
  to: ByteArray,
}

/// A staker's redeemer: the run it steps, the links it passes (read by the
/// spend), the staging of the run's bytes, and its segments, in the order of
/// `links`.
pub type ScriptDecodeStakeRedeemerV1 {
  run: OutputReference,
  links: List<ScriptDecodeLinkV1>,
  staging: List<Int>,
  segments: List<ScriptDecodeSegmentV1>,
}

/// The fields of a run a staker reads; the machine stays Data.
type RunHeader {
  script_hash: ByteArray,
  language: Int,
  flat_start: Int,
  flat_length: Int,
  byte_length: Int,
  machine: Data,
}

/// The run token's asset name: `"R" ‖ H`.
pub fn run_token_name(script_hash: ByteArray) -> AssetName {
  builtin.cons_bytearray(0x52, script_hash)
}

/// The script hash Cardano computes for `script_bytes` in `language`.
pub fn script_hash_v1(language: Int, script_bytes: ByteArray) -> ByteArray {
  blake2b_224(builtin.cons_bytearray(language, script_bytes))
}

/// The bytes staged in the named reference inputs, in order: each datum's
/// first field, a byte string. The datum's constructor and its other fields
/// are not checked: the bytes are bound by their hash, which `Bind` makes
/// the run's script hash and every `Walk` checks against it.
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
      bytearray.concat(
        builtin.un_b_data(
          builtin.head_list(builtin.snd_pair(builtin.un_constr_data(datum))),
        ),
        rest,
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

/// The machine hash a segment chain ends at, from the links the stakers in
/// `stakers` pass for the run at `own_ref`, starting `from` the spent run's
/// machine hash. Each staker's links are read from its redeemer under exactly
/// `Withdraw(Script(staker))`, and every one must name the run. The links
/// must tile positions 0, 1, ... with no gap, overlap or link left over, each
/// starting where the one before it ends.
fn chain_end(
  stakers: List<ScriptHash>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  own_ref: OutputReference,
  from: ByteArray,
) -> ByteArray {
  let own_ref: Data = own_ref
  let chains = list.foldr(
    stakers,
    [],
    fn(staker, chains) {
      when pairs.get_first(redeemers, Withdraw(Script(staker))) is {
        None -> chains
        Some(redeemer) -> {
          expect [run,
                links, ..] = builtin.snd_pair(builtin.un_constr_data(redeemer))
          expect run == own_ref
          [builtin.un_list_data(links), ..chains]
        }
      }
    },
  )
  follow(chains, 0, from)
}

fn follow(chains: List<List<Data>>, position: Int, from: ByteArray) -> ByteArray {
  when take_link(chains, position) is {
    None -> from
    Some((link_from, link_to, rest)) -> {
      expect link_from == from
      follow(rest, position + 1, link_to)
    }
  }
}

/// The link at `position` at the head of one of `chains`, with the chains
/// after it is taken; None when every chain is spent. A chain left with no
/// link at `position` fails.
fn take_link(
  chains: List<List<Data>>,
  position: Int,
) -> Option<(ByteArray, ByteArray, List<List<Data>>)> {
  when chains is {
    [] -> None
    [links, ..rest] ->
      when links is {
        [] -> take_link(rest, position)
        [link, ..more] -> {
          expect [at,
                link_from,
                link_to] = builtin.snd_pair(builtin.un_constr_data(link))
          if builtin.un_i_data(at) == position {
            Some(
              (
                builtin.un_b_data(link_from),
                builtin.un_b_data(link_to),
                [more, ..rest],
              ),
            )
          } else {
            expect Some((found_from, found_to, others)) = take_link(
              rest,
              position,
            )
            Some((found_from, found_to, [links, ..others]))
          }
        }
      }
  }
}

fn data_hash(data: Data) -> ByteArray {
  blake2b_256(builtin.serialise_data(data))
}

validator script_decode_v1(stakers: List<ScriptHash>) {
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
      Walk { run_output } -> {
        // One run per transaction, and no token of this policy is minted or
        // burnt, so the run token can only move into the successor. A mint
        // runs this policy's mint handler: Bind refuses a transaction that
        // spends a run (`expect [] = own_inputs`), and Finish needs its one
        // run to hold a record (`record_of_v1`), which leaves a machine that
        // no segment changes (each staker steps only its own class, and no
        // class holds a terminal control), while a Walk must change it
        // (`to != from` below).
        expect [_] = own_inputs(tx.inputs, policy)
        expect InlineDatum(datum) = own_input.output.datum
        expect
            [
              script_hash, language, flat_start, flat_length, byte_length,
              machine,
            ]
          = builtin.snd_pair(builtin.un_constr_data(datum))
        let from = data_hash(machine)
        let to = chain_end(stakers, tx.redeemers, own_ref, from)
        expect to != from
        expect
            Some(Output {
              address,
              value,
              datum: InlineDatum(successor),
              reference_script,
            })
          = list.at(tx.outputs, run_output)
        expect
            Pair(0,
            [
              next_script_hash, next_language, next_flat_start, next_flat_length,
              next_byte_length, next_machine,
            ])
          = builtin.un_constr_data(successor)
        and {
          address == own_address(policy),
          reference_script == None,
          dict.to_pairs(assets.tokens(value, policy)) == [
            Pair(run_token_name(run.script_hash), 1),
          ],
          next_script_hash == script_hash,
          next_language == language,
          next_flat_start == flat_start,
          next_flat_length == flat_length,
          next_byte_length == byte_length,
          data_hash(next_machine) == to,
        }
      }
      Close ->
        assets.quantity_of(tx.mint, policy, run_token_name(run.script_hash)) == -1
    }
  }

  else(_) {
    fail
  }
}

/// Holds when every segment is of the staker's class, starts at the machine
/// whose hash its link passes as `from`, and steps to the machine whose hash
/// its link passes as `to`. The run's bytes are re-bound to the script hash
/// of the run the redeemer names, read from the transaction. The spend binds
/// the links to its own run and chains them, so the run named here is the
/// spent run.
///
/// Each staker reads its redeemer without a structural check: the redeemer is
/// passed as `Data` to a function typed over `ScriptDecodeStakeRedeemerV1`.
/// A full check is redundant because every field that matters is bound by
/// hash. Each segment's machine must hash to its link's `from`, and the spend
/// accepts the links only when the first `from` is the hash of the spent
/// run's machine (written by a Bind or a checked Walk) and each later `from`
/// is the previous link's `to` (the hash of a machine a checked stepper
/// produced). Revealed frames are checked against the machine's stack root by
/// the stepper. The spend compares the run named here with its own input as
/// `Data`. A redeemer of the wrong shape makes a builtin fail, so it is
/// refused.
fn stake(
  redeemer: ScriptDecodeStakeRedeemerV1,
  tx: Transaction,
  in_class: fn(Control) -> Bool,
  step: fn(DecodeEnv, Machine, List<Frame>, ByteArray, Int) -> Machine,
) -> Bool {
  expect Some(run_input) = transaction.find_input(tx.inputs, redeemer.run)
  expect InlineDatum(datum) = run_input.output.datum
  expect run: RunHeader = datum
  let script_bytes = staged_bytes_v1(tx.reference_inputs, redeemer.staging)
  expect script_hash_v1(run.language, script_bytes) == run.script_hash
  let env = decode_env_at_v1(
    script_bytes,
    run.flat_start,
    run.flat_length,
    run.byte_length,
  )
  links_hold(env, redeemer.segments, redeemer.links, in_class, step)
}

fn links_hold(
  env: DecodeEnv,
  segments: List<ScriptDecodeSegmentV1>,
  links: List<ScriptDecodeLinkV1>,
  in_class: fn(Control) -> Bool,
  step: fn(DecodeEnv, Machine, List<Frame>, ByteArray, Int) -> Machine,
) -> Bool {
  when segments is {
    [] -> links == []
    [segment, ..rest] -> {
      expect [link, ..more] = links
      let machine = segment.machine
      expect in_class(machine.control)
      expect link.from == machine_hash_v1(machine)
      expect
        link.to == machine_hash_v1(
          step(env, machine, segment.reveals, segment.base, segment.fuel),
        )
      links_hold(env, rest, more, in_class, step)
    }
  }
}

/// Steps the term class: terms, their folds, naturals, magnitudes, constant
/// types and the final control.
validator script_decode_term_v1 {
  withdraw(redeemer: Data, _credential: Credential, tx: Transaction) {
    expect run: fn(Data, Transaction) -> Bool = fn(
      r: ScriptDecodeStakeRedeemerV1,
      tx: Transaction,
    ) {
      stake(r, tx, decoder.is_term_control_v1, decoder.step_term_v1)
    }
    run(redeemer, tx)
  }

  else(_) {
    fail
  }
}

/// Steps the value class: one constant component of a known type.
validator script_decode_value_v1 {
  withdraw(redeemer: Data, _credential: Credential, tx: Transaction) {
    expect run: fn(Data, Transaction) -> Bool = fn(
      r: ScriptDecodeStakeRedeemerV1,
      tx: Transaction,
    ) {
      stake(r, tx, decoder.is_value_control_v1, decoder.step_value_v1)
    }
    run(redeemer, tx)
  }

  else(_) {
    fail
  }
}

/// Steps the list class: list elements and Value map entries, one bit at a
/// time.
validator script_decode_list_v1 {
  withdraw(redeemer: Data, _credential: Credential, tx: Transaction) {
    expect run: fn(Data, Transaction) -> Bool = fn(
      r: ScriptDecodeStakeRedeemerV1,
      tx: Transaction,
    ) {
      stake(r, tx, decoder.is_list_control_v1, decoder.step_list_v1)
    }
    run(redeemer, tx)
  }

  else(_) {
    fail
  }
}

/// Steps the fold class: folding a finished sequence into its summary.
validator script_decode_fold_v1 {
  withdraw(redeemer: Data, _credential: Credential, tx: Transaction) {
    expect run: fn(Data, Transaction) -> Bool = fn(
      r: ScriptDecodeStakeRedeemerV1,
      tx: Transaction,
    ) {
      stake(r, tx, decoder.is_fold_control_v1, decoder.step_fold_v1)
    }
    run(redeemer, tx)
  }

  else(_) {
    fail
  }
}

/// Steps the byte-string class.
validator script_decode_bytes_v1 {
  withdraw(redeemer: Data, _credential: Credential, tx: Transaction) {
    expect run: fn(Data, Transaction) -> Bool = fn(
      r: ScriptDecodeStakeRedeemerV1,
      tx: Transaction,
    ) {
      stake(r, tx, decoder.is_bytes_control_v1, decoder.step_bytes_v1)
    }
    run(redeemer, tx)
  }

  else(_) {
    fail
  }
}

/// Steps the Data class.
validator script_decode_data_v1 {
  withdraw(redeemer: Data, _credential: Credential, tx: Transaction) {
    expect run: fn(Data, Transaction) -> Bool = fn(
      r: ScriptDecodeStakeRedeemerV1,
      tx: Transaction,
    ) {
      stake(r, tx, decoder.is_data_control_v1, decoder.step_data_v1)
    }
    run(redeemer, tx)
  }

  else(_) {
    fail
  }
}
