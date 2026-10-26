//// The script-sources middle yields read the auxiliary evidence the
//// non-output semantic resolver carries by its constructor. These vectors
//// drive the stage-four begin and stage-six fold-asset yields through
//// `withdraw` over an honest step, carried under the constructor the
//// evidence type gives each step, and under another constructor.
////
//// Whole-validator vectors for `script_sources_middle_yields_v1.stage_three_replay`
//// at its maximum shape: the last input of the maximum-input resolution
//// schedule, replayed against a prior ledger whose membership proof for that
//// input has the full 64 `Branch` steps a Midgard MPF path admits.
////
//// `replay_yield_accepts_the_deepest_ledger_proof` is the budget row: it runs
//// the deployed withdraw handler, not the library function, so its cost is the
//// replay step's cost at the deepest admissible proof. Its singleton-proof
//// twin and the fixture-only row below it bound the proof's own share by
//// subtraction.

use aiken/builtin.{slice_bytearray}
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, nibbles, suffix}
use aiken/merkle_patricia_forestry/merkling.{merkle_16}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Spend, Transaction, placeholder,
}
use fraud_proofs/validation_trace/script_sources_middle_yields_v1 as yields
use midgard/computation_thread as ct
use midgard/ledger_output_commitment_v1
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_machine_tests/constants.{h_a, h_b}
use midgard/validation_machine_tests/fixtures_phase_a.{singleton_root}
use midgard/validation_machine_tests/fixtures_script_sources.{
  maximum_input_schedule_replay_step,
  maximum_spend_input_schedule_terminal_vector, mint_fold_asset_step,
  stage_four_begin_step,
}
use midgard/validation_resolution_v1.{
  PreparedValidationResolutionStateV1, ValidationResolutionStateV1,
}
use midgard/validation_trace_v1.{ValidationMachineStateV1}

const dispatcher = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const prover = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const hash = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const yield_hash = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

// ## Evidence constructors

/// A transaction spending one dispatcher input whose prepared resolution
/// starts at `pre`, its action carrying `transition`, the auxiliary
/// evidence `auxiliary` and the middle role `role`.
fn dispatch(
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: Data,
  role: Int,
) -> Transaction {
  let prepared = PreparedValidationResolutionStateV1 {
    version: 1,
    resolution: ValidationResolutionStateV1 {
      version: 1,
      pre_state: pre,
      operator_successor_hash: hash,
      challenger_successor_hash: hash,
    },
    evidence_hash: hash,
  }
  let datum: Data = ct.StepDatum { fraud_prover: prover, data: Some(prepared) }
  let zero: Data = 0
  let transition_data: Data = transition
  let role_data: Data = role
  let action = builtin.constr_data(
    0,
    [zero, zero, transition_data, auxiliary, role_data, zero],
  )
  let output_reference = OutputReference {
    transaction_id: hash,
    output_index: 0,
  }
  Transaction {
    ..placeholder,
    inputs: [
      Input {
        output_reference,
        output: Output {
          address: address.from_script(dispatcher),
          value: assets.from_lovelace(2_000_000),
          datum: InlineDatum(datum),
          reference_script: None,
        },
      },
    ],
    redeemers: [Pair(Spend(output_reference), builtin.constr_data(1, [action]))],
  }
}

fn stage_four_begin_with(tag: Int) -> Bool {
  let (pre, transition, carriage) = stage_four_begin_step()
  let carriage_data: Data = carriage
  yields.stage_four_begin.withdraw(
    [dispatcher],
    #"",
    builtin.i_data(0),
    address.Script(dispatcher),
    dispatch(pre, transition, builtin.constr_data(tag, [carriage_data]), 3),
  )
}

fn stage_six_fold_asset_with(tag: Int) -> Bool {
  let (pre, transition, chunk_proof, next_chunk_proof) = mint_fold_asset_step()
  let chunk_data: Data = chunk_proof
  let next_data: Data = next_chunk_proof
  yields.stage_six_fold_asset.withdraw(
    [dispatcher],
    builtin.i_data(0),
    address.Script(dispatcher),
    dispatch(
      pre,
      transition,
      builtin.constr_data(tag, [chunk_data, next_data]),
      6,
    ),
  )
}

/// `TransactionRedeemerItemBeginWitness`, the stage-four begin evidence.
test stage_four_begin_yield_accepts_its_evidence_constructor() {
  stage_four_begin_with(27)
}

test stage_four_begin_yield_refuses_another_evidence_constructor() fail {
  stage_four_begin_with(29)
}

/// `MintFoldAssetWitness`, the stage-six fold-asset evidence.
test stage_six_fold_asset_yield_accepts_its_evidence_constructor() {
  stage_six_fold_asset_with(35)
}

test stage_six_fold_asset_yield_refuses_another_evidence_constructor() fail {
  stage_six_fold_asset_with(39)
}


// ## Synthetic ledger proofs
//
// The library has no prover, so the root a proof opens is rebuilt by mirroring
// the library's own fold. A ground sibling at every level forces the widest
// `Branch` step, and 64 of them span the whole path.

fn branch_neighbors(seed: Int) -> ByteArray {
  let a = blake2b_256(cbor.serialise(seed))
  let b = blake2b_256(a)
  let c = blake2b_256(b)
  let d = blake2b_256(c)
  a
    |> bytearray.concat(b)
    |> bytearray.concat(c)
    |> bytearray.concat(d)
}

fn branch_steps(count: Int) -> mpf.Proof {
  if count <= 0 {
    []
  } else {
    list.push(
      branch_steps(count - 1),
      mpf.Branch { skip: 0, neighbors: branch_neighbors(count - 1) },
    )
  }
}

/// The root of the trie in which `proof` opens `key` to `value`.
fn membership_root(
  key: ByteArray,
  value: ByteArray,
  cursor: Int,
  proof: mpf.Proof,
) -> ByteArray {
  let path = blake2b_256(key)
  when proof is {
    [] -> combine(suffix(path, cursor), blake2b_256(value))
    [mpf.Branch { skip, neighbors }, ..rest] -> {
      let next_cursor = cursor + 1 + skip
      combine(
        nibbles(path, cursor, next_cursor - 1),
        merkle_16(
          nibble(path, next_cursor - 1),
          membership_root(key, value, next_cursor, rest),
          slice_bytearray(0, 32, neighbors),
          slice_bytearray(32, 32, neighbors),
          slice_bytearray(64, 32, neighbors),
          slice_bytearray(96, 32, neighbors),
        ),
      )
    }
    _ -> fail
  }
}

/// The last input's descriptor with one more lovelace: a well-formed value
/// the prior ledger does not hold.
fn substituted_value(value: ByteArray) -> ByteArray {
  let descriptor = ledger_output_commitment_v1.decode(value)
  ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      ..descriptor,
      lovelace: descriptor.lovelace + 1,
    },
  )
}

// ## Whole-validator harness

/// The yield transaction for one stage-3 replay: a single dispatcher input
/// carrying the prepared state, spent with the `VerifyNonOutput` action shape
/// (`[input, output, transition, auxiliary, role, yield_ref]`).
fn replay_yield_tx(
  pre: validation_trace_v1.ValidationMachineStateV1,
  transition: Data,
  auxiliary: Data,
) -> Transaction {
  let prepared = PreparedValidationResolutionStateV1 {
    version: 1,
    resolution: ValidationResolutionStateV1 {
      version: 1,
      pre_state: pre,
      operator_successor_hash: h_a,
      challenger_successor_hash: h_b,
    },
    evidence_hash: h_a,
  }
  let datum: Data = ct.StepDatum { fraud_prover: prover, data: Some(prepared) }
  let zero: Data = 0
  let role: Data = 1
  let redeemer = builtin.constr_data(
    1,
    [builtin.constr_data(0, [zero, zero, transition, auxiliary, role, zero])],
  )
  let output_reference = OutputReference {
    transaction_id: h_a,
    output_index: 0,
  }
  Transaction {
    ..placeholder,
    inputs: [
      Input {
        output_reference,
        output: Output {
          address: address.from_script(dispatcher),
          value: assets.from_lovelace(2_000_000),
          datum: InlineDatum(datum),
          reference_script: None,
        },
      },
    ],
    redeemers: [Pair(Spend(output_reference), redeemer)],
  }
}

/// The replay of `value` over a ledger whose root is the one `ledger_proof`
/// opens `ledger_value` to, carrying `proof` in the witness.
fn replay_tx(
  ledger_value: ByteArray,
  ledger_proof: mpf.Proof,
  value: ByteArray,
  proof: mpf.Proof,
) -> Transaction {
  let vector = maximum_spend_input_schedule_terminal_vector()
  let root = membership_root(vector.last_key, ledger_value, 0, ledger_proof)
  let (pre, transition, auxiliary) = maximum_input_schedule_replay_step(
    vector,
    root,
    value,
    proof,
  )
  let transition_data: Data = transition
  let auxiliary_data: Data = auxiliary
  replay_yield_tx(pre, transition_data, auxiliary_data)
}

fn run(tx: Transaction) -> Bool {
  yields.stage_three_replay.withdraw(
    [dispatcher],
    builtin.i_data(0),
    address.Script(yield_hash),
    tx,
  )
}

fn last_value() -> ByteArray {
  maximum_spend_input_schedule_terminal_vector().last_value
}

test replay_yield_accepts_the_deepest_ledger_proof() {
  let proof = branch_steps(64)
  and {
    list.length(proof) == 64,
    run(replay_tx(last_value(), proof, last_value(), proof)),
  }
}

/// Twin of the budget row with the empty proof of a one-entry ledger.
test replay_yield_accepts_the_singleton_ledger_proof() {
  and {
    membership_root(
      maximum_spend_input_schedule_terminal_vector().last_key,
      last_value(),
      0,
      [],
    ) == singleton_root(
      maximum_spend_input_schedule_terminal_vector().last_key,
      last_value(),
    ),
    run(replay_tx(last_value(), [], last_value(), [])),
  }
}

/// The budget row's fixture alone, without the validator run.
test replay_yield_fixture_alone() {
  let proof = branch_steps(64)
  let tx = replay_tx(last_value(), proof, last_value(), proof)
  list.length(tx.inputs) == 1
}

test replay_yield_refuses_a_value_the_prior_ledger_does_not_hold() {
  let proof = branch_steps(64)
  !run(replay_tx(last_value(), proof, substituted_value(last_value()), proof))
}

test replay_yield_refuses_a_proof_for_another_ledger() {
  !run(
    replay_tx(last_value(), branch_steps(64), last_value(), branch_steps(63)),
  )
}
