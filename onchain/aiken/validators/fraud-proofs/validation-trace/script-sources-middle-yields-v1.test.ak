//// The script-sources middle yields read the auxiliary evidence the
//// non-output semantic resolver carries by its constructor. These vectors
//// drive the stage-four begin and stage-six fold-asset yields through
//// `withdraw` over an honest step, carried under the constructor the
//// evidence type gives each step, and under another constructor.

use aiken/builtin
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Spend, Transaction, placeholder,
}
use fraud_proofs/validation_trace/script_sources_middle_yields_v1 as yields
use midgard/computation_thread as ct
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_machine_tests/fixtures_script_sources.{
  mint_fold_asset_step, stage_four_begin_step,
}
use midgard/validation_resolution_v1.{
  PreparedValidationResolutionStateV1, ValidationResolutionStateV1,
}
use midgard/validation_trace_v1.{ValidationMachineStateV1}

const dispatcher = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const prover = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const hash = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

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
