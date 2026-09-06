use aiken/builtin
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Spend, Transaction, placeholder,
}
use midgard/computation_thread as ct
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_resolution_v1.{
  PreparedValidationResolutionStateV1, ValidationResolutionStateV1,
}
use midgard/validation_semantic_yield_v1 as handshake
use midgard/validation_trace_v1.{
  Normal, Pending, ScriptSources, ValidationMachineStateV1,
}

const dispatcher = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const other = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const hash = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

fn fixture() -> Transaction {
  let state = ValidationMachineStateV1 {
    machine_version: 1,
    event_key_hash: hash,
    transaction_id: hash,
    transaction_commitment: hash,
    validation_context_hash: hash,
    source_kind: Normal,
    prior_ledger_root: hash,
    phase: ScriptSources,
    program_counter: 4,
    work_root: hash,
    execution_cpu: 0,
    execution_memory: 0,
    verdict: Pending,
    rejection_code_hash: hash,
    ledger_delta_root: hash,
  }
  let prepared = PreparedValidationResolutionStateV1 {
    version: 1,
    resolution: ValidationResolutionStateV1 {
      version: 1,
      pre_state: state,
      operator_successor_hash: hash,
      challenger_successor_hash: hash,
    },
    evidence_hash: hash,
  }
  let datum: Data = ct.StepDatum { fraud_prover: other, data: Some(prepared) }
  let transition: Data = ValidationOneStepWitnessV1 {
    work_witness_cbor: #"80",
    claimed_successor: state,
  }
  let zero: Data = 0
  let auxiliary = builtin.constr_data(0, [])
  let action = builtin.constr_data(
    0,
    [zero, zero, transition, auxiliary, zero, zero],
  )
  let redeemer = builtin.constr_data(1, [action])
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
    redeemers: [Pair(Spend(output_reference), redeemer)],
  }
}

test handshake_preserves_the_exact_prepared_transition_and_action() {
  let (state, transition, fields) = handshake.unique_semantic_dispatch_v1(
    [dispatcher],
    fixture(),
  )
  and {
    state.resolution.pre_state.program_counter == 4,
    transition.work_witness_cbor == #"80",
    fields == [builtin.constr_data(0, []), builtin.i_data(0), builtin.i_data(0)],
  }
}

test handshake_refuses_two_dispatcher_inputs() fail {
  let tx = fixture()
  expect [input] = tx.inputs
  expect _ = handshake.unique_semantic_dispatch_v1(
    [dispatcher],
    Transaction {
      ..tx,
      inputs: [
        input,
        Input {
          ..input,
          output_reference: OutputReference {
            transaction_id: hash,
            output_index: 1,
          },
        },
      ],
    },
  )
}

test handshake_refuses_a_foreign_dispatcher() fail {
  expect _ = handshake.unique_semantic_dispatch_v1([other], fixture())
}

test handshake_refuses_missing_spend_redeemer() fail {
  expect _ = handshake.unique_semantic_dispatch_v1(
    [dispatcher],
    Transaction { ..fixture(), redeemers: [] },
  )
}

test handshake_refuses_duplicate_spend_redeemer() fail {
  let tx = fixture()
  expect [redeemer] = tx.redeemers
  expect _ = handshake.unique_semantic_dispatch_v1(
    [dispatcher],
    Transaction { ..tx, redeemers: [redeemer, redeemer] },
  )
}

test handshake_refuses_cancel_as_a_semantic_action() fail {
  let tx = fixture()
  expect [Pair(purpose, _)] = tx.redeemers
  expect _ = handshake.unique_semantic_dispatch_v1(
    [dispatcher],
    Transaction { ..tx, redeemers: [Pair(purpose, builtin.constr_data(0, []))] },
  )
}
