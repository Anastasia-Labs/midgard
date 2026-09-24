use aiken/builtin
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Spend, Transaction,
  placeholder,
}
use midgard/computation_thread as ct
use midgard/fraud_proofs/transition_trace/final_l1_event as handshake
use midgard/fraud_proofs/transition_trace/timed_history

const dispatcher = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const other = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const hash = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

fn raw_proof(fault_tag: Int, witness_tag: Int) -> Data {
  builtin.constr_data(
    0,
    [
      builtin.b_data(hash),
      builtin.i_data(42),
      builtin.constr_data(fault_tag, [builtin.constr_data(witness_tag, [])]),
    ],
  )
}

fn fixture() -> Transaction {
  let datum: Data = ct.StepDatum {
    fraud_prover: other,
    data: Some(raw_proof(5, 0)),
  }
  let witness: Data = 23
  let args = handshake.Args {
    input_index: 0,
    output_index: 1,
    hub_ref_input_index: 2,
    fraud_proof_mint_redeemer_index: 3,
    event_reference: timed_history.Reference {
      order_index: 4,
      external_data_index: Some(5),
    },
    completed_fraud_witness: witness,
    yield_ref_input_index: 6,
  }
  let redeemer: Data = ct.Continue(args)
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

test binds_actual_input_datum_and_continue_arguments() {
  let (input, datum, args) = handshake.dispatch(dispatcher, fixture())
  let witness: Data = 23
  and {
    input.output_reference == OutputReference {
      transaction_id: hash,
      output_index: 0,
    },
    datum.fraud_prover == other,
    datum.data == Some(raw_proof(5, 0)),
    args.input_index == 0,
    args.output_index == 1,
    args.hub_ref_input_index == 2,
    args.fraud_proof_mint_redeemer_index == 3,
    args.event_reference == timed_history.Reference {
      order_index: 4,
      external_data_index: Some(5),
    },
    args.completed_fraud_witness == witness,
    args.yield_ref_input_index == 6,
  }
}

test rejects_missing_dispatcher() fail {
  let actual = handshake.dispatch(other, fixture())
  actual == handshake.dispatch(dispatcher, fixture())
}

test rejects_multiple_dispatcher_inputs() fail {
  let tx = fixture()
  expect [input] = tx.inputs
  let second = Input {
    ..input,
    output_reference: OutputReference { transaction_id: hash, output_index: 1 },
  }
  let actual = handshake.dispatch(
    dispatcher,
    Transaction { ..tx, inputs: [input, second] },
  )
  actual == handshake.dispatch(dispatcher, fixture())
}

test ignores_unrelated_input_and_redeemer() {
  let tx = fixture()
  expect [input] = tx.inputs
  let unrelated = Input {
    output_reference: OutputReference { transaction_id: hash, output_index: 1 },
    output: Output { ..input.output, address: address.from_script(other) },
  }
  let redeemer: Data = 0
  handshake.dispatch(
    dispatcher,
    Transaction {
      ..tx,
      inputs: [unrelated, input],
      redeemers: [
        Pair(Spend(unrelated.output_reference), redeemer),
        ..tx.redeemers
      ],
    },
  ) == handshake.dispatch(dispatcher, tx)
}

test rejects_redeemer_for_different_output_reference() fail {
  let tx = fixture()
  expect [Pair(_, redeemer)] = tx.redeemers
  let actual = handshake.dispatch(
    dispatcher,
    Transaction {
      ..tx,
      redeemers: [
        Pair(
          Spend(OutputReference { transaction_id: hash, output_index: 1 }),
          redeemer,
        ),
      ],
    },
  )
  actual == handshake.dispatch(dispatcher, fixture())
}

test rejects_duplicate_spend_redeemers() fail {
  let tx = fixture()
  expect [redeemer] = tx.redeemers
  let actual = handshake.dispatch(
    dispatcher,
    Transaction { ..tx, redeemers: [redeemer, redeemer] },
  )
  actual == handshake.dispatch(dispatcher, fixture())
}

test rejects_cancel_as_semantic_continuation() fail {
  let tx = fixture()
  expect [Pair(purpose, _)] = tx.redeemers
  let cancel: Data = ct.Cancel {
    input_index: 0,
    computation_thread_mint_redeemer_index: 0,
  }
  let actual = handshake.dispatch(
    dispatcher,
    Transaction { ..tx, redeemers: [Pair(purpose, cancel)] },
  )
  actual == handshake.dispatch(dispatcher, fixture())
}

test requires_actual_inline_datum() fail {
  let tx = fixture()
  expect [input] = tx.inputs
  let missing = Input {
    ..input,
    output: Output { ..input.output, datum: NoDatum },
  }
  let actual = handshake.dispatch(
    dispatcher,
    Transaction { ..tx, inputs: [missing] },
  )
  actual == handshake.dispatch(dispatcher, fixture())
}

test omitted_deposit_selects_history_yield() {
  handshake.role_for(raw_proof(5, 0)) == handshake.yield_role
}

test omitted_withdrawal_selects_history_yield() {
  handshake.role_for(raw_proof(5, 1)) == handshake.yield_role
}

test omitted_forced_selects_forced_yield() {
  handshake.role_for(raw_proof(5, 2)) == handshake.forced_yield_role
}

test outside_deposit_selects_history_yield() {
  handshake.role_for(raw_proof(7, 0)) == handshake.yield_role
}

test outside_withdrawal_selects_history_yield() {
  handshake.role_for(raw_proof(7, 1)) == handshake.yield_role
}

test outside_forced_selects_forced_yield() {
  handshake.role_for(raw_proof(7, 2)) == handshake.forced_yield_role
}

test rejects_unrelated_fault_family() fail {
  let actual = handshake.role_for(raw_proof(6, 0))
  actual == handshake.yield_role
}

test rejects_unknown_event_kind() fail {
  let actual = handshake.role_for(raw_proof(5, 3))
  actual == handshake.yield_role
}

test rejects_wrong_proof_constructor() fail {
  let actual = handshake.role_for(
    builtin.constr_data(1, builtin.unconstr_fields(raw_proof(5, 0))),
  )
  actual == handshake.yield_role
}

test rejects_extra_proof_field() fail {
  let actual = handshake.role_for(
    builtin.constr_data(
      0,
      [builtin.i_data(0), ..builtin.unconstr_fields(raw_proof(5, 0))],
    ),
  )
  actual == handshake.yield_role
}

test rejects_extra_fault_field() fail {
  let actual = handshake.role_for(
    builtin.constr_data(
      0,
      [
        builtin.i_data(0),
        builtin.i_data(0),
        builtin.constr_data(5, [builtin.constr_data(0, []), builtin.i_data(0)]),
      ],
    ),
  )
  actual == handshake.yield_role
}
