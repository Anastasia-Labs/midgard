//// Staged transition source openings use original assets, never the pointer
//// UTxO, structural funding, or an operator-authored event archive.

use aiken/builtin
use aiken/cbor
use cardano/address
use cardano/assets.{Value}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/event_history/node_role
use midgard/event_history/order_facts
use midgard/event_history/ordering
use midgard/event_history/proof_fixture as fixture
use midgard/event_history/retained_data
use midgard/fraud_proofs/fabricated_deposit/step_01 as deposit
use midgard/fraud_proofs/fabricated_deposit/step_02 as deposit_evidence
use midgard/fraud_proofs/native_binding_fixture_v1 as native
use midgard/fraud_proofs/transition_trace/deposit_source
use midgard/fraud_proofs/transition_trace/final_yield
use midgard/fraud_proofs/transition_trace/proof
use midgard/fraud_proofs/transition_trace/proof_carriage
use midgard/ledger_state
use midgard/user_events

const asset_policy = #"31313131313131313131313131313131313131313131313131313131"

fn policy() -> ByteArray {
  deposit_evidence.fixture_deposit_event_policy_v1()
}

fn payload() -> order_facts.Payload {
  order_facts.DepositPayload {
    event: deposit_evidence.authentic_deposit_datum_v1(
      deposit.authentic_deposit_id_v1(),
      deposit.authentic_deposit_info_v1(),
      15,
    ).event,
  }
}

fn original() -> Value {
  assets.from_lovelace(3_000_000) |> assets.add(asset_policy, #"01", 17)
}

fn reference() -> Input {
  let initial = fixture.order_reference(
    policy(),
    user_events.out_ref_to_nonce(deposit.authentic_deposit_id_v1()),
    payload(),
    15,
  )
  let node = ordering.authenticate(
    initial.output,
    policy(),
    initial.output.address,
  )
  let facts = order_facts.Facts {
    ..order_facts.from_node(node),
    structural_lovelace: 2_000_000,
  }
  let raw: Data = facts
  let content: Data = node_role.Order { facts: raw }
  let next: ordering.Link<Data> = ordering.Link { ..node, payload: content }
  Input {
    ..initial,
    output: Output {
      ..initial.output,
      datum: InlineDatum(next),
      value: original()
        |> assets.add(#"", #"", 2_000_000)
        |> assets.add(policy(), user_events.out_ref_to_nonce(facts.event_id), 1),
    },
  }
}

fn capture(
  references: List<Input>,
  external: Option<Int>,
) -> (order_facts.Commitment, order_facts.Payload, Value) {
  let params = fixture.parameters()
  order_facts.capture_open(
    Transaction {
      ..fixture.after_header(transaction.placeholder),
      reference_inputs: references,
    },
    policy(),
    address.from_script(policy()),
    params.retention_address,
    order_facts.Deposit,
    deposit.authentic_deposit_id_v1(),
    20,
    params.inline_limit,
    params.max_payload_bytes,
    params.max_payload_nodes,
    0,
    external,
  )
}

test captured_opening_contains_only_original_assets() {
  let (retained, opened, value) = capture([reference()], None)
  and {
    value == original(),
    opened == payload(),
    retained.inclusion_time == 15,
    order_facts.reopen(retained, opened, value) == original(),
  }
}

test staged_opening_survives_pointer_and_output_reference_changes() {
  let before = reference()
  let node = ordering.authenticate(
    before.output,
    policy(),
    before.output.address,
  )
  let next: ordering.Link<Data> = ordering.Link {
    ..node,
    next: Some(
      #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    ),
    protected_until: node.protected_until + 1000,
  }
  let after = Input {
    output_reference: OutputReference {
      transaction_id: #"abababababababababababababababababababababababababababababababab",
      output_index: 7,
    },
    output: Output { ..before.output, datum: InlineDatum(next) },
  }
  let (retained, opened, value) = capture([before], None)
  and {
    capture([after], None) == (retained, opened, value),
    order_facts.reopen(retained, opened, value) == original(),
  }
}

test refuses_structural_lovelace_as_deposited_value() fail {
  let (retained, opened, value) = capture([reference()], None)
  order_facts.reopen(retained, opened, assets.add(value, #"", #"", 2_000_000)) == value
}

test refuses_history_nft_as_deposited_value() fail {
  let (retained, opened, value) = capture([reference()], None)
  order_facts.reopen(
    retained,
    opened,
    assets.add(
      value,
      policy(),
      user_events.out_ref_to_nonce(retained.event_id),
      1,
    ),
  ) == value
}

test refuses_changed_native_asset_quantity() fail {
  let (retained, opened, value) = capture([reference()], None)
  order_facts.reopen(
    retained,
    opened,
    assets.add(value, asset_policy, #"01", 1),
  ) == value
}

test refuses_changed_deposit_projection_fields() fail {
  let (retained, opened, value) = capture([reference()], None)
  expect order_facts.DepositPayload { event } = opened
  let forged_data: Data = 42
  let forged = order_facts.DepositPayload {
    event: ledger_state.DepositEvent {
      ..event,
      info: ledger_state.DepositInfo {
        ..event.info,
        l2_datum: Some(forged_data),
      },
    },
  }
  order_facts.reopen(retained, forged, value) == value
}

test external_capture_retains_complete_authenticated_opening() {
  let before = reference()
  let node = ordering.authenticate(
    before.output,
    policy(),
    before.output.address,
  )
  let facts = order_facts.from_node(node)
  let retained = retained_data.EventData {
    event_key: user_events.out_ref_to_nonce(facts.event_id),
    event_payload: payload(),
    reclaim_auth: address.VerificationKey(native.fraud_prover_key_hash),
  }
  let external_facts = order_facts.Facts {
    ..facts,
    location: order_facts.External {
      storage_datum_hash: retained_data.datum_hash(retained),
    },
  }
  let raw: Data = external_facts
  let content: Data = node_role.Order { facts: raw }
  let external_node: ordering.Link<Data> = ordering.Link {
    ..node,
    payload: content,
  }
  let external = Input {
    output_reference: OutputReference {
      transaction_id: #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd",
      output_index: 0,
    },
    output: Output {
      address: fixture.parameters().retention_address,
      value: assets.from_lovelace(3_000_000),
      datum: InlineDatum(retained),
      reference_script: None,
    },
  }
  let (commitment, opened, value) = capture(
    [
      Input {
        ..before,
        output: Output { ..before.output, datum: InlineDatum(external_node) },
      },
      external,
    ],
    Some(1),
  )
  let (inline_commitment, _, _) = capture([before], None)
  and {
    commitment == inline_commitment,
    order_facts.reopen(commitment, opened, value) == original(),
  }
}

fn staged_state(retained: order_facts.Commitment) -> final_yield.State {
  final_yield.State {
    ..final_yield.initial(1, proof_carriage.Commitment { hash: #"" }),
    phase: 7,
    deposit_source_cbor: cbor.serialise(retained),
  }
}

fn staged_args(opening: Option<order_facts.Opening>) -> final_yield.Args {
  final_yield.Args {
    input_index: 0,
    output_index: 0,
    hub_ref_input_index: 0,
    fraud_proof_mint_redeemer_index: 0,
    yield_ref_input_indices: [],
    proof_ref_indices: [],
    output_ref_indices: [],
    deposit_event_ref_index: 999,
    deposit_external_ref_index: None,
    completed_fraud_witness: None,
    deposit_opening: when opening is {
      None -> None
      Some(value) -> {
        let raw: Data = value
        Some(raw)
      }
    },
  }
}

fn original_data() -> Data {
  let data: Data = original()
  data
}

test staged_deposit_reopens_without_any_live_reference() {
  let (retained, payload, value) = capture([reference()], None)
  let source = deposit_source.open(
    staged_state(retained),
    staged_args(Some(order_facts.Opening { payload, original_assets: value })),
  )
  let projected = proof.projected_original_deposit_output(
    source.event.info,
    order_facts.reopen(source.commitment, payload, source.value),
  )
  and {
    source.value == original_data(),
    source.commitment == retained,
    projected.value.lovelace == 3_000_000,
    projected.value.assets == [
      Pair(#"3131313131313131313131313131313131313131313131313131313101", 17),
    ],
  }
}

test staged_deposit_requires_the_retained_opening() fail {
  let (retained, _, _) = capture([reference()], None)
  deposit_source.open(staged_state(retained), staged_args(None)).value == original_data()
}

test staged_deposit_rejects_an_old_outref_policy_asset_receipt() fail {
  let (retained, payload, value) = capture([reference()], None)
  let out_ref: Data = reference().output_reference
  let legacy: Data = [
    out_ref,
    builtin.b_data(policy()),
    builtin.b_data(user_events.out_ref_to_nonce(retained.event_id)),
  ]
  let state = final_yield.State {
    ..staged_state(retained),
    deposit_source_cbor: cbor.serialise(legacy),
  }
  deposit_source.open(
    state,
    staged_args(Some(order_facts.Opening { payload, original_assets: value })),
  ).value == original_data()
}

test staged_deposit_rejects_malformed_deferred_opening() fail {
  let (retained, _, _) = capture([reference()], None)
  let malformed: Data = 42
  let args = final_yield.Args {
    ..staged_args(None),
    deposit_opening: Some(malformed),
  }
  deposit_source.open(staged_state(retained), args).value == original_data()
}

test staged_deposit_rejects_withdrawal_kind_commitment() fail {
  let (retained, payload, value) = capture([reference()], None)
  let wrong_kind = order_facts.Commitment {
    ..retained,
    kind: order_facts.Withdrawal,
  }
  deposit_source.open(
    staged_state(wrong_kind),
    staged_args(Some(order_facts.Opening { payload, original_assets: value })),
  ).value == original_data()
}
