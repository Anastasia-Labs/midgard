//// Family evidence adversaries and independent wire measurements for the
//// sorted authenticated history. These are unit vectors; applied transaction
//// and contention acceptance remains a separate gate.

use aiken/cbor
use aiken/interval
use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use midgard/common/utils
use midgard/event_history/node_role
use midgard/event_history/order_facts
use midgard/event_history/ordering
use midgard/event_history/proof_evidence
use midgard/event_history/proof_fixture as fixture
use midgard/fraud_proofs/fabricated_deposit/step_01 as d1
use midgard/fraud_proofs/fabricated_deposit/step_02 as d2
use midgard/fraud_proofs/fabricated_deposit/step_03 as d3
use midgard/fraud_proofs/fabricated_deposit/step_04 as d4
use midgard/fraud_proofs/fabricated_withdrawal/step_01 as w1
use midgard/fraud_proofs/fabricated_withdrawal/step_02 as w2
use midgard/fraud_proofs/fabricated_withdrawal/step_03 as w3
use midgard/fraud_proofs/fabricated_withdrawal/step_04 as w4
use midgard/fraud_proofs/native_binding_fixture_v1 as native
use midgard/ledger_state
use midgard/user_events
use midgard/user_events/deposit
use midgard/user_events/withdrawal

fn evidence_tx(reference: Input) -> Transaction {
  Transaction {
    ..fixture.after_header(transaction.placeholder),
    reference_inputs: [reference],
  }
}

fn deposit_payload() -> order_facts.Payload {
  order_facts.DepositPayload {
    event: ledger_state.DepositEvent {
      id: d1.authentic_deposit_id_v1(),
      info: d1.authentic_deposit_info_v1(),
    },
  }
}

fn deposit_reference() -> Input {
  fixture.order_reference(
    d2.fixture_deposit_event_policy_v1(),
    user_events.out_ref_to_nonce(d1.authentic_deposit_id_v1()),
    deposit_payload(),
    15,
  )
}

fn capture(tx: Transaction) -> order_facts.Commitment {
  let policy = d2.fixture_deposit_event_policy_v1()
  proof_evidence.presence(
    tx,
    policy,
    address.from_script(policy),
    order_facts.Deposit,
    d1.authentic_deposit_id_v1(),
    20,
    fixture.parameters(),
    0,
    None,
  )
}

fn absence(tx: Transaction) -> Bool {
  let policy = d2.fixture_deposit_event_policy_v1()
  proof_evidence.absence(
    tx,
    policy,
    address.from_script(policy),
    d1.authentic_deposit_id_v1(),
    20,
    0,
  )
}

test arbitrary_id_absent_from_authenticated_empty_root() {
  let policy = d2.fixture_deposit_event_policy_v1()
  let source = fixture.absent_reference(policy, d1.authentic_deposit_id_v1())
  let content: Data = node_role.RootContent
  let root: ordering.Link<Data> = ordering.Link {
    position: ordering.Root,
    next: None,
    protected_until: 100000,
    payload: content,
  }
  let reference = Input {
    ..source,
    output: Output {
      ..source.output,
      value: assets.from_lovelace(3000000) |> assets.add(policy, #"", 1),
      datum: InlineDatum(root),
    },
  }
  absence(evidence_tx(reference))
}

test exact_filler_is_absence() {
  absence(
    evidence_tx(
      fixture.absent_reference(
        d2.fixture_deposit_event_policy_v1(),
        d1.authentic_deposit_id_v1(),
      ),
    ),
  )
}

test genuine_order_cannot_establish_absence() {
  !absence(evidence_tx(deposit_reference()))
}

test live_nonce_without_history_token_cannot_establish_absence() fail {
  absence(
    evidence_tx(
      d1.unspent_output_reference_input_v1(d1.authentic_deposit_id_v1()),
    ),
  )
}

test capture_rejects_spent_reference() fail {
  let reference = deposit_reference()
  capture(Transaction { ..evidence_tx(reference), inputs: [reference] }).inclusion_time == 15
}

test absence_rejects_spent_reference() fail {
  let reference = fixture.absent_reference(
    d2.fixture_deposit_event_policy_v1(),
    d1.authentic_deposit_id_v1(),
  )
  absence(Transaction { ..evidence_tx(reference), inputs: [reference] })
}

test capture_rejects_at_header_end() fail {
  capture(
    Transaction {
      ..evidence_tx(deposit_reference()),
      validity_range: interval.between(20, 21),
    },
  ).inclusion_time == 15
}

test capture_rejects_at_merge_maturity() fail {
  capture(
    Transaction {
      ..evidence_tx(deposit_reference()),
      validity_range: interval.between(
        21,
        20 + ledger_state.block_maturity_duration_v1,
      ),
    },
  ).inclusion_time == 15
}

test absence_rejects_at_merge_maturity() fail {
  let reference = fixture.absent_reference(
    d2.fixture_deposit_event_policy_v1(),
    d1.authentic_deposit_id_v1(),
  )
  absence(
    Transaction {
      ..evidence_tx(reference),
      validity_range: interval.between(
        21,
        20 + ledger_state.block_maturity_duration_v1,
      ),
    },
  )
}

test immutable_commitment_survives_pointer_continuation() {
  let reference = deposit_reference()
  let before = ordering.authenticate(
    reference.output,
    d2.fixture_deposit_event_policy_v1(),
    reference.output.address,
  )
  let after: ordering.Link<Data> = ordering.Link {
    ..before,
    next: Some(
      #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    ),
    protected_until: 200000,
  }
  let continuation = Input {
    ..reference,
    output: Output { ..reference.output, datum: InlineDatum(after) },
  }
  capture(evidence_tx(reference)) == capture(evidence_tx(continuation))
}

fn deposit_datum(time: Int) -> deposit.DepositDatum {
  d2.authentic_deposit_datum_v1(
    d1.authentic_deposit_id_v1(),
    d1.authentic_deposit_info_v1(),
    time,
  )
}

fn deposit_state(time: Int) -> d3.State {
  d3.fixture_state_v1(
    d2.fixture_state_v1(d1.authentic_deposit_block_v1()),
    d2.observed_verdict_v1(deposit_datum(time)),
  )
}

fn open_deposit(
  time: Int,
  original_assets: assets.Value,
) -> d4.FabricatedDepositFaultV1 {
  d3.open_authentic_deposit_content_v1(
    deposit_state(time),
    d3.RetainedEventData {
      payload: d2.history_payload(deposit_datum(time)),
      original_assets,
    },
  )
}

test deposit_equal_content_before_window_is_timing_fault() {
  open_deposit(10, fixture.original_assets()) == d4.IneligibleDepositEvent {
    event_inclusion_time: 10,
  }
}

test deposit_equal_content_after_window_is_timing_fault() {
  open_deposit(21, fixture.original_assets()) == d4.IneligibleDepositEvent {
    event_inclusion_time: 21,
  }
}

test deposit_honest_eligible_equal_content_cannot_be_convicted() fail {
  open_deposit(15, fixture.original_assets()) == d4.NonexistentDepositIdentity
}

test deposit_original_value_cannot_be_replaced_by_prover() fail {
  open_deposit(10, assets.from_lovelace(1)) == d4.NonexistentDepositIdentity
}

test deposit_timing_rule_requires_actual_ineligibility() {
  let base = d3.fixture_next_state_v1(
    deposit_state(15),
    d4.IneligibleDepositEvent { event_inclusion_time: 15 },
  )
  !d4.fabricated_deposit_fault_is_established_v1(base)
}

test deposit_history_wire_measurement() {
  let absent02 = d2.fixture_state_v1(d1.fabricated_identity_block_v1())
  let absent03 = d3.fixture_state_v1(absent02, d2.DepositIdentityAbsent)
  let absent04 = d3.fixture_next_state_v1(
    absent03,
    d4.NonexistentDepositIdentity,
  )
  let present02 = d2.fixture_state_v1(d1.mismatched_content_block_v1())
  let observed = d2.observed_verdict_v1(deposit_datum(15))
  let present03 = d3.fixture_state_v1(present02, observed)
  let opening = d3.RetainedEventData {
    payload: d2.history_payload(deposit_datum(15)),
    original_assets: fixture.original_assets(),
  }
  let fault = d3.open_authentic_deposit_content_v1(present03, opening)
  let present04 = d3.fixture_next_state_v1(present03, fault)
  trace @"deposit_fi02": cbor.serialise(absent02)
  trace @"deposit_fi03": cbor.serialise(absent03)
  trace @"deposit_fi04": cbor.serialise(absent04)
  trace @"deposit_mm02": cbor.serialise(present02)
  trace @"deposit_mm03": cbor.serialise(present03)
  trace @"deposit_mm04": cbor.serialise(present04)
  trace @"deposit_observed": cbor.serialise(observed)
  trace @"deposit_opening": cbor.serialise(opening)
  d4.fabricated_deposit_fault_is_established_v1(present04)
}

fn withdrawal_datum(time: Int) -> withdrawal.Datum {
  w2.authentic_withdrawal_datum_v1(
    w1.authentic_withdrawal_id_v1(),
    w1.authentic_withdrawal_info_v1(),
    time,
  )
}

fn withdrawal_state(time: Int) -> w3.State {
  w3.fixture_state_v1(
    w2.fixture_state_v1(w1.authentic_withdrawal_block_v1()),
    w2.observed_verdict_v1(withdrawal_datum(time)),
  )
}

fn open_withdrawal(
  time: Int,
  original_assets: assets.Value,
) -> w4.FabricatedWithdrawalFaultV1 {
  w3.open_authentic_withdrawal_content_v1(
    withdrawal_state(time),
    w3.RetainedEventData {
      payload: w2.history_payload(withdrawal_datum(time)),
      original_assets,
    },
  )
}

test withdrawal_equal_content_before_window_is_timing_fault() {
  open_withdrawal(10, fixture.original_assets()) == w4.IneligibleWithdrawalEvent {
    event_inclusion_time: 10,
  }
}

test withdrawal_equal_content_after_window_is_timing_fault() {
  open_withdrawal(21, fixture.original_assets()) == w4.IneligibleWithdrawalEvent {
    event_inclusion_time: 21,
  }
}

test withdrawal_honest_eligible_equal_content_cannot_be_convicted() fail {
  open_withdrawal(15, fixture.original_assets()) == w4.NonexistentWithdrawalIdentity
}

test withdrawal_original_value_cannot_be_replaced_by_prover() fail {
  open_withdrawal(10, assets.from_lovelace(1)) == w4.NonexistentWithdrawalIdentity
}

test withdrawal_timing_rule_requires_actual_ineligibility() {
  let base = w3.fixture_next_state_v1(
    withdrawal_state(15),
    w4.IneligibleWithdrawalEvent { event_inclusion_time: 15 },
  )
  !w4.fabricated_withdrawal_fault_is_established_v1(base)
}

test withdrawal_history_wire_measurement() {
  let absent02 = w2.fixture_state_v1(w1.fabricated_identity_block_v1())
  let absent03 = w3.fixture_state_v1(absent02, w2.WithdrawalIdentityAbsent)
  let absent04 = w3.fixture_next_state_v1(
    absent03,
    w4.NonexistentWithdrawalIdentity,
  )
  let present02 = w2.fixture_state_v1(w1.mismatched_content_block_v1())
  let observed = w2.observed_verdict_v1(withdrawal_datum(15))
  let present03 = w3.fixture_state_v1(present02, observed)
  let opening = w3.RetainedEventData {
    payload: w2.history_payload(withdrawal_datum(15)),
    original_assets: fixture.original_assets(),
  }
  let fault = w3.open_authentic_withdrawal_content_v1(present03, opening)
  let present04 = w3.fixture_next_state_v1(present03, fault)
  trace @"withdrawal_fi02": cbor.serialise(absent02)
  trace @"withdrawal_fi03": cbor.serialise(absent03)
  trace @"withdrawal_fi04": cbor.serialise(absent04)
  trace @"withdrawal_mm02": cbor.serialise(present02)
  trace @"withdrawal_mm03": cbor.serialise(present03)
  trace @"withdrawal_mm04": cbor.serialise(present04)
  trace @"withdrawal_observed": cbor.serialise(observed)
  trace @"withdrawal_opening": cbor.serialise(opening)
  w4.fabricated_withdrawal_fault_is_established_v1(present04)
}

test deposit_malformed_value_data_cannot_open_captured_value() fail {
  d3.open_authentic_deposit_content_v1(
    deposit_state(10),
    d3.RetainedEventData {
      payload: d2.history_payload(deposit_datum(10)),
      original_assets: 42,
    },
  ) == d4.IneligibleDepositEvent { event_inclusion_time: 10 }
}

test withdrawal_malformed_value_data_cannot_open_captured_value() fail {
  w3.open_authentic_withdrawal_content_v1(
    withdrawal_state(10),
    w3.RetainedEventData {
      payload: w2.history_payload(withdrawal_datum(10)),
      original_assets: 42,
    },
  ) == w4.IneligibleWithdrawalEvent { event_inclusion_time: 10 }
}
