use aiken/cbor
use aiken/collection/list
use aiken/interval
use aiken/primitive/bytearray
use cardano/address.{VerificationKey}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use env
use midgard/event_history/node_role.{Order}
use midgard/event_history/order_facts.{
  Deposit, DepositPayload, External, Facts, Inline, Payload,
}
use midgard/event_history/ordering.{Key, Link}
use midgard/event_history/retained_data.{EventData}
use midgard/ledger_state.{
  DepositEvent, DepositInfo, WithdrawalBody, WithdrawalEvent, WithdrawalInfo,
  WithdrawalIsValid,
}
use midgard/user_events

const policy = #"11111111111111111111111111111111111111111111111111111111"

const retention = #"22222222222222222222222222222222222222222222222222222222"

const owner = #"33333333333333333333333333333333333333333333333333333333"

const tx_id =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const nonce = OutputReference { transaction_id: tx_id, output_index: 0 }

fn payload() -> Payload {
  DepositPayload {
    event: DepositEvent {
      id: nonce,
      info: DepositInfo {
        l2_address: address.from_verification_key(owner),
        l2_network_id: 0,
        l2_datum: None,
      },
    },
  }
}

fn facts() -> Facts {
  Facts {
    event_id: nonce,
    inclusion_time: 400 + env.event_wait_duration,
    location: Inline { payload: payload() },
    structural_lovelace: 3_000_000,
    structural_refund_key: owner,
  }
}

fn order_output(
  facts: Facts,
  next: Option<ByteArray>,
  protected_until: Int,
) -> Output {
  let raw_facts: Data = facts
  let content: Data = Order { facts: raw_facts }
  let raw: Data = Link {
    position: Key(user_events.out_ref_to_nonce(nonce)),
    next,
    protected_until,
    payload: content,
  }
  Output {
    address: address.from_script(policy),
    value: assets.from_lovelace(10_000_000)
      |> assets.add(policy, user_events.out_ref_to_nonce(nonce), 1),
    datum: InlineDatum(raw),
    reference_script: None,
  }
}

fn admission(facts: Facts) -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [
      Input {
        output_reference: nonce,
        output: Output {
          address: address.from_verification_key(owner),
          value: assets.from_lovelace(20_000_000),
          datum: NoDatum,
          reference_script: None,
        },
      },
    ],
    outputs: [order_output(facts, None, 600)],
    validity_range: interval.between(300, 400),
  }
}

fn admit(
  tx: Transaction,
  limit: Int,
  external_index: Option<Int>,
) -> (Facts, Payload, assets.Value) {
  order_facts.admit(
    Deposit,
    tx,
    policy,
    address.from_script(policy),
    address.from_script(retention),
    limit,
    15000,
    1024,
    0,
    0,
    external_index,
  )
}

test admission_binds_nonce_time_payload_and_actual_original_assets() {
  let (actual_facts, actual_payload, actual_assets) = admit(
    admission(facts()),
    1000,
    None,
  )
  and {
    actual_facts == facts(),
    actual_payload == payload(),
    actual_assets == assets.from_lovelace(7_000_000),
  }
}

/// The admission fixture with only the deposit's L2 network id replaced.
fn admit_with_l2_network_id(l2_network_id: Int) -> Payload {
  expect DepositPayload { event } = payload()
  let changed = DepositPayload {
    event: DepositEvent {
      ..event,
      info: DepositInfo { ..event.info, l2_network_id },
    },
  }
  let (_, admitted, _) = admit(
    admission(Facts { ..facts(), location: Inline { payload: changed } }),
    1000,
    None,
  )
  expect admitted == changed
  admitted
}

test admission_accepts_testnet_l2_network_id() {
  expect DepositPayload { event } = admit_with_l2_network_id(0)
  event.info.l2_network_id == 0
}

test admission_accepts_mainnet_l2_network_id() {
  expect DepositPayload { event } = admit_with_l2_network_id(1)
  event.info.l2_network_id == 1
}

test admission_rejects_l2_network_id_two() fail {
  expect _ = admit_with_l2_network_id(2)
  True
}

test admission_rejects_negative_l2_network_id() fail {
  expect _ = admit_with_l2_network_id(-1)
  True
}

test admission_rejects_large_l2_network_id() fail {
  expect _ = admit_with_l2_network_id(18_446_744_073_709_551_616)
  True
}

test admission_rejects_inline_payload_with_too_many_data_nodes() fail {
  expect DepositPayload { event } = payload()
  let wide: Data = list.repeat(0, 1024)
  let info = DepositInfo { ..event.info, l2_datum: Some(wide) }
  let changed = Facts {
    ..facts(),
    location: Inline {
      payload: DepositPayload { event: DepositEvent { ..event, info } },
    },
  }
  expect _ = admit(admission(changed), 15_000, None)
  True
}

test open_payload_rejects_total_byte_limit_even_below_inline_limit() fail {
  expect _ = order_facts.open_payload(
    facts(),
    placeholder,
    address.from_script(retention),
    1000,
    1,
    1024,
    None,
  )
  True
}

test admission_rejects_backdated_inclusion() fail {
  expect _ = admit(
    admission(Facts { ..facts(), inclusion_time: facts().inclusion_time - 1 }),
    1000,
    None,
  )
  True
}

test admission_cannot_use_a_different_nonce() fail {
  let tx = admission(facts())
  expect [original] = tx.inputs
  let changed = Input {
    ..original,
    output_reference: OutputReference { transaction_id: tx_id, output_index: 1 },
  }
  expect _ = admit(Transaction { ..tx, inputs: [changed] }, 1000, None)
  True
}

test inline_exact_serialized_boundary_succeeds() {
  let size = bytearray.length(cbor.serialise(payload()))
  let (_, actual, _) = admit(admission(facts()), size, None)
  actual == payload()
}

test inline_one_byte_over_bound_rejects() fail {
  let size = bytearray.length(cbor.serialise(payload()))
  expect _ = admit(admission(facts()), size - 1, None)
  True
}

fn data_output() -> Input {
  let event_payload: Data = payload()
  let data = EventData {
    event_key: user_events.out_ref_to_nonce(nonce),
    event_payload,
    reclaim_auth: VerificationKey(owner),
  }
  let raw: Data = data
  Input {
    output_reference: OutputReference { transaction_id: tx_id, output_index: 2 },
    output: Output {
      address: address.from_script(retention),
      value: assets.from_lovelace(50_000_000),
      datum: InlineDatum(raw),
      reference_script: None,
    },
  }
}

fn external_facts() -> Facts {
  expect InlineDatum(raw) = data_output().output.datum
  expect data: EventData = raw
  Facts {
    ..facts(),
    location: External { storage_datum_hash: retained_data.datum_hash(data) },
  }
}

test prepublication_is_separate_from_admission_time_and_value() {
  let tx = admission(external_facts())
  let (actual, opened, deposited) = admit(
    Transaction { ..tx, reference_inputs: [data_output()] },
    1,
    Some(0),
  )
  and {
    actual.inclusion_time == facts().inclusion_time,
    opened == payload(),
    deposited == assets.from_lovelace(7_000_000),
  }
}

test external_admission_cannot_use_a_new_sibling_output() fail {
  let tx = admission(external_facts())
  expect [order] = tx.outputs
  expect _ = admit(
    Transaction { ..tx, outputs: [order, data_output().output] },
    1,
    Some(0),
  )
  True
}

test external_admission_rejects_hash_only_promise() fail {
  expect _ = admit(admission(external_facts()), 1, None)
  True
}

test external_admission_cannot_consume_its_publication() fail {
  let tx = admission(external_facts())
  expect [nonce_input] = tx.inputs
  expect _ = admit(
    Transaction {
      ..tx,
      inputs: [nonce_input, data_output()],
      reference_inputs: [data_output()],
    },
    1,
    Some(0),
  )
  True
}

fn capture_tx(output: Output, out_index: Int) -> Transaction {
  Transaction {
    ..placeholder,
    reference_inputs: [
      Input {
        output_reference: OutputReference {
          transaction_id: tx_id,
          output_index: out_index,
        },
        output,
      },
    ],
    validity_range: interval.between(60_501, 60_600),
  }
}

fn capture(tx: Transaction) -> order_facts.Commitment {
  order_facts.capture(
    tx,
    policy,
    address.from_script(policy),
    address.from_script(retention),
    Deposit,
    nonce,
    60_500,
    1000,
    15000,
    1024,
    0,
    None,
  )
}

test staged_opening_preserves_original_value_after_pointer_continuation() {
  let initial = capture(capture_tx(order_output(facts(), None, 600), 3))
  let rewritten = capture(
    capture_tx(
      order_output(
        facts(),
        Some(
          #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        ),
        60_700,
      ),
      4,
    ),
  )
  and {
    initial == rewritten,
    order_facts.opens(initial, payload(), assets.from_lovelace(7_000_000)),
    !order_facts.opens(initial, payload(), assets.from_lovelace(7_000_001)),
  }
}

test staged_opening_rejects_substituted_payload() {
  let initial = capture(capture_tx(order_output(facts(), None, 600), 3))
  let different = DepositPayload {
    event: DepositEvent {
      id: nonce,
      info: DepositInfo {
        l2_address: address.from_verification_key(owner),
        l2_network_id: 1,
        l2_datum: None,
      },
    },
  }
  !order_facts.opens(initial, different, assets.from_lovelace(7_000_000))
}

test capture_before_header_horizon_rejects() fail {
  let tx = capture_tx(order_output(facts(), None, 600), 3)
  expect _ = capture(
    Transaction { ..tx, validity_range: interval.between(60_500, 60_600) },
  )
  True
}

fn withdrawal_payload() -> Payload {
  order_facts.WithdrawalPayload {
    event: WithdrawalEvent {
      id: nonce,
      info: WithdrawalInfo {
        body: WithdrawalBody {
          l2_outref: nonce,
          l2_owner: owner,
          l2_value: [Pair(#"", [Pair(#"", 20_000_000)])],
          l1_address: address.from_verification_key(owner),
          l1_datum: NoDatum,
        },
        signature: Pair(
          #"5555555555555555555555555555555555555555555555555555555555555555",
          #"6666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666",
        ),
        validity: WithdrawalIsValid,
      },
    },
    refund_address: address.from_verification_key(owner),
    refund_datum: NoDatum,
  }
}

fn withdrawal_facts() -> Facts {
  Facts {
    ..facts(),
    location: Inline { payload: withdrawal_payload() },
    structural_lovelace: 0,
  }
}

fn admit_withdrawal(tx: Transaction) -> (Facts, Payload, assets.Value) {
  order_facts.admit(
    order_facts.Withdrawal,
    tx,
    policy,
    address.from_script(policy),
    address.from_script(retention),
    1000,
    15000,
    1024,
    0,
    0,
    None,
  )
}

test withdrawal_preserves_submitted_body_and_all_locked_order_ada() {
  let (_, submitted, locked) = admit_withdrawal(admission(withdrawal_facts()))
  and {
    submitted == withdrawal_payload(),
    locked == assets.from_lovelace(10_000_000),
  }
}

test withdrawal_cannot_redirect_locked_funding_into_deposit_structural_refund() fail {
  expect _ = admit_withdrawal(
    admission(Facts { ..withdrawal_facts(), structural_lovelace: 1 }),
  )
  True
}

test withdrawal_cannot_carry_extra_native_assets() fail {
  let tx = admission(withdrawal_facts())
  expect [output] = tx.outputs
  expect _ = admit_withdrawal(
    Transaction {
      ..tx,
      outputs: [
        Output { ..output, value: output.value |> assets.add(owner, #"01", 1) },
      ],
    },
  )
  True
}

test withdrawal_does_not_authenticate_a_deposit_payload() fail {
  expect _ = admit_withdrawal(
    admission(Facts { ..facts(), structural_lovelace: 0 }),
  )
  True
}

test operator_verdict_is_not_a_mutation_of_the_submitted_withdrawal() {
  let submitted = withdrawal_payload()
  expect order_facts.WithdrawalPayload { event, refund_address, refund_datum } = submitted
  let classified = order_facts.WithdrawalPayload {
    event: WithdrawalEvent {
      ..event,
      info: WithdrawalInfo {
        ..event.info,
        validity: ledger_state.IncorrectWithdrawalSignature,
      },
    },
    refund_address,
    refund_datum,
  }
  let original = order_facts.commitment(
    policy,
    order_facts.Withdrawal,
    withdrawal_facts(),
    submitted,
    assets.from_lovelace(10_000_000),
  )
  and {
    order_facts.opens(original, submitted, assets.from_lovelace(10_000_000)),
    !order_facts.opens(original, classified, assets.from_lovelace(10_000_000)),
    order_facts.withdrawal_info_matches(
      submitted,
      WithdrawalInfo {
        ..event.info,
        validity: ledger_state.IncorrectWithdrawalSignature,
      },
    ),
    !order_facts.withdrawal_info_matches(
      submitted,
      WithdrawalInfo { ..event.info, signature: Pair(#"00", #"01") },
    ),
  }
}

test admission_does_not_accept_a_later_operator_verdict_as_initial_marker() fail {
  expect order_facts.WithdrawalPayload { event, refund_address, refund_datum } = withdrawal_payload()
  let classified = order_facts.WithdrawalPayload {
    event: WithdrawalEvent {
      ..event,
      info: WithdrawalInfo {
        ..event.info,
        validity: ledger_state.IncorrectWithdrawalSignature,
      },
    },
    refund_address,
    refund_datum,
  }
  expect _ = admit_withdrawal(
    admission(
      Facts { ..withdrawal_facts(), location: Inline { payload: classified } },
    ),
  )
  True
}

// Reading an Order's facts from a reference input (the scheduler's neglected
// deposit / withdrawal strike). Every negative returns True whenever the reader
// returns at all: a `fail` test also passes on False, so comparing against a
// value the unchecked reader would not produce could not fail.

const other_policy = #"55555555555555555555555555555555555555555555555555555555"

fn reference(output: Output, out_index: Int) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: tx_id,
      output_index: out_index,
    },
    output,
  }
}

fn wallet_output() -> Output {
  Output {
    address: address.from_verification_key(owner),
    value: assets.from_lovelace(5_000_000),
    datum: NoDatum,
    reference_script: None,
  }
}

fn read_order(references: List<Input>, index: Int) -> Facts {
  order_facts.referenced(references, index, policy, address.from_script(policy))
}

test referenced_order_yields_its_inclusion_time() {
  let references = [
    reference(wallet_output(), 0),
    reference(order_output(facts(), None, 600), 1),
  ]
  and {
    read_order(references, 1) == facts(),
    read_order(references, 1).inclusion_time == 400 + env.event_wait_duration,
  }
}

test referenced_order_is_read_after_pointer_continuation() {
  let rewritten = order_output(
    facts(),
    Some(#"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"),
    60_700,
  )
  read_order([reference(rewritten, 4)], 0) == facts()
}

test referenced_rejects_an_index_past_the_reference_inputs() fail {
  read_order([reference(order_output(facts(), None, 600), 0)], 1) == facts()
}

test referenced_rejects_a_mis_indexed_unauthenticated_input() fail {
  let references = [
    reference(wallet_output(), 0),
    reference(order_output(facts(), None, 600), 1),
  ]
  read_order(references, 0) == facts()
}

test referenced_rejects_a_node_of_another_history_policy() fail {
  let references = [reference(order_output(facts(), None, 600), 0)]
  order_facts.referenced(
    references,
    0,
    other_policy,
    address.from_script(policy),
  ) == facts()
}

test referenced_rejects_a_node_at_another_address() fail {
  let references = [reference(order_output(facts(), None, 600), 0)]
  order_facts.referenced(references, 0, policy, address.from_script(retention)) == facts()
}

test referenced_rejects_an_order_datum_without_its_list_token() fail {
  let order = order_output(facts(), None, 600)
  let forged = Output { ..order, value: assets.from_lovelace(10_000_000) }
  read_order([reference(forged, 0)], 0) == facts()
}

test referenced_rejects_facts_for_another_event_id() fail {
  let moved = Facts {
    ..facts(),
    event_id: OutputReference { transaction_id: tx_id, output_index: 1 },
  }
  read_order([reference(order_output(moved, None, 600), 0)], 0) == moved
}

test referenced_rejects_a_legacy_optimistic_deposit_datum() fail {
  let legacy: Data = user_events.OptimisticDatum {
    event: payload(),
    inclusion_time: 400 + env.event_wait_duration,
    witness: owner,
    refund_address: address.from_verification_key(owner),
    refund_datum: NoDatum,
  }
  let order = order_output(facts(), None, 600)
  let old = Output { ..order, datum: InlineDatum(legacy) }
  read_order([reference(old, 0)], 0).inclusion_time == 400 + env.event_wait_duration
}
