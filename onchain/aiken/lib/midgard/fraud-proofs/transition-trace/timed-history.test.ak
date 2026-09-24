use aiken/cbor
use aiken/interval
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{VerificationKey}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use env
use midgard/event_history/node_role
use midgard/event_history/order_facts
use midgard/event_history/ordering
use midgard/event_history/proof_evidence
use midgard/event_history/proof_fixture as history
use midgard/event_history/retained_data
use midgard/fraud_proofs/transition_trace/timed_history
use midgard/ledger_state
use midgard/transition_trace
use midgard/user_events

const h28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h28_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h32_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const h32_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const h64_a =
  #"33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333"

const event_id = OutputReference { transaction_id: h32_a, output_index: 0 }

const reference =
  timed_history.Reference { order_index: 0, external_data_index: None }

fn parameters() -> timed_history.Parameters {
  timed_history.Parameters {
    policy: h28_a,
    list_address: address.from_script(h28_a),
    data: history.parameters(),
  }
}

fn base_header() -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    prev_utxos_root: h32_a,
    utxos_root: h32_b,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 10,
    end_time: 20,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: h28_b,
    operator_vkey: h28_a,
    protocol_version: ledger_state.protocol_version_v1,
  }
}

fn deposit() -> ledger_state.DepositEvent {
  ledger_state.DepositEvent {
    id: event_id,
    info: ledger_state.DepositInfo {
      l2_address: address.from_verification_key(h28_b),
      l2_network_id: 0,
      l2_datum: None,
    },
  }
}

fn withdrawal() -> ledger_state.WithdrawalEvent {
  ledger_state.WithdrawalEvent {
    id: event_id,
    info: ledger_state.WithdrawalInfo {
      body: ledger_state.WithdrawalBody {
        l2_outref: OutputReference { transaction_id: h32_b, output_index: 3 },
        l2_owner: h28_b,
        l2_value: [],
        l1_address: address.from_verification_key(h28_b),
        l1_datum: NoDatum,
      },
      signature: Pair(h32_a, h64_a),
      validity: ledger_state.WithdrawalIsValid,
    },
  }
}

fn payload(kind: order_facts.Kind) -> order_facts.Payload {
  when kind is {
    order_facts.Deposit -> order_facts.DepositPayload { event: deposit() }
    order_facts.Withdrawal ->
      order_facts.WithdrawalPayload {
        event: withdrawal(),
        refund_address: address.from_verification_key(h28_b),
        refund_datum: NoDatum,
      }
  }
}

fn transaction(kind: order_facts.Kind, time: Int) -> Transaction {
  Transaction {
    ..placeholder,
    validity_range: interval.between(21, 22),
    reference_inputs: [
      history.order_reference(
        h28_a,
        user_events.out_ref_to_nonce(event_id),
        payload(kind),
        time,
      ),
    ],
  }
}

fn absent(
  kind: order_facts.Kind,
) -> transition_trace.RootNonMembershipProof<OutputReference> {
  transition_trace.RootNonMembershipProof {
    domain: when kind is {
      order_facts.Deposit -> transition_trace.DepositsRootDomain
      order_facts.Withdrawal -> transition_trace.WithdrawalsRootDomain
    },
    root: env.empty_merkle_tree_root,
    phas_root: env.empty_merkle_tree_root,
    count: 0,
    key: event_id,
    proof: [],
  }
}

fn membership(
  domain: transition_trace.RootDomain,
  value: value,
  value_bytes: ByteArray,
) -> transition_trace.RootMembershipProof<OutputReference, value> {
  let phas_root = mpf.root(
    mpf.insert(mpf.empty, cbor.serialise(event_id), value_bytes, []),
  )
  transition_trace.RootMembershipProof {
    domain,
    root: transition_trace.commit_counted_root(domain, phas_root, 1),
    phas_root,
    count: 1,
    key: event_id,
    value,
    proof: [],
  }
}

fn omitted(tx: Transaction, kind: order_facts.Kind) -> Bool {
  timed_history.omitted(
    tx,
    parameters(),
    reference,
    kind,
    base_header(),
    absent(kind),
  )
}

fn deposit_outside(tx: Transaction, info: ledger_state.DepositInfo) -> Bool {
  let source = membership(
    transition_trace.DepositsRootDomain,
    info,
    cbor.serialise(info),
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    deposits_root: source.root,
    deposit_count: 1,
  }
  timed_history.out_of_window_deposit(
    tx,
    parameters(),
    reference,
    header,
    source,
  )
}

fn withdrawal_outside(
  tx: Transaction,
  info: ledger_state.WithdrawalInfo,
) -> Bool {
  let source = membership(
    transition_trace.WithdrawalsRootDomain,
    info,
    cbor.serialise(info),
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    withdrawals_root: source.root,
    withdrawal_count: 1,
  }
  timed_history.out_of_window_withdrawal(
    tx,
    parameters(),
    reference,
    header,
    source,
  )
}

test omitted_deposit_uses_open_start_closed_end_eligibility() {
  and {
    !omitted(transaction(order_facts.Deposit, 10), order_facts.Deposit),
    omitted(transaction(order_facts.Deposit, 11), order_facts.Deposit),
    omitted(transaction(order_facts.Deposit, 20), order_facts.Deposit),
    !omitted(transaction(order_facts.Deposit, 21), order_facts.Deposit),
  }
}

test omitted_withdrawal_uses_open_start_closed_end_eligibility() {
  and {
    !omitted(transaction(order_facts.Withdrawal, 10), order_facts.Withdrawal),
    omitted(transaction(order_facts.Withdrawal, 11), order_facts.Withdrawal),
    omitted(transaction(order_facts.Withdrawal, 20), order_facts.Withdrawal),
    !omitted(transaction(order_facts.Withdrawal, 21), order_facts.Withdrawal),
  }
}

test eligible_deposit_cannot_be_challenged_as_outside() {
  !deposit_outside(transaction(order_facts.Deposit, 20), deposit().info)
}

test eligible_withdrawal_cannot_be_challenged_as_outside() {
  !withdrawal_outside(
    transaction(order_facts.Withdrawal, 20),
    withdrawal().info,
  )
}

test excluded_start_deposit_and_late_deposit_are_outside() {
  and {
    deposit_outside(transaction(order_facts.Deposit, 10), deposit().info),
    deposit_outside(transaction(order_facts.Deposit, 21), deposit().info),
  }
}

test withdrawal_operator_verdict_is_not_submitted_content() {
  let invalid = ledger_state.WithdrawalInfo {
    ..withdrawal().info,
    validity: ledger_state.IncorrectWithdrawalSignature,
  }
  and {
    withdrawal_outside(
      transaction(order_facts.Withdrawal, 10),
      withdrawal().info,
    ),
    withdrawal_outside(transaction(order_facts.Withdrawal, 21), invalid),
    !withdrawal_outside(transaction(order_facts.Withdrawal, 20), invalid),
  }
}

test mismatched_deposit_content_does_not_prove_timing() {
  let wrong = ledger_state.DepositInfo { ..deposit().info, l2_network_id: 1 }
  !deposit_outside(transaction(order_facts.Deposit, 21), wrong)
}

test mismatched_withdrawal_body_does_not_prove_timing() {
  let body = ledger_state.WithdrawalBody {
    ..withdrawal().info.body,
    l2_owner: h28_a,
  }
  !withdrawal_outside(
    transaction(order_facts.Withdrawal, 21),
    ledger_state.WithdrawalInfo { ..withdrawal().info, body },
  )
}

test mismatched_withdrawal_signature_does_not_prove_timing() {
  !withdrawal_outside(
    transaction(order_facts.Withdrawal, 21),
    ledger_state.WithdrawalInfo {
      ..withdrawal().info,
      signature: Pair(h32_b, h64_a),
    },
  )
}

test capture_before_header_end_rejects() fail {
  let tx = transaction(order_facts.Deposit, 20)
  omitted(
    Transaction { ..tx, validity_range: interval.between(20, 21) },
    order_facts.Deposit,
  )
}

test capture_at_merge_deadline_rejects() fail {
  let tx = transaction(order_facts.Withdrawal, 20)
  let end = base_header().end_time + ledger_state.block_maturity_duration_v1
  omitted(
    Transaction { ..tx, validity_range: interval.between(end - 1, end) },
    order_facts.Withdrawal,
  )
}

test consumed_order_is_not_reference_authority() fail {
  let tx = transaction(order_facts.Deposit, 20)
  omitted(
    Transaction { ..tx, inputs: tx.reference_inputs },
    order_facts.Deposit,
  )
}

test filler_cannot_be_an_omitted_order() fail {
  omitted(
    Transaction {
      ..transaction(order_facts.Deposit, 20),
      reference_inputs: [history.absent_reference(h28_a, event_id)],
    },
    order_facts.Deposit,
  )
}

test wrong_event_id_rejects() fail {
  let source = transition_trace.RootNonMembershipProof {
    ..absent(order_facts.Deposit),
    key: OutputReference { transaction_id: h32_b, output_index: 0 },
  }
  timed_history.omitted(
    transaction(order_facts.Deposit, 20),
    parameters(),
    reference,
    order_facts.Deposit,
    base_header(),
    source,
  )
}

test wrong_history_kind_rejects() fail {
  omitted(transaction(order_facts.Withdrawal, 20), order_facts.Deposit)
}

test wrong_list_address_rejects() fail {
  let params = timed_history.Parameters {
    ..parameters(),
    list_address: address.from_script(h28_b),
  }
  timed_history.omitted(
    transaction(order_facts.Deposit, 20),
    params,
    reference,
    order_facts.Deposit,
    base_header(),
    absent(order_facts.Deposit),
  )
}

test wrong_proof_domain_rejects() {
  !timed_history.omitted(
    transaction(order_facts.Deposit, 20),
    parameters(),
    reference,
    order_facts.Deposit,
    base_header(),
    absent(order_facts.Withdrawal),
  )
}

test pointer_continuation_with_same_facts_keeps_timing() {
  let tx = transaction(order_facts.Deposit, 20)
  expect [order] = tx.reference_inputs
  expect InlineDatum(raw) = order.output.datum
  expect node: ordering.Link<Data> = raw
  let changed: Data = ordering.Link {
    ..node,
    next: Some(
      #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    ),
    protected_until: 200000,
  }
  let successor = Input {
    ..order,
    output_reference: OutputReference { transaction_id: h32_b, output_index: 7 },
    output: Output { ..order.output, datum: InlineDatum(changed) },
  }
  omitted(
    Transaction { ..tx, reference_inputs: [successor] },
    order_facts.Deposit,
  )
}

fn external(kind: order_facts.Kind) -> Transaction {
  let tx = transaction(kind, 20)
  expect [order] = tx.reference_inputs
  let data = retained_data.EventData {
    event_key: user_events.out_ref_to_nonce(event_id),
    event_payload: payload(kind),
    reclaim_auth: VerificationKey(h28_b),
  }
  let facts = order_facts.Facts {
    ..history.facts(payload(kind), 20),
    location: order_facts.External {
      storage_datum_hash: retained_data.datum_hash(data),
    },
  }
  let raw_facts: Data = facts
  let content: Data = node_role.Order { facts: raw_facts }
  let node: Data = ordering.Link {
    position: ordering.Key(user_events.out_ref_to_nonce(event_id)),
    next: None,
    protected_until: 100000,
    payload: content,
  }
  let published = Input {
    output_reference: OutputReference { transaction_id: h32_b, output_index: 1 },
    output: Output {
      address: parameters().data.retention_address,
      value: assets.from_lovelace(5000000),
      datum: InlineDatum(data),
      reference_script: None,
    },
  }
  Transaction {
    ..tx,
    reference_inputs: [
      Input {
        ..order,
        output: Output { ..order.output, datum: InlineDatum(node) },
      },
      published,
    ],
  }
}

fn omitted_external(tx: Transaction, kind: order_facts.Kind) -> Bool {
  timed_history.omitted(
    tx,
    parameters(),
    timed_history.Reference { order_index: 0, external_data_index: Some(1) },
    kind,
    base_header(),
    absent(kind),
  )
}

test existing_external_deposit_and_withdrawal_data_authenticate() {
  and {
    omitted_external(external(order_facts.Deposit), order_facts.Deposit),
    omitted_external(external(order_facts.Withdrawal), order_facts.Withdrawal),
  }
}

test external_hash_without_data_rejects() fail {
  expect [order, _] = external(order_facts.Deposit).reference_inputs
  omitted_external(
    Transaction { ..external(order_facts.Deposit), reference_inputs: [order] },
    order_facts.Deposit,
  )
}

test new_sibling_publication_is_not_external_authority() fail {
  let tx = external(order_facts.Withdrawal)
  expect [order, data] = tx.reference_inputs
  omitted_external(
    Transaction { ..tx, reference_inputs: [order], outputs: [data.output] },
    order_facts.Withdrawal,
  )
}

test consumed_external_data_rejects() fail {
  let tx = external(order_facts.Deposit)
  expect [_, data] = tx.reference_inputs
  omitted_external(Transaction { ..tx, inputs: [data] }, order_facts.Deposit)
}

test complete_external_datum_hash_binds_reclaim_authority() fail {
  let tx = external(order_facts.Withdrawal)
  expect [order, published] = tx.reference_inputs
  expect InlineDatum(raw) = published.output.datum
  expect data: retained_data.EventData = raw
  let changed: Data = retained_data.EventData {
    ..data,
    reclaim_auth: VerificationKey(h28_a),
  }
  let forged = Input {
    ..published,
    output: Output { ..published.output, datum: InlineDatum(changed) },
  }
  omitted_external(
    Transaction { ..tx, reference_inputs: [order, forged] },
    order_facts.Withdrawal,
  )
}

test wrong_policy_cannot_authenticate_an_order() fail {
  let params = timed_history.Parameters { ..parameters(), policy: h28_b }
  timed_history.omitted(
    transaction(order_facts.Deposit, 20),
    params,
    reference,
    order_facts.Deposit,
    base_header(),
    absent(order_facts.Deposit),
  )
}

test history_nft_quantity_must_be_exactly_one() fail {
  let tx = transaction(order_facts.Deposit, 20)
  expect [order] = tx.reference_inputs
  let doubled = assets.add(
    order.output.value,
    h28_a,
    user_events.out_ref_to_nonce(event_id),
    1,
  )
  omitted(
    Transaction {
      ..tx,
      reference_inputs: [
        Input { ..order, output: Output { ..order.output, value: doubled } },
      ],
    },
    order_facts.Deposit,
  )
}

test short_legacy_nft_name_cannot_authenticate_history() fail {
  let tx = transaction(order_facts.Deposit, 20)
  expect [order] = tx.reference_inputs
  let legacy = assets.from_lovelace(3000000) |> assets.add(h28_a, #"01", 1)
  omitted(
    Transaction {
      ..tx,
      reference_inputs: [
        Input { ..order, output: Output { ..order.output, value: legacy } },
      ],
    },
    order_facts.Deposit,
  )
}

test malformed_successor_cannot_supply_timing_authority() fail {
  let tx = transaction(order_facts.Deposit, 20)
  expect [order] = tx.reference_inputs
  expect InlineDatum(raw) = order.output.datum
  expect node: ordering.Link<Data> = raw
  let invalid: Data = ordering.Link {
    ..node,
    next: Some(user_events.out_ref_to_nonce(event_id)),
  }
  omitted(
    Transaction {
      ..tx,
      reference_inputs: [
        Input {
          ..order,
          output: Output { ..order.output, datum: InlineDatum(invalid) },
        },
      ],
    },
    order_facts.Deposit,
  )
}

test external_data_at_wrong_retention_script_rejects() fail {
  let tx = external(order_facts.Withdrawal)
  expect [order, published] = tx.reference_inputs
  let wrong = Input {
    ..published,
    output: Output { ..published.output, address: address.from_script(h28_b) },
  }
  omitted_external(
    Transaction { ..tx, reference_inputs: [order, wrong] },
    order_facts.Withdrawal,
  )
}

test inline_bound_is_enforced_during_timing_capture() fail {
  let params = timed_history.Parameters {
    ..parameters(),
    data: proof_evidence.Parameters { ..parameters().data, inline_limit: 1 },
  }
  timed_history.omitted(
    transaction(order_facts.Deposit, 20),
    params,
    reference,
    order_facts.Deposit,
    base_header(),
    absent(order_facts.Deposit),
  )
}

test external_payload_bound_is_enforced_during_timing_capture() fail {
  let params = timed_history.Parameters {
    ..parameters(),
    data: proof_evidence.Parameters {
      ..parameters().data,
      max_payload_bytes: 1,
    },
  }
  timed_history.omitted(
    external(order_facts.Deposit),
    params,
    timed_history.Reference { ..reference, external_data_index: Some(1) },
    order_facts.Deposit,
    base_header(),
    absent(order_facts.Deposit),
  )
}

test later_id_reuse_cannot_prove_omission_in_an_earlier_header() {
  !omitted(transaction(order_facts.Deposit, 200), order_facts.Deposit)
}
