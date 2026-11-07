//// Real singleton trie roots exercise membership over authenticated event bytes.

use aiken/cbor
use aiken/interval
use aiken/merkle_patricia_forestry as mpf
use aiken_design_patterns/linked_list
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use env
use midgard/event_history/admission as semantics
use midgard/event_history/node_role.{Filler, Order, RootContent}
use midgard/event_history/order_facts.{
  Deposit, DepositPayload, Facts, Inline, Payload, WithdrawalPayload,
}
use midgard/event_history/ordering.{Key, Link, Root}
use midgard/event_history/retirement.{
  AbsorbDeposit, InitializeWithdrawalPayout, Purpose, RefundInvalidWithdrawal,
  Witness,
}
use midgard/ledger_state.{
  DepositEvent, DepositInfo, WithdrawalBody, WithdrawalEvent, WithdrawalInfo,
  WithdrawalIsValid,
}
use midgard/payout
use midgard/settlement
use midgard/state_queue
use midgard/transition_trace
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

fn params() -> semantics.Parameters {
  semantics.Parameters {
    policy,
    address: address.from_script(policy),
    retention_address: address.from_script(retention),
    kind: Deposit,
    protection_duration: 200,
    inline_limit: 1000,
    max_payload_bytes: 15000,
    max_payload_nodes: 1024,
  }
}

fn filler(next: Option<ByteArray>, protection: Int) -> Output {
  let role: Data = Filler { refund_key: owner }
  let raw: Data = Link {
    position: Key(user_events.out_ref_to_nonce(nonce)),
    next,
    protected_until: protection,
    payload: role,
  }
  Output {
    ..order_output(facts(), next, protection),
    datum: InlineDatum(raw),
    value: assets.from_lovelace(5_000_000)
      |> assets.add(policy, user_events.out_ref_to_nonce(nonce), 1),
  }
}

fn root(next: Option<ByteArray>, protection: Int) -> Output {
  let role: node_role.Content<Data> = RootContent
  let raw: Data = Link {
    position: Root,
    next,
    protected_until: protection,
    payload: role,
  }
  Output {
    ..filler(None, 0),
    datum: InlineDatum(raw),
    value: assets.from_lovelace(3_000_000) |> assets.add(policy, #"", 1),
  }
}

fn history_input(output: Output) -> Input {
  Input {
    output_reference: OutputReference { transaction_id: tx_id, output_index: 2 },
    output,
  }
}

fn refund() -> Output {
  Output {
    address: address.from_verification_key(owner),
    value: assets.from_lovelace(5_000_000),
    datum: NoDatum,
    reference_script: None,
  }
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

fn authority() -> retirement.Authority {
  retirement.Authority {
    state_queue_policy: retention,
    settlement_policy: owner,
    reserve_address: address.from_script(owner),
    payout_policy: owner,
    payout_address: address.from_script(owner),
  }
}

fn reference(
  policy_id: ByteArray,
  name: ByteArray,
  raw: Data,
  index: Int,
) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: tx_id,
      output_index: index,
    },
    output: Output {
      address: address.from_script(policy_id),
      value: assets.from_lovelace(3_000_000) |> assets.add(policy_id, name, 1),
      datum: InlineDatum(raw),
      reference_script: None,
    },
  }
}

fn frontier(end_time: Int) -> Input {
  let confirmed: Data = ledger_state.ConfirmedState {
    header_hash: #"01",
    prev_header_hash: #"02",
    utxo_root: #"03",
    start_time: 0,
    end_time,
    protocol_version: ledger_state.protocol_version_v1,
  }
  let root: linked_list.Element<Data, Data> = linked_list.Element {
    data: linked_list.Root { data: confirmed },
    link: None,
  }
  reference(retention, state_queue.confirmed_state_asset_name, root, 10)
}

fn membership(
  purpose: Purpose,
) -> transition_trace.RootMembershipProof<ByteArray, ByteArray> {
  let (domain, info) = when purpose is {
    AbsorbDeposit -> {
      expect DepositPayload { event } = payload()
      (transition_trace.DepositsRootDomain, cbor.serialise(event.info))
    }
    _ -> {
      expect WithdrawalPayload { event, .. } = withdrawal_payload()
      let info = when purpose is {
        RefundInvalidWithdrawal { validity } ->
          WithdrawalInfo { ..event.info, validity }
        _ -> event.info
      }
      (transition_trace.WithdrawalsRootDomain, cbor.serialise(info))
    }
  }
  let phas_root = mpf.insert(mpf.empty, cbor.serialise(nonce), info, [])
    |> mpf.root
  transition_trace.RootMembershipProof {
    domain,
    root: transition_trace.commit_counted_root(domain, phas_root, 1),
    phas_root,
    count: 1,
    key: cbor.serialise(nonce),
    value: info,
    proof: [],
  }
}

fn witness(purpose: Purpose) -> Witness {
  let proof = membership(purpose)
  Witness {
    predecessor_input_index: 0,
    order_input_index: 1,
    predecessor_output_index: 0,
    funds_output_index: 1,
    structural_refund_output_index: if purpose == AbsorbDeposit {
      Some(2)
    } else {
      None
    },
    confirmed_reference_index: 0,
    settlement_reference_index: 1,
    external_reference_index: None,
    membership: retirement.Membership {
      phas_root: proof.phas_root,
      count: proof.count,
      proof: proof.proof,
    },
    purpose,
  }
}

fn retiring(purpose: Purpose) -> Transaction {
  let proof = membership(purpose)
  let roots = settlement.Datum {
    deposits_root: proof.root,
    withdrawals_root: proof.root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: env.empty_merkle_tree_root,
    resolution_claim: None,
  }
  let key = user_events.out_ref_to_nonce(nonce)
  let order_facts = if purpose == AbsorbDeposit {
    facts()
  } else {
    withdrawal_facts()
  }
  let base_output = Output {
    ..refund(),
    value: assets.from_lovelace(10_000_000),
  }
  let funds = when purpose is {
    AbsorbDeposit ->
      Output {
        ..base_output,
        address: authority().reserve_address,
        value: assets.from_lovelace(7_000_000),
      }
    RefundInvalidWithdrawal { .. } -> base_output
    InitializeWithdrawalPayout -> {
      expect WithdrawalPayload { event, .. } = withdrawal_payload()
      let datum: Data = payout.Datum {
        l2_value: event.info.body.l2_value,
        l1_address: event.info.body.l1_address,
        l1_datum: event.info.body.l1_datum,
      }
      Output {
        ..base_output,
        address: authority().payout_address,
        datum: InlineDatum(datum),
        value: assets.add(base_output.value, owner, key, 1),
      }
    }
  }
  Transaction {
    ..placeholder,
    inputs: [
      history_input(root(Some(key), 200)),
      Input {
        output_reference: OutputReference {
          transaction_id: tx_id,
          output_index: 3,
        },
        output: order_output(order_facts, None, 200),
      },
    ],
    reference_inputs: [
      frontier(facts().inclusion_time),
      reference(owner, #"01", roots, 11),
    ],
    outputs: [
      root(None, 100_200),
      funds,
      Output { ..refund(), value: assets.from_lovelace(3_000_000) },
    ],
    mint: if purpose == InitializeWithdrawalPayout {
      assets.from_asset(policy, key, -1) |> assets.add(owner, key, 1)
    } else {
      assets.from_asset(policy, key, -1)
    },
    validity_range: interval.between(99_900, 100_000),
  }
}

fn retire(tx: Transaction, w: Witness) -> Bool {
  let parameters = semantics.Parameters {
    ..params(),
    kind: if w.purpose == AbsorbDeposit {
      Deposit
    } else {
      order_facts.Withdrawal
    },
  }
  retirement.retire(tx, parameters, authority(), w)
}

test deposit_unlinks_only_after_frontier_and_settlement() {
  retire(retiring(AbsorbDeposit), witness(AbsorbDeposit))
}

test settlement_membership_does_not_replace_finality() fail {
  let tx = retiring(AbsorbDeposit)
  expect [_, settlement_ref] = tx.reference_inputs
  retire(
    Transaction {
      ..tx,
      reference_inputs: [frontier(facts().inclusion_time - 1), settlement_ref],
    },
    witness(AbsorbDeposit),
  )
}

test forged_frontier_without_authenticated_token_rejects() fail {
  let tx = retiring(AbsorbDeposit)
  expect [confirmed, settlement_ref] = tx.reference_inputs
  retire(
    Transaction {
      ..tx,
      reference_inputs: [
        Input {
          ..confirmed,
          output: Output {
            ..confirmed.output,
            value: assets.from_lovelace(3_000_000),
          },
        },
        settlement_ref,
      ],
    },
    witness(AbsorbDeposit),
  )
}

test deposit_cannot_route_structural_ada_to_reserve() fail {
  let tx = retiring(AbsorbDeposit)
  expect [continued, funds, refund] = tx.outputs
  retire(
    Transaction {
      ..tx,
      outputs: [
        continued,
        Output { ..funds, value: assets.from_lovelace(10_000_000) },
        refund,
      ],
    },
    witness(AbsorbDeposit),
  )
}

test deposit_requires_structural_refund() fail {
  retire(
    retiring(AbsorbDeposit),
    Witness { ..witness(AbsorbDeposit), structural_refund_output_index: None },
  )
}

test structural_refund_can_be_topped_up_but_not_reduced() {
  let tx = retiring(AbsorbDeposit)
  expect [continued, funds, refund] = tx.outputs
  retire(
    Transaction {
      ..tx,
      outputs: [
        continued,
        funds,
        Output { ..refund, value: assets.from_lovelace(4_000_000) },
      ],
    },
    witness(AbsorbDeposit),
  )
}

test settlement_leaf_content_must_match_original() fail {
  let tx = retiring(AbsorbDeposit)
  expect [predecessor, order] = tx.inputs
  expect DepositPayload { event } = payload()
  let changed = DepositPayload {
    event: DepositEvent {
      ..event,
      info: DepositInfo { ..event.info, l2_network_id: 999 },
    },
  }
  retire(
    Transaction {
      ..tx,
      inputs: [
        predecessor,
        Input {
          ..order,
          output: order_output(
            Facts { ..facts(), location: Inline { payload: changed } },
            None,
            200,
          ),
        },
      ],
    },
    witness(AbsorbDeposit),
  )
}

test retirement_rejects_a_forged_membership_root() fail {
  let w = witness(AbsorbDeposit)
  retire(
    retiring(AbsorbDeposit),
    Witness {
      ..w,
      membership: retirement.Membership {
        ..w.membership,
        phas_root: #"abababababababababababababababababababababababababababababababab",
      },
    },
  )
}

test retirement_verifies_membership_without_redeemer_carried_leaf() {
  let tx = retiring(AbsorbDeposit)
  and {
    tx.redeemers == [],
    retire(tx, witness(AbsorbDeposit)),
  }
}

test finalized_invalid_withdrawal_refunds_original_locked_value() {
  let purpose = RefundInvalidWithdrawal {
    validity: ledger_state.IncorrectWithdrawalSignature,
  }
  retire(retiring(purpose), witness(purpose))
}

test valid_withdrawal_cannot_use_invalid_refund_path() fail {
  let purpose = RefundInvalidWithdrawal { validity: WithdrawalIsValid }
  retire(retiring(purpose), witness(purpose))
}

test finalized_valid_withdrawal_starts_exact_payout() {
  retire(
    retiring(InitializeWithdrawalPayout),
    witness(InitializeWithdrawalPayout),
  )
}

test payout_cannot_skip_its_authentication_token() fail {
  let tx = retiring(InitializeWithdrawalPayout)
  retire(
    Transaction {
      ..tx,
      mint: assets.from_asset(policy, user_events.out_ref_to_nonce(nonce), -1),
    },
    witness(InitializeWithdrawalPayout),
  )
}

test payout_cannot_exceed_withdrawal_target() fail {
  let tx = retiring(InitializeWithdrawalPayout)
  expect [predecessor, input] = tx.inputs
  expect [continued, funds, refund] = tx.outputs
  retire(
    Transaction {
      ..tx,
      inputs: [
        predecessor,
        Input {
          ..input,
          output: Output {
            ..input.output,
            value: assets.add(input.output.value, #"", #"", 20_000_000),
          },
        },
      ],
      outputs: [
        continued,
        Output { ..funds, value: assets.add(funds.value, #"", #"", 20_000_000) },
        refund,
      ],
    },
    witness(InitializeWithdrawalPayout),
  )
}
