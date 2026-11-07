//// Full-transaction fixtures for the scheduler's strike for inactivity
//// (`GoToNextDueToSkippedOperator`, `validate_operator_inactivity_and_get_its_link`).
////
//// A strike is allowed only on neglected L1 work: it must cite a deposit,
//// withdrawal or tx order that no committed block covers (its inclusion time
//// is after the state-queue tail's `end_time`) and that is past
//// `user_events_negligence_timeout`. An idle network, with no undelivered user
//// event, is never strikable.
////
//// Each fixture is one complete strike transaction, run through both the
//// scheduler spending validator and the active-operators spending validator
//// (`StrikeForInactivity`).
////
//// Layout:
//// - inputs: [0] scheduler, [1] the struck operator's active node;
//// - reference inputs: [0] hub oracle, [1] state-queue tail (the
////   confirmed-state root), [2] the new shift's operator node, [3] the cited
////   user event;
//// - outputs: [0] scheduler, [1] the struck node with one more strike;
//// - redeemers: [0] scheduler spend, [1] active-node spend.

use aiken/builtin
use aiken/interval
use aiken/math
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Spend, Transaction,
}
use env
use midgard/event_history/node_role.{Order}
use midgard/event_history/order_facts.{External, Facts}
use midgard/event_history/ordering.{Key, Link}
use midgard/hub_oracle as hub
use midgard/ledger_state.{
  ConfirmedState, ForcedTxProofSourceV1, TxOrderEventV1, TxOrderPayloadV1,
  protocol_version_v1,
}
use midgard/operator_directory/active_operators
use midgard/scheduler.{
  ActiveOperator, GoToNextDueToSkippedOperator, NeglectedDeposit,
  NeglectedTxOrder, NeglectedUserEvent, NeglectedWithdrawal, SchedDatum,
  SpendRedeemer,
}
use midgard/state_queue
use midgard/user_events
use operator_directory/active_operators as active_operators_validator
use scheduler as scheduler_validator

// ## Deployment identities

const hub_policy = #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const scheduler_policy =
  #"a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2"

const active_operators_policy =
  #"a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3"

const registered_operators_policy =
  #"a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4"

const state_queue_policy =
  #"a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5"

const deposit_policy =
  #"a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6"

const withdrawal_policy =
  #"a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7"

const tx_order_policy =
  #"a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8"

const other_policy = #"a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9"

// ## Fixture values

/// The scheduled operator, struck for inactivity. Its node is the list tail.
const struck_operator =
  #"c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2"

/// The operator whose node links to the struck one; it takes the next shift.
const next_operator =
  #"c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1"

const inputs_transaction_id =
  #"0101010101010101010101010101010101010101010101010101010101010101"

const reference_inputs_transaction_id =
  #"0202020202020202020202020202020202020202020202020202020202020202"

const event_nonce =
  OutputReference {
    transaction_id: #"0303030303030303030303030303030303030303030303030303030303030303",
    output_index: 0,
  }

const element_lovelace = 2_000_000

/// The struck operator's shift starts here.
const shift_start = 100_000_000

/// The validity range every fixture uses, one minute wide.
const validity_width = 60_000

/// `max_inactivity_between_block_commitments` before it was deleted: an idle
/// tail this old used to license a strike with no cited event.
const former_commitment_gap = 1_200_000

const scheduler_input_index = 0

const active_node_input_index = 1

const hub_ref_input_index = 0

const state_queue_ref_input_index = 1

const next_operator_node_ref_input_index = 2

const event_ref_input_index = 3

// ## Primitive builders

fn out_ref(transaction_id: ByteArray, output_index: Int) -> OutputReference {
  OutputReference { transaction_id, output_index }
}

fn reference_input(index: Int, output: Output) -> Input {
  Input {
    output_reference: out_ref(reference_inputs_transaction_id, index),
    output,
  }
}

fn hub_datum() -> hub.Datum {
  let other = address.from_script(other_policy)
  hub.Datum {
    registered_operators: registered_operators_policy,
    active_operators: active_operators_policy,
    retired_operators: other_policy,
    scheduler: scheduler_policy,
    state_queue: state_queue_policy,
    fraud_proof_catalogue: other_policy,
    fraud_proof: other_policy,
    deposit: deposit_policy,
    withdrawal: withdrawal_policy,
    tx_order: tx_order_policy,
    settlement: other_policy,
    payout: other_policy,
    registered_operators_addr: address.from_script(registered_operators_policy),
    active_operators_addr: address.from_script(active_operators_policy),
    retired_operators_addr: other,
    scheduler_addr: address.from_script(scheduler_policy),
    state_queue_addr: address.from_script(state_queue_policy),
    fraud_proof_catalogue_addr: other,
    fraud_proof_addr: other,
    deposit_addr: address.from_script(deposit_policy),
    withdrawal_addr: address.from_script(withdrawal_policy),
    tx_order_addr: address.from_script(tx_order_policy),
    settlement_addr: other,
    reserve_addr: other,
    payout_addr: other,
    reserve_observer: other_policy,
  }
}

fn hub_output() -> Output {
  Output {
    address: address.from_script(hub_policy),
    value: assets.from_lovelace(element_lovelace)
      |> assets.add(hub_policy, hub.asset_name, 1),
    datum: InlineDatum(hub_datum()),
    reference_script: None,
  }
}

/// The state-queue tail: an empty queue whose confirmed state ends at
/// `tail_end`, so the events the chain has delivered are exactly those with
/// `inclusion_time <= tail_end`.
fn state_queue_tail_output(tail_end: Int) -> Output {
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Root(
      ConfirmedState {
        header_hash: #"d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1",
        prev_header_hash: #"d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2",
        utxo_root: #"e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1",
        start_time: 0,
        end_time: tail_end,
        protocol_version: protocol_version_v1,
      },
    ),
    link: None,
  }
  Output {
    address: address.from_script(state_queue_policy),
    value: assets.from_lovelace(element_lovelace)
      |> assets.add(
          state_queue_policy,
          state_queue.confirmed_state_asset_name,
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn active_node_output(
  key: ByteArray,
  link: Option<ByteArray>,
  inactivity_strikes: Int,
) -> Output {
  let datum: active_operators.Datum = linked_list.Element {
    data: linked_list.Node(
      active_operators.NodeData { bond_unlock_time: None, inactivity_strikes },
    ),
    link,
  }
  Output {
    address: address.from_script(active_operators_policy),
    value: assets.from_lovelace(element_lovelace)
      |> assets.add(
          active_operators_policy,
          bytearray.concat(active_operators.node_asset_name_prefix, key),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn scheduler_output(datum: SchedDatum) -> Output {
  Output {
    address: address.from_script(scheduler_policy),
    value: assets.from_lovelace(element_lovelace)
      |> assets.add(scheduler_policy, scheduler.asset_name, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

// ## Cited user events

/// The user event a strike cites, with its inclusion time.
type CitedEvent {
  CitedDeposit { inclusion_time: Int }
  CitedWithdrawal { inclusion_time: Int }
  CitedTxOrder { inclusion_time: Int }
}

fn inclusion_time_of(cited: CitedEvent) -> Int {
  when cited is {
    CitedDeposit { inclusion_time } -> inclusion_time
    CitedWithdrawal { inclusion_time } -> inclusion_time
    CitedTxOrder { inclusion_time } -> inclusion_time
  }
}

/// A deposit's or a withdrawal's Order node in its event-history list.
fn order_node_output(policy: ByteArray, inclusion_time: Int) -> Output {
  let key = user_events.out_ref_to_nonce(event_nonce)
  let facts: Data = Facts {
    event_id: event_nonce,
    inclusion_time,
    location: External {
      storage_datum_hash: #"f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1",
    },
    structural_lovelace: 3_000_000,
    structural_refund_key: next_operator,
  }
  let content: Data = Order { facts }
  let node: Data = Link {
    position: Key(key),
    next: None,
    protected_until: 0,
    payload: content,
  }
  Output {
    address: address.from_script(policy),
    value: assets.from_lovelace(10_000_000) |> assets.add(policy, key, 1),
    datum: InlineDatum(node),
    reference_script: None,
  }
}

fn tx_order_output(inclusion_time: Int) -> Output {
  let datum: user_events.OptimisticDatum<TxOrderEventV1> = user_events.OptimisticDatum {
    event: TxOrderEventV1 {
      id: event_nonce,
      tx: TxOrderPayloadV1 {
        tx_id: #"0404040404040404040404040404040404040404040404040404040404040404",
        transaction_commitment: #"0505050505050505050505050505050505050505050505050505050505050505",
        submitted_source: ForcedTxProofSourceV1 {
          compact_cbor: #"01",
          witness_set_compact_cbor: #"0203",
          field_preimage_lengths_cbor: #"04",
        },
      },
    },
    inclusion_time,
    witness: other_policy,
    refund_address: address.from_verification_key(next_operator),
    refund_datum: NoDatum,
  }
  Output {
    address: address.from_script(tx_order_policy),
    value: assets.from_lovelace(5_000_000)
      |> assets.add(
          tx_order_policy,
          user_events.out_ref_to_nonce(event_nonce),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn cited_event_output(cited: CitedEvent) -> Output {
  when cited is {
    CitedDeposit { inclusion_time } ->
      order_node_output(deposit_policy, inclusion_time)
    CitedWithdrawal { inclusion_time } ->
      order_node_output(withdrawal_policy, inclusion_time)
    CitedTxOrder { inclusion_time } -> tx_order_output(inclusion_time)
  }
}

fn neglected_user_event(cited: CitedEvent) -> NeglectedUserEvent {
  when cited is {
    CitedDeposit { .. } ->
      NeglectedDeposit { deposit_ref_input_index: event_ref_input_index }
    CitedWithdrawal { .. } ->
      NeglectedWithdrawal { withdrawal_ref_input_index: event_ref_input_index }
    CitedTxOrder { .. } ->
      NeglectedTxOrder { tx_order_ref_input_index: event_ref_input_index }
  }
}

/// The on-chain inactivity threshold for an event, recomputed independently:
/// the later of the shift's grace period end and the event's negligence
/// deadline.
fn threshold_of(inclusion_time: Int) -> Int {
  math.max(
    shift_start + env.new_shift_inactivity_grace_period,
    inclusion_time + env.user_events_negligence_timeout,
  )
}

// ## Strike transaction assembly

fn scheduler_input() -> Input {
  Input {
    output_reference: out_ref(inputs_transaction_id, scheduler_input_index),
    output: scheduler_output(
      ActiveOperator { operator: struck_operator, start_time: shift_start },
    ),
  }
}

fn struck_node_input() -> Input {
  Input {
    output_reference: out_ref(inputs_transaction_id, active_node_input_index),
    output: active_node_output(struck_operator, None, 0),
  }
}

fn active_node_redeemer() -> active_operators.SpendRedeemer {
  active_operators.StrikeForInactivity {
    active_node_input_index,
    active_node_output_index: 1,
    operator: struck_operator,
    active_node_link: None,
    scheduler_input_index,
    scheduler_redeemer_index: 0,
    hub_oracle_ref_input_index: hub_ref_input_index,
  }
}

/// The scheduler redeemer as raw data, with the cited-event field supplied by
/// the caller, so a fixture can carry an encoding the type no longer has.
fn raw_scheduler_redeemer(neglected: Data) -> Data {
  let index = fn(i: Int) -> Data { builtin.i_data(i) }
  builtin.constr_data(
    0,
    [
      index(scheduler_input_index),
      index(0),
      builtin.constr_data(
        2,
        [
          index(next_operator_node_ref_input_index),
          index(active_node_input_index),
          index(1),
          index(state_queue_ref_input_index),
          index(hub_ref_input_index),
          neglected,
        ],
      ),
    ],
  )
}

fn scheduler_redeemer(cited: CitedEvent) -> SpendRedeemer {
  SpendRedeemer {
    scheduler_input_index,
    scheduler_output_index: 0,
    advancing_approach: GoToNextDueToSkippedOperator {
      new_shifts_operator_node_ref_input_index: next_operator_node_ref_input_index,
      skipped_operator_node_input_index: active_node_input_index,
      active_operators_spend_redeemer_index: 1,
      state_queue_ref_input_index,
      hub_oracle_ref_input_index: hub_ref_input_index,
      neglected_user_event: neglected_user_event(cited),
    },
  }
}

/// One strike transaction: the validity range starts at `valid_from` and the
/// new shift starts at its inclusive upper bound.
fn strike_transaction(
  cited: CitedEvent,
  tail_end: Int,
  valid_from: Int,
  scheduler_redeemer_data: Data,
) -> Transaction {
  let valid_to = valid_from + validity_width
  let node_redeemer: Data = active_node_redeemer()
  Transaction {
    ..transaction.placeholder,
    inputs: [scheduler_input(), struck_node_input()],
    reference_inputs: [
      reference_input(hub_ref_input_index, hub_output()),
      reference_input(
        state_queue_ref_input_index,
        state_queue_tail_output(tail_end),
      ),
      reference_input(
        next_operator_node_ref_input_index,
        active_node_output(next_operator, Some(struck_operator), 0),
      ),
      reference_input(event_ref_input_index, cited_event_output(cited)),
    ],
    outputs: [
      scheduler_output(
        ActiveOperator { operator: next_operator, start_time: valid_to },
      ),
      active_node_output(struck_operator, None, 1),
    ],
    redeemers: [
      Pair(Spend(scheduler_input().output_reference), scheduler_redeemer_data),
      Pair(Spend(struck_node_input().output_reference), node_redeemer),
    ],
    validity_range: interval.between(valid_from, valid_to),
  }
}

/// Runs the scheduler and the active-operators spending validators over the
/// same transaction.
fn strike_is_accepted(tx: Transaction, redeemer: SpendRedeemer) -> Bool {
  let scheduler_datum: SchedDatum = ActiveOperator {
    operator: struck_operator,
    start_time: shift_start,
  }
  and {
    scheduler_validator.spend.spend(
      registered_operators_policy,
      address.from_script(active_operators_policy),
      active_operators_policy,
      scheduler_policy,
      hub_policy,
      Some(scheduler_datum),
      redeemer,
      scheduler_input().output_reference,
      tx,
    ),
    active_operators_validator.spend.spend(
      active_operators_policy,
      hub_policy,
      None,
      active_node_redeemer(),
      struck_node_input().output_reference,
      tx,
    ),
  }
}

/// A strike citing `cited` against a tail ending at `tail_end`, with the
/// validity range starting at `valid_from`.
fn strike(cited: CitedEvent, tail_end: Int, valid_from: Int) -> Bool {
  let redeemer = scheduler_redeemer(cited)
  let redeemer_data: Data = redeemer
  strike_is_accepted(
    strike_transaction(cited, tail_end, valid_from, redeemer_data),
    redeemer,
  )
}

/// A strike whose validity range starts just after the event's threshold.
fn strike_just_after_threshold(cited: CitedEvent, tail_end: Int) -> Bool {
  strike(cited, tail_end, threshold_of(inclusion_time_of(cited)) + 1)
}

// ## Neglected work past the negligence timeout is strikable

/// The tail ends as the shift starts; the event lands one second later.
const undelivered_inclusion = shift_start + 1_000

test scheduler_strike_accepts_neglected_deposit_past_the_timeout() {
  strike_just_after_threshold(
    CitedDeposit { inclusion_time: undelivered_inclusion },
    shift_start,
  )
}

test scheduler_strike_accepts_neglected_withdrawal_past_the_timeout() {
  strike_just_after_threshold(
    CitedWithdrawal { inclusion_time: undelivered_inclusion },
    shift_start,
  )
}

test scheduler_strike_accepts_neglected_tx_order_past_the_timeout() {
  strike_just_after_threshold(
    CitedTxOrder { inclusion_time: undelivered_inclusion },
    shift_start,
  )
}

// ## Neglected work not yet past the timeout is not strikable

test scheduler_strike_rejects_neglected_deposit_at_the_timeout() fail {
  let cited = CitedDeposit { inclusion_time: undelivered_inclusion }
  strike(cited, shift_start, threshold_of(undelivered_inclusion))
}

test scheduler_strike_rejects_neglected_withdrawal_at_the_timeout() fail {
  let cited = CitedWithdrawal { inclusion_time: undelivered_inclusion }
  strike(cited, shift_start, threshold_of(undelivered_inclusion))
}

test scheduler_strike_rejects_neglected_tx_order_at_the_timeout() fail {
  let cited = CitedTxOrder { inclusion_time: undelivered_inclusion }
  strike(cited, shift_start, threshold_of(undelivered_inclusion))
}

// ## An idle network is not strikable

/// The tail's own end time: the tail block (here the confirmed state) covers
/// an event included exactly then, so the event is delivered.
test scheduler_strike_rejects_event_the_tail_delivered() fail {
  strike_just_after_threshold(
    CitedDeposit { inclusion_time: shift_start },
    shift_start,
  )
}

/// The control for the case above: one millisecond later the event is
/// undelivered and the same strike is accepted.
test scheduler_strike_accepts_event_just_after_the_tail() {
  strike_just_after_threshold(
    CitedDeposit { inclusion_time: shift_start + 1 },
    shift_start,
  )
}

/// An idle network well past the deleted commitment-gap threshold: the only
/// user event on chain was delivered before the tail ended, so there is
/// nothing to cite and the strike is refused however late it lands.
test scheduler_strike_rejects_idle_tail_past_the_former_commitment_gap() fail {
  let tail_end = shift_start
  let valid_from = math.max(
    tail_end + former_commitment_gap,
    threshold_of(tail_end - 5_000),
  ) + 1
  strike(
    CitedTxOrder { inclusion_time: tail_end - 5_000 },
    tail_end,
    valid_from,
  )
}

/// The deleted arm's encoding (constructor 0, no fields) no longer decodes as
/// a cited event, so the scheduler redeemer it rides in is refused.
test scheduler_strike_rejects_the_deleted_no_event_encoding() fail {
  let raw = raw_scheduler_redeemer(builtin.constr_data(0, []))
  expect redeemer: SpendRedeemer = raw
  let cited = CitedDeposit { inclusion_time: undelivered_inclusion }
  strike_is_accepted(
    strike_transaction(
      cited,
      shift_start,
      threshold_of(undelivered_inclusion) + 1,
      raw,
    ),
    redeemer,
  )
}

/// The control for the case above: the same raw redeemer citing the deposit
/// (constructor 0 with its reference-input index) is accepted.
test scheduler_strike_accepts_the_raw_deposit_encoding() {
  let raw = raw_scheduler_redeemer(
    builtin.constr_data(0, [builtin.i_data(event_ref_input_index)]),
  )
  expect redeemer: SpendRedeemer = raw
  let cited = CitedDeposit { inclusion_time: undelivered_inclusion }
  strike_is_accepted(
    strike_transaction(
      cited,
      shift_start,
      threshold_of(undelivered_inclusion) + 1,
      raw,
    ),
    redeemer,
  )
}

// ## The shift-start grace period

/// An event left over from an earlier shift is already past its negligence
/// deadline when the shift starts, so the grace period decides the threshold.
const leftover_tail_end = shift_start - 2_000_000

test scheduler_strike_accepts_leftover_event_after_the_grace_period() {
  let cited = CitedDeposit { inclusion_time: leftover_tail_end + 1 }
  and {
    threshold_of(leftover_tail_end + 1) == shift_start + env.new_shift_inactivity_grace_period,
    strike_just_after_threshold(cited, leftover_tail_end),
  }
}

test scheduler_strike_rejects_leftover_event_within_the_grace_period() fail {
  let cited = CitedDeposit { inclusion_time: leftover_tail_end + 1 }
  strike(
    cited,
    leftover_tail_end,
    shift_start + env.new_shift_inactivity_grace_period,
  )
}

// ## The threshold must fall inside the convicted shift

/// An event whose negligence deadline is the shift's end convicts no one in
/// this shift.
test scheduler_strike_rejects_event_overdue_only_at_the_shift_end() fail {
  let inclusion_time = shift_start + env.shift_duration - env.user_events_negligence_timeout
  strike_just_after_threshold(CitedDeposit { inclusion_time }, shift_start)
}

test scheduler_strike_accepts_event_overdue_just_before_the_shift_end() {
  let inclusion_time = shift_start + env.shift_duration - env.user_events_negligence_timeout - 1
  strike_just_after_threshold(CitedDeposit { inclusion_time }, shift_start)
}
