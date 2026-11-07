//// Output obligations are derived from each executed operation. An observer
//// validates its own operation and authenticates the other kind's observer by
//// the deployed policy and exact zero withdrawal, never by a supplied subset.

use aiken/collection/dict
use aiken/collection/list
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/common/utils
use midgard/event_history/actions

pub fn operation_outputs(operation: actions.Operation) -> List<Int> {
  when operation is {
    actions.InsertFiller { predecessor_output_index, filler_output_index, .. } ->
      [predecessor_output_index, filler_output_index]
    actions.InsertOrder { predecessor_output_index, order_output_index, .. } ->
      [predecessor_output_index, order_output_index]
    actions.PromoteFiller { order_output_index, refund_output_index, .. } ->
      [order_output_index, refund_output_index]
    actions.ReclaimFiller { predecessor_output_index, refund_output_index, .. } ->
      [predecessor_output_index, refund_output_index]
    actions.RetireOrder {
      predecessor_output_index,
      funds_output_index,
      structural_refund_output_index,
    } -> {
      let required = [predecessor_output_index, funds_output_index]
      when structural_refund_output_index is {
        None -> required
        Some(index) -> [index, ..required]
      }
    }
  }
}

pub fn observer_outputs(observer: actions.Observe) -> List<Int> {
  when observer is {
    actions.Initialize { root_output_index, .. } -> [root_output_index]
    actions.Apply { operation, .. } -> operation_outputs(operation)
  }
}

pub fn distinct_valid_outputs(indices: List<Int>, output_count: Int) -> Bool {
  and {
    list.all(indices, fn(index) { index >= 0 && index < output_count }),
    list.length(list.unique(indices)) == list.length(indices),
  }
}

/// Both policy invocations run their complete semantic checks. Reading the
/// other's public output claims is safe only because that same invocation must
/// execute and validate the claims, including its delegated retirement witness.
pub fn exclusive(
  tx: Transaction,
  own_policy: PolicyId,
  other_policy: PolicyId,
  own_observer: actions.Observe,
) -> Bool {
  expect own_policy != other_policy
  let own = observer_outputs(own_observer)
  let others = when
    list.find(tx.withdrawals, fn(pair) { pair.1st == Script(other_policy) })
  is {
    None -> {
      expect dict.is_empty(assets.tokens(tx.mint, other_policy))
      expect
        !list.any(
          tx.inputs,
          fn(input) {
            input.output.address.payment_credential == Script(other_policy) || !dict.is_empty(
              assets.tokens(input.output.value, other_policy),
            )
          },
        )
      []
    }
    Some(Pair(_, amount)) -> {
      expect amount == 0
      expect other: actions.Observe = utils.get_unique_withdraw_redeemer(
        tx.redeemers,
        other_policy,
      )
      observer_outputs(other)
    }
  }
  distinct_valid_outputs(list.concat(own, others), list.length(tx.outputs))
}
