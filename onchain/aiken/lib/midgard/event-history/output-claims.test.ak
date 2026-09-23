use aiken/collection/list
use cardano/address.{Script}
use cardano/assets
use cardano/transaction.{Input, Output, Transaction, Withdraw, placeholder}
use midgard/event_history/actions
use midgard/event_history/output_claims

const own = #"11111111111111111111111111111111111111111111111111111111"

const other = #"22222222222222222222222222222222222222222222222222222222"

fn reclaim(predecessor: Int, refund: Int) -> actions.Observe {
  actions.Apply {
    hub_reference_index: 0,
    operation: actions.ReclaimFiller {
      predecessor_input_index: 0,
      filler_input_index: 1,
      predecessor_output_index: predecessor,
      refund_output_index: refund,
    },
  }
}

fn outputs() -> List<Output> {
  list.repeat(
    Output {
      address: address.from_script(own),
      value: assets.from_lovelace(3000000),
      datum: transaction.NoDatum,
      reference_script: None,
    },
    6,
  )
}

fn paired(declared: actions.Observe) -> Transaction {
  let raw: Data = declared
  Transaction {
    ..placeholder,
    outputs: outputs(),
    withdrawals: [Pair(Script(other), 0)],
    redeemers: [Pair(Withdraw(Script(other)), raw)],
  }
}

test distinct_cross_kind_refunds_and_continuations_pass() {
  output_claims.exclusive(paired(reclaim(2, 3)), own, other, reclaim(0, 1))
}

test shared_cross_kind_refund_fails() {
  !output_claims.exclusive(paired(reclaim(2, 1)), own, other, reclaim(0, 1))
}

test payment_cannot_reuse_another_continuation() {
  !output_claims.exclusive(paired(reclaim(2, 0)), own, other, reclaim(0, 1))
}

test duplicate_claim_within_one_operation_fails() {
  !output_claims.exclusive(paired(reclaim(2, 3)), own, other, reclaim(0, 0))
}

test negative_output_index_fails() {
  !output_claims.exclusive(paired(reclaim(2, 3)), own, other, reclaim(0, -1))
}

test nonexistent_output_index_fails() {
  !output_claims.exclusive(paired(reclaim(2, 3)), own, other, reclaim(0, 6))
}

test absent_other_kind_allows_unrelated_change() {
  output_claims.exclusive(
    Transaction { ..placeholder, outputs: outputs() },
    own,
    other,
    reclaim(0, 1),
  )
}

test nonzero_other_withdrawal_is_not_authority() fail {
  let tx = paired(reclaim(2, 3))
  output_claims.exclusive(
    Transaction { ..tx, withdrawals: [Pair(Script(other), 1)] },
    own,
    other,
    reclaim(0, 1),
  )
}

test redeemer_without_other_withdrawal_cannot_hide_mint() fail {
  let tx = paired(reclaim(2, 3))
  output_claims.exclusive(
    Transaction {
      ..tx,
      withdrawals: [],
      mint: assets.from_asset(other, #"", 1),
    },
    own,
    other,
    reclaim(0, 1),
  )
}

test other_withdrawal_requires_its_actual_redeemer() fail {
  let tx = paired(reclaim(2, 3))
  output_claims.exclusive(
    Transaction { ..tx, redeemers: [] },
    own,
    other,
    reclaim(0, 1),
  )
}

test another_script_redeemer_is_not_other_kind_authority() fail {
  let raw: Data = reclaim(2, 3)
  let tx = paired(reclaim(2, 3))
  output_claims.exclusive(
    Transaction { ..tx, redeemers: [Pair(Withdraw(Script(own)), raw)] },
    own,
    other,
    reclaim(0, 1),
  )
}

test duplicate_other_redeemer_is_rejected() fail {
  let tx = paired(reclaim(2, 3))
  output_claims.exclusive(
    Transaction { ..tx, redeemers: list.concat(tx.redeemers, tx.redeemers) },
    own,
    other,
    reclaim(0, 1),
  )
}

test consumed_other_list_cannot_be_hidden_without_withdrawal() fail {
  let input = Input {
    output_reference: transaction.OutputReference {
      transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(other),
      value: assets.from_lovelace(3000000),
      datum: transaction.NoDatum,
      reference_script: None,
    },
  }
  output_claims.exclusive(
    Transaction { ..placeholder, inputs: [input], outputs: outputs() },
    own,
    other,
    reclaim(0, 1),
  )
}

test retired_structural_refund_is_a_separate_claim() {
  let retirement = actions.Apply {
    hub_reference_index: 0,
    operation: actions.RetireOrder {
      predecessor_output_index: 2,
      funds_output_index: 3,
      structural_refund_output_index: Some(1),
    },
  }
  !output_claims.exclusive(paired(retirement), own, other, reclaim(0, 1))
}

test distinct_retirement_funds_and_structural_refund_pass() {
  let retirement = actions.Apply {
    hub_reference_index: 0,
    operation: actions.RetireOrder {
      predecessor_output_index: 2,
      funds_output_index: 3,
      structural_refund_output_index: Some(4),
    },
  }
  output_claims.exclusive(paired(retirement), own, other, reclaim(0, 1))
}

test initialization_claim_cannot_reuse_a_payment() {
  let initialization = actions.Initialize {
    nonce_input_index: 0,
    root_output_index: 1,
  }
  !output_claims.exclusive(paired(initialization), own, other, reclaim(0, 1))
}

test two_kinds_require_distinct_deployment_policies() fail {
  output_claims.exclusive(paired(reclaim(2, 3)), own, own, reclaim(0, 1))
}
