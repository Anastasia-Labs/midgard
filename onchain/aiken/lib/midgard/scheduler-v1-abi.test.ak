use aiken/cbor
use midgard/scheduler

test canonical_scheduler_v1_typescript_abi_vector() {
  and {
    cbor.serialise(scheduler.NoActiveOperators) == #"d87980",
    cbor.serialise(scheduler.ActiveOperator { operator: #"aa", start_time: 1 }) == #"d87a9f41aa01ff",
    cbor.serialise(scheduler.Init) == #"d87980",
    cbor.serialise(scheduler.Deinit) == #"d87a80",
    cbor.serialise(scheduler.NoNeglectedUserEvent) == #"d87980",
    cbor.serialise(scheduler.NeglectedDeposit { deposit_ref_input_index: 1 }) == #"d87a9f01ff",
    cbor.serialise(
      scheduler.NeglectedWithdrawal { withdrawal_ref_input_index: 2 },
    ) == #"d87b9f02ff",
    cbor.serialise(scheduler.NeglectedTxOrder { tx_order_ref_input_index: 3 }) == #"d87c9f03ff",
    cbor.serialise(scheduler.OperatorRetirement) == #"d87980",
    cbor.serialise(scheduler.OperatorSlashing) == #"d87a80",
    cbor.serialise(
      scheduler.GoToNextDueToEndOfShift {
        new_shifts_operator_node_ref_input_index: 1,
      },
    ) == #"d8799f01ff",
    cbor.serialise(
      scheduler.RewindDueToEndOfShift {
        active_operators_root_ref_input_index: 1,
        active_operators_last_node_ref_input_index: 2,
        registered_element_ref_input_index: 3,
      },
    ) == #"d87a9f010203ff",
    cbor.serialise(
      scheduler.GoToNextDueToSkippedOperator {
        new_shifts_operator_node_ref_input_index: 1,
        skipped_operator_node_input_index: 2,
        active_operators_spend_redeemer_index: 3,
        state_queue_ref_input_index: 4,
        hub_oracle_ref_input_index: 5,
        neglected_user_event: scheduler.NeglectedDeposit {
          deposit_ref_input_index: 6,
        },
      },
    ) == #"d87b9f0102030405d87a9f06ffff",
    cbor.serialise(
      scheduler.RewindDueToSkippedOperator {
        active_operators_root_ref_input_index: 1,
        skipped_operator_node_input_index: 2,
        active_operators_spend_redeemer_index: 3,
        state_queue_ref_input_index: 4,
        hub_oracle_ref_input_index: 5,
        m_active_operators_last_node_ref_input_index: Some(6),
        registered_element_ref_input_index: 7,
        neglected_user_event: scheduler.NeglectedWithdrawal {
          withdrawal_ref_input_index: 8,
        },
      },
    ) == #"d87c9f0102030405d8799f06ff07d87b9f08ffff",
    cbor.serialise(
      scheduler.GoToNextDueToOperatorRemoval {
        active_operators_mint_redeemer_index: 1,
        removal_reason: scheduler.OperatorRetirement,
      },
    ) == #"d87d9f01d87980ff",
    cbor.serialise(
      scheduler.RewindDueToOperatorRemoval {
        active_operators_mint_redeemer_index: 1,
        m_active_operators_last_node_ref_input_index: None,
        removal_reason: scheduler.OperatorSlashing,
        registered_element_ref_input_index: 2,
      },
    ) == #"d87e9f01d87a80d87a8002ff",
    cbor.serialise(
      scheduler.AppointFirstOperator {
        new_shifts_operator_node_ref_input_index: 1,
        registered_element_ref_input_index: 2,
      },
    ) == #"d87f9f0102ff",
    cbor.serialise(
      scheduler.SpendRedeemer {
        scheduler_input_index: 1,
        scheduler_output_index: 2,
        advancing_approach: scheduler.AppointFirstOperator {
          new_shifts_operator_node_ref_input_index: 3,
          registered_element_ref_input_index: 4,
        },
      },
    ) == #"d8799f0102d87f9f0304ffff",
  }
}
