use aiken/crypto.{ScriptHash, VerificationKeyHash}
use cardano/transaction.{Input}
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/protocol_parameters.{shift_duration}

pub const asset_name = "MIDGARD_SCHEDULER"

pub type Datum {
  NoActiveOperators
  ActiveOperator { operator: VerificationKeyHash, start_time: PosixTime }
}

pub type MintRedeemer {
  Init
  Deinit
}

pub type OperatorRemovalReason {
  OperatorRetirement
  OperatorSlashing
}

pub type NeglectedUserEvent {
  NoNeglecteddUserEvent
  NeglectedDeposit { deposit_ref_input_index: Int }
  NeglectedWithdrawal { withdrawal_ref_input_index: Int }
  NeglectedTxOrder { tx_order_ref_input_index: Int }
}

pub type AdvancingApproach {
  RewindDueToEndOfShift {
    active_operators_root_ref_input_index: Int,
    active_operators_last_node_ref_input_index: Int,
    registered_element_ref_input_index: Int,
  }
  GoToNextDueToEndOfShift { new_shifts_operator_node_ref_input_index: Int }
  RewindDueToSkippedOperator {
    active_operators_root_ref_input_index: Int,
    skipped_operator_node_input_index: Int,
    active_operators_spend_redeemer_index: Int,
    state_queue_ref_input_index: Int,
    hub_oracle_ref_input_index: Int,
    // If only one operator exists in the active set, we won't want to refer to
    // the new shift's operator UTxO. TODO? I'm assuming we can't refer to a
    // UTxO that's getting spent in the same transaction.
    m_active_operators_last_node_ref_input_index: Option<Int>,
    registered_element_ref_input_index: Int,
    neglected_user_event: NeglectedUserEvent,
  }
  GoToNextDueToSkippedOperator {
    new_shifts_operator_node_ref_input_index: Int,
    skipped_operator_node_input_index: Int,
    active_operators_spend_redeemer_index: Int,
    state_queue_ref_input_index: Int,
    hub_oracle_ref_input_index: Int,
  }
  RewindDueToOperatorRemoval {
    active_operators_last_node_ref_input_index: Int,
    active_operators_mint_redeemer_index: Int,
    m_active_operators_last_node_ref_input_index: Option<Int>,
    removal_reason: OperatorRemovalReason,
    registered_element_ref_input_index: Int,
  }
  GoToNextDueToOperatorRemoval {
    active_operators_mint_redeemer_index: Int,
    removal_reason: OperatorRemovalReason,
  }
  AppointFirstOperator {
    new_shifts_operator_node_ref_input_index: Int,
    registered_element_ref_input_index: Int,
  }
}

pub type SpendRedeemer {
  scheduler_input_index: Int,
  scheduler_output_index: Int,
  advancing_approach: AdvancingApproach,
}

/// The end of a shift is the start of the shift, shifted forward by the
/// `shift_duration` parameter.
pub fn shift_end_time(start_time: PosixTime) -> PosixTime {
  start_time + shift_duration
}

pub fn get_datum(
  reference_inputs: List<Input>,
  scheduler_policy_id: ScriptHash,
  scheduler_ref_input_index: Int,
) -> Datum {
  expect scheduler_datum: Datum =
    utils.get_authentic_input_datum_with_nft_at(
      reference_inputs,
      scheduler_policy_id,
      asset_name,
      scheduler_ref_input_index,
    )
  scheduler_datum
}
