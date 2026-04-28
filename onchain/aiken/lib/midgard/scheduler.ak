use aiken/builtin
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
  NoNeglectedUserEvent
  NeglectedDeposit { deposit_ref_input_index: Int }
  NeglectedWithdrawal { withdrawal_ref_input_index: Int }
  NeglectedTxOrder { tx_order_ref_input_index: Int }
}

pub type AdvancingApproach {
  GoToNextDueToEndOfShift { new_shifts_operator_node_ref_input_index: Int }
  RewindDueToEndOfShift {
    active_operators_root_ref_input_index: Int,
    active_operators_last_node_ref_input_index: Int,
    registered_element_ref_input_index: Int,
  }
  GoToNextDueToSkippedOperator {
    new_shifts_operator_node_ref_input_index: Int,
    skipped_operator_node_input_index: Int,
    active_operators_spend_redeemer_index: Int,
    state_queue_ref_input_index: Int,
    hub_oracle_ref_input_index: Int,
    neglected_user_event: NeglectedUserEvent,
  }
  RewindDueToSkippedOperator {
    active_operators_root_ref_input_index: Int,
    skipped_operator_node_input_index: Int,
    active_operators_spend_redeemer_index: Int,
    state_queue_ref_input_index: Int,
    hub_oracle_ref_input_index: Int,
    // If only one operator exists in the active set, we won't be able to refer
    // to the operator's UTxO as the next shift's, since it's being spent in the
    // transaction.
    m_active_operators_last_node_ref_input_index: Option<Int>,
    registered_element_ref_input_index: Int,
    neglected_user_event: NeglectedUserEvent,
  }
  GoToNextDueToOperatorRemoval {
    active_operators_mint_redeemer_index: Int,
    removal_reason: OperatorRemovalReason,
  }
  RewindDueToOperatorRemoval {
    active_operators_mint_redeemer_index: Int,
    m_active_operators_last_node_ref_input_index: Option<Int>,
    removal_reason: OperatorRemovalReason,
    registered_element_ref_input_index: Int,
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
  let scheduler_datum_data =
    utils.get_authentic_input_datum_with_nft_at(
      reference_inputs,
      scheduler_policy_id,
      asset_name,
      scheduler_ref_input_index,
    )
  // HACKY FIX: Using `expect` fails to pass current LE's local UPC evaluation
  //            This is just there as a workaround.
  let Pair(constr_idx, constr_fields) = builtin.un_constr_data(scheduler_datum_data)
  if constr_idx == 0 {
    NoActiveOperators
  } else {
    ActiveOperator {
      operator: constr_fields |> builtin.head_list |> builtin.un_b_data,
      start_time: constr_fields |> builtin.tail_list |> builtin.head_list |> builtin.un_i_data,
    }
  }
}
