use aiken_design_patterns/linked_list
use midgard/common/types.{PosixTime}

pub type NodeData {
  bond_unlock_time: Option<PosixTime>,
}

pub type Datum =
  linked_list.Element<Void, NodeData>

pub type SpendRedeemer {
  ListStateTransition
  UpdateBondHoldNewState {
    active_node_output_index: Int,
    hub_oracle_ref_input_index: Int,
    state_queue_redeemer_index: Int,
  }
  UpdateBondHoldNewSettlement {
    active_node_output_index: Int,
    hub_oracle_ref_input_index: Int,
    settlement_input_index: Int,
    settlement_redeemer_index: Int,
    new_bond_unlock_time: PosixTime,
  }
}

pub type MintRedeemer {
  Init
  Deinit
  ActivateOperator {
    new_active_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_appended_node_output_index: Int,
    active_operator_anchor_node_output_index: Int,
    registered_operators_redeemer_index: Int,
  }
  RemoveOperatorBadState {
    slashed_active_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_slashed_node_input_index: Int,
    active_operator_anchor_node_input_index: Int,
    state_queue_redeemer_index: Int,
  }
  RemoveOperatorBadSettlement {
    slashed_active_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_slashed_node_input_index: Int,
    active_operator_anchor_node_input_index: Int,
    settlement_input_index: Int,
    settlement_redeemer_index: Int,
  }
  RetireOperator {
    active_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_removed_node_input_index: Int,
    active_operator_anchor_node_input_index: Int,
    retired_operator_inserted_node_output_index: Int,
    retired_operators_redeemer_index: Int,
  }
}
