use midgard/common/types.{PosixTime}

pub type Datum {
  bond_unlock_time: Option<PosixTime>,
}

pub type MintRedeemer {
  Init
  Deinit
  RetireOperator {
    new_retired_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    retired_operator_appended_node_output_index: Int,
    retired_operator_anchor_node_output_index: Int,
    active_operators_redeemer_index: Int,
  }
  RecoverOperatorBond {
    retired_operator_key: ByteArray,
    removed_node_input_index: Int,
    anchor_node_input_index: Int,
  }
  RemoveOperatorBadState {
    slashed_retired_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    retired_operator_slashed_node_input_index: Int,
    retired_operator_anchor_node_input_index: Int,
    state_queue_redeemer_index: Int,
  }
  RemoveOperatorBadSettlement {
    slashed_retired_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    retired_operator_slashed_node_input_index: Int,
    retired_operator_anchor_node_input_index: Int,
    settlement_input_index: Int,
    settlement_redeemer_index: Int,
  }
}
