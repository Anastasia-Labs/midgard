use midgard/common/types.{POSIXTime}

pub type Datum {
  commitment_time: Option<POSIXTime>,
}

pub type SpendRedeemer {
  ListStateTransition
  UpdateCommitmentTime {
    active_node_input_index: Int,
    prev_active_node_ref_input_index: Int,
    hub_oracle_ref_input_index: Int,
    state_queue_redeemer_index: Int,
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
  RemoveOperatorSlashBond {
    slashed_active_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_slashed_node_input_index: Int,
    active_operator_anchor_node_input_index: Int,
    state_queue_redeemer_index: Int,
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
