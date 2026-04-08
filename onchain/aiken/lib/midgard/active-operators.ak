use aiken/builtin
use aiken/collection/list
use midgard/common/types.{PosixTime}

pub type Datum {
  bond_unlock_time: Option<PosixTime>,
}

// Active-operator node data is stored as raw `Data` inside the shared linked-list
// node datum. Decode the single record field explicitly from that raw payload
// instead of relying on a direct typed cast here.
/// Returns the bond unlock time encoded in raw data.
/// Extracts the bond unlock time from serialized datum data.
pub fn bond_unlock_time_from_data(data: Data) -> Option<PosixTime> {
  let datum_fields = builtin.un_constr_data(data).2nd
  expect Some(bond_unlock_time_data) = list.at(datum_fields, 0)
  expect bond_unlock_time: Option<PosixTime> = bond_unlock_time_data
  bond_unlock_time
}

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
