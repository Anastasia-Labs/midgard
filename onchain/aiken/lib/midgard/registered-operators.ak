use aiken/builtin
use aiken/collection/list
use midgard/common/types.{PosixTime}

pub type Datum {
  activation_time: PosixTime,
}

/// Returns the activation time encoded in raw data.
/// Extracts the activation time from serialized datum data.
pub fn activation_time_from_data(data: Data) -> PosixTime {
  let datum_fields = builtin.un_constr_data(data).2nd
  expect Some(activation_time_data) = list.at(datum_fields, 0)
  expect activation_time: PosixTime = activation_time_data
  activation_time
}

pub type OperatorStatus {
  Registered
  Active
  Retired
}

pub type MintRedeemer {
  Init
  Deinit
  Register {
    key_to_prepend: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_ref_input_index: Int,
    active_operator_asset_name: ByteArray,
    retired_operator_ref_input_index: Int,
    retired_operator_asset_name: ByteArray,
    prepended_node_output_index: Int,
    anchor_node_output_index: Int,
  }
  Activate {
    node_to_activate_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    retired_operator_ref_input_index: Int,
    retired_operator_asset_name: ByteArray,
    removed_node_input_index: Int,
    anchor_node_input_index: Int,
    active_operators_inserted_node_output_index: Int,
    active_operators_anchor_node_output_index: Int,
  }
  Deregister {
    node_to_deregister_key: ByteArray,
    removed_node_input_index: Int,
    anchor_node_input_index: Int,
  }
  RemoveDuplicateSlashBond {
    duplicate_node_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    duplicate_node_ref_input_index: Int,
    duplicate_node_ref_input_asset_name: ByteArray,
    removed_node_input_index: Int,
    anchor_node_input_index: Int,
    witness_status: OperatorStatus,
  }
}
