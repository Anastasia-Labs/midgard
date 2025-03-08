use midgard/common/types.{PosixTime}
use midgard/protocol_parameters.{registration_duration}

pub type Datum {
  registration_time: PosixTime,
}

pub type WitnessStatus {
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
    witness_status: WitnessStatus,
  }
}

/// A registered operator is eligible to active when the registration_duration
/// elapses after the operator's registration time.
pub fn eligible_activation_time(registration_time: PosixTime) -> PosixTime {
  registration_time + registration_duration
}
