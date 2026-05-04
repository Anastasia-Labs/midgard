use aiken/builtin
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use midgard/common/types.{PosixTime}
use midgard/operator_directory

/// TODO: Refer to state queue's library module.
pub const root_asset_name: AssetName = "MIDGARD_REGISTERED_OPERATORS"

/// Short for "Midgard Registered". In hex it will be: #"4d524547"
///
/// > [!NOTE]
/// > This should not be longer than 4 bytes, as each node's asset name carries
/// > this prefix, followed by its activation-time key.
pub const node_asset_name_prefix: AssetName = "MREG"

pub const node_asset_name_prefix_length: Int =
  bytearray.length(node_asset_name_prefix)

pub type NodeData {
  operator: VerificationKeyHash,
}

pub type Datum =
  operator_directory.Datum<NodeData>

pub type DuplicateOperatorStatus {
  DuplicateIsRegistered
  DuplicateIsActive { hub_oracle_ref_input_index: Int }
  DuplicateIsRetired
}

pub type MintRedeemer {
  Init { output_index: Int }
  Deinit { input_index: Int }
  RegisterOperator {
    registering_operator: VerificationKeyHash,
    root_input_index: Int,
    root_output_index: Int,
    registered_node_output_index: Int,
    hub_oracle_ref_input_index: Int,
    active_operators_element_ref_input_index: Int,
    retired_operators_element_ref_input_index: Int,
  }
  ActivateOperator {
    activating_operator: VerificationKeyHash,
    anchor_element_input_index: Int,
    removed_node_input_index: Int,
    anchor_element_output_index: Int,
    hub_oracle_ref_input_index: Int,
    retired_operators_element_ref_input_index: Int,
    active_operators_redeemer_index: Int,
  }
  DeregisterOperator {
    deregistering_operator: VerificationKeyHash,
    anchor_element_input_index: Int,
    removed_node_input_index: Int,
    anchor_element_output_index: Int,
  }
  SlashDuplicateOperator {
    duplicate_operator: VerificationKeyHash,
    anchor_element_input_index: Int,
    removed_node_input_index: Int,
    anchor_element_output_index: Int,
    duplicate_node_ref_input_index: Int,
    duplicate_operator_status: DuplicateOperatorStatus,
  }
}

/// Converts the given integer to a big endian byte array, in as few bytes as
/// possible (TODO?).
pub fn activation_time_to_node_key(posix_time: PosixTime) -> ByteArray {
  builtin.integer_to_bytearray(True, 0, posix_time)
}

pub fn node_key_to_activation_time(node_key: ByteArray) -> PosixTime {
  builtin.bytearray_to_integer(True, node_key)
}

pub fn finalize_linked_list(
  eval: linked_list.Eval,
  nft_policy_id: PolicyId,
) -> Bool {
  eval
    |> linked_list.run_eval_with(
        nft_policy_id,
        root_asset_name,
        node_asset_name_prefix,
        node_asset_name_prefix_length,
      )
}
