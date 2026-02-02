use aiken/crypto.{VerificationKeyHash}
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName}
use midgard/common/types.{PosixTime}

/// TODO: Refer to state queue's library module.
pub const root_asset_name: AssetName = "MIDGARD_REGISTERED_OPERATORS"

/// Short for "Midgard Registered". In hex it will be: #"4d524753"
///
/// > [!NOTE]
/// > This should not be longer than 4 bytes, as each node's asset name carries
/// > this prefix, followed by its operator's public key hash, which is 28 bytes
/// > long.
pub const node_asset_name_prefix: AssetName = "MRGS"

pub type NodeData {
  bond_unlock_time: Option<PosixTime>,
}

pub type Datum =
  linked_list.Element<Void, NodeData>

pub type OperatorStatus {
  Registered
  Active
  Retired
}

pub type MintRedeemer {
  Init { output_index: Int }
  Deinit { input_index: Int }
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
    operator_to_activate: VerificationKeyHash,
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
