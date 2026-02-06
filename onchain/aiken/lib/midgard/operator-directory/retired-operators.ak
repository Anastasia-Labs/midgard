use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use midgard/common/types.{PosixTime}
use midgard/operator_directory

/// TODO: Refer to state queue's library module.
pub const root_asset_name: AssetName = "MIDGARD_RETIRED_OPERATORS"

/// Short for "Midgard Retired". In hex it will be: #"4d524554"
///
/// > [!NOTE]
/// > This should not be longer than 4 bytes, as each node's asset name carries
/// > this prefix, followed by its operator's public key hash, which is 28 bytes
/// > long.
pub const node_asset_name_prefix: AssetName = "MRET"

pub const node_asset_name_prefix_length: Int =
  bytearray.length(node_asset_name_prefix)

pub type NodeData {
  bond_unlock_time: Option<PosixTime>,
}

pub type Datum =
  operator_directory.Datum<NodeData>

pub type MintRedeemer {
  Init { output_index: Int }
  Deinit { input_index: Int }
  RetireOperator {
    new_retired_operator_key: ByteArray,
    bond_unlock_time: Option<PosixTime>,
    hub_oracle_ref_input_index: Int,
    retired_operator_anchor_element_input_index: Int,
    retired_operator_anchor_element_output_index: Int,
    retired_operator_inserted_node_output_index: Int,
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
