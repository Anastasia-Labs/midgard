use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use midgard/common/types.{PosixTime}
use midgard/common/utils

/// TODO: Refer to state queue's library module.
pub const root_asset_name: AssetName = "MIDGARD_ACTIVE_OPERATORS"

/// Short for "Midgard Active". In hex it will be: #"4d414354"
///
/// > [!NOTE]
/// > This must consist of 4 bytes. Refer to state queue's library module for
/// > more details.
pub const node_asset_name_prefix: AssetName = "MACT"

pub const node_asset_name_prefix_length: Int =
  bytearray.length(node_asset_name_prefix)

pub type NodeData {
  bond_unlock_time: Option<PosixTime>,
}

pub type Datum =
  linked_list.Element<Void, NodeData>

pub type SpendRedeemer {
  ListStateTransition
  UpdateBondHoldNewState {
    active_node_input_index: Int,
    active_node_output_index: Int,
    hub_oracle_ref_input_index: Int,
    state_queue_input_index: Int,
    state_queue_redeemer_index: Int,
  }
  UpdateBondHoldNewSettlement {
    active_node_input_index: Int,
    active_node_output_index: Int,
    hub_oracle_ref_input_index: Int,
    settlement_input_index: Int,
    settlement_redeemer_index: Int,
    new_bond_unlock_time: PosixTime,
  }
}

pub type MintRedeemer {
  Init { output_index: Int }
  Deinit { input_index: Int }
  ActivateOperator {
    new_active_operator_key: VerificationKeyHash,
    hub_oracle_ref_input_index: Int,
    active_operator_anchor_element_input_index: Int,
    active_operator_anchor_element_output_index: Int,
    active_operator_inserted_node_output_index: Int,
    registered_operators_redeemer_index: Int,
  }
  RemoveOperatorBadState {
    slashed_active_operator_key: VerificationKeyHash,
    hub_oracle_ref_input_index: Int,
    active_operator_anchor_element_input_index: Int,
    active_operator_slashed_node_input_index: Int,
    active_operator_anchor_element_output_index: Int,
    state_queue_redeemer_index: Int,
  }
  RemoveOperatorBadSettlement {
    slashed_active_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_slashed_node_input_index: Int,
    active_operator_anchor_node_input_index: Int,
    settlement_input_index: Int,
    m_settlement_redeemer_index: Option<Int>,
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

pub fn operator_is_not_a_member(
  operator: VerificationKeyHash,
  reference_inputs: List<Input>,
  element_input_index: Int,
  nft_policy_id: PolicyId,
) -> Bool {
  utils.key_is_not_a_member_of_ordered_linked_list(
    operator,
    reference_inputs,
    element_input_index,
  )
    |> finalize_linked_list(nft_policy_id)
}
