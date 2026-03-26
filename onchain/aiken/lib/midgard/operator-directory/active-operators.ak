use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use midgard/common/types.{PosixTime}
use midgard/operator_directory.{OperatorStatusRedeemer, SlashingArguments}

/// TODO: Refer to state queue's library module.
pub const root_asset_name: AssetName = "MIDGARD_ACTIVE_OPERATORS"

/// Short for "Midgard Active". In hex it will be: #"4d414354"
///
/// > [!NOTE]
/// > This should not be longer than 4 bytes, as each node's asset name carries
/// > this prefix, followed by its operator's public key hash, which is 28 bytes
/// > long.
pub const node_asset_name_prefix: AssetName = "MACT"

pub const node_asset_name_prefix_length: Int =
  bytearray.length(node_asset_name_prefix)

pub type NodeData {
  bond_unlock_time: Option<PosixTime>,
  inactivity_strikes: Int,
}

pub type Datum =
  operator_directory.Datum<NodeData>

pub type SpendRedeemer {
  ListStateTransition
  UpdateBondHoldNewState {
    active_operator: VerificationKeyHash,
    active_node_input_index: Int,
    active_node_output_index: Int,
    hub_oracle_ref_input_index: Int,
    state_queue_input_index: Int,
    state_queue_redeemer_index: Int,
  }
  UpdateBondHoldNewSettlement {
    active_operator: VerificationKeyHash,
    active_node_input_index: Int,
    active_node_output_index: Int,
    hub_oracle_ref_input_index: Int,
    settlement_input_index: Int,
    settlement_redeemer_index: Int,
    new_bond_unlock_time: PosixTime,
  }
  StrikeForInactivity {
    active_node_input_index: Int,
    active_node_output_index: Int,
    operator: VerificationKeyHash,
    scheduler_input_index: Int,
    scheduler_redeemer_index: Int,
    hub_oracle_ref_input_index: Int,
  }
}

pub type MintRedeemer {
  Init { output_index: Int }
  Deinit { input_index: Int }
  ActivateOperator {
    new_active_operator_key: VerificationKeyHash,
    new_active_operator_bond_unlock_time: Option<PosixTime>,
    active_operator_anchor_element_input_index: Int,
    active_operator_anchor_element_output_index: Int,
    active_operator_inserted_node_output_index: Int,
    registered_operators_redeemer_index: Int,
  }
  RetireOperator {
    active_operator_key: VerificationKeyHash,
    hub_oracle_ref_input_index: Int,
    active_operator_anchor_element_input_index: Int,
    active_operator_removed_node_input_index: Int,
    active_operator_anchor_element_output_index: Int,
    retired_operators_redeemer_index: Int,
    penalize_for_inactivity: Bool,
    operator_status_redeemer: OperatorStatusRedeemer,
  }
  SlashOperator { slashing_arguments: SlashingArguments }
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
