use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{OutputReference}
use midgard/common/types.{PosixTime}
use midgard/operator_directory.{SlashingArguments}

/// TODO: Refer to state queue's library module.
pub const root_asset_name: AssetName = "MidgardRetiredOperators"

pub const hub_oracle_mint_asset_name: AssetName = "RetiredOperatorsMint"

pub const hub_oracle_spend_asset_name: AssetName = "RetiredOperatorsSpend"

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
  Deinit
  RetireOperator {
    new_retired_operator_key: VerificationKeyHash,
    bond_unlock_time: Option<PosixTime>,
    active_operators_mint_hub_oracle_ref_index: Int,
    retired_operator_anchor_element_output_index: Int,
    retired_operator_inserted_node_output_index: Int,
    active_operators_redeemer_index: Int,
  }
  RecoverOperatorBond {
    retired_operator_key: VerificationKeyHash,
    retired_operator_anchor_element_input_outref: OutputReference,
    retired_operator_anchor_element_output_index: Int,
  }
  SlashOperator { slashing_arguments: SlashingArguments }
}

pub fn finalize_linked_list(
  eval: linked_list.ElementEval<Bool>,
  nft_policy_id: PolicyId,
) -> Bool {
  eval
    |> linked_list.run_element_with(
        nft_policy_id,
        root_asset_name,
        node_asset_name_prefix,
        node_asset_name_prefix_length,
      )
}
