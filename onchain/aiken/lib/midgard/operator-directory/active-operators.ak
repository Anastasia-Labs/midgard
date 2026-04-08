use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{Input, Redeemer, ScriptPurpose}
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/operator_directory.{SlashingArguments}
use midgard/scheduler

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
    active_node_link: linked_list.Link,
    scheduler_input_index: Int,
    scheduler_redeemer_index: Int,
    hub_oracle_ref_input_index: Int,
  }
}

pub type OperatorRemovalSchedulerSync {
  ShowOperatorIsInactive { scheduler_ref_input_index: Int }
  ShowSchedulerIsAdvancing {
    scheduler_input_index: Int,
    scheduler_redeemer_index: Int,
    removing_operators_anchor_element_key: Option<linked_list.NodeKey>,
    removing_operator_is_the_last_member: Bool,
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
    operator_removal_scheduler_sync: OperatorRemovalSchedulerSync,
  }
  SlashOperator {
    slashing_arguments: SlashingArguments,
    operator_removal_scheduler_sync: OperatorRemovalSchedulerSync,
  }
}

/// If the subject operator is currently appointed active, this function
/// validates scheduler UTxO is spent. Otherwise, shows that the currently
/// appointed operator is different from the subject operator.
pub fn validate_scheduler_syncs_with_operator_removal(
  operator_removal_scheduler_sync: OperatorRemovalSchedulerSync,
  subject_operator: VerificationKeyHash,
  subject_operators_anchor_key: Option<linked_list.NodeKey>,
  subject_operators_link: linked_list.Link,
  hub_datum: hub.Datum,
  inputs: List<Input>,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  when operator_removal_scheduler_sync is {
    ShowOperatorIsInactive { scheduler_ref_input_index } -> {
      let scheduler_datum =
        reference_inputs
          |> scheduler.get_datum(hub_datum.scheduler, scheduler_ref_input_index)
      when scheduler_datum is {
        scheduler.NoActiveOperators -> True
        scheduler.ActiveOperator { operator, .. } ->
          operator != subject_operator
      }
    }
    ShowSchedulerIsAdvancing {
      scheduler_input_index,
      scheduler_redeemer_index,
      removing_operators_anchor_element_key,
      removing_operator_is_the_last_member,
    } -> {
      // Redeemer arguments must be validated for scheduler to use.
      expect
        removing_operators_anchor_element_key == subject_operators_anchor_key

      let (scheduler_datum_data, scheduler_redeemer_data) =
        utils.get_inline_datum_and_spending_redeemer_data_at(
          hub_datum.scheduler_addr,
          scheduler_input_index,
          scheduler_redeemer_index,
          inputs,
          redeemers,
        )
      expect scheduler.ActiveOperator { operator, .. } = scheduler_datum_data
      expect scheduler.SpendRedeemer {
        scheduler_input_index: input_index_from_scheduler_redeemer,
        advancing_approach,
        ..
      } = scheduler_redeemer_data
      expect operator == subject_operator
      expect scheduler_input_index == input_index_from_scheduler_redeemer
      expect
        when advancing_approach is {
          scheduler.GoToNextDueToOperatorRemoval { .. } |
          scheduler.RewindDueToOperatorRemoval { .. } -> True
          _ -> False
        }
      when subject_operators_link is {
        None -> removing_operator_is_the_last_member == True
        Some(_) -> removing_operator_is_the_last_member == False
      }
    }
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
