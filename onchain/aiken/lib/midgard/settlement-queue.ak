use aiken/crypto.{VerificationKey}
use aiken/merkle_patricia_forestry as mpf
use aiken_design_patterns/linked_list/unordered.{NodeDatum}
use cardano/assets.{AssetName}
use midgard/common/types.{PosixTime}
use midgard/registered_operators.{OperatorStatus}

pub type Datum =
  NodeDatum

pub type NodeData {
  deposits_root: ByteArray,
  withdrawals_root: ByteArray,
  start_time: PosixTime,
  end_time: PosixTime,
  resolution_claim: Option<ResolutionClaim>,
}

pub type ResolutionClaim {
  resolution_time: PosixTime,
  operator: VerificationKey,
}

pub type SpendRedeemer {
  ListStateTransition
  AttachResolutionClaim {
    node_input_index: Int,
    node_output_index: Int,
    hub_ref_input_index: Int,
    active_operators_node_input_index: Int,
    active_operators_redeemer_index: Int,
    operator: VerificationKey,
    scheduler_ref_input_index: Int,
  }
  DisproveResolutionClaim {
    node_input_index: Int,
    node_output_index: Int,
    hub_ref_input_index: Int,
    operators_redeemer_index: Int,
    operator: VerificationKey,
    operator_status: OperatorStatus,
    unresolved_event_ref_input_index: Int,
    unresolved_event_asset_name: AssetName,
    event_type: EventType,
    membership_proof: mpf.Proof,
  }
}

pub type EventType {
  Deposit
  Withdrawal
}

pub type MintRedeemer {
  Init
  Deinit
  AppendSettlementNode {
    key_to_add: ByteArray,
    anchor_node_output_index: Int,
    node_output_index: Int,
    state_queue_merge_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
  RemoveSettlementNode {
    key_to_remove: ByteArray,
    removed_node_input_index: Int,
    anchor_node_input_index: Int,
  }
}
