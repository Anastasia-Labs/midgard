use aiken/merkle_patricia_forestry as mpf
use aiken_design_patterns/linked_list/unordered.{NodeDatum}
use cardano/assets.{AssetName}
use midgard/common/types.{PosixTime, PubKeyHash}

pub type Datum =
  NodeDatum

/// The `operator` is optional to model UTxOs subject to proven frauds..
pub type NodeData {
  deposits_root: ByteArray,
  withdrawals_root: ByteArray,
  start_time: PosixTime,
  end_time: PosixTime,
  time_of_claimed_as_resolved: Option<PosixTime>,
  operator: Option<PubKeyHash>,
}

pub type SpendRedeemer {
  ListStateTransition
  UpdateResolvedFlag {
    node_input_index: Int,
    node_output_index: Int,
    hub_ref_input_index: Int,
    active_operators_node_input_index: Int,
    active_operators_redeemer_index: Int,
    purpose: Purpose,
  }
  ConcludeSettlement
}

pub type Purpose {
  ClaimAsResolved
  RevokeResolvedFlag {
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
  SpawnSettlement {
    anchor_node_output_index: Int,
    node_output_index: Int,
    state_queue_merge_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
  RemoveSettlement {
    key_to_remove: ByteArray,
    removed_node_input_index: Int,
    anchor_node_input_index: Int,
  }
}

pub const settlement_resolution_duration: Int = 0
