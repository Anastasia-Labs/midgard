use aiken_design_patterns/linked_list/unordered.{NodeDatum}
use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}
use midgard/registered_operators.{OperatorStatus}

/// TODO?
/// > [!NOTE]
/// > Keep in mind that validators may depend on the lexicographic ordering
/// > between `confirmed_token_name` and `block_token_name`.
pub const confirmed_asset_name: AssetName = "CONFIRMED_STATE"

pub const block_asset_name: AssetName = #""

pub type Datum =
  NodeDatum

pub type Config {
  init_utxo: OutputReference,
  refund_waiting_period: Int,
}

pub type MintRedeemer {
  Init
  Deinit
  CommitBlockHeader {
    operator: ByteArray,
    scheduler_ref_input_index: Int,
    active_node_input_index: Int,
    header_node_output_index: Int,
    previous_header_node_output_index: Int,
    active_operators_redeemer_index: Int,
  }
  MergeToConfirmedState {
    header_node_key: ByteArray,
    header_node_input_index: Int,
    confirmed_state_node_input_index: Int,
    confirmed_state_node_output_index: Int,
    settlement_queue_redeemer_index: Int,
  }
  RemoveFraudulentBlockHeader {
    removed_node_input_index: Int,
    predecessor_node_input_index: Int,
    operators_redeemer_index: Int,
    fraud_proof_ref_input_index: Int,
    fraud_proof_asset_name: ByteArray,
    fraudulent_operator: ByteArray,
    operator_status: OperatorStatus,
  }
}
