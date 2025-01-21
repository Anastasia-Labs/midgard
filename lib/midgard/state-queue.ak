use aiken_design_patterns/linked_list/unordered.{NodeDatum}
use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}

/// TODO?
/// > [!NOTE]
/// > Keep in mind that validators may depend on the lexicographic ordering
/// > between `confirmed_token_name` and `block_token_name`.
pub const confirmed_asset_name: AssetName = "CONFIRMED_STATE"

pub const block_asset_name: AssetName = #""

pub type Config {
  init_utxo: OutputReference,
  refund_waiting_period: Int,
}

pub type Datum =
  NodeDatum

pub type MintRedeemer {
  Init
  Deinit
  CommitBlockHeader { operator: ByteArray }
  MergeToConfirmedState
  RemoveFraudulentBlockHeader { fraudulent_operator: ByteArray }
}
