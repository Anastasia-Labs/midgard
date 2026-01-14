use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName}
use cardano/transaction.{Input, OutputReference}
use midgard/common/utils
use midgard/ledger_state.{ConfirmedState, Header}
use midgard/registered_operators.{OperatorStatus}

/// TODO: It might be better to be space-conscious and use an empty name.
pub const confirmed_state_asset_name: AssetName = "MIDGARD_CONFIRMED_STATE"

/// Short for "Midgard Block". In hex it'll be: #"4d424c43"
///
/// > [!NOTE]
/// > This must be exactly 4 bytes as it is followed by header hashes which are
/// > hashed with Blake2b-224. This makes up block asset names to occupy all
/// > available 32 bytes.
pub const block_asset_name_prefix: AssetName = "MBLC"

pub const block_asset_name_prefix_length: Int =
  bytearray.length(block_asset_name_prefix)

pub type Datum =
  linked_list.Element<ConfirmedState, Header>

pub type MintRedeemer {
  Init { output_index: Int }
  Deinit { input_index: Int }
  CommitBlockHeader {
    latest_block_input_index: Int,
    new_block_output_index: Int,
    continued_latest_block_output_index: Int,
    operator: VerificationKeyHash,
    scheduler_ref_input_index: Int,
    active_operators_redeemer_index: Int,
  }
  MergeToConfirmedState {
    header_node_key: ByteArray,
    header_node_input_index: Int,
    confirmed_state_node_input_index: Int,
    confirmed_state_node_output_index: Int,
    settlement_redeemer_index: Int,
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

pub fn get_confirmed_datum(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
) {
  expect NodeDatum {
    data: confirmed_state_datum_data,
    key: unordered.Empty,
    ..
  } =
    utils.get_authentic_input_datum_with_policy_at(
      reference_inputs,
      state_queue_policy,
      state_queue_node_ref_input_index,
    )

  expect coerced_confirmed_state: ConfirmedState = confirmed_state_datum_data

  coerced_confirmed_state
}

pub fn get_block_datum(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
  return: fn(Header, ByteArray) -> result,
) -> result {
  expect NodeDatum { data: header_datum_data, key: unordered.Key { key }, .. } =
    utils.get_authentic_input_datum_with_policy_at(
      reference_inputs,
      state_queue_policy,
      state_queue_node_ref_input_index,
    )

  expect coerced_header: Header = header_datum_data

  return(coerced_header, key)
}
