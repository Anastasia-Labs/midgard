use aiken_design_patterns/linked_list/unordered.{NodeDatum}
use cardano/assets.{AssetName}
use cardano/transaction.{InlineDatum, Input, OutputReference}
use midgard/common/utils
use midgard/ledger_state.{ConfirmedState, Header}
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

fn get_node_datum_data(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
) {
  let state_queue_node_input =
    utils.get_authentic_input_with_policy_at(
      reference_inputs,
      state_queue_policy,
      state_queue_node_ref_input_index,
    )

  expect InlineDatum(state_queue_datum_data) =
    state_queue_node_input.output.datum

  state_queue_datum_data
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
    get_node_datum_data(
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
    get_node_datum_data(
      reference_inputs,
      state_queue_policy,
      state_queue_node_ref_input_index,
    )

  expect coerced_header: Header = header_datum_data

  return(coerced_header, key)
}
