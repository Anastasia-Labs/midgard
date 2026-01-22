use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{Input, OutputReference}
use midgard/common/utils
use midgard/ledger_state.{ConfirmedState, Header, HeaderHash}

/// TODO: It might be better to be space-conscious and use an empty name.
pub const confirmed_state_asset_name: AssetName = "MIDGARD_CONFIRMED_STATE"

/// Short for "Midgard Block". In hex it will be: #"4d424c43"
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

pub type SpendRedeemer {
  ListStateTransition
  CutOffFraudulentBlockHeader {
    anchor_element_input_index: Int,
    anchor_element_output_index: Int,
    operators_redeemer_index: Int,
    fraud_proof_ref_input_index: Int,
    fraudulent_blocks_header_hash: HeaderHash,
    fraudulent_operator: VerificationKeyHash,
    operator_is_active: Bool,
  }
}

pub type MintRedeemer {
  Init { output_index: Int }
  Deinit { input_index: Int }
  CommitBlockHeader {
    latest_block_input_index: Int,
    new_block_output_index: Int,
    continued_latest_block_output_index: Int,
    operator: VerificationKeyHash,
    scheduler_ref_input_index: Int,
    active_operators_input_index: Int,
    active_operators_redeemer_index: Int,
  }
  MergeToConfirmedState {
    header_node_key: ByteArray,
    header_node_input_index: Int,
    confirmed_state_input_index: Int,
    confirmed_state_output_index: Int,
    settlement_redeemer_index: Int,
  }
  RemoveFraudulentBlocksLink {
    fraudulent_node_input_index: Int,
    fraudulent_node_output_index: Int,
    link_input_index: Int,
    fraud_proof_ref_input_index: Int,
  }
  RemoveFraudulentBlockHeader {
    fraudulent_node_input_index: Int,
    fraud_proof_ref_input_index: Int,
  }
}

pub fn get_confirmed_state(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
) -> ConfirmedState {
  expect Some(Input { output, .. }) =
    reference_inputs |> list.at(state_queue_node_ref_input_index)
  let linked_list_eval = {
    let
      m_state_queue_element_key,
      state_queue_element_data,
      _state_queue_element_link,
    <- linked_list.get_element_info(output)
    expect None = m_state_queue_element_key
    expect coerced_confirmed_state: ConfirmedState = state_queue_element_data
    coerced_confirmed_state
  }
  linked_list_eval
    |> linked_list.run_element_with(
        state_queue_policy,
        confirmed_state_asset_name,
        block_asset_name_prefix,
        block_asset_name_prefix_length,
      )
}

pub fn get_block_datum(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
  return: fn(Header, ByteArray) -> result,
) -> result {
  expect Some(Input { output, .. }) =
    reference_inputs |> list.at(state_queue_node_ref_input_index)
  let linked_list_eval = {
    let
      m_state_queue_element_key,
      state_queue_element_data,
      _state_queue_element_link,
    <- linked_list.get_element_info(output)
    expect Some(header_hash) = m_state_queue_element_key
    expect coerced_header: Header = state_queue_element_data
    return(coerced_header, header_hash)
  }
  linked_list_eval
    |> linked_list.run_element_with(
        state_queue_policy,
        confirmed_state_asset_name,
        block_asset_name_prefix,
        block_asset_name_prefix_length,
      )
}

pub fn finalize_linked_list(
  eval: linked_list.Eval,
  state_queue_policy: PolicyId,
) -> Bool {
  eval
    |> linked_list.run_eval_with(
        state_queue_policy,
        confirmed_state_asset_name,
        block_asset_name_prefix,
        block_asset_name_prefix_length,
      )
}
