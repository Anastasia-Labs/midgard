use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{Input, Mint, Redeemer, ScriptPurpose}
use midgard/common/utils
use midgard/ledger_state.{ConfirmedState, Header, HeaderHash}

/// TODO: It might be better to be space-conscious and use an empty name.
pub const confirmed_state_asset_name: AssetName = "MIDGARD_CONFIRMED_STATE"

/// Short for "Midgard Block". In hex it will be: #"4d424c43"
///
/// > [!NOTE]
/// > This should not be longer than 4 bytes, as it is followed by header hashes
/// > which are hashed with Blake2b-224. 4 bytes makes up block asset names to
/// > occupy all available 32 bytes.
pub const block_asset_name_prefix: AssetName = "MBLC"

pub const block_asset_name_prefix_length: Int =
  bytearray.length(block_asset_name_prefix)

pub type Datum =
  linked_list.Element<ConfirmedState, Header>

pub type SlashingApproach {
  SlashActiveOperator { active_operators_redeemer_index: Int }
  SlashRetiredOperator { retired_operators_redeemer_index: Int }
  OperatorAlreadySlashed {
    active_operators_element_ref_input_index: Int,
    retired_operators_element_ref_input_index: Int,
  }
}

pub type BlockRemovalApproach {
  RemoveLastFraudulentBlock {
    anchor_element_input_index: Int,
    anchor_element_output_index: Int,
  }
  RemoveFraudulentBlocksLink {
    fraudulent_node_output_index: Int,
    removed_block_input_index: Int,
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
    m_settlement_redeemer_index: Option<Int>,
  }
  RemoveFraudulentBlockHeader {
    fraudulent_operator: VerificationKeyHash,
    fraudulent_blocks_header_hash: HeaderHash,
    slashing_approach: SlashingApproach,
    fraudulent_node_input_index: Int,
    fraud_proof_ref_input_index: Int,
    approach: BlockRemovalApproach,
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
      _state_queue_element_lovelace,
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
  return: fn(Header, HeaderHash) -> result,
) -> result {
  expect Some(Input { output, .. }) =
    reference_inputs |> list.at(state_queue_node_ref_input_index)
  let linked_list_eval = {
    let
      _state_queue_element_lovelace,
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

pub fn get_prev_header_hash_of_node(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
  expected_state_queue_node_header_hash: HeaderHash,
) -> HeaderHash {
  let
    header,
    retrieved_state_queue_node_header_hash,
  <-
    get_block_datum(
      reference_inputs,
      state_queue_policy,
      state_queue_node_ref_input_index,
    )
  expect
    retrieved_state_queue_node_header_hash == expected_state_queue_node_header_hash
  header.prev_header_hash
}

pub fn get_fraudulent_operator_of_removing_block(
  state_queue_policy_id: PolicyId,
  state_queue_mint_redeemer_index: Int,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> VerificationKeyHash {
  expect RemoveFraudulentBlockHeader { fraudulent_operator, .. } =
    redeemers
      |> utils.get_redeemer_at(
          expected_purpose: Mint(state_queue_policy_id),
          redeemer_index: state_queue_mint_redeemer_index,
        )
  fraudulent_operator
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
