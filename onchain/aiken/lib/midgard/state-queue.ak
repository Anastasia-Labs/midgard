use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{Input, Output, OutputReference}
use midgard/ledger_state.{
  ConfirmedState, DepositsRoot, EventToStepRoot, ForcedTransactionsRoot,
  HeaderHash, HeaderV1, MidgardTxsRoot, TransitionTraceRoot,
  ValidationTracesRoot, WithdrawalsRoot, protocol_version_v1,
}

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

pub const no_da_attestation: ByteArray = ""

pub type StateQueueNode {
  header: HeaderV1,
  da_attestation: ByteArray,
}

pub type HeaderView =
  HeaderV1

pub fn decode_header_view(header: HeaderV1) -> HeaderView {
  expect header.protocol_version == protocol_version_v1
  header
}

pub type Datum =
  linked_list.Element<ConfirmedState, StateQueueNode>

pub type SpendRedeemer {
  LinkedListMutation
  AttachDaAttestation {
    state_queue_input_index: Int,
    da_attestation_mint_redeemer_index: Int,
  }
}

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
    anchor_element_input_outref: OutputReference,
    anchor_element_output_index: Int,
  }
  RemoveFraudulentBlocksLink {
    fraudulent_node_input_outref: OutputReference,
    fraudulent_node_output_index: Int,
  }
}

pub type MintRedeemer {
  InitV1 { output_index: Int }
  Deinit
  CommitBlockHeader {
    new_block_output_index: Int,
    continued_latest_block_output_index: Int,
    operator: VerificationKeyHash,
    scheduler_ref_input_index: Int,
    active_operators_input_index: Int,
    active_operators_redeemer_index: Int,
  }
  RemoveFraudulentBlockHeader {
    fraudulent_operator: VerificationKeyHash,
    fraudulent_blocks_header_hash: HeaderHash,
    slashing_approach: SlashingApproach,
    fraud_proof_ref_input_index: Int,
    block_removal_approach: BlockRemovalApproach,
  }
  MergeToConfirmedStateV1 {
    header_node_key: ByteArray,
    confirmed_state_input_outref: OutputReference,
    confirmed_state_output_index: Int,
    m_settlement_redeemer_index: Option<Int>,
    merged_block_withdrawals_root: WithdrawalsRoot,
    merged_block_forced_transactions_root: ForcedTransactionsRoot,
    merged_block_transactions_root: MidgardTxsRoot,
    merged_block_deposits_root: DepositsRoot,
    merged_block_transition_trace_root: TransitionTraceRoot,
    merged_block_event_to_step_root: EventToStepRoot,
    merged_block_validation_traces_root: ValidationTracesRoot,
    merged_block_withdrawal_count: Int,
    merged_block_forced_transaction_count: Int,
    merged_block_l2_transaction_count: Int,
    merged_block_deposit_count: Int,
    merged_block_total_event_count: Int,
    merged_block_transition_step_count: Int,
    merged_block_validation_trace_count: Int,
  }
}

pub fn get_confirmed_state(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
) -> ConfirmedState {
  expect Some(Input { output, .. }) = reference_inputs
    |> list.at(state_queue_node_ref_input_index)
  let linked_list_eval = {
    let
        _state_queue_element_address,
        _state_queue_element_lovelace,
        m_state_queue_element_key,
        state_queue_element_data,
        _state_queue_element_link
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

pub fn get_block_datum_v1(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
  return: fn(HeaderV1, HeaderHash) -> result,
) -> result {
  get_state_queue_node(
    reference_inputs,
    state_queue_policy,
    state_queue_node_ref_input_index,
    fn(state_queue_node, header_hash) {
      let header = decode_header_view(state_queue_node.header)
      return(header, header_hash)
    },
  )
}

pub fn get_state_queue_node(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
  return: fn(StateQueueNode, HeaderHash) -> result,
) -> result {
  expect Some(Input { output, .. }) = reference_inputs
    |> list.at(state_queue_node_ref_input_index)
  let linked_list_eval = {
    let
        _state_queue_element_address,
        _state_queue_element_lovelace,
        m_state_queue_element_key,
        state_queue_element_data,
        _state_queue_element_link
      <- linked_list.get_element_info(output)
    expect Some(header_hash) = m_state_queue_element_key
    expect state_queue_node: StateQueueNode = state_queue_element_data
    return(state_queue_node, header_hash)
  }
  linked_list_eval
    |> linked_list.run_element_with(
        state_queue_policy,
        confirmed_state_asset_name,
        block_asset_name_prefix,
        block_asset_name_prefix_length,
      )
}

fn get_node_info(
  output: Output,
  state_queue_policy: PolicyId,
  return: fn(Int, HeaderHash, StateQueueNode, linked_list.Link) -> result,
) -> result {
  let linked_list_eval = {
    let _address, lovelace, m_header_hash, data, link <- linked_list.get_element_info(
      output,
    )
    expect Some(header_hash) = m_header_hash
    expect state_queue_node: StateQueueNode = data
    return(lovelace, header_hash, state_queue_node, link)
  }
  linked_list_eval
    |> linked_list.run_element_with(
        state_queue_policy,
        confirmed_state_asset_name,
        block_asset_name_prefix,
        block_asset_name_prefix_length,
      )
}

pub fn validate_da_attestation_attachment(
  inputs: List<Input>,
  outputs: List<Output>,
  state_queue_policy: PolicyId,
  state_queue_input_index: Int,
  state_queue_output_index: Int,
  expected_header_hash: HeaderHash,
  da_attestation_policy_id: PolicyId,
) -> Bool {
  expect Some(Input { output: input_output, .. }) = inputs
    |> list.at(state_queue_input_index)
  expect Some(output) = outputs |> list.at(state_queue_output_index)
  let Output { address: input_address, .. } = input_output
  let Output { address: output_address, .. } = output
  expect input_address == output_address

  let input_lovelace, input_header_hash, input_block, input_link <- get_node_info(
    input_output,
    state_queue_policy,
  )
  let output_lovelace, output_header_hash, output_block, output_link <- get_node_info(
    output,
    state_queue_policy,
  )

  expect input_header_hash == expected_header_hash
  expect output_header_hash == expected_header_hash
  expect output_lovelace >= input_lovelace
  expect output_link == input_link
  expect input_block.da_attestation == no_da_attestation
  expect output_block.header == input_block.header
  expect output_block.da_attestation == da_attestation_policy_id
  True
}

pub fn get_prev_header_hash_of_node_v1(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
  expected_state_queue_node_header_hash: HeaderHash,
) -> HeaderHash {
  let header, retrieved_state_queue_node_header_hash <- get_block_datum_v1(
    reference_inputs,
    state_queue_policy,
    state_queue_node_ref_input_index,
  )
  expect
    retrieved_state_queue_node_header_hash == expected_state_queue_node_header_hash
  header.prev_header_hash
}

pub fn finalize_linked_list(
  eval: linked_list.ElementEval<Bool>,
  state_queue_policy: PolicyId,
) -> Bool {
  eval
    |> linked_list.run_element_with(
        state_queue_policy,
        confirmed_state_asset_name,
        block_asset_name_prefix,
        block_asset_name_prefix_length,
      )
}
