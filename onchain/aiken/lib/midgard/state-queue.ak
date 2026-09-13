use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{Input, Output, OutputReference, ValidityRange}
use midgard/availability_challenge as availability
use midgard/common/utils
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

pub const no_da_attestation: availability.StateQueueStatusV1 =
  availability.Unattested

/// F04's canonical V1 deadline for applying a DA attestation to a committed
/// block. The block's `end_time` is commit-bound by Q60, so this duration has
/// one authenticated on-chain origin rather than a submitter-selected clock.
pub const da_attestation_timeout_v1: Int = 3_600_000

pub type StateQueueNode {
  header: HeaderV1,
  da_attestation: availability.StateQueueStatusV1,
}

pub type StateQueueNodeV1 =
  StateQueueNode

pub type HeaderView =
  HeaderV1

pub fn decode_header_view(header: HeaderV1) -> HeaderView {
  expect header.protocol_version == protocol_version_v1
  header
}

/// Bind the canonical event interval's end to the L1 transaction that commits
/// it. The start remains the preceding header's end, while the end is the
/// normalized inclusive upper bound of a short Cardano validity interval.
pub fn commit_bound_header_time_is_valid(
  header_start_time: Int,
  header_end_time: Int,
  commit_validity_range: ValidityRange,
) -> Bool {
  let (_, inclusive_commit_upper_bound) = utils.get_inclusive_bounds_of_a_short_validity_range(
    commit_validity_range,
  )
  and {
    header_start_time < header_end_time,
    header_end_time == inclusive_commit_upper_bound,
  }
}

pub type Datum =
  linked_list.Element<ConfirmedState, StateQueueNode>

pub type SpendRedeemer {
  LinkedListMutation
  AttachDaAttestation {
    state_queue_input_index: Int,
    da_attestation_mint_redeemer_index: Int,
  }
  /// Authorizes only the retained-DA status transitions whose availability
  /// policy redeemer names this exact queue input and its exact continuation.
  /// Timeout correction deliberately uses `LinkedListMutation`: it removes a
  /// node and must never be laundered through this status-only arm.
  AvailabilityStatusUpdate {
    state_queue_input_index: Int,
    state_queue_output_index: Int,
    availability_mint_redeemer_index: Int,
  }
}

/// How the fraudulent operator's bond is reached by a
/// `RemoveFraudulentBlockHeader` transaction.
///
/// The two bond-consuming constructors — and only they — name the output that
/// carries the fraud prover's reward. `OperatorAlreadySlashed` consumes no
/// operator-directory node, so it has no bond to route and cannot even express
/// a reward destination: that is the type-level half of the D4 exclusivity
/// ruling (2026-08-11 owner ruling 7), and the handler enforces the other half
/// by refusing any reward-shaped output on that arm.
///
/// `m_fraud_prover_reward_output_index` is `None` only for a deployment whose
/// compiled reward is zero. Both release profiles compile a nonzero reward, so
/// their bond-consuming arms must name the exact reward output.
pub type SlashingApproach {
  SlashActiveOperator {
    active_operators_redeemer_index: Int,
    m_fraud_prover_reward_output_index: Option<Int>,
  }
  SlashRetiredOperator {
    retired_operators_redeemer_index: Int,
    m_fraud_prover_reward_output_index: Option<Int>,
  }
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

/// Ordered correction for a head-of-line block whose DA attestation was not
/// applied before `da_attestation_timeout_v1`.
///
/// Descendants are pruned first because their state transitions build on the
/// timed-out block. Only once its link is empty may the root remove the timed-
/// out head itself; otherwise the root could point at a header whose
/// `prev_header_hash` and `prev_utxos_root` no longer match confirmed state.
pub type AttestationTimeoutRemovalApproach {
  PruneTimedOutBlockDescendant {
    confirmed_state_ref_input_index: Int,
    timed_out_node_input_outref: OutputReference,
    timed_out_node_output_index: Int,
  }
  RemoveTimedOutHead {
    confirmed_state_input_outref: OutputReference,
    confirmed_state_output_index: Int,
  }
}

pub type MintRedeemer {
  InitV1 { output_index: Int }
  Deinit
  CommitBlockHeader {
    yield_to_ref_input_index: Int,
    new_block_output_index: Int,
    continued_latest_block_output_index: Int,
    operator: VerificationKeyHash,
    scheduler_ref_input_index: Int,
    active_operators_input_index: Int,
    active_operators_redeemer_index: Int,
    /// Required exactly when the queue is non-empty. The singleton root binds
    /// the append to the current head for Q61's timeout fence.
    m_confirmed_state_ref_input_index: Option<Int>,
    /// Required when the current head differs from the consumed append anchor
    /// (the tail). Omitted for a one-node queue, where the authenticated anchor
    /// datum is already the head datum.
    m_head_state_queue_node_ref_input_index: Option<Int>,
  }
  RemoveFraudulentBlockHeader {
    yield_to_ref_input_index: Int,
    fraudulent_operator: VerificationKeyHash,
    fraudulent_blocks_header_hash: HeaderHash,
    slashing_approach: SlashingApproach,
    fraud_proof_ref_input_index: Int,
    block_removal_approach: BlockRemovalApproach,
  }
  /// Permissionless liveness correction. It never consumes an operator bond:
  /// missing DA attestation may be committee failure, so it is not evidence of
  /// operator fraud. The current state-queue node is the authoritative
  /// applied-attestation state and the shared input that serializes this path
  /// against `AttachDaAttestation`.
  RemoveUnattestedBlockAfterTimeout {
    yield_to_ref_input_index: Int,
    timed_out_header_hash: HeaderHash,
    removal_approach: AttestationTimeoutRemovalApproach,
  }
  /// Deterministic DA-availability correction. The dedicated availability
  /// validator authenticates the expired challenged bond and exact slash on
  /// the first transaction; the CorrectionLock singleton then carries the
  /// same DACH identity across permissionless descendant pruning.
  RemoveUnavailableBlockAfterTimeout {
    yield_to_ref_input_index: Int,
    unavailable_header_hash: HeaderHash,
    challenge_asset_name: AssetName,
    removal_approach: AttestationTimeoutRemovalApproach,
  }
  MergeToConfirmedStateV1 {
    yield_to_ref_input_index: Int,
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

/// Canonical fieldless redeemer used by every state-queue rewarding validator.
/// The rewarding validator binds its work to the exact state-queue mint
/// redeemer in the same transaction, so the withdrawal does not duplicate the
/// operational payload.
pub type YieldRedeemerV1 {
  YieldStateQueueV1
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

/// Authenticate the singleton confirmed-state root and expose its current
/// head link. Referencing this UTxO lets append and correction paths agree on
/// one queue head without consuming the root on ordinary appends.
pub fn get_confirmed_state_root(
  reference_inputs: List<Input>,
  state_queue_policy: ByteArray,
  confirmed_state_ref_input_index: Int,
  return: fn(ConfirmedState, Option<HeaderHash>) -> result,
) -> result {
  expect Some(Input { output, .. }) = reference_inputs
    |> list.at(confirmed_state_ref_input_index)
  let linked_list_eval = {
    let
        _confirmed_state_address,
        _confirmed_state_lovelace,
        m_confirmed_state_key,
        confirmed_state_data,
        head_link
      <- linked_list.get_element_info(output)
    expect m_confirmed_state_key == None
    expect confirmed_state: ConfirmedState = confirmed_state_data
    return(confirmed_state, head_link)
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
  da_bond_asset_name: AssetName,
  validity_range: ValidityRange,
) -> Bool {
  expect
    validate_da_availability_status_transition(
      inputs,
      outputs,
      state_queue_policy,
      state_queue_input_index,
      state_queue_output_index,
      expected_header_hash,
      no_da_attestation,
      availability.Attested { da_bond_asset_name },
    )
  expect Some(Input { output: input_output, .. }) = inputs
    |> list.at(state_queue_input_index)
  let _, _, input_block, _ <- get_node_info(input_output, state_queue_policy)
  let (_, inclusive_tx_upper_bound) = utils.get_inclusive_bounds_of_a_short_validity_range(
    validity_range,
  )
  expect
    inclusive_tx_upper_bound <= input_block.header.end_time + da_attestation_timeout_v1
  True
}

/// Exact in-place availability status transition for one authenticated linked-
/// list node. The availability policy uses this for Attested→Challenged and
/// Challenged→Published; the DA policy uses the wrapper above for the
/// deadline-bound Unattested→Attested transition.
pub fn validate_da_availability_status_transition(
  inputs: List<Input>,
  outputs: List<Output>,
  state_queue_policy: PolicyId,
  state_queue_input_index: Int,
  state_queue_output_index: Int,
  expected_header_hash: HeaderHash,
  expected_input_status: availability.StateQueueStatusV1,
  expected_output_status: availability.StateQueueStatusV1,
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
  let input_header_data: Data = input_block.header
  expect input_header_hash == utils.serialise_and_hash_28(input_header_data)
  expect output_lovelace >= input_lovelace
  expect output_link == input_link
  expect input_block.da_attestation == expected_input_status
  expect output_block.header == input_block.header
  expect output_block.da_attestation == expected_output_status
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
