use aiken/builtin
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken_design_patterns/linked_list
use cardano/address.{
  Address, from_script, from_verification_key, with_delegation_key,
}
use cardano/assets.{AssetName, Lovelace, PolicyId, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Redeemer,
  ScriptPurpose, Transaction, ValidityRange,
}
use env
use midgard/availability_challenge as availability
use midgard/common/utils
use midgard/correction_lock
use midgard/da_attestation_types as da_attestation
use midgard/fraud_proof
use midgard/hub_oracle as hub
use midgard/ledger_state.{
  ConfirmedState, HeaderV1, block_maturity_duration_v1,
  confirmed_state_next_header_protocol_version_v1, genesis_confirmed_state_v1,
  header_v1_is_valid, protocol_version_v1,
}
use midgard/operator_directory.{SlashOperatorForBadState}
use midgard/operator_directory/active_operators
use midgard/operator_directory/retired_operators
use midgard/scheduler
use midgard/settlement
use midgard/state_queue.{
  AttachDaAttestation, AttestationTimeoutRemovalApproach,
  AvailabilityStatusUpdate, BlockRemovalApproach, CommitBlockHeader, Deinit,
  HeaderView, InitV1, LinkedListMutation, MergeToConfirmedStateV1, MintRedeemer,
  OperatorAlreadySlashed, PruneTimedOutBlockDescendant,
  RemoveFraudulentBlockHeader, RemoveFraudulentBlocksLink,
  RemoveLastFraudulentBlock, RemoveTimedOutHead,
  RemoveUnattestedBlockAfterTimeout, RemoveUnavailableBlockAfterTimeout,
  SlashActiveOperator, SlashRetiredOperator, SlashingApproach, SpendRedeemer,
  StateQueueNode, commit_bound_header_time_is_valid, confirmed_state_asset_name,
  da_attestation_timeout_v1, decode_header_view, finalize_linked_list,
  get_confirmed_state_root, get_state_queue_node, no_da_attestation,
}
use midgard/state_queue_yield

/// Pure CommitBlockHeader structural seam. The full validator authenticates
/// the linked-list output before reaching this predicate.
pub fn commit_block_header_output_is_valid_v1(header: HeaderV1) -> Bool {
  and {
    header.start_time < header.end_time,
    header_v1_is_valid(header),
  }
}

/// The scheduler reference and active-operator spend are authenticated by
/// their respective scripts before their keys reach this pure equality check.
pub fn commit_block_header_operator_is_legitimate_v1(
  header_operator: ByteArray,
  redeemer_operator: ByteArray,
  scheduler_operator: ByteArray,
  active_operator: ByteArray,
) -> Bool {
  and {
    header_operator == redeemer_operator,
    scheduler_operator == redeemer_operator,
    active_operator == redeemer_operator,
  }
}

/// Exact carry-over required when appending after another queued block.
pub fn commit_block_header_carries_previous_block_v1(
  output_header: HeaderV1,
  previous_header_hash: ByteArray,
  previous_header: HeaderV1,
) -> Bool {
  and {
    output_header.prev_header_hash == previous_header_hash,
    output_header.prev_utxos_root == previous_header.utxos_root,
    output_header.start_time == previous_header.end_time,
    output_header.protocol_version == previous_header.protocol_version,
  }
}

/// Exact carry-over required when the linked-list anchor is the confirmed
/// state. `confirmed_state_next_header_protocol_version_v1` distinguishes the
/// sole genesis sentinel from authenticated ordinary V1 states.
pub fn commit_block_header_carries_confirmed_state_v1(
  output_header: HeaderV1,
  confirmed_state: ConfirmedState,
) -> Bool {
  when confirmed_state_next_header_protocol_version_v1(confirmed_state) is {
    Some(next_protocol_version) -> and {
        output_header.prev_header_hash == confirmed_state.header_hash,
        output_header.prev_utxos_root == confirmed_state.utxo_root,
        output_header.start_time == confirmed_state.end_time,
        output_header.protocol_version == next_protocol_version,
      }
    None -> False
  }
}

/// Q61 append fence. An unattested head accepts descendants only through a
/// validity window ending strictly before its timeout boundary. Once attached,
/// the exact configured DA policy marker restores appendability. An unknown
/// non-empty marker authorizes neither branch.
pub fn state_queue_head_allows_append_v1(
  head: StateQueueNode,
  _da_attestation_policy_id: PolicyId,
  validity_range: ValidityRange,
) -> Bool {
  let header = decode_header_view(head.header)
  let (_, inclusive_tx_upper_bound) = utils.get_inclusive_bounds_of_a_short_validity_range(
    validity_range,
  )
  and {
    header_v1_is_valid(header),
    when head.da_attestation is {
      availability.Unattested ->
        inclusive_tx_upper_bound < header.end_time + da_attestation_timeout_v1
      availability.Attested { .. } -> True
      availability.Challenged { .. } -> False
      availability.Published { .. } -> True
    },
  }
}

/// Resolve the current head observed by a non-empty append. The append helper
/// has already authenticated `append_anchor_data` as the consumed tail. The
/// singleton root decides whether that tail is also the head; a deeper queue
/// must supply the separately authenticated head node.
pub fn authenticated_queue_head_for_append_v1(
  reference_inputs: List<Input>,
  state_queue_policy_id: PolicyId,
  append_anchor_header_hash: ByteArray,
  append_anchor_data: Data,
  confirmed_state_ref_input_index: Int,
  m_head_state_queue_node_ref_input_index: Option<Int>,
) -> StateQueueNode {
  let head_header_hash = get_confirmed_state_root(
    reference_inputs,
    state_queue_policy_id,
    confirmed_state_ref_input_index,
    fn(confirmed_state, head_link) {
      expect
        confirmed_state_next_header_protocol_version_v1(confirmed_state) != None
      expect Some(head_header_hash) = head_link
      head_header_hash
    },
  )
  let head_node = if head_header_hash == append_anchor_header_hash {
    expect m_head_state_queue_node_ref_input_index == None
    expect authenticated_head: StateQueueNode = append_anchor_data
    authenticated_head
  } else {
    expect Some(head_ref_input_index) = m_head_state_queue_node_ref_input_index
    get_state_queue_node(
      reference_inputs,
      state_queue_policy_id,
      head_ref_input_index,
      fn(authenticated_head, authenticated_head_hash) {
        expect authenticated_head_hash == head_header_hash
        authenticated_head
      },
    )
  }
  let head_header_data: Data = head_node.header
  expect head_header_hash == utils.serialise_and_hash_28(head_header_data)
  head_node
}

/// Exact fraud-prover reward routing (F04 §2.3; 2026-08-11 owner ruling 7, D3).
///
/// The reward-bearing slash transaction pays `expected_reward` lovelace,
/// ADA-only, to the **enterprise** address whose payment credential is the
/// fraud prover preserved in the authentic fraud-proof datum. Nothing else
/// about the transaction may redirect it: a stake-delegated variant of the same
/// key, a script address, a submitter's change address, or an output smuggling
/// tokens or a reference script alongside the reward all fail this predicate.
///
/// Over- and under-payment are refused symmetrically. The operator's bond
/// tranche is conserved exactly by this output plus the transaction fee
/// (`operator_directory.slash_fraudulent_operator_and_get_info` floors the fee
/// at the penalty), so one lovelace either way is a lovelace taken from the
/// Cardano treasury's share of the bond or from the prover's.
pub fn fraud_prover_reward_output_is_exact_v1(
  reward_output: Output,
  fraud_prover: VerificationKeyHash,
  expected_reward: Lovelace,
) -> Bool {
  let Output { address, value, datum, reference_script } = reward_output
  and {
    address == from_verification_key(fraud_prover),
    assets.lovelace_of(value) == expected_reward,
    assets.without_lovelace(value) == assets.zero,
    datum == NoDatum,
    reference_script == None,
  }
}

/// D4 exclusivity (2026-08-11 owner ruling 7): the reward is payable only by
/// the bond-consuming arms of `RemoveFraudulentBlockHeader`. Every other shape
/// of that transaction — in practice the `OperatorAlreadySlashed` arm, which is
/// what a second claim against an already-forfeited bond must use — has to
/// carry no reward-shaped payment at all, so a duplicate fraud-proof token for
/// the same header stays harmless-but-unrewarded rather than funding a second
/// claim.
pub fn no_output_pays_fraud_prover_reward_v1(
  outputs: List<Output>,
  fraud_prover: VerificationKeyHash,
  expected_reward: Lovelace,
) -> Bool {
  !list.any(
    outputs,
    fn(output) {
      fraud_prover_reward_output_is_exact_v1(
        output,
        fraud_prover,
        expected_reward,
      )
    },
  )
}

/// Routes the reward for one bond-consuming slashing arm.
///
/// `None` is accepted only when `expected_reward` is zero. Both release
/// profiles compile a nonzero reward, so their bond-consuming arms must name
/// the exact reward output.
pub fn fraud_prover_reward_routing_is_exact_v1(
  outputs: List<Output>,
  m_reward_output_index: Option<Int>,
  fraud_prover: VerificationKeyHash,
  expected_reward: Lovelace,
) -> Bool {
  when m_reward_output_index is {
    None -> expected_reward == 0
    Some(reward_output_index) -> {
      expect Some(reward_output) = outputs |> list.at(reward_output_index)
      and {
        fraud_prover_reward_output_is_exact_v1(
          reward_output,
          fraud_prover,
          expected_reward,
        ),
        list.length(
          list.filter(
            outputs,
            fn(output) { output.address == from_verification_key(fraud_prover) },
          ),
        ) == 1,
      }
    }
  }
}

/// The complete reward obligation of a bond-consuming
/// `RemoveFraudulentBlockHeader` arm: exact routing to the immutable fraud
/// prover enterprise address named by the authentic fraud-proof datum.
///
/// The slash is deliberately permissionless after a proof token exists. The
/// prover authenticated its identity when opening the computation thread; a
/// second signature here would let an unavailable prover block correction and
/// is not needed to protect any prover-controlled input. Exact address, value,
/// output uniqueness, bond consumption and the already-slashed branch remain
/// the complete no-redirection/no-double-reward boundary.
///
/// ## Why the reward rides the slash rather than the target block's removal
///
/// F04 §2.3's closing bullet reads "only the transaction that both consumes the
/// operator-directory node and removes the fraud-proved target block can pay
/// the reward". Those two events cannot always coincide. A fraudulent block
/// with successors must have them pruned first, one `RemoveFraudulentBlocksLink`
/// transaction at a time, and every one of those transactions must itself
/// present a `slashing_approach`; `OperatorAlreadySlashed` proves NON-membership
/// in both directories, so it is only constructible after the slash. The slash
/// therefore necessarily happens on the first pruning transaction, and pinning
/// the reward to `RemoveLastFraudulentBlock` would demand payment from a
/// transaction that consumes no bond.
///
/// The reward is pinned to the bond instead, which is where F04 §2.4 puts the
/// idempotency boundary in its own words: "at most one reward from an
/// operator's slashable bond". The singleton operator-directory node is
/// consumed exactly once, so exactly one reward is payable however many
/// fraud-proof tokens exist for the header — the "harmless-but-unrewarded"
/// duplicate the D4 ruling was chosen to produce.
pub fn route_fraud_prover_reward_v1(
  outputs: List<Output>,
  m_reward_output_index: Option<Int>,
  fraud_prover: VerificationKeyHash,
  expected_reward: Lovelace,
) -> Bool {
  fraud_prover_reward_routing_is_exact_v1(
    outputs,
    m_reward_output_index,
    fraud_prover,
    expected_reward,
  )
}

pub type MergeCommitments {
  withdrawals_root: ByteArray,
  forced_transactions_root: ByteArray,
  transactions_root: ByteArray,
  deposits_root: ByteArray,
  transition_trace_root: ByteArray,
  event_to_step_root: ByteArray,
  validation_traces_root: ByteArray,
  withdrawal_count: Int,
  forced_transaction_count: Int,
  l2_transaction_count: Int,
  deposit_count: Int,
  total_event_count: Int,
  transition_step_count: Int,
  validation_trace_count: Int,
}

pub fn merge_commitments_match_header(
  commitments: MergeCommitments,
  header: HeaderView,
) -> Bool {
  and {
    commitments.withdrawals_root == header.withdrawals_root,
    commitments.forced_transactions_root == header.forced_transactions_root,
    commitments.transactions_root == header.transactions_root,
    commitments.deposits_root == header.deposits_root,
    commitments.transition_trace_root == header.transition_trace_root,
    commitments.event_to_step_root == header.event_to_step_root,
    commitments.validation_traces_root == header.validation_traces_root,
    commitments.withdrawal_count == header.withdrawal_count,
    commitments.forced_transaction_count == header.forced_transaction_count,
    commitments.l2_transaction_count == header.l2_transaction_count,
    commitments.deposit_count == header.deposit_count,
    commitments.total_event_count == header.total_event_count,
    commitments.transition_step_count == header.transition_step_count,
    commitments.validation_trace_count == header.validation_trace_count,
  }
}

pub fn merge_settlement_binding_matches_header(
  header: HeaderView,
  header_node_key: ByteArray,
  m_settlement_id: Option<ByteArray>,
) -> Bool {
  let carries_l2_material = header_carries_l2_material(header)
  if carries_l2_material {
    m_settlement_id == Some(header_node_key)
  } else {
    m_settlement_id == None
  }
}

pub fn merge_settlement_id_at_route(
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  settlement_script_hash: PolicyId,
  m_settlement_redeemer_index: Option<Int>,
) -> Option<ByteArray> {
  when m_settlement_redeemer_index is {
    None -> None
    Some(settlement_redeemer_index) -> {
      let settlement_minting_redeemer_data = redeemers
        |> utils.get_redeemer_at(
            Mint(settlement_script_hash),
            settlement_redeemer_index,
          )
      expect settlement.Spawn { settlement_id, .. }: settlement.MintRedeemer = settlement_minting_redeemer_data
      Some(settlement_id)
    }
  }
}

fn header_carries_l2_material(header: HeaderView) -> Bool {
  or {
    header.transactions_root != env.empty_merkle_tree_root,
    header.deposits_root != env.empty_merkle_tree_root,
    header.withdrawals_root != env.empty_merkle_tree_root,
    header.forced_transactions_root != env.empty_merkle_tree_root,
  }
}

fn merge_to_confirmed_state(
  own_policy_id: PolicyId,
  da_attestation_policy_id: PolicyId,
  settlement_script_hash: PolicyId,
  self: Transaction,
  expected_protocol_version: Int,
  header_node_key: ByteArray,
  confirmed_state_input_outref: OutputReference,
  confirmed_state_output_index: Int,
  m_settlement_redeemer_index: Option<Int>,
  commitments: MergeCommitments,
) -> Bool {
  let Transaction { inputs, outputs, mint, redeemers, validity_range, .. } = self
  expect Some(continued_confirmed_state_output) = outputs
    |> list.at(confirmed_state_output_index)

  {
    let
        _confirmed_state_input,
        _confirmed_state_lovelace_change,
        input_confirmed_state_data,
        _input_header_node,
        _input_header_lovelace,
        input_header_node_key,
        input_header_node_data,
        _input_header_link,
        output_confirmed_state_data
      <- linked_list.fold_from_root(
      anchor_root_input_outref: confirmed_state_input_outref,
      continued_anchor_root_output: continued_confirmed_state_output,
      inputs: inputs,
      tx_mint: mint,
    )

    expect input_header_node_key == header_node_key
    expect
        StateQueueNode { header: input_header_data, da_attestation }: StateQueueNode
      = input_header_node_data
    expect
      when da_attestation is {
        availability.Attested { .. } -> True
        availability.Published { .. } -> True
        _ -> False
      }

    let input_header = decode_header_view(input_header_data)
    expect input_header.protocol_version == expected_protocol_version
    expect header_v1_is_valid(input_header)
    expect merge_commitments_match_header(commitments, input_header)

    let inclusive_tx_lower_bound = utils.get_inclusive_lower_bound_of_interval(
      validity_range,
    )
    expect
      inclusive_tx_lower_bound >= input_header.end_time + block_maturity_duration_v1

    expect
        ConfirmedState {
          start_time: input_confirmed_state_start_time,
          header_hash: input_confirmed_state_header_hash,
          prev_header_hash: input_confirmed_state_prev_header_hash,
          utxo_root: input_confirmed_state_utxo_root,
          end_time: input_confirmed_state_end_time,
          protocol_version: input_confirmed_state_protocol_version,
        }
      = input_confirmed_state_data
    expect
      Some(expected_protocol_version) == confirmed_state_next_header_protocol_version_v1(
        ConfirmedState {
          header_hash: input_confirmed_state_header_hash,
          prev_header_hash: input_confirmed_state_prev_header_hash,
          utxo_root: input_confirmed_state_utxo_root,
          start_time: input_confirmed_state_start_time,
          end_time: input_confirmed_state_end_time,
          protocol_version: input_confirmed_state_protocol_version,
        },
      )
    let expected_output_confirmed_state_data: Data = ConfirmedState {
      header_hash: header_node_key,
      prev_header_hash: input_confirmed_state_header_hash,
      utxo_root: input_header.utxos_root,
      start_time: input_confirmed_state_start_time,
      end_time: input_header.end_time,
      protocol_version: input_header.protocol_version,
    }
    expect output_confirmed_state_data == expected_output_confirmed_state_data

    if header_carries_l2_material(input_header) {
      merge_settlement_binding_matches_header(
        input_header,
        header_node_key,
        merge_settlement_id_at_route(
          redeemers,
          settlement_script_hash,
          m_settlement_redeemer_index,
        ),
      )
    } else {
      and {
        m_settlement_redeemer_index == None,
        merge_settlement_binding_matches_header(
          input_header,
          header_node_key,
          None,
        ),
      }
    }
  }
    |> finalize_linked_list(own_policy_id)
}

validator spend(
  state_queue_mint_script_hash: ByteArray,
  da_attestation_policy_id: PolicyId,
  availability_policy_id: PolicyId,
) {
  spend(
    _datum: Option<Data>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      LinkedListMutation ->
        linked_list.spend_for_adding_or_removing_an_element(
          state_queue_mint_script_hash,
          tx.mint,
        )
      AttachDaAttestation {
        state_queue_input_index,
        da_attestation_mint_redeemer_index,
      } -> {
        expect Some(Input { output_reference, .. }) = tx.inputs
          |> list.at(state_queue_input_index)
        expect output_reference == own_ref

        let mint_redeemer_data = tx.redeemers
          |> utils.get_redeemer_at(
              Mint(da_attestation_policy_id),
              da_attestation_mint_redeemer_index,
            )
        expect
            da_attestation.ApplyToStateQueue {
              state_queue_input_index: redeemer_state_queue_input_index,
              ..
            }: da_attestation.MintRedeemer
          = mint_redeemer_data
        redeemer_state_queue_input_index == state_queue_input_index
      }
      AvailabilityStatusUpdate {
        state_queue_input_index,
        state_queue_output_index,
        availability_mint_redeemer_index,
      } -> {
        expect Some(Input { output_reference, .. }) = tx.inputs
          |> list.at(state_queue_input_index)
        expect output_reference == own_ref

        availability_status_update_is_authorized_v1(
          tx.redeemers,
          availability_policy_id,
          availability_mint_redeemer_index,
          state_queue_input_index,
          state_queue_output_index,
        )
      }
    }
  }

  else(_) {
    fail
  }
}

/// Cross-policy authorization seam for the state-queue spending validator.
/// The availability mint policy owns the semantic status transition; this
/// helper proves that its selected redeemer names the exact queue input and
/// continuation being spent here. Only the three status-preserving arms are
/// admitted. Timeout removes queue structure and is therefore excluded.
pub fn availability_status_update_is_authorized_v1(
  redeemers: List<Pair<ScriptPurpose, Data>>,
  availability_policy_id: PolicyId,
  availability_mint_redeemer_index: Int,
  state_queue_input_index: Int,
  state_queue_output_index: Int,
) -> Bool {
  let redeemer_data = redeemers
    |> utils.get_redeemer_at(
        Mint(availability_policy_id),
        availability_mint_redeemer_index,
      )
  expect availability_redeemer: availability.MintRedeemerV1 = redeemer_data
  when availability_redeemer is {
    availability.MintBondFromAttestation {
      state_queue_input_index: bound_input,
      state_queue_output_index: bound_output,
      ..
    } -> and {
        bound_input == state_queue_input_index,
        bound_output == state_queue_output_index,
      }
    availability.OpenChallenge {
      state_queue_input_index: bound_input,
      state_queue_output_index: bound_output,
      ..
    } -> and {
        bound_input == state_queue_input_index,
        bound_output == state_queue_output_index,
      }
    availability.CloseChallenge {
      state_queue_input_index: bound_input,
      state_queue_output_index: bound_output,
      ..
    } -> and {
        bound_input == state_queue_input_index,
        bound_output == state_queue_output_index,
      }
    availability.SettleTranche { .. } -> False
    availability.TimeoutChallenge { .. } -> False
  }
}

/// Remove the node currently linked from the fraud-proved anchor.
///
/// `linked_list.remove` authenticates both list inputs and proves that the
/// removed node is the anchor's current successor.  That structural relation
/// is the authority for pruning: after an earlier child has been removed, the
/// anchor can point directly at a later descendant whose own `prev_header_hash`
/// (and operator) necessarily names the original chain rather than the anchor.
/// Requiring the descendant's operator to equal the fraudulent block's
/// operator would therefore deadlock correction whenever the scheduler rotates.
pub fn remove_fraudulent_blocks_link_v1(
  state_queue_policy_id: PolicyId,
  inputs: List<Input>,
  outputs: List<Output>,
  mint: assets.Value,
  fraudulent_blocks_header_hash: ByteArray,
  fraudulent_node_input_outref: OutputReference,
  fraudulent_node_output_index: Int,
) -> Bool {
  expect Some(fraudulent_node_output) = outputs
    |> list.at(fraudulent_node_output_index)
  {
    let
        _fraudulent_header_input,
        _fraudulent_header_lovelace_change,
        m_fraudulent_header_hash,
        _fraudulent_header_data,
        _removed_block_input,
        _removed_node_lovelace,
        _removed_node_header_hash,
        removed_node_header_data,
        _removed_node_link
      <- linked_list.remove(
      anchor_input_outref: fraudulent_node_input_outref,
      continued_anchor_element_output: fraudulent_node_output,
      inputs: inputs,
      tx_mint: mint,
    )
    expect Some(fraudulent_blocks_header_hash) == m_fraudulent_header_hash
    expect StateQueueNode { header: removed_node_header, .. }: StateQueueNode = removed_node_header_data
    expect
      decode_header_view(removed_node_header).protocol_version == protocol_version_v1
    True
  }
    |> finalize_linked_list(state_queue_policy_id)
}

/// Remove the fraud-proved block itself once it is the terminal queue node.
///
/// Unlike descendant pruning, this branch must bind the removed header's own
/// operator to the operator being slashed: this is the target block rather than
/// a structurally invalid descendant committed by a later scheduled operator.
pub fn remove_last_fraudulent_block_v1(
  state_queue_policy_id: PolicyId,
  inputs: List<Input>,
  outputs: List<Output>,
  mint: assets.Value,
  fraudulent_operator: VerificationKeyHash,
  fraudulent_blocks_header_hash: ByteArray,
  anchor_element_input_outref: OutputReference,
  anchor_element_output_index: Int,
) -> Bool {
  expect Some(anchor_element_output) = outputs
    |> list.at(anchor_element_output_index)
  {
    let
        _anchor_input,
        _anchor_lovelace_change,
        _m_anchor_key,
        _anchor_data,
        _removed_node_input,
        _removed_node_lovelace,
        removed_node_header_hash,
        removed_node_header_data,
        removed_node_link
      <- linked_list.remove(
      anchor_input_outref: anchor_element_input_outref,
      continued_anchor_element_output: anchor_element_output,
      inputs: inputs,
      tx_mint: mint,
    )
    expect fraudulent_blocks_header_hash == removed_node_header_hash
    expect StateQueueNode { header: removed_node_header, .. }: StateQueueNode = removed_node_header_data
    let removed_header_view = decode_header_view(removed_node_header)
    expect removed_header_view.operator_vkey == fraudulent_operator
    removed_node_link == None
  }
    |> finalize_linked_list(state_queue_policy_id)
}

/// The authenticated state-queue node is the applied-attestation singleton.
/// `AttachDaAttestation` changes this exact node from `no_da_attestation` to
/// the configured DA policy, so a timeout correction and an attachment spend
/// the same UTxO and cannot both confirm. A separately minted or thresholded
/// DAAT is not an absence oracle and is deliberately not treated as one: until
/// it wins that state transition, the block remains unapplied-attestation
/// state. Refetching after either confirmation makes the losing race reject.
pub fn unattested_block_timeout_elapsed_v1(
  timed_out_header_hash: ByteArray,
  authenticated_header_hash: ByteArray,
  node: StateQueueNode,
  validity_range: ValidityRange,
) -> Bool {
  let header = decode_header_view(node.header)
  let inclusive_tx_lower_bound = utils.get_inclusive_lower_bound_of_interval(
    validity_range,
  )
  and {
    authenticated_header_hash == timed_out_header_hash,
    authenticated_header_hash == crypto.blake2b_224(
      builtin.serialise_data(node.header),
    ),
    node.da_attestation == no_da_attestation,
    header_v1_is_valid(header),
    inclusive_tx_lower_bound >= header.end_time + da_attestation_timeout_v1,
  }
}

/// Prune the current descendant of an unattested, timed-out head before the
/// head itself is removed. The authenticated list link, not the descendant's
/// operator, makes it invalid once its predecessor is removed.
pub fn prune_timed_out_block_descendant_v1(
  state_queue_policy_id: PolicyId,
  inputs: List<Input>,
  outputs: List<Output>,
  reference_inputs: List<Input>,
  mint: assets.Value,
  validity_range: ValidityRange,
  timed_out_header_hash: ByteArray,
  confirmed_state_ref_input_index: Int,
  timed_out_node_input_outref: OutputReference,
  timed_out_node_output_index: Int,
) -> Bool {
  // A timed-out node may authorize pruning only after it is the current head.
  // The root NFT is a singleton; reading that UTxO as a reference input both
  // authenticates the head link and makes a concurrent root merge/removal
  // invalidate this transaction.
  expect Some(Input { output: confirmed_state_output, .. }) = reference_inputs
    |> list.at(confirmed_state_ref_input_index)
  expect
    {
      let
          _confirmed_state_address,
          _confirmed_state_lovelace,
          m_confirmed_state_key,
          confirmed_state_data,
          confirmed_state_link
        <- linked_list.get_element_info(confirmed_state_output)
      expect m_confirmed_state_key == None
      expect confirmed_state: ConfirmedState = confirmed_state_data
      expect
        confirmed_state_next_header_protocol_version_v1(confirmed_state) != None
      confirmed_state_link == Some(timed_out_header_hash)
    }
      |> finalize_linked_list(state_queue_policy_id)

  expect Some(timed_out_node_output) = outputs
    |> list.at(timed_out_node_output_index)
  {
    let
        _timed_out_node_input,
        _timed_out_node_lovelace_change,
        m_timed_out_node_header_hash,
        timed_out_node_data,
        _removed_descendant_input,
        _removed_descendant_lovelace,
        _removed_descendant_header_hash,
        removed_descendant_data,
        _removed_descendant_link
      <- linked_list.remove(
      anchor_input_outref: timed_out_node_input_outref,
      continued_anchor_element_output: timed_out_node_output,
      inputs: inputs,
      tx_mint: mint,
    )
    expect Some(authenticated_header_hash) = m_timed_out_node_header_hash
    expect timed_out_node: StateQueueNode = timed_out_node_data
    expect
      unattested_block_timeout_elapsed_v1(
        timed_out_header_hash,
        authenticated_header_hash,
        timed_out_node,
        validity_range,
      )
    expect removed_descendant: StateQueueNode = removed_descendant_data
    expect
      decode_header_view(removed_descendant.header).protocol_version == protocol_version_v1
    True
  }
    |> finalize_linked_list(state_queue_policy_id)
}

/// Remove the timed-out block from the state-queue root after every descendant
/// has been pruned. Requiring the removed link to be `None` prevents confirmed
/// state from being re-linked directly to a transition built on the discarded
/// block.
pub fn remove_unattested_head_after_timeout_v1(
  state_queue_policy_id: PolicyId,
  inputs: List<Input>,
  outputs: List<Output>,
  mint: assets.Value,
  validity_range: ValidityRange,
  timed_out_header_hash: ByteArray,
  confirmed_state_input_outref: OutputReference,
  confirmed_state_output_index: Int,
) -> Bool {
  expect Some(confirmed_state_output) = outputs
    |> list.at(confirmed_state_output_index)
  {
    let
        _confirmed_state_input,
        _confirmed_state_lovelace_change,
        m_confirmed_state_key,
        confirmed_state_data,
        _timed_out_node_input,
        _timed_out_node_lovelace,
        authenticated_header_hash,
        timed_out_node_data,
        timed_out_node_link
      <- linked_list.remove(
      anchor_input_outref: confirmed_state_input_outref,
      continued_anchor_element_output: confirmed_state_output,
      inputs: inputs,
      tx_mint: mint,
    )
    expect m_confirmed_state_key == None
    expect confirmed_state: ConfirmedState = confirmed_state_data
    expect
      confirmed_state_next_header_protocol_version_v1(confirmed_state) != None
    expect timed_out_node: StateQueueNode = timed_out_node_data
    expect
      unattested_block_timeout_elapsed_v1(
        timed_out_header_hash,
        authenticated_header_hash,
        timed_out_node,
        validity_range,
      )
    timed_out_node_link == None
  }
    |> finalize_linked_list(state_queue_policy_id)
}

fn unavailable_target_matches_v1(
  unavailable_header_hash: ByteArray,
  authenticated_header_hash: ByteArray,
  challenge_asset_name: ByteArray,
  node: StateQueueNode,
) -> Bool {
  and {
    authenticated_header_hash == unavailable_header_hash,
    authenticated_header_hash == crypto.blake2b_224(
      builtin.serialise_data(node.header),
    ),
    node.da_attestation == availability.Challenged {
      da_bond_asset_name: when node.da_attestation is {
        availability.Challenged { da_bond_asset_name, .. } -> da_bond_asset_name
        _ -> fail
      },
      challenge_asset_name,
    },
    header_v1_is_valid(decode_header_view(node.header)),
  }
}

pub fn prune_unavailable_block_descendant_v1(
  state_queue_policy_id: PolicyId,
  inputs: List<Input>,
  outputs: List<Output>,
  reference_inputs: List<Input>,
  mint: assets.Value,
  unavailable_header_hash: ByteArray,
  challenge_asset_name: ByteArray,
  confirmed_state_ref_input_index: Int,
  unavailable_node_input_outref: OutputReference,
  unavailable_node_output_index: Int,
) -> Bool {
  expect Some(Input { output: confirmed_state_output, .. }) = reference_inputs
    |> list.at(confirmed_state_ref_input_index)
  expect
    {
      let _address, _lovelace, m_key, confirmed_state_data, head_link <- linked_list.get_element_info(
        confirmed_state_output,
      )
      expect m_key == None
      expect confirmed_state: ConfirmedState = confirmed_state_data
      expect
        confirmed_state_next_header_protocol_version_v1(confirmed_state) != None
      head_link == Some(unavailable_header_hash)
    }
      |> finalize_linked_list(state_queue_policy_id)

  expect Some(unavailable_node_output) = outputs
    |> list.at(unavailable_node_output_index)
  {
    let
        _anchor_input,
        _anchor_lovelace_change,
        m_anchor_header_hash,
        anchor_node_data,
        _removed_input,
        _removed_lovelace,
        _removed_header_hash,
        removed_node_data,
        _removed_link
      <- linked_list.remove(
      anchor_input_outref: unavailable_node_input_outref,
      continued_anchor_element_output: unavailable_node_output,
      inputs: inputs,
      tx_mint: mint,
    )
    expect Some(authenticated_header_hash) = m_anchor_header_hash
    expect anchor_node: StateQueueNode = anchor_node_data
    expect
      unavailable_target_matches_v1(
        unavailable_header_hash,
        authenticated_header_hash,
        challenge_asset_name,
        anchor_node,
      )
    expect removed_node: StateQueueNode = removed_node_data
    decode_header_view(removed_node.header).protocol_version == protocol_version_v1
  }
    |> finalize_linked_list(state_queue_policy_id)
}

pub fn remove_unavailable_head_v1(
  state_queue_policy_id: PolicyId,
  inputs: List<Input>,
  outputs: List<Output>,
  mint: assets.Value,
  unavailable_header_hash: ByteArray,
  challenge_asset_name: ByteArray,
  confirmed_state_input_outref: OutputReference,
  confirmed_state_output_index: Int,
) -> Bool {
  expect Some(confirmed_state_output) = outputs
    |> list.at(confirmed_state_output_index)
  {
    let
        _root_input,
        _root_lovelace_change,
        m_root_key,
        confirmed_state_data,
        _removed_input,
        _removed_lovelace,
        authenticated_header_hash,
        removed_node_data,
        removed_link
      <- linked_list.remove(
      anchor_input_outref: confirmed_state_input_outref,
      continued_anchor_element_output: confirmed_state_output,
      inputs: inputs,
      tx_mint: mint,
    )
    expect m_root_key == None
    expect confirmed_state: ConfirmedState = confirmed_state_data
    expect
      confirmed_state_next_header_protocol_version_v1(confirmed_state) != None
    expect removed_node: StateQueueNode = removed_node_data
    expect
      unavailable_target_matches_v1(
        unavailable_header_hash,
        authenticated_header_hash,
        challenge_asset_name,
        removed_node,
      )
    removed_link == None
  }
    |> finalize_linked_list(state_queue_policy_id)
}

pub fn validate_commit_block_header_v1(
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
  active_operators_script_hash: PolicyId,
  active_operators_addr: Address,
  scheduler_script_hash: PolicyId,
  da_attestation_policy_id: PolicyId,
  own_policy_id: PolicyId,
  self: Transaction,
  new_block_output_index: Int,
  continued_latest_block_output_index: Int,
  operator: VerificationKeyHash,
  scheduler_ref_input_index: Int,
  active_operators_input_index: Int,
  active_operators_redeemer_index: Int,
  m_confirmed_state_ref_input_index: Option<Int>,
  m_head_state_queue_node_ref_input_index: Option<Int>,
) -> Bool {
  let correction_lock_address = from_script(correction_lock_script_hash)
  let
      Transaction {
        inputs,
        outputs,
        reference_inputs,
        mint,
        extra_signatories,
        redeemers,
        validity_range,
        ..
      }
    = self
  // Commit the block header to the state queue, with the operator's
  // content:
  // 1. Let `operator` be a redeemer argument indicating the pub key hash
  //    of the operator committing the block header.
  // 2. The transaction must be signed by `operator`.
  expect operator |> utils.has_signed(extra_signatories)

  expect Some(continued_anchor_element_output) = outputs
    |> list.at(continued_latest_block_output_index)
  expect Some(new_block_output) = outputs
    |> list.at(new_block_output_index)

  // 3. Indicated outputs must properly adhere to unordered linked list
  //    append requirements.
  {
    let
        _anchor_input,
        _anchor_lovelace_change,
        m_anchor_key,
        anchor_data,
        _output_header_lovelace,
        output_header_key,
        output_node_data
      <- linked_list.append_unordered(
      continued_anchor_element_output: continued_anchor_element_output,
      new_node_output: new_block_output,
      inputs: inputs,
      tx_mint: mint,
    )

    expect
      correction_lock.references_idle(
        reference_inputs,
        hub_oracle_script_hash,
        correction_lock_address,
      )

    expect
        StateQueueNode { header: output_header_data, da_attestation }: StateQueueNode
      = output_node_data
    expect da_attestation == no_da_attestation

    // 4. The new block in state queue must have the hash of the header
    //    as its linked list key.
    let output_header_hash = crypto.blake2b_224(
      builtin.serialise_data(output_header_data),
    )
    expect output_header_key == output_header_hash

    // 5. New element's data must be `HeaderV1` with a valid time range.
    let output_header = decode_header_view(output_header_data)
    let output_operator_vkey = output_header.operator_vkey
    expect commit_block_header_output_is_valid_v1(output_header)
    expect
      commit_bound_header_time_is_valid(
        output_header.start_time,
        output_header.end_time,
        validity_range,
      )

    // Verify that it is the operator's turn to commit according to the
    // scheduler:
    // 7. The transaction must include a reference input with the
    //    scheduler NFT. Let that input's datum be the `scheduler_state`.
    expect scheduler.ActiveOperator { operator: scheduler_operator, .. } = scheduler.get_datum(
      reference_inputs: reference_inputs,
      scheduler_policy_id: scheduler_script_hash,
      scheduler_ref_input_index: scheduler_ref_input_index,
    )

    // Update the operator's timestamp in the active operators set:
    // 9. The transaction must include an input, spent via the
    //    `UpdateBondHoldNewState` redeemer, of an `active_operators`
    //    node with a key matching the `operator`.
    expect active_operators.UpdateBondHoldNewState { active_operator, .. } = utils.get_spending_redeemer_data_at(
      active_operators_addr,
      active_operators_input_index,
      active_operators_redeemer_index,
      inputs,
      redeemers,
    )
    // 6, 8, 9. The header, authenticated scheduler state, and
    // authenticated active-operator spend must all name the redeemer
    // operator.
    expect
      commit_block_header_operator_is_legitimate_v1(
        output_operator_vkey,
        operator,
        scheduler_operator,
        active_operator,
      )

    // To validate the output header, we must take two routes depending on
    // whether the anchor element is the root (i.e. confirmed state), or a
    // node (i.e. latest block in the queue).
    when m_anchor_key is {
      Some(header_hash) -> {
        // Q61 starvation fence. A non-empty append observes the
        // singleton root's current head. If the tail anchor is not that
        // head, a second reference authenticates the head node. Timeout
        // pruning spends/continues the same head, so an old append
        // conflicts; the upper-bound rule prevents a refetched append
        // from extending the queue after the deadline.
        expect Some(confirmed_state_ref_input_index) = m_confirmed_state_ref_input_index
        let head_node = authenticated_queue_head_for_append_v1(
          reference_inputs,
          own_policy_id,
          header_hash,
          anchor_data,
          confirmed_state_ref_input_index,
          m_head_state_queue_node_ref_input_index,
        )
        expect
          state_queue_head_allows_append_v1(
            head_node,
            da_attestation_policy_id,
            validity_range,
          )

        // 10a. New block must properly carry over data from previous
        //      block.
        expect StateQueueNode { header: anchor_header_data, .. }: StateQueueNode
          = anchor_data
        let anchor_header = decode_header_view(anchor_header_data)
        expect
          commit_block_header_carries_previous_block_v1(
            output_header,
            header_hash,
            anchor_header,
          )

        // Done.
        True
      }
      None -> {
        // The consumed anchor is the root, so the queue is empty and
        // there is no head to fence or reference.
        expect m_confirmed_state_ref_input_index == None
        expect m_head_state_queue_node_ref_input_index == None

        // 10b. New block must properly carry over data from confirmed
        //      state.
        expect
            ConfirmedState {
              header_hash,
              prev_header_hash,
              utxo_root,
              start_time,
              end_time,
              protocol_version,
            }
          = anchor_data
        expect
          commit_block_header_carries_confirmed_state_v1(
            output_header,
            ConfirmedState {
              header_hash,
              prev_header_hash,
              utxo_root,
              start_time,
              end_time,
              protocol_version,
            },
          )

        // Done.
        True
      }
    }
  }
    |> finalize_linked_list(own_policy_id)
}

pub fn validate_remove_unattested_v1(
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
  own_policy_id: PolicyId,
  self: Transaction,
  timed_out_header_hash: ByteArray,
  removal_approach: AttestationTimeoutRemovalApproach,
) -> Bool {
  let correction_lock_address = from_script(correction_lock_script_hash)
  let
      Transaction {
        inputs,
        outputs,
        reference_inputs,
        mint,
        validity_range,
        ..
      }
    = self

  // Q61: the state-queue node itself serializes DA attachment and
  // permissionless timeout correction. No operator-directory input,
  // slash redeemer, fraud proof, reward, or signature authorizes this
  // liveness path.
  expect _correction_lock_input = correction_lock.unique_input(
    inputs,
    hub_oracle_script_hash,
    correction_lock_address,
  )
  when removal_approach is {
    PruneTimedOutBlockDescendant {
      confirmed_state_ref_input_index,
      timed_out_node_input_outref,
      timed_out_node_output_index,
    } ->
      prune_timed_out_block_descendant_v1(
        own_policy_id,
        inputs: inputs,
        outputs: outputs,
        reference_inputs: reference_inputs,
        mint: mint,
        validity_range: validity_range,
        timed_out_header_hash: timed_out_header_hash,
        confirmed_state_ref_input_index: confirmed_state_ref_input_index,
        timed_out_node_input_outref: timed_out_node_input_outref,
        timed_out_node_output_index: timed_out_node_output_index,
      )
    RemoveTimedOutHead {
      confirmed_state_input_outref,
      confirmed_state_output_index,
    } ->
      remove_unattested_head_after_timeout_v1(
        own_policy_id,
        inputs: inputs,
        outputs: outputs,
        mint: mint,
        validity_range: validity_range,
        timed_out_header_hash: timed_out_header_hash,
        confirmed_state_input_outref: confirmed_state_input_outref,
        confirmed_state_output_index: confirmed_state_output_index,
      )
  }
}

pub fn validate_remove_unavailable_v1(
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
  availability_policy_id: PolicyId,
  own_policy_id: PolicyId,
  self: Transaction,
  unavailable_header_hash: ByteArray,
  challenge_asset_name: AssetName,
  removal_approach: AttestationTimeoutRemovalApproach,
) -> Bool {
  let correction_lock_address = from_script(correction_lock_script_hash)
  let Transaction { inputs, outputs, reference_inputs, mint, .. } = self
  let lock_input = correction_lock.unique_input(
    inputs,
    hub_oracle_script_hash,
    correction_lock_address,
  )
  let expected_lock = correction_lock.Locked {
    target_header_hash: unavailable_header_hash,
    correction_identity: correction_lock.AvailabilityChallenge {
      challenge_asset_name,
    },
  }
  expect
    when correction_lock.decode_datum(lock_input.output) is {
      correction_lock.Idle ->
        quantity_of(mint, availability_policy_id, challenge_asset_name) == -1
      current -> and {
          current == expected_lock,
          quantity_of(mint, availability_policy_id, challenge_asset_name) == 0,
        }
    }
  when removal_approach is {
    PruneTimedOutBlockDescendant {
      confirmed_state_ref_input_index,
      timed_out_node_input_outref,
      timed_out_node_output_index,
    } ->
      prune_unavailable_block_descendant_v1(
        own_policy_id,
        inputs,
        outputs,
        reference_inputs,
        mint,
        unavailable_header_hash,
        challenge_asset_name,
        confirmed_state_ref_input_index,
        timed_out_node_input_outref,
        timed_out_node_output_index,
      )
    RemoveTimedOutHead {
      confirmed_state_input_outref,
      confirmed_state_output_index,
    } ->
      remove_unavailable_head_v1(
        own_policy_id,
        inputs,
        outputs,
        mint,
        unavailable_header_hash,
        challenge_asset_name,
        confirmed_state_input_outref,
        confirmed_state_output_index,
      )
  }
}

pub fn validate_remove_fraudulent_v1(
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
  active_operators_script_hash: PolicyId,
  retired_operators_script_hash: PolicyId,
  fraud_proof_script_hash: PolicyId,
  own_policy_id: PolicyId,
  self: Transaction,
  fraudulent_operator: VerificationKeyHash,
  fraudulent_blocks_header_hash: ByteArray,
  slashing_approach: SlashingApproach,
  fraud_proof_ref_input_index: Int,
  block_removal_approach: BlockRemovalApproach,
) -> Bool {
  let correction_lock_address = from_script(correction_lock_script_hash)
  let
      Transaction {
        inputs,
        outputs,
        reference_inputs,
        mint,
        redeemers,
        extra_signatories,
        ..
      }
    = self

  // 0. Verify that fraud has been proved for the specified block header,
  //    and read the fraud prover off the same authentic reference input.
  //
  //    This is the guard that used to sit at the end of this handler as
  //    step 5; it is unmoved in meaning — the referenced token's policy
  //    and header suffix are authenticated exactly as before, and the
  //    header equality below is the same `expect`. It is read first only
  //    because the prover it also yields is what the slashing arms route
  //    the reward to, and the fraud-proof datum is where F04 §2.3 says
  //    that identity lives.
  let referenced_fraudulent_header_hash, fraud_prover <- fraud_proof.get_proven_fraud_record(
    reference_inputs,
    fraud_proof_script_hash,
    fraud_proof_ref_input_index,
  )
  expect referenced_fraudulent_header_hash == fraudulent_blocks_header_hash

  expect _correction_lock_input = correction_lock.unique_input(
    inputs,
    hub_oracle_script_hash,
    correction_lock_address,
  )

  // Slash the fraudulent operator:
  // 1. Let `slashing_approach` be a sum type indicating whether the
  //    fraudulent operator is active, retired, or already slashed.
  expect
    when slashing_approach is {
      SlashActiveOperator {
        active_operators_redeemer_index,
        m_fraud_prover_reward_output_index,
      } -> {
        // 2a. If the operator is active:
        //     The transaction must Remove a node from the
        //     `active_operators` set via the `SlashOperatorBadState`
        //     redeemer. The `slashed_operator` argument provided to that
        //     redeemer must match `fraudulent_operator`.
        let slashing_reason_from_active_operators = operator_directory.cross_validate_slashing_reason(
          fraudulent_operator,
          active_operators_script_hash,
          active_operators_redeemer_index,
          fn(redeemer_data: Data) {
            expect active_operators.SlashOperator { slashing_arguments, .. } = redeemer_data
            slashing_arguments
          },
          redeemers,
        )
        expect SlashOperatorForBadState { .. } = slashing_reason_from_active_operators

        // 2a-i. This arm consumes the operator's bond, so it is the
        //       reward-bearing transaction: exactly
        //       `env.fraud_prover_reward` must reach the prover named by
        //       the fraud-proof datum. Correction remains permissionless
        //       once the permanent proof token exists.
        route_fraud_prover_reward_v1(
          outputs,
          m_fraud_prover_reward_output_index,
          fraud_prover,
          env.fraud_prover_reward,
        )
      }
      SlashRetiredOperator {
        retired_operators_redeemer_index,
        m_fraud_prover_reward_output_index,
      } -> {
        // 2b. If the operator is retired:
        //     The transaction must Remove a node from the
        //     `retired_operators` set via the `SlashOperatorBadState`
        //     redeemer. The `slashed_operator` argument provided to that
        //     redeemer must match `fraudulent_operator`.
        let slashing_reason_from_retired_operators = operator_directory.cross_validate_slashing_reason(
          fraudulent_operator,
          retired_operators_script_hash,
          retired_operators_redeemer_index,
          fn(redeemer_data: Data) {
            expect retired_operators.SlashOperator { slashing_arguments } = redeemer_data
            slashing_arguments
          },
          redeemers,
        )
        expect SlashOperatorForBadState { .. } = slashing_reason_from_retired_operators

        // 2b-i. Same reward routing as the active arm: the retired
        //       operator's bond is the same single slashable tranche.
        route_fraud_prover_reward_v1(
          outputs,
          m_fraud_prover_reward_output_index,
          fraud_prover,
          env.fraud_prover_reward,
        )
      }
      OperatorAlreadySlashed {
        active_operators_element_ref_input_index,
        retired_operators_element_ref_input_index,
      } ->
        // 2c. If the operator has already been slashed:
        //     The transaction must refer to elements from both the active
        //     and retired operators directories (which are ordered linked
        //     lists), and show that neither lists include the fraudulent
        //     operator.
        and {
          // 2c-i. No bond is consumed here, so no reward may be paid —
          //       the D4 exclusivity half that keeps a second claim
          //       against an already-forfeited bond unrewarded.
          no_output_pays_fraud_prover_reward_v1(
            outputs,
            fraud_prover,
            env.fraud_prover_reward,
          ),
          fraudulent_operator
            |> operator_directory.operator_is_not_a_member(
                reference_inputs: reference_inputs,
                element_ref_input_index: active_operators_element_ref_input_index,
              )
            |> active_operators.finalize_linked_list(
                active_operators_script_hash,
              ),
          fraudulent_operator
            |> operator_directory.operator_is_not_a_member(
                reference_inputs: reference_inputs,
                element_ref_input_index: retired_operators_element_ref_input_index,
              )
            |> retired_operators.finalize_linked_list(
                nft_policy_id: retired_operators_script_hash,
              ),
        }
    }

  // Remove fraudulent block or its link from the state queue:
  // 3. Let `block_removal_approach` be a redeemer argument that specifies
  //    whether the fraudulent block itself can be removed:
  expect
    when block_removal_approach is {
      RemoveFraudulentBlocksLink {
        fraudulent_node_input_outref,
        fraudulent_node_output_index,
      } ->
        // 4a. If the fraudulent block has a link, its successor node must
        //     be removed first.
        //
        //     > [!NOTE]
        //     > The successor node is also considered fraudulent because
        //     > of getting appended to a fraudulent block, so it does not
        //     > need to have an associated fraud proof token. Its own
        //     > operator is deliberately irrelevant: the authenticated
        //     > list link proves it is a descendant of the removed anchor,
        //     > including after operator rotation or earlier pruning.
        remove_fraudulent_blocks_link_v1(
          own_policy_id,
          inputs: inputs,
          outputs: outputs,
          mint: mint,
          fraudulent_blocks_header_hash: fraudulent_blocks_header_hash,
          fraudulent_node_input_outref: fraudulent_node_input_outref,
          fraudulent_node_output_index: fraudulent_node_output_index,
        )
      RemoveLastFraudulentBlock {
        anchor_element_input_outref,
        anchor_element_output_index,
      } ->
        // 4b. If the fraudulent block has no links, it can be removed
        //     from the very end of the state queue.
        remove_last_fraudulent_block_v1(
          own_policy_id,
          inputs: inputs,
          outputs: outputs,
          mint: mint,
          fraudulent_operator: fraudulent_operator,
          fraudulent_blocks_header_hash: fraudulent_blocks_header_hash,
          anchor_element_input_outref: anchor_element_input_outref,
          anchor_element_output_index: anchor_element_output_index,
        )
    }

  // Done. (The fraud-proof reference input that used to be verified here
  // as step 5 is verified at step 0 above, where the prover it carries is
  // needed by the reward routing.)
  True
}

pub fn validate_merge_v1(
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
  settlement_script_hash: PolicyId,
  da_attestation_policy_id: PolicyId,
  own_policy_id: PolicyId,
  self: Transaction,
  header_node_key: ByteArray,
  confirmed_state_input_outref: OutputReference,
  confirmed_state_output_index: Int,
  m_settlement_redeemer_index: Option<Int>,
  merged_block_withdrawals_root: ByteArray,
  merged_block_forced_transactions_root: ByteArray,
  merged_block_transactions_root: ByteArray,
  merged_block_deposits_root: ByteArray,
  merged_block_transition_trace_root: ByteArray,
  merged_block_event_to_step_root: ByteArray,
  merged_block_validation_traces_root: ByteArray,
  merged_block_withdrawal_count: Int,
  merged_block_forced_transaction_count: Int,
  merged_block_l2_transaction_count: Int,
  merged_block_deposit_count: Int,
  merged_block_total_event_count: Int,
  merged_block_transition_step_count: Int,
  merged_block_validation_trace_count: Int,
) -> Bool {
  let correction_lock_address = from_script(correction_lock_script_hash)
  expect
    correction_lock.references_idle(
      self.reference_inputs,
      hub_oracle_script_hash,
      correction_lock_address,
    )
  merge_to_confirmed_state(
    own_policy_id,
    da_attestation_policy_id,
    settlement_script_hash,
    self,
    protocol_version_v1,
    header_node_key,
    confirmed_state_input_outref,
    confirmed_state_output_index,
    m_settlement_redeemer_index,
    MergeCommitments {
      withdrawals_root: merged_block_withdrawals_root,
      forced_transactions_root: merged_block_forced_transactions_root,
      transactions_root: merged_block_transactions_root,
      deposits_root: merged_block_deposits_root,
      transition_trace_root: merged_block_transition_trace_root,
      event_to_step_root: merged_block_event_to_step_root,
      validation_traces_root: merged_block_validation_traces_root,
      withdrawal_count: merged_block_withdrawal_count,
      forced_transaction_count: merged_block_forced_transaction_count,
      l2_transaction_count: merged_block_l2_transaction_count,
      deposit_count: merged_block_deposit_count,
      total_event_count: merged_block_total_event_count,
      transition_step_count: merged_block_transition_step_count,
      validation_trace_count: merged_block_validation_trace_count,
    },
  )
}

fn require_operational_yield_v1(
  reference_script_auth_policy_id: PolicyId,
  role_asset_name: AssetName,
  yield_to_ref_input_index: Int,
  self: Transaction,
) -> Bool {
  let Transaction { reference_inputs, withdrawals, redeemers, .. } = self
  expect _yield_script_hash = state_queue_yield.require_authenticated_zero_yield(
    reference_inputs,
    withdrawals,
    redeemers,
    reference_script_auth_policy_id,
    role_asset_name,
    yield_to_ref_input_index,
  )
  True
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
  active_operators_script_hash: PolicyId,
  active_operators_addr: Address,
  retired_operators_script_hash: PolicyId,
  scheduler_script_hash: PolicyId,
  fraud_proof_script_hash: PolicyId,
  settlement_script_hash: PolicyId,
  da_attestation_policy_id: PolicyId,
  availability_policy_id: PolicyId,
  reference_script_auth_policy_id: PolicyId,
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    let correction_lock_address = from_script(correction_lock_script_hash)
    when redeemer is {
      InitV1 { output_index } -> {
        let Transaction { outputs, mint, validity_range, .. } = self

        expect Some(root_output) = outputs |> list.at(output_index)
        let correction_lock_output = correction_lock.unique_output(
          outputs,
          hub_oracle_script_hash,
          correction_lock_address,
        )
        expect
          correction_lock.decode_datum(correction_lock_output) == correction_lock.Idle
        expect
          quantity_of(mint, hub_oracle_script_hash, correction_lock.asset_name) == 1
        let (_current_time_lower, current_time_upper) = utils.get_inclusive_bounds_of_a_short_validity_range(
          validity_range,
        )

        expect Some(expected_genesis_state) = genesis_confirmed_state_v1(
          current_time_upper,
        )
        let expected_root_data: Data = expected_genesis_state

        expect
          linked_list.init(
            nonce_validated: quantity_of(
              mint,
              hub_oracle_script_hash,
              hub.asset_name,
            ) == 1,
            produced_element_output: root_output,
            tx_mint: mint,
            root_validator: fn(_, _root_lovelace: Lovelace, root_data: Data) -> Bool {
              root_data == expected_root_data
            },
          )
            |> linked_list.run_root_with(
                own_policy_id,
                confirmed_state_asset_name,
              )

        True
      }
      Deinit -> {
        let Transaction { inputs, outputs, mint, .. } = self

        // 1. Hub oracle NFT must be getting burnt.
        expect quantity_of(mint, hub_oracle_script_hash, hub.asset_name) == -1

        let correction_lock_input = correction_lock.unique_input(
          inputs,
          hub_oracle_script_hash,
          correction_lock_address,
        )
        expect
          correction_lock.decode_datum(correction_lock_input.output) == correction_lock.Idle
        expect
          quantity_of(mint, hub_oracle_script_hash, correction_lock.asset_name) == -1
        expect correction_lock.has_no_output(outputs, hub_oracle_script_hash)
        // 2. Linked list de-initialization must be valid. No other validations
        //    required.
        expect
          linked_list.deinit(
            inputs: inputs,
            tx_mint: mint,
            root_validator: fn(_, _, _) -> Bool { True },
          )
            |> linked_list.run_root_with(
                own_policy_id,
                confirmed_state_asset_name,
              )

        // Done.
        True
      }
      CommitBlockHeader { yield_to_ref_input_index, .. } ->
        require_operational_yield_v1(
          reference_script_auth_policy_id,
          state_queue_yield.commit_role,
          yield_to_ref_input_index,
          self,
        )
      RemoveUnattestedBlockAfterTimeout { yield_to_ref_input_index, .. } ->
        require_operational_yield_v1(
          reference_script_auth_policy_id,
          state_queue_yield.unattested_timeout_role,
          yield_to_ref_input_index,
          self,
        )
      RemoveUnavailableBlockAfterTimeout { yield_to_ref_input_index, .. } ->
        require_operational_yield_v1(
          reference_script_auth_policy_id,
          state_queue_yield.unavailable_timeout_role,
          yield_to_ref_input_index,
          self,
        )
      RemoveFraudulentBlockHeader { yield_to_ref_input_index, .. } ->
        require_operational_yield_v1(
          reference_script_auth_policy_id,
          state_queue_yield.fraud_removal_role,
          yield_to_ref_input_index,
          self,
        )
      MergeToConfirmedStateV1 { yield_to_ref_input_index, .. } ->
        require_operational_yield_v1(
          reference_script_auth_policy_id,
          state_queue_yield.merge_role,
          yield_to_ref_input_index,
          self,
        )
    }
  }

  else(_) {
    fail
  }
}

const q49_header_hash_a =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const q49_header_hash_b =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const q49_genesis_header_hash =
  #"00000000000000000000000000000000000000000000000000000000"

const q49_operator = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const q49_other_operator =
  #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const q49_utxo_root_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const q49_utxo_root_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

fn q49_header_fixture() -> HeaderV1 {
  HeaderV1 {
    prev_utxos_root: q49_utxo_root_a,
    utxos_root: q49_utxo_root_b,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 100,
    end_time: 200,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: q49_header_hash_a,
    operator_vkey: q49_operator,
    protocol_version: protocol_version_v1,
  }
}

test q49_l295_header_shape_control_and_interval_mutations() {
  let control = q49_header_fixture()
  and {
    commit_block_header_output_is_valid_v1(control),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, start_time: control.end_time },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, start_time: control.end_time + 1 },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, protocol_version: 0 },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, protocol_version: 2 },
    ),
  }
}

test q49_l295_validation_context_scalar_control_and_mutations() {
  let control = q49_header_fixture()
  let supported_network_and_runtime_values = HeaderV1 {
    ..control,
    block_slot: 1,
    expected_network_id: 1,
    min_fee_a: 44,
    min_fee_b: 155_381,
  }
  and {
    commit_block_header_output_is_valid_v1(control),
    commit_block_header_output_is_valid_v1(supported_network_and_runtime_values),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, block_slot: -1 },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, expected_network_id: -1 },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, expected_network_id: 2 },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, expected_network_id: 255 },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, min_fee_a: -1 },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, min_fee_b: -1 },
    ),
  }
}

test q49_l295_scheduled_operator_control_and_independent_mutations() {
  and {
    commit_block_header_operator_is_legitimate_v1(
      q49_operator,
      q49_operator,
      q49_operator,
      q49_operator,
    ),
    !commit_block_header_operator_is_legitimate_v1(
      q49_other_operator,
      q49_operator,
      q49_operator,
      q49_operator,
    ),
    !commit_block_header_operator_is_legitimate_v1(
      q49_operator,
      q49_operator,
      q49_other_operator,
      q49_operator,
    ),
    !commit_block_header_operator_is_legitimate_v1(
      q49_operator,
      q49_operator,
      q49_operator,
      q49_other_operator,
    ),
    !commit_block_header_operator_is_legitimate_v1(
      q49_operator,
      q49_other_operator,
      q49_operator,
      q49_operator,
    ),
  }
}

test q49_l295_previous_header_control_and_independent_mutations() {
  let previous = q49_header_fixture()
  let output = HeaderV1 {
    ..q49_header_fixture(),
    prev_header_hash: q49_header_hash_b,
    prev_utxos_root: previous.utxos_root,
    start_time: previous.end_time,
    end_time: previous.end_time + 100,
  }
  and {
    commit_block_header_carries_previous_block_v1(
      output,
      q49_header_hash_b,
      previous,
    ),
    !commit_block_header_carries_previous_block_v1(
      HeaderV1 { ..output, prev_header_hash: q49_header_hash_a },
      q49_header_hash_b,
      previous,
    ),
    !commit_block_header_carries_previous_block_v1(
      HeaderV1 { ..output, prev_utxos_root: q49_utxo_root_a },
      q49_header_hash_b,
      previous,
    ),
    !commit_block_header_carries_previous_block_v1(
      HeaderV1 { ..output, start_time: previous.end_time - 1 },
      q49_header_hash_b,
      previous,
    ),
    !commit_block_header_carries_previous_block_v1(
      HeaderV1 { ..output, start_time: previous.end_time + 1 },
      q49_header_hash_b,
      previous,
    ),
    !commit_block_header_carries_previous_block_v1(
      HeaderV1 { ..output, protocol_version: 2 },
      q49_header_hash_b,
      previous,
    ),
  }
}

test q49_l295_confirmed_genesis_control_and_independent_mutations() {
  expect Some(genesis) = genesis_confirmed_state_v1(100)
  let output = HeaderV1 {
    ..q49_header_fixture(),
    prev_header_hash: q49_genesis_header_hash,
    prev_utxos_root: env.empty_merkle_tree_root,
  }
  let malformed_header_hash = ConfirmedState {
    ..genesis,
    header_hash: q49_header_hash_a,
  }
  let malformed_prev_hash = ConfirmedState {
    ..genesis,
    prev_header_hash: q49_header_hash_a,
  }
  let malformed_utxo_root = ConfirmedState {
    ..genesis,
    utxo_root: q49_utxo_root_a,
  }
  let malformed_interval = ConfirmedState {
    ..genesis,
    end_time: genesis.start_time + 1,
  }
  and {
    commit_block_header_carries_confirmed_state_v1(output, genesis),
    !commit_block_header_carries_confirmed_state_v1(
      HeaderV1 { ..output, prev_header_hash: q49_header_hash_b },
      genesis,
    ),
    !commit_block_header_carries_confirmed_state_v1(
      HeaderV1 { ..output, prev_utxos_root: q49_utxo_root_b },
      genesis,
    ),
    !commit_block_header_carries_confirmed_state_v1(
      HeaderV1 { ..output, start_time: genesis.end_time - 1 },
      genesis,
    ),
    !commit_block_header_carries_confirmed_state_v1(
      HeaderV1 { ..output, start_time: genesis.end_time + 1 },
      genesis,
    ),
    !commit_block_header_carries_confirmed_state_v1(
      HeaderV1 { ..output, protocol_version: 2 },
      genesis,
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      malformed_header_hash,
    ),
    !commit_block_header_carries_confirmed_state_v1(output, malformed_prev_hash),
    !commit_block_header_carries_confirmed_state_v1(output, malformed_utxo_root),
    !commit_block_header_carries_confirmed_state_v1(output, malformed_interval),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..genesis, protocol_version: protocol_version_v1 },
    ),
  }
}

test q49_l295_confirmed_ordinary_control_and_independent_mutations() {
  let ordinary = ConfirmedState {
    header_hash: q49_header_hash_b,
    prev_header_hash: q49_header_hash_a,
    utxo_root: q49_utxo_root_a,
    start_time: 0,
    end_time: 100,
    protocol_version: protocol_version_v1,
  }
  let output = HeaderV1 {
    ..q49_header_fixture(),
    prev_header_hash: q49_header_hash_b,
  }
  and {
    commit_block_header_carries_confirmed_state_v1(output, ordinary),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, header_hash: q49_header_hash_a },
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, header_hash: q49_genesis_header_hash },
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, start_time: -1 },
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, start_time: ordinary.end_time + 1 },
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, protocol_version: 0 },
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, protocol_version: 2 },
    ),
  }
}

// ## D3/D4 fraud-prover reward routing (2026-08-11 owner ruling 7)
//
// The lovelace figure below is F04 §2.2's bounded-acceptance
// `fraud_prover_reward` (400 tADA) used purely as a **test vector**. Nothing
// here is an economics authority: the compiled validator routes
// `env.fraud_prover_reward` unchanged. Exercising the seams at the bounded
// profile amount makes "one lovelace over" and "one lovelace under"
// observable without duplicating the environment as test authority.

const d3_fraud_prover =
  #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const d3_other_key = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const d3_reward: Lovelace = 400_000_000

const d3_foreign_policy =
  #"abababababababababababababababababababababababababababab"

fn d3_reward_output(lovelace: Lovelace) -> Output {
  Output {
    address: from_verification_key(d3_fraud_prover),
    value: assets.from_lovelace(lovelace),
    datum: NoDatum,
    reference_script: None,
  }
}

fn d3_unrelated_output() -> Output {
  Output {
    address: from_verification_key(d3_other_key),
    value: assets.from_lovelace(d3_reward),
    datum: NoDatum,
    reference_script: None,
  }
}

/// D3 exactness: the reward reaches the fraud prover's enterprise address and
/// nowhere else, at exactly the ruled amount. Over- and under-payment are
/// refused symmetrically because the bond tranche is conserved by this output
/// plus the transaction fee, so a lovelace either way is taken from the
/// treasury's share or from the prover's.
test d3_reward_output_exactness_control_and_mutations() {
  let control = d3_reward_output(d3_reward)
  and {
    fraud_prover_reward_output_is_exact_v1(control, d3_fraud_prover, d3_reward),
    !fraud_prover_reward_output_is_exact_v1(
      d3_reward_output(d3_reward + 1),
      d3_fraud_prover,
      d3_reward,
    ),
    !fraud_prover_reward_output_is_exact_v1(
      d3_reward_output(d3_reward - 1),
      d3_fraud_prover,
      d3_reward,
    ),
    !fraud_prover_reward_output_is_exact_v1(
      Output { ..control, address: from_verification_key(d3_other_key) },
      d3_fraud_prover,
      d3_reward,
    ),
    !fraud_prover_reward_output_is_exact_v1(
      Output {
        ..control,
        address: from_verification_key(d3_fraud_prover)
          |> with_delegation_key(d3_other_key),
      },
      d3_fraud_prover,
      d3_reward,
    ),
    !fraud_prover_reward_output_is_exact_v1(
      Output { ..control, address: from_script(d3_fraud_prover) },
      d3_fraud_prover,
      d3_reward,
    ),
    !fraud_prover_reward_output_is_exact_v1(
      Output {
        ..control,
        value: assets.from_lovelace(d3_reward)
          |> assets.add(d3_foreign_policy, "smuggled", 1),
      },
      d3_fraud_prover,
      d3_reward,
    ),
    !fraud_prover_reward_output_is_exact_v1(
      Output { ..control, reference_script: Some(d3_foreign_policy) },
      d3_fraud_prover,
      d3_reward,
    ),
    !fraud_prover_reward_output_is_exact_v1(
      Output { ..control, datum: InlineDatum(0) },
      d3_fraud_prover,
      d3_reward,
    ),
  }
}

/// D3 routing: the redeemer names the reward output, and omitting it is
/// accepted only while the expected reward is zero. Under either release
/// profile a missing reward output is a refusal, because paying nothing is not
/// paying `fraud_prover_reward`.
test d3_reward_routing_control_and_mutations() {
  let outputs = [d3_unrelated_output(), d3_reward_output(d3_reward)]
  and {
    fraud_prover_reward_routing_is_exact_v1(
      outputs,
      Some(1),
      d3_fraud_prover,
      d3_reward,
    ),
    !fraud_prover_reward_routing_is_exact_v1(
      outputs,
      Some(0),
      d3_fraud_prover,
      d3_reward,
    ),
    fraud_prover_reward_routing_is_exact_v1(outputs, None, d3_fraud_prover, 0),
    !fraud_prover_reward_routing_is_exact_v1(
      outputs,
      None,
      d3_fraud_prover,
      d3_reward,
    ),
    !fraud_prover_reward_routing_is_exact_v1(
      [d3_reward_output(d3_reward), d3_reward_output(d3_reward + 1)],
      Some(0),
      d3_fraud_prover,
      d3_reward,
    ),
  }
}

/// D3 routing: a redeemer that names an output which does not exist cannot
/// launder the obligation away.
test d3_reward_routing_rejects_out_of_range_index() fail {
  fraud_prover_reward_routing_is_exact_v1(
    [d3_reward_output(d3_reward)],
    Some(7),
    d3_fraud_prover,
    d3_reward,
  )
}

/// D4 exclusivity: the arms that consume no bond may pay no reward. The second
/// row is the double-claim refusal — a repeat `RemoveFraudulentBlockHeader`
/// against an already-forfeited bond (the only shape a duplicate fraud-proof
/// token for the same header can take) cannot carry the payout.
///
/// The third row states the guard's exact reach rather than implying more: it
/// pins reward-*shaped* payments, so an amount that is not the reward is not
/// caught. Nothing is lost by that — such a payment can only come from the
/// submitter's own inputs, since this arm consumes no operator-directory node.
test d4_reward_exclusivity_control_and_double_claim() {
  and {
    no_output_pays_fraud_prover_reward_v1(
      [d3_unrelated_output()],
      d3_fraud_prover,
      d3_reward,
    ),
    !no_output_pays_fraud_prover_reward_v1(
      [d3_unrelated_output(), d3_reward_output(d3_reward)],
      d3_fraud_prover,
      d3_reward,
    ),
    no_output_pays_fraud_prover_reward_v1(
      [d3_reward_output(d3_reward + 1)],
      d3_fraud_prover,
      d3_reward,
    ),
  }
}

/// D3 permissionless reward routing: after the prover identity is frozen by
/// thread Init, reward validity depends only on the exact immutable enterprise
/// address and amount. A missing reward output remains valid only for an exact
/// zero-reward profile.
test d3_reward_routing_does_not_require_the_prover_signature() {
  let outputs = [d3_unrelated_output(), d3_reward_output(d3_reward)]
  and {
    route_fraud_prover_reward_v1(outputs, Some(1), d3_fraud_prover, d3_reward),
    route_fraud_prover_reward_v1(outputs, None, d3_fraud_prover, 0),
    !route_fraud_prover_reward_v1(outputs, None, d3_fraud_prover, d3_reward),
  }
}
