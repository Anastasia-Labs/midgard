use aiken/builtin
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken_design_patterns/linked_list
use cardano/address.{
  Address, from_script, from_verification_key, with_delegation_key,
}
use cardano/assets.{Lovelace, PolicyId, quantity_of}
use cardano/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, Redeemer, ScriptPurpose,
  Transaction,
}
use env
use midgard/common/utils
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
  AttachDaAttestation, CommitBlockHeader, Deinit, HeaderView, InitV1,
  LinkedListMutation, MergeToConfirmedStateV1, MintRedeemer,
  OperatorAlreadySlashed, RemoveFraudulentBlockHeader,
  RemoveFraudulentBlocksLink, RemoveLastFraudulentBlock, SlashActiveOperator,
  SlashRetiredOperator, SpendRedeemer, StateQueueNode,
  commit_bound_header_time_is_valid, confirmed_state_asset_name,
  decode_header_view, finalize_linked_list, no_da_attestation,
}

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
  let Output { address, value, reference_script, .. } = reward_output
  and {
    address == from_verification_key(fraud_prover),
    assets.lovelace_of(value) == expected_reward,
    assets.without_lovelace(value) == assets.zero,
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
/// `None` is the placeholder-economics case and nothing else: it is accepted
/// only while `expected_reward` is zero, which is exactly the state
/// `onchain/aiken/env/{default,testnet}.ak` compile today and which F04 §2.5
/// records as Q53's to replace. Under any real F04 profile the index is
/// mandatory, because paying nothing is not paying `fraud_prover_reward`.
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
      fraud_prover_reward_output_is_exact_v1(
        reward_output,
        fraud_prover,
        expected_reward,
      )
    }
  }
}

/// The complete reward obligation of a bond-consuming
/// `RemoveFraudulentBlockHeader` arm: exact routing to the fraud prover named
/// by the fraud-proof datum, and — when the transaction actually bears a reward
/// output — that prover's own signature on the transaction that pays it.
///
/// The signature is F04 §2.3's second binding — "the same prover must be an
/// extra signatory of the reward-bearing slash transaction … on-chain
/// enforcement, not an SDK preflight, establishes both bindings" — and it is
/// what stops a third party from forcing a payout on a prover's behalf while
/// spending the prover's one-shot claim.
///
/// ## Why the signature is gated on the reward-bearing case
///
/// F04 §2.3 scopes that binding to "the reward-bearing slash transaction", and
/// the 2026-08-12 orchestrator ruling on #603 pins the scope: the signature is
/// demanded exactly when the reward output exists — the `Some` arm. A
/// zero-profile slash routes its reward by omitting the output (`None`, which
/// `fraud_prover_reward_routing_is_exact_v1` accepts only while
/// `expected_reward` is zero), pays the prover nothing, and so protects no
/// asset by demanding the prover's cooperation; an unconditional demand would
/// only add a liveness dependency, letting an absent prover block the slash of
/// a fraudulent operator. Under any nonzero profile the two conditions
/// coincide: `None` fails the routing conjunct outright, so every transaction
/// that clears this predicate is a `Some` transaction and is signed.
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
  extra_signatories: List<VerificationKeyHash>,
) -> Bool {
  and {
    fraud_prover_reward_routing_is_exact_v1(
      outputs,
      m_reward_output_index,
      fraud_prover,
      expected_reward,
    ),
    when m_reward_output_index is {
      // No reward output: nothing is paid to the prover, so nothing of the
      // prover's is spent and no signature is owed.
      None -> True
      Some(_) -> fraud_prover |> utils.has_signed(extra_signatories)
    },
  }
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
    expect da_attestation == da_attestation_policy_id

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
    }
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  active_operators_script_hash: PolicyId,
  active_operators_addr: Address,
  retired_operators_script_hash: PolicyId,
  scheduler_script_hash: PolicyId,
  fraud_proof_script_hash: PolicyId,
  settlement_script_hash: PolicyId,
  da_attestation_policy_id: PolicyId,
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      InitV1 { output_index } -> {
        let Transaction { outputs, mint, validity_range, .. } = self

        expect Some(root_output) = outputs |> list.at(output_index)

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
        let Transaction { inputs, mint, .. } = self

        // 1. Hub oracle NFT must be getting burnt.
        expect quantity_of(mint, hub_oracle_script_hash, hub.asset_name) == -1

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
      CommitBlockHeader {
        new_block_output_index,
        continued_latest_block_output_index,
        operator,
        scheduler_ref_input_index,
        active_operators_input_index,
        active_operators_redeemer_index,
      } -> {
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
          expect active_operators.UpdateBondHoldNewState { active_operator, .. }
            = utils.get_spending_redeemer_data_at(
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
              // 10a. New block must properly carry over data from previous
              //      block.
              expect
                  StateQueueNode { header: anchor_header_data, .. }: StateQueueNode
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
      RemoveFraudulentBlockHeader {
        fraudulent_operator,
        fraudulent_blocks_header_hash,
        slashing_approach,
        fraud_proof_ref_input_index,
        block_removal_approach,
      } -> {
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
        expect
          referenced_fraudulent_header_hash == fraudulent_blocks_header_hash

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
                  expect
                      active_operators.SlashOperator { slashing_arguments, .. }
                    = redeemer_data
                  slashing_arguments
                },
                redeemers,
              )
              expect SlashOperatorForBadState { .. } = slashing_reason_from_active_operators

              // 2a-i. This arm consumes the operator's bond, so it is the
              //       reward-bearing transaction: exactly
              //       `env.fraud_prover_reward` must reach the prover named by
              //       the fraud-proof datum, and — whenever that payment is
              //       actually made — that prover must have signed.
              route_fraud_prover_reward_v1(
                outputs,
                m_fraud_prover_reward_output_index,
                fraud_prover,
                env.fraud_prover_reward,
                extra_signatories,
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
                  expect retired_operators.SlashOperator { slashing_arguments }
                    = redeemer_data
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
                extra_signatories,
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
            } -> {
              // 4a. If the fraudulent block has a link, its successor node must
              //     be removed first.
              //
              //     > [!NOTE]
              //     > The successor node is also considered fraudulent because
              //     > of getting appended to a fraudulent block, so it does not
              //     > need to have an associated fraud proof token.
              expect Some(fraudulent_node_output) = outputs
                |> list.at(fraudulent_node_output_index)
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
              expect
                Some(fraudulent_blocks_header_hash) == m_fraudulent_header_hash
              expect
                  StateQueueNode { header: removed_node_header, .. }: StateQueueNode
                = removed_node_header_data
              let removed_header_view = decode_header_view(removed_node_header)
              removed_header_view.operator_vkey == fraudulent_operator
            }
            RemoveLastFraudulentBlock {
              anchor_element_input_outref,
              anchor_element_output_index,
            } -> {
              // 4b. If the fraudulent block has no links, it can be removed
              //     from the very end of the state queue.
              expect Some(anchor_element_output) = outputs
                |> list.at(anchor_element_output_index)
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
              expect
                  StateQueueNode { header: removed_node_header, .. }: StateQueueNode
                = removed_node_header_data
              let removed_header_view = decode_header_view(removed_node_header)
              expect removed_header_view.operator_vkey == fraudulent_operator
              removed_node_link == None
            }
          }
            |> finalize_linked_list(own_policy_id)

        // Done. (The fraud-proof reference input that used to be verified here
        // as step 5 is verified at step 0 above, where the prover it carries is
        // needed by the reward routing.)
        True
      }
      MergeToConfirmedStateV1 {
        header_node_key,
        confirmed_state_input_outref,
        confirmed_state_output_index,
        m_settlement_redeemer_index,
        merged_block_withdrawals_root,
        merged_block_forced_transactions_root,
        merged_block_transactions_root,
        merged_block_deposits_root,
        merged_block_transition_trace_root,
        merged_block_event_to_step_root,
        merged_block_validation_traces_root,
        merged_block_withdrawal_count,
        merged_block_forced_transaction_count,
        merged_block_l2_transaction_count,
        merged_block_deposit_count,
        merged_block_total_event_count,
        merged_block_transition_step_count,
        merged_block_validation_trace_count,
      } ->
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
  }

  // TODO: Add redeemers for escape hatch blocks

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
// `env.fraud_prover_reward` unchanged, and no value in
// `onchain/aiken/env/{default,testnet}.ak` moves in this lane. Exercising the
// seams at a real amount is what makes "one lovelace over" and "one lovelace
// under" observable while the compiled placeholder is still zero.

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
  }
}

/// D3 routing: the redeemer names the reward output, and omitting it is
/// accepted only while the compiled reward is zero — the placeholder economics
/// F04 §2.5 assigns to Q53. Under any real profile a missing reward output is
/// a refusal, because paying nothing is not paying `fraud_prover_reward`.
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

/// D3 signature binding (F04 §2.3's second binding, scoped by the 2026-08-12
/// orchestrator ruling on #603): a slash transaction that actually pays the
/// reward must carry the prover's own signature, and one that does not pay it
/// must not demand one.
///
/// Rows 1–2 are the reward-bearing pair: the same exact payment passes with the
/// prover among the extra signatories and refuses without. Row 3 pins the
/// binding to the prover specifically rather than to "somebody signed" — a
/// third party's signature on a payout to the prover is still a refusal, which
/// is the shape that would let a stranger spend the prover's one-shot claim.
///
/// Row 4 is the zero-profile positive the ruling turns on: with the reward at
/// zero the routing is `None`, nothing reaches the prover, and the transaction
/// clears with no prover signature at all — so an absent prover cannot hold up
/// the slash of a fraudulent operator. Row 5 states the other half of that
/// gating, that the zero profile is the only way to reach it: at a real reward
/// `None` still refuses on the routing conjunct.
test d3_reward_signature_binding_is_gated_on_the_reward_bearing_case() {
  let outputs = [d3_unrelated_output(), d3_reward_output(d3_reward)]
  and {
    route_fraud_prover_reward_v1(
      outputs,
      Some(1),
      d3_fraud_prover,
      d3_reward,
      [d3_fraud_prover],
    ),
    !route_fraud_prover_reward_v1(
      outputs,
      Some(1),
      d3_fraud_prover,
      d3_reward,
      [],
    ),
    !route_fraud_prover_reward_v1(
      outputs,
      Some(1),
      d3_fraud_prover,
      d3_reward,
      [d3_other_key],
    ),
    route_fraud_prover_reward_v1(outputs, None, d3_fraud_prover, 0, []),
    !route_fraud_prover_reward_v1(
      outputs,
      None,
      d3_fraud_prover,
      d3_reward,
      [d3_fraud_prover],
    ),
  }
}
