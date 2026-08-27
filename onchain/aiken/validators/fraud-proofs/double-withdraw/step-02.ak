//// ## Double-Withdraw Fraud Proof — step-02 (work item W-C3)
////
//// Finalizes the thread into a permanent fraud-proof token, and is therefore
//// the last place the family can refuse. It re-authenticates the challenged
//// header out of the state-queue node the thread token names (so the carried
//// state cannot be filed against a different block), binds the **second**
//// committed withdrawal leaf through the same counted-root walk step-01 used,
//// and requires the decisive predicate: distinct identities, one drained L2
//// UTxO, both leaves payable.

use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/double_withdraw/step_01
use midgard/fraud_proofs/double_withdraw/step_02.{
  Args, Datum, SpendRedeemer, State, double_withdraw_fault_is_established_v1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/transition_trace

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        hub_ref_input_index,
        state_queue_node_ref_input_index,
        fraud_proof_mint_redeemer_index,
        committed_withdrawal,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
            _own_script_hash,
            computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data
          <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
        )

        expect Some(state) = m_input_state_data
        let
            State {
              challenged_header_hash,
              first_withdrawal_id: _first_withdrawal_id,
              first_l2_outref: _first_l2_outref,
            }
          = state

        // 2. The fraud-proof token inherits the thread token's asset name, so
        //    that name is the permanent record of what was proven and against
        //    which block. The header is re-authenticated out of the
        //    state-queue node whose key must equal the asset name's 28-byte
        //    suffix, and the carried state must name the same header — a
        //    thread whose state names a different block would publish its
        //    conviction against the wrong header.
        let header, header_hash <- step_01.verify_challenged_header_v1(
          computation_thread_token_asset_name,
          hub_oracle,
          hub_ref_input_index,
          state_queue_node_ref_input_index,
          reference_inputs,
        )
        expect challenged_header_hash == header_hash

        // 3. The challenged header must genuinely commit the second
        //    withdrawal leaf, through the same counted-root walk that bound
        //    the first.
        expect
          step_01.verify_committed_withdrawal_membership_v1(
            header,
            committed_withdrawal,
          )

        // 4. The decisive predicate: a second, distinct payable leaf draining
        //    the L2 UTxO the first payable leaf already drained. Presenting
        //    the same leaf twice fails the identity inequality; an honest
        //    duplicate fails payability; an unrelated withdrawal fails the
        //    outref equality.
        expect
          double_withdraw_fault_is_established_v1(state, committed_withdrawal)

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)
//
// Each selector drives the real `main.spend` handler on the exact thread state
// step-01's positive selector produces, so the finalized conviction is
// measured against the same two-leaf withdrawals MPF the first step
// authenticated.

fn state_from_first_leaf(block: step_01.DoubleWithdrawBlockV1) -> State {
  State {
    challenged_header_hash: block.block.header_hash,
    first_withdrawal_id: block.first_id,
    first_l2_outref: block.first_info.body.l2_outref,
  }
}

fn state_from_second_leaf(block: step_01.DoubleWithdrawBlockV1) -> State {
  State {
    challenged_header_hash: block.block.header_hash,
    first_withdrawal_id: block.second_id,
    first_l2_outref: block.second_info.body.l2_outref,
  }
}

fn run_step_02(
  state: State,
  membership: step_01.CommittedWithdrawalSourceProofV1,
  block: step_01.DoubleWithdrawBlockV1,
  thread_asset_name: AssetName,
) -> Bool {
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        hub_ref_input_index: 0,
        state_queue_node_ref_input_index: 1,
        fraud_proof_mint_redeemer_index: 0,
        committed_withdrawal: membership,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_with_reference_inputs_v1(
      thread_asset_name,
      step_01.block_reference_inputs_v1(block),
    ),
  )
}

fn run_own_thread(
  block: step_01.DoubleWithdrawBlockV1,
  membership: step_01.CommittedWithdrawalSourceProofV1,
) -> Bool {
  run_step_02(
    state_from_first_leaf(block),
    membership,
    block,
    step_01.block_thread_asset_name_v1(block),
  )
}

/// Positive selector: two distinct payable leaves over one L2 UTxO finalize
/// into a permanent fraud-proof token — the committed double payout.
test double_withdraw_step_02_accepts_the_exact_double_withdraw_fault() {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  and {
    run_own_thread(block, step_01.second_withdrawal_membership_v1(block)),
    run_step_02(
      state_from_second_leaf(block),
      step_01.first_withdrawal_membership_v1(block),
      block,
      step_01.block_thread_asset_name_v1(block),
    ),
  }
}

/// The prover cannot present the first leaf **twice**: the same committed
/// leaf re-bound at step-02 fails the identity inequality, so an honest block
/// with a single payable withdrawal is not convictable by self-pairing.
test double_withdraw_step_02_rejects_the_same_leaf_twice() fail {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  run_own_thread(block, step_01.first_withdrawal_membership_v1(block))
}

/// Honest-block negative selector: a second payable withdrawal draining a
/// **different** L2 UTxO is the ordinary two-withdrawal block, not a fault.
test double_withdraw_step_02_rejects_distinct_l2_outrefs() fail {
  let block = step_01.honest_distinct_outref_block_v1()
  run_own_thread(block, step_01.second_withdrawal_membership_v1(block))
}

/// Honest-block negative selector: the honest same-outref duplicate is tagged
/// non-payable, so it cannot serve as the second leaf either — the family
/// refuses the honest commitment at its exact decisive check.
test double_withdraw_step_02_rejects_the_honest_duplicate() fail {
  let block = step_01.honest_duplicate_block_v1()
  run_own_thread(block, step_01.second_withdrawal_membership_v1(block))
}

/// A leaf the challenged header does not commit cannot finalize: a membership
/// witness carried with another block's counted root fails the counted-root
/// equality.
test double_withdraw_step_02_rejects_a_forged_membership() fail {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  let forged = step_01.honest_distinct_outref_block_v1()
  run_own_thread(block, step_01.second_withdrawal_membership_v1(forged))
}

/// The witness's restated `count` must be the header's `withdrawal_count`.
test double_withdraw_step_02_rejects_a_wrong_count() fail {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  let real = step_01.second_withdrawal_membership_v1(block)
  run_own_thread(
    block,
    transition_trace.RootMembershipProof { ..real, count: real.count + 1 },
  )
}

/// The fault must be filed against the header the thread token names: a
/// carried state naming a different block than the asset-name suffix (and its
/// state-queue node) cannot finalize.
test double_withdraw_step_02_rejects_a_wrong_source_identity() fail {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  let other = step_01.honest_distinct_outref_block_v1()
  run_step_02(
    state_from_first_leaf(block),
    step_01.second_withdrawal_membership_v1(block),
    block,
    step_01.block_thread_asset_name_v1(other),
  )
}

// ## Cancellation

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  let asset_name = step_01.block_thread_asset_name_v1(block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_from_first_leaf(block)),
      },
    ),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

/// The prover may abandon the thread at the terminal step too.
test double_withdraw_step_02_cancels_under_the_prover_signature() {
  run_cancel(True)
}

/// Nobody else may.
test double_withdraw_step_02_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
