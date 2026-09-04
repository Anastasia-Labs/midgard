//// ## Double-Withdraw Fraud Proof — step-01 (work item W-C3)
////
//// Authenticates the challenged block header out of the state-queue node the
//// computation thread names, and binds the **first** payable withdrawal leaf:
//// a committed `(WithdrawalId, WithdrawalInfo)` whose `validity` is
//// `WithdrawalIsValid`. The forwarded state carries the header hash, the
//// leaf's identity and its drained `body.l2_outref`, all fixed-size.
////
//// The membership machinery is the transition-trace family's, not a second
//// one: `midgard/fraud_proofs/double_withdraw/step_01`'s
//// `verify_committed_withdrawal_membership_v1` is a direct call into
//// `transition_trace.verify_root_membership_with_bytes` under
//// `WithdrawalsRootDomain` — the same walk the fabricated-withdrawal family
//// and the settlement spend path key.
////
//// The step chain is 01 → 02 (terminal), with `ct.Cancel` available at both
//// steps.

use aiken/crypto.{ScriptHash}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/double_withdraw/step_01.{
  Args, Datum, SpendRedeemer, verify_challenged_header_v1,
  verify_committed_withdrawal_membership_v1,
}
use midgard/fraud_proofs/double_withdraw/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/ledger_state
use midgard/transition_trace

validator main(
  step_02_validator_script_hash: ScriptHash,
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
        committed_withdrawal,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
            _own_script_hash,
            computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )

        expect None = m_input_state_data

        // 2. The header must be the authentic state-queue block the thread
        //    token names.
        let header, header_hash <- verify_challenged_header_v1(
          computation_thread_token_asset_name,
          hub_oracle,
          hub_ref_input_index,
          state_queue_node_ref_input_index,
          reference_inputs,
        )

        // 3. The challenged header must genuinely commit the withdrawal leaf
        //    under dispute. Without this the family would adjudicate a leaf no
        //    block ever claimed.
        expect
          verify_committed_withdrawal_membership_v1(
            header,
            committed_withdrawal,
          )

        // 4. The bound leaf must be **payable**: only a `WithdrawalIsValid`
        //    verdict pays out at settlement, and the family's fault is a
        //    double *payout*. An honest block's duplicate leaf is tagged
        //    non-payable, so it cannot enter the thread as its first subject.
        expect
          committed_withdrawal.value.validity == ledger_state.WithdrawalIsValid

        // 5. The thread advances to step-02 carrying the header identity and
        //    the first leaf's fixed-size facts.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          challenged_header_hash: header_hash,
          first_withdrawal_id: committed_withdrawal.key,
          first_l2_outref: committed_withdrawal.value.body.l2_outref,
        }
        expect output_state_data == expected_output_state

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
// Every selector drives the real `main.spend` handler above on a genuine block
// fixture whose header commits a real **two-leaf** withdrawals MPF under the
// counted `withdrawals_root`, with genuine `Leaf` neighbor proof steps.

fn first_step_datum() -> Datum {
  ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: None }
}

fn run_step_01(
  block: step_01.DoubleWithdrawBlockV1,
  membership: step_01.CommittedWithdrawalSourceProofV1,
  thread_asset_name: AssetName,
) -> Bool {
  let expected_output_state: Data = step_02.State {
    challenged_header_hash: block.block.header_hash,
    first_withdrawal_id: membership.key,
    first_l2_outref: membership.value.body.l2_outref,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(first_step_datum()),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        hub_ref_input_index: 0,
        state_queue_node_ref_input_index: 1,
        committed_withdrawal: membership,
      },
    ),
    fixture.own_out_ref(),
    step_01.continue_step_l1_tx_v1(
      thread_asset_name,
      expected_output_state,
      step_01.block_reference_inputs_v1(block),
    ),
  )
}

/// Positive selector: the challenged header's counted `withdrawals_root`
/// really commits the first payable leaf, so step-01 forwards its identity and
/// drained L2 outref. Both leaves of the fraudulent block can serve as the
/// thread's first subject.
test double_withdraw_step_01_binds_a_payable_leaf() {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  and {
    run_step_01(
      block,
      step_01.first_withdrawal_membership_v1(block),
      step_01.block_thread_asset_name_v1(block),
    ),
    run_step_01(
      block,
      step_01.second_withdrawal_membership_v1(block),
      step_01.block_thread_asset_name_v1(block),
    ),
  }
}

/// Honest-block negative selector: the honest duplicate leaf is tagged
/// non-payable (`SpentWithdrawalUtxo`), so it cannot enter the family as a
/// first subject — the entry that keeps the bare same-outref predicate from
/// convicting honest operators.
test double_withdraw_step_01_rejects_a_non_payable_leaf() fail {
  let block = step_01.honest_duplicate_block_v1()
  run_step_01(
    block,
    step_01.second_withdrawal_membership_v1(block),
    step_01.block_thread_asset_name_v1(block),
  )
}

/// A leaf the challenged header does not commit cannot enter the family: a
/// membership witness carried with another block's counted root fails the
/// counted-root equality against `header.withdrawals_root`.
test double_withdraw_step_01_rejects_a_forged_root() fail {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  let forged = step_01.double_withdraw_block_v1(
    step_01.first_withdrawal_id_v1(),
    step_01.payable_withdrawal_info_v1(step_01.other_l2_outref_v1()),
    step_01.second_withdrawal_id_v1(),
    step_01.payable_withdrawal_info_v1(step_01.other_l2_outref_v1()),
  )
  run_step_01(
    block,
    step_01.first_withdrawal_membership_v1(forged),
    step_01.block_thread_asset_name_v1(block),
  )
}

/// The witness's restated `count` must be the header's `withdrawal_count`: the
/// count is hashed into the counted `withdrawals_root`, so a witness cannot
/// keep the raw MPF root and quietly change the cardinality.
test double_withdraw_step_01_rejects_a_wrong_count() fail {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  let real = step_01.first_withdrawal_membership_v1(block)
  run_step_01(
    block,
    transition_trace.RootMembershipProof { ..real, count: real.count + 1 },
    step_01.block_thread_asset_name_v1(block),
  )
}

/// The state-queue node must be the block the thread token names: a thread
/// whose asset-name suffix is another header's hash cannot open this block.
test double_withdraw_step_01_rejects_a_wrong_header_identity() fail {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  let other = step_01.honest_distinct_outref_block_v1()
  run_step_01(
    block,
    step_01.first_withdrawal_membership_v1(block),
    step_01.block_thread_asset_name_v1(other),
  )
}

// ## Cancellation

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let block = step_01.fraudulent_double_withdraw_block_v1()
  let asset_name = step_01.block_thread_asset_name_v1(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(first_step_datum()),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

/// The prover may abandon the thread: the NFT burns under the prover's
/// signature.
test double_withdraw_step_01_cancels_under_the_prover_signature() {
  run_cancel(True)
}

/// Nobody else may.
test double_withdraw_step_01_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
