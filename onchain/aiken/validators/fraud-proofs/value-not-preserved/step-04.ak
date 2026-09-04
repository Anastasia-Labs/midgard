//// ## Value-Not-Preserved Fraud Proof — step-04
////
//// Finalizes the thread into a permanent fraud-proof token, and is therefore
//// the last place the family can refuse. It re-checks the two things a
//// permanent conviction must not get wrong: that the fault is filed under
//// this family's catalogue category, and that the completed fold really is a
//// fault — non-zero, in the claimed direction. A balanced fold refuses here
//// no matter how it was assembled: the transaction that never touches the
//// claimed asset (0 == 0), the wrong-asset claim against a transaction
//// unbalanced in a different unit, and the minted-and-fully-paid-out token
//// all end at `final_delta == 0` and mint nothing.

use aiken/builtin
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/value_not_preserved/step_01.{
  ClaimedAssetDeflated, ClaimedAssetInflated, ClaimedAssetV1,
  ClaimedImbalanceDirectionV1, fraud_category_is_value_not_preserved_v1,
}
use midgard/fraud_proofs/value_not_preserved/step_04.{
  Args, Datum, SpendRedeemer, State, value_not_preserved_fault_is_established_v1,
}
use midgard/fraud_proofs/value_not_preserved/thread_fixture_v1 as tf

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,
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
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

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

        // 2. The fraud-proof token inherits the thread token's asset name, so
        //    that name is the permanent record of what was proven and against
        //    which block: the category prefix must be this family's.
        expect
          fraud_category_is_value_not_preserved_v1(
            computation_thread_token_asset_name,
          )

        // 3. The decisive inequality: the completed fold is unbalanced in the
        //    claimed direction. This is the machine's
        //    `reject_value_not_preserved` for the claimed unit — a balanced
        //    fold, whatever the route here, mints nothing.
        expect value_not_preserved_fault_is_established_v1(state)

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Family selectors

fn completed_state(
  block: tf.ValueBlockV1,
  claimed_asset: ClaimedAssetV1,
  claimed_direction: ClaimedImbalanceDirectionV1,
  final_delta: Int,
) -> State {
  State {
    bad_tx_id: block.block.tx_id,
    claimed_asset,
    claimed_direction,
    final_delta,
  }
}

fn run_step_04(state: State, thread_asset_name: ByteArray) -> Bool {
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
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
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(thread_asset_name),
  )
}

fn run_own_thread(block: tf.ValueBlockV1, state: State) -> Bool {
  run_step_04(state, tf.thread_asset_name_v1(block))
}

/// Positive selector (token claim): the inflated token fold (`−10 < 0`,
/// claimed inflated) finalizes into a permanent fraud-proof token under this
/// family's category.
test value_not_preserved_step_04_convicts_an_inflated_token() {
  let block = tf.value_block_v1(
    tf.token_inflated_tx_v1(),
    tf.spent_output_with_tokens_v1(),
  )
  run_own_thread(
    block,
    completed_state(block, tf.claimed_token_v1(), ClaimedAssetInflated, -10),
  )
}

/// Positive selector (deflated direction): value destroyed (`+10 > 0`,
/// claimed deflated) is the other convictable arm.
test value_not_preserved_step_04_convicts_a_deflated_token() {
  let block = tf.value_block_v1(
    tf.token_deflated_tx_v1(),
    tf.spent_output_with_tokens_v1(),
  )
  run_own_thread(
    block,
    completed_state(block, tf.claimed_token_v1(), ClaimedAssetDeflated, 10),
  )
}

/// Positive selector (ADA claim): the inflated lovelace fold (`−500_000 <
/// 0`) convicts — fund theft in the distinguished unit.
test value_not_preserved_step_04_convicts_inflated_ada() {
  let block = tf.value_block_v1(
    tf.ada_inflated_tx_v1(),
    tf.spent_output_ada_only_v1(),
  )
  run_own_thread(
    block,
    completed_state(block, step_01.AdaAsset, ClaimedAssetInflated, -500_000),
  )
}

/// Valid-block negative selector: a balanced fold (`0`) satisfies neither
/// direction and mints nothing — the transaction that conserves the claimed
/// asset, the wrong-asset claim against a transaction unbalanced in a
/// *different* unit, and the minted-and-fully-paid-out token all land here.
test value_not_preserved_step_04_rejects_a_balanced_fold() fail {
  let block = tf.value_block_v1(
    tf.token_balanced_tx_v1(),
    tf.spent_output_with_tokens_v1(),
  )
  run_own_thread(
    block,
    completed_state(block, tf.claimed_token_v1(), ClaimedAssetInflated, 0),
  )
}

/// A direction mismatch refuses: the fold ended `+10` (deflated) but the
/// claim named inflation, and a permanent token must record the fault that
/// was actually proven.
test value_not_preserved_step_04_rejects_a_direction_mismatch() fail {
  let block = tf.value_block_v1(
    tf.token_deflated_tx_v1(),
    tf.spent_output_with_tokens_v1(),
  )
  run_own_thread(
    block,
    completed_state(block, tf.claimed_token_v1(), ClaimedAssetInflated, 10),
  )
}

/// A thread minted under another family's catalogue category cannot finalize
/// here: its fraud-proof token would be filed under that other category's
/// rule. The substituted prefix is the transition-trace family's
/// `#"00000004"`.
test value_not_preserved_step_04_rejects_a_wrong_category() fail {
  let block = tf.value_block_v1(
    tf.token_inflated_tx_v1(),
    tf.spent_output_with_tokens_v1(),
  )
  run_step_04(
    completed_state(block, tf.claimed_token_v1(), ClaimedAssetInflated, -10),
    #"00000004" |> foreign_thread_asset_name(block),
  )
}

fn foreign_thread_asset_name(
  category_id: ByteArray,
  block: tf.ValueBlockV1,
) -> ByteArray {
  builtin.append_bytearray(category_id, block.block.header_hash)
}
