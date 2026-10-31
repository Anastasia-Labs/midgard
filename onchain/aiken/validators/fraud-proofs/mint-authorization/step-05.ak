//// ## Mint-Authorization Fault Proof — Step 05 (W-C14)
////
//// The terminal step. Everything substantive was proven upstream — step-01
//// bound an operator-ACCEPTED committed transaction, step-02 bound the
//// claim to the thread's header and read the policy id off the committed
//// mint item, and the direction legs (step-03 alone for direction B,
//// step-03 + step-04's completed scan for direction A) proved the policy
//// unauthorized. This step checks the closed verdict's shape and finalizes:
//// the thread NFT burns and the permanent fraud-proof token mints under the
//// same asset name (`category_id ‖ header_hash`), settling at the
//// fraud-proof address. The state-queue node NFT burns with the block's
//// removal; the fraud-proof token never does.

use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/mint_authorization/engine
use midgard/fraud_proofs/mint_authorization/step_05.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf

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

        // 1. Generic finalization validations must pass: thread NFT burned,
        //    fraud-proof token minted under the same asset name and settled
        //    at the fraud-proof address.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
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

        // 2. The verdict must be a closed one: a well-formed state whose
        //    direction is in this family's two-value domain. Either leg
        //    writes exactly such a state and nothing else reaches this
        //    address with the thread NFT.
        expect Some(State { policy_id: _, direction }) = m_input_state_data
        expect or {
            direction == engine.direction_script_absent,
            direction == engine.direction_script_unsatisfied,
          }

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

const accused_policy_id: ByteArray =
  #"42424242424242424242424242424242424242424242424242424242"

fn run_finalize(m_state: Option<State>) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: m_state,
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
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// A closed direction-A verdict finalizes into the permanent fraud-proof
/// token.
test mint_authorization_step_05_finalizes_a_direction_a_verdict() {
  run_finalize(
    Some(
      State {
        policy_id: accused_policy_id,
        direction: engine.direction_script_absent,
      },
    ),
  )
}

/// A closed direction-B verdict finalizes too.
test mint_authorization_step_05_finalizes_a_direction_b_verdict() {
  run_finalize(
    Some(
      State {
        policy_id: tf.h28_y,
        direction: engine.direction_script_unsatisfied,
      },
    ),
  )
}

/// No verdict, no token: a thread arriving without state cannot finalize.
test mint_authorization_step_05_rejects_a_missing_state() fail {
  run_finalize(None)
}

/// The verdict's direction domain is exactly {0, 1}.
test mint_authorization_step_05_rejects_an_out_of_domain_direction() fail {
  run_finalize(Some(State { policy_id: accused_policy_id, direction: 2 }))
}

// ## Cancellation

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            policy_id: accused_policy_id,
            direction: engine.direction_script_absent,
          },
        ),
      },
    ),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

/// The prover may abandon the thread even at the last step.
test mint_authorization_step_05_cancels_under_the_prover_signature() {
  run_cancel(True)
}

/// Nobody else may.
test mint_authorization_step_05_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
