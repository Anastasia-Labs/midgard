//// ## L2 transaction mis-tag fraud proof — step 02
////
//// Finalizes the computation thread and re-asserts the committed non-zero
//// scalar on the permanent fraud-proof-token mint path.

use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/l2_tx_mistag/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

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

        expect Some(State { bad_tx_id: _, committed_validity_code }) = m_input_state_data

        // Decisive check: finalize and evidence mint are unreachable for an
        // honest code-0 normal leaf.
        expect committed_validity_code != 0

        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn run_finalize(committed_validity_code: Int) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(State { bad_tx_id: block.tx_id, committed_validity_code }),
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

/// A state carrying the authenticated code 1 finalizes and mints evidence.
test l2_tx_mistag_step_02_finalizes_a_nonzero_validity_code() {
  run_finalize(1)
}

/// Decisive-check refusal: code 0 cannot reach the evidence-token mint path.
test l2_tx_mistag_step_02_rejects_a_code_zero_state() fail {
  run_finalize(0)
}
