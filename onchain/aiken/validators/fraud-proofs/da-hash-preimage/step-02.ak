//// ## DA Hash/Preimage Fraud Proof — step 02 (GOAL_SPEC.md Q44)
////
//// Finalizes exactly the non-normal verdict step 01 derived from the
//// authenticated raw `transactions_root` leaf.

use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/da_hash_preimage/rule
use midgard/fraud_proofs/da_hash_preimage/step_02.{
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

        expect Some(State { verdict }) = m_input_state_data
        expect rule.is_da_hash_preimage_violation_v1(verdict)
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn finalize_datum(verdict: rule.VerdictV1) -> Datum {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(State { verdict }),
  }
}

fn run_finalize(verdict: rule.VerdictV1) -> Bool {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(finalize_datum(verdict)),
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

test da_hash_preimage_step_02_convicts_malformed_source() {
  run_finalize(rule.MalformedSource)
}

test da_hash_preimage_step_02_convicts_key_mismatch() {
  run_finalize(rule.KeyMismatch)
}

test da_hash_preimage_step_02_convicts_malformed_proof_source() {
  run_finalize(rule.MalformedProofSource)
}

test da_hash_preimage_step_02_convicts_derived_id_mismatch() {
  run_finalize(rule.DerivedIdMismatch)
}

test da_hash_preimage_step_02_rejects_valid_block_challenge() fail {
  run_finalize(rule.NoViolation)
}
