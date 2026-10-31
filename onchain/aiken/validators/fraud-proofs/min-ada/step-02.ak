//// Q27 min-Ada step 02: open the claimed transaction output; the rewarding
//// yield script walks the outputs field.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/min_ada/step_02.{Args, Datum, SpendRedeemer, State}
use midgard/fraud_proofs/min_ada/step_03 as step_03
use midgard/fraud_proofs/min_ada/yield
use midgard/state_queue_yield.{require_authenticated_zero_yield}

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  reference_script_auth_policy_id: PolicyId,
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
        yield_to_ref_input_index,
        ..
      }) -> {
        let
            Transaction {
              inputs,
              outputs,
              reference_inputs,
              withdrawals,
              redeemers,
              ..
            }
          = tx
        expect Some(step_datum) = datum
        let
            own_script_hash,
            _computation_thread_token_asset_name,
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
          tx.extra_signatories,
        )
        expect Some(_) = m_input_state_data
        expect _shape = if output_script_hash == own_script_hash {
          expect _: State = output_state_data
          True
        } else {
          expect output_script_hash == step_03_validator_script_hash
          expect step_03.MinAdaTxScan { .. }: step_03.State = output_state_data
          True
        }
        expect _yield_hash = require_authenticated_zero_yield(
          reference_inputs,
          withdrawals,
          redeemers,
          reference_script_auth_policy_id,
          yield.tx_role,
          yield_to_ref_input_index,
        )
        True
      }
    }
  }

  else(_) {
    fail
  }
}
