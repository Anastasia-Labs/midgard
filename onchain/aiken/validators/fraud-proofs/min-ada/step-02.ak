//// Q27 min-Ada step 02: transaction predicate or post-root UTxO membership.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/min_ada/step_01.{MinAdaTx, MinAdaUtxo}
use midgard/fraud_proofs/min_ada/step_02.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/min_ada/step_03 as step_03
use midgard/fraud_proofs/min_ada/yield
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/state_queue_yield.{require_authenticated_zero_yield}

validator main(
  step_03_validator_script_hash: ScriptHash,
  _step_05_validator_script_hash: ScriptHash,
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
        outputs_opening,
        post_membership,
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
            _own_script_hash,
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
        )
        expect Some(state) = m_input_state_data
        when state.fault is {
          MinAdaTx { output_index: bad_output_index } -> {
            expect Some(opening) = outputs_opening
            expect None = post_membership
            expect None = state.post_utxo
            expect output_script_hash == step_03_validator_script_hash
            expect step_03.MinAdaTxDescriptor { .. }: step_03.State = output_state_data
            expect _ = bad_output_index
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
          MinAdaUtxo -> {
            expect None = outputs_opening
            expect Some(membership) = post_membership
            expect Some(post) = state.post_utxo
            expect output_script_hash == step_03_validator_script_hash
            let expected_output_state: Data = step_03.MinAdaUtxoDescriptor {
              descriptor_cbor: post.descriptor_cbor,
              out_ref_key: ledger_outref_key(post.out_ref),
              prev_utxos_root: post.prev_utxos_root,
            }
            expect output_state_data == expected_output_state
            expect _ = membership
            expect _yield_hash = require_authenticated_zero_yield(
              reference_inputs,
              withdrawals,
              redeemers,
              reference_script_auth_policy_id,
              yield.utxo_role,
              yield_to_ref_input_index,
            )
            True
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
