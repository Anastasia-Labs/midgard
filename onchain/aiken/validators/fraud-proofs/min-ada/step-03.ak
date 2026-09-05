//// Q27 min-Ada UTxO predicate step. Post-root membership is already
//// authenticated by step 02; this transaction applies only the exact compiled
//// predicate and writes the compact predecessor-proof state.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use env
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/min_ada/scan as min_ada_scan
use midgard/fraud_proofs/min_ada/step_03.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/min_ada/step_04 as step_04
use midgard/fraud_proofs/min_ada/step_05 as step_05
use midgard/fraud_proofs/transaction_output_non_canonical/rule as scan_rule
use midgard/ledger_output_commitment_v1
use midgard/validation_machine/shared as vm_shared

validator main(
  step_04_validator_script_hash: ScriptHash,
  step_05_validator_script_hash: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, window }) -> {
        let Transaction { inputs, outputs, .. } = tx
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
        )
        expect Some(state) = m_input_state_data
        when state is {
          step_03.MinAdaTxScan { direction, scan } -> {
            let next = min_ada_scan.advance(scan, window)
            expect next.outcome != scan_rule.outcome_non_canonical
            expect output_script_hash == own_script_hash
            let expected: Data = if next.outcome == scan_rule.outcome_canonical {
              step_03.MinAdaTxDescriptor {
                direction,
                total_length: next.item_length,
                lovelace: next.control.lovelace,
              }
            } else {
              step_03.MinAdaTxScan { direction, scan: next }
            }
            output_state_data == expected
          }
          step_03.MinAdaTxDescriptor { direction, total_length, lovelace } -> {
            expect output_script_hash == step_05_validator_script_hash
            let expected_output_state: Data = step_05.PredicateAndCulpabilityAuthenticated
            expect output_state_data == expected_output_state
            let meets = vm_shared.output_meets_min_ada_v1(
              env.coins_per_utxo_byte,
              total_length,
              lovelace,
            )
            if direction == 1 {
              meets
            } else {
              expect direction == 0
              !meets
            }
          }
          step_03.MinAdaUtxoDescriptor {
            descriptor_cbor,
            out_ref_key,
            prev_utxos_root,
          } -> {
            expect output_script_hash == step_04_validator_script_hash
            let expected_output_state: Data = step_04.State {
              out_ref_key,
              prev_utxos_root,
            }
            expect output_state_data == expected_output_state
            let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
            !vm_shared.output_meets_min_ada_v1(
              env.coins_per_utxo_byte,
              descriptor.total_length,
              descriptor.lovelace,
            )
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
