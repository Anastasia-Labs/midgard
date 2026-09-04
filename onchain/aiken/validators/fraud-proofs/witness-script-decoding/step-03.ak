use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_script_decoding/engine as scan_engine
use midgard/fraud_proofs/witness_script_decoding/rule
use midgard/fraud_proofs/witness_script_decoding/step_03.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_script_scan_v1

validator main(
  step_04_validator_script_hash: ScriptHash,
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
        control_cbor,
        chunk_proof,
        next_chunk_proof,
        frames,
        step_budget,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let
            own_script_hash,
            _thread_asset_name,
            _fraud_prover,
            input_state,
            next_script_hash,
            next_state
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        expect rule.state_is_authentic_v1(state)
        expect state.next_expected_script_hash == own_script_hash
        let expected = if state.result_class != rule.result_pending {
          expect and {
              control_cbor == #"",
              chunk_proof == None,
              next_chunk_proof == None,
              frames == [],
              step_budget == 0,
            }
          expect next_script_hash == step_04_validator_script_hash
          rule.closed_state_v1(
            state,
            state.result_class,
            step_04_validator_script_hash,
          )
        } else {
          expect step_budget > 0
          expect control_cbor == state.control_cbor
          let control = native_script_scan_v1.decode_structure_control_v1(
            control_cbor,
          )
          let window = if control.stage == native_script_scan_v1.structure_stage_token {
            expect Some(proof) = chunk_proof
            Some(
              rule.authenticated_window_v1(
                state,
                control,
                proof,
                next_chunk_proof,
              ),
            )
          } else {
            expect and {
                chunk_proof == None,
                next_chunk_proof == None,
              }
            None
          }
          when
            scan_engine.budgeted_scan_v1(control, window, frames, step_budget)
          is {
            scan_engine.ScanRefusedV1 { refusal_class } -> {
              expect next_script_hash == step_04_validator_script_hash
              rule.closed_state_v1(
                state,
                rule.mapped_refusal_class_v1(refusal_class),
                step_04_validator_script_hash,
              )
            }
            scan_engine.ScanAdvancedV1 { control: next_control } ->
              if native_script_scan_v1.structure_terminal_is_exact_v1(
                next_control,
              ) {
                expect next_script_hash == step_04_validator_script_hash
                rule.closed_state_v1(
                  state,
                  rule.result_no_fault,
                  step_04_validator_script_hash,
                )
              } else {
                expect next_script_hash == own_script_hash
                let advanced = rule.advanced_state_v1(
                  state,
                  next_control,
                  own_script_hash,
                )
                expect advanced.checkpoint_hash != state.checkpoint_hash
                advanced
              }
          }
        }
        let expected_data: Data = expected
        next_state == expected_data
      }
    }
  }

  else(_) {
    fail
  }
}
