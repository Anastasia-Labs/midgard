use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_script_decoding/engine as scan_engine
use midgard/fraud_proofs/output_reference_script_decoding/rule
use midgard/fraud_proofs/output_reference_script_decoding/step_05.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_script_scan_v1

validator main(step_06_hash: ScriptHash, computation_thread_policy: PolicyId) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
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
        let own_hash, _name, _prover, input_state, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        expect rule.scan_state_is_authentic_v1(state)
        expect state.next_expected_script_hash == own_hash
        let expected = if state.result_class != rule.result_pending {
          expect and {
              control_cbor == #"",
              chunk_proof == None,
              next_chunk_proof == None,
              frames == [],
              step_budget == 0,
            }
          expect next_hash == step_06_hash
          rule.closed_scan_state_v1(state, state.result_class, step_06_hash)
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
              expect next_hash == step_06_hash
              rule.closed_scan_state_v1(
                state,
                rule.mapped_refusal_class_v1(refusal_class),
                step_06_hash,
              )
            }
            scan_engine.ScanAdvancedV1 { control: next_control } ->
              if native_script_scan_v1.structure_terminal_is_exact_v1(
                next_control,
              ) {
                expect next_hash == step_06_hash
                rule.closed_scan_state_v1(
                  state,
                  rule.result_no_fault,
                  step_06_hash,
                )
              } else {
                expect next_hash == own_hash
                let advanced = rule.advanced_scan_state_v1(
                  state,
                  next_control,
                  own_hash,
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
