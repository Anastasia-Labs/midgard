use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1 as opening
use midgard/fraud_proofs/native_script_invalid/rule
use midgard/fraud_proofs/native_script_invalid/step_02.{
  Args, CertifyScriptField, Datum, SelectCertifiedScript, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_script_invalid/step_03
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardVersionedScript, NativeCardanoScript,
}
use midgard/native_tx_field_access_v1.{field_item_at}
use midgard/native_tx_machine_walk_v1 as walk

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
      ct.Continue(action) -> {
        expect Some(step_datum) = datum
        let Pair(input_index, output_index) = when action is {
          Args { input_index, output_index, .. } |
          CertifyScriptField { input_index, output_index, .. } |
          SelectCertifiedScript { input_index, output_index, .. } ->
            Pair(input_index, output_index)
        }
        let
            own_script_hash,
            _,
            _,
            m_state,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = m_state
        let anchor = opening.WitnessAnchor {
          tx_id: state.bad_tx_id,
          witness_set_hash: state.bad_tx_witness_set_hash,
          source_kind: state.subject.source_kind,
        }
        when action is {
          Args { script_index, script_tx_wits_opening, .. } -> {
            expect state.grammar_checkpoint_hash == #""
            expect rule.bind_script_index(state.subject, script_index)
            let view = opening.opened_field_view(
              script_tx_wits_opening,
              anchor,
              opening.script_witnesses_field_index,
              tx.reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let selected = field_item_at(view, script_index)
            expect output_script_hash == step_03_validator_script_hash
            expect output_state_data == selected_state(state, selected)
            True
          }
          CertifyScriptField {
            script_tx_wits_opening,
            checkpoint_bytes,
            item_budget,
            ..
          } -> {
            expect !state.grammar_complete
            expect state.script_checkpoint_hash == #""
            expect item_budget > 0 && item_budget <= 32
            let Pair(view, prior) = if state.grammar_checkpoint_hash == #"" {
              expect checkpoint_bytes == #""
              opening.opened_field_grammar_certification(
                script_tx_wits_opening,
                anchor,
                opening.script_witnesses_field_index,
                tx.reference_inputs,
                field_preimage_certificate_policy_id,
              )
            } else {
              opening.resume_opened_field_grammar_certification(
                script_tx_wits_opening,
                anchor,
                opening.script_witnesses_field_index,
                state.grammar_checkpoint_hash,
                checkpoint_bytes,
                tx.reference_inputs,
                field_preimage_certificate_policy_id,
              )
            }
            let next = walk.certify_field_grammar(view, prior, item_budget)
            let expected: Data = State {
              ..state,
              grammar_checkpoint_hash: walk.field_grammar_checkpoint_hash(next),
              grammar_complete: walk.field_grammar_is_complete(next),
            }
            expect output_script_hash == own_script_hash
            expect output_state_data == expected
            True
          }
          SelectCertifiedScript {
            script_index,
            script_tx_wits_opening,
            grammar_checkpoint_bytes,
            walk_checkpoint_bytes,
            item_budget,
            ..
          } -> {
            expect state.grammar_complete
            expect rule.bind_script_index(state.subject, script_index)
            expect item_budget > 0 && item_budget <= 32
            let Pair(view, prior) = if state.script_checkpoint_hash == #"" {
              expect walk_checkpoint_bytes == #""
              opening.opened_certified_field_walk_from_grammar(
                script_tx_wits_opening,
                anchor,
                opening.script_witnesses_field_index,
                state.grammar_checkpoint_hash,
                grammar_checkpoint_bytes,
                tx.reference_inputs,
                field_preimage_certificate_policy_id,
              )
            } else {
              opening.resume_opened_field_walk(
                script_tx_wits_opening,
                anchor,
                opening.script_witnesses_field_index,
                state.script_checkpoint_hash,
                walk_checkpoint_bytes,
                tx.reference_inputs,
                field_preimage_certificate_policy_id,
              )
            }
            let remaining = script_index - walk.walk_next_item_index(prior)
            expect remaining >= 0
            if remaining < item_budget {
              let at_script = walk.walk_skip(view, prior, remaining)
              let Pair(selected, _) = walk.walk_next(view, at_script)
              expect output_script_hash == step_03_validator_script_hash
              expect output_state_data == selected_state(state, selected)
              True
            } else {
              let next = walk.walk_skip(view, prior, item_budget)
              let expected: Data = State {
                ..state,
                script_checkpoint_hash: walk.field_walk_checkpoint_hash(next),
              }
              expect output_script_hash == own_script_hash
              expect output_state_data == expected
              True
            }
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn selected_state(state: State, item: ByteArray) -> Data {
  let Pair(offset, script) = decode_midgard_versioned_script_at(item, 0)
  expect offset == bytearray.length(item)
  expect encode_midgard_versioned_script(script) == item
  expect MidgardVersionedScript { language: NativeCardanoScript, .. } = script
  let output: Data = step_03.State {
    subject: state.subject,
    bad_tx_id: state.bad_tx_id,
    bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
    script_item_hash: blake2b_256(item),
    validity_interval_start: state.validity_interval_start,
    validity_interval_end: state.validity_interval_end,
  }
  output
}
