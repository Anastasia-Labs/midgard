use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  NativeTxAnchorV1, WitnessAnchor, opened_certified_field_walk_from_grammar,
  resume_opened_field_grammar_certification, script_witnesses_field_index,
}
use midgard/fraud_proofs/missing_native_script_tx/step_06.{
  GrammarCertification, SemanticScan, State, direct_script_witness_limit,
  staged_batch_limit,
}
use midgard/fraud_proofs/missing_native_script_tx/step_07.{
  Args, Datum, ResumeGrammarCertification, SpendRedeemer, StartSemanticScan,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/native_tx_machine_walk_v1.{
  certify_field_grammar, field_grammar_checkpoint_hash,
  field_grammar_is_complete, field_walk_checkpoint_hash, walk_fold,
  walk_is_complete, walk_remaining,
}
use midgard/script_proof_v1

validator main(
  step_08_validator_script_hash: ScriptHash,
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
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
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
          input_index_of(action),
          output_index_of(action),
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = m_input_state_data
        when action is {
          ResumeGrammarCertification {
            script_tx_wits_opening,
            checkpoint_bytes,
            item_budget,
            ..
          } -> {
            expect expect_valid_staged_budget(item_budget)
            expect GrammarCertification { checkpoint_hash } = state.phase
            let Pair(view, resumed) = resume_opened_field_grammar_certification(
              script_tx_wits_opening,
              anchor_of(state),
              script_witnesses_field_index,
              checkpoint_hash,
              checkpoint_bytes,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            // A terminal grammar state may only cross through the other arm;
            // it cannot self-loop indefinitely under a terminal certificate.
            expect !field_grammar_is_complete(resumed)
            let next = certify_field_grammar(view, resumed, item_budget)
            expect output_script_hash == own_script_hash
            expect_output_state(
              output_state_data,
              State {
                ..state,
                phase: GrammarCertification {
                  checkpoint_hash: field_grammar_checkpoint_hash(next),
                },
              },
            )
          }
          StartSemanticScan {
            script_tx_wits_opening,
            grammar_checkpoint_bytes,
            item_budget,
            ..
          } -> {
            expect expect_valid_staged_budget(item_budget)
            expect GrammarCertification { checkpoint_hash } = state.phase
            let Pair(view, start) = opened_certified_field_walk_from_grammar(
              script_tx_wits_opening,
              anchor_of(state),
              script_witnesses_field_index,
              checkpoint_hash,
              grammar_checkpoint_bytes,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            expect walk_remaining(start) > direct_script_witness_limit
            let Pair(found, next) = walk_fold(
              view,
              start,
              item_budget,
              False,
              fn(found, index, item) {
                scan_script_witness(
                  state.expected_missing_script_hash,
                  found,
                  index,
                  item,
                )
              },
            )
            expect !walk_is_complete(next)
            expect output_script_hash == step_08_validator_script_hash
            expect_output_state(
              output_state_data,
              State {
                ..state,
                phase: SemanticScan {
                  checkpoint_hash: field_walk_checkpoint_hash(next),
                  required_script_is_present: found,
                },
              },
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

fn input_index_of(action: Args) -> Int {
  when action is {
    ResumeGrammarCertification { input_index, .. } -> input_index
    StartSemanticScan { input_index, .. } -> input_index
  }
}

fn output_index_of(action: Args) -> Int {
  when action is {
    ResumeGrammarCertification { output_index, .. } -> output_index
    StartSemanticScan { output_index, .. } -> output_index
  }
}

fn anchor_of(state: State) -> NativeTxAnchorV1 {
  WitnessAnchor {
    tx_id: state.bad_tx_id,
    witness_set_hash: state.bad_tx_witness_set_hash,
  }
}

fn expect_valid_staged_budget(item_budget: Int) -> Bool {
  expect item_budget > 0
  expect item_budget <= staged_batch_limit
  True
}

fn expect_output_state(output_state_data: Data, expected_state: State) -> Bool {
  let expected: Data = expected_state
  expect output_state_data == expected
  True
}

fn scan_script_witness(
  expected_missing_script_hash: ByteArray,
  found: Bool,
  _index: Int,
  item: ByteArray,
) -> Bool {
  let Pair(_offset, script_wit) = decode_midgard_versioned_script_at(item, 0)
  expect encode_midgard_versioned_script(script_wit) == item
  or {
    found,
    script_proof_v1.versioned_script_hash(script_wit) == expected_missing_script_hash,
  }
}
