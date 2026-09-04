use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  NativeTxAnchorV1, WitnessAnchor, opened_certified_field_walk_from_grammar,
  opened_field_grammar_certification, redeemers_field_index,
  resume_opened_field_grammar_certification,
}
use midgard/fraud_proofs/script_integrity_hash_missing/redeemer_grammar.{
  Args, Datum, Finish, Resume, SpendRedeemer, Start,
}
use midgard/fraud_proofs/script_integrity_hash_missing/state.{
  DecisionStateV1, RedeemerGrammar, ScriptComplete, StagedStateV1,
  staged_batch_limit,
}
use midgard/native_tx_machine_walk_v1.{
  certify_field_grammar, field_grammar_checkpoint_hash,
  field_grammar_is_complete, walk_remaining,
}

validator main(
  step_04_validator_script_hash: ScriptHash,
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
            _,
            _,
            input_state,
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
        expect Some(state) = input_state
        when action is {
          Start { opening, item_budget, .. } -> {
            expect valid_budget(item_budget)
            expect ScriptComplete { contains_non_native_script } = state.phase
            let Pair(view, start) = opened_field_grammar_certification(
              opening,
              anchor(state),
              redeemers_field_index,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let next = certify_field_grammar(view, start, item_budget)
            expect output_script_hash == own_script_hash
            expect_state(
              output_state_data,
              StagedStateV1 {
                ..state,
                phase: RedeemerGrammar {
                  checkpoint_hash: field_grammar_checkpoint_hash(next),
                  contains_non_native_script,
                },
              },
            )
          }
          Resume { opening, checkpoint_bytes, item_budget, .. } -> {
            expect valid_budget(item_budget)
            expect
                RedeemerGrammar { checkpoint_hash, contains_non_native_script }
              = state.phase
            let Pair(view, resumed) = resume_opened_field_grammar_certification(
              opening,
              anchor(state),
              redeemers_field_index,
              checkpoint_hash,
              checkpoint_bytes,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            expect !field_grammar_is_complete(resumed)
            let next = certify_field_grammar(view, resumed, item_budget)
            expect output_script_hash == own_script_hash
            expect_state(
              output_state_data,
              StagedStateV1 {
                ..state,
                phase: RedeemerGrammar {
                  checkpoint_hash: field_grammar_checkpoint_hash(next),
                  contains_non_native_script,
                },
              },
            )
          }
          Finish { opening, checkpoint_bytes, .. } -> {
            expect
                RedeemerGrammar { checkpoint_hash, contains_non_native_script }
              = state.phase
            let Pair(_, start) = opened_certified_field_walk_from_grammar(
              opening,
              anchor(state),
              redeemers_field_index,
              checkpoint_hash,
              checkpoint_bytes,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            expect output_script_hash == step_04_validator_script_hash
            let expected: Data = DecisionStateV1 {
              subject: state.subject,
              script_integrity_hash: state.script_integrity_hash,
              contains_non_native_script,
              has_redeemers: walk_remaining(start) > 0,
            }
            expect output_state_data == expected
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

fn anchor(state: StagedStateV1) -> NativeTxAnchorV1 {
  WitnessAnchor {
    tx_id: state.subject.transaction_id,
    witness_set_hash: state.witness_set_hash,
  }
}

fn input_index_of(action: Args) -> Int {
  when action is {
    Start { input_index, .. } -> input_index
    Resume { input_index, .. } -> input_index
    Finish { input_index, .. } -> input_index
  }
}

fn output_index_of(action: Args) -> Int {
  when action is {
    Start { output_index, .. } -> output_index
    Resume { output_index, .. } -> output_index
    Finish { output_index, .. } -> output_index
  }
}

fn valid_budget(budget: Int) -> Bool {
  expect and {
      budget > 0,
      budget <= staged_batch_limit,
    }
  True
}

fn expect_state(actual: Data, expected: StagedStateV1) -> Bool {
  let expected_data: Data = expected
  expect actual == expected_data
  True
}
