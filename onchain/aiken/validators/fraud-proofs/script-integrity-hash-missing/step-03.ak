use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, WitnessFieldOpening, WitnessTxOpening, anchored_field_view,
  anchored_native_tx, opened_field_grammar_certification,
  script_witnesses_field_index,
}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_compact_cbor_v1}
use midgard/fraud_proofs/script_integrity_hash_missing/rule
use midgard/fraud_proofs/script_integrity_hash_missing/state.{
  DecisionStateV1, ScriptGrammar, StagedStateV1, direct_field_item_limit,
  staged_batch_limit,
}
use midgard/fraud_proofs/script_integrity_hash_missing/step_03.{
  Args, Datum, Direct, SpendRedeemer, StartStaged,
}
use midgard/native_tx_field_access_v1.{
  field_item_count, provisional_field_item_count_for_certification,
}
use midgard/native_tx_machine_walk_v1.{
  certify_field_grammar, field_grammar_checkpoint_hash,
  field_grammar_is_complete,
}

validator main(
  step_04_validator_script_hash: ScriptHash,
  script_grammar_validator_script_hash: ScriptHash,
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
        let _, _, _, input_state, output_script_hash, output_state_data <- continue(
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
          Direct {
            native_tx_compact_cbor,
            witness_set,
            script_witnesses,
            redeemers,
            ..
          } -> {
            let anchored = anchored_native_tx(
              WitnessTxOpening { native_tx_compact_cbor, witness_set },
              WitnessAnchor {
                tx_id: state.subject.transaction_id,
                witness_set_hash: state.witness_set_hash,
              },
            )
            let script_view = anchored_field_view(
              anchored,
              6,
              script_witnesses,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let redeemer_view = anchored_field_view(
              anchored,
              8,
              redeemers,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            expect and {
                field_item_count(script_view) <= direct_field_item_limit,
                field_item_count(redeemer_view) <= direct_field_item_limit,
              }
            let verified = verify_native_tx_compact_cbor_v1(
              state.subject.transaction_id,
              native_tx_compact_cbor,
            )
            expect output_script_hash == step_04_validator_script_hash
            let expected: Data = DecisionStateV1 {
              subject: state.subject,
              script_integrity_hash: verified.tx_compact.body.script_integrity_hash,
              contains_non_native_script: rule.field_view_contains_non_native_script_v1(
                script_view,
              ),
              has_redeemers: field_item_count(redeemer_view) > 0,
            }
            expect output_state_data == expected
            True
          }
          StartStaged { script_witnesses_opening, item_budget, .. } -> {
            expect valid_budget(item_budget)
            let Pair(view, start) = opened_field_grammar_certification(
              script_witnesses_opening,
              WitnessAnchor {
                tx_id: state.subject.transaction_id,
                witness_set_hash: state.witness_set_hash,
              },
              script_witnesses_field_index,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            expect
              provisional_field_item_count_for_certification(view) > direct_field_item_limit
            let next = certify_field_grammar(view, start, item_budget)
            expect !field_grammar_is_complete(next)
            expect WitnessFieldOpening { native_tx_compact_cbor, .. } = script_witnesses_opening
            let verified = verify_native_tx_compact_cbor_v1(
              state.subject.transaction_id,
              native_tx_compact_cbor,
            )
            expect output_script_hash == script_grammar_validator_script_hash
            let expected: Data = StagedStateV1 {
              subject: state.subject,
              witness_set_hash: state.witness_set_hash,
              script_integrity_hash: verified.tx_compact.body.script_integrity_hash,
              phase: ScriptGrammar {
                checkpoint_hash: field_grammar_checkpoint_hash(next),
              },
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

fn input_index_of(action: Args) -> Int {
  when action is {
    Direct { input_index, .. } -> input_index
    StartStaged { input_index, .. } -> input_index
  }
}

fn output_index_of(action: Args) -> Int {
  when action is {
    Direct { output_index, .. } -> output_index
    StartStaged { output_index, .. } -> output_index
  }
}

fn valid_budget(budget: Int) -> Bool {
  expect and {
      budget > 0,
      budget <= staged_batch_limit,
    }
  True
}
