use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  NativeTxAnchorV1, WitnessAnchor, resume_opened_field_walk,
  script_witnesses_field_index,
}
use midgard/fraud_proofs/script_integrity_hash_missing/rule.{
  contains_non_native_script_items_v1,
}
use midgard/fraud_proofs/script_integrity_hash_missing/script_scan.{
  Datum, SpendRedeemer,
}
use midgard/fraud_proofs/script_integrity_hash_missing/state.{
  ScriptComplete, ScriptScan, StagedStateV1, staged_batch_limit,
}
use midgard/native_tx_machine_walk_v1.{
  field_walk_checkpoint_hash, walk_fold, walk_is_complete,
}

validator main(
  redeemer_grammar_validator_script_hash: ScriptHash,
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
      ct.Continue(args) -> {
        expect and {
            args.item_budget > 0,
            args.item_budget <= staged_batch_limit,
          }
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
          args.input_index,
          args.output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        expect ScriptScan { checkpoint_hash, contains_non_native_script } = state.phase
        let Pair(view, resumed) = resume_opened_field_walk(
          args.opening,
          anchor(state),
          script_witnesses_field_index,
          checkpoint_hash,
          args.checkpoint_bytes,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let Pair(found, next) = walk_fold(
          view,
          resumed,
          args.item_budget,
          contains_non_native_script,
          fn(found, _index, item) {
            or {
              found,
              contains_non_native_script_items_v1([item]),
            }
          },
        )
        if walk_is_complete(next) {
          expect output_script_hash == redeemer_grammar_validator_script_hash
          expect_state(
            output_state_data,
            StagedStateV1 {
              ..state,
              phase: ScriptComplete { contains_non_native_script: found },
            },
          )
        } else {
          expect output_script_hash == own_script_hash
          expect_state(
            output_state_data,
            StagedStateV1 {
              ..state,
              phase: ScriptScan {
                checkpoint_hash: field_walk_checkpoint_hash(next),
                contains_non_native_script: found,
              },
            },
          )
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

fn expect_state(actual: Data, expected: StagedStateV1) -> Bool {
  let expected_data: Data = expected
  expect actual == expected_data
  True
}
