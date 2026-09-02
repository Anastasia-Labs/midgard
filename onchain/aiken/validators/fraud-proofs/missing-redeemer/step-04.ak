use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, resume_opened_field_walk,
}
use midgard/fraud_proofs/missing_redeemer/rule.{
  ScanStateV1, decision_v1, redeemer_field_index, scan_batch, scan_item_v1,
}
use midgard/fraud_proofs/missing_redeemer/step_04.{Args, Datum, SpendRedeemer}
use midgard/native_tx_field_access_v1.{FieldViewV1}
use midgard/native_tx_machine_walk_v1.{
  FieldWalkCheckpointV1, field_walk_checkpoint_hash, walk_is_complete, walk_next,
  walk_next_item_index,
}

validator main(
  step_05_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(args) -> {
        expect and {
            args.item_budget > 0,
            args.item_budget <= scan_batch,
          }
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        let Pair(view, resumed) = resume_opened_field_walk(
          args.opening,
          WitnessAnchor {
            tx_id: state.authenticated.bound.subject.transaction_id,
            witness_set_hash: state.authenticated.bound.witness_set_hash,
          },
          redeemer_field_index,
          state.checkpoint_hash,
          args.checkpoint_bytes,
          reference_inputs,
          certificate_policy,
        )
        let Pair(scanned, next) = advance(
          view,
          resumed,
          state,
          args.item_budget,
        )
        if or {
          scanned.found,
          walk_is_complete(next),
        } {
          let expected: Data = decision_v1(scanned)
          and {
            output_hash == step_05_hash,
            output_state == expected,
          }
        } else {
          let expected: Data = ScanStateV1 {
            ..scanned,
            checkpoint_hash: field_walk_checkpoint_hash(next),
          }
          and {
            output_hash == own_hash,
            output_state == expected,
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn advance(
  view: FieldViewV1,
  checkpoint: FieldWalkCheckpointV1,
  state: ScanStateV1,
  budget: Int,
) -> Pair<ScanStateV1, FieldWalkCheckpointV1> {
  if or {
    budget == 0,
    state.found,
    walk_is_complete(checkpoint),
  } {
    Pair(state, checkpoint)
  } else {
    let index = walk_next_item_index(checkpoint)
    let Pair(item, next) = walk_next(view, checkpoint)
    advance(view, next, scan_item_v1(state, index, item), budget - 1)
  }
}
