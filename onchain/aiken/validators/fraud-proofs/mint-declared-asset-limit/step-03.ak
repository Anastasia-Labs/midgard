use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{BodyAnchor, resume_opened_field_walk}
use midgard/fraud_proofs/mint_declared_asset_limit/rule.{
  FoldStateV1, begin_policy_v1, consume_assets_v1, decision_v1, fold_policy_cost,
  mint_field_index, outcome_scanning, staged_fold_budget,
}
use midgard/fraud_proofs/mint_declared_asset_limit/step_03.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_field_access_v1.{FieldViewV1}
use midgard/native_tx_machine_walk_v1.{
  FieldWalkCheckpointV1, field_walk_checkpoint_hash, walk_is_complete, walk_next,
  walk_next_item_index,
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
      ct.Continue(args) -> {
        expect and {
            args.budget > 0,
            args.budget <= staged_fold_budget,
          }
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        expect state.outcome == outcome_scanning
        let Pair(view, resumed) = resume_opened_field_walk(
          args.opening,
          BodyAnchor { tx_id: state.subject.transaction_id },
          mint_field_index,
          state.checkpoint_hash,
          args.checkpoint_bytes,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let Pair(folded, next) = advance(view, resumed, state, args.budget)
        if folded.outcome == outcome_scanning {
          // A self-loop must move the fold: no identical re-commitment.
          expect and {
              folded != state,
              !walk_is_complete(next),
            }
          let expected: Data = FoldStateV1 {
            ..folded,
            checkpoint_hash: field_walk_checkpoint_hash(next),
          }
          and {
            output_hash == own_hash,
            output_state == expected,
          }
        } else {
          let expected: Data = decision_v1(folded)
          and {
            output_hash == step_04_validator_script_hash,
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

/// Spends `budget` work units over the authenticated walk. The walk
/// checkpoint advances only when an item closes, so a partially consumed
/// policy is re-read from the same authenticated position next time.
fn advance(
  view: FieldViewV1,
  checkpoint: FieldWalkCheckpointV1,
  state: FoldStateV1,
  budget: Int,
) -> Pair<FoldStateV1, FieldWalkCheckpointV1> {
  if state.outcome != outcome_scanning {
    Pair(state, checkpoint)
  } else if and {
    state.active_policy == #"",
    budget < fold_policy_cost,
  } {
    Pair(state, checkpoint)
  } else {
    let Pair(item, next) = walk_next(view, checkpoint)
    let item_index = walk_next_item_index(checkpoint)
    let Pair(opened, remaining) = if state.active_policy == #"" {
      Pair(begin_policy_v1(state, item_index, item), budget - fold_policy_cost)
    } else {
      Pair(state, budget)
    }
    if opened.outcome != outcome_scanning {
      Pair(opened, checkpoint)
    } else {
      let Pair(consumed, left) = consume_assets_v1(
        opened,
        item_index,
        item,
        remaining,
      )
      if consumed.active_policy == #"" {
        advance(view, next, consumed, left)
      } else {
        Pair(consumed, checkpoint)
      }
    }
  }
}
