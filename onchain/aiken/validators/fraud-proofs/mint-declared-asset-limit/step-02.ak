use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_certified_field_walk_from_grammar,
  opened_field_grammar_certification, opened_field_walk,
  resume_opened_field_grammar_certification,
}
use midgard/fraud_proofs/mint_declared_asset_limit/rule.{
  initial_fold_v1, mint_field_index, staged_policy_budget,
}
use midgard/fraud_proofs/mint_declared_asset_limit/state.{Bound, Grammar}
use midgard/fraud_proofs/mint_declared_asset_limit/step_02.{
  ActionV1, AuthenticateDirect, Datum, FinishGrammar, ResumeGrammar,
  SpendRedeemer, StartGrammar,
}
use midgard/native_tx_field_access_v1.{
  field_count_requires_certification, field_item_at,
}
use midgard/native_tx_machine_walk_v1.{
  certify_field_grammar, field_grammar_checkpoint_hash,
  field_grammar_is_complete, field_walk_checkpoint_hash,
}

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
      ct.Continue(action) ->
        execute(
          action,
          datum,
          own_out_ref,
          tx,
          step_03_validator_script_hash,
          computation_thread_token_policy_id,
          field_preimage_certificate_policy_id,
        )
    }
  }

  else(_) {
    fail
  }
}

fn execute(
  action: ActionV1,
  datum: Option<Datum>,
  own_out_ref: OutputReference,
  tx: Transaction,
  step_03_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) -> Bool {
  let Transaction { inputs, outputs, reference_inputs, .. } = tx
  expect Some(step_datum) = datum
  let own_hash, _, _, input_state, output_hash, output_state <- continue(
    thread_policy,
    step_datum,
    input_index(action),
    output_index(action),
    own_out_ref,
    inputs,
    outputs,
  )
  expect Some(state) = input_state
  when action is {
    AuthenticateDirect { opening, .. } -> {
      expect Bound { bound } = state
      let Pair(view, start) = opened_field_walk(
        opening,
        BodyAnchor { tx_id: bound.subject.transaction_id },
        mint_field_index,
        reference_inputs,
        certificate_policy,
      )
      expect !field_count_requires_certification(view)
      let expected: Data = initial_fold_v1(
        bound,
        field_item_at(view, bound.policy_index),
        field_walk_checkpoint_hash(start),
      )
      and {
        output_hash == step_03_hash,
        output_state == expected,
      }
    }
    StartGrammar { opening, item_budget, .. } -> {
      expect valid_budget(item_budget)
      expect Bound { bound } = state
      let Pair(view, start) = opened_field_grammar_certification(
        opening,
        BodyAnchor { tx_id: bound.subject.transaction_id },
        mint_field_index,
        reference_inputs,
        certificate_policy,
      )
      let next = certify_field_grammar(view, start, item_budget)
      let expected: Data = Grammar {
        bound,
        checkpoint_hash: field_grammar_checkpoint_hash(next),
      }
      and {
        output_hash == own_hash,
        output_state == expected,
      }
    }
    ResumeGrammar { opening, checkpoint_bytes, item_budget, .. } -> {
      expect valid_budget(item_budget)
      expect Grammar { bound, checkpoint_hash } = state
      let Pair(view, resumed) = resume_opened_field_grammar_certification(
        opening,
        BodyAnchor { tx_id: bound.subject.transaction_id },
        mint_field_index,
        checkpoint_hash,
        checkpoint_bytes,
        reference_inputs,
        certificate_policy,
      )
      expect !field_grammar_is_complete(resumed)
      let next = certify_field_grammar(view, resumed, item_budget)
      let expected: Data = Grammar {
        bound,
        checkpoint_hash: field_grammar_checkpoint_hash(next),
      }
      and {
        output_hash == own_hash,
        output_state == expected,
      }
    }
    FinishGrammar { opening, checkpoint_bytes, .. } -> {
      expect Grammar { bound, checkpoint_hash } = state
      let Pair(view, start) = opened_certified_field_walk_from_grammar(
        opening,
        BodyAnchor { tx_id: bound.subject.transaction_id },
        mint_field_index,
        checkpoint_hash,
        checkpoint_bytes,
        reference_inputs,
        certificate_policy,
      )
      let expected: Data = initial_fold_v1(
        bound,
        field_item_at(view, bound.policy_index),
        field_walk_checkpoint_hash(start),
      )
      and {
        output_hash == step_03_hash,
        output_state == expected,
      }
    }
  }
}

fn valid_budget(value: Int) -> Bool {
  and {
    value > 0,
    value <= staged_policy_budget,
  }
}

fn input_index(action: ActionV1) -> Int {
  when action is {
    AuthenticateDirect { input_index, .. } -> input_index
    StartGrammar { input_index, .. } -> input_index
    ResumeGrammar { input_index, .. } -> input_index
    FinishGrammar { input_index, .. } -> input_index
  }
}

fn output_index(action: ActionV1) -> Int {
  when action is {
    AuthenticateDirect { output_index, .. } -> output_index
    StartGrammar { output_index, .. } -> output_index
    ResumeGrammar { output_index, .. } -> output_index
    FinishGrammar { output_index, .. } -> output_index
  }
}
