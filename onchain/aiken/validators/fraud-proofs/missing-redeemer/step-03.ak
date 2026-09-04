use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, opened_certified_field_walk_from_grammar,
  opened_field_grammar_certification, opened_field_walk,
  resume_opened_field_grammar_certification,
}
use midgard/fraud_proofs/missing_redeemer/rule.{
  initial_scan_v1, redeemer_field_index, scan_batch,
}
use midgard/fraud_proofs/missing_redeemer/step_03.{
  ActionV1, AuthenticateDirect, Datum, FinishGrammar, Grammar, Ready,
  ResumeGrammar, SpendRedeemer, StartGrammar,
}
use midgard/native_tx_field_access_v1.{
  field_count_requires_certification, field_item_count,
}
use midgard/native_tx_machine_walk_v1.{
  certify_field_grammar, field_grammar_checkpoint_hash,
  field_grammar_is_complete, field_walk_checkpoint_hash,
}

validator main(
  step_04_hash: ScriptHash,
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
      ct.Continue(action) ->
        execute(
          action,
          datum,
          own_ref,
          tx,
          step_04_hash,
          thread_policy,
          certificate_policy,
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
  own_ref: OutputReference,
  tx: Transaction,
  step_04_hash: ScriptHash,
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
    own_ref,
    inputs,
    outputs,
  )
  expect Some(state) = input_state
  when action is {
    AuthenticateDirect { opening, .. } -> {
      expect Ready { authenticated } = state
      let Pair(view, start) = opened_field_walk(
        opening,
        WitnessAnchor {
          tx_id: authenticated.bound.subject.transaction_id,
          witness_set_hash: authenticated.bound.witness_set_hash,
        },
        redeemer_field_index,
        reference_inputs,
        certificate_policy,
      )
      expect !field_count_requires_certification(view)
      let expected: Data = initial_scan_v1(
        authenticated,
        field_walk_checkpoint_hash(start),
        field_item_count(view),
      )
      and {
        output_hash == step_04_hash,
        output_state == expected,
      }
    }
    StartGrammar { opening, item_budget, .. } -> {
      expect and {
          item_budget > 0,
          item_budget <= scan_batch,
        }
      expect Ready { authenticated } = state
      let Pair(view, start) = opened_field_grammar_certification(
        opening,
        WitnessAnchor {
          tx_id: authenticated.bound.subject.transaction_id,
          witness_set_hash: authenticated.bound.witness_set_hash,
        },
        redeemer_field_index,
        reference_inputs,
        certificate_policy,
      )
      let next = certify_field_grammar(view, start, item_budget)
      let expected: Data = Grammar {
        authenticated,
        checkpoint_hash: field_grammar_checkpoint_hash(next),
      }
      and {
        output_hash == own_hash,
        output_state == expected,
      }
    }
    ResumeGrammar { opening, checkpoint_bytes, item_budget, .. } -> {
      expect and {
          item_budget > 0,
          item_budget <= scan_batch,
        }
      expect Grammar { authenticated, checkpoint_hash } = state
      let Pair(view, resumed) = resume_opened_field_grammar_certification(
        opening,
        WitnessAnchor {
          tx_id: authenticated.bound.subject.transaction_id,
          witness_set_hash: authenticated.bound.witness_set_hash,
        },
        redeemer_field_index,
        checkpoint_hash,
        checkpoint_bytes,
        reference_inputs,
        certificate_policy,
      )
      expect !field_grammar_is_complete(resumed)
      let next = certify_field_grammar(view, resumed, item_budget)
      let expected: Data = Grammar {
        authenticated,
        checkpoint_hash: field_grammar_checkpoint_hash(next),
      }
      and {
        output_hash == own_hash,
        output_state == expected,
      }
    }
    FinishGrammar { opening, checkpoint_bytes, .. } -> {
      expect Grammar { authenticated, checkpoint_hash } = state
      let Pair(view, start) = opened_certified_field_walk_from_grammar(
        opening,
        WitnessAnchor {
          tx_id: authenticated.bound.subject.transaction_id,
          witness_set_hash: authenticated.bound.witness_set_hash,
        },
        redeemer_field_index,
        checkpoint_hash,
        checkpoint_bytes,
        reference_inputs,
        certificate_policy,
      )
      let expected: Data = initial_scan_v1(
        authenticated,
        field_walk_checkpoint_hash(start),
        field_item_count(view),
      )
      and {
        output_hash == step_04_hash,
        output_state == expected,
      }
    }
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
