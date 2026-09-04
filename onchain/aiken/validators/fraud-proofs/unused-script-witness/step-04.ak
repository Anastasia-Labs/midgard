use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/unused_script_witness/rule.{
  ReverseScanV1, SourceOpeningV1, alternate_sources_complete_v1,
  authenticate_earlier_source_v1,
}
use midgard/fraud_proofs/unused_script_witness/step_04.{
  Args, Datum, SpendRedeemer,
}

validator main(
  step_05_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
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
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        let scanned = advance_sources(state, args.openings)
        expect alternate_sources_complete_v1(scanned)
        let expected: Data = scanned
        and {
          output_hash == step_05_validator_script_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn advance_sources(
  state: ReverseScanV1,
  openings: List<SourceOpeningV1>,
) -> ReverseScanV1 {
  when openings is {
    [] -> state
    [opening, ..rest] ->
      advance_sources(authenticate_earlier_source_v1(state, opening), rest)
  }
}
