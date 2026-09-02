use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_script_source/rule.{
  SourceDescriptorV1, SourceScanStateV1, scan_complete_v1, scan_source_v1,
  staged_source_budget, state_is_authentic_v1,
}
use midgard/fraud_proofs/missing_script_source/step_05.{
  Args, Datum, SpendRedeemer,
}

validator main(
  step_06_validator_script_hash: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, sources, item_budget }) -> {
        expect and {
            item_budget >= 0,
            item_budget <= staged_source_budget,
            list.length(sources) <= item_budget,
          }
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        expect and {
            state_is_authentic_v1(state),
            state.next_expected_script_hash == own_hash,
          }
        let advanced = advance(
          state,
          sources,
          own_hash,
          step_06_validator_script_hash,
        )
        let expected: Data = advanced
        let complete = scan_complete_v1(advanced)
        and {
          output_hash == if complete {
            step_06_validator_script_hash
          } else {
            own_hash
          },
          output_state == expected,
          or {
            list.length(sources) > 0,
            complete,
          },
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn advance(
  state: SourceScanStateV1,
  sources: List<SourceDescriptorV1>,
  own_hash: ByteArray,
  final_hash: ByteArray,
) -> SourceScanStateV1 {
  when sources is {
    [] -> state
    [source, ..rest] -> {
      let next_cursor = state.cursor + 1
      let next_hash = if next_cursor == state.authenticated.purpose.source_count {
        final_hash
      } else {
        own_hash
      }
      advance(
        scan_source_v1(state, source, next_hash),
        rest,
        own_hash,
        final_hash,
      )
    }
  }
}
