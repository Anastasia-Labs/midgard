use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_game_v1
use midgard/validation_resolution_v1

pub type Datum =
  ct.StepDatum<validation_game_v1.ValidationGameStateV1>

pub type PrepareResolutionActionV1 {
  PrepareResolution {
    input_index: Int,
    output_index: Int,
    resolver_index: Int,
    evidence: validation_resolution_v1.ValidationBoundaryEvidenceV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<PrepareResolutionActionV1>

validator main(
  one_step_resolver_script_hashes: List<ScriptHash>,
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
      ct.Continue(PrepareResolution {
        input_index,
        output_index,
        resolver_index,
        evidence,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
          fn(
            _input_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(state) = input_state
            expect Some(expected_resolver_script_hash) = list.at(
              one_step_resolver_script_hashes,
              resolver_index,
            )
            let expected_resolution_state: Data = validation_resolution_v1.prepare(
              state.dispute,
              evidence.pre_state,
              evidence.operator_post,
              evidence.challenger_post,
            )
            and {
              list.length(one_step_resolver_script_hashes) == validation_resolution_v1.resolver_count,
              resolver_index == validation_resolution_v1.resolver_index(
                evidence.pre_state.phase,
              ),
              output_script_hash == expected_resolver_script_hash,
              output_state == expected_resolution_state,
            }
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
