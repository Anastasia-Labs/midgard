use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/canonical_decode_item_staging_v1 as staging
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine_v1.{verify_canonical_decode_item_successor_v1}
use midgard/validation_resolution_v1.{winning_resolution}

pub type Datum =
  ct.StepDatum<staging.VerifiedCanonicalDecodeItemV1>

pub type ActionV1 {
  Settle { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
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
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Settle { input_index, output_index }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        continue(
          computation_thread_policy_id,
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
            let prepared = state.observed.prepared
            let authenticated = prepared.authenticated
            let expected_output_state: Data = winning_resolution()
            and {
              staging.verified_is_well_formed(state),
              verify_canonical_decode_item_successor_v1(
                authenticated.base.resolution.pre_state,
                authenticated.transition,
                prepared.source,
                state.proof,
              ),
              output_script_hash == award_script_hash,
              output_state == expected_output_state,
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
