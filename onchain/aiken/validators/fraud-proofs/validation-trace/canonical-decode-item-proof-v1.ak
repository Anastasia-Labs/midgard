use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/canonical_decode_item_staging_v1 as staging
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine_v1.{verify_canonical_decode_item_observation_v1}

pub type Datum =
  ct.StepDatum<staging.ObservedCanonicalDecodeItemV1>

pub type ActionV1 {
  Verify { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  successor_verifier_script_hash: ScriptHash,
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
      ct.Continue(Verify { input_index, output_index }) -> {
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
            expect Some(observed) = input_state
            let prepared = observed.prepared
            let expected_output_state: Data = staging.verify(
              observed,
              verify_canonical_decode_item_observation_v1(
                prepared.authenticated.transition,
                prepared.source,
                observed.observation,
              ),
            )
            and {
              staging.observed_is_well_formed(observed),
              output_script_hash == successor_verifier_script_hash,
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
