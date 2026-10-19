use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/mint_purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as door

validator main(
  source_lookup_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<door.MintDatum>,
    redeemer: door.MintScanRedeemer,
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
      ct.Continue(action) -> {
        let Pair(input_index, output_index) = when action is {
          door.Scan { args } -> Pair(args.input_index, args.output_index)
          door.Finish { input_index, output_index } ->
            Pair(input_index, output_index)
        }
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        when action is {
          door.Scan { args } -> {
            expect args.budget <= door.scan_batch
            let Pair(advanced, _) = mint_purpose.advance_v1(
              state.scan,
              args.opening,
              args.checkpoint_bytes,
              args.budget,
              tx.reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let expected: Data = door.MintStateV1 { ..state, scan: advanced }
            and {
              output_hash == own_hash,
              output_state == expected,
            }
          }
          door.Finish { .. } -> {
            let expected: Data = door.finish_mint_v1(state)
            and {
              output_hash == source_lookup_script_hash,
              output_state == expected,
            }
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
