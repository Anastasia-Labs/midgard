use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/redeemer
use midgard/fraud_proofs/field_opening_v1.{redeemers_field_index}
use midgard/fraud_proofs/missing_redeemer/rule
use midgard/fraud_proofs/missing_redeemer/step_03.{Datum, SpendRedeemer}

validator main(
  next_step_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  field_preimage_certificate_policy: PolicyId,
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
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(args) -> {
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        expect and {
            !state.scan.complete,
            !state.scan.found,
            args.budget <= rule.scan_batch,
          }
        let (advanced, _, complete) = cursor.certify_with_completion_at_v1(
          state.scan.cursor,
          redeemers_field_index,
          args.opening,
          args.checkpoint_bytes,
          args.budget,
          tx.reference_inputs,
          field_preimage_certificate_policy,
        )
        let expected: Data = rule.ScanStateV1 {
          ..state,
          scan: redeemer.PointerStateV1 {
            ..state.scan,
            cursor: advanced,
            complete,
          },
        }
        and {
          output_hash == if advanced.phase == cursor.Walking {
            next_step_hash
          } else {
            own_hash
          },
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
