use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold
use midgard/fraud_proofs/distinct_asset_accumulation_limit/workers.{
  Datum, FieldRedeemer,
}

/// Takes a nonempty batch of spent-input items for the resolving step, which
/// returns here (by the hash recorded in the batch) while the field lasts.
validator main(
  resolve_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: FieldRedeemer,
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
      ct.Continue(args) -> {
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        let expected: Data = fold.take_inputs_v1(
          state,
          own_hash,
          args.opening,
          args.checkpoint_bytes,
          args.budget,
          tx.reference_inputs,
          certificate_policy,
        )
        output_hash == resolve_hash && output_state == expected
      }
    }
  }

  else(_) {
    fail
  }
}
