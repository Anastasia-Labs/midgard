use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold
use midgard/fraud_proofs/distinct_asset_accumulation_limit/workers.{
  RankDatum, RankRedeemer,
}

/// Places the crossing spent input in the input-resolution schedule by
/// ranking it among the reference inputs, and hands the crossing to the
/// terminal.
validator main(
  terminal_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<RankDatum>,
    redeemer: RankRedeemer,
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
        let _, _, _, input_state, output_hash, output_state <- continue(
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
        let expected: Data = fold.rank_v1(
          state,
          args.opening,
          args.rank,
          tx.reference_inputs,
          certificate_policy,
        )
        output_hash == terminal_hash && output_state == expected
      }
    }
  }

  else(_) {
    fail
  }
}
