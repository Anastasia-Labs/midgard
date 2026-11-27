use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/script_integrity_hash_mismatch/terminal.{
  Args, Datum, SpendRedeemer, convicts_v1,
}

validator main(
  fraud_policy: PolicyId,
  fraud_address: Address,
  thread_policy: PolicyId,
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
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        witness_set_compact_cbor,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state <- finalize(
          thread_policy,
          fraud_policy,
          fraud_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_ref,
          inputs,
          outputs,
          redeemers,
        )
        expect Some(state) = input_state
        convicts_v1(state, witness_set_compact_cbor)
      }
    }
  }

  else(_) {
    fail
  }
}
