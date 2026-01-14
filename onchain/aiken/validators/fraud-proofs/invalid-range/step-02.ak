use aiken_design_patterns/validity_range_normalization.{
  Always, ClosedRange, FromNegInf, ToPosInf,
} as vrn
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/invalid_range/step_02.{
  Args, Datum, SpendRedeemer, State,
}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
        <-
          finalize(
            computation_thread_token_policy_id,
            fraud_proof_token_policy_id,
            fraud_proof_token_address,
            step_datum,
            input_index,
            output_index,
            fraud_proof_mint_redeemer_index,
            own_out_ref,
            inputs,
            outputs,
            redeemers,
          )

        expect Some(State {
          block_valid_from,
          block_valid_to,
          bad_tx_normalized_validity_range,
        }) = m_input_state_data

        // 2. One or more **bounds** of the bad transaction's validity interval
        //    must fall out of the block's. Meaning unbounded ends are not
        //    validated against the block's time range.
        //
        //    Also if the range is invalid (i.e. lower bound is greater than or
        //    equal to the upper bound), the fraud proof should be able to
        //    succeed.
        //
        //    > [!NOTE]
        //    > We are assuming inclusive lower bound and exclusive upper bound
        //    > in the block. TODO: This needs clarification in specs.
        expect
          when bad_tx_normalized_validity_range is {
            ClosedRange { lower, upper } -> or {
                lower < block_valid_from,
                upper >= block_valid_to,
              }
            FromNegInf { upper } -> upper >= block_valid_to
            ToPosInf { lower } -> lower < block_valid_from
            Always -> fail @"The tx does not have an invalid time range"
            InvalidRange -> True
          }

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
