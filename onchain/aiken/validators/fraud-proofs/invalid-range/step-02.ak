use aiken_design_patterns/validity_range_normalization.{
  Always, ClosedRange, FromNegInf, InvalidRange, ToPosInf,
} as vrn
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/invalid_range/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

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
            m_input_state_data
          <- finalize(
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

        expect
            Some(State {
              block_valid_from,
              block_valid_to,
              bad_tx_normalized_validity_range,
            })
          = m_input_state_data

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

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)

fn run_finalize(state: State) -> Bool {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// Positive selector: both malformed-interval shapes finalize into a permanent
/// fraud-proof token — a closed interval whose lower bound falls below the
/// block's `start_time`, and an inverted interval, which overlaps no block
/// interval at all. The inverted case is the executable disposition of F21
/// physical structural row L302 (Q49-L302), previously an unverified inference.
test invalid_range_step_02_accepts_uncovered_and_inverted_intervals() {
  and {
    run_finalize(
      State {
        block_valid_from: 10,
        block_valid_to: 20,
        bad_tx_normalized_validity_range: ClosedRange { lower: 5, upper: 8 },
      },
    ),
    run_finalize(
      State {
        block_valid_from: 10,
        block_valid_to: 20,
        bad_tx_normalized_validity_range: InvalidRange,
      },
    ),
  }
}

/// Valid-block negative selector: in a valid block every transaction interval
/// lies inside the header's `[start_time, end_time)` window, so a challenge
/// carrying a covered interval cannot finalize.
test invalid_range_step_02_rejects_valid_block_challenge() fail {
  run_finalize(
    State {
      block_valid_from: 10,
      block_valid_to: 20,
      bad_tx_normalized_validity_range: ClosedRange { lower: 10, upper: 19 },
    },
  )
}
