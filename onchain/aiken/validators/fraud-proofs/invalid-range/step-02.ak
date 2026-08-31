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

        expect Some(State { block_slot, bad_tx_normalized_validity_range }) = m_input_state_data

        // 2. Canonical Phase B evaluates the normalized transaction interval
        //    at `header.block_slot`. A valid closed interval includes both
        //    endpoints after the native exclusive upper bound was normalized
        //    to an inclusive one. The proof succeeds iff that exact slot lies
        //    outside the normalized interval, or the interval is malformed.
        expect
          when bad_tx_normalized_validity_range is {
            ClosedRange { lower, upper } -> or {
                lower > block_slot,
                upper < block_slot,
              }
            FromNegInf { upper } -> upper < block_slot
            ToPosInf { lower } -> lower > block_slot
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
/// fraud-proof token — a closed interval whose upper bound falls below the
/// block's replay slot, and an inverted interval. The inverted case is the
/// executable disposition of F21
/// physical structural row L302 (Q49-L302), previously an unverified inference.
test invalid_range_step_02_accepts_uncovered_and_inverted_intervals() {
  and {
    run_finalize(
      State {
        block_slot: 10,
        bad_tx_normalized_validity_range: ClosedRange { lower: 5, upper: 8 },
      },
    ),
    run_finalize(
      State {
        block_slot: 10,
        bad_tx_normalized_validity_range: ClosedRange { lower: 11, upper: 20 },
      },
    ),
    run_finalize(
      State {
        block_slot: 10,
        bad_tx_normalized_validity_range: FromNegInf { upper: 9 },
      },
    ),
    run_finalize(
      State {
        block_slot: 10,
        bad_tx_normalized_validity_range: ToPosInf { lower: 11 },
      },
    ),
    run_finalize(
      State { block_slot: 10, bad_tx_normalized_validity_range: InvalidRange },
    ),
  }
}

test invalid_range_step_02_rejects_valid_lower_boundary() fail {
  run_finalize(
    State {
      block_slot: 10,
      bad_tx_normalized_validity_range: ToPosInf { lower: 10 },
    },
  )
}

test invalid_range_step_02_rejects_valid_upper_boundary() fail {
  run_finalize(
    State {
      block_slot: 10,
      bad_tx_normalized_validity_range: FromNegInf { upper: 10 },
    },
  )
}

test invalid_range_step_02_rejects_always_valid_range() fail {
  run_finalize(
    State { block_slot: 10, bad_tx_normalized_validity_range: Always },
  )
}

/// Valid-block negative selector: in a valid block every transaction interval
/// contains the committed replay slot, so a challenge carrying either exact
/// boundary cannot finalize.
test invalid_range_step_02_rejects_valid_block_challenge() fail {
  run_finalize(
    State {
      block_slot: 10,
      bad_tx_normalized_validity_range: ClosedRange { lower: 10, upper: 10 },
    },
  )
}
