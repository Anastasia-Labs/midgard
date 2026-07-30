use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_collection_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/zero_input/step_02.{Args, Datum, SpendRedeemer, State}

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

        expect Some(State { bad_tx_spend_inputs_hash }) = m_input_state_data

        // 2. The spend inputs collection must be empty under the canonical
        //    native-V1 bounded-collection commitment (field 0 is the
        //    consensus-ordered spend-inputs field).
        expect
          bad_tx_spend_inputs_hash == bounded_collection_v1.from_items(0, [])

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn finalize_datum(state: State) -> Datum {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(state),
  }
}

fn run_finalize(state: State) -> Bool {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(finalize_datum(state)),
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

/// The canonical native empty spend-inputs commitment finalizes the proof.
test zero_input_step_02_accepts_empty_native_spend_inputs() {
  run_finalize(
    State { bad_tx_spend_inputs_hash: bounded_collection_v1.from_items(0, []) },
  )
}

/// Valid-block negative selector: a transaction from a valid block spends at
/// least one input, so its canonical spend-inputs hash is non-empty and the
/// challenge cannot finalize.
test zero_input_step_02_rejects_valid_block_challenge() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  run_finalize(
    State {
      bad_tx_spend_inputs_hash: valid_block.compact.body.spend_inputs_hash,
    },
  )
}
