use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/reference_input_no_idx/step_03.{
  Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/reference_input_no_idx/step_04

validator main(
  step_04_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
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
      ct.Continue(tx_inclusion_args) -> {
        // 1. Generic validations for passing the producing tx to the next step
        //    must pass.
        //
        //    This function already validates this new tx is included in the
        //    same block as the first tx via the computation thread token's
        //    asset name.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data,
            _header,
            producing_tx_id,
            producing_tx_view
          <- pass_native_tx_to_next_step(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        expect
            Some(State {
              bad_reference_input_tx_id,
              bad_reference_input_output_index,
            })
          = m_input_state_data

        // 2. The verified producing tx ID must match the bad transaction's
        //    reference input.
        expect producing_tx_id == bad_reference_input_tx_id

        // 3. The UTxO must be produced at next step's script hash with the
        //    verified producing transaction's native outputs hash included in
        //    its datum, and the bad reference input's output index (which is
        //    expected to be larger than the last output in the provided
        //    preimage).
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          producing_tx_outputs_hash: producing_tx_view.tx_compact.body.outputs_hash,
          bad_reference_input_output_index,
        }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Selectors
//
// Step-03 re-enters the block binding for a second committed transaction: the
// producing transaction of the reference input forwarded by step-02. It may
// only forward the outputs commitment of a transaction whose canonical id is
// exactly the forwarded `bad_reference_input_tx_id`.

fn run_step_03(
  producing: fixture.NativeBlockFixtureV1,
  bad_reference_input_tx_id: ByteArray,
  bad_reference_input_output_index: Int,
) -> Bool {
  let expected_output_state: Data = step_04.State {
    producing_tx_outputs_hash: producing.compact.body.outputs_hash,
    bad_reference_input_output_index,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    producing,
    expected_output_state,
    producing.transactions_phas_root,
    producing.tx_id,
    producing.compact_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State { bad_reference_input_tx_id, bad_reference_input_output_index },
        ),
      },
    ),
    ct.Continue(fixture.native_inclusion_args_v1(producing)),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: the producing transaction named by the forwarded
/// reference input is genuinely committed under the same block's counted
/// `transactions_root`, so its canonical outputs commitment and the challenged
/// output index reach step-04.
test reference_input_no_idx_step_03_forwards_the_producing_tx_outputs_hash() {
  let producing = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_step_03(producing, producing.tx_id, 7)
}

/// Valid-block negative selector: in a valid block every reference input names
/// its true producing transaction. A prover who binds some other committed
/// transaction to the forwarded reference-input id is stopped here, so a
/// reference input that really exists cannot be walked towards a
/// non-existence verdict.
test reference_input_no_idx_step_03_rejects_a_substituted_producing_transaction() fail {
  let producing = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let other_tx_id = #"3131313131313131313131313131313131313131313131313131313131313131"
  run_step_03(producing, other_tx_id, 7)
}
