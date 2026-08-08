use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/missing_native_script_tx/step_06.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_versioned_script}
use midgard/fraud_proofs/native_tx/types.{
  MidgardVersionedScript, NativeCardanoScript,
}
use midgard/native_tx_field_access_v1
use midgard/script_proof_v1

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
        script_tx_wits_preimage,
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
              expected_missing_script_hash,
              verified_script_wits_hash,
            })
          = m_input_state_data

        // 2. The provided script-witness preimage must match the canonical
        //    native collection hash carried from the previous steps
        //    (consensus field 6, canonical versioned-script CBOR per item).
        expect
          native_tx_field_access_v1.field_commitment_from_items(list.map(script_tx_wits_preimage, encode_midgard_versioned_script)) == verified_script_wits_hash

        // 3. The bad transaction's script witnesses must not contain the
        //    required native script (canonical versioned-script hash).
        let required_script_is_present = script_tx_wits_preimage
          |> list.any(
              fn(script_wit) {
                script_proof_v1.versioned_script_hash(script_wit) == expected_missing_script_hash
              },
            )
        expect required_script_is_present == False

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn run_finalize(expected_missing_script_hash: ByteArray) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let state = State {
    expected_missing_script_hash,
    verified_script_wits_hash: block.witness_set_compact.script_tx_wits_hash,
  }
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
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
        script_tx_wits_preimage: block.tx.witness_set.script_tx_wits,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// The canonical native script-witness preimage of the committed transaction
/// proves the required native script is absent, finalizing the proof.
test missing_native_script_tx_step_06_accepts_absent_script() {
  run_finalize(
    script_proof_v1.versioned_script_hash(
      MidgardVersionedScript {
        language: NativeCardanoScript,
        script_bytes: #"8201818200581c30303030303030303030303030303030303030303030",
      },
    ),
  )
}

/// Valid-block negative selector: the required native script is present in the
/// transaction's canonical script-witness collection, so the challenge cannot
/// finalize.
test missing_native_script_tx_step_06_rejects_valid_block_challenge() fail {
  run_finalize(
    script_proof_v1.versioned_script_hash(
      fixture.sample_native_script_witness(),
    ),
  )
}
