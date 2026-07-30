use aiken/collection/list
use aiken/crypto.{verify_ed25519_signature}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_collection_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/invalid_signature/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_address_witness}

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
        addr_tx_wits_preimage,
        bad_addr_tx_wit_index,
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

        expect Some(State { bad_tx_id, bad_addr_tx_wits_hash }) = m_input_state_data

        // 2. Provided address-witness preimage must match the canonical native
        //    collection hash carried from the previous step (consensus field 7,
        //    list of canonically encoded key/signature array items).
        expect
          bounded_collection_v1.from_items(
            7,
            list.map(addr_tx_wits_preimage, encode_midgard_address_witness),
          ) == bad_addr_tx_wits_hash

        // 3. The referenced witness's signature over the native transaction ID
        //    must be invalid.
        let bad_addr_tx_wit = addr_tx_wits_preimage
          |> list.expect_at(bad_addr_tx_wit_index)
        expect
          verify_ed25519_signature(
            bad_addr_tx_wit.verification_key,
            bad_tx_id,
            bad_addr_tx_wit.signature,
          ) == False

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

/// The canonical native address-witness preimage of the committed transaction
/// reveals an invalid signature over the native transaction ID, finalizing
/// the proof.
test invalid_signature_step_02_accepts_invalid_signature_witness() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let state = State {
    bad_tx_id: block.tx_id,
    bad_addr_tx_wits_hash: block.witness_set_compact.addr_tx_wits_hash,
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
        addr_tx_wits_preimage: block.tx.witness_set.addr_tx_wits,
        bad_addr_tx_wit_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// A preimage that does not hash to the carried native collection hash (for
/// example the legacy `Pairs`-map encoding shape) cannot be used as witness
/// evidence.
test invalid_signature_step_02_rejects_mismatched_witness_preimage() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let state = State {
    bad_tx_id: block.tx_id,
    bad_addr_tx_wits_hash: block.witness_set_compact.addr_tx_wits_hash,
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
        addr_tx_wits_preimage: [],
        bad_addr_tx_wit_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}
