use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_collection_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/missing_signature/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_address_witness}

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
        addr_tx_wits_preimage,
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
              missing_required_signer_vkey,
              verified_addr_tx_wits_hash,
            })
          = m_input_state_data

        // 2. Address-witness preimage must match the canonical native
        //    collection hash carried from the previous steps (consensus
        //    field 7).
        expect
          bounded_collection_v1.from_items(
            7,
            list.map(addr_tx_wits_preimage, encode_midgard_address_witness),
          ) == verified_addr_tx_wits_hash

        // 3. Required signer's verification key must not be in the
        //    address-witness collection.
        let required_signature_is_present = addr_tx_wits_preimage
          |> list.any(
              fn(addr_tx_wit) {
                addr_tx_wit.verification_key == missing_required_signer_vkey
              },
            )
        expect required_signature_is_present == False

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn run_finalize(missing_required_signer_vkey: ByteArray) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let state = State {
    missing_required_signer_vkey,
    verified_addr_tx_wits_hash: block.witness_set_compact.addr_tx_wits_hash,
  }
  let asset_name = fixture.ct_token_asset_name(block)
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
        addr_tx_wits_preimage: block.tx.witness_set.addr_tx_wits,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// The canonical native address-witness preimage of the committed transaction
/// proves the required signer's key is absent, finalizing the proof.
test missing_signature_step_04_accepts_absent_signer() {
  run_finalize(
    #"7070707070707070707070707070707070707070707070707070707070707070",
  )
}

/// Valid-block negative selector: the required signer's key is present in the
/// transaction's canonical address-witness collection, so the challenge
/// cannot finalize.
test missing_signature_step_04_rejects_valid_block_challenge() fail {
  run_finalize(fixture.sample_witness_verification_key())
}
