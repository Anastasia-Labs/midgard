use aiken/crypto.{verify_ed25519_signature}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, WitnessAnchor, address_witnesses_field_index,
  opened_field_view,
}
use midgard/fraud_proofs/missing_signature/forced_witness.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, encode_midgard_address_witness,
}
use midgard/native_tx_field_access_v1.{field_item_at}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  field_preimage_certificate_policy_id: PolicyId,
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
        addr_tx_wits_opening,
        witness_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx

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

        expect Some(state) = m_input_state_data
        when state.required_signer_hash is {
          None -> {
            expect None = addr_tx_wits_opening
            expect witness_index == 0
          }
          Some(required_signer_hash) -> {
            expect Some(opening) = addr_tx_wits_opening
            let view = opened_field_view(
              opening,
              WitnessAnchor {
                tx_id: state.verified_tx_id,
                witness_set_hash: state.verified_witness_set_hash,
              },
              address_witnesses_field_index,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let witness = decode_midgard_address_witness_cbor(
              field_item_at(view, witness_index),
            )
            expect
              utils.get_verification_key_hash(witness.verification_key) == required_signer_hash
            expect
              verify_ed25519_signature(
                witness.verification_key,
                state.verified_tx_id,
                witness.signature,
              )
          }
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

fn run_witness(
  required_signer_hash: Option<ByteArray>,
  opening: Option<FieldOpeningV1>,
  witness_index: Int,
) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            verified_tx_id: block.tx_id,
            verified_witness_set_hash: block.compact.witness_set_hash,
            forced_source_key: #"80",
            signer_index: 1,
            required_signer_hash,
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        addr_tx_wits_opening: opening,
        witness_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  )
}

test missing_signature_forced_witness_finalizes_an_impossible_coordinate() {
  run_witness(None, None, 0)
}

test missing_signature_forced_witness_rejects_a_missing_opening() fail {
  run_witness(Some(#""), None, 0)
}

test missing_signature_forced_witness_rejects_a_present_but_forged_signature() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_witness(
    Some(
      utils.get_verification_key_hash(fixture.sample_witness_verification_key()),
    ),
    Some(
      fixture.inline_witness_opening(
        block.compact_cbor,
        block.witness_set_compact,
        [encode_midgard_address_witness(fixture.sample_address_witness())],
      ),
    ),
    0,
  )
}
