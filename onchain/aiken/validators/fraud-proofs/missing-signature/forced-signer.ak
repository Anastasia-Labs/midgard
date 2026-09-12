use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, required_signers_field_index,
}
use midgard/fraud_proofs/missing_signature/forced_signer.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_signature/forced_witness
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/native_tx_field_access_v1.{field_item_at, field_item_count}

validator main(
  forced_witness_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
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
      ct.Continue(Args { input_index, output_index, required_signers_opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )

        expect Some(state) = m_input_state_data
        // 2. Fetch the required signer hash out of field 4's authenticated
        //    preimage through the §8.8 door. §5.3 fixes the item at a raw
        //    28-byte hash, so signer `n` is one multiplication and one slice —
        //    no reproduction of the signer list, and no re-hash of it.
        let required_signers_view = opened_field_view(
          required_signers_opening,
          BodyAnchor { tx_id: state.verified_tx_id, source_kind: 1 },
          required_signers_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        // An impossible reason coordinate is itself a contradiction. The
        // authenticated count comes from the same grammar-certified field.
        let required_signer_hash = if state.signer_index < 0 || state.signer_index >= field_item_count(
          required_signers_view,
        ) {
          None
        } else {
          Some(field_item_at(required_signers_view, state.signer_index))
        }
        expect output_script_hash == forced_witness_validator_script_hash
        let expected_output_state: Data = forced_witness.State {
          verified_tx_id: state.verified_tx_id,
          verified_witness_set_hash: state.verified_witness_set_hash,
          forced_source_key: state.forced_source_key,
          signer_index: state.signer_index,
          required_signer_hash,
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

fn run_signer(index: Int, expected_hash: Option<ByteArray>) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let state = State {
    verified_tx_id: block.tx_id,
    verified_witness_set_hash: block.compact.witness_set_hash,
    forced_source_key: #"80",
    signer_index: index,
  }
  let output: Data = forced_witness.State {
    verified_tx_id: state.verified_tx_id,
    verified_witness_set_hash: state.verified_witness_set_hash,
    forced_source_key: state.forced_source_key,
    signer_index: state.signer_index,
    required_signer_hash: expected_hash,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
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
        required_signers_opening: fixture.inline_body_opening(
          forced_fixture.from_native_compact(block.compact_cbor),
          block.tx.body.required_signers,
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output,
    ),
  )
}

test missing_signature_forced_signer_opens_exact_coordinate() {
  run_signer(
    0,
    Some(
      utils.get_verification_key_hash(fixture.sample_witness_verification_key()),
    ),
  )
}

test missing_signature_forced_signer_proves_absent_coordinate() {
  run_signer(1, None)
}

test missing_signature_forced_signer_proves_negative_coordinate() {
  run_signer(-1, None)
}

test missing_signature_forced_signer_rejects_forged_hash() fail {
  run_signer(0, Some(#""))
}

test missing_signature_forced_signer_rejects_missing_hash_at_valid_coordinate() fail {
  run_signer(0, None)
}
