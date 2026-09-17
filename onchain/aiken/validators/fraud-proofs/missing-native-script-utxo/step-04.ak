use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_native_script_utxo/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_native_script_utxo/step_05
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTxOutput, MidgardVersionedScript, NativeCardanoScript,
  ScriptCredential,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_output_v1
use midgard/script_proof_v1

validator main(
  step_05_validator_script_hash: ScriptHash,
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
        missing_native_script_bytes,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _own_script_hash,
            _thread_asset_name,
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
        expect
            Some(State {
              out_ref,
              descriptor_cbor,
              bad_tx_id,
              bad_tx_witness_set_hash,
            })
          = m_input_state_data
        let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
        expect descriptor.output_index == out_ref.output_index
        expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
          descriptor.address,
        )
        expect ScriptCredential(expected_missing_script_hash) = address.payment_credential
        expect
          expected_missing_script_hash == script_proof_v1.versioned_script_hash(
            MidgardVersionedScript {
              language: NativeCardanoScript,
              script_bytes: missing_native_script_bytes,
            },
          )
        expect output_script_hash == step_05_validator_script_hash
        let expected_output_state: Data = step_05.State {
          expected_missing_script_hash,
          bad_tx_id,
          bad_tx_witness_set_hash,
          phase: step_05.Ready,
        }
        expect output_state_data == expected_output_state
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn run_descriptor(
  script_locked: Bool,
  descriptor_index: Int,
  missing_native_script_bytes: ByteArray,
) -> Bool {
  let base_tx = fixture.valid_native_tx_v1()
  expect [base_output] = base_tx.body.outputs
  let expected_script_hash = script_proof_v1.versioned_script_hash(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: fixture.sample_native_script_bytes(),
    },
  )
  let payment_credential = if script_locked {
    ScriptCredential(expected_script_hash)
  } else {
    base_output.address.payment_credential
  }
  let output = MidgardTxOutput {
    ..base_output,
    address: MidgardAddress { ..base_output.address, payment_credential },
  }
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(
    descriptor_index,
    encode_midgard_tx_output(output),
  )
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let out_ref = OutputReference {
    transaction_id: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    output_index: 0,
  }
  let block = fixture.native_block_fixture_v1(base_tx)
  let input_state = State {
    out_ref,
    descriptor_cbor,
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
  }
  let output_state: Data = step_05.State {
    expected_missing_script_hash: expected_script_hash,
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
    phase: step_05.Ready,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      Args { input_index: 0, output_index: 0, missing_native_script_bytes },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output_state,
    ),
  )
}

test missing_native_script_utxo_step_04_authenticates_the_script_credential_preimage() {
  run_descriptor(True, 0, fixture.sample_native_script_bytes())
}

test missing_native_script_utxo_step_04_rejects_a_key_locked_output() fail {
  run_descriptor(False, 0, fixture.sample_native_script_bytes())
}

test missing_native_script_utxo_step_04_rejects_the_wrong_native_preimage() fail {
  run_descriptor(
    True,
    0,
    #"8200581c42424242424242424242424242424242424242424242424242424242",
  )
}

test missing_native_script_utxo_step_04_rejects_a_descriptor_index_mismatch() fail {
  run_descriptor(True, 1, fixture.sample_native_script_bytes())
}
