use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/chunked_inclusion_v1.{
  ChunkedProofClaim, Membership, PublishedProofCarriage,
  published_chunk_membership,
}
use midgard/fraud_proofs/common.{
  PublishedChunkMembership, RedeemerCarriedMembership, cancel, continue,
  verify_membership_carried,
}
use midgard/fraud_proofs/missing_native_script_utxo/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_native_script_utxo/step_04
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTxInput, MidgardTxOutput, ScriptCredential,
}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1

validator main(
  step_04_validator_script_hash: ScriptHash,
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
        out_ref,
        descriptor_cbor,
        membership,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx
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
              input_with_missing_script,
              bad_tx_id,
              bad_tx_witness_set_hash,
              prev_utxos_root,
            })
          = m_input_state_data
        expect out_ref.transaction_id == input_with_missing_script.tx_id
        expect out_ref.output_index == input_with_missing_script.output_index
        expect
          verify_membership_carried(
            membership,
            prev_utxos_root,
            ledger_outref_key(out_ref),
            descriptor_cbor,
            reference_inputs,
            redeemers,
          )
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          out_ref,
          descriptor_cbor,
          bad_tx_id,
          bad_tx_witness_set_hash,
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

fn run_membership(
  script_locked: Bool,
  descriptor_cbor_override: Option<ByteArray>,
) -> Bool {
  let base_tx = fixture.valid_native_tx_v1()
  expect [base_output] = base_tx.body.outputs
  let native_hash = #"44444444444444444444444444444444444444444444444444444444"
  let payment_credential = if script_locked {
    ScriptCredential(native_hash)
  } else {
    base_output.address.payment_credential
  }
  let output = MidgardTxOutput {
    ..base_output,
    address: MidgardAddress { ..base_output.address, payment_credential },
  }
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(
    0,
    encode_midgard_tx_output(output),
  )
  let honest_descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let descriptor_cbor = when descriptor_cbor_override is {
    None -> honest_descriptor_cbor
    Some(value) -> value
  }
  let out_ref = OutputReference {
    transaction_id: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    output_index: 0,
  }
  let key = ledger_outref_key(out_ref)
  let root = mpf.root(mpf.insert(mpf.empty, key, honest_descriptor_cbor, []))
  let block = fixture.native_block_fixture_v1(base_tx)
  let input_state = State {
    input_with_missing_script: MidgardTxInput {
      tx_id: out_ref.transaction_id,
      output_index: out_ref.output_index,
    },
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
    prev_utxos_root: root,
  }
  let expected_output_state: Data = step_04.State {
    out_ref,
    descriptor_cbor,
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
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
      Args {
        input_index: 0,
        output_index: 0,
        out_ref,
        descriptor_cbor,
        membership: RedeemerCarriedMembership {
          membership_proof: [],
          membership_proof_script_redeemer_index: 0,
        },
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_l1_tx_v1(
      block,
      expected_output_state,
      root,
      key,
      honest_descriptor_cbor,
    ),
  )
}

test missing_native_script_utxo_step_03_authenticates_the_prior_script_output() {
  run_membership(True, None)
}

test missing_native_script_utxo_step_03_rejects_a_forged_descriptor() fail {
  run_membership(True, Some(#"80"))
}

fn run_published_membership(indices: List<Int>) -> Bool {
  let base_tx = fixture.valid_native_tx_v1()
  expect [base_output] = base_tx.body.outputs
  let native_hash = #"44444444444444444444444444444444444444444444444444444444"
  let output = MidgardTxOutput {
    ..base_output,
    address: MidgardAddress {
      ..base_output.address,
      payment_credential: ScriptCredential(native_hash),
    },
  }
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(
    0,
    encode_midgard_tx_output(output),
  )
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let out_ref = OutputReference {
    transaction_id: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    output_index: 0,
  }
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_branch_steps(64)
  let root = fixture.adversarial_membership_root(key, descriptor_cbor, steps)
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: indices,
  }
  let block = fixture.native_block_fixture_v1(base_tx)
  let input_state = State {
    input_with_missing_script: MidgardTxInput {
      tx_id: out_ref.transaction_id,
      output_index: out_ref.output_index,
    },
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
    prev_utxos_root: root,
  }
  let expected_output_state: Data = step_04.State {
    out_ref,
    descriptor_cbor,
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
  }
  let claim = ChunkedProofClaim {
    mode: Membership,
    merkle_root: root,
    key_bytes: key,
    value_hash: blake2b_256(descriptor_cbor),
    ordered_chunk_reference_input_indices: indices,
  }
  let l1_tx = fixture.continue_step_chunked_l1_tx_v1(
    block,
    expected_output_state,
    chunks,
    claim,
  )
  and {
    published_chunk_membership(
      l1_tx.reference_inputs,
      carriage,
      root,
      key,
      descriptor_cbor,
    ),
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
        Args {
          input_index: 0,
          output_index: 0,
          out_ref,
          descriptor_cbor,
          membership: PublishedChunkMembership(carriage),
        },
      ),
      fixture.own_out_ref(),
      l1_tx,
    ),
  }
}

test missing_native_script_utxo_step_03_authenticates_a_maximum_64_step_published_membership() {
  run_published_membership([2, 3, 4, 5])
}

test missing_native_script_utxo_step_03_rejects_reordered_published_chunks() fail {
  run_published_membership([2, 4, 3, 5])
}
