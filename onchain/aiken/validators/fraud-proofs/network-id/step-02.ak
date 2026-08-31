//// Q35 network-id step 02: decisive check and permanent proof-token mint.

use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, OutputReference, Redeemer, ScriptPurpose, Transaction,
}
use env
use midgard/computation_thread as ct
use midgard/fraud_proofs/chunked_inclusion_v1.{
  ChunkedProofClaim, Membership, NonMembership, PublishedProofCarriage,
  absent_value_hash, published_chunk_membership, published_chunk_non_membership,
}
use midgard/fraud_proofs/common.{
  PublishedChunkMembership, PublishedChunkNonMembership,
  RedeemerCarriedMembership, RedeemerCarriedNonMembership, cancel, finalize,
  verify_membership_carried, verify_non_membership_carried,
}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, outputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_output_cbor, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTransaction, MidgardTransactionBody, MidgardTxOutput,
}
use midgard/fraud_proofs/network_id/rule.{
  is_output_network_violation_v1, is_transaction_network_violation_v1,
}
use midgard/fraud_proofs/network_id/step_01.{
  Introduced, NetworkChanged, NetworkIdFaultV1, OutputNetwork, OutputNetworkUtxo,
  PostUtxoPredecessorClaimV1, PostUtxoStateV1, TransactionNetwork,
}
use midgard/fraud_proofs/network_id/step_02.{
  Args, Datum, IntroducedPredecessor, NetworkChangedPredecessor,
  PostUtxoPredecessorCarriageV1, SpendRedeemer, State,
}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_output_v1
use midgard/native_tx_field_access_v1.{field_item_at}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        outputs_opening,
        predecessor_carriage,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
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

        when state.fault is {
          TransactionNetwork -> {
            // Cardano permits the body field to be absent. Canonical native
            // V1 spells absence as 255; only an explicitly different id is a
            // TRANSACTION-NETWORK violation.
            expect None = outputs_opening
            expect None = predecessor_carriage
            expect None = state.post_utxo
            is_transaction_network_violation_v1(
              state.committed_tx_network_id,
              state.expected_network_id,
            )
          }
          OutputNetwork { output_index: bad_output_index } -> {
            expect Some(opening) = outputs_opening
            expect None = predecessor_carriage
            expect None = state.post_utxo
            let outputs_view = opened_field_view(
              opening,
              BodyAnchor { tx_id: state.bad_tx_id },
              outputs_field_index,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let bad_output = decode_midgard_tx_output_cbor(
              field_item_at(outputs_view, bad_output_index),
            )
            is_output_network_violation_v1(
              bad_output.address.network_id,
              state.expected_network_id,
            )
          }
          OutputNetworkUtxo { observed_network_id } -> {
            expect None = outputs_opening
            expect Some(post) = state.post_utxo
            expect Some(predecessor) = predecessor_carriage
            let key = ledger_outref_key(post.out_ref)
            when post.predecessor is {
              Introduced -> {
                expect IntroducedPredecessor(carriage) = predecessor
                expect
                  verify_non_membership_carried(
                    carriage,
                    post.prev_utxos_root,
                    key,
                    reference_inputs,
                    redeemers,
                  )
              }
              NetworkChanged { previous_descriptor_cbor } -> {
                expect NetworkChangedPredecessor(carriage) = predecessor
                expect previous_descriptor_cbor != post.descriptor_cbor
                expect
                  verify_membership_carried(
                    carriage,
                    post.prev_utxos_root,
                    key,
                    previous_descriptor_cbor,
                    reference_inputs,
                    redeemers,
                  )
                let previous = ledger_output_commitment_v1.decode(
                  previous_descriptor_cbor,
                )
                expect previous.output_index == post.out_ref.output_index
                expect Some(previous_address) = ledger_output_v1.decode_canonical_address_bytes(
                  previous.address,
                )
                expect previous_address.network_id == state.expected_network_id
              }
            }
            is_output_network_violation_v1(
              observed_network_id,
              state.expected_network_id,
            )
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn output_at_network(
  output: MidgardTxOutput,
  network_id: Int,
) -> MidgardTxOutput {
  let address = output.address
  MidgardTxOutput {
    ..output,
    address: MidgardAddress { ..address, network_id },
  }
}

fn protected_output_at_network(
  output: MidgardTxOutput,
  network_id: Int,
) -> MidgardTxOutput {
  let address = output.address
  MidgardTxOutput {
    ..output,
    address: MidgardAddress { ..address, protected: True, network_id },
  }
}

fn with_outputs(outputs: List<MidgardTxOutput>) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs },
  }
}

fn repeated_output(output: MidgardTxOutput, count: Int) -> List<MidgardTxOutput> {
  if count == 0 {
    []
  } else {
    [output, ..repeated_output(output, count - 1)]
  }
}

fn state_for(
  block: fixture.NativeBlockFixtureV1,
  fault: NetworkIdFaultV1,
) -> State {
  State {
    bad_tx_id: block.tx_id,
    committed_tx_network_id: block.compact.body.network_id,
    expected_network_id: 0,
    fault,
    post_utxo: None,
  }
}

fn run_finalize(
  block: fixture.NativeBlockFixtureV1,
  fault: NetworkIdFaultV1,
  carried_outputs: Option<List<MidgardTxOutput>>,
) -> Bool {
  let outputs_opening = when carried_outputs is {
    None -> None
    Some(items) ->
      Some(
        fixture.inline_body_opening(
          block.compact_cbor,
          items |> list.map(encode_midgard_tx_output),
        ),
      )
  }
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_for(block, fault)),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        outputs_opening,
        predecessor_carriage: None,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  )
}

fn post_utxo_material(
  network_id: Int,
) -> (OutputReference, ByteArray, ByteArray) {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let output_cbor = encode_midgard_tx_output(
    output_at_network(output, network_id),
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let out_ref = OutputReference {
    transaction_id: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    output_index: 0,
  }
  let root = mpf.root(
    mpf.insert(mpf.empty, ledger_outref_key(out_ref), descriptor_cbor, []),
  )
  (out_ref, descriptor_cbor, root)
}

fn run_post_finalize(
  observed_network_id: Int,
  post_descriptor_cbor: ByteArray,
  prev_utxos_root: ByteArray,
  predecessor_claim: PostUtxoPredecessorClaimV1,
  predecessor_carriage: PostUtxoPredecessorCarriageV1,
  withdrawal_redeemer: Pair<ScriptPurpose, Redeemer>,
) -> Bool {
  run_post_finalize_with_reference_inputs(
    observed_network_id,
    post_descriptor_cbor,
    prev_utxos_root,
    predecessor_claim,
    predecessor_carriage,
    withdrawal_redeemer,
    [],
  )
}

fn run_post_finalize_with_reference_inputs(
  observed_network_id: Int,
  post_descriptor_cbor: ByteArray,
  prev_utxos_root: ByteArray,
  predecessor_claim: PostUtxoPredecessorClaimV1,
  predecessor_carriage: PostUtxoPredecessorCarriageV1,
  withdrawal_redeemer: Pair<ScriptPurpose, Redeemer>,
  reference_inputs: List<Input>,
) -> Bool {
  let (out_ref, _, _) = post_utxo_material(observed_network_id)
  let fault = OutputNetworkUtxo { observed_network_id }
  let state = State {
    bad_tx_id: out_ref.transaction_id,
    committed_tx_network_id: 255,
    expected_network_id: 0,
    fault,
    post_utxo: Some(
      PostUtxoStateV1 {
        out_ref,
        descriptor_cbor: post_descriptor_cbor,
        prev_utxos_root,
        predecessor: predecessor_claim,
      },
    ),
  }
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  let base_tx = fixture.finalize_step_l1_tx_v1(asset_name)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
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
        fraud_proof_mint_redeemer_index: 0,
        outputs_opening: None,
        predecessor_carriage: Some(predecessor_carriage),
      },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..base_tx,
      reference_inputs,
      redeemers: list.concat(base_tx.redeemers, [withdrawal_redeemer]),
    },
  )
}

test network_id_step_02_convicts_an_explicit_transaction_network_mismatch() {
  let base = fixture.valid_native_tx_v1()
  let bad = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id: 1 },
  }
  run_finalize(fixture.native_block_fixture_v1(bad), TransactionNetwork, None)
}

test network_id_step_02_rejects_an_absent_transaction_network() fail {
  let base = fixture.valid_native_tx_v1()
  let absent = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id: 255 },
  }
  run_finalize(
    fixture.native_block_fixture_v1(absent),
    TransactionNetwork,
    None,
  )
}

test network_id_step_02_rejects_a_matching_transaction_network() fail {
  run_finalize(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    TransactionNetwork,
    None,
  )
}

test network_id_step_02_convicts_a_wrong_output_network() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let bad_outputs = [output_at_network(output, 1)]
  let block = fixture.native_block_fixture_v1(with_outputs(bad_outputs))
  run_finalize(block, OutputNetwork { output_index: 0 }, Some(bad_outputs))
}

test network_id_step_02_convicts_a_protected_wrong_network_output() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let bad_outputs = [protected_output_at_network(output, 1)]
  let block = fixture.native_block_fixture_v1(with_outputs(bad_outputs))
  run_finalize(block, OutputNetwork { output_index: 0 }, Some(bad_outputs))
}

test network_id_step_02_convicts_foreign_raw_network_nibble_2() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let bad_outputs = [output_at_network(output, 2)]
  let block = fixture.native_block_fixture_v1(with_outputs(bad_outputs))
  run_finalize(block, OutputNetwork { output_index: 0 }, Some(bad_outputs))
}

test network_id_step_02_convicts_foreign_raw_network_nibble_15() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  // Midgard reserves raw nibble bit 3 as protection, so raw 15 decodes as a
  // protected address on logical foreign network 7.
  let bad_outputs = [protected_output_at_network(output, 7)]
  let block = fixture.native_block_fixture_v1(with_outputs(bad_outputs))
  run_finalize(block, OutputNetwork { output_index: 0 }, Some(bad_outputs))
}

test network_id_step_02_convicts_an_authenticated_post_utxo_network() {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let key = ledger_outref_key(out_ref)
  run_post_finalize(
    2,
    post_descriptor,
    env.empty_merkle_tree_root,
    Introduced,
    IntroducedPredecessor(
      RedeemerCarriedNonMembership {
        non_membership_proof: [],
        non_membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.pexcludes_withdraw_redeemer_entry(
      env.empty_merkle_tree_root,
      key,
      [],
    ),
  )
}

test network_id_step_02_convicts_an_introduction_over_a_nonempty_predecessor() {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let key = ledger_outref_key(out_ref)
  let (prev_root, absence_proof) = fixture.absence_witness()
  run_post_finalize(
    2,
    post_descriptor,
    prev_root,
    Introduced,
    IntroducedPredecessor(
      RedeemerCarriedNonMembership {
        non_membership_proof: absence_proof,
        non_membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.pexcludes_withdraw_redeemer_entry(prev_root, key, absence_proof),
  )
}

test network_id_step_02_convicts_an_expected_to_foreign_network_mutation() {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let (_, previous_descriptor, prev_root) = post_utxo_material(0)
  let key = ledger_outref_key(out_ref)
  run_post_finalize(
    2,
    post_descriptor,
    prev_root,
    NetworkChanged { previous_descriptor_cbor: previous_descriptor },
    NetworkChangedPredecessor(
      RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.phas_withdraw_redeemer_entry(prev_root, key, previous_descriptor),
  )
}

test network_id_step_02_rejects_an_inherited_wrong_network_utxo() fail {
  let (out_ref, post_descriptor, post_root) = post_utxo_material(2)
  let key = ledger_outref_key(out_ref)
  run_post_finalize(
    2,
    post_descriptor,
    post_root,
    NetworkChanged { previous_descriptor_cbor: post_descriptor },
    NetworkChangedPredecessor(
      RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.phas_withdraw_redeemer_entry(post_root, key, post_descriptor),
  )
}

test network_id_step_02_rejects_a_foreign_to_foreign_network_mutation() fail {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let (_, previous_descriptor, prev_root) = post_utxo_material(3)
  let key = ledger_outref_key(out_ref)
  run_post_finalize(
    2,
    post_descriptor,
    prev_root,
    NetworkChanged { previous_descriptor_cbor: previous_descriptor },
    NetworkChangedPredecessor(
      RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.phas_withdraw_redeemer_entry(prev_root, key, previous_descriptor),
  )
}

test network_id_step_02_rejects_a_forged_predecessor_root() fail {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let (_, previous_descriptor, prev_root) = post_utxo_material(0)
  run_post_finalize(
    2,
    post_descriptor,
    prev_root,
    NetworkChanged { previous_descriptor_cbor: previous_descriptor },
    NetworkChangedPredecessor(
      RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.phas_withdraw_redeemer_entry(
      env.empty_merkle_tree_root,
      ledger_outref_key(out_ref),
      previous_descriptor,
    ),
  )
}

test network_id_step_02_rejects_a_forged_predecessor_key() fail {
  let (_, post_descriptor, _) = post_utxo_material(2)
  let (_, previous_descriptor, prev_root) = post_utxo_material(0)
  let forged_out_ref = OutputReference {
    transaction_id: #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
    output_index: 0,
  }
  run_post_finalize(
    2,
    post_descriptor,
    prev_root,
    NetworkChanged { previous_descriptor_cbor: previous_descriptor },
    NetworkChangedPredecessor(
      RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.phas_withdraw_redeemer_entry(
      prev_root,
      ledger_outref_key(forged_out_ref),
      previous_descriptor,
    ),
  )
}

test network_id_step_02_rejects_a_forged_predecessor_value() fail {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let (_, previous_descriptor, prev_root) = post_utxo_material(0)
  let (_, forged_descriptor, _) = post_utxo_material(1)
  run_post_finalize(
    2,
    post_descriptor,
    prev_root,
    NetworkChanged { previous_descriptor_cbor: previous_descriptor },
    NetworkChangedPredecessor(
      RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.phas_withdraw_redeemer_entry(
      prev_root,
      ledger_outref_key(out_ref),
      forged_descriptor,
    ),
  )
}

test network_id_step_02_rejects_a_forged_predecessor_proof() fail {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let (_, previous_descriptor, prev_root) = post_utxo_material(0)
  let forged_proof = [
    mpf.Leaf {
      skip: 0,
      key: #"1111111111111111111111111111111111111111111111111111111111111111",
      value: #"2222222222222222222222222222222222222222222222222222222222222222",
    },
  ]
  run_post_finalize(
    2,
    post_descriptor,
    prev_root,
    NetworkChanged { previous_descriptor_cbor: previous_descriptor },
    NetworkChangedPredecessor(
      RedeemerCarriedMembership {
        membership_proof: forged_proof,
        membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.phas_withdraw_redeemer_entry(
      prev_root,
      ledger_outref_key(out_ref),
      previous_descriptor,
    ),
  )
}

test network_id_step_02_rejects_a_cross_step_predecessor_kind_swap() fail {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let (_, previous_descriptor, prev_root) = post_utxo_material(0)
  run_post_finalize(
    2,
    post_descriptor,
    prev_root,
    Introduced,
    NetworkChangedPredecessor(
      RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.phas_withdraw_redeemer_entry(
      prev_root,
      ledger_outref_key(out_ref),
      previous_descriptor,
    ),
  )
}

test network_id_step_02_authenticates_a_maximum_64_step_published_predecessor_membership() {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let (_, previous_descriptor, _) = post_utxo_material(0)
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_branch_steps(64)
  let prev_root = fixture.adversarial_membership_root(
    key,
    previous_descriptor,
    steps,
  )
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let indices = [0, 1, 2, 3]
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: indices,
  }
  let claim = ChunkedProofClaim {
    mode: Membership,
    merkle_root: prev_root,
    key_bytes: key,
    value_hash: blake2b_256(previous_descriptor),
    ordered_chunk_reference_input_indices: indices,
  }
  and {
    published_chunk_membership(
      chunks,
      carriage,
      prev_root,
      key,
      previous_descriptor,
    ),
    run_post_finalize_with_reference_inputs(
      2,
      post_descriptor,
      prev_root,
      NetworkChanged { previous_descriptor_cbor: previous_descriptor },
      NetworkChangedPredecessor(PublishedChunkMembership(carriage)),
      fixture.chunked_verify_withdraw_redeemer_entry(claim),
      chunks,
    ),
  }
}

test network_id_step_02_authenticates_a_maximum_64_step_published_predecessor_non_membership() {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_non_membership_steps(63)
  let prev_root = fixture.adversarial_non_membership_root(key, steps)
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let indices = [0, 1, 2, 3]
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: indices,
  }
  let claim = ChunkedProofClaim {
    mode: NonMembership,
    merkle_root: prev_root,
    key_bytes: key,
    value_hash: absent_value_hash,
    ordered_chunk_reference_input_indices: indices,
  }
  and {
    published_chunk_non_membership(chunks, carriage, prev_root, key),
    run_post_finalize_with_reference_inputs(
      2,
      post_descriptor,
      prev_root,
      Introduced,
      IntroducedPredecessor(PublishedChunkNonMembership(carriage)),
      fixture.chunked_verify_withdraw_redeemer_entry(claim),
      chunks,
    ),
  }
}

test network_id_step_02_rejects_reordered_published_predecessor_chunks() fail {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let (_, previous_descriptor, _) = post_utxo_material(0)
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_branch_steps(64)
  let prev_root = fixture.adversarial_membership_root(
    key,
    previous_descriptor,
    steps,
  )
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let reordered = [0, 2, 1, 3]
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: reordered,
  }
  let claim = ChunkedProofClaim {
    mode: Membership,
    merkle_root: prev_root,
    key_bytes: key,
    value_hash: blake2b_256(previous_descriptor),
    ordered_chunk_reference_input_indices: reordered,
  }
  and {
    published_chunk_membership(
      chunks,
      carriage,
      prev_root,
      key,
      previous_descriptor,
    ),
    run_post_finalize_with_reference_inputs(
      2,
      post_descriptor,
      prev_root,
      NetworkChanged { previous_descriptor_cbor: previous_descriptor },
      NetworkChangedPredecessor(PublishedChunkMembership(carriage)),
      fixture.chunked_verify_withdraw_redeemer_entry(claim),
      chunks,
    ),
  }
}

test network_id_step_02_rejects_a_cross_step_predecessor_chunk_order_mismatch() fail {
  let (out_ref, post_descriptor, _) = post_utxo_material(2)
  let (_, previous_descriptor, _) = post_utxo_material(0)
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_branch_steps(64)
  let prev_root = fixture.adversarial_membership_root(
    key,
    previous_descriptor,
    steps,
  )
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: [0, 1, 2, 3],
  }
  let mismatched_claim = ChunkedProofClaim {
    mode: Membership,
    merkle_root: prev_root,
    key_bytes: key,
    value_hash: blake2b_256(previous_descriptor),
    ordered_chunk_reference_input_indices: [0, 2, 1, 3],
  }
  run_post_finalize_with_reference_inputs(
    2,
    post_descriptor,
    prev_root,
    NetworkChanged { previous_descriptor_cbor: previous_descriptor },
    NetworkChangedPredecessor(PublishedChunkMembership(carriage)),
    fixture.chunked_verify_withdraw_redeemer_entry(mismatched_claim),
    chunks,
  )
}

/// Exercises a non-trivial high-cardinality opening. This is intentionally not
/// labelled the canonical maximum: Q38 owns the exact maximum-output frontier
/// and its required tiered-carriage proof-fit evidence.
test network_id_step_02_convicts_the_last_high_cardinality_output() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let honest = output_at_network(output, 0)
  let bad = output_at_network(output, 1)
  let max_outputs = list.concat(repeated_output(honest, 11), [bad])
  let block = fixture.native_block_fixture_v1(with_outputs(max_outputs))
  run_finalize(block, OutputNetwork { output_index: 11 }, Some(max_outputs))
}

test network_id_step_02_rejects_a_matching_output_network() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize(
    block,
    OutputNetwork { output_index: 0 },
    Some(block.tx.body.outputs),
  )
}

test network_id_step_02_rejects_a_protected_matching_network_output() fail {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let protected_outputs = [protected_output_at_network(output, 0)]
  let block = fixture.native_block_fixture_v1(with_outputs(protected_outputs))
  run_finalize(
    block,
    OutputNetwork { output_index: 0 },
    Some(protected_outputs),
  )
}

test network_id_step_02_rejects_a_matching_post_utxo_network() fail {
  let (out_ref, post_descriptor, _) = post_utxo_material(0)
  let key = ledger_outref_key(out_ref)
  run_post_finalize(
    0,
    post_descriptor,
    env.empty_merkle_tree_root,
    Introduced,
    IntroducedPredecessor(
      RedeemerCarriedNonMembership {
        non_membership_proof: [],
        non_membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.pexcludes_withdraw_redeemer_entry(
      env.empty_merkle_tree_root,
      key,
      [],
    ),
  )
}

test network_id_step_02_rejects_an_out_of_range_output_index() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize(
    block,
    OutputNetwork { output_index: 12 },
    Some(block.tx.body.outputs),
  )
}

test network_id_step_02_rejects_a_forged_outputs_preimage() fail {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let block = fixture.native_block_fixture_v1(base)
  run_finalize(
    block,
    OutputNetwork { output_index: 0 },
    Some([output_at_network(output, 1)]),
  )
}

test network_id_step_02_rejects_unused_output_evidence_for_tx_claim() fail {
  let base = fixture.valid_native_tx_v1()
  let bad = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id: 1 },
  }
  let block = fixture.native_block_fixture_v1(bad)
  run_finalize(block, TransactionNetwork, Some(block.tx.body.outputs))
}
