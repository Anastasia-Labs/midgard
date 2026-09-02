//// Q35 network-id step 01: counted-root binding and acceptance gate.

use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use env
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/chunked_inclusion_v1.{
  ChunkedProofClaim, Membership, PublishedProofCarriage,
  published_chunk_membership,
}
use midgard/fraud_proofs/common.{
  NativeTxInclusionCarriage, PublishedChunkMembership, RedeemerCarriedMembership,
  cancel, continue, pass_native_tx_to_next_step_carried,
  verify_membership_carried,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTransaction, MidgardTransactionBody, MidgardTxOutput,
  TxIsInvalid,
}
use midgard/fraud_proofs/network_id/rule.{is_supported_network_id_v1}
use midgard/fraud_proofs/network_id/step_01.{
  Args, Datum, ForcedDispatchV1, ForcedNetworkIdMismatch, Introduced,
  NetworkChanged, NetworkIdFaultV1, OutputNetwork, OutputNetworkUtxo,
  PostUtxoMembershipV1, PostUtxoStateV1, SpendRedeemer, TransactionNetwork,
}
use midgard/fraud_proofs/network_id/step_02 as step_02
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/hub_oracle as hub
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_output_v1
use midgard/ledger_state
use midgard/mpf_proof_v1
use midgard/state_queue

validator main(
  step_02_validator_script_hash: ScriptHash,
  forced_step_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  expected_network_id: Int,
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
        tx_inclusion,
        post_utxo_membership,
        forced_source,
        fault,
      }) -> {
        expect is_supported_network_id_v1(expected_network_id)
        when fault is {
          TransactionNetwork -> {
            expect Some(inclusion) = tx_inclusion
            expect None = post_utxo_membership
            expect None = forced_source
            pass_transaction_claim(
              step_02_validator_script_hash,
              computation_thread_token_policy_id,
              hub_oracle,
              expected_network_id,
              datum,
              inclusion,
              fault,
              own_out_ref,
              tx,
            )
          }
          OutputNetwork { output_index } -> {
            expect output_index >= 0
            expect Some(inclusion) = tx_inclusion
            expect None = post_utxo_membership
            expect None = forced_source
            pass_transaction_claim(
              step_02_validator_script_hash,
              computation_thread_token_policy_id,
              hub_oracle,
              expected_network_id,
              datum,
              inclusion,
              fault,
              own_out_ref,
              tx,
            )
          }
          OutputNetworkUtxo { .. } -> {
            expect None = tx_inclusion
            expect Some(membership) = post_utxo_membership
            expect None = forced_source
            pass_post_utxo_claim(
              step_02_validator_script_hash,
              computation_thread_token_policy_id,
              hub_oracle,
              expected_network_id,
              datum,
              membership,
              fault,
              own_out_ref,
              tx,
            )
          }
          ForcedNetworkIdMismatch -> {
            expect None = tx_inclusion
            expect None = post_utxo_membership
            expect Some(source) = forced_source
            pass_forced_claim(
              forced_step_validator_script_hash,
              computation_thread_token_policy_id,
              datum,
              source,
              own_out_ref,
              tx,
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

fn pass_forced_claim(
  forced_step_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Datum>,
  source: ForcedDispatchV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(step_datum) = datum
  let _, _, _, input_state, output_script_hash, output_state_data <- continue(
    computation_thread_token_policy_id,
    step_datum,
    source.input_index,
    source.output_index,
    own_out_ref,
    inputs,
    outputs,
  )
  let empty_state: Data = None
  and {
    input_state == None,
    output_script_hash == forced_step_validator_script_hash,
    output_state_data == empty_state,
  }
}

fn pass_transaction_claim(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  expected_network_id: Int,
  datum: Option<Datum>,
  tx_inclusion: NativeTxInclusionCarriage,
  fault: NetworkIdFaultV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let
      _own_script_hash,
      _computation_thread_token_asset_name,
      _fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data,
      _header,
      bad_tx_id,
      bad_tx_view
    <- pass_native_tx_to_next_step_carried(
    computation_thread_token_policy_id,
    hub_oracle,
    datum,
    tx_inclusion,
    own_out_ref,
    tx,
  )

  expect None = m_input_state_data
  // Network rules only affect an accepted transaction. A rejected leaf is an
  // honest no-op and must never produce a permanent proof token.
  expect bad_tx_view.tx_compact.validity_code == 0
  expect output_script_hash == step_02_validator_script_hash
  let expected_output_state: Data = step_02.State {
    bad_tx_id,
    committed_tx_network_id: bad_tx_view.tx_compact.body.network_id,
    expected_network_id,
    fault,
    post_utxo: None,
    forced_source_key: None,
  }
  expect output_state_data == expected_output_state
  True
}

fn pass_post_utxo_claim(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  expected_network_id: Int,
  datum: Option<Datum>,
  membership: PostUtxoMembershipV1,
  claimed_fault: NetworkIdFaultV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx
  expect Some(step_datum) = datum
  let
      _own_script_hash,
      computation_thread_token_asset_name,
      _fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data
    <- continue(
    computation_thread_token_policy_id,
    step_datum,
    membership.input_index,
    membership.output_index,
    own_out_ref,
    inputs,
    outputs,
  )
  expect None = m_input_state_data
  expect output_script_hash == step_02_validator_script_hash

  let hub.Datum { state_queue: state_queue_policy, .. } = hub.get_datum(
    reference_inputs,
    hub_oracle,
    membership.hub_ref_input_index,
  )
  let header, state_queue_node_key <- state_queue.get_block_datum_v1(
    reference_inputs,
    state_queue_policy,
    membership.state_queue_node_ref_input_index,
  )
  expect
    state_queue_node_key == bytearray.drop(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    )

  let descriptor = ledger_output_commitment_v1.decode(
    membership.descriptor_cbor,
  )
  expect descriptor.output_index == membership.out_ref.output_index
  expect
    verify_membership_carried(
      membership.membership,
      header.utxos_root,
      ledger_outref_key(membership.out_ref),
      membership.descriptor_cbor,
      reference_inputs,
      redeemers,
    )
  expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
    descriptor.address,
  )
  expect OutputNetworkUtxo { observed_network_id } = claimed_fault
  expect observed_network_id == address.network_id
  let expected_output_state: Data = step_02.State {
    bad_tx_id: membership.out_ref.transaction_id,
    committed_tx_network_id: 255,
    expected_network_id,
    fault: claimed_fault,
    post_utxo: Some(
      PostUtxoStateV1 {
        out_ref: membership.out_ref,
        descriptor_cbor: membership.descriptor_cbor,
        prev_utxos_root: header.prev_utxos_root,
        predecessor: membership.predecessor,
      },
    ),
    forced_source_key: None,
  }
  expect output_state_data == expected_output_state
  True
}

fn with_tx_network(network_id: Int) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id },
  }
}

fn block_at_utxos_roots(
  base: fixture.NativeBlockFixtureV1,
  prev_utxos_root: ByteArray,
  utxos_root: ByteArray,
) -> fixture.NativeBlockFixtureV1 {
  let header = ledger_state.HeaderV1 {
    ..base.header,
    prev_utxos_root,
    utxos_root,
  }
  let header_data: Data = header
  fixture.NativeBlockFixtureV1 {
    ..base,
    header,
    header_hash: utils.serialise_and_hash_28(header_data),
  }
}

fn post_utxo_fixture(
  network_id: Int,
) -> Pair<fixture.NativeBlockFixtureV1, PostUtxoMembershipV1> {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let address = output.address
  let claimed_output = MidgardTxOutput {
    ..output,
    address: MidgardAddress { ..address, network_id },
  }
  let output_cbor = encode_midgard_tx_output(claimed_output)
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let out_ref = OutputReference {
    transaction_id: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    output_index: 0,
  }
  let key = ledger_outref_key(out_ref)
  let root = mpf.root(mpf.insert(mpf.empty, key, descriptor_cbor, []))
  let block = block_at_utxos_roots(
    fixture.native_block_fixture_v1(base),
    env.empty_merkle_tree_root,
    root,
  )
  Pair(
    block,
    PostUtxoMembershipV1 {
      input_index: 0,
      output_index: 0,
      hub_ref_input_index: 0,
      state_queue_node_ref_input_index: 1,
      out_ref,
      descriptor_cbor,
      membership: RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 0,
      },
      predecessor: Introduced,
    },
  )
}

fn run_post_utxo_step_01(
  block: fixture.NativeBlockFixtureV1,
  membership: PostUtxoMembershipV1,
  observed_network_id: Int,
) -> Bool {
  run_post_utxo_step_01_with_withdrawal_claim(
    block,
    membership,
    observed_network_id,
    block.header.utxos_root,
    ledger_outref_key(membership.out_ref),
    membership.descriptor_cbor,
  )
}

fn run_post_utxo_step_01_with_withdrawal_claim(
  block: fixture.NativeBlockFixtureV1,
  membership: PostUtxoMembershipV1,
  observed_network_id: Int,
  withdrawal_root: ByteArray,
  withdrawal_key: ByteArray,
  withdrawal_value: ByteArray,
) -> Bool {
  let fault = OutputNetworkUtxo { observed_network_id }
  let expected_state = post_utxo_state_data(block, membership, fault)
  main.spend(
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    0,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        tx_inclusion: None,
        post_utxo_membership: Some(membership),
        forced_source: None,
        fault,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_l1_tx_v1(
      block,
      expected_state,
      withdrawal_root,
      withdrawal_key,
      withdrawal_value,
    ),
  )
}

fn post_utxo_state_data(
  block: fixture.NativeBlockFixtureV1,
  membership: PostUtxoMembershipV1,
  fault: NetworkIdFaultV1,
) -> Data {
  let state: Data = step_02.State {
    bad_tx_id: membership.out_ref.transaction_id,
    committed_tx_network_id: 255,
    expected_network_id: 0,
    fault,
    post_utxo: Some(
      PostUtxoStateV1 {
        out_ref: membership.out_ref,
        descriptor_cbor: membership.descriptor_cbor,
        prev_utxos_root: block.header.prev_utxos_root,
        predecessor: membership.predecessor,
      },
    ),
    forced_source_key: None,
  }
  state
}

fn run_step_01(
  bad_tx: MidgardTransaction,
  expected_network_id: Int,
  fault: NetworkIdFaultV1,
) -> Bool {
  let block = fixture.native_block_fixture_v1(bad_tx)
  let expected_state: Data = step_02.State {
    bad_tx_id: block.tx_id,
    committed_tx_network_id: block.compact.body.network_id,
    expected_network_id,
    fault,
    post_utxo: None,
    forced_source_key: None,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    expected_network_id,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        tx_inclusion: Some(fixture.native_inclusion_carriage_v1(block)),
        post_utxo_membership: None,
        forced_source: None,
        fault,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_l1_tx_v1(
      block,
      expected_state,
      block.transactions_phas_root,
      block.tx_id,
      block.source_cbor,
    ),
  )
}

test network_id_step_01_binds_an_accepted_transaction_claim() {
  run_step_01(with_tx_network(1), 0, TransactionNetwork)
}

test network_id_step_01_binds_an_accepted_output_claim() {
  run_step_01(with_tx_network(0), 0, OutputNetwork { output_index: 0 })
}

test network_id_step_01_rejects_an_honestly_rejected_transaction() fail {
  let bad = with_tx_network(1)
  run_step_01(
    MidgardTransaction { ..bad, validity: TxIsInvalid },
    0,
    TransactionNetwork,
  )
}

test network_id_step_01_rejects_an_invalid_deployment_network() fail {
  run_step_01(with_tx_network(1), 7, TransactionNetwork)
}

test network_id_step_01_rejects_a_negative_output_index() fail {
  run_step_01(with_tx_network(0), 0, OutputNetwork { output_index: -1 })
}

test network_id_step_01_authenticates_a_post_utxo_descriptor() {
  let Pair(block, membership) = post_utxo_fixture(2)
  run_post_utxo_step_01(block, membership, 2)
}

test network_id_step_01_rejects_a_forged_post_utxo_descriptor() fail {
  let Pair(block, membership) = post_utxo_fixture(2)
  run_post_utxo_step_01(
    block,
    PostUtxoMembershipV1 { ..membership, descriptor_cbor: #"80" },
    2,
  )
}

test network_id_step_01_rejects_a_forged_post_utxo_value() fail {
  let Pair(block, membership) = post_utxo_fixture(2)
  let Pair(_, other) = post_utxo_fixture(3)
  run_post_utxo_step_01_with_withdrawal_claim(
    block,
    PostUtxoMembershipV1 {
      ..membership,
      descriptor_cbor: other.descriptor_cbor,
    },
    3,
    block.header.utxos_root,
    ledger_outref_key(membership.out_ref),
    membership.descriptor_cbor,
  )
}

test network_id_step_01_rejects_a_forged_post_utxo_key() fail {
  let Pair(block, membership) = post_utxo_fixture(2)
  run_post_utxo_step_01_with_withdrawal_claim(
    block,
    PostUtxoMembershipV1 {
      ..membership,
      out_ref: OutputReference {
        transaction_id: #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
        output_index: 0,
      },
    },
    2,
    block.header.utxos_root,
    ledger_outref_key(membership.out_ref),
    membership.descriptor_cbor,
  )
}

test network_id_step_01_rejects_a_forged_post_utxo_root() fail {
  let Pair(block, membership) = post_utxo_fixture(2)
  let forged_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  run_post_utxo_step_01_with_withdrawal_claim(
    forged_block,
    membership,
    2,
    block.header.utxos_root,
    ledger_outref_key(membership.out_ref),
    membership.descriptor_cbor,
  )
}

test network_id_step_01_freezes_a_network_changing_predecessor_claim() {
  let Pair(post_block, post_membership) = post_utxo_fixture(2)
  let Pair(_, previous_membership) = post_utxo_fixture(0)
  let key = ledger_outref_key(post_membership.out_ref)
  let prev_root = mpf.root(
    mpf.insert(mpf.empty, key, previous_membership.descriptor_cbor, []),
  )
  run_post_utxo_step_01(
    block_at_utxos_roots(post_block, prev_root, post_block.header.utxos_root),
    PostUtxoMembershipV1 {
      ..post_membership,
      predecessor: NetworkChanged {
        previous_descriptor_cbor: previous_membership.descriptor_cbor,
      },
    },
    2,
  )
}

test network_id_step_01_authenticates_an_introduction_over_a_nonempty_predecessor() {
  let Pair(post_block, post_membership) = post_utxo_fixture(2)
  let (prev_root, _) = fixture.absence_witness()
  run_post_utxo_step_01(
    block_at_utxos_roots(post_block, prev_root, post_block.header.utxos_root),
    PostUtxoMembershipV1 { ..post_membership, predecessor: Introduced },
    2,
  )
}

test network_id_step_01_authenticates_a_maximum_64_step_published_post_membership() {
  let Pair(base_block, base_membership) = post_utxo_fixture(2)
  let key = ledger_outref_key(base_membership.out_ref)
  let steps = fixture.adversarial_branch_steps(64)
  let root = fixture.adversarial_membership_root(
    key,
    base_membership.descriptor_cbor,
    steps,
  )
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let indices = [2, 3, 4, 5]
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: indices,
  }
  let membership = PostUtxoMembershipV1 {
    ..base_membership,
    membership: PublishedChunkMembership(carriage),
  }
  let block = block_at_utxos_roots(
    base_block,
    base_block.header.prev_utxos_root,
    root,
  )
  let fault = OutputNetworkUtxo { observed_network_id: 2 }
  let claim = ChunkedProofClaim {
    mode: Membership,
    merkle_root: root,
    key_bytes: key,
    value_hash: blake2b_256(membership.descriptor_cbor),
    ordered_chunk_reference_input_indices: indices,
  }
  and {
    published_chunk_membership(
      fixture.continue_step_chunked_l1_tx_v1(
        block,
        post_utxo_state_data(block, membership, fault),
        chunks,
        claim,
      ).reference_inputs,
      carriage,
      root,
      key,
      membership.descriptor_cbor,
    ),
    main.spend(
      fixture.next_step_script_hash,
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.hub_oracle_script_hash,
      0,
      Some(fixture.step_datum_without_state()),
      ct.Continue(
        Args {
          tx_inclusion: None,
          post_utxo_membership: Some(membership),
          forced_source: None,
          fault,
        },
      ),
      fixture.own_out_ref(),
      fixture.continue_step_chunked_l1_tx_v1(
        block,
        post_utxo_state_data(block, membership, fault),
        chunks,
        claim,
      ),
    ),
  }
}

test network_id_step_01_rejects_reordered_published_post_chunks() fail {
  let Pair(base_block, base_membership) = post_utxo_fixture(2)
  let key = ledger_outref_key(base_membership.out_ref)
  let steps = fixture.adversarial_branch_steps(64)
  let root = fixture.adversarial_membership_root(
    key,
    base_membership.descriptor_cbor,
    steps,
  )
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let reordered = [2, 4, 3, 5]
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: reordered,
  }
  let membership = PostUtxoMembershipV1 {
    ..base_membership,
    membership: PublishedChunkMembership(carriage),
  }
  let block = block_at_utxos_roots(
    base_block,
    base_block.header.prev_utxos_root,
    root,
  )
  let fault = OutputNetworkUtxo { observed_network_id: 2 }
  let claim = ChunkedProofClaim {
    mode: Membership,
    merkle_root: root,
    key_bytes: key,
    value_hash: blake2b_256(membership.descriptor_cbor),
    ordered_chunk_reference_input_indices: reordered,
  }
  and {
    published_chunk_membership(
      fixture.continue_step_chunked_l1_tx_v1(
        block,
        post_utxo_state_data(block, membership, fault),
        chunks,
        claim,
      ).reference_inputs,
      carriage,
      root,
      key,
      membership.descriptor_cbor,
    ),
    main.spend(
      fixture.next_step_script_hash,
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.hub_oracle_script_hash,
      0,
      Some(fixture.step_datum_without_state()),
      ct.Continue(
        Args {
          tx_inclusion: None,
          post_utxo_membership: Some(membership),
          forced_source: None,
          fault,
        },
      ),
      fixture.own_out_ref(),
      fixture.continue_step_chunked_l1_tx_v1(
        block,
        post_utxo_state_data(block, membership, fault),
        chunks,
        claim,
      ),
    ),
  }
}

test network_id_step_01_rejects_a_cross_step_chunk_order_mismatch() fail {
  let Pair(base_block, base_membership) = post_utxo_fixture(2)
  let key = ledger_outref_key(base_membership.out_ref)
  let steps = fixture.adversarial_branch_steps(64)
  let root = fixture.adversarial_membership_root(
    key,
    base_membership.descriptor_cbor,
    steps,
  )
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: [2, 3, 4, 5],
  }
  let membership = PostUtxoMembershipV1 {
    ..base_membership,
    membership: PublishedChunkMembership(carriage),
  }
  let block = block_at_utxos_roots(
    base_block,
    base_block.header.prev_utxos_root,
    root,
  )
  let fault = OutputNetworkUtxo { observed_network_id: 2 }
  let mismatched_claim = ChunkedProofClaim {
    mode: Membership,
    merkle_root: root,
    key_bytes: key,
    value_hash: blake2b_256(membership.descriptor_cbor),
    ordered_chunk_reference_input_indices: [2, 4, 3, 5],
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    0,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        tx_inclusion: None,
        post_utxo_membership: Some(membership),
        forced_source: None,
        fault,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_chunked_l1_tx_v1(
      block,
      post_utxo_state_data(block, membership, fault),
      chunks,
      mismatched_claim,
    ),
  )
}
