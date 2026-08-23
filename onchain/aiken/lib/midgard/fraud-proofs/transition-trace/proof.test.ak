use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry.{Leaf}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, OutputReference}
use env
use midgard/common/utils
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/components as native_components
use midgard/fraud_proofs/native_tx/preimages as native_preimages
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/transition_trace/proof
use midgard/hub_oracle
use midgard/ledger_output_descriptor_v1
use midgard/ledger_state
use midgard/native_tx_field_access_v1
use midgard/transition_trace
use midgard/user_events
use midgard/user_events/deposit
use midgard/user_events/tx_order_v1 as tx_order
use midgard/validation_claim_v1
use midgard/validation_machine_v1
use midgard/validation_trace_v1

const h28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h28_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h32_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const h32_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const h32_c =
  #"3333333333333333333333333333333333333333333333333333333333333333"

const h64_a =
  #"44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444"

const fraud_category_id = #"00000004"

const zero_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const validation_context_cbor =
  #"9f01546d6964676172642d636f6e73656e7375732d76311400000000ff"

fn out_ref(ix: Int) -> OutputReference {
  OutputReference { transaction_id: h32_a, output_index: ix }
}

fn dummy_address() -> Address {
  address.from_script(h28_a)
}

fn dummy_hub_datum() -> hub_oracle.Datum {
  hub_oracle.Datum {
    registered_operators: h28_a,
    active_operators: h28_a,
    retired_operators: h28_a,
    scheduler: h28_a,
    state_queue: h28_a,
    fraud_proof_catalogue: h28_a,
    fraud_proof: h28_a,
    deposit: h28_a,
    withdrawal: h28_a,
    tx_order: h28_a,
    settlement: h28_a,
    payout: h28_a,
    registered_operators_addr: dummy_address(),
    active_operators_addr: dummy_address(),
    retired_operators_addr: dummy_address(),
    scheduler_addr: dummy_address(),
    state_queue_addr: dummy_address(),
    fraud_proof_catalogue_addr: dummy_address(),
    fraud_proof_addr: dummy_address(),
    deposit_addr: dummy_address(),
    withdrawal_addr: dummy_address(),
    tx_order_addr: dummy_address(),
    settlement_addr: dummy_address(),
    reserve_addr: dummy_address(),
    payout_addr: dummy_address(),
    reserve_observer: h28_a,
  }
}

fn base_header() -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    prev_utxos_root: h32_a,
    utxos_root: h32_b,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 10,
    end_time: 20,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: h28_b,
    operator_vkey: h28_a,
    protocol_version: ledger_state.protocol_version_v1,
  }
}

fn traced_header(
  trace_root: ByteArray,
  event_to_step_root: ByteArray,
) -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    ..base_header(),
    transition_trace_root: trace_root,
    event_to_step_root,
    withdrawal_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }
}

fn prove(
  header: ledger_state.HeaderV1,
  fault: proof.TransitionFault,
) -> proof.TransitionFaultProof {
  let header_data: Data = header
  proof.TransitionFaultProof {
    challenged_header_hash: utils.serialise_and_hash_28(header_data),
    header,
    fault,
  }
}

fn asset_name_for(header: ledger_state.HeaderV1) -> ByteArray {
  let header_data: Data = header
  fraud_category_id
    |> bytearray.concat(utils.serialise_and_hash_28(header_data))
}

fn trace_membership(
  transition_step: ledger_state.TransitionStep,
) -> transition_trace.IndexedTraceProof {
  let key_bytes = cbor.serialise(transition_step.step_index)
  let value_bytes = cbor.serialise(transition_step)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.TransitionTraceRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.TransitionTraceRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: transition_step.step_index,
    value: transition_step,
    proof: [],
  }
}

fn empty_non_membership(
  domain: transition_trace.RootDomain,
  key: key,
) -> transition_trace.RootNonMembershipProof<key> {
  transition_trace.RootNonMembershipProof {
    domain,
    root: env.empty_merkle_tree_root,
    phas_root: env.empty_merkle_tree_root,
    count: 0,
    key,
    proof: [],
  }
}

fn step(
  ix: Int,
  event_key: ledger_state.EventKey,
  phase: ledger_state.TransitionPhase,
  pre: ByteArray,
  post: ByteArray,
) -> ledger_state.TransitionStep {
  ledger_state.TransitionStep {
    schema_version: 1,
    step_index: ix,
    event_key,
    phase,
    pre_utxos_root: pre,
    post_utxos_root: post,
  }
}

fn withdrawal_event_key(ix: Int) -> ledger_state.EventKey {
  ledger_state.WithdrawalEventKey { withdrawal_id: out_ref(ix) }
}

fn event_to_step_membership(
  event_key: ledger_state.EventKey,
  step_index: Int,
  phase: ledger_state.TransitionPhase,
) -> transition_trace.RootMembershipProof<
  ledger_state.EventKey,
  ledger_state.EventToStepValue,
> {
  let value = ledger_state.EventToStepValue { step_index, phase }
  let key_bytes = cbor.serialise(event_key)
  let value_bytes = cbor.serialise(value)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.EventToStepRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.EventToStepRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: event_key,
    value,
    proof: [],
  }
}

fn withdrawal_source_membership(
  withdrawal_id: ledger_state.WithdrawalId,
  info: ledger_state.WithdrawalInfo,
) -> transition_trace.RootMembershipProof<
  ledger_state.WithdrawalId,
  ledger_state.WithdrawalInfo,
> {
  let key_bytes = cbor.serialise(withdrawal_id)
  let value_bytes = cbor.serialise(info)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.WithdrawalsRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.WithdrawalsRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: withdrawal_id,
    value: info,
    proof: [],
  }
}

fn deposit_source_membership(
  deposit_id: ledger_state.DepositId,
  info: ledger_state.DepositInfo,
) -> transition_trace.RootMembershipProof<
  ledger_state.DepositId,
  ledger_state.DepositInfo,
> {
  let key_bytes = cbor.serialise(deposit_id)
  let value_bytes = cbor.serialise(info)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.DepositsRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.DepositsRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: deposit_id,
    value: info,
    proof: [],
  }
}

fn forced_source_membership(
  tx_order_id: ledger_state.TxOrderId,
  tx: ledger_state.ForcedInclusionTxV1,
) -> transition_trace.RootMembershipProof<
  ledger_state.TxOrderId,
  ledger_state.ForcedInclusionTxV1,
> {
  let key_bytes = cbor.serialise(tx_order_id)
  let value_bytes = cbor.serialise(tx)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.ForcedTransactionsV1RootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.ForcedTransactionsV1RootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: tx_order_id,
    value: tx,
    proof: [],
  }
}

fn validation_descriptor_membership(
  event_key: ledger_state.EventKey,
  descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
) -> transition_trace.RootMembershipProof<
  ledger_state.EventKey,
  validation_trace_v1.ValidationTraceDescriptorV1,
> {
  let key_bytes = cbor.serialise(event_key)
  let value_bytes = cbor.serialise(descriptor)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.ValidationTracesRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.ValidationTracesRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: event_key,
    value: descriptor,
    proof: [],
  }
}

/// An L2 transaction source committed under the transactions root.
///
/// Under §4's flat reversion the L2 transition authenticates the compact bytes
/// against the tx-id anchor through the §8.8 door, and #584 retired the
/// proof-source `transaction_commitment` from `L2TransactionSourceV1` outright
/// once `validation_claim_v1` — the last reader — was re-anchored onto the
/// derivation instead of the carried field. So there is nothing left for a
/// caller to choose here: the source is `(tx_id, source)` and nothing else, and
/// L2 source membership is verified over raw key/value bytes.
fn raw_l2_source_membership(
  tx_id: ByteArray,
  compact_cbor: ByteArray,
) -> transition_trace.RootMembershipProof<ByteArray, ByteArray> {
  let witness_set_compact_cbor = native_l2_witness_set_compact_cbor()
  let field_preimage_lengths_cbor = native_l2_field_preimage_lengths_cbor()
  let source = ledger_state.L2TransactionSourceV1 {
    tx_id,
    source: ledger_state.NativeTxProofSourceV1 {
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    },
  }
  let source_cbor = cbor.serialise(source)
  let phas_root = mpf.root(mpf.insert(mpf.empty, tx_id, source_cbor, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.TransactionsV1RootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.TransactionsV1RootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: tx_id,
    value: source_cbor,
    proof: [],
  }
}

fn ledger_outref_key(out_ref: OutputReference) -> ByteArray {
  native_components.encode_midgard_tx_input(
    native_types.MidgardTxInput {
      tx_id: out_ref.transaction_id,
      output_index: out_ref.output_index,
    },
  )
}

fn native_input(
  tx_id: ByteArray,
  output_index: Int,
) -> native_types.MidgardTxInput {
  native_types.MidgardTxInput { tx_id, output_index }
}

fn native_output() -> native_types.MidgardTxOutput {
  native_types.MidgardTxOutput {
    address: native_types.MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: native_types.PubKeyCredential(h28_a),
      stake_credential: None,
    },
    value: native_types.MidgardValue { lovelace: 1_000_000, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
}

fn native_output_cbor() -> ByteArray {
  native_components.encode_midgard_tx_output(native_output())
}

/// The exact bytes one output occupies in `utxos_root`.
///
/// Section 5.3 fixes the MPT value as the output's `LedgerOutputCommitmentV1`
/// descriptor, "not ... the full output bytes". Every ledger fixture below is
/// produced through this function rather than pinned as a literal, so a
/// descriptor that stopped agreeing with what `apply_l2_outputs` and
/// `validate_valid_deposit_transition` derive shows up as a failing root
/// comparison instead of a stale wire format that quietly still matches.
fn ledger_value(output_index: Int, output_cbor: ByteArray) -> ByteArray {
  expect Some(value) = ledger_output_descriptor_v1.ledger_value_v1(
    output_index,
    output_cbor,
  )
  value
}

fn native_output_ledger_value(output_index: Int) -> ByteArray {
  ledger_value(output_index, native_output_cbor())
}

fn sample_deposit_info(network_id: Int) -> ledger_state.DepositInfo {
  ledger_state.DepositInfo {
    l2_address: dummy_address(),
    l2_network_id: network_id,
    l2_datum: None,
  }
}

fn deposit_value(event_asset_name: ByteArray) {
  assets.from_lovelace(2_000_000)
    |> assets.add(h28_a, event_asset_name, 1)
    |> assets.add(h28_b, #"aa", 7)
}

fn native_deposit_output(network_id: Int) -> native_types.MidgardTxOutput {
  native_types.MidgardTxOutput {
    address: native_types.MidgardAddress {
      protected: False,
      network_id,
      payment_credential: native_types.ScriptCredential(h28_a),
      stake_credential: None,
    },
    value: native_types.MidgardValue {
      lovelace: 2_000_000,
      assets: [Pair(h28_b |> bytearray.concat(#"aa"), 7)],
    },
    datum_cbor: None,
    script_ref: None,
  }
}

fn native_deposit_output_cbor(network_id: Int) -> ByteArray {
  native_components.encode_midgard_tx_output(native_deposit_output(network_id))
}

fn deposit_reference_input(
  deposit_id: ledger_state.DepositId,
  info: ledger_state.DepositInfo,
  event_asset_name: ByteArray,
) -> Input {
  let deposit_datum = deposit.DepositDatum {
    event: ledger_state.DepositEvent { id: deposit_id, info },
    inclusion_time: 15,
    witness: h28_a,
  }
  let deposit_datum_data: Data = deposit_datum
  Input {
    output_reference: out_ref(99),
    output: Output {
      address: dummy_address(),
      value: deposit_value(event_asset_name),
      datum: InlineDatum(deposit_datum_data),
      reference_script: None,
    },
  }
}

fn ledger_delete_witness(
  key: ByteArray,
  value: ByteArray,
) -> proof.LedgerDeleteWitness {
  proof.LedgerDeleteWitness {
    key,
    value,
    membership_proof: [],
    delete_proof: [],
  }
}

fn ledger_insert_witness(
  key: ByteArray,
  value: ByteArray,
) -> proof.LedgerInsertWitness {
  proof.LedgerInsertWitness {
    key,
    value,
    non_membership_proof: [],
    insert_proof: [],
  }
}

fn forced_tx(
  validity: ledger_state.MidgardTxValidity,
  compact_cbor: ByteArray,
) -> ledger_state.ForcedInclusionTxV1 {
  ledger_state.ForcedInclusionTxV1 {
    tx_id: h32_a,
    source: ledger_state.NativeTxProofSourceV1 {
      compact_cbor,
      witness_set_compact_cbor: #"",
      field_preimage_lengths_cbor: #"",
    },
    operator_validity: validity,
  }
}

fn empty_native_transaction() -> native_types.MidgardTransaction {
  let empty_mint: Data = []
  native_types.MidgardTransaction {
    version: native_types.native_tx_version_v1,
    validity: native_types.TxIsValid,
    body: native_types.MidgardTransactionBody {
      inputs: [],
      reference_inputs: [],
      outputs: [],
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers: [],
      required_signers: [],
      mint: empty_mint,
      script_integrity_hash: zero_hash,
      auxiliary_data_hash: zero_hash,
      network_id: 0,
    },
    witness_set: native_types.MidgardTransactionWitnessSet {
      addr_tx_wits: [],
      script_tx_wits: [],
      redeemer_tx_wits: [],
    },
  }
}

fn authenticated_forced_tx(
  operator_validity: ledger_state.MidgardTxValidity,
) -> ledger_state.ForcedInclusionTxV1 {
  let tx = empty_native_transaction()
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  let tx_id = native_compact.native_tx_id_for_version(
    native_types.native_tx_version_v1,
    native_compact.encode_native_tx_body_compact(compact.body),
  )
  ledger_state.ForcedInclusionTxV1 {
    tx_id,
    source: ledger_state.NativeTxProofSourceV1 {
      compact_cbor: proof_source.compact_cbor,
      witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
      field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    },
    operator_validity,
  }
}

fn authenticated_tx_order_payload() -> ledger_state.TxOrderPayloadV1 {
  let tx = empty_native_transaction()
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  ledger_state.TxOrderPayloadV1 {
    tx_id: native_compact.native_tx_id_for_version(
      native_types.native_tx_version_v1,
      native_compact.encode_native_tx_body_compact(compact.body),
    ),
    transaction_commitment: native_transaction.midgard_transaction_proof_commitment_v1(
      tx,
    ),
    source: ledger_state.NativeTxProofSourceV1 {
      compact_cbor: proof_source.compact_cbor,
      witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
      field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    },
  }
}

fn tx_order_reference_input(
  inclusion_time: Int,
  event_asset_name: ByteArray,
) -> Input {
  let datum: tx_order.Datum = user_events.OptimisticDatum {
    event: ledger_state.TxOrderEventV1 {
      id: out_ref(0),
      tx: authenticated_tx_order_payload(),
    },
    inclusion_time,
    witness: h28_a,
    refund_address: dummy_address(),
    refund_datum: NoDatum,
  }
  let datum_data: Data = datum
  Input {
    output_reference: out_ref(99),
    output: Output {
      address: dummy_address(),
      value: assets.from_asset(h28_a, event_asset_name, 1),
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
  }
}

fn omitted_forced_event_is_valid(inclusion_time: Int) -> Bool {
  let header = base_header()
  let event_asset_name = #"01"
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OmittedDueL1Event {
        witness: proof.OmittedDueForcedTransaction {
          event_ref_input_index: 0,
          event_asset_name,
          validity_override: ledger_state.TxIsValid,
          source_non_membership: empty_non_membership(
            transition_trace.ForcedTransactionsV1RootDomain,
            out_ref(0),
          ),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [tx_order_reference_input(inclusion_time, event_asset_name)],
  )
}

fn accepted_forced_transition_case(
  operator_validity: ledger_state.MidgardTxValidity,
  phase: ledger_state.TransitionPhase,
  claimed_post_root: ByteArray,
  terminal_post_root: ByteArray,
) -> Pair<
  ledger_state.HeaderV1,
  proof.AcceptedTransactionTransitionMismatchWitness,
> {
  let event_key = ledger_state.ForcedTransactionEventKey {
    tx_order_id: out_ref(0),
  }
  let source = authenticated_forced_tx(operator_validity)
  let source_membership = forced_source_membership(out_ref(0), source)
  let opened_step = step(0, event_key, phase, h32_a, claimed_post_root)
  let transition_membership = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(event_key, 0, phase)
  let event_key_hash = blake2b_256(cbor.serialise(event_key))
  let validation_context_hash = validation_trace_v1.hash_validation_context(
    validation_context_cbor,
  )
  let initial_work_witness = validation_machine_v1.encode_transaction_field_scan_witness(
    source.source.compact_cbor,
    source.source.witness_set_compact_cbor,
    source.source.field_preimage_lengths_cbor,
    validation_context_cbor,
    0,
    0,
    0,
    -1,
    0,
  )
  let initial_state = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash,
    transaction_id: source.tx_id,
    // The §7 proof-source commitment of the source's own triple. Derived here
    // because `ForcedInclusionTxV1` no longer carries one — which is exactly
    // what `validation_claim_v1.source_binding_is_exact` now derives too, so a
    // fixture that hard-coded a value would stop testing the binding.
    //
    // Derived from the `source` parameter's own three members rather than from
    // the transaction `authenticated_forced_tx` happens to build them out of, so
    // the fixture keeps agreeing with the claim if a caller ever passes a source
    // assembled some other way.
    transaction_commitment: native_compact.native_tx_proof_commitment_v1(
      source.source.compact_cbor,
      source.source.witness_set_compact_cbor,
      source.source.field_preimage_lengths_cbor,
    ),
    validation_context_hash,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h32_a,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 0,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      0,
      initial_work_witness,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero_hash,
    ledger_delta_root: h32_c,
  }
  let terminal_acceptance_witness_cbor = validation_machine_v1.encode_terminal_acceptance_witness_v1(
    terminal_post_root,
    0,
    [],
  )
  let terminal_state = validation_trace_v1.ValidationMachineStateV1 {
    ..initial_state,
    phase: validation_trace_v1.Terminal,
    program_counter: 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      1,
      terminal_acceptance_witness_cbor,
    ),
    verdict: validation_trace_v1.Accepted,
  }
  let initial_state_hash = validation_trace_v1.hash_machine_state(initial_state)
  let terminal_state_hash = validation_trace_v1.hash_machine_state(
    terminal_state,
  )
  let initial_leaf = validation_trace_v1.trace_leaf_hash(initial_state_hash)
  let terminal_leaf = validation_trace_v1.trace_leaf_hash(terminal_state_hash)
  let descriptor = validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_branch_hash(
      initial_leaf,
      terminal_leaf,
    ),
    step_count: 1,
    initial_state_hash,
    terminal_state_hash,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: zero_hash,
  }
  let descriptor_membership = validation_descriptor_membership(
    event_key,
    descriptor,
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    utxos_root: claimed_post_root,
    forced_transactions_root: source_membership.root,
    transition_trace_root: transition_membership.root,
    event_to_step_root: event_to_step.root,
    validation_traces_root: descriptor_membership.root,
    forced_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
    validation_trace_count: 1,
  }
  Pair(
    header,
    proof.AcceptedTransactionTransitionMismatchWitness {
      claim: validation_claim_v1.ValidationClaimWitnessV1 {
        version: validation_claim_v1.claim_version,
        descriptor_membership,
        transition_step_membership: transition_membership,
        event_to_step_membership: event_to_step,
        source_membership: validation_claim_v1.ForcedValidationSource {
          membership: source_membership,
        },
        validation_context_cbor,
        initial_state,
        terminal_state,
        initial_state_proof: validation_trace_v1.ValidationTraceProof {
          state_index: 0,
          state_hash: initial_state_hash,
          siblings: [terminal_leaf],
        },
        terminal_state_proof: validation_trace_v1.ValidationTraceProof {
          state_index: 1,
          state_hash: terminal_state_hash,
          siblings: [initial_leaf],
        },
      },
      terminal_acceptance_witness_cbor,
    },
  )
}

/// The block-committed compact transaction for an L2 transition vector.
///
/// Fields 0 and 2 commit to `field_commitment(preimage)` — §4's flat hash over
/// the raw enveloped bytes, which is what the door checks. It used to decode
/// each preimage back into items and re-derive the commitment from those, and
/// dropping that round trip is not only shorter: it is the difference between a
/// fixture that can only express preimages the decoder accepts and one that can
/// commit **any** bytes. A miscounted §5.1 header is exactly the case worth
/// committing (see the §7.3 abort-never-clamp vector below), and the old shape
/// could not build it — the fixture aborted before the validator ever saw the
/// bytes.
fn native_l2_compact_cbor(
  spend_inputs_preimage: ByteArray,
  outputs_preimage: ByteArray,
) -> ByteArray {
  native_compact.encode_native_tx_compact_v1(
    native_types.NativeTxCompact {
      body: native_types.NativeTxBodyCompact {
        spend_inputs_hash: native_tx_field_access_v1.field_commitment(
          spend_inputs_preimage,
        ),
        reference_inputs_hash: h32_b,
        outputs_hash: native_tx_field_access_v1.field_commitment(
          outputs_preimage,
        ),
        fee: 0,
        validity_interval_start: -1,
        validity_interval_end: -1,
        required_observers_hash: h32_b,
        required_signers_hash: h32_b,
        mint_hash: h32_b,
        script_integrity_hash: h32_b,
        auxiliary_data_hash: h32_b,
        network_id: 0,
      },
      witness_set_hash: blake2b_256(native_l2_witness_set_compact_cbor()),
      validity_code: 0,
    },
  )
}

fn native_l2_witness_set_compact_cbor() -> ByteArray {
  native_compact.encode_native_tx_witness_set_compact(
    native_types.NativeTxWitnessSetCompact {
      addr_tx_wits_hash: h32_a,
      script_tx_wits_hash: h32_b,
      redeemer_tx_wits_hash: h32_c,
    },
  )
}

fn native_l2_field_preimage_lengths_cbor() -> ByteArray {
  native_compact.encode_native_tx_field_preimage_lengths_v1(
    native_types.NativeTxFieldPreimageLengthsV1 {
      spend_inputs: 0,
      reference_inputs: 0,
      outputs: 0,
      required_observers: 0,
      required_signers: 0,
      mint: 0,
      address_witnesses: 0,
      script_witnesses: 0,
      redeemers: 0,
    },
  )
}

fn native_l2_tx_id(compact_cbor: ByteArray) -> ByteArray {
  let compact = native_compact.decode_native_tx_compact_v1(compact_cbor)
  native_compact.native_tx_id_for_version(
    native_types.native_tx_version_v1,
    native_compact.encode_native_tx_body_compact(compact.body),
  )
}

fn valid_against_header(
  header: ledger_state.HeaderV1,
  fault: proof.TransitionFault,
) -> Bool {
  proof.validate_transition_fault_proof(
    prove(header, fault),
    asset_name_for(header),
    dummy_hub_datum(),
    [],
  )
}

test accepts_trace_boundary_fault() {
  let opened_step = step(
    0,
    withdrawal_event_key(0),
    ledger_state.Withdrawal,
    h32_c,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let header = traced_header(trace_proof.root, env.empty_merkle_tree_root)

  valid_against_header(
    header,
    proof.TraceBoundaryFault { side: proof.TraceStart, trace_proof },
  )
}

test rejects_trace_boundary_when_header_binding_is_wrong() {
  let opened_step = step(
    0,
    withdrawal_event_key(0),
    ledger_state.Withdrawal,
    h32_c,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let header = traced_header(trace_proof.root, env.empty_merkle_tree_root)
  let bad_header = ledger_state.HeaderV1 { ..header, protocol_version: 99 }

  !proof.validate_transition_fault_proof(
    prove(
      bad_header,
      proof.TraceBoundaryFault { side: proof.TraceStart, trace_proof },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [],
  )
}

test rejects_trace_fault_for_wrong_fraud_category_prefix() {
  let opened_step = step(
    0,
    withdrawal_event_key(0),
    ledger_state.Withdrawal,
    h32_c,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let header = traced_header(trace_proof.root, env.empty_merkle_tree_root)

  !proof.validate_transition_fault_proof(
    prove(
      header,
      proof.TraceBoundaryFault { side: proof.TraceStart, trace_proof },
    ),
    #"00000003" |> bytearray.concat(utils.serialise_and_hash_28(header)),
    dummy_hub_datum(),
    [],
  )
}

test accepts_trace_link_fault() {
  let first = step(
    0,
    withdrawal_event_key(0),
    ledger_state.Withdrawal,
    h32_a,
    h32_b,
  )
  let second = step(
    1,
    withdrawal_event_key(1),
    ledger_state.Withdrawal,
    h32_c,
    h32_b,
  )
  let first_proof = trace_membership(first)
  let second_proof = trace_membership(second)
  let phas_root = #"3bcfcd1e874525cfeeebf5e14ca50d42f13e40eb5aec6035553e387334073b11"
  let root = #"7dcbc5316a6560bab11727549dde22de2571a24942627f1085befa031ea9a21b"
  let adjacent = transition_trace.AdjacentTraceProof {
    lower: transition_trace.RootMembershipProof {
      ..first_proof,
      root,
      phas_root,
      count: 2,
      proof: [
        Leaf {
          skip: 0,
          key: #"ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25",
          value: #"d9ede6e662618c9f127593e18134e7cec399e940a4aa9265fe174e62ff93bd82",
        },
      ],
    },
    upper: transition_trace.RootMembershipProof {
      ..second_proof,
      root,
      phas_root,
      count: 2,
      proof: [
        Leaf {
          skip: 0,
          key: #"03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314",
          value: #"36151b072a63e8b9b548aa519018508e16675e2403d738fca55a195e962aaa12",
        },
      ],
    },
  }
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transition_trace_root: root,
    withdrawal_count: 2,
    total_event_count: 2,
    transition_step_count: 2,
  }

  valid_against_header(header, proof.TraceLinkFault { adjacent })
}

test accepts_event_to_step_mismatch_fault() {
  let event_key = withdrawal_event_key(0)
  let opened_step = step(0, event_key, ledger_state.Withdrawal, h32_a, h32_b)
  let trace_proof = trace_membership(opened_step)
  let wrong_mapping = event_to_step_membership(
    event_key,
    7,
    ledger_state.Withdrawal,
  )
  let header = traced_header(trace_proof.root, wrong_mapping.root)

  valid_against_header(
    header,
    proof.EventToStepMismatch {
      trace_proof,
      event_to_step: transition_trace.EventToStepMembership {
        membership: wrong_mapping,
      },
    },
  )
}

test accepts_source_membership_mismatch_fault() {
  let event_key = withdrawal_event_key(0)
  let opened_step = step(0, event_key, ledger_state.Withdrawal, h32_a, h32_b)
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Withdrawal,
  )
  let header = ledger_state.HeaderV1 {
    ..traced_header(trace_proof.root, event_to_step.root),
    withdrawal_count: 0,
  }

  valid_against_header(
    header,
    proof.SourceMembershipMismatch {
      witness: proof.MappedEventMissingFromSource {
        trace_proof,
        event_to_step,
        source_non_membership: proof.WithdrawalSourceNonMembership {
          non_membership: empty_non_membership(
            transition_trace.WithdrawalsRootDomain,
            out_ref(0),
          ),
        },
      },
    },
  )
}

test accepts_l2_mapped_event_missing_from_source_fault() {
  let tx_id = h32_c
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let opened_step = step(0, event_key, ledger_state.L2Transaction, h32_a, h32_b)
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.L2Transaction,
  )
  let header = ledger_state.HeaderV1 {
    ..traced_header(trace_proof.root, event_to_step.root),
    l2_transaction_count: 0,
  }

  valid_against_header(
    header,
    proof.SourceMembershipMismatch {
      witness: proof.MappedEventMissingFromSource {
        trace_proof,
        event_to_step,
        source_non_membership: proof.L2TransactionSourceNonMembership {
          non_membership: empty_non_membership(
            transition_trace.TransactionsV1RootDomain,
            tx_id,
          ),
        },
      },
    },
  )
}

test accepts_l2_source_event_missing_trace_fault() {
  let tx_id = h32_c
  // native_l2_compact_cbor takes §5.1 field *preimages*, not raw 32-byte
  // hashes. It no longer decodes them — under §4 it hashes them flat — so
  // passing h32_a/h32_b would no longer abort here; it would commit two
  // well-formed-looking hashes over nonsense bytes and the failure would
  // surface much later, inside the door, as a fault that fails for the wrong
  // reason. Every caller of this helper passes encode_{input,output}_preimage
  // output, and the two negative vectors that deliberately do otherwise say so.
  let spend_inputs_preimage = native_preimages.encode_input_preimage(
    [native_input(h32_a, 0)],
  )
  let outputs_preimage = native_preimages.encode_output_preimage(
    [native_output()],
  )
  let source_membership = raw_l2_source_membership(
    tx_id,
    native_l2_compact_cbor(spend_inputs_preimage, outputs_preimage),
  )
  let event_to_step_non_membership = empty_non_membership(
    transition_trace.EventToStepRootDomain,
    ledger_state.L2TransactionEventKey { tx_id },
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transactions_root: source_membership.root,
    l2_transaction_count: 1,
  }

  valid_against_header(
    header,
    proof.SourceMembershipMismatch {
      witness: proof.SourceEventMissingTrace {
        source_membership: proof.L2TransactionSourceMembership {
          membership: source_membership,
        },
        event_to_step_non_membership,
      },
    },
  )
}

fn invalid_withdrawal_info() -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    body: ledger_state.WithdrawalBody {
      l2_outref: out_ref(9),
      l2_owner: h28_a,
      l2_value: [],
      l1_address: dummy_address(),
      l1_datum: NoDatum,
    },
    signature: Pair(h32_a, h64_a),
    validity: ledger_state.IncorrectWithdrawalSignature,
  }
}

fn valid_withdrawal_info() -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    ..invalid_withdrawal_info(),
    validity: ledger_state.WithdrawalIsValid,
  }
}

test accepts_valid_withdrawal_delete_transition_fault() {
  let event_key = withdrawal_event_key(0)
  let spent_key = ledger_outref_key(out_ref(9))
  let spent_value = native_output_ledger_value(out_ref(9).output_index)
  let pre_root = mpf.root(mpf.insert(mpf.empty, spent_key, spent_value, []))
  let expected_post_root = proof.delete_root(
    pre_root,
    spent_key,
    spent_value,
    [],
  )
  let opened_step = step(0, event_key, ledger_state.Withdrawal, pre_root, h32_b)
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Withdrawal,
  )
  let withdrawal_membership = withdrawal_source_membership(
    out_ref(0),
    valid_withdrawal_info(),
  )
  let header = ledger_state.HeaderV1 {
    ..traced_header(trace_proof.root, event_to_step.root),
    withdrawals_root: withdrawal_membership.root,
  }

  expect expected_post_root != opened_step.post_utxos_root
  valid_against_header(
    header,
    proof.InvalidOneStepTransition {
      witness: proof.ValidWithdrawalTransition {
        trace_proof,
        event_to_step,
        source_membership: withdrawal_membership,
        spent_utxo: ledger_delete_witness(spent_key, spent_value),
      },
    },
  )
}

test rejects_valid_withdrawal_delete_when_trace_matches_expected_post_root() {
  let event_key = withdrawal_event_key(0)
  let spent_key = ledger_outref_key(out_ref(9))
  let spent_value = native_output_ledger_value(out_ref(9).output_index)
  let pre_root = mpf.root(mpf.insert(mpf.empty, spent_key, spent_value, []))
  let expected_post_root = proof.delete_root(
    pre_root,
    spent_key,
    spent_value,
    [],
  )
  let opened_step = step(
    0,
    event_key,
    ledger_state.Withdrawal,
    pre_root,
    expected_post_root,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Withdrawal,
  )
  let withdrawal_membership = withdrawal_source_membership(
    out_ref(0),
    valid_withdrawal_info(),
  )
  let header = ledger_state.HeaderV1 {
    ..traced_header(trace_proof.root, event_to_step.root),
    withdrawals_root: withdrawal_membership.root,
  }

  !valid_against_header(
    header,
    proof.InvalidOneStepTransition {
      witness: proof.ValidWithdrawalTransition {
        trace_proof,
        event_to_step,
        source_membership: withdrawal_membership,
        spent_utxo: ledger_delete_witness(spent_key, spent_value),
      },
    },
  )
}

test accepts_invalid_withdrawal_no_op_transition_fault() {
  let event_key = withdrawal_event_key(0)
  let opened_step = step(0, event_key, ledger_state.Withdrawal, h32_a, h32_b)
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Withdrawal,
  )
  let withdrawal_membership = withdrawal_source_membership(
    out_ref(0),
    invalid_withdrawal_info(),
  )
  let header = ledger_state.HeaderV1 {
    ..traced_header(trace_proof.root, event_to_step.root),
    withdrawals_root: withdrawal_membership.root,
  }

  valid_against_header(
    header,
    proof.InvalidOneStepTransition {
      witness: proof.InvalidWithdrawalNoOpTransition {
        trace_proof,
        event_to_step,
        source_membership: withdrawal_membership,
      },
    },
  )
}

test rejects_invalid_withdrawal_no_op_when_trace_does_not_mutate_root() {
  let event_key = withdrawal_event_key(0)
  let opened_step = step(0, event_key, ledger_state.Withdrawal, h32_a, h32_a)
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Withdrawal,
  )
  let withdrawal_membership = withdrawal_source_membership(
    out_ref(0),
    invalid_withdrawal_info(),
  )
  let header = ledger_state.HeaderV1 {
    ..traced_header(trace_proof.root, event_to_step.root),
    withdrawals_root: withdrawal_membership.root,
  }

  !valid_against_header(
    header,
    proof.InvalidOneStepTransition {
      witness: proof.InvalidWithdrawalNoOpTransition {
        trace_proof,
        event_to_step,
        source_membership: withdrawal_membership,
      },
    },
  )
}

test accepts_invalid_forced_transaction_no_op_transition_fault() {
  let event_key = ledger_state.ForcedTransactionEventKey {
    tx_order_id: out_ref(0),
  }
  let opened_step = step(
    0,
    event_key,
    ledger_state.ForcedTransaction,
    h32_a,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.ForcedTransaction,
  )
  let source_membership = forced_source_membership(
    out_ref(0),
    forced_tx(ledger_state.FailedScript, #""),
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    forced_transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    forced_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }

  valid_against_header(
    header,
    proof.InvalidOneStepTransition {
      witness: proof.InvalidForcedTransactionNoOpTransition {
        trace_proof,
        event_to_step,
        source_membership,
      },
    },
  )
}

test accepts_valid_forced_transaction_wrong_accepted_post_root_fault() {
  let Pair(header, witness) = accepted_forced_transition_case(
    ledger_state.TxIsValid,
    ledger_state.ForcedTransaction,
    h32_b,
    h32_c,
  )
  proof.validate_accepted_transaction_fault_proof(
    prove(header, proof.AcceptedTransactionTransitionMismatch { witness }),
    asset_name_for(header),
  )
}

test rejects_valid_forced_transaction_matching_accepted_post_root_fault() {
  let Pair(header, witness) = accepted_forced_transition_case(
    ledger_state.TxIsValid,
    ledger_state.ForcedTransaction,
    h32_b,
    h32_b,
  )
  !proof.validate_accepted_transaction_fault_proof(
    prove(header, proof.AcceptedTransactionTransitionMismatch { witness }),
    asset_name_for(header),
  )
}

test rejects_invalid_forced_source_with_accepted_validation_claim() {
  let Pair(header, witness) = accepted_forced_transition_case(
    ledger_state.FailedScript,
    ledger_state.ForcedTransaction,
    h32_b,
    h32_c,
  )
  !proof.validate_accepted_transaction_fault_proof(
    prove(header, proof.AcceptedTransactionTransitionMismatch { witness }),
    asset_name_for(header),
  )
}

test rejects_forced_event_with_l2_transition_phase_in_accepted_claim() {
  let Pair(header, witness) = accepted_forced_transition_case(
    ledger_state.TxIsValid,
    ledger_state.L2Transaction,
    h32_b,
    h32_c,
  )
  !proof.validate_accepted_transaction_fault_proof(
    prove(header, proof.AcceptedTransactionTransitionMismatch { witness }),
    asset_name_for(header),
  )
}

test accepts_forced_transaction_classified_as_normal_fault() {
  let event_key = ledger_state.ForcedTransactionEventKey {
    tx_order_id: out_ref(0),
  }
  let opened_step = step(0, event_key, ledger_state.L2Transaction, h32_a, h32_b)
  let trace_proof = trace_membership(opened_step)
  let source_membership = forced_source_membership(
    out_ref(0),
    forced_tx(ledger_state.TxIsValid, #""),
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    forced_transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    forced_transaction_count: 1,
    l2_transaction_count: 0,
    total_event_count: 1,
    transition_step_count: 1,
  }

  valid_against_header(
    header,
    proof.SourceMembershipMismatch {
      witness: proof.SourcePhaseMismatch {
        trace_proof,
        source_membership: proof.ForcedTransactionSourceMembership {
          membership: source_membership,
        },
      },
    },
  )
}

test accepts_normal_transaction_classified_as_forced_fault() {
  let event_key = ledger_state.L2TransactionEventKey { tx_id: h32_a }
  let opened_step = step(
    0,
    event_key,
    ledger_state.ForcedTransaction,
    h32_a,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let source_membership = raw_l2_source_membership(h32_a, #"d87980")
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    l2_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }

  valid_against_header(
    header,
    proof.SourceMembershipMismatch {
      witness: proof.SourcePhaseMismatch {
        trace_proof,
        source_membership: proof.L2TransactionSourceMembership {
          membership: source_membership,
        },
      },
    },
  )
}

test rejects_unknown_transition_step_schema_version() {
  let event_key = ledger_state.ForcedTransactionEventKey {
    tx_order_id: out_ref(0),
  }
  let opened_step = ledger_state.TransitionStep {
    ..step(0, event_key, ledger_state.ForcedTransaction, h32_a, h32_b),
    schema_version: 23,
  }
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.ForcedTransaction,
  )
  let source_membership = forced_source_membership(
    out_ref(0),
    forced_tx(ledger_state.FailedScript, #""),
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    forced_transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    forced_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }

  !valid_against_header(
    header,
    proof.InvalidOneStepTransition {
      witness: proof.InvalidForcedTransactionNoOpTransition {
        trace_proof,
        event_to_step,
        source_membership,
      },
    },
  )
}

test accepts_valid_deposit_transition_fault() {
  let deposit_id = out_ref(0)
  let deposit_info = sample_deposit_info(0)
  let event_key = ledger_state.DepositEventKey { deposit_id }
  let projected_key = ledger_outref_key(deposit_id)
  let projected_value = ledger_value(
    deposit_id.output_index,
    native_deposit_output_cbor(0),
  )
  let expected_post_root = proof.insert_root(
    env.empty_merkle_tree_root,
    projected_key,
    projected_value,
    [],
  )
  let opened_step = step(
    0,
    event_key,
    ledger_state.Deposit,
    env.empty_merkle_tree_root,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Deposit,
  )
  let source_membership = deposit_source_membership(deposit_id, deposit_info)
  let event_asset_name = #"01"
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    deposits_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    deposit_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }

  expect expected_post_root != opened_step.post_utxos_root
  proof.validate_transition_fault_proof(
    prove(
      header,
      proof.InvalidOneStepTransition {
        witness: proof.ValidDepositTransition {
          trace_proof,
          event_to_step,
          source_membership,
          event_ref_input_index: 0,
          event_asset_name,
          projected_utxo: ledger_insert_witness(projected_key, projected_value),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [deposit_reference_input(deposit_id, deposit_info, event_asset_name)],
  )
}

test rejects_valid_deposit_transition_with_arbitrary_projected_output() {
  let deposit_id = out_ref(0)
  let deposit_info = sample_deposit_info(0)
  let event_key = ledger_state.DepositEventKey { deposit_id }
  let projected_key = ledger_outref_key(deposit_id)
  let opened_step = step(
    0,
    event_key,
    ledger_state.Deposit,
    env.empty_merkle_tree_root,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Deposit,
  )
  let source_membership = deposit_source_membership(deposit_id, deposit_info)
  let event_asset_name = #"01"
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    deposits_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    deposit_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }

  !proof.validate_transition_fault_proof(
    prove(
      header,
      proof.InvalidOneStepTransition {
        witness: proof.ValidDepositTransition {
          trace_proof,
          event_to_step,
          source_membership,
          event_ref_input_index: 0,
          event_asset_name,
          projected_utxo: ledger_insert_witness(projected_key, #"deadbeef"),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [deposit_reference_input(deposit_id, deposit_info, event_asset_name)],
  )
}

test rejects_valid_deposit_transition_with_wrong_network_id() {
  let deposit_id = out_ref(0)
  let deposit_info = sample_deposit_info(0)
  let event_key = ledger_state.DepositEventKey { deposit_id }
  let projected_key = ledger_outref_key(deposit_id)
  let wrong_network_projected_value = ledger_value(
    deposit_id.output_index,
    native_deposit_output_cbor(1),
  )
  let opened_step = step(
    0,
    event_key,
    ledger_state.Deposit,
    env.empty_merkle_tree_root,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Deposit,
  )
  let source_membership = deposit_source_membership(deposit_id, deposit_info)
  let event_asset_name = #"01"
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    deposits_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    deposit_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }

  !proof.validate_transition_fault_proof(
    prove(
      header,
      proof.InvalidOneStepTransition {
        witness: proof.ValidDepositTransition {
          trace_proof,
          event_to_step,
          source_membership,
          event_ref_input_index: 0,
          event_asset_name,
          projected_utxo: ledger_insert_witness(
            projected_key,
            wrong_network_projected_value,
          ),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [deposit_reference_input(deposit_id, deposit_info, event_asset_name)],
  )
}

/// Closing the unilateral slashing hole, stated as a test.
///
/// Until this repoint `validate_valid_deposit_transition` compared the
/// challenger's witness value against the deposit's full projected output
/// bytes, while every producer of `utxos_root` -- the node, the validation
/// machine, the watcher's block replay and DA reconstruction -- inserts the
/// section 5.3 descriptor. So the replayed insert could never reproduce an
/// honest block's post-root, the fault condition is exactly a post-root
/// mismatch, and the deposit arm has no descriptor-correct fallback: any
/// challenger could mint against an honest operator, and an honest deposit
/// dispute could not be proven at all.
///
/// The arm now derives the descriptor itself, so the full-output witness is
/// refused on `projected_utxo.value == expected_value` before any root is
/// computed. The honest twin below mints over the same block, which is what
/// makes this a value check and not an unrelated failure.
test rejects_deposit_transition_replaying_full_output_bytes() {
  let deposit_id = out_ref(0)
  let deposit_info = sample_deposit_info(0)
  let event_key = ledger_state.DepositEventKey { deposit_id }
  let projected_key = ledger_outref_key(deposit_id)
  let full_output_bytes = native_deposit_output_cbor(0)
  let opened_step = step(
    0,
    event_key,
    ledger_state.Deposit,
    env.empty_merkle_tree_root,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Deposit,
  )
  let source_membership = deposit_source_membership(deposit_id, deposit_info)
  let event_asset_name = #"01"
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    deposits_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    deposit_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }

  // The full output bytes and the descriptor are genuinely different values;
  // without this the test could pass on an accidental equality.
  expect
    full_output_bytes != ledger_value(
      deposit_id.output_index,
      native_deposit_output_cbor(0),
    )
  !proof.validate_transition_fault_proof(
    prove(
      header,
      proof.InvalidOneStepTransition {
        witness: proof.ValidDepositTransition {
          trace_proof,
          event_to_step,
          source_membership,
          event_ref_input_index: 0,
          event_asset_name,
          projected_utxo: ledger_insert_witness(
            projected_key,
            full_output_bytes,
          ),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [deposit_reference_input(deposit_id, deposit_info, event_asset_name)],
  )
}

// Q43 structural-N/A adversarial control: a canonical valid normal L2
// transaction must apply its exact delete/insert transition. Reusing its
// authenticated pre-root as the claimed post-root makes the transition a
// no-op; the accepted-transaction final route must still convict it.
test accepts_valid_l2_transaction_no_op_transition_fault() {
  let spent_input = native_input(h32_a, 0)
  let spent_key = native_components.encode_midgard_tx_input(spent_input)
  let spent_value = native_output_ledger_value(spent_input.output_index)
  let produced_output = native_output()
  let produced_value = ledger_value(
    0,
    native_components.encode_midgard_tx_output(produced_output),
  )
  let spend_inputs_preimage = native_preimages.encode_input_preimage(
    [spent_input],
  )
  let outputs_preimage = native_preimages.encode_output_preimage(
    [produced_output],
  )
  let compact_cbor = native_l2_compact_cbor(
    spend_inputs_preimage,
    outputs_preimage,
  )
  let tx_id = native_l2_tx_id(compact_cbor)
  let produced_key = native_components.encode_midgard_tx_input(
    native_input(tx_id, 0),
  )
  let source_membership = raw_l2_source_membership(tx_id, compact_cbor)
  let pre_root = mpf.root(mpf.insert(mpf.empty, spent_key, spent_value, []))
  let expected_post_root = proof.insert_root(
    proof.delete_root(pre_root, spent_key, spent_value, []),
    produced_key,
    produced_value,
    [],
  )
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let opened_step = step(
    0,
    event_key,
    ledger_state.L2Transaction,
    pre_root,
    pre_root,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.L2Transaction,
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    l2_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }

  expect expected_post_root != opened_step.post_utxos_root
  proof.validate_accepted_transaction_fault_proof(
    prove(
      header,
      proof.InvalidOneStepTransition {
        witness: proof.L2TransactionTransition {
          trace_proof,
          event_to_step,
          source_membership,
          spend_inputs_preimage,
          outputs_preimage,
          spent_utxos: [ledger_delete_witness(spent_key, spent_value)],
          produced_utxos: [ledger_insert_witness(produced_key, produced_value)],
        },
      },
    ),
    asset_name_for(header),
  )
}

/// The same hole on the L2 arm, and the reason the ruling calls it live.
///
/// `apply_l2_outputs` replays the challenger's insert values into
/// `utxos_root`; it verifies insert-key non-membership only, so before the
/// repoint a challenger could hand it the full output bytes, watch the replay
/// diverge from every honest descriptor-built post-root, and mint. Every
/// witness in this pair is constructible from real DA -- the delete path is
/// format-agnostic -- which is what made the hole unilateral rather than
/// theoretical.
///
/// The arm now derives the descriptor from the authenticated output, so the
/// full-output value is refused before any root is computed. This test is
/// `fail` rather than a `False` verdict because `apply_l2_outputs` binds the
/// value with `expect`; the honest twin below proves the rest of the witness
/// is sound, so the abort is the value check and nothing else.
test l2_full_output_replay_against_a_descriptor_ledger_cannot_mint() fail {
  let Pair(header, fault) = descriptor_ledger_l2_dispute(
    native_components.encode_midgard_tx_output(native_output()),
  )
  proof.validate_accepted_transaction_fault_proof(
    prove(header, fault),
    asset_name_for(header),
  )
}

/// The honest half: the same block, the same fault, the descriptor as the
/// inserted value. This is the dispute the stale arm could not prove.
test l2_descriptor_dispute_over_a_descriptor_ledger_mints() {
  let Pair(header, fault) = descriptor_ledger_l2_dispute(
    ledger_value(0, native_components.encode_midgard_tx_output(native_output())),
  )
  proof.validate_accepted_transaction_fault_proof(
    prove(header, fault),
    asset_name_for(header),
  )
}

/// One descriptor-built ledger and one genuine no-op fault, parameterised by
/// the value the challenger claims was inserted. Everything else -- the
/// authenticated transaction, the pre-root, the trace and event-to-step
/// bindings, the delete witness -- is identical between the two tests above,
/// so the only difference under test is the wire format of the inserted value.
fn descriptor_ledger_l2_dispute(
  produced_witness_value: ByteArray,
) -> Pair<ledger_state.HeaderV1, proof.TransitionFault> {
  let spent_input = native_input(h32_a, 0)
  let spent_key = native_components.encode_midgard_tx_input(spent_input)
  let spent_value = native_output_ledger_value(spent_input.output_index)
  let produced_output = native_output()
  let spend_inputs_preimage = native_preimages.encode_input_preimage(
    [spent_input],
  )
  let outputs_preimage = native_preimages.encode_output_preimage(
    [produced_output],
  )
  let compact_cbor = native_l2_compact_cbor(
    spend_inputs_preimage,
    outputs_preimage,
  )
  let tx_id = native_l2_tx_id(compact_cbor)
  let produced_key = native_components.encode_midgard_tx_input(
    native_input(tx_id, 0),
  )
  let source_membership = raw_l2_source_membership(tx_id, compact_cbor)
  let pre_root = mpf.root(mpf.insert(mpf.empty, spent_key, spent_value, []))
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let opened_step = step(
    0,
    event_key,
    ledger_state.L2Transaction,
    pre_root,
    pre_root,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.L2Transaction,
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    l2_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }
  Pair(
    header,
    proof.InvalidOneStepTransition {
      witness: proof.L2TransactionTransition {
        trace_proof,
        event_to_step,
        source_membership,
        spend_inputs_preimage,
        outputs_preimage,
        spent_utxos: [ledger_delete_witness(spent_key, spent_value)],
        produced_utxos: [
          ledger_insert_witness(produced_key, produced_witness_value),
        ],
      },
    },
  )
}

test rejects_l2_transaction_when_trace_matches_expected_post_root() {
  let spent_input = native_input(h32_a, 0)
  let spent_key = native_components.encode_midgard_tx_input(spent_input)
  let spent_value = native_output_ledger_value(spent_input.output_index)
  let produced_output = native_output()
  let produced_value = ledger_value(
    0,
    native_components.encode_midgard_tx_output(produced_output),
  )
  let spend_inputs_preimage = native_preimages.encode_input_preimage(
    [spent_input],
  )
  let outputs_preimage = native_preimages.encode_output_preimage(
    [produced_output],
  )
  let compact_cbor = native_l2_compact_cbor(
    spend_inputs_preimage,
    outputs_preimage,
  )
  let tx_id = native_l2_tx_id(compact_cbor)
  let produced_key = native_components.encode_midgard_tx_input(
    native_input(tx_id, 0),
  )
  let source_membership = raw_l2_source_membership(tx_id, compact_cbor)
  let pre_root = mpf.root(mpf.insert(mpf.empty, spent_key, spent_value, []))
  let expected_post_root = proof.insert_root(
    proof.delete_root(pre_root, spent_key, spent_value, []),
    produced_key,
    produced_value,
    [],
  )
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let opened_step = step(
    0,
    event_key,
    ledger_state.L2Transaction,
    pre_root,
    expected_post_root,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.L2Transaction,
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    l2_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }

  !proof.validate_accepted_transaction_fault_proof(
    prove(
      header,
      proof.InvalidOneStepTransition {
        witness: proof.L2TransactionTransition {
          trace_proof,
          event_to_step,
          source_membership,
          spend_inputs_preimage,
          outputs_preimage,
          spent_utxos: [ledger_delete_witness(spent_key, spent_value)],
          produced_utxos: [ledger_insert_witness(produced_key, produced_value)],
        },
      },
    ),
    asset_name_for(header),
  )
}

test rejects_l2_transaction_with_wrong_authenticated_transaction_id() fail {
  let spent_input = native_input(h32_a, 0)
  let spent_key = native_components.encode_midgard_tx_input(spent_input)
  let spent_value = native_output_ledger_value(spent_input.output_index)
  let produced_output = native_output()
  let produced_value = ledger_value(
    0,
    native_components.encode_midgard_tx_output(produced_output),
  )
  let spend_inputs_preimage = native_preimages.encode_input_preimage(
    [spent_input],
  )
  let outputs_preimage = native_preimages.encode_output_preimage(
    [produced_output],
  )
  let compact_cbor = native_l2_compact_cbor(
    spend_inputs_preimage,
    outputs_preimage,
  )
  let wrong_tx_id = h32_c
  let produced_key = native_components.encode_midgard_tx_input(
    native_input(wrong_tx_id, 0),
  )
  let source_membership = raw_l2_source_membership(wrong_tx_id, compact_cbor)
  let pre_root = mpf.root(mpf.insert(mpf.empty, spent_key, spent_value, []))
  let event_key = ledger_state.L2TransactionEventKey { tx_id: wrong_tx_id }
  let opened_step = step(
    0,
    event_key,
    ledger_state.L2Transaction,
    pre_root,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.L2Transaction,
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    l2_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }
  proof.validate_accepted_transaction_fault_proof(
    prove(
      header,
      proof.InvalidOneStepTransition {
        witness: proof.L2TransactionTransition {
          trace_proof,
          event_to_step,
          source_membership,
          spend_inputs_preimage,
          outputs_preimage,
          spent_utxos: [ledger_delete_witness(spent_key, spent_value)],
          produced_utxos: [ledger_insert_witness(produced_key, produced_value)],
        },
      },
    ),
    asset_name_for(header),
  )
}

test rejects_l2_transaction_with_uncommitted_spend_preimage() fail {
  let spent_input = native_input(h32_a, 0)
  let spent_key = native_components.encode_midgard_tx_input(spent_input)
  let spent_value = native_output_ledger_value(spent_input.output_index)
  let produced_output = native_output()
  let produced_value = ledger_value(
    0,
    native_components.encode_midgard_tx_output(produced_output),
  )
  let committed_spend_inputs_preimage = native_preimages.encode_input_preimage(
    [spent_input],
  )
  let outputs_preimage = native_preimages.encode_output_preimage(
    [produced_output],
  )
  let compact_cbor = native_l2_compact_cbor(
    committed_spend_inputs_preimage,
    outputs_preimage,
  )
  let tx_id = native_l2_tx_id(compact_cbor)
  let produced_key = native_components.encode_midgard_tx_input(
    native_input(tx_id, 0),
  )
  let source_membership = raw_l2_source_membership(tx_id, compact_cbor)
  let pre_root = mpf.root(mpf.insert(mpf.empty, spent_key, spent_value, []))
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let opened_step = step(
    0,
    event_key,
    ledger_state.L2Transaction,
    pre_root,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.L2Transaction,
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    l2_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }
  proof.validate_accepted_transaction_fault_proof(
    prove(
      header,
      proof.InvalidOneStepTransition {
        witness: proof.L2TransactionTransition {
          trace_proof,
          event_to_step,
          source_membership,
          spend_inputs_preimage: native_preimages.encode_input_preimage(
            [native_input(h32_b, 0)],
          ),
          outputs_preimage,
          spent_utxos: [ledger_delete_witness(spent_key, spent_value)],
          produced_utxos: [ledger_insert_witness(produced_key, produced_value)],
        },
      },
    ),
    asset_name_for(header),
  )
}

/// A one-input/one-output L2 transition fault whose ledger witnesses are built
/// from the items the **supplied** preimages carry, rather than from the ones
/// the block committed.
///
/// That inversion is the whole point of the shape, and it is what the older
/// `rejects_l2_transaction_with_uncommitted_spend_preimage` vector above does
/// not do. `apply_l2_spends` matches each walked input against its delete
/// witness and `apply_l2_outputs` matches each walked output against its insert
/// witness, so a vector that substitutes a preimage and leaves the committed
/// witnesses in place is refused by *those* checks whether the door is there or
/// not: it goes red for a reason that has nothing to do with §7.1, and would
/// stay red if the authenticate-once hash were deleted outright.
///
/// Deriving the witnesses from the supplied items removes every such reason.
/// The fault is internally consistent end to end and the post-root inequality
/// it claims still holds, so the door's hash of the supplied preimage against
/// the committed `spend_inputs_hash`/`outputs_hash` is the only thing standing
/// between a prover and a `True` — which is what makes these neutralisation
/// selectors rather than merely failing tests.
///
/// Verified by mutation, not by argument. Neutralising the door's
/// authenticate-once check alone (`whole_view`'s
/// `expect field_commitment(preimage) == expected_hash`) turns exactly the two
/// substitution vectors below red and nothing else in this module — while
/// `rejects_l2_transaction_with_uncommitted_spend_preimage` above stays green
/// under that same mutation, because `apply_l2_spends` rejects its mismatched
/// delete witness first. That is the concrete reason these two exist alongside
/// it rather than instead of it, and it is why they, not it, are the selectors
/// the Q21 execution ledger names.
///
/// `post_utxos_root` is a root the transition cannot produce, so the fault's own
/// `expected_post_root != post_utxos_root` claim is satisfied for the right
/// reason.
fn substituted_preimage_l2_fault(
  compact_cbor: ByteArray,
  supplied_spend_inputs_preimage: ByteArray,
  supplied_outputs_preimage: ByteArray,
  supplied_spend_input: native_types.MidgardTxInput,
  supplied_output: native_types.MidgardTxOutput,
) -> Pair<ledger_state.HeaderV1, proof.TransitionFault> {
  let spent_key = native_components.encode_midgard_tx_input(
    supplied_spend_input,
  )
  let spent_value = native_output_ledger_value(
    supplied_spend_input.output_index,
  )
  let produced_value = ledger_value(
    0,
    native_components.encode_midgard_tx_output(supplied_output),
  )
  let tx_id = native_l2_tx_id(compact_cbor)
  let produced_key = native_components.encode_midgard_tx_input(
    native_input(tx_id, 0),
  )
  let source_membership = raw_l2_source_membership(tx_id, compact_cbor)
  let pre_root = mpf.root(mpf.insert(mpf.empty, spent_key, spent_value, []))
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let opened_step = step(
    0,
    event_key,
    ledger_state.L2Transaction,
    pre_root,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.L2Transaction,
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    l2_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }
  Pair(
    header,
    proof.InvalidOneStepTransition {
      witness: proof.L2TransactionTransition {
        trace_proof,
        event_to_step,
        source_membership,
        spend_inputs_preimage: supplied_spend_inputs_preimage,
        outputs_preimage: supplied_outputs_preimage,
        spent_utxos: [ledger_delete_witness(spent_key, spent_value)],
        produced_utxos: [ledger_insert_witness(produced_key, produced_value)],
      },
    },
  )
}

/// §7.1 on field 0. The prover hands the door a well-formed spend-inputs
/// preimage for a transaction the block never committed, with every ledger
/// witness consistent with it.
test rejects_l2_transaction_with_a_substituted_spend_inputs_preimage() fail {
  let substituted_spend_input = native_input(h32_c, 0)
  let produced_output = native_output()
  let outputs_preimage = native_preimages.encode_output_preimage(
    [produced_output],
  )
  let compact_cbor = native_l2_compact_cbor(
    native_preimages.encode_input_preimage([native_input(h32_a, 0)]),
    outputs_preimage,
  )
  let Pair(header, fault) = substituted_preimage_l2_fault(
    compact_cbor,
    native_preimages.encode_input_preimage([substituted_spend_input]),
    outputs_preimage,
    substituted_spend_input,
    produced_output,
  )
  proof.validate_accepted_transaction_fault_proof(
    prove(header, fault),
    asset_name_for(header),
  )
}

/// §7.1 on field 2 — the second of the two openings, which the field-0 vector
/// above cannot reach. Without it, an opening that named field 0 twice, or one
/// that skipped the outputs door entirely, would keep the whole suite green.
test rejects_l2_transaction_with_a_substituted_outputs_preimage() fail {
  let spent_input = native_input(h32_a, 0)
  let substituted_output = native_deposit_output(0)
  let spend_inputs_preimage = native_preimages.encode_input_preimage(
    [spent_input],
  )
  let compact_cbor = native_l2_compact_cbor(
    spend_inputs_preimage,
    native_preimages.encode_output_preimage([native_output()]),
  )
  let Pair(header, fault) = substituted_preimage_l2_fault(
    compact_cbor,
    spend_inputs_preimage,
    native_preimages.encode_output_preimage([substituted_output]),
    spent_input,
    substituted_output,
  )
  proof.validate_accepted_transaction_fault_proof(
    prove(header, fault),
    asset_name_for(header),
  )
}

/// §7.3 abort-never-clamp, on a preimage the block itself committed.
///
/// The committed field-0 preimage declares two items in its §5.1 header and
/// carries one. Because the block committed these exact bytes the door's
/// authenticate-once hash *matches* — the two substitution vectors above cannot
/// reach this state — and the question becomes what a family does with a field
/// whose commitment is honest and whose contents are malformed. The required
/// answer is: abort. Clamping to the one item actually present would let a
/// malformed commitment be disputed as though it were well-formed, and the
/// walked spend-input list is fed straight into the ledger-root recomputation.
///
/// What this pins is that behaviour, not any single line of the door, and the
/// distinction is why it is cited as §7.3 rather than §7.4. A miscounted header
/// is §7.4's subject — count consistency at view construction — but mutation
/// puts the boundary precisely, and §7.4 is not the guard that fires:
/// neutralising `whole_view`'s arithmetic
/// `header_len + stride * count == total_length` check does **not** turn this
/// vector red, because the walk then runs off the end of the bytes and the
/// §7.3 out-of-range item read aborts instead. Two independent guards refuse it,
/// which is the right outcome for a fail-closed property but means this test is
/// deliberately *not* listed as a Q21 neutralisation selector — it does not
/// isolate one check, and a selector that cannot say which check it is watching
/// is not one.
test rejects_l2_transaction_with_a_miscounted_committed_spend_inputs_preimage() fail {
  let spent_input = native_input(h32_a, 0)
  let produced_output = native_output()
  let miscounted_spend_inputs_preimage = bytearray.concat(
    native_tx_field_access_v1.encode_field_array_header(2),
    bytearray.drop(native_preimages.encode_input_preimage([spent_input]), 1),
  )
  let outputs_preimage = native_preimages.encode_output_preimage(
    [produced_output],
  )
  let compact_cbor = native_l2_compact_cbor(
    miscounted_spend_inputs_preimage,
    outputs_preimage,
  )
  let Pair(header, fault) = substituted_preimage_l2_fault(
    compact_cbor,
    miscounted_spend_inputs_preimage,
    outputs_preimage,
    spent_input,
    produced_output,
  )
  proof.validate_accepted_transaction_fault_proof(
    prove(header, fault),
    asset_name_for(header),
  )
}

test accepts_duplicate_trace_event_fault() {
  let event_key = withdrawal_event_key(0)
  let left_step = step(0, event_key, ledger_state.Withdrawal, h32_a, h32_b)
  let right_step = step(1, event_key, ledger_state.Withdrawal, h32_b, h32_c)
  let left_trace = trace_membership(left_step)
  let right_trace = trace_membership(right_step)
  let phas_root = #"dbf61b46d5c4def32178e8415221a5f46769dffe0ce62557a3d7498069936dcf"
  let root = #"30207d3fb9261eef53138441ef4ece4ba72f3e2e9f79e2004a122f1bffb305d3"
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transition_trace_root: root,
    withdrawal_count: 2,
    total_event_count: 2,
    transition_step_count: 2,
  }

  valid_against_header(
    header,
    proof.DuplicateTraceEvent {
      left_trace: transition_trace.RootMembershipProof {
        ..left_trace,
        root,
        phas_root,
        count: 2,
        proof: [
          Leaf {
            skip: 0,
            key: #"ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25",
            value: #"36238ae7ed1534b6a689b761df8b19e3ced707b1d2b03d0335b703473e68b7d3",
          },
        ],
      },
      right_trace: transition_trace.RootMembershipProof {
        ..right_trace,
        root,
        phas_root,
        count: 2,
        proof: [
          Leaf {
            skip: 0,
            key: #"03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314",
            value: #"36151b072a63e8b9b548aa519018508e16675e2403d738fca55a195e962aaa12",
          },
        ],
      },
    },
  )
}

test accepts_count_fault() {
  let proof_root = transition_trace.RootCountProof {
    domain: transition_trace.TransitionTraceRootDomain,
    root: env.empty_merkle_tree_root,
    phas_root: env.empty_merkle_tree_root,
    count: 0,
  }
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transition_step_count: 1,
  }

  valid_against_header(
    header,
    proof.CountFault {
      witness: proof.TransitionTraceRootCountMismatch { proof: proof_root },
    },
  )
}

test accepts_omitted_due_deposit_fault() {
  let deposit_info = sample_deposit_info(0)
  let deposit_datum = deposit.DepositDatum {
    event: ledger_state.DepositEvent { id: out_ref(0), info: deposit_info },
    inclusion_time: 15,
    witness: h28_a,
  }
  let deposit_datum_data: Data = deposit_datum
  let event_asset_name = #"01"
  let reference_input = Input {
    output_reference: out_ref(99),
    output: Output {
      address: dummy_address(),
      value: assets.from_asset(h28_a, event_asset_name, 1),
      datum: InlineDatum(deposit_datum_data),
      reference_script: None,
    },
  }
  let header = base_header()

  proof.validate_transition_fault_proof(
    prove(
      header,
      proof.OmittedDueL1Event {
        witness: proof.OmittedDueDeposit {
          event_ref_input_index: 0,
          event_asset_name,
          source_non_membership: empty_non_membership(
            transition_trace.DepositsRootDomain,
            out_ref(0),
          ),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [reference_input],
  )
}

test accepts_omitted_forced_order_at_authenticated_end_time_boundary() {
  omitted_forced_event_is_valid(base_header().end_time)
}

test rejects_omitted_forced_order_at_excluded_start_time_boundary() {
  !omitted_forced_event_is_valid(base_header().start_time)
}

test rejects_omitted_forced_order_published_after_challenged_block() {
  !omitted_forced_event_is_valid(base_header().end_time + 1)
}

test accepts_late_forced_order_as_out_of_window_source_event() {
  let event_asset_name = #"01"
  let source_membership = forced_source_membership(
    out_ref(0),
    authenticated_forced_tx(ledger_state.TxIsValid),
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    forced_transactions_root: source_membership.root,
    forced_transaction_count: 1,
  }
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OutOfWindowSourceEvent {
        witness: proof.OutOfWindowForcedTransaction {
          event_ref_input_index: 0,
          event_asset_name,
          validity_override: ledger_state.TxIsValid,
          source_membership,
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [tx_order_reference_input(header.end_time + 1, event_asset_name)],
  )
}

test accepts_out_of_window_deposit_fault() {
  let deposit_info = sample_deposit_info(0)
  let deposit_datum = deposit.DepositDatum {
    event: ledger_state.DepositEvent { id: out_ref(0), info: deposit_info },
    inclusion_time: 30,
    witness: h28_a,
  }
  let deposit_datum_data: Data = deposit_datum
  let event_asset_name = #"01"
  let source_membership = deposit_source_membership(out_ref(0), deposit_info)
  let reference_input = Input {
    output_reference: out_ref(99),
    output: Output {
      address: dummy_address(),
      value: assets.from_asset(h28_a, event_asset_name, 1),
      datum: InlineDatum(deposit_datum_data),
      reference_script: None,
    },
  }
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    deposits_root: source_membership.root,
    deposit_count: 1,
  }

  proof.validate_transition_fault_proof(
    prove(
      header,
      proof.OutOfWindowSourceEvent {
        witness: proof.OutOfWindowDeposit {
          event_ref_input_index: 0,
          event_asset_name,
          source_membership,
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [reference_input],
  )
}
