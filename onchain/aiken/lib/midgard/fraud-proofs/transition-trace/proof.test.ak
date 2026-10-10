use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry.{Leaf}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, suffix}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, OutputReference}
use env
use midgard/common/utils
use midgard/event_history/order_facts
use midgard/event_history/proof_fixture as history
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/components as native_components
use midgard/fraud_proofs/native_tx/preimages as native_preimages
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/transition_trace/proof
use midgard/fraud_proofs/transition_trace/timing_fixture
use midgard/hub_oracle
use midgard/ledger_output_descriptor_v1
use midgard/ledger_state
use midgard/mpf_branch_opening_fixture as opening_fixture
use midgard/native_tx_field_access_v1
use midgard/rejection_reason_v1
use midgard/transition_trace
use midgard/user_events
use midgard/user_events/tx_order_v1 as tx_order
use midgard/validation_claim_v1
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/fixtures_ledger
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

fn empty_root_count_proof(
  domain: transition_trace.RootDomain,
) -> transition_trace.RootCountProof {
  transition_trace.RootCountProof {
    domain,
    root: env.empty_merkle_tree_root,
    phas_root: env.empty_merkle_tree_root,
    count: 0,
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

fn deposit_value() {
  assets.from_lovelace(2_000_000)
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
) -> Input {
  timing_fixture.deposit_reference(
    h28_a,
    deposit_id,
    info,
    deposit_value(),
    3_000_000,
  )
}

fn ledger_delete_witness(
  key: ByteArray,
  value: ByteArray,
) -> proof.LedgerDeleteWitness {
  proof.LedgerDeleteWitness { key, value, opening: #"", delete_proof: [] }
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
  validity: rejection_reason_v1.OperatorVerdictV1,
  compact_cbor: ByteArray,
) -> ledger_state.ForcedInclusionTxV1 {
  ledger_state.ForcedInclusionTxV1 {
    tx_id: h32_a,
    submitted_source: ledger_state.ForcedTxProofSourceV1 {
      compact_cbor,
      witness_set_compact_cbor: #"",
      field_preimage_lengths_cbor: #"",
    },
    verdict: validity,
  }
}

fn verdict_plutus_failed() -> rejection_reason_v1.OperatorVerdictV1 {
  rejection_reason_v1.ForcedTxInvalid {
    reason: rejection_reason_v1.PlutusExecutionFailed { execution_index: 0 },
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
  verdict: rejection_reason_v1.OperatorVerdictV1,
) -> ledger_state.ForcedInclusionTxV1 {
  authenticated_forced_tx_of(empty_native_transaction(), verdict)
}

/// The authenticated empty forced transaction, carrying the given native
/// validity interval scalars (slots; a negative scalar is open).
fn authenticated_forced_tx_with_interval(
  verdict: rejection_reason_v1.OperatorVerdictV1,
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> ledger_state.ForcedInclusionTxV1 {
  let empty = empty_native_transaction()
  authenticated_forced_tx_of(
    native_types.MidgardTransaction {
      ..empty,
      body: native_types.MidgardTransactionBody {
        ..empty.body,
        validity_interval_start,
        validity_interval_end,
      },
    },
    verdict,
  )
}

fn authenticated_forced_tx_of(
  tx: native_types.MidgardTransaction,
  verdict: rejection_reason_v1.OperatorVerdictV1,
) -> ledger_state.ForcedInclusionTxV1 {
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  let tx_id = native_compact.native_tx_id_for_version(
    native_types.native_tx_version_v1,
    native_compact.encode_native_tx_body_compact(compact.body),
  )
  ledger_state.ForcedInclusionTxV1 {
    tx_id,
    submitted_source: forced_fixture.from_native_source(
      ledger_state.NativeTxProofSourceV1 {
        compact_cbor: proof_source.compact_cbor,
        witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
        field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
      },
    ),
    verdict,
  }
}

fn authenticated_tx_order_payload() -> ledger_state.TxOrderPayloadV1 {
  tx_order_payload_of(
    authenticated_forced_tx(rejection_reason_v1.ForcedTxValid),
  )
}

fn authenticated_tx_order_payload_with_interval(
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> ledger_state.TxOrderPayloadV1 {
  tx_order_payload_of(
    authenticated_forced_tx_with_interval(
      rejection_reason_v1.ForcedTxValid,
      validity_interval_start,
      validity_interval_end,
    ),
  )
}

fn tx_order_payload_of(
  leaf: ledger_state.ForcedInclusionTxV1,
) -> ledger_state.TxOrderPayloadV1 {
  ledger_state.TxOrderPayloadV1 {
    tx_id: leaf.tx_id,
    transaction_commitment: native_compact.forced_tx_proof_commitment_v1(
      leaf.submitted_source.compact_cbor,
      leaf.submitted_source.witness_set_compact_cbor,
      leaf.submitted_source.field_preimage_lengths_cbor,
    ),
    submitted_source: leaf.submitted_source,
  }
}

fn tx_order_reference_input(
  inclusion_time: Int,
  event_asset_name: ByteArray,
) -> Input {
  tx_order_reference_input_for(
    inclusion_time,
    event_asset_name,
    authenticated_tx_order_payload(),
  )
}

fn tx_order_reference_input_for(
  inclusion_time: Int,
  event_asset_name: ByteArray,
  payload: ledger_state.TxOrderPayloadV1,
) -> Input {
  let datum: tx_order.Datum = user_events.OptimisticDatum {
    event: ledger_state.TxOrderEventV1 { id: out_ref(0), tx: payload },
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
  let event_asset_name = #"01"

  let header = base_header()
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OmittedDueL1Event {
        witness: proof.OmittedDueForcedTransaction {
          event_ref_input_index: 0,
          event_asset_name,
          validity_override: rejection_reason_v1.ForcedTxValid,
          source_non_membership: empty_non_membership(
            transition_trace.ForcedTransactionsV1RootDomain,
            out_ref(0),
          ),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    timing_fixture.evidence(
      header,
      [tx_order_reference_input(inclusion_time, event_asset_name)],
    ),
  )
}

fn accepted_forced_transition_case(
  verdict: rejection_reason_v1.OperatorVerdictV1,
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
  let source = authenticated_forced_tx(verdict)
  let source_membership = forced_source_membership(out_ref(0), source)
  let opened_step = step(0, event_key, phase, h32_a, claimed_post_root)
  let transition_membership = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(event_key, 0, phase)
  let event_key_hash = blake2b_256(cbor.serialise(event_key))
  let validation_context_hash = validation_trace_v1.hash_validation_context(
    validation_context_cbor,
  )
  let initial_work_witness = vm_witness.encode_transaction_field_scan_witness(
    source.submitted_source.compact_cbor,
    source.submitted_source.witness_set_compact_cbor,
    source.submitted_source.field_preimage_lengths_cbor,
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
    transaction_commitment: native_compact.forced_tx_proof_commitment_v1(
      source.submitted_source.compact_cbor,
      source.submitted_source.witness_set_compact_cbor,
      source.submitted_source.field_preimage_lengths_cbor,
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
  let terminal_acceptance_witness_cbor = vm_witness.encode_terminal_acceptance_witness_v1(
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
    None,
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
    None,
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
    None,
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
  let phas_root = #"11c11a47e2535bc274909e7745319d95ff62b1d1a6bdc2ce11ef27743e5738da"
  let root = #"fb52b5d24775a6d3d068ffee0bdc1f3fd0df169d16c6bcc2ed239417dad3f32b"
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
        source_membership: proof.L2KeyOpening {
          tx_id: source_membership.key,
          value_hash: blake2b_256(source_membership.value),
          phas_root: source_membership.phas_root,
          proof: source_membership.proof,
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

fn withdrawal_reference_input(inclusion_time: Int) -> Input {
  history.order_reference(
    h28_a,
    user_events.out_ref_to_nonce(out_ref(0)),
    order_facts.WithdrawalPayload {
      event: ledger_state.WithdrawalEvent {
        id: out_ref(0),
        info: valid_withdrawal_info(),
      },
      refund_address: dummy_address(),
      refund_datum: NoDatum,
    },
    inclusion_time,
  )
}

fn omitted_withdrawal_event_is_valid(
  header: ledger_state.HeaderV1,
  inclusion_time: Int,
  source_non_membership: transition_trace.RootNonMembershipProof<
    ledger_state.WithdrawalId,
  >,
) -> Bool {
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OmittedDueL1Event {
        witness: proof.OmittedDueWithdrawal { source_non_membership },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    timing_fixture.evidence(
      header,
      [withdrawal_reference_input(inclusion_time)],
    ),
  )
}

fn out_of_window_withdrawal_is_valid(
  inclusion_time: Int,
  mutate_body: Bool,
  committed_validity: ledger_state.WithdrawalValidity,
) -> Bool {
  let committed_info = ledger_state.WithdrawalInfo {
    ..valid_withdrawal_info(),
    validity: committed_validity,
    body: if mutate_body {
      ledger_state.WithdrawalBody {
        ..valid_withdrawal_info().body,
        l2_outref: out_ref(999),
      }
    } else {
      valid_withdrawal_info().body
    },
  }
  let source_membership = withdrawal_source_membership(
    out_ref(0),
    committed_info,
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    withdrawals_root: source_membership.root,
    withdrawal_count: 1,
  }
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OutOfWindowSourceEvent {
        witness: proof.OutOfWindowWithdrawal { source_membership },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    timing_fixture.evidence(
      header,
      [withdrawal_reference_input(inclusion_time)],
    ),
  )
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
    #"",
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

// ## Withdrawing the last ledger entry
//
// An honest node commits an empty ledger as `env.empty_merkle_tree_root`, not
// as the MPF library's `null_hash`. The step that withdraws the only entry is
// honest exactly when its post root is that sentinel.

fn last_entry_withdrawal_fault_is_valid(post_root: ByteArray) -> Bool {
  withdrawal_delete_fault_from_is_valid(
    mpf.root(
      mpf.insert(
        mpf.empty,
        ledger_outref_key(out_ref(9)),
        native_output_ledger_value(9),
        [],
      ),
    ),
    post_root,
    [],
    #"",
  )
}

test rejects_a_last_entry_withdrawal_that_commits_the_empty_ledger() {
  !last_entry_withdrawal_fault_is_valid(env.empty_merkle_tree_root)
}

test accepts_a_last_entry_withdrawal_that_commits_the_library_empty_root() {
  last_entry_withdrawal_fault_is_valid(null_hash)
}

// The L2 replay route folds each spent input through `apply_delete_witness`
// and, for a transaction with no outputs, convicts exactly when the folded root
// differs from the step's post root. Spending the ledger's only entry folds to
// the committed sentinel, so the route convicts a block that names the drained
// ledger `null_hash` and not the honest block.
test l2_replay_names_a_drained_ledger_by_the_committed_sentinel() {
  let key = ledger_outref_key(out_ref(9))
  let value = native_output_ledger_value(9)
  let drained = proof.apply_delete_witness(
    mpf.root(mpf.insert(mpf.empty, key, value, [])),
    ledger_delete_witness(key, value),
  )
  let convicts = fn(post_root) { drained != post_root }
  and {
    !convicts(env.empty_merkle_tree_root),
    convicts(null_hash),
  }
}

// ## A challenger cannot steer the terminal neighbour of a one-step mutation
//
// The challenger supplies the delete and insert proofs of a one-step
// transition fault. The including root authenticates the terminal neighbour
// only up to the reading the excluding root makes of it, so a proof that
// re-reads that neighbour would give a root the honest trie never has, and
// convict an honest operator whose trace step holds the true root.

// path(out_ref 9 key) = 5 a 1 3 ..., path(#"000a") = 5 e 3 c ...: the spent
// output's only neighbour sits behind a skip of one.
const withdrawal_neighbour_key = #"000a"

const neighbour_value = #"0b"

fn withdrawal_neighbour_step() -> mpf.ProofStep {
  Leaf {
    skip: 1,
    key: blake2b_256(withdrawal_neighbour_key),
    value: blake2b_256(neighbour_value),
  }
}

fn withdrawal_neighbour_only_root() -> ByteArray {
  mpf.root(mpf.insert(mpf.empty, withdrawal_neighbour_key, neighbour_value, []))
}

fn withdrawal_pre_root() -> ByteArray {
  mpf.root(
    mpf.insert(
      mpf.from_root(withdrawal_neighbour_only_root()),
      ledger_outref_key(out_ref(9)),
      native_output_ledger_value(9),
      [withdrawal_neighbour_step()],
    ),
  )
}

fn withdrawal_delete_witness(
  delete_proof: mpf.Proof,
  opening: ByteArray,
) -> proof.LedgerDeleteWitness {
  proof.LedgerDeleteWitness {
    key: ledger_outref_key(out_ref(9)),
    value: native_output_ledger_value(9),
    opening,
    delete_proof,
  }
}

fn withdrawal_delete_fault_is_valid(
  post_root: ByteArray,
  delete_proof: mpf.Proof,
  opening: ByteArray,
) -> Bool {
  withdrawal_delete_fault_from_is_valid(
    withdrawal_pre_root(),
    post_root,
    delete_proof,
    opening,
  )
}

fn withdrawal_delete_fault_from_is_valid(
  pre_root: ByteArray,
  post_root: ByteArray,
  delete_proof: mpf.Proof,
  opening: ByteArray,
) -> Bool {
  let event_key = withdrawal_event_key(0)
  let opened_step = step(
    0,
    event_key,
    ledger_state.Withdrawal,
    pre_root,
    post_root,
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
  valid_against_header(
    header,
    proof.InvalidOneStepTransition {
      witness: proof.ValidWithdrawalTransition {
        trace_proof,
        event_to_step,
        source_membership: withdrawal_membership,
        spent_utxo: withdrawal_delete_witness(delete_proof, opening),
      },
    },
  )
}

test honest_withdrawal_delete_proof_resolves_to_the_honest_post_root() {
  let honest = [withdrawal_neighbour_step()]
  and {
    proof.apply_delete_witness(
      withdrawal_pre_root(),
      withdrawal_delete_witness(honest, #""),
    ) == withdrawal_neighbour_only_root(),
    !withdrawal_delete_fault_is_valid(
      withdrawal_neighbour_only_root(),
      honest,
      #"",
    ),
    withdrawal_delete_fault_is_valid(h32_b, honest, #""),
  }
}

test rejects_a_tampered_terminal_leaf_key_against_an_honest_withdrawal_step() fail {
  let neighbour_path = blake2b_256(withdrawal_neighbour_key)
  let tampered = [
    Leaf {
      skip: 1,
      key: bytearray.concat(#"7e", bytearray.drop(neighbour_path, 1)),
      value: blake2b_256(neighbour_value),
    },
  ]
  withdrawal_delete_fault_is_valid(
    withdrawal_neighbour_only_root(),
    tampered,
    #"",
  )
}

test rejects_a_terminal_fork_masquerade_against_an_honest_withdrawal_step() fail {
  let neighbour_path = blake2b_256(withdrawal_neighbour_key)
  let masquerade = [
    mpf.Fork {
      skip: 1,
      neighbor: mpf.Neighbor {
        nibble: nibble(neighbour_path, 1),
        prefix: suffix(neighbour_path, 2),
        root: blake2b_256(neighbour_value),
      },
    },
  ]
  withdrawal_delete_fault_is_valid(
    withdrawal_neighbour_only_root(),
    masquerade,
    #"",
  )
}

// The spent output's only neighbour re-read as a terminal Branch: the lone
// n4 group of the spent output's slot (10) holds the neighbour's slot (14).
// The step proves the spent output is in the pre-root, and the root it
// leaves behind is a branch with one child, which the honest trie never
// holds.
fn withdrawal_branch_masquerade() -> mpf.ProofStep {
  let spent_path = blake2b_256(ledger_outref_key(out_ref(9)))
  let neighbour_path = blake2b_256(withdrawal_neighbour_key)
  let children = opening_fixture.children(
    [
      Pair(
        nibble(neighbour_path, 1),
        combine(suffix(neighbour_path, 2), blake2b_256(neighbour_value)),
      ),
    ],
  )
  mpf.Branch {
    skip: 1,
    neighbors: opening_fixture.neighbors(children, nibble(spent_path, 1)),
  }
}

test withdrawal_branch_masquerade_proves_the_spent_output() {
  let spent_path = blake2b_256(ledger_outref_key(out_ref(9)))
  let neighbour_path = blake2b_256(withdrawal_neighbour_key)
  and {
    nibble(spent_path, 1) == 10,
    nibble(neighbour_path, 1) == 14,
    mpf.has(
      mpf.from_root(withdrawal_pre_root()),
      ledger_outref_key(out_ref(9)),
      native_output_ledger_value(9),
      [withdrawal_branch_masquerade()],
    ),
  }
}

test rejects_a_terminal_branch_masquerade_against_an_honest_withdrawal_step() fail {
  withdrawal_delete_fault_is_valid(
    withdrawal_neighbour_only_root(),
    [withdrawal_branch_masquerade()],
    #"",
  )
}

// The same step with an opening that splits the neighbour's pair (14, 15)
// into the neighbour and an empty slot.
test rejects_a_terminal_branch_masquerade_with_an_opening_against_an_honest_withdrawal_step() fail {
  let neighbour_path = blake2b_256(withdrawal_neighbour_key)
  let opening = #"01"
    |> bytearray.concat(
        combine(suffix(neighbour_path, 2), blake2b_256(neighbour_value)),
      )
    |> bytearray.concat(null_hash)
  withdrawal_delete_fault_is_valid(
    withdrawal_neighbour_only_root(),
    [withdrawal_branch_masquerade()],
    opening,
  )
}

// A pre-root whose root branch holds the spent output (slot 5) and two other
// outputs (slots 8 and 9) that share its lone n8 group. The honest delete
// keeps a two-child branch and sends the group's opening.
test honest_withdrawal_delete_opens_a_lone_neighbour_group() {
  let spent_key = ledger_outref_key(out_ref(9))
  let spent_value = native_output_ledger_value(9)
  let slot_leaf = fn(slot) {
    let key = opening_fixture.slot_key(slot)
    Pair(slot, combine(suffix(blake2b_256(key), 1), blake2b_256(key)))
  }
  let children = opening_fixture.children(
    [
      Pair(
        5,
        combine(suffix(blake2b_256(spent_key), 1), blake2b_256(spent_value)),
      ),
      slot_leaf(8),
      slot_leaf(9),
    ],
  )
  let pre_root = combine(#"", opening_fixture.merkle(children))
  let post_root = combine(
    #"",
    opening_fixture.merkle(
      opening_fixture.children([slot_leaf(8), slot_leaf(9)]),
    ),
  )
  let steps = [
    mpf.Branch { skip: 0, neighbors: opening_fixture.neighbors(children, 5) },
  ]
  let opening = opening_fixture.opening(children, 5)
  and {
    nibble(blake2b_256(spent_key), 0) == 5,
    bytearray.length(opening) == 66,
    mpf.root(mpf.delete(mpf.from_root(pre_root), spent_key, spent_value, steps)) == post_root,
    proof.apply_delete_witness(
      pre_root,
      proof.LedgerDeleteWitness {
        key: spent_key,
        value: spent_value,
        opening,
        delete_proof: steps,
      },
    ) == post_root,
    !withdrawal_delete_fault_from_is_valid(pre_root, post_root, steps, opening),
    withdrawal_delete_fault_from_is_valid(pre_root, h32_b, steps, opening),
  }
}

// Without the opening the same honest delete is refused, so the prover must
// send it.
test honest_withdrawal_delete_of_a_lone_group_needs_the_opening() fail {
  let spent_key = ledger_outref_key(out_ref(9))
  let spent_value = native_output_ledger_value(9)
  let slot_leaf = fn(slot) {
    let key = opening_fixture.slot_key(slot)
    Pair(slot, combine(suffix(blake2b_256(key), 1), blake2b_256(key)))
  }
  let children = opening_fixture.children(
    [
      Pair(
        5,
        combine(suffix(blake2b_256(spent_key), 1), blake2b_256(spent_value)),
      ),
      slot_leaf(8),
      slot_leaf(9),
    ],
  )
  let pre_root = combine(#"", opening_fixture.merkle(children))
  proof.apply_delete_witness(
    pre_root,
    proof.LedgerDeleteWitness {
      key: spent_key,
      value: spent_value,
      opening: #"",
      delete_proof: [
        mpf.Branch {
          skip: 0,
          neighbors: opening_fixture.neighbors(children, 5),
        },
      ],
    },
  ) != pre_root
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
    forced_tx(verdict_plutus_failed(), #""),
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
    rejection_reason_v1.ForcedTxValid,
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
    rejection_reason_v1.ForcedTxValid,
    ledger_state.ForcedTransaction,
    h32_b,
    h32_b,
  )
  !proof.validate_accepted_transaction_fault_proof(
    prove(header, proof.AcceptedTransactionTransitionMismatch { witness }),
    asset_name_for(header),
  )
}

// An accepted claim over a transaction that deletes the ledger's only entry
// ends at the terminal root the ledger delta admits for the drain. That root is
// the committed empty-ledger sentinel, so the claim convicts a block that
// commits the library's `null_hash` for the drained ledger and does not convict
// the honest block that commits the sentinel.
test accepted_claim_names_a_drained_ledger_by_the_committed_sentinel() {
  let entry = fixtures_ledger.ledger_delta_singleton_entry_root()
  let admits = fn(root) {
    fixtures_ledger.ledger_delta_singleton_finalize(0, entry, root)
  }
  expect admits(env.empty_merkle_tree_root) != admits(null_hash)
  let terminal_root = if admits(env.empty_merkle_tree_root) {
    env.empty_merkle_tree_root
  } else {
    null_hash
  }
  let convicts = fn(post_root) {
    let Pair(header, witness) = accepted_forced_transition_case(
      rejection_reason_v1.ForcedTxValid,
      ledger_state.ForcedTransaction,
      post_root,
      terminal_root,
    )
    proof.validate_accepted_transaction_fault_proof(
      prove(header, proof.AcceptedTransactionTransitionMismatch { witness }),
      asset_name_for(header),
    )
  }
  and {
    !convicts(env.empty_merkle_tree_root),
    convicts(null_hash),
  }
}

test rejects_invalid_forced_source_with_accepted_validation_claim() {
  let Pair(header, witness) = accepted_forced_transition_case(
    verdict_plutus_failed(),
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
    rejection_reason_v1.ForcedTxValid,
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
    forced_tx(rejection_reason_v1.ForcedTxValid, #""),
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
        source_membership: proof.ForcedKeyOpening {
          tx_order_id: source_membership.key,
          value_hash: blake2b_256(cbor.serialise(source_membership.value)),
          phas_root: source_membership.phas_root,
          proof: source_membership.proof,
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
        source_membership: proof.L2KeyOpening {
          tx_id: source_membership.key,
          value_hash: blake2b_256(source_membership.value),
          phas_root: source_membership.phas_root,
          proof: source_membership.proof,
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
    forced_tx(verdict_plutus_failed(), #""),
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
          projected_utxo: ledger_insert_witness(projected_key, projected_value),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [deposit_reference_input(deposit_id, deposit_info)],
    Some(
      timing_fixture.evidence(
        header,
        [deposit_reference_input(deposit_id, deposit_info)],
      ),
    ),
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
          projected_utxo: ledger_insert_witness(projected_key, #"deadbeef"),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [deposit_reference_input(deposit_id, deposit_info)],
    Some(
      timing_fixture.evidence(
        header,
        [deposit_reference_input(deposit_id, deposit_info)],
      ),
    ),
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
          projected_utxo: ledger_insert_witness(
            projected_key,
            wrong_network_projected_value,
          ),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [deposit_reference_input(deposit_id, deposit_info)],
    Some(
      timing_fixture.evidence(
        header,
        [deposit_reference_input(deposit_id, deposit_info)],
      ),
    ),
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
          projected_utxo: ledger_insert_witness(
            projected_key,
            full_output_bytes,
          ),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [deposit_reference_input(deposit_id, deposit_info)],
    Some(
      timing_fixture.evidence(
        header,
        [deposit_reference_input(deposit_id, deposit_info)],
      ),
    ),
  )
}

// path(out_ref 0 key) = 6 f b 0 ..., path(#"0008") = 6 8 e 6 ... and
// path(#"0001") = 0 1 c f ...: the projected deposit lands beside #"0008" at
// the odd cursor 1, under a root that branches at nibble 0.
const deposit_near_key = #"0008"

const deposit_far_key = #"0001"

const far_value = #"0c"

fn deposit_far_step() -> mpf.ProofStep {
  Leaf {
    skip: 0,
    key: blake2b_256(deposit_far_key),
    value: blake2b_256(far_value),
  }
}

fn deposit_honest_insert_proof() -> mpf.Proof {
  [
    deposit_far_step(),
    Leaf {
      skip: 0,
      key: blake2b_256(deposit_near_key),
      value: blake2b_256(neighbour_value),
    },
  ]
}

fn deposit_pre_root() -> ByteArray {
  mpf.root(
    mpf.insert(
      mpf.insert(mpf.empty, deposit_far_key, far_value, []),
      deposit_near_key,
      neighbour_value,
      [deposit_far_step()],
    ),
  )
}

fn deposit_projected_value() -> ByteArray {
  ledger_value(out_ref(0).output_index, native_deposit_output_cbor(0))
}

fn deposit_honest_post_root() -> ByteArray {
  mpf.root(
    mpf.insert(
      mpf.from_root(deposit_pre_root()),
      ledger_outref_key(out_ref(0)),
      deposit_projected_value(),
      deposit_honest_insert_proof(),
    ),
  )
}

fn deposit_insert_witness(insert_proof: mpf.Proof) -> proof.LedgerInsertWitness {
  proof.LedgerInsertWitness {
    key: ledger_outref_key(out_ref(0)),
    value: deposit_projected_value(),
    non_membership_proof: deposit_honest_insert_proof(),
    insert_proof,
  }
}

fn deposit_insert_fault_is_valid(
  post_root: ByteArray,
  insert_proof: mpf.Proof,
) -> Bool {
  let deposit_id = out_ref(0)
  let deposit_info = sample_deposit_info(0)
  let event_key = ledger_state.DepositEventKey { deposit_id }
  let opened_step = step(
    0,
    event_key,
    ledger_state.Deposit,
    deposit_pre_root(),
    post_root,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Deposit,
  )
  let source_membership = deposit_source_membership(deposit_id, deposit_info)
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    deposits_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    deposit_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }
  proof.validate_transition_fault_proof(
    prove(
      header,
      proof.InvalidOneStepTransition {
        witness: proof.ValidDepositTransition {
          trace_proof,
          event_to_step,
          source_membership,
          projected_utxo: deposit_insert_witness(insert_proof),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [deposit_reference_input(deposit_id, deposit_info)],
    Some(
      timing_fixture.evidence(
        header,
        [deposit_reference_input(deposit_id, deposit_info)],
      ),
    ),
  )
}

test honest_deposit_insert_proof_resolves_to_the_honest_post_root() {
  let honest = deposit_honest_insert_proof()
  and {
    proof.apply_insert_witness(
      deposit_pre_root(),
      deposit_insert_witness(honest),
    ) == deposit_honest_post_root(),
    !deposit_insert_fault_is_valid(deposit_honest_post_root(), honest),
    deposit_insert_fault_is_valid(h32_b, honest),
  }
}

test rejects_an_inflated_terminal_leaf_skip_against_an_honest_deposit_step() fail {
  let inflated = [
    deposit_far_step(),
    Leaf {
      skip: 1,
      key: blake2b_256(deposit_near_key),
      value: blake2b_256(neighbour_value),
    },
  ]
  deposit_insert_fault_is_valid(deposit_honest_post_root(), inflated)
}

// The near leaf at odd cursor 1 re-read as a branch at nibble 0 whose prefix
// is its suffix without its marker. Leaf and branch node preimages are
// disjoint, so its collapse is not the true leaf and the predecessor check
// against the true pre-root refuses it.
test rejects_a_terminal_fork_masquerade_against_an_honest_deposit_step() fail {
  let near_path = blake2b_256(deposit_near_key)
  let masquerade = [
    deposit_far_step(),
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor {
        nibble: 0,
        prefix: bytearray.drop(suffix(near_path, 1), 1),
        root: blake2b_256(neighbour_value),
      },
    },
  ]
  deposit_insert_fault_is_valid(deposit_honest_post_root(), masquerade)
}

// Q43 structural-N/A adversarial control: a canonical valid normal L2
// transaction must apply its exact delete/insert transition. Reusing its
// authenticated pre-root as the claimed post-root makes the transition a
// no-op; the accepted-transaction final route must still convict it.
test accepts_valid_l2_transaction_no_op_transition_fault() {
  let spent_input = native_input(h32_a, 0)
  let spent_key = native_components.encode_midgard_tx_input(spent_input)
  let produced_output = native_output()
  let produced_value = ledger_value(
    0,
    native_components.encode_midgard_tx_output(produced_output),
  )
  // Both fixture outputs have index zero and identical content. Their ledger
  // keys differ by transaction id; derive their shared descriptor only once.
  let spent_value = produced_value
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
  // The only input is consumed, leaving exactly the produced output. Build
  // the expected tree independently instead of running the proof's delete
  // and insert helpers a second time as part of this measurement's fixture.
  let expected_post_root = mpf.root(
    mpf.insert(mpf.empty, produced_key, produced_value, []),
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

fn common_nibbles(left: ByteArray, right: ByteArray, cursor: Int) -> Int {
  if nibble(left, cursor) == nibble(right, cursor) {
    common_nibbles(left, right, cursor + 1)
  } else {
    cursor
  }
}

// An L2 transaction spends one output of a two-output ledger. The other
// output sits in the root slot the produced output lands in, so the honest
// spend ends in a Leaf step and the produced output then forks off that
// neighbour. Reading the neighbour as the lone group of a terminal Branch
// still proves the spend, but leaves a one-child root branch; the produced
// output's insert keeps that branch, so the replayed post-root differs from
// the honest one. `steered` picks the Branch reading for the spend and the
// matching insert witness.
fn l2_spend_branch_dispute(
  steered: Bool,
  claimed_post_root: Option<ByteArray>,
) -> Pair<ledger_state.HeaderV1, proof.TransitionFault> {
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
  let spent_path = blake2b_256(spent_key)
  let produced_path = blake2b_256(produced_key)
  let slot = nibble(produced_path, 0)
  expect nibble(spent_path, 0) != slot
  let neighbour_key = opening_fixture.slot_key(slot)
  let neighbour_path = blake2b_256(neighbour_key)
  let neighbour_value_hash = blake2b_256(neighbour_key)
  let neighbour_child = combine(suffix(neighbour_path, 1), neighbour_value_hash)
  let pre_root = combine(
    #"",
    opening_fixture.merkle(
      opening_fixture.children(
        [
          Pair(
            nibble(spent_path, 0),
            combine(suffix(spent_path, 1), blake2b_256(spent_value)),
          ),
          Pair(slot, neighbour_child),
        ],
      ),
    ),
  )
  let shared = common_nibbles(neighbour_path, produced_path, 0)
  let neighbour_leaf = fn(skip) {
    Leaf { skip, key: neighbour_path, value: neighbour_value_hash }
  }
  let honest_post_root = mpf.root(
    mpf.insert(
      mpf.insert(mpf.empty, neighbour_key, neighbour_key, []),
      produced_key,
      produced_value,
      [neighbour_leaf(shared)],
    ),
  )
  let lone_neighbour = opening_fixture.children([Pair(slot, neighbour_child)])
  let (delete_proof, insert_proof) = if steered {
    (
      [
        mpf.Branch {
          skip: 0,
          neighbors: opening_fixture.neighbors(
            lone_neighbour,
            nibble(spent_path, 0),
          ),
        },
      ],
      [
        mpf.Branch {
          skip: 0,
          neighbors: opening_fixture.neighbors(lone_neighbour, slot),
        },
        neighbour_leaf(shared - 1),
      ],
    )
  } else {
    ([neighbour_leaf(0)], [neighbour_leaf(shared)])
  }
  let post_root = when claimed_post_root is {
    Some(root) -> root
    None -> honest_post_root
  }
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let opened_step = step(
    0,
    event_key,
    ledger_state.L2Transaction,
    pre_root,
    post_root,
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
        spent_utxos: [
          proof.LedgerDeleteWitness {
            key: spent_key,
            value: spent_value,
            opening: #"",
            delete_proof,
          },
        ],
        produced_utxos: [
          proof.LedgerInsertWitness {
            key: produced_key,
            value: produced_value,
            non_membership_proof: insert_proof,
            insert_proof,
          },
        ],
      },
    },
  )
}

fn l2_spend_branch_dispute_is_valid(
  steered: Bool,
  claimed_post_root: Option<ByteArray>,
) -> Bool {
  let Pair(header, fault) = l2_spend_branch_dispute(steered, claimed_post_root)
  proof.validate_accepted_transaction_fault_proof(
    prove(header, fault),
    asset_name_for(header),
  )
}

test honest_l2_spend_of_a_one_neighbour_ledger_resolves_to_the_honest_post_root() {
  and {
    !l2_spend_branch_dispute_is_valid(False, None),
    l2_spend_branch_dispute_is_valid(False, Some(h32_b)),
  }
}

test l2_spend_branch_masquerade_proves_the_spent_output() {
  let Pair(header, fault) = l2_spend_branch_dispute(True, None)
  expect
      proof.InvalidOneStepTransition{witness: proof.L2TransactionTransition {
        trace_proof,
        spent_utxos,
        ..
      }}
    = fault
  expect [spent] = spent_utxos
  and {
    header.l2_transaction_count == 1,
    mpf.has(
      mpf.from_root(trace_proof.value.pre_utxos_root),
      spent.key,
      spent.value,
      spent.delete_proof,
    ),
  }
}

test rejects_a_terminal_branch_masquerade_against_an_honest_l2_spend() fail {
  l2_spend_branch_dispute_is_valid(True, None)
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
    proof.delete_root(pre_root, spent_key, spent_value, [], #""),
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
  let phas_root = #"df64f827b76c8b113a639af3a96beb006fb7e8105cd3a9005432473b4a204eac"
  let root = #"8c803cca83c457c9ebe70456bf8dcd622f10f3da25bcf0514181ebe44dccca56"
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
  let proof_root = empty_root_count_proof(
    transition_trace.TransitionTraceRootDomain,
  )
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

test rejects_count_fault_when_transition_step_count_matches_committed_root() {
  let header = base_header()
  !valid_against_header(
    header,
    proof.CountFault {
      witness: proof.TransitionTraceRootCountMismatch {
        proof: empty_root_count_proof(
          transition_trace.TransitionTraceRootDomain,
        ),
      },
    },
  )
}

test accepts_header_total_count_mismatch() {
  let header = ledger_state.HeaderV1 { ..base_header(), total_event_count: 1 }
  valid_against_header(
    header,
    proof.CountFault { witness: proof.HeaderTotalCountMismatch },
  )
}

test rejects_header_total_count_mismatch_when_counts_match() {
  let header = base_header()
  !valid_against_header(
    header,
    proof.CountFault { witness: proof.HeaderTotalCountMismatch },
  )
}

test accepts_header_transition_step_count_mismatch() {
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transition_step_count: 1,
  }
  valid_against_header(
    header,
    proof.CountFault { witness: proof.HeaderTransitionStepCountMismatch },
  )
}

test rejects_header_transition_step_count_mismatch_when_counts_match() {
  let header = base_header()
  !valid_against_header(
    header,
    proof.CountFault { witness: proof.HeaderTransitionStepCountMismatch },
  )
}

test accepts_withdrawals_source_root_count_mismatch() {
  let header = ledger_state.HeaderV1 { ..base_header(), withdrawal_count: 1 }
  valid_against_header(
    header,
    proof.CountFault {
      witness: proof.SourceRootCountMismatch {
        proof: empty_root_count_proof(transition_trace.WithdrawalsRootDomain),
      },
    },
  )
}

test rejects_withdrawals_source_root_count_mismatch_when_count_matches() {
  let header = base_header()
  !valid_against_header(
    header,
    proof.CountFault {
      witness: proof.SourceRootCountMismatch {
        proof: empty_root_count_proof(transition_trace.WithdrawalsRootDomain),
      },
    },
  )
}

test accepts_event_to_step_root_count_mismatch() {
  let header = ledger_state.HeaderV1 { ..base_header(), total_event_count: 1 }
  valid_against_header(
    header,
    proof.CountFault {
      witness: proof.EventToStepRootCountMismatch {
        proof: empty_root_count_proof(transition_trace.EventToStepRootDomain),
      },
    },
  )
}

test rejects_event_to_step_root_count_mismatch_when_count_matches() {
  let header = base_header()
  !valid_against_header(
    header,
    proof.CountFault {
      witness: proof.EventToStepRootCountMismatch {
        proof: empty_root_count_proof(transition_trace.EventToStepRootDomain),
      },
    },
  )
}

test accepts_omitted_due_deposit_fault() {
  let deposit_info = sample_deposit_info(0)
  let reference_input = history.order_reference(
    h28_a,
    user_events.out_ref_to_nonce(out_ref(0)),
    order_facts.DepositPayload {
      event: ledger_state.DepositEvent { id: out_ref(0), info: deposit_info },
    },
    15,
  )
  let header = base_header()

  proof.validate_transition_fault_proof(
    prove(
      header,
      proof.OmittedDueL1Event {
        witness: proof.OmittedDueDeposit {
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
    Some(timing_fixture.evidence(header, [reference_input])),
  )
}

test accepts_omitted_withdrawal_at_authenticated_end_time_boundary() {
  let header = base_header()
  omitted_withdrawal_event_is_valid(
    header,
    header.end_time,
    empty_non_membership(transition_trace.WithdrawalsRootDomain, out_ref(0)),
  )
}

test rejects_omitted_withdrawal_at_excluded_start_time_boundary() {
  let header = base_header()
  !omitted_withdrawal_event_is_valid(
    header,
    header.start_time,
    empty_non_membership(transition_trace.WithdrawalsRootDomain, out_ref(0)),
  )
}

test rejects_omitted_withdrawal_published_after_challenged_block() {
  let header = base_header()
  !omitted_withdrawal_event_is_valid(
    header,
    header.end_time + 1,
    empty_non_membership(transition_trace.WithdrawalsRootDomain, out_ref(0)),
  )
}

test rejects_omitted_withdrawal_already_committed_in_source_root() {
  let source_membership = withdrawal_source_membership(
    out_ref(0),
    valid_withdrawal_info(),
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    withdrawals_root: source_membership.root,
    withdrawal_count: 1,
  }
  !omitted_withdrawal_event_is_valid(
    header,
    header.end_time,
    empty_non_membership(transition_trace.WithdrawalsRootDomain, out_ref(0)),
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
    authenticated_forced_tx(rejection_reason_v1.ForcedTxValid),
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
          validity_override: rejection_reason_v1.ForcedTxValid,
          source_membership,
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    timing_fixture.evidence(
      header,
      [tx_order_reference_input(header.end_time + 1, event_asset_name)],
    ),
  )
}

// The early side of the forced due rule `start_time < inclusion_time <=
// end_time`: an order at or before `start_time` is out of window, one
// millisecond after it is due.
test accepts_forced_order_at_start_time_as_out_of_window_source_event() {
  included_interval_forced_order_is_out_of_window(
    -1,
    -1,
    slot_domain_header().start_time,
  )
}

test accepts_forced_order_before_start_time_as_out_of_window_source_event() {
  included_interval_forced_order_is_out_of_window(
    -1,
    -1,
    slot_domain_header().start_time - 1,
  )
}

test rejects_forced_order_just_after_start_time_as_out_of_window_source_event() {
  !included_interval_forced_order_is_out_of_window(
    -1,
    -1,
    slot_domain_header().start_time + 1,
  )
}

// Forced-order due-ness in the real time domains: header event times are
// POSIX milliseconds, native validity scalars are slots, and `block_slot` is
// the slot the validation machine checks the interval against. Due-ness is the
// inclusion window alone; the interval decides only the machine verdict, so an
// order whose interval excludes `block_slot` is still due, and the honest
// block includes it with a `ValidityIntervalExcludesBlockSlot` verdict.
const slot_domain_block_slot: Int = 70_000_000

fn slot_domain_header() -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    ..base_header(),
    start_time: 1_700_000_000_000,
    end_time: 1_700_000_020_000,
    block_slot: slot_domain_block_slot,
  }
}

/// `OmittedDueForcedTransaction` against an empty forced root, for an order
/// with the given interval and inclusion time.
fn omitted_interval_forced_order_is_valid(
  validity_interval_start: Int,
  validity_interval_end: Int,
  inclusion_time: Int,
) -> Bool {
  let header = slot_domain_header()
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OmittedDueL1Event {
        witness: proof.OmittedDueForcedTransaction {
          event_ref_input_index: 0,
          event_asset_name: #"01",
          validity_override: rejection_reason_v1.ForcedTxValid,
          source_non_membership: empty_non_membership(
            transition_trace.ForcedTransactionsV1RootDomain,
            out_ref(0),
          ),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    timing_fixture.evidence(
      header,
      [
        tx_order_reference_input_for(
          inclusion_time,
          #"01",
          authenticated_tx_order_payload_with_interval(
            validity_interval_start,
            validity_interval_end,
          ),
        ),
      ],
    ),
  )
}

/// `OutOfWindowForcedTransaction` against a block that includes the order with
/// the given interval and inclusion time.
fn included_interval_forced_order_is_out_of_window(
  validity_interval_start: Int,
  validity_interval_end: Int,
  inclusion_time: Int,
) -> Bool {
  let source_membership = forced_source_membership(
    out_ref(0),
    authenticated_forced_tx_with_interval(
      rejection_reason_v1.ForcedTxValid,
      validity_interval_start,
      validity_interval_end,
    ),
  )
  let header = ledger_state.HeaderV1 {
    ..slot_domain_header(),
    forced_transactions_root: source_membership.root,
    forced_transaction_count: 1,
  }
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OutOfWindowSourceEvent {
        witness: proof.OutOfWindowForcedTransaction {
          event_ref_input_index: 0,
          event_asset_name: #"01",
          validity_override: rejection_reason_v1.ForcedTxValid,
          source_membership,
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    timing_fixture.evidence(
      header,
      [
        tx_order_reference_input_for(
          inclusion_time,
          #"01",
          authenticated_tx_order_payload_with_interval(
            validity_interval_start,
            validity_interval_end,
          ),
        ),
      ],
    ),
  )
}

/// Both polarities for one interval: inside the inclusion window the order is
/// due (omission convicts, inclusion does not); one millisecond past the window
/// it is not (inclusion convicts, omission does not).
fn interval_forced_order_due_by_inclusion_time_only(
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> Bool {
  let header = slot_domain_header()
  let inside = header.end_time
  let after = header.end_time + 1
  and {
    omitted_interval_forced_order_is_valid(
      validity_interval_start,
      validity_interval_end,
      inside,
    ),
    !included_interval_forced_order_is_out_of_window(
      validity_interval_start,
      validity_interval_end,
      inside,
    ),
    !omitted_interval_forced_order_is_valid(
      validity_interval_start,
      validity_interval_end,
      after,
    ),
    included_interval_forced_order_is_out_of_window(
      validity_interval_start,
      validity_interval_end,
      after,
    ),
  }
}

test forced_order_with_bounded_interval_containing_block_slot_is_due_by_inclusion_time() {
  interval_forced_order_due_by_inclusion_time_only(
    slot_domain_block_slot - 100,
    slot_domain_block_slot + 100,
  )
}

test forced_order_with_bounded_interval_ending_before_block_slot_is_due_by_inclusion_time() {
  interval_forced_order_due_by_inclusion_time_only(
    slot_domain_block_slot - 200,
    slot_domain_block_slot - 100,
  )
}

test forced_order_with_bounded_interval_starting_after_block_slot_is_due_by_inclusion_time() {
  interval_forced_order_due_by_inclusion_time_only(
    slot_domain_block_slot + 100,
    slot_domain_block_slot + 200,
  )
}

test forced_order_with_open_start_interval_is_due_by_inclusion_time() {
  interval_forced_order_due_by_inclusion_time_only(
    -1,
    slot_domain_block_slot - 100,
  )
}

test forced_order_with_open_end_interval_is_due_by_inclusion_time() {
  interval_forced_order_due_by_inclusion_time_only(
    slot_domain_block_slot + 100,
    -1,
  )
}

test forced_order_with_unbounded_interval_is_due_by_inclusion_time() {
  interval_forced_order_due_by_inclusion_time_only(-1, -1)
}

// A scalar below -1 decodes and authenticates at order mint; the machine
// rejects it as `ValidityIntervalMalformed`, so the order is still due.
test forced_order_with_malformed_negative_interval_is_due_by_inclusion_time() {
  interval_forced_order_due_by_inclusion_time_only(-2, 5)
}

test forced_order_with_inverted_interval_is_due_by_inclusion_time() {
  interval_forced_order_due_by_inclusion_time_only(
    slot_domain_block_slot + 100,
    slot_domain_block_slot - 100,
  )
}

fn late_rejected_forced_source_is_valid(
  mutate_witness: Bool,
  mutate_reason: Bool,
) -> Bool {
  let original = authenticated_forced_tx(verdict_plutus_failed())
  let source_membership = forced_source_membership(
    out_ref(0),
    ledger_state.ForcedInclusionTxV1 {
      ..original,
      submitted_source: ledger_state.ForcedTxProofSourceV1 {
        ..original.submitted_source,
        witness_set_compact_cbor: if mutate_witness {
          #"80"
        } else {
          original.submitted_source.witness_set_compact_cbor
        },
      },
    },
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
          event_asset_name: #"01",
          validity_override: if mutate_reason {
            rejection_reason_v1.ForcedTxInvalid {
              reason: rejection_reason_v1.PlutusExecutionFailed {
                execution_index: 1,
              },
            }
          } else {
            verdict_plutus_failed()
          },
          source_membership,
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    timing_fixture.evidence(
      header,
      [tx_order_reference_input(header.end_time + 1, #"01")],
    ),
  )
}

test accepts_late_rejected_forced_order_from_submitted_valid_bytes() {
  late_rejected_forced_source_is_valid(False, False)
}

test rejects_late_rejected_forced_order_with_changed_witness_source() {
  !late_rejected_forced_source_is_valid(True, False)
}

test rejects_late_rejected_forced_order_with_changed_verdict() {
  !late_rejected_forced_source_is_valid(False, True)
}

test accepts_late_withdrawal_as_out_of_window_source_event() {
  out_of_window_withdrawal_is_valid(
    base_header().end_time + 1,
    False,
    ledger_state.IncorrectWithdrawalSignature,
  )
}

test rejects_in_window_withdrawal_as_out_of_window_source_event() {
  !out_of_window_withdrawal_is_valid(
    base_header().end_time,
    False,
    ledger_state.IncorrectWithdrawalSignature,
  )
}

test rejects_out_of_window_withdrawal_with_mismatched_submitted_body() {
  !out_of_window_withdrawal_is_valid(
    base_header().end_time + 1,
    True,
    ledger_state.WithdrawalIsValid,
  )
}

test accepts_out_of_window_deposit_fault() {
  let deposit_info = sample_deposit_info(0)
  let source_membership = deposit_source_membership(out_ref(0), deposit_info)
  let reference_input = history.order_reference(
    h28_a,
    user_events.out_ref_to_nonce(out_ref(0)),
    order_facts.DepositPayload {
      event: ledger_state.DepositEvent { id: out_ref(0), info: deposit_info },
    },
    30,
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    deposits_root: source_membership.root,
    deposit_count: 1,
  }

  proof.validate_transition_fault_proof(
    prove(
      header,
      proof.OutOfWindowSourceEvent {
        witness: proof.OutOfWindowDeposit { source_membership },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [reference_input],
    Some(timing_fixture.evidence(header, [reference_input])),
  )
}

fn n2_source() -> proof.SourceKeyOpening {
  let membership = raw_l2_source_membership(h32_a, #"d87980")
  proof.L2KeyOpening {
    tx_id: h32_a,
    value_hash: blake2b_256(membership.value),
    phas_root: membership.phas_root,
    proof: [],
  }
}

fn n2_descriptor() -> validation_trace_v1.ValidationTraceDescriptorV1 {
  validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: 1,
    machine_version: 1,
    trace_root: h32_a,
    step_count: 1,
    initial_state_hash: h32_b,
    terminal_state_hash: h32_c,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: zero_hash,
  }
}

fn n2_run_root(event_key: ledger_state.EventKey, value: ByteArray) -> ByteArray {
  mpf.root(mpf.insert(mpf.empty, cbor.serialise(event_key), value, []))
}

fn n2_header(run_root: ByteArray) -> ledger_state.HeaderV1 {
  let source = raw_l2_source_membership(h32_a, #"d87980")
  ledger_state.HeaderV1 {
    ..base_header(),
    transactions_root: source.root,
    l2_transaction_count: 1,
    validation_trace_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
    validation_traces_root: transition_trace.commit_counted_root(
      transition_trace.ValidationTracesRootDomain,
      run_root,
      1,
    ),
  }
}

fn n2_missing_fault(
  run_root: ByteArray,
  absence: mpf.Proof,
) -> proof.TransitionFault {
  proof.SourceMembershipMismatch {
    witness: proof.SourceEventMissingValidationRun {
      source: n2_source(),
      run_phas_root: run_root,
      run_absence_proof: absence,
    },
  }
}

test n2_missing_run_convicts_committed_source() {
  // A distinct run is present under the authenticated count label.
  let other = ledger_state.L2TransactionEventKey { tx_id: h32_b }
  let value = cbor.serialise(n2_descriptor())
  let root = n2_run_root(other, value)
  let path = blake2b_256(cbor.serialise(other))
  let absence = [mpf.Leaf { skip: 0, key: path, value: blake2b_256(value) }]
  proof.validate_source_fault_proof(
    prove(n2_header(root), n2_missing_fault(root, absence)),
    asset_name_for(n2_header(root)),
  )
}

test n2_present_run_refuses_missing_claim() fail {
  let root = n2_run_root(
    ledger_state.L2TransactionEventKey { tx_id: h32_a },
    cbor.serialise(n2_descriptor()),
  )
  proof.validate_source_fault_proof(
    prove(n2_header(root), n2_missing_fault(root, [])),
    asset_name_for(n2_header(root)),
  )
}

test n2_foreign_run_convicts() {
  let key = ledger_state.L2TransactionEventKey { tx_id: h32_b }
  let value = cbor.serialise(n2_descriptor())
  let root = n2_run_root(key, value)
  let source = raw_l2_source_membership(h32_a, #"d87980")
  let absence = [
    mpf.Leaf {
      skip: 0,
      key: blake2b_256(h32_a),
      value: blake2b_256(source.value),
    },
  ]
  let header = n2_header(root)
  proof.validate_source_fault_proof(
    prove(
      header,
      proof.SourceMembershipMismatch {
        witness: proof.ForeignValidationRun {
          event_key: cbor.serialise(key),
          value_hash: blake2b_256(value),
          run_phas_root: root,
          run_proof: [],
          source_phas_root: source.phas_root,
          source_absence_proof: absence,
        },
      },
    ),
    asset_name_for(header),
  )
}

test n2_existing_source_refuses_foreign_claim() fail {
  let key = ledger_state.L2TransactionEventKey { tx_id: h32_a }
  let value = cbor.serialise(n2_descriptor())
  let root = n2_run_root(key, value)
  let source = raw_l2_source_membership(h32_a, #"d87980")
  let header = n2_header(root)
  proof.validate_source_fault_proof(
    prove(
      header,
      proof.SourceMembershipMismatch {
        witness: proof.ForeignValidationRun {
          event_key: cbor.serialise(key),
          value_hash: blake2b_256(value),
          run_phas_root: root,
          run_proof: [],
          source_phas_root: source.phas_root,
          source_absence_proof: [],
        },
      },
    ),
    asset_name_for(header),
  )
}

fn n2_malformed_claim(value: ByteArray) -> Bool {
  let key = ledger_state.L2TransactionEventKey { tx_id: h32_a }
  let root = n2_run_root(key, value)
  let header = n2_header(root)
  proof.validate_source_fault_proof(
    prove(
      header,
      proof.SourceMembershipMismatch {
        witness: proof.MalformedValidationRun {
          event_key: key,
          value,
          run_phas_root: root,
          run_proof: [],
        },
      },
    ),
    asset_name_for(header),
  )
}

test n2_garbled_run_convicts() {
  n2_malformed_claim(#"ff")
}

test n2_zero_step_run_convicts() {
  n2_malformed_claim(
    cbor.serialise(
      validation_trace_v1.ValidationTraceDescriptorV1 {
        ..n2_descriptor(),
        step_count: 0,
      },
    ),
  )
}

test n2_honest_descriptor_refuses_malformed_claim() fail {
  n2_malformed_claim(cbor.serialise(n2_descriptor()))
}

test n2_descriptor_decoder_is_total_over_wrong_data_shapes() {
  list.all(
    [
      #"ff", #"00", #"40", #"80", #"a0", #"d87980",
      #"d8799f0101582000000000000000000000000000000000000000000000000000000000000000000140400040ff",
    ],
    fn(bytes) { !validation_trace_v1.descriptor_bytes_are_well_formed(bytes) },
  )
}

// The value digest cannot be rebound to another event, root or count label.
test n2_source_key_opening_is_exact() {
  let root = n2_run_root(
    ledger_state.L2TransactionEventKey { tx_id: h32_b },
    cbor.serialise(n2_descriptor()),
  )
  let header = n2_header(root)
  let source = raw_l2_source_membership(h32_a, #"d87980")
  let forged = proof.L2KeyOpening {
    tx_id: h32_c,
    value_hash: blake2b_256(source.value),
    phas_root: source.phas_root,
    proof: [],
  }
  !proof.validate_source_fault_proof(
    prove(
      header,
      proof.SourceMembershipMismatch {
        witness: proof.SourceEventMissingValidationRun {
          source: forged,
          run_phas_root: root,
          run_absence_proof: [],
        },
      },
    ),
    asset_name_for(header),
  )
}

test n2_nontransaction_run_key_is_foreign() {
  let key = ledger_state.WithdrawalEventKey { withdrawal_id: out_ref(0) }
  let value = cbor.serialise(n2_descriptor())
  let root = n2_run_root(key, value)
  let header = n2_header(root)
  proof.validate_source_fault_proof(
    prove(
      header,
      proof.SourceMembershipMismatch {
        witness: proof.ForeignValidationRun {
          event_key: cbor.serialise(key),
          value_hash: blake2b_256(value),
          run_phas_root: root,
          run_proof: [],
          source_phas_root: env.empty_merkle_tree_root,
          source_absence_proof: [],
        },
      },
    ),
    asset_name_for(header),
  )
}

test n2_descriptor_decoder_refuses_wrong_fields_and_versions() {
  let honest = n2_descriptor()
  and {
    validation_trace_v1.descriptor_bytes_are_well_formed(cbor.serialise(honest)),
    !validation_trace_v1.descriptor_bytes_are_well_formed(
      cbor.serialise(
        validation_trace_v1.ValidationTraceDescriptorV1 {
          ..honest,
          machine_version: 2,
        },
      ),
    ),
    !validation_trace_v1.descriptor_bytes_are_well_formed(
      cbor.serialise(
        validation_trace_v1.ValidationTraceDescriptorV1 {
          ..honest,
          schema_version: 2,
        },
      ),
    ),
    !validation_trace_v1.descriptor_bytes_are_well_formed(
      cbor.serialise(
        validation_trace_v1.ValidationTraceDescriptorV1 {
          ..honest,
          trace_root: #"00",
        },
      ),
    ),
    !validation_trace_v1.descriptor_bytes_are_well_formed(
      cbor.serialise(
        validation_trace_v1.ValidationTraceDescriptorV1 {
          ..honest,
          verdict: validation_trace_v1.Pending,
        },
      ),
    ),
    !validation_trace_v1.descriptor_bytes_are_well_formed(
      cbor.serialise(
        validation_trace_v1.ValidationTraceDescriptorV1 {
          ..honest,
          step_count: 4_294_967_296,
        },
      ),
    ),
    !validation_trace_v1.descriptor_bytes_are_well_formed(
      cbor.serialise(
        validation_trace_v1.ValidationTraceDescriptorV1 {
          ..honest,
          verdict: validation_trace_v1.Rejected,
        },
      ),
    ),
  }
}

test n2_all_runs_missing_is_provable() {
  let header = n2_header(env.empty_merkle_tree_root)
  proof.validate_source_fault_proof(
    prove(header, n2_missing_fault(env.empty_merkle_tree_root, [])),
    asset_name_for(header),
  )
}

test n2_bignum_leaf_is_malformed_and_provable() {
  n2_malformed_claim(#"c24100")
}

fn n2_extra_raw_key_is_provable(key: ByteArray) -> Bool {
  let legitimate_key = cbor.serialise(
    ledger_state.L2TransactionEventKey { tx_id: h32_a },
  )
  let value = cbor.serialise(n2_descriptor())
  let legitimate_path = blake2b_256(legitimate_key)
  let neighbor = [
    mpf.Leaf { skip: 0, key: legitimate_path, value: blake2b_256(value) },
  ]
  let legitimate = mpf.insert(mpf.empty, legitimate_key, value, [])
  let root = mpf.root(mpf.insert(legitimate, key, value, neighbor))
  let header = n2_header(root)
  proof.validate_source_fault_proof(
    prove(
      header,
      proof.SourceMembershipMismatch {
        witness: proof.ForeignValidationRun {
          event_key: key,
          value_hash: blake2b_256(value),
          run_phas_root: root,
          run_proof: neighbor,
          source_phas_root: env.empty_merkle_tree_root,
          source_absence_proof: [],
        },
      },
    ),
    asset_name_for(header),
  )
}

test n2_extra_garbled_run_key_is_foreign_despite_honest_count_label() {
  n2_extra_raw_key_is_provable(#"00")
}

test n2_extra_bignum_run_key_is_foreign_without_decoder_abort() {
  n2_extra_raw_key_is_provable(#"c24100")
}

fn n2_forced_foreign_claim(existing: Bool) -> Bool {
  let event_key = ledger_state.ForcedTransactionEventKey {
    tx_order_id: out_ref(0),
  }
  let value = cbor.serialise(n2_descriptor())
  let run_root = n2_run_root(event_key, value)
  let source = forced_source_membership(
    out_ref(0),
    forced_tx(verdict_plutus_failed(), #""),
  )
  let header = ledger_state.HeaderV1 {
    ..n2_header(run_root),
    forced_transactions_root: if existing {
      source.root
    } else {
      env.empty_merkle_tree_root
    },
    forced_transaction_count: if existing {
      1
    } else {
      0
    },
  }
  proof.validate_source_fault_proof(
    prove(
      header,
      proof.SourceMembershipMismatch {
        witness: proof.ForeignValidationRun {
          event_key: cbor.serialise(event_key),
          value_hash: blake2b_256(value),
          run_phas_root: run_root,
          run_proof: [],
          source_phas_root: if existing {
            source.phas_root
          } else {
            env.empty_merkle_tree_root
          },
          source_absence_proof: [],
        },
      },
    ),
    asset_name_for(header),
  )
}

test n2_forced_foreign_run_convicts() {
  n2_forced_foreign_claim(False)
}

test n2_existing_forced_source_refuses_foreign_claim() fail {
  n2_forced_foreign_claim(True)
}

test n2_forced_source_missing_run_convicts() {
  let source = forced_source_membership(
    out_ref(0),
    forced_tx(verdict_plutus_failed(), #""),
  )
  let header = ledger_state.HeaderV1 {
    ..n2_header(env.empty_merkle_tree_root),
    forced_transactions_root: source.root,
    forced_transaction_count: 1,
  }
  proof.validate_source_fault_proof(
    prove(
      header,
      proof.SourceMembershipMismatch {
        witness: proof.SourceEventMissingValidationRun {
          source: proof.ForcedKeyOpening {
            tx_order_id: source.key,
            value_hash: blake2b_256(cbor.serialise(source.value)),
            phas_root: source.phas_root,
            proof: [],
          },
          run_phas_root: env.empty_merkle_tree_root,
          run_absence_proof: [],
        },
      },
    ),
    asset_name_for(header),
  )
}

test n2_fixed_descriptor_wire_matches_typed_codec_at_all_count_width_boundaries() {
  list.all(
    [-1, 0, 1, 23, 24, 255, 256, 65_535, 65_536, 4_294_967_295, 4_294_967_296],
    fn(count) {
      list.all(
        [
          validation_trace_v1.Pending,
          validation_trace_v1.Accepted,
          validation_trace_v1.Rejected,
        ],
        fn(verdict) {
          list.all(
            [zero_hash, h32_a],
            fn(rejection) {
              let descriptor = validation_trace_v1.ValidationTraceDescriptorV1 {
                ..n2_descriptor(),
                step_count: count,
                verdict,
                rejection_code_hash: rejection,
              }
              validation_trace_v1.descriptor_bytes_are_well_formed(
                cbor.serialise(descriptor),
              ) == validation_trace_v1.descriptor_is_well_formed(descriptor)
            },
          )
        },
      )
    },
  )
}

test n2_fixed_descriptor_wire_refuses_nonminimal_count_spellings() {
  let bytes = cbor.serialise(n2_descriptor())
  let prefix = builtin.slice_bytearray(0, 39, bytes)
  let suffix = builtin.slice_bytearray(
    40,
    builtin.length_of_bytearray(bytes) - 40,
    bytes,
  )
  list.all(
    [#"1801", #"190001", #"1a00000001", #"1b0000000000000001", #"c24101"],
    fn(count) {
      !validation_trace_v1.descriptor_bytes_are_well_formed(
        builtin.append_bytearray(
          prefix,
          builtin.append_bytearray(count, suffix),
        ),
      )
    },
  )
}
