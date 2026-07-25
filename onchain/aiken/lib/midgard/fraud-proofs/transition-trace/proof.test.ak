use aiken/cbor
use aiken/merkle_patricia_forestry.{Leaf}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, OutputReference}
use env
use midgard/bounded_blob_v1
use midgard/common/utils
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/components as native_components
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/transition_trace/proof
use midgard/hub_oracle
use midgard/ledger_state
use midgard/transition_trace
use midgard/user_events/deposit

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

fn raw_l2_source_membership(
  tx_id: ByteArray,
  compact_cbor: ByteArray,
) -> transition_trace.RootMembershipProof<ByteArray, ByteArray> {
  let phas_root = mpf.root(mpf.insert(mpf.empty, tx_id, compact_cbor, []))
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
    value: compact_cbor,
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
  let deposit_datum = deposit.Datum {
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
    transaction_commitment: h32_b,
    source: ledger_state.NativeTxProofSourceV1 {
      compact_cbor,
      witness_set_compact_cbor: #"",
      field_preimage_lengths_cbor: #"",
    },
    operator_validity: validity,
  }
}

fn native_l2_compact_cbor(
  spend_inputs_preimage: ByteArray,
  outputs_preimage: ByteArray,
) -> ByteArray {
  native_compact.encode_native_tx_compact_v1(
    native_types.NativeTxCompact {
      body: native_types.NativeTxBodyCompact {
        spend_inputs_hash: bounded_blob_v1.from_preimage(
          0,
          spend_inputs_preimage,
        ),
        reference_inputs_hash: h32_b,
        outputs_hash: bounded_blob_v1.from_preimage(2, outputs_preimage),
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
      witness_set_hash: h32_c,
      validity_code: 0,
    },
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
  let source_membership = raw_l2_source_membership(
    tx_id,
    native_l2_compact_cbor(h32_a, h32_b),
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
  let spent_value = native_output_cbor()
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
  let spent_value = native_output_cbor()
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
  let projected_value = native_deposit_output_cbor(0)
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
  let wrong_network_projected_value = native_deposit_output_cbor(1)
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
  let deposit_datum = deposit.Datum {
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

test accepts_out_of_window_deposit_fault() {
  let deposit_info = sample_deposit_info(0)
  let deposit_datum = deposit.Datum {
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
