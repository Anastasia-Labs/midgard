//// VerifySource, driven through the deployed validator: the C1 order (source
//// authenticated, R-DESC, the challenger's claim, N5, the open guard, the
//// bond), each in both polarities, and the case output it starts.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/interval
use aiken/merkle_patricia_forestry as mpf
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use fraud_proofs/validation_trace/source_v1 as source_validator
use midgard/computation_thread as ct
use midgard/dispute_bond_v1.{DisputeBondV1}
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/thread_census_fixture_v1 as census
use midgard/ledger_state
use midgard/native_tx_field_access_v1
use midgard/rejection_reason_v1
use midgard/transition_trace
use midgard/validation_case_v1
use midgard/validation_claim_v1
use midgard/validation_dispute_v1
use midgard/validation_game_v1
use midgard/validation_machine/witness as vm_witness
use midgard/validation_trace_v1

const case_hash = #"01010101010101010101010101010101010101010101010101010101"

const bond_hash = #"03030303030303030303030303030303030303030303030303030303"

const source_hash = #"04040404040404040404040404040404040404040404040404040404"

const thread_policy =
  #"05050505050505050505050505050505050505050505050505050505"

const thread_name = #"0606"

const fraud_prover = #"07070707070707070707070707070707070707070707070707070707"

const header_hash = #"08080808080808080808080808080808080808080808080808080808"

const bond_lovelace = 10_000_000_000

const thread_lovelace = 5_000_000

const fee = 400_000

const end_time = 1_000_000

/// The node's `block_time`, later than the header's `end_time`.
const block_end = 1_005_000

const h28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h32_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const h32_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const h32_c =
  #"3333333333333333333333333333333333333333333333333333333333333333"

const zero_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const step_count = 10

// ## The source transaction and its membership

fn native_tx(redeemers: Int) -> native_types.MidgardTransaction {
  let empty_mint: Data = []
  let redeemer = native_types.MidgardRedeemerWitness {
    purpose: native_types.SpendRedeemer,
    index: 0,
    redeemer_cbor: cbor.serialise(42),
    execution_units: native_types.MidgardExecutionUnits {
      memory: 10,
      steps: 20,
    },
  }
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
      redeemer_tx_wits: if redeemers == 0 {
        []
      } else {
        [redeemer]
      },
    },
  }
}

fn native_tx_id_of(tx: native_types.MidgardTransaction) -> ByteArray {
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  native_compact.native_tx_id_for_version(
    native_types.native_tx_version_v1,
    native_compact.encode_native_tx_body_compact(compact.body),
  )
}

fn proof_source_of(
  tx: native_types.MidgardTransaction,
) -> ledger_state.NativeTxProofSourceV1 {
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  ledger_state.NativeTxProofSourceV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
  }
}

fn tx_order_id() -> ledger_state.TxOrderId {
  OutputReference { transaction_id: h32_a, output_index: 0 }
}

fn normal_source(
  redeemers: Int,
) -> validation_claim_v1.ValidationSourceMembershipV1 {
  let tx = native_tx(redeemers)
  let tx_id = native_tx_id_of(tx)
  let value = ledger_state.L2TransactionSourceV1 {
    tx_id,
    source: proof_source_of(tx),
  }
  let phas_root = mpf.root(
    mpf.insert(mpf.empty, tx_id, cbor.serialise(value), []),
  )
  validation_claim_v1.NormalValidationSource {
    membership: transition_trace.RootMembershipProof {
      domain: transition_trace.TransactionsV1RootDomain,
      root: transition_trace.commit_counted_root(
        transition_trace.TransactionsV1RootDomain,
        phas_root,
        1,
      ),
      phas_root,
      count: 1,
      key: tx_id,
      value,
      proof: [],
    },
  }
}

fn forced_source(
  verdict: rejection_reason_v1.OperatorVerdictV1,
) -> validation_claim_v1.ValidationSourceMembershipV1 {
  let tx = native_tx(1)
  let value = ledger_state.ForcedInclusionTxV1 {
    tx_id: native_tx_id_of(tx),
    submitted_source: forced_fixture.from_native_source(proof_source_of(tx)),
    verdict,
  }
  let phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      cbor.serialise(tx_order_id()),
      cbor.serialise(value),
      [],
    ),
  )
  validation_claim_v1.ForcedValidationSource {
    membership: transition_trace.RootMembershipProof {
      domain: transition_trace.ForcedTransactionsV1RootDomain,
      root: transition_trace.commit_counted_root(
        transition_trace.ForcedTransactionsV1RootDomain,
        phas_root,
        1,
      ),
      phas_root,
      count: 1,
      key: tx_order_id(),
      value,
      proof: [],
    },
  }
}

fn header_for(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> ledger_state.HeaderV1 {
  let base = ledger_state.HeaderV1 {
    prev_utxos_root: h32_a,
    utxos_root: h32_b,
    withdrawals_root: zero_hash,
    forced_transactions_root: zero_hash,
    transactions_root: zero_hash,
    deposits_root: zero_hash,
    transition_trace_root: zero_hash,
    event_to_step_root: zero_hash,
    validation_traces_root: zero_hash,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: end_time - 1_000,
    end_time,
    block_slot: 7,
    expected_network_id: 0,
    min_fee_a: 44,
    min_fee_b: 155_381,
    prev_header_hash: h28_a,
    operator_vkey: h28_a,
    protocol_version: ledger_state.protocol_version_v1,
  }
  when source is {
    validation_claim_v1.ForcedValidationSource { membership } ->
      ledger_state.HeaderV1 {
        ..base,
        forced_transactions_root: membership.root,
        forced_transaction_count: 1,
      }
    validation_claim_v1.NormalValidationSource { membership } ->
      ledger_state.HeaderV1 {
        ..base,
        transactions_root: membership.root,
        l2_transaction_count: 1,
      }
  }
}

fn event_key_for(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> ledger_state.EventKey {
  when source is {
    validation_claim_v1.ForcedValidationSource { .. } ->
      ledger_state.ForcedTransactionEventKey { tx_order_id: tx_order_id() }
    validation_claim_v1.NormalValidationSource { membership } ->
      ledger_state.L2TransactionEventKey { tx_id: membership.key }
  }
}

// ## The committed claim

fn context_cbor(header: ledger_state.HeaderV1) -> ByteArray {
  let context: Data = [
    builtin.i_data(1),
    builtin.b_data(#"6d6964676172642d636f6e73656e7375732d7631"),
    builtin.i_data(header.end_time),
    builtin.i_data(header.expected_network_id),
    builtin.i_data(header.min_fee_a),
    builtin.i_data(header.min_fee_b),
    builtin.i_data(header.block_slot),
  ]
  cbor.serialise(context)
}

/// The initial state derivation builds for a normal source: the one the
/// operator must commit.
fn derived_initial_state(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> validation_trace_v1.ValidationMachineStateV1 {
  expect validation_claim_v1.NormalValidationSource { membership } = source
  let proof_source = membership.value.source
  let context = context_cbor(header_for(source))
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: blake2b_256(cbor.serialise(event_key_for(source))),
    transaction_id: membership.value.tx_id,
    transaction_commitment: native_compact.native_tx_proof_commitment_v1(
      proof_source.compact_cbor,
      proof_source.witness_set_compact_cbor,
      proof_source.field_preimage_lengths_cbor,
    ),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      context,
    ),
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: h32_a,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 0,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      0,
      vm_witness.encode_transaction_field_scan_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        context,
        0,
        0,
        0,
        -1,
        0,
      ),
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero_hash,
  }
}

fn placeholder_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: zero_hash,
    transaction_id: zero_hash,
    transaction_commitment: zero_hash,
    validation_context_hash: zero_hash,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: zero_hash,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 0,
    work_root: zero_hash,
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero_hash,
  }
}

/// A Terminal state at `step_count` carrying `verdict` and its rejection hash.
fn terminal_state(
  verdict: validation_trace_v1.ValidationVerdict,
  rejection_code_hash: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..placeholder_state(),
    phase: validation_trace_v1.Terminal,
    program_counter: step_count,
    verdict,
    rejection_code_hash,
  }
}

fn claim(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> validation_claim_v1.ValidationClaimWitnessV1 {
  let event_key = event_key_for(source)
  let phase = when source is {
    validation_claim_v1.ForcedValidationSource { .. } ->
      ledger_state.ForcedTransaction
    validation_claim_v1.NormalValidationSource { .. } ->
      ledger_state.L2Transaction
  }
  validation_claim_v1.ValidationClaimWitnessV1 {
    version: validation_claim_v1.claim_version,
    transition_step_membership: transition_trace.RootMembershipProof {
      domain: transition_trace.TransitionTraceRootDomain,
      root: zero_hash,
      phas_root: zero_hash,
      count: 1,
      key: 0,
      value: ledger_state.TransitionStep {
        schema_version: ledger_state.transition_step_schema_version_v1,
        step_index: 0,
        event_key,
        phase,
        pre_utxos_root: h32_a,
        post_utxos_root: h32_b,
      },
      proof: [],
    },
    event_to_step_membership: transition_trace.RootMembershipProof {
      domain: transition_trace.EventToStepRootDomain,
      root: zero_hash,
      phas_root: zero_hash,
      count: 1,
      key: event_key,
      value: ledger_state.EventToStepValue { step_index: 0, phase },
      proof: [],
    },
    source_membership: source,
  }
}

fn pending(
  source: validation_claim_v1.ValidationSourceMembershipV1,
  challenger_root: ByteArray,
) -> validation_game_v1.PendingValidationClaimV1 {
  validation_game_v1.PendingValidationClaimV1 {
    challenged_header_hash: header_hash,
    challenged_header: header_for(source),
    claim: claim(source),
    challenger_root,
    step_count,
    block_end,
  }
}

// The honest case: a normal Plutus source, its derived initial state and a
// challenger claiming rejection at S.

fn honest_source() -> validation_claim_v1.ValidationSourceMembershipV1 {
  normal_source(1)
}

fn honest_initial() -> validation_trace_v1.ValidationMachineStateV1 {
  derived_initial_state(honest_source())
}

// ## The challenger's claim: a Terminal Rejected state at S, proven as leaf S

fn claimed_terminal() -> validation_trace_v1.ValidationMachineStateV1 {
  terminal_state(validation_trace_v1.Rejected, h32_a)
}

fn claimed_terminal_hash() -> ByteArray {
  validation_trace_v1.hash_machine_state(claimed_terminal())
}

/// A fixed sibling path of the trace depth for `step_count`.
fn siblings() -> List<ByteArray> {
  [zero_hash, zero_hash, zero_hash, zero_hash]
}

fn fold_path(
  siblings: List<ByteArray>,
  index: Int,
  hash: ByteArray,
) -> ByteArray {
  when siblings is {
    [] -> hash
    [sibling, ..rest] ->
      fold_path(
        rest,
        index / 2,
        if index % 2 == 0 {
          validation_trace_v1.trace_branch_hash(hash, sibling)
        } else {
          validation_trace_v1.trace_branch_hash(sibling, hash)
        },
      )
  }
}

/// The trace root whose leaf `index` holds `state_hash`.
fn root_with(index: Int, state_hash: ByteArray) -> ByteArray {
  fold_path(siblings(), index, validation_trace_v1.trace_leaf_hash(state_hash))
}

fn proof_of(
  index: Int,
  state_hash: ByteArray,
) -> validation_trace_v1.ValidationTraceProof {
  validation_trace_v1.ValidationTraceProof {
    state_index: index,
    state_hash,
    siblings: siblings(),
  }
}

/// The pending claim of a challenger whose root holds `terminal` at S.
fn claiming(
  terminal: validation_trace_v1.ValidationMachineStateV1,
) -> validation_game_v1.PendingValidationClaimV1 {
  pending(
    honest_source(),
    root_with(step_count, validation_trace_v1.hash_machine_state(terminal)),
  )
}

fn honest_pending() -> validation_game_v1.PendingValidationClaimV1 {
  claiming(claimed_terminal())
}

// ## The transaction

fn own_out_ref() -> OutputReference {
  OutputReference { transaction_id: h32_c, output_index: 0 }
}

fn thread_value(lovelace: Int) -> assets.Value {
  assets.from_lovelace(lovelace) |> assets.add(thread_policy, thread_name, 1)
}

fn opener_input(
  out_ref: OutputReference,
  state: validation_game_v1.PendingValidationClaimV1,
  name: ByteArray,
) -> Input {
  let datum: Data = ct.StepDatum { fraud_prover, data: Some(state) }
  Input {
    output_reference: out_ref,
    output: Output {
      address: address.from_script(source_hash),
      value: assets.from_lovelace(thread_lovelace)
        |> assets.add(thread_policy, name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn case_id_of(
  state: validation_game_v1.PendingValidationClaimV1,
  out_ref: OutputReference,
) -> ByteArray {
  validation_case_v1.case_nft_name(
    header_hash,
    validation_claim_v1.claim_event_key(state.claim),
    out_ref,
  )
}

fn honest_case_id() -> ByteArray {
  case_id_of(honest_pending(), own_out_ref())
}

/// The case VerifySource at `upper` starts, agreeing on `initial_hash`.
fn case_datum(
  state: validation_game_v1.PendingValidationClaimV1,
  initial_hash: ByteArray,
  upper: Int,
) -> Data {
  let case: Data = validation_case_v1.open_case(
    header_hash,
    validation_claim_v1.claim_event_key(state.claim),
    state.challenger_root,
    step_count,
    claimed_terminal_hash(),
    fraud_prover,
    initial_hash,
    upper,
  )
  case
}

fn case_value(case_id: ByteArray, lovelace: Int) -> assets.Value {
  thread_value(lovelace) |> assets.add(case_hash, case_id, 1)
}

fn case_output_with(datum: Data, value: assets.Value) -> Output {
  Output {
    address: address.from_script(case_hash),
    value,
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn honest_initial_hash() -> ByteArray {
  validation_trace_v1.hash_machine_state(honest_initial())
}

fn case_output_at(upper: Int) -> Output {
  case_output_with(
    case_datum(honest_pending(), honest_initial_hash(), upper),
    case_value(honest_case_id(), thread_lovelace),
  )
}

fn bond_datum_for(
  header: ByteArray,
  prover: ByteArray,
  case_id: ByteArray,
) -> Data {
  let datum: Data = DisputeBondV1 {
    challenged_header_hash: header,
    fraud_prover: prover,
    case_id,
  }
  datum
}

fn bond_datum() -> Data {
  bond_datum_for(header_hash, fraud_prover, honest_case_id())
}

fn bond_output_holding(
  lovelace: Int,
  datum: Data,
  tokens: assets.Value,
) -> Output {
  Output {
    address: address.from_script(bond_hash),
    value: assets.merge(assets.from_lovelace(lovelace), tokens),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn bond_marker_of(case_id: ByteArray) -> assets.Value {
  assets.from_asset(case_hash, validation_case_v1.bond_marker_name(case_id), 1)
}

/// A bond output holding the honest case's bond marker.
fn bond_output(lovelace: Int, datum: Data) -> Output {
  bond_output_holding(lovelace, datum, bond_marker_of(honest_case_id()))
}

fn tx_with(
  state: validation_game_v1.PendingValidationClaimV1,
  upper: Int,
  outputs: List<Output>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [opener_input(own_out_ref(), state, thread_name)],
    outputs,
    fee,
    validity_range: interval.between(
      upper - validation_dispute_v1.dispute_max_validity_range_milliseconds,
      upper,
    ),
  }
}

fn tx_at(upper: Int, outputs: List<Output>) -> Transaction {
  tx_with(honest_pending(), upper, outputs)
}

const upper = end_time + 60_000

fn open_outputs(upper: Int) -> List<Output> {
  [case_output_at(upper), bond_output(bond_lovelace, bond_datum())]
}

fn verify_claiming(
  state: validation_game_v1.PendingValidationClaimV1,
  terminal: validation_trace_v1.ValidationMachineStateV1,
  proof: validation_trace_v1.ValidationTraceProof,
  tx: Transaction,
) -> Bool {
  source_validator.main.spend(
    case_hash,
    bond_hash,
    bond_lovelace,
    thread_policy,
    Some(ct.StepDatum { fraud_prover, data: Some(state) }),
    ct.Continue(
      source_validator.VerifySource {
        input_index: 0,
        output_index: 0,
        challenger_terminal: terminal,
        challenger_terminal_proof: proof,
      },
    ),
    own_out_ref(),
    tx,
  )
}

fn verify(
  state: validation_game_v1.PendingValidationClaimV1,
  tx: Transaction,
) -> Bool {
  verify_claiming(
    state,
    claimed_terminal(),
    proof_of(step_count, claimed_terminal_hash()),
    tx,
  )
}

fn with_outputs(outputs: List<Output>) -> Bool {
  verify(honest_pending(), tx_at(upper, outputs))
}

fn with_case_output(case_output: Output) -> Bool {
  with_outputs([case_output, bond_output(bond_lovelace, bond_datum())])
}

// ## The honest open

test verify_source_starts_the_case_with_the_bond() {
  with_outputs(open_outputs(upper))
}

test verify_source_with_a_fraud_prover_signature_absent_is_accepted() {
  verify(
    honest_pending(),
    census.without_signers(tx_at(upper, open_outputs(upper))),
  )
}

// ## The case output

test case_another_agreed_initial_hash_is_refused() fail {
  with_case_output(
    case_output_with(
      case_datum(honest_pending(), h32_a, upper),
      case_value(honest_case_id(), thread_lovelace),
    ),
  )
}

test case_another_start_time_is_refused() fail {
  with_case_output(
    case_output_with(
      case_datum(honest_pending(), honest_initial_hash(), upper - 1),
      case_value(honest_case_id(), thread_lovelace),
    ),
  )
}

test case_at_another_address_is_refused() fail {
  with_case_output(
    Output { ..case_output_at(upper), address: address.from_script(bond_hash) },
  )
}

test case_with_a_stake_credential_is_refused() fail {
  with_case_output(
    Output {
      ..case_output_at(upper),
      address: address.from_script(case_hash)
        |> address.with_delegation_key(h28_a),
    },
  )
}

test case_without_the_case_nft_is_refused() fail {
  with_case_output(
    Output { ..case_output_at(upper), value: thread_value(thread_lovelace) },
  )
}

test case_without_the_thread_token_is_refused() fail {
  with_case_output(
    Output {
      ..case_output_at(upper),
      value: assets.from_lovelace(thread_lovelace)
        |> assets.add(case_hash, honest_case_id(), 1),
    },
  )
}

test case_with_another_token_is_refused() fail {
  with_case_output(
    Output {
      ..case_output_at(upper),
      value: case_value(honest_case_id(), thread_lovelace)
        |> assets.add(h28_a, thread_name, 1),
    },
  )
}

test case_nft_named_from_another_opener_is_refused() fail {
  let other = case_id_of(
    honest_pending(),
    OutputReference { transaction_id: h32_c, output_index: 1 },
  )
  with_case_output(
    Output {
      ..case_output_at(upper),
      value: case_value(other, thread_lovelace),
    },
  )
}

test case_below_the_opener_lovelace_is_refused() fail {
  with_case_output(
    Output {
      ..case_output_at(upper),
      value: case_value(honest_case_id(), thread_lovelace - 1),
    },
  )
}

test case_above_the_opener_lovelace_is_accepted() {
  with_case_output(
    Output {
      ..case_output_at(upper),
      value: case_value(honest_case_id(), thread_lovelace + 1),
    },
  )
}

/// An opener holding less than the case floor still opens a case holding the
/// floor, and no less.
fn small_opener_opens_with(case_lovelace: Int) -> Bool {
  let base = tx_at(upper, open_outputs(upper))
  expect [opener] = base.inputs
  let small = Input {
    ..opener,
    output: Output {
      ..opener.output,
      value: thread_value(validation_case_v1.case_floor_lovelace - 1_000),
    },
  }
  let case_output = Output {
    ..case_output_at(upper),
    value: case_value(honest_case_id(), case_lovelace),
  }
  expect [_, bond] = base.outputs
  verify(
    honest_pending(),
    Transaction { ..base, inputs: [small], outputs: [case_output, bond] },
  )
}

test case_at_the_case_floor_is_accepted() {
  small_opener_opens_with(validation_case_v1.case_floor_lovelace)
}

test case_below_the_case_floor_is_refused() fail {
  small_opener_opens_with(validation_case_v1.case_floor_lovelace - 1)
}

// ## 1. The source must be authenticated

test verify_source_refuses_an_unauthenticated_source() fail {
  let state = honest_pending()
  let unauthenticated = validation_game_v1.PendingValidationClaimV1 {
    ..state,
    challenged_header: ledger_state.HeaderV1 {
      ..state.challenged_header,
      transactions_root: h32_a,
    },
  }
  verify(unauthenticated, tx_with(unauthenticated, upper, open_outputs(upper)))
}

// ## 2. R-DESC

/// The agreed initial state the validator derives for `state`.
fn agreed_initial_hash(
  state: validation_game_v1.PendingValidationClaimV1,
) -> ByteArray {
  validation_trace_v1.hash_machine_state(
    validation_claim_v1.derive_initial_state(
      state.challenged_header,
      validation_claim_v1.claim_event_key(state.claim),
      state.claim.source_membership,
      state.claim.transition_step_membership.value.pre_utxos_root,
    ),
  )
}

/// The open of `state` with every output named from its own case, so only
/// R-DESC can refuse it.
fn opens_pending(state: validation_game_v1.PendingValidationClaimV1) -> Bool {
  let case_id = case_id_of(state, own_out_ref())
  let case_output = case_output_with(
    case_datum(state, agreed_initial_hash(state), upper),
    case_value(case_id, thread_lovelace),
  )
  let bond = bond_output_holding(
    bond_lovelace,
    bond_datum_for(header_hash, fraud_prover, case_id),
    bond_marker_of(case_id),
  )
  verify(state, tx_with(state, upper, [case_output, bond]))
}

fn root_for_terminal() -> ByteArray {
  root_with(step_count, claimed_terminal_hash())
}

test r_desc_a_normal_source_with_redeemers_opens() {
  opens_pending(honest_pending())
}

test r_desc_a_normal_source_without_redeemers_cannot_open() fail {
  let source = normal_source(0)
  opens_pending(pending(source, root_for_terminal()))
}

fn forced_pending() -> validation_game_v1.PendingValidationClaimV1 {
  pending(forced_source(rejection_reason_v1.ForcedTxValid), root_for_terminal())
}

/// A forced source carrying redeemers is authenticated under its header, so
/// the refusal below is R-DESC's.
test r_desc_a_forced_source_with_redeemers_is_authenticated() {
  let state = forced_pending()
  when
    validation_claim_v1.committed_claim_source_witness_set(
      state.challenged_header,
      state.claim,
    )
  is {
    Some(witness_set) ->
      witness_set.redeemer_tx_wits_hash != native_tx_field_access_v1.empty_field_commitment
    None -> False
  }
}

test r_desc_a_consistent_forced_source_cannot_open() fail {
  opens_pending(forced_pending())
}

// ## 3. The challenger's claim

/// The open, claiming `terminal` with `proof` against `state`.
fn opens_claiming(
  state: validation_game_v1.PendingValidationClaimV1,
  terminal: validation_trace_v1.ValidationMachineStateV1,
  proof: validation_trace_v1.ValidationTraceProof,
) -> Bool {
  let terminal_hash = validation_trace_v1.hash_machine_state(terminal)
  let case: Data = validation_case_v1.open_case(
    header_hash,
    validation_claim_v1.claim_event_key(state.claim),
    state.challenger_root,
    step_count,
    terminal_hash,
    fraud_prover,
    honest_initial_hash(),
    upper,
  )
  verify_claiming(
    state,
    terminal,
    proof,
    tx_with(
      state,
      upper,
      [
        case_output_with(case, case_value(honest_case_id(), thread_lovelace)),
        bond_output(bond_lovelace, bond_datum()),
      ],
    ),
  )
}

/// Opens with `terminal` proven at S under the challenger's root.
fn opens_with_terminal(
  terminal: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  opens_claiming(
    claiming(terminal),
    terminal,
    proof_of(step_count, validation_trace_v1.hash_machine_state(terminal)),
  )
}

test claim_a_terminal_rejected_state_at_s_opens() {
  opens_with_terminal(claimed_terminal())
}

test claim_a_state_that_is_not_terminal_is_refused() fail {
  opens_with_terminal(
    validation_trace_v1.ValidationMachineStateV1 {
      ..claimed_terminal(),
      phase: validation_trace_v1.CanonicalDecode,
    },
  )
}

test claim_an_accepted_terminal_is_refused() fail {
  opens_with_terminal(
    validation_trace_v1.ValidationMachineStateV1 {
      ..claimed_terminal(),
      verdict: validation_trace_v1.Accepted,
      rejection_code_hash: zero_hash,
    },
  )
}

/// The proof is valid for leaf S - 1 of the challenger's root, and the state
/// is the claimed one: only the leaf index is wrong.
test claim_a_proof_of_another_leaf_is_refused() fail {
  opens_claiming(
    pending(honest_source(), root_with(step_count - 1, claimed_terminal_hash())),
    claimed_terminal(),
    proof_of(step_count - 1, claimed_terminal_hash()),
  )
}

/// Leaf S holds another state's hash.
test claim_a_proof_of_another_state_is_refused() fail {
  opens_claiming(
    pending(honest_source(), root_with(step_count, h32_c)),
    claimed_terminal(),
    proof_of(step_count, h32_c),
  )
}

test claim_a_proof_under_another_root_is_refused() fail {
  opens_claiming(
    pending(honest_source(), h32_c),
    claimed_terminal(),
    proof_of(step_count, claimed_terminal_hash()),
  )
}

/// The claim proven at S, with the case recording another step count.
test claim_at_another_step_count_is_refused() fail {
  let state = validation_game_v1.PendingValidationClaimV1 {
    ..honest_pending(),
    step_count: step_count + 1,
  }
  verify(state, tx_with(state, upper, open_outputs(upper)))
}

// ### Terminal coherence
//
// A thread opens only from a defense of a case VerifySource opened. The case
// holds the challenger's Terminal Rejected state at S, and a defense is a state
// at S other than that state which, if Terminal, is Accepted
// (`validation_case_v1.defense_is_valid`). A party that agrees with the
// challenger's state at S therefore has no thread to open.

/// Whether a defender whose trace holds `state` at S may defend `case`.
fn defends_with(
  case: validation_case_v1.CaseV1,
  state: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  let state_hash = validation_trace_v1.hash_machine_state(state)
  validation_case_v1.defense_is_valid(
    case,
    root_with(step_count, state_hash),
    state,
    proof_of(step_count, state_hash),
  )
}

test only_a_state_at_s_other_than_the_challengers_opens_a_thread() {
  let state = honest_pending()
  let case = validation_case_v1.open_case(
    header_hash,
    validation_claim_v1.claim_event_key(state.claim),
    state.challenger_root,
    step_count,
    claimed_terminal_hash(),
    fraud_prover,
    agreed_initial_hash(state),
    upper,
  )
  let opened: Data = case
  let terminal = claimed_terminal()
  and {
    // The case is the one VerifySource opens.
    verify(state, tx_at(upper, open_outputs(upper))),
    case_datum(state, honest_initial_hash(), upper) == opened,
    // The challenger's own state at S does not defend.
    !defends_with(case, terminal),
    // Nor does any other rejection: another rejection hash or program counter.
    !defends_with(
      case,
      validation_trace_v1.ValidationMachineStateV1 {
        ..terminal,
        rejection_code_hash: h32_b,
      },
    ),
    !defends_with(
      case,
      validation_trace_v1.ValidationMachineStateV1 {
        ..terminal,
        program_counter: step_count - 1,
      },
    ),
    // An Accepted terminal defends, and so does a trace still running at S.
    defends_with(
      case,
      terminal_state(
        validation_trace_v1.Accepted,
        validation_trace_v1.no_rejection_code_hash,
      ),
    ),
    defends_with(
      case,
      validation_trace_v1.ValidationMachineStateV1 {
        ..terminal,
        phase: validation_trace_v1.Cek,
        verdict: validation_trace_v1.Pending,
        rejection_code_hash: validation_trace_v1.no_rejection_code_hash,
      },
    ),
  }
}

// ## 4. N5: the agreed initial state

/// The open recording `initial_hash` as the agreed initial state.
fn opens_agreeing_on(initial_hash: ByteArray) -> Bool {
  let state = honest_pending()
  verify(
    state,
    tx_with(
      state,
      upper,
      [
        case_output_with(
          case_datum(state, initial_hash, upper),
          case_value(honest_case_id(), thread_lovelace),
        ),
        bond_output(bond_lovelace, bond_datum()),
      ],
    ),
  )
}

test n5_the_derived_initial_state_opens() {
  opens_agreeing_on(honest_initial_hash())
}

/// An initial state that derivation would not build: no case opens from it.
test n5_an_initial_state_that_is_not_derived_cannot_open() fail {
  opens_agreeing_on(
    validation_trace_v1.hash_machine_state(
      validation_trace_v1.ValidationMachineStateV1 {
        ..honest_initial(),
        prior_ledger_root: h32_b,
      },
    ),
  )
}

// ## Q1: the case is named from the claim's event

/// The open naming its case, bond and case datum from `event_key`.
fn opens_naming_event(event_key: ledger_state.EventKey) -> Bool {
  let state = honest_pending()
  let case_id = validation_case_v1.case_nft_name(
    header_hash,
    event_key,
    own_out_ref(),
  )
  let case: Data = validation_case_v1.open_case(
    header_hash,
    event_key,
    state.challenger_root,
    step_count,
    claimed_terminal_hash(),
    fraud_prover,
    honest_initial_hash(),
    upper,
  )
  verify(
    state,
    tx_with(
      state,
      upper,
      [
        case_output_with(case, case_value(case_id, thread_lovelace)),
        bond_output_holding(
          bond_lovelace,
          bond_datum_for(header_hash, fraud_prover, case_id),
          bond_marker_of(case_id),
        ),
      ],
    ),
  )
}

/// Two challengers of one event, claiming different traces.
test q1_two_claims_on_one_event_name_one_case() {
  let other = pending(honest_source(), h32_c)
  and {
    other.challenger_root != honest_pending().challenger_root,
    case_id_of(other, own_out_ref()) == honest_case_id(),
  }
}

test q1_claims_on_two_events_name_two_cases() {
  let other = pending(
    forced_source(rejection_reason_v1.ForcedTxValid),
    root_for_terminal(),
  )
  and {
    validation_claim_v1.claim_event_key(other.claim) != validation_claim_v1.claim_event_key(
      honest_pending().claim,
    ),
    case_id_of(other, own_out_ref()) != honest_case_id(),
  }
}

test q1_an_open_naming_the_claimed_event_opens() {
  opens_naming_event(event_key_for(honest_source()))
}

test q1_an_open_naming_another_event_is_refused() fail {
  opens_naming_event(
    event_key_for(forced_source(rejection_reason_v1.ForcedTxValid)),
  )
}

// ## 5. The open guard: `upper <= block_end + M/2`

fn open_deadline() -> Int {
  block_end + ledger_state.block_maturity_duration_v1 / 2
}

fn opens_at(at: Int) -> Bool {
  verify(honest_pending(), tx_at(at, open_outputs(at)))
}

test verify_source_opens_at_the_open_deadline() {
  opens_at(open_deadline())
}

test verify_source_refuses_to_open_after_the_open_deadline() fail {
  opens_at(open_deadline() + 1)
}

/// The guard runs from the node's `block_time`, which is later than the
/// header's `end_time` here: one millisecond past `end_time + M/2` still opens.
test verify_source_open_guard_reads_the_node_block_time() {
  opens_at(end_time + ledger_state.block_maturity_duration_v1 / 2 + 1)
}

test verify_source_refuses_a_range_longer_than_l_d() fail {
  let tx = tx_at(upper, open_outputs(upper))
  verify(
    honest_pending(),
    Transaction {
      ..tx,
      validity_range: interval.between(
        upper - validation_dispute_v1.dispute_max_validity_range_milliseconds - 1,
        upper,
      ),
    },
  )
}

// ## 6. The bond

fn case_output() -> Output {
  case_output_at(upper)
}

test bond_missing_is_refused() fail {
  with_outputs([case_output()])
}

test bond_twice_is_refused() fail {
  with_outputs(
    [
      case_output(),
      bond_output(bond_lovelace, bond_datum()),
      bond_output(bond_lovelace, bond_datum()),
    ],
  )
}

test bond_one_lovelace_short_is_refused() fail {
  with_outputs([case_output(), bond_output(bond_lovelace - 1, bond_datum())])
}

test bond_above_the_minimum_is_accepted() {
  with_outputs([case_output(), bond_output(bond_lovelace + 1, bond_datum())])
}

test bond_for_another_prover_is_refused() fail {
  with_outputs(
    [
      case_output(),
      bond_output(
        bond_lovelace,
        bond_datum_for(header_hash, h28_a, honest_case_id()),
      ),
    ],
  )
}

test bond_for_another_block_is_refused() fail {
  with_outputs(
    [
      case_output(),
      bond_output(
        bond_lovelace,
        bond_datum_for(h28_a, fraud_prover, honest_case_id()),
      ),
    ],
  )
}

test bond_without_its_marker_is_refused() fail {
  with_outputs(
    [
      case_output(),
      bond_output_holding(bond_lovelace, bond_datum(), assets.zero),
    ],
  )
}

test bond_holding_the_marker_of_another_case_is_refused() fail {
  with_outputs(
    [
      case_output(),
      bond_output_holding(bond_lovelace, bond_datum(), bond_marker_of(h32_a)),
    ],
  )
}

test bond_holding_another_token_beside_its_marker_is_refused() fail {
  with_outputs(
    [
      case_output(),
      bond_output_holding(
        bond_lovelace,
        bond_datum(),
        bond_marker_of(honest_case_id()) |> assets.add(thread_policy, #"01", 1),
      ),
    ],
  )
}

test bond_for_another_case_is_refused() fail {
  with_outputs(
    [
      case_output(),
      bond_output(
        bond_lovelace,
        bond_datum_for(header_hash, fraud_prover, h32_a),
      ),
    ],
  )
}

test a_wallet_input_beside_the_opener_is_accepted() {
  let base = tx_at(upper, open_outputs(upper))
  verify(
    honest_pending(),
    Transaction {
      ..base,
      inputs: list.concat(
        base.inputs,
        [
          Input {
            output_reference: OutputReference {
              transaction_id: h32_c,
              output_index: 1,
            },
            output: Output {
              address: address.from_verification_key(fraud_prover),
              value: assets.from_lovelace(bond_lovelace),
              datum: transaction.NoDatum,
              reference_script: None,
            },
          },
        ],
      ),
    },
  )
}

/// Two openers in one transaction each need their own bond, and the one bond
/// output there names one case.
test one_bond_cannot_open_two_cases() fail {
  let second_ref = OutputReference { transaction_id: h32_c, output_index: 1 }
  let second_name = #"0607"
  let second_case_id = case_id_of(honest_pending(), second_ref)
  let base = tx_at(upper, open_outputs(upper))
  let tx = Transaction {
    ..base,
    inputs: list.concat(
      base.inputs,
      [opener_input(second_ref, honest_pending(), second_name)],
    ),
    outputs: list.concat(
      base.outputs,
      [
        case_output_with(
          case_datum(honest_pending(), honest_initial_hash(), upper),
          assets.from_lovelace(thread_lovelace)
            |> assets.add(thread_policy, second_name, 1)
            |> assets.add(case_hash, second_case_id, 1),
        ),
      ],
    ),
  }
  source_validator.main.spend(
    case_hash,
    bond_hash,
    bond_lovelace,
    thread_policy,
    Some(ct.StepDatum { fraud_prover, data: Some(honest_pending()) }),
    ct.Continue(
      source_validator.VerifySource {
        input_index: 1,
        output_index: 2,
        challenger_terminal: claimed_terminal(),
        challenger_terminal_proof: proof_of(step_count, claimed_terminal_hash()),
      },
    ),
    second_ref,
    tx,
  )
}

// ## The opener's input

test verify_source_at_another_input_index_is_refused() fail {
  verify_claiming(
    honest_pending(),
    claimed_terminal(),
    proof_of(step_count, claimed_terminal_hash()),
    Transaction {
      ..tx_at(upper, open_outputs(upper)),
      inputs: [
        Input {
          output_reference: OutputReference {
            transaction_id: h32_a,
            output_index: 0,
          },
          output: Output {
            address: address.from_verification_key(fraud_prover),
            value: assets.from_lovelace(bond_lovelace),
            datum: transaction.NoDatum,
            reference_script: None,
          },
        },
        opener_input(own_out_ref(), honest_pending(), thread_name),
      ],
    },
  )
}

// ## Census: who funds and who signs VerifySource
//
// Anyone may submit VerifySource. The opener's lovelace moves into the case
// whole: the transaction fee and the bond come from other inputs.

test census_verify_source_paying_the_fee_from_the_opener_is_refused() fail {
  verify(
    honest_pending(),
    census.lone_input_paying(census.cap)(tx_at(upper, open_outputs(upper))),
  )
}

test census_verify_source_beside_a_wallet_input_keeps_the_opener_value() {
  verify(
    honest_pending(),
    census.beside_wallet_input(0)(tx_at(upper, open_outputs(upper))),
  )
}

test census_verify_source_beside_a_wallet_input_taking_opener_value_is_refused() fail {
  verify(
    honest_pending(),
    census.beside_wallet_input(1)(tx_at(upper, open_outputs(upper))),
  )
}
