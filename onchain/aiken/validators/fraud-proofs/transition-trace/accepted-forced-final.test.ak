//// An accepted forced transaction replayed end to end through the validators:
//// the route's forced arm opens a kind-2 thread at the accepted-transaction
//// final, the binding, open and outputs phases advance it, and the phase-5
//// final convicts exactly when the committed step's post-root differs from the
//// replayed one.
////
//// The forced transaction is the empty native transaction, so phase 0 finds
//// no spend input to delete and phase 10 certifies an empty outputs field;
//// the replay leaves the pre-root unchanged.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Redeemer,
  ScriptPurpose, Spend, Transaction, Withdraw,
}
use env
use fraud_proofs/transition_trace/accepted_transaction_v1
use fraud_proofs/transition_trace/accepted_transaction_yields
use fraud_proofs/transition_trace/route_v1
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proof
use midgard/fraud_proofs/field_opening_v1.{BodyAnchor, BodyFieldOpening}
use midgard/fraud_proofs/field_opening_v1 as opening
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/transition_trace/final_yield
use midgard/fraud_proofs/transition_trace/proof
use midgard/fraud_proofs/transition_trace/proof_carriage
use midgard/ledger_state
use midgard/native_tx_field_access_v1 as field_access
use midgard/native_tx_machine_walk_v1 as walk
use midgard/rejection_reason_v1
use midgard/transition_trace

const thread_policy =
  #"c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1"

const fraud_proof_policy =
  #"c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2"

const auth_policy = #"c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3"

const certificate_policy =
  #"c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4"

const route_hash = #"d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0"

/// The accepted-transaction final's script hash: entry 4 of the route's list.
const final_hash = #"d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4"

const open_hash = #"e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0"

const outputs_hash = #"e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1"

const replay_hash = #"e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2"

const fraud_prover = #"f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0"

const pre_root =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const wrong_post_root =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const zero_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const thread_lovelace = 5000000

fn final_hashes() -> List<ByteArray> {
  [
    #"d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1",
    #"d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2",
    #"d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3",
    #"d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5", final_hash,
    #"d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6",
    #"d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7",
    #"d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8d8",
  ]
}

fn fraud_proof_address() -> Address {
  address.from_script(
    #"c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5",
  )
}

fn thread_out_ref() -> OutputReference {
  OutputReference {
    transaction_id: #"abababababababababababababababababababababababababababababababab",
    output_index: 0,
  }
}

fn order_id() -> ledger_state.TxOrderId {
  OutputReference {
    transaction_id: #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd",
    output_index: 0,
  }
}

// The disputed forced transaction and its committed evidence.

fn empty_native_transaction() -> native_types.MidgardTransaction {
  let empty_mint: Data = builtin.list_data([])
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

fn accepted_leaf() -> ledger_state.ForcedInclusionTxV1 {
  let tx = empty_native_transaction()
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  ledger_state.ForcedInclusionTxV1 {
    tx_id: native_compact.native_tx_id_for_version(
      native_types.native_tx_version_v1,
      native_compact.encode_native_tx_body_compact(compact.body),
    ),
    submitted_source: forced_fixture.from_native_transaction(tx),
    verdict: rejection_reason_v1.ForcedTxValid,
  }
}

fn one_leaf_membership(
  domain: transition_trace.RootDomain,
  key: key,
  value: value,
  key_bytes: ByteArray,
  value_bytes: ByteArray,
) -> transition_trace.RootMembershipProof<key, value> {
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain,
    root: transition_trace.commit_counted_root(domain, phas_root, 1),
    phas_root,
    count: 1,
    key,
    value,
    proof: [],
  }
}

/// The committed fault proof for a forced step whose post-root is
/// `post_root`, and the challenged header it names.
fn fault_proof(post_root: ByteArray) -> proof.TransitionFaultProof {
  let leaf = accepted_leaf()
  let event_key = ledger_state.ForcedTransactionEventKey {
    tx_order_id: order_id(),
  }
  let step = ledger_state.TransitionStep {
    schema_version: 1,
    step_index: 0,
    event_key,
    phase: ledger_state.ForcedTransaction,
    pre_utxos_root: pre_root,
    post_utxos_root: post_root,
  }
  let trace_proof = one_leaf_membership(
    transition_trace.TransitionTraceRootDomain,
    0,
    step,
    cbor.serialise(0),
    cbor.serialise(step),
  )
  let step_value = ledger_state.EventToStepValue {
    step_index: 0,
    phase: ledger_state.ForcedTransaction,
  }
  let event_to_step = one_leaf_membership(
    transition_trace.EventToStepRootDomain,
    event_key,
    step_value,
    cbor.serialise(event_key),
    cbor.serialise(step_value),
  )
  let key_bytes = cbor.serialise(order_id())
  let value_bytes = cbor.serialise(leaf)
  let source = one_leaf_membership(
    transition_trace.ForcedTransactionsV1RootDomain,
    key_bytes,
    value_bytes,
    key_bytes,
    value_bytes,
  )
  let header = ledger_state.HeaderV1 {
    prev_utxos_root: pre_root,
    utxos_root: post_root,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: source.root,
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: 1,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 1,
    transition_step_count: 1,
    validation_trace_count: 0,
    start_time: 10,
    end_time: 20,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    operator_vkey: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    protocol_version: ledger_state.protocol_version_v1,
  }
  let header_data: Data = header
  proof.TransitionFaultProof {
    challenged_header_hash: utils.serialise_and_hash_28(header_data),
    header,
    fault: proof.InvalidOneStepTransition {
      witness: proof.ValidForcedTransactionTransition {
        trace_proof,
        event_to_step,
        source_membership: source,
      },
    },
  }
}

// The published proof chunks.

fn proof_bytes(post_root: ByteArray) -> ByteArray {
  cbor.serialise(fault_proof(post_root))
}

fn split(bytes: ByteArray) -> List<ByteArray> {
  if bytearray.length(bytes) <= proof_carriage.chunk_bytes {
    [bytes]
  } else {
    [
      bytearray.take(bytes, proof_carriage.chunk_bytes),
      ..split(bytearray.drop(bytes, proof_carriage.chunk_bytes))
    ]
  }
}

fn chunk_inputs(post_root: ByteArray) -> List<Input> {
  list.indexed_map(
    split(proof_bytes(post_root)),
    fn(ix, chunk) {
      let datum: Data = chunk
      Input {
        output_reference: OutputReference {
          transaction_id: #"efefefefefefefefefefefefefefefefefefefefefefefefefefefefefefefef",
          output_index: ix,
        },
        output: Output {
          address: address.from_verification_key(fraud_prover),
          value: assets.from_lovelace(2000000),
          datum: InlineDatum(datum),
          reference_script: None,
        },
      }
    },
  )
}

fn chunk_indices(post_root: ByteArray) -> List<Int> {
  list.indexed_map(split(proof_bytes(post_root)), fn(ix, _) { ix })
}

fn commitment(post_root: ByteArray) -> proof_carriage.Commitment {
  proof_carriage.Commitment { hash: blake2b_256(proof_bytes(post_root)) }
}

fn thread_asset_name(post_root: ByteArray) -> ByteArray {
  #"00000004"
    |> bytearray.concat(fault_proof(post_root).challenged_header_hash)
}

// The L1 transactions that move the thread.

fn thread_value(post_root: ByteArray) -> assets.Value {
  assets.from_lovelace(thread_lovelace)
    |> assets.add(thread_policy, thread_asset_name(post_root), 1)
}

fn thread_output(
  script_hash: ByteArray,
  post_root: ByteArray,
  state: Data,
) -> Output {
  Output {
    address: address.from_script(script_hash),
    value: thread_value(post_root),
    datum: InlineDatum(state),
    reference_script: None,
  }
}

fn step_datum(state: Option<final_yield.State>) -> final_yield.Datum {
  ct.StepDatum { fraud_prover, data: state }
}

fn yield_reference(role: ByteArray, script_hash: ByteArray) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(script_hash),
      value: assets.from_lovelace(2000000)
        |> assets.add(auth_policy, role, 1),
      datum: NoDatum,
      reference_script: Some(script_hash),
    },
  }
}

fn args(
  post_root: ByteArray,
  yield_indices: List<Int>,
  fraud_proof_mint_redeemer_index: Int,
) -> final_yield.Args {
  final_yield.Args {
    input_index: 0,
    output_index: 0,
    hub_ref_input_index: 0,
    fraud_proof_mint_redeemer_index,
    yield_ref_input_indices: yield_indices,
    proof_ref_indices: chunk_indices(post_root),
    output_ref_indices: [],
    deposit_event_ref_index: 0,
    deposit_external_ref_index: None,
    deposit_opening: None,
    completed_fraud_witness: None,
  }
}

fn spend_redeemer(args: final_yield.Args) -> Redeemer {
  let redeemer: final_yield.SpendRedeemer = ct.Continue(args)
  let data: Data = redeemer
  data
}

/// A transaction that spends the thread at `final_hash` in `state`, with the
/// given yield's authenticated reference script and zero withdrawal.
fn final_tx(
  post_root: ByteArray,
  state: final_yield.State,
  args: final_yield.Args,
  yield_hash: ByteArray,
  yield_role: ByteArray,
  yield_redeemer: Data,
  extra_redeemers: Pairs<ScriptPurpose, Redeemer>,
  outputs: List<Output>,
) -> Transaction {
  let input_datum: Data = step_datum(Some(state))
  let chunks = chunk_inputs(post_root)
  Transaction {
    ..transaction.placeholder,
    extra_signatories: [fraud_prover],
    inputs: [
      Input {
        output_reference: thread_out_ref(),
        output: thread_output(final_hash, post_root, input_datum),
      },
    ],
    reference_inputs: list.concat(
      chunks,
      [yield_reference(yield_role, yield_hash)],
    ),
    outputs,
    withdrawals: [Pair(Script(yield_hash), 0)],
    redeemers: list.concat(
      extra_redeemers,
      [
        Pair(Spend(thread_out_ref()), spend_redeemer(args)),
        Pair(Withdraw(Script(yield_hash)), yield_redeemer),
      ],
    ),
  }
}

fn successor(post_root: ByteArray, next: final_yield.State) -> List<Output> {
  let datum: Data = step_datum(Some(next))
  [thread_output(final_hash, post_root, datum)]
}

fn yield_index(post_root: ByteArray) -> Int {
  list.length(chunk_indices(post_root))
}

fn forced_compact() -> ByteArray {
  accepted_leaf().submitted_source.compact_cbor
}

fn empty_field_opening() -> opening.FieldOpeningV1 {
  BodyFieldOpening {
    native_tx_compact_cbor: forced_compact(),
    carriage: field_access.Inline {
      preimage: field_access.encode_field_preimage([]),
    },
  }
}

fn anchor() -> opening.NativeTxAnchorV1 {
  BodyAnchor { tx_id: accepted_leaf().tx_id, source_kind: 1 }
}

// Each move, as the validators it runs.

/// The route's forced arm: a challenger publishes the proof and the route
/// opens a kind-2 thread at the accepted-transaction final.
fn routes(post_root: ByteArray) -> Bool {
  let opened: Data = step_datum(
    Some(final_yield.initial(2, commitment(post_root))),
  )
  let unrouted: route_v1.Datum = ct.StepDatum { fraud_prover, data: None }
  let tx = Transaction {
    ..transaction.placeholder,
    extra_signatories: [fraud_prover],
    inputs: [
      Input {
        output_reference: thread_out_ref(),
        output: Output {
          address: address.from_script(route_hash),
          value: thread_value(post_root),
          datum: NoDatum,
          reference_script: None,
        },
      },
    ],
    reference_inputs: chunk_inputs(post_root),
    outputs: [thread_output(final_hash, post_root, opened)],
  }
  route_v1.main.spend(
    final_hashes(),
    thread_policy,
    Some(unrouted),
    ct.Continue(
      route_v1.Args {
        input_index: 0,
        output_index: 0,
        proof: None,
        proof_ref_indices: chunk_indices(post_root),
      },
    ),
    thread_out_ref(),
    tx,
  )
}

/// The final's spend of the thread in `state`, under the transaction's own
/// Continue redeemer.
fn final_spend(state: final_yield.State, tx: Transaction) -> Bool {
  let (_, args) = final_yield.dispatch(final_hash, tx.inputs, tx.redeemers)
  accepted_transaction_v1.main.spend(
    thread_policy,
    fraud_proof_policy,
    fraud_proof_address(),
    auth_policy,
    Some(step_datum(Some(state))),
    ct.Continue(args),
    thread_out_ref(),
    tx,
  )
}

/// Phase 6: the final checks the forced binding and moves to phase 0.
fn binds(post_root: ByteArray) -> Bool {
  let state = final_yield.initial(2, commitment(post_root))
  let input_datum: Data = step_datum(Some(state))
  let tx = Transaction {
    ..transaction.placeholder,
    extra_signatories: [fraud_prover],
    inputs: [
      Input {
        output_reference: thread_out_ref(),
        output: thread_output(final_hash, post_root, input_datum),
      },
    ],
    reference_inputs: chunk_inputs(post_root),
    outputs: successor(post_root, final_yield.State { ..state, phase: 0 }),
    redeemers: [
      Pair(Spend(thread_out_ref()), spend_redeemer(args(post_root, [], 0))),
    ],
  }
  final_spend(state, tx)
}

fn phase_0(post_root: ByteArray) -> final_yield.State {
  final_yield.State {
    ..final_yield.initial(2, commitment(post_root)),
    phase: 0,
  }
}

fn phase_10(post_root: ByteArray) -> final_yield.State {
  let Pair(_, start) = opening.opened_field_walk(
    empty_field_opening(),
    anchor(),
    opening.spend_inputs_field_index,
    [],
    certificate_policy,
  )
  final_yield.State {
    ..phase_0(post_root),
    phase: 10,
    tx_id: accepted_leaf().tx_id,
    inputs_walk: walk.field_walk_checkpoint_hash(start),
    current_root: pre_root,
  }
}

/// Phase 0: the open yield anchors the accepted leaf's transaction and finds
/// no spend input, so the thread moves to phase 10.
fn opens(post_root: ByteArray) -> Bool {
  let carriage = field_access.Inline {
    preimage: field_access.encode_field_preimage([]),
  }
  let carriage_data: Data = carriage
  let tx = final_tx(
    post_root,
    phase_0(post_root),
    args(post_root, [yield_index(post_root)], 0),
    open_hash,
    final_yield.l2_open_role,
    carriage_data,
    [],
    successor(post_root, phase_10(post_root)),
  )
  and {
    final_spend(phase_0(post_root), tx),
    accepted_transaction_yields.l2_open.withdraw(
      final_hash,
      certificate_policy,
      carriage,
      Script(open_hash),
      tx,
    ),
  }
}

fn phase_5(post_root: ByteArray) -> final_yield.State {
  let Pair(view, prior) = opening.opened_field_grammar_certification(
    empty_field_opening(),
    anchor(),
    opening.outputs_field_index,
    [],
    certificate_policy,
  )
  let next = walk.certify_field_grammar(
    view,
    prior,
    accepted_transaction_yields.outputs_grammar_batch,
  )
  let Pair(_, start) = opening.opened_certified_field_walk_from_grammar(
    empty_field_opening(),
    anchor(),
    opening.outputs_field_index,
    walk.field_grammar_checkpoint_hash(next),
    walk.encode_field_grammar_checkpoint(next),
    [],
    certificate_policy,
  )
  final_yield.State {
    ..phase_10(post_root),
    phase: 5,
    outputs_walk: walk.field_walk_checkpoint_hash(start),
    output_count: 0,
  }
}

/// Phase 10: the outputs yield certifies the empty outputs field; with no
/// output to insert the thread moves straight to phase 5.
fn certifies_outputs(post_root: ByteArray) -> Bool {
  let step = final_yield.WalkStep {
    opening: empty_field_opening(),
    checkpoint_bytes: #"",
  }
  let step_data: Data = step
  let tx = final_tx(
    post_root,
    phase_10(post_root),
    args(post_root, [yield_index(post_root)], 0),
    outputs_hash,
    final_yield.l2_outputs_role,
    step_data,
    [],
    successor(post_root, phase_5(post_root)),
  )
  and {
    final_spend(phase_10(post_root), tx),
    accepted_transaction_yields.l2_outputs.withdraw(
      final_hash,
      certificate_policy,
      step,
      Script(outputs_hash),
      tx,
    ),
  }
}

/// Phase 5, the kind-0 final: the thread finalizes into a fraud-proof token,
/// and the replay yield returns whether the replayed root differs from the
/// committed post-root.
fn final_tx_at_phase_5(post_root: ByteArray) -> Transaction {
  let asset_name = thread_asset_name(post_root)
  let mint: Data = fraud_proof.MintRedeemer {
    computation_thread_token_asset_name: asset_name,
    computation_thread_mint_redeemer_index: 1,
  }
  let proof_datum: Data = fraud_proof.Datum { fraud_prover }
  let unit: Data = Void
  final_tx(
    post_root,
    phase_5(post_root),
    args(post_root, [yield_index(post_root)], 0),
    replay_hash,
    final_yield.l2_replay_role,
    unit,
    [Pair(Mint(fraud_proof_policy), mint)],
    [
      Output {
        address: fraud_proof_address(),
        value: assets.from_lovelace(thread_lovelace)
          |> assets.add(fraud_proof_policy, asset_name, 1),
        datum: InlineDatum(proof_datum),
        reference_script: None,
      },
    ],
  )
}

fn finalizes(post_root: ByteArray) -> Bool {
  final_spend(phase_5(post_root), final_tx_at_phase_5(post_root))
}

fn replay_convicts(post_root: ByteArray) -> Bool {
  let unit: Data = Void
  accepted_transaction_yields.l2_replay.withdraw(
    final_hash,
    unit,
    Script(replay_hash),
    final_tx_at_phase_5(post_root),
  )
}

/// Every move before the final verdict.
fn reaches_the_final(post_root: ByteArray) -> Bool {
  and {
    routes(post_root),
    binds(post_root),
    opens(post_root),
    certifies_outputs(post_root),
    finalizes(post_root),
  }
}

test accepted_forced_tx_with_a_wrong_post_root_is_convicted() {
  and {
    reaches_the_final(wrong_post_root),
    replay_convicts(wrong_post_root),
  }
}

test honest_accepted_forced_tx_is_not_convicted() {
  and {
    reaches_the_final(pre_root),
    !replay_convicts(pre_root),
  }
}
