//// VerifySource, driven through the deployed validator: the C1 order (source
//// authenticated, check 3, Gap B, R-DESC, open), the bond output, the fee
//// reserve and the maturity horizon, each in both polarities.

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
use env
use fraud_proofs/validation_trace/source_v1 as source_validator
use midgard/computation_thread as ct
use midgard/dispute_bond_v1.{DisputeBondV1}
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_state
use midgard/rejection_reason_v1
use midgard/transition_trace
use midgard/validation_claim_v1
use midgard/validation_dispute_v1
use midgard/validation_game_v1
use midgard/validation_machine/witness as vm_witness
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

const game_hash = #"01010101010101010101010101010101010101010101010101010101"

const award_hash = #"02020202020202020202020202020202020202020202020202020202"

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
    ledger_delta_root: h32_c,
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
    ledger_delta_root: zero_hash,
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

fn descriptor(
  initial_state_hash: ByteArray,
  terminal_state_hash: ByteArray,
  verdict: validation_trace_v1.ValidationVerdict,
  rejection_code_hash: ByteArray,
) -> validation_trace_v1.ValidationTraceDescriptorV1 {
  validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: h32_c,
    step_count,
    initial_state_hash,
    terminal_state_hash,
    verdict,
    rejection_code_hash,
  }
}

fn proof() -> validation_trace_v1.ValidationTraceProof {
  validation_trace_v1.ValidationTraceProof {
    state_index: 0,
    state_hash: zero_hash,
    siblings: [],
  }
}

fn claim(
  source: validation_claim_v1.ValidationSourceMembershipV1,
  operator: validation_trace_v1.ValidationTraceDescriptorV1,
  initial_state: validation_trace_v1.ValidationMachineStateV1,
) -> validation_claim_v1.ValidationClaimWitnessV1 {
  let event_key = event_key_for(source)
  validation_claim_v1.ValidationClaimWitnessV1 {
    version: validation_claim_v1.claim_version,
    descriptor_membership: transition_trace.RootMembershipProof {
      domain: transition_trace.ValidationTracesRootDomain,
      root: zero_hash,
      phas_root: zero_hash,
      count: 1,
      key: event_key,
      value: operator,
      proof: [],
    },
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
        phase: ledger_state.L2Transaction,
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
      value: ledger_state.EventToStepValue {
        step_index: 0,
        phase: ledger_state.L2Transaction,
      },
      proof: [],
    },
    source_membership: source,
    validation_context_cbor: context_cbor(header_for(source)),
    initial_state,
    terminal_state: placeholder_state(),
    initial_state_proof: proof(),
    terminal_state_proof: proof(),
  }
}

fn pending(
  source: validation_claim_v1.ValidationSourceMembershipV1,
  operator: validation_trace_v1.ValidationTraceDescriptorV1,
  initial_state: validation_trace_v1.ValidationMachineStateV1,
  challenger: validation_trace_v1.ValidationTraceDescriptorV1,
  supplied_terminal: Option<validation_trace_v1.ValidationMachineStateV1>,
) -> validation_game_v1.PendingValidationClaimV1 {
  validation_game_v1.PendingValidationClaimV1 {
    challenged_header_hash: header_hash,
    challenged_header: header_for(source),
    claim: claim(source, operator, initial_state),
    challenger_descriptor: challenger,
    open_time_upper: end_time,
    terminal_state: supplied_terminal,
  }
}

// The honest dispute: a normal Plutus source, the operator's derived initial
// state, an Accepted operator descriptor and a challenger claiming rejection.

fn honest_source() -> validation_claim_v1.ValidationSourceMembershipV1 {
  normal_source(1)
}

fn honest_initial() -> validation_trace_v1.ValidationMachineStateV1 {
  derived_initial_state(honest_source())
}

fn honest_operator() -> validation_trace_v1.ValidationTraceDescriptorV1 {
  descriptor(
    validation_trace_v1.hash_machine_state(honest_initial()),
    h32_b,
    validation_trace_v1.Accepted,
    zero_hash,
  )
}

fn honest_challenger() -> validation_trace_v1.ValidationTraceDescriptorV1 {
  descriptor(
    validation_trace_v1.hash_machine_state(honest_initial()),
    h32_c,
    validation_trace_v1.Rejected,
    h32_a,
  )
}

fn honest_pending() -> validation_game_v1.PendingValidationClaimV1 {
  pending(
    honest_source(),
    honest_operator(),
    honest_initial(),
    honest_challenger(),
    None,
  )
}

// ## The transaction

fn own_out_ref() -> OutputReference {
  OutputReference { transaction_id: h32_c, output_index: 0 }
}

fn thread_value(lovelace: Int) -> assets.Value {
  assets.from_lovelace(lovelace) |> assets.add(thread_policy, thread_name, 1)
}

fn thread_output(script_hash: ByteArray, lovelace: Int, state: Data) -> Output {
  let datum: Data = ct.StepDatum { fraud_prover, data: Some(state) }
  Output {
    address: address.from_script(script_hash),
    value: thread_value(lovelace),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn bond_datum() -> Data {
  let datum: Data = DisputeBondV1 {
    challenged_header_hash: header_hash,
    fraud_prover,
    forfeit_after: dispute_bond_v1.forfeit_after(end_time),
  }
  datum
}

fn bond_output(lovelace: Int, datum: Data) -> Output {
  Output {
    address: address.from_script(bond_hash),
    value: assets.from_lovelace(lovelace),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn tx_at(upper: Int, outputs: List<Output>) -> Transaction {
  let input_datum: Data = ct.StepDatum {
    fraud_prover,
    data: Some(honest_pending()),
  }
  Transaction {
    ..transaction.placeholder,
    inputs: [
      Input {
        output_reference: own_out_ref(),
        output: Output {
          address: address.from_script(source_hash),
          value: thread_value(thread_lovelace),
          datum: InlineDatum(input_datum),
          reference_script: None,
        },
      },
    ],
    outputs,
    fee,
    validity_range: interval.between(
      upper - validation_dispute_v1.dispute_max_validity_range_milliseconds,
      upper,
    ),
  }
}

fn reserved() -> Int {
  thread_lovelace + validation_dispute_v1.thread_fee_reserve_lovelace
}

/// The game a VerifySource at `upper` opens, agreeing on `low_hash`. Built
/// with `open`, which does not check the maturity horizon, so a refusal past
/// maturity is the validator's alone.
fn opened_at(
  state: validation_game_v1.PendingValidationClaimV1,
  low_hash: ByteArray,
  upper: Int,
) -> Data {
  let game: Data = validation_game_v1.ValidationGameStateV1 {
    challenged_header_hash: header_hash,
    operator_vkey: h28_a,
    dispute: validation_dispute_v1.open(
      state.claim.descriptor_membership.value,
      state.challenger_descriptor,
      low_hash,
      upper,
    ),
  }
  game
}

fn opened(
  state: validation_game_v1.PendingValidationClaimV1,
  upper: Int,
) -> Data {
  opened_at(
    state,
    state.claim.descriptor_membership.value.initial_state_hash,
    upper,
  )
}

fn open_outputs(
  state: validation_game_v1.PendingValidationClaimV1,
  upper: Int,
) -> List<Output> {
  [
    thread_output(game_hash, reserved(), opened(state, upper)),
    bond_output(bond_lovelace, bond_datum()),
  ]
}

fn award_outputs() -> List<Output> {
  [
    thread_output(
      award_hash,
      thread_lovelace,
      validation_resolution_v1.winning_resolution(),
    ),
  ]
}

const upper = end_time + 60_000

fn verify(
  state: validation_game_v1.PendingValidationClaimV1,
  tx: Transaction,
) -> Bool {
  source_validator.main.spend(
    game_hash,
    award_hash,
    bond_hash,
    bond_lovelace,
    thread_policy,
    Some(ct.StepDatum { fraud_prover, data: Some(state) }),
    ct.Continue(
      source_validator.VerifySource { input_index: 0, output_index: 0 },
    ),
    own_out_ref(),
    tx,
  )
}

// ## 5. The honest open

test verify_source_opens_the_game_with_the_bond_and_the_reserve() {
  verify(honest_pending(), tx_at(upper, open_outputs(honest_pending(), upper)))
}

test verify_source_open_is_not_an_award() fail {
  verify(honest_pending(), tx_at(upper, award_outputs()))
}

test verify_source_refuses_another_agreed_low_hash() fail {
  let state = honest_pending()
  verify(
    state,
    tx_at(
      upper,
      [
        thread_output(game_hash, reserved(), opened_at(state, h32_a, upper)),
        bond_output(bond_lovelace, bond_datum()),
      ],
    ),
  )
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
  verify(unauthenticated, tx_at(upper, award_outputs()))
}

// ## 2. Check 3 is decisive

/// C1: a normal Plutus source, the operator descriptor claims rejection, both
/// descriptors commit to the same terminal hash and the supplied terminal
/// state is coherent with the descriptor. Check 3 still convicts.
test c1_check_3_convicts_a_rejected_normal_descriptor_even_with_equal_terminals() {
  let terminal = terminal_state(validation_trace_v1.Rejected, h32_a)
  let terminal_hash = validation_trace_v1.hash_machine_state(terminal)
  let initial_hash = validation_trace_v1.hash_machine_state(honest_initial())
  let operator = descriptor(
    initial_hash,
    terminal_hash,
    validation_trace_v1.Rejected,
    h32_a,
  )
  let challenger = descriptor(
    initial_hash,
    terminal_hash,
    validation_trace_v1.Rejected,
    h32_b,
  )
  verify(
    pending(
      honest_source(),
      operator,
      honest_initial(),
      challenger,
      Some(terminal),
    ),
    tx_at(upper, award_outputs()),
  )
}

test c1_check_3_conviction_pays_only_the_award() fail {
  let terminal = terminal_state(validation_trace_v1.Rejected, h32_a)
  let terminal_hash = validation_trace_v1.hash_machine_state(terminal)
  let initial_hash = validation_trace_v1.hash_machine_state(honest_initial())
  let operator = descriptor(
    initial_hash,
    terminal_hash,
    validation_trace_v1.Rejected,
    h32_a,
  )
  let challenger = descriptor(
    initial_hash,
    terminal_hash,
    validation_trace_v1.Rejected,
    h32_b,
  )
  let state = pending(
    honest_source(),
    operator,
    honest_initial(),
    challenger,
    Some(terminal),
  )
  verify(state, tx_at(upper, open_outputs(state, upper)))
}

test check_3_convicts_an_accepted_descriptor_on_a_rejecting_forced_leaf() {
  let source = forced_source(
    rejection_reason_v1.ForcedTxInvalid {
      reason: rejection_reason_v1.FeeBelowMinimum,
    },
  )
  verify(
    pending(
      source,
      honest_operator(),
      placeholder_state(),
      honest_challenger(),
      None,
    ),
    tx_at(upper, award_outputs()),
  )
}

// ## 3. Gap B

fn gap_b(
  supplied: validation_trace_v1.ValidationMachineStateV1,
  committed: validation_trace_v1.ValidationMachineStateV1,
) -> validation_game_v1.PendingValidationClaimV1 {
  let terminal_hash = validation_trace_v1.hash_machine_state(committed)
  let initial_hash = validation_trace_v1.hash_machine_state(honest_initial())
  pending(
    honest_source(),
    descriptor(
      initial_hash,
      terminal_hash,
      validation_trace_v1.Accepted,
      zero_hash,
    ),
    honest_initial(),
    descriptor(initial_hash, terminal_hash, validation_trace_v1.Rejected, h32_a),
    Some(supplied),
  )
}

test gap_b_incoherent_terminal_goes_to_award() {
  let incoherent = validation_trace_v1.ValidationMachineStateV1 {
    ..terminal_state(validation_trace_v1.Accepted, zero_hash),
    program_counter: step_count - 1,
  }
  verify(gap_b(incoherent, incoherent), tx_at(upper, award_outputs()))
}

test gap_b_coherent_terminal_has_no_successor() fail {
  let coherent = terminal_state(validation_trace_v1.Accepted, zero_hash)
  verify(gap_b(coherent, coherent), tx_at(upper, award_outputs()))
}

test gap_b_coherent_terminal_cannot_open_the_game() fail {
  let coherent = terminal_state(validation_trace_v1.Accepted, zero_hash)
  let state = gap_b(coherent, coherent)
  verify(state, tx_at(upper, open_outputs(state, upper)))
}

test gap_b_refuses_a_terminal_that_is_not_the_committed_one() fail {
  let committed = terminal_state(validation_trace_v1.Accepted, zero_hash)
  let incoherent = validation_trace_v1.ValidationMachineStateV1 {
    ..committed,
    program_counter: step_count - 1,
  }
  verify(gap_b(incoherent, committed), tx_at(upper, award_outputs()))
}

/// An incoherent operator terminal supplied although the challenger commits
/// to another terminal hash: without the presence rule it would go to award.
test gap_b_refuses_a_terminal_when_the_terminal_hashes_differ() fail {
  let incoherent = validation_trace_v1.ValidationMachineStateV1 {
    ..terminal_state(validation_trace_v1.Accepted, zero_hash),
    program_counter: step_count - 1,
  }
  let state = validation_game_v1.PendingValidationClaimV1 {
    ..gap_b(incoherent, incoherent),
    challenger_descriptor: honest_challenger(),
  }
  verify(state, tx_at(upper, award_outputs()))
}

/// Equal terminal hashes with no terminal supplied: without the presence rule
/// the game would open, since the descriptors still disagree on the verdict.
test gap_b_requires_the_terminal_when_the_terminal_hashes_are_equal() fail {
  let state = validation_game_v1.PendingValidationClaimV1 {
    ..honest_pending(),
    challenger_descriptor: validation_trace_v1.ValidationTraceDescriptorV1 {
      ..honest_challenger(),
      terminal_state_hash: honest_operator().terminal_state_hash,
    },
  }
  verify(state, tx_at(upper, open_outputs(state, upper)))
}

// ## 4. R-DESC

fn native_only() -> validation_game_v1.PendingValidationClaimV1 {
  let source = normal_source(0)
  let initial = derived_initial_state(source)
  let initial_hash = validation_trace_v1.hash_machine_state(initial)
  pending(
    source,
    descriptor(initial_hash, h32_b, validation_trace_v1.Accepted, zero_hash),
    initial,
    descriptor(initial_hash, h32_c, validation_trace_v1.Rejected, h32_a),
    None,
  )
}

fn consistent_forced() -> validation_game_v1.PendingValidationClaimV1 {
  pending(
    forced_source(rejection_reason_v1.ForcedTxValid),
    honest_operator(),
    placeholder_state(),
    honest_challenger(),
    None,
  )
}

test r_desc_a_normal_source_without_redeemers_cannot_open() fail {
  verify(native_only(), tx_at(upper, open_outputs(native_only(), upper)))
}

test r_desc_a_normal_source_without_redeemers_is_not_an_award() fail {
  verify(native_only(), tx_at(upper, award_outputs()))
}

test r_desc_a_consistent_forced_source_cannot_open() fail {
  verify(
    consistent_forced(),
    tx_at(upper, open_outputs(consistent_forced(), upper)),
  )
}

test r_desc_a_consistent_forced_source_is_not_an_award() fail {
  verify(consistent_forced(), tx_at(upper, award_outputs()))
}

// ## S-1: an initial state derivation would not build is the operator's fault

test a_committed_initial_state_that_is_not_derived_goes_to_award() {
  let bogus = validation_trace_v1.ValidationMachineStateV1 {
    ..honest_initial(),
    prior_ledger_root: h32_b,
  }
  let state = pending(
    honest_source(),
    honest_operator(),
    bogus,
    honest_challenger(),
    None,
  )
  verify(state, tx_at(upper, award_outputs()))
}

// ## The bond

fn with_outputs(outputs: List<Output>) -> Bool {
  verify(honest_pending(), tx_at(upper, outputs))
}

fn game_output() -> Output {
  thread_output(game_hash, reserved(), opened(honest_pending(), upper))
}

test bond_missing_is_refused() fail {
  with_outputs([game_output()])
}

test bond_twice_is_refused() fail {
  with_outputs(
    [
      game_output(),
      bond_output(bond_lovelace, bond_datum()),
      bond_output(bond_lovelace, bond_datum()),
    ],
  )
}

test bond_one_lovelace_short_is_refused() fail {
  with_outputs([game_output(), bond_output(bond_lovelace - 1, bond_datum())])
}

test bond_above_the_minimum_is_accepted() {
  with_outputs([game_output(), bond_output(bond_lovelace + 1, bond_datum())])
}

test bond_for_another_prover_is_refused() fail {
  let datum: Data = DisputeBondV1 {
    challenged_header_hash: header_hash,
    fraud_prover: h28_a,
    forfeit_after: dispute_bond_v1.forfeit_after(end_time),
  }
  with_outputs([game_output(), bond_output(bond_lovelace, datum)])
}

test bond_forfeitable_early_is_refused() fail {
  let datum: Data = DisputeBondV1 {
    challenged_header_hash: header_hash,
    fraud_prover,
    forfeit_after: dispute_bond_v1.forfeit_after(end_time) - 1,
  }
  with_outputs([game_output(), bond_output(bond_lovelace, datum)])
}

test bond_for_another_block_is_refused() fail {
  let datum: Data = DisputeBondV1 {
    challenged_header_hash: h28_a,
    fraud_prover,
    forfeit_after: dispute_bond_v1.forfeit_after(end_time),
  }
  with_outputs([game_output(), bond_output(bond_lovelace, datum)])
}

/// The honest opening transaction with `extra` spent beside the thread.
fn with_input_beside_the_thread(extra: Input, outputs: List<Output>) -> Bool {
  let base = tx_at(
    upper,
    list.concat(open_outputs(honest_pending(), upper), outputs),
  )
  expect [thread_input] = base.inputs
  verify(
    honest_pending(),
    Transaction { ..base, inputs: [thread_input, extra] },
  )
}

test a_wallet_input_beside_the_thread_is_accepted() {
  with_input_beside_the_thread(
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
    [],
  )
}

/// Two pending threads of one prover on one block cannot open in the same
/// transaction: each game needs its own bond.
test one_bond_cannot_open_two_games() fail {
  let second_thread = thread_name
  let pending_datum: Data = ct.StepDatum {
    fraud_prover,
    data: Some(honest_pending()),
  }
  let game_datum: Data = ct.StepDatum {
    fraud_prover,
    data: Some(opened(honest_pending(), upper)),
  }
  with_input_beside_the_thread(
    Input {
      output_reference: OutputReference {
        transaction_id: h32_c,
        output_index: 1,
      },
      output: Output {
        address: address.from_script(source_hash),
        value: assets.from_lovelace(thread_lovelace)
          |> assets.add(thread_policy, second_thread, 1),
        datum: InlineDatum(pending_datum),
        reference_script: None,
      },
    },
    [
      Output {
        address: address.from_script(game_hash),
        value: assets.from_lovelace(reserved())
          |> assets.add(thread_policy, second_thread, 1),
        datum: InlineDatum(game_datum),
        reference_script: None,
      },
    ],
  )
}

// ## The fee reserve

test reserve_one_lovelace_short_is_refused() fail {
  with_outputs(
    [
      thread_output(game_hash, reserved() - 1, opened(honest_pending(), upper)),
      bond_output(bond_lovelace, bond_datum()),
    ],
  )
}

test reserve_is_sixty_six_moves_at_the_cap() {
  validation_dispute_v1.thread_fee_reserve_lovelace == 66 * env.max_dispute_move_fee_lovelace
}

// ## The maturity horizon

fn latest_upper() -> Int {
  end_time + ledger_state.block_maturity_duration_v1 - validation_dispute_v1.max_dispute_duration_milliseconds
}

test verify_source_opens_at_the_last_instant_maturity_allows() {
  let at = latest_upper()
  verify(honest_pending(), tx_at(at, open_outputs(honest_pending(), at)))
}

test verify_source_refuses_to_open_past_maturity() fail {
  let at = latest_upper() + 1
  verify(
    honest_pending(),
    tx_at(
      at,
      [
        thread_output(game_hash, reserved(), opened(honest_pending(), at)),
        bond_output(bond_lovelace, bond_datum()),
      ],
    ),
  )
}
