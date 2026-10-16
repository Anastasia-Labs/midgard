//// #640 gate-5 tests: the §2.4.3(d)/(e) source-authoritativeness predicates,
//// each exercised positively and negatively.
////
//// The predicate tests drive the public door
//// `committed_claim_source_is_authenticated`, which reads only the claim's
//// event key (its event-to-step membership's key) and the source membership —
//// every other witness field is an inert dummy here on purpose.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{OutputReference}
use env
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_state
use midgard/rejection_reason_v1
use midgard/transition_trace
use midgard/validation_claim_v1
use midgard/validation_trace_v1

const h28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h32_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const h32_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const zero_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

// ## Fixtures for the §2.4.3(d)/(e) predicate tests

fn native_tx_with(
  validity: native_types.MidgardTxValidity,
) -> native_types.MidgardTransaction {
  let empty_mint: Data = []
  native_types.MidgardTransaction {
    version: native_types.native_tx_version_v1,
    validity,
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

fn forced_source(
  validity: native_types.MidgardTxValidity,
  verdict: rejection_reason_v1.OperatorVerdictV1,
) -> validation_claim_v1.ValidationSourceMembershipV1 {
  let tx = native_tx_with(validity)
  let forced = ledger_state.ForcedInclusionTxV1 {
    tx_id: native_tx_id_of(tx),
    submitted_source: forced_fixture.from_native_source(proof_source_of(tx)),
    verdict,
  }
  let key_bytes = cbor.serialise(tx_order_id())
  let value_bytes = cbor.serialise(forced)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
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
      value: forced,
      proof: [],
    },
  }
}

fn normal_source(
  validity: native_types.MidgardTxValidity,
) -> validation_claim_v1.ValidationSourceMembershipV1 {
  let tx = native_tx_with(validity)
  let tx_id = native_tx_id_of(tx)
  let source = ledger_state.L2TransactionSourceV1 {
    tx_id,
    source: proof_source_of(tx),
  }
  let value_bytes = cbor.serialise(source)
  let phas_root = mpf.root(mpf.insert(mpf.empty, tx_id, value_bytes, []))
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
      value: source,
      proof: [],
    },
  }
}

fn source_root(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> ByteArray {
  when source is {
    validation_claim_v1.ForcedValidationSource { membership } -> membership.root
    validation_claim_v1.NormalValidationSource { membership } -> membership.root
  }
}

fn header_for(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> ledger_state.HeaderV1 {
  let base = ledger_state.HeaderV1 {
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
    prev_header_hash: h28_a,
    operator_vkey: h28_a,
    protocol_version: ledger_state.protocol_version_v1,
  }
  when source is {
    validation_claim_v1.ForcedValidationSource { .. } ->
      ledger_state.HeaderV1 {
        ..base,
        forced_transactions_root: source_root(source),
        forced_transaction_count: 1,
      }
    validation_claim_v1.NormalValidationSource { .. } ->
      ledger_state.HeaderV1 {
        ..base,
        transactions_root: source_root(source),
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

/// A witness whose only live fields are the event-to-step membership's key and
/// the source membership — exactly what
/// `committed_claim_source_is_authenticated` reads.
fn witness_for(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> validation_claim_v1.ValidationClaimWitnessV1 {
  let event_key = event_key_for(source)
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
        phase: ledger_state.ForcedTransaction,
        pre_utxos_root: h32_a,
        post_utxos_root: h32_a,
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
        phase: ledger_state.ForcedTransaction,
      },
      proof: [],
    },
    source_membership: source,
  }
}

fn source_is_authenticated(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> Bool {
  validation_claim_v1.committed_claim_source_is_authenticated(
    header_for(source),
    witness_for(source),
  )
}

// Forced-source authoritativeness: validity-free source, sole verdict claim.

test forced_source_accepts_valid_verdict() {
  source_is_authenticated(
    forced_source(native_types.TxIsValid, rejection_reason_v1.ForcedTxValid),
  )
}

test forced_source_does_not_copy_native_invalid_bit() {
  source_is_authenticated(
    forced_source(native_types.TxIsInvalid, rejection_reason_v1.ForcedTxValid),
  )
}

test forced_source_accepts_invalid_verdict() {
  source_is_authenticated(
    forced_source(
      native_types.TxIsInvalid,
      rejection_reason_v1.ForcedTxInvalid {
        reason: rejection_reason_v1.PlutusExecutionFailed {
          purpose_kind: 0,
          purpose_index: 0,
        },
      },
    ),
  )
}

test forced_source_accepts_rejection_of_admission_valid_submission() {
  source_is_authenticated(
    forced_source(
      native_types.TxIsValid,
      rejection_reason_v1.ForcedTxInvalid {
        reason: rejection_reason_v1.FeeBelowMinimum,
      },
    ),
  )
}

// ## §2.4.3(d) Normal-source authoritativeness (acceptance scalar)

test normal_source_accepts_validity_code_zero() {
  source_is_authenticated(normal_source(native_types.TxIsValid))
}

test normal_source_rejects_validity_code_one() {
  !source_is_authenticated(normal_source(native_types.TxIsInvalid))
}

// ## The authenticated witness set and rule R-DESC

fn with_one_redeemer(
  tx: native_types.MidgardTransaction,
) -> native_types.MidgardTransaction {
  native_types.MidgardTransaction {
    ..tx,
    witness_set: native_types.MidgardTransactionWitnessSet {
      ..tx.witness_set,
      redeemer_tx_wits: [
        native_types.MidgardRedeemerWitness {
          purpose: native_types.SpendRedeemer,
          index: 0,
          redeemer_cbor: cbor.serialise(42),
          execution_units: native_types.MidgardExecutionUnits {
            memory: 10,
            steps: 20,
          },
        },
      ],
    },
  }
}

fn normal_source_of_tx(
  tx: native_types.MidgardTransaction,
) -> validation_claim_v1.ValidationSourceMembershipV1 {
  let tx_id = native_tx_id_of(tx)
  let source = ledger_state.L2TransactionSourceV1 {
    tx_id,
    source: proof_source_of(tx),
  }
  let phas_root = mpf.root(
    mpf.insert(mpf.empty, tx_id, cbor.serialise(source), []),
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
      value: source,
      proof: [],
    },
  }
}

fn requires_trace(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> Bool {
  expect Some(witness_set) = validation_claim_v1.committed_claim_source_witness_set(
    header_for(source),
    witness_for(source),
  )
  validation_claim_v1.source_requires_trace(source, witness_set)
}

test r_desc_a_normal_source_with_a_redeemer_has_a_trace() {
  requires_trace(
    normal_source_of_tx(
      with_one_redeemer(native_tx_with(native_types.TxIsValid)),
    ),
  )
}

// ## The claim
//
// Each case below commits one step and one event under the header's own
// roots, so the whole claim (structure and source) is exercised, not only the
// predicate under test. The block commits no validation-trace descriptor: the
// header's validation-trace pair stays the empty tree.

fn counted_membership(
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

fn step_phase_for(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> ledger_state.TransitionPhase {
  when source is {
    validation_claim_v1.ForcedValidationSource { .. } ->
      ledger_state.ForcedTransaction
    validation_claim_v1.NormalValidationSource { .. } ->
      ledger_state.L2Transaction
  }
}

/// Distinct, non-zero context values, so a derivation that reads the wrong
/// header field cannot agree by accident.
fn distinct_context(header: ledger_state.HeaderV1) -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    ..header,
    start_time: 1_700_000_000_000,
    end_time: 1_700_000_020_000,
    block_slot: 86_400,
    expected_network_id: 1,
    min_fee_a: 44,
    min_fee_b: 155_381,
  }
}

/// The claim on `source`'s event, whose step is named `step_event_key`.
fn claim_naming(
  source: validation_claim_v1.ValidationSourceMembershipV1,
  step_event_key: ledger_state.EventKey,
  post_utxos_root: ByteArray,
) -> (ledger_state.HeaderV1, validation_claim_v1.ValidationClaimWitnessV1) {
  let event_key = event_key_for(source)
  let phase = step_phase_for(source)
  let base = distinct_context(header_for(source))
  let step = ledger_state.TransitionStep {
    schema_version: ledger_state.transition_step_schema_version_v1,
    step_index: 0,
    event_key: step_event_key,
    phase,
    pre_utxos_root: h32_a,
    post_utxos_root,
  }
  let transition_step_membership = counted_membership(
    transition_trace.TransitionTraceRootDomain,
    0,
    step,
    cbor.serialise(0),
    cbor.serialise(step),
  )
  let event_to_step = ledger_state.EventToStepValue { step_index: 0, phase }
  let event_to_step_membership = counted_membership(
    transition_trace.EventToStepRootDomain,
    event_key,
    event_to_step,
    cbor.serialise(event_key),
    cbor.serialise(event_to_step),
  )
  let header = ledger_state.HeaderV1 {
    ..base,
    transition_trace_root: transition_step_membership.root,
    transition_step_count: 1,
    event_to_step_root: event_to_step_membership.root,
    total_event_count: 1,
  }
  (
    header,
    validation_claim_v1.ValidationClaimWitnessV1 {
      version: validation_claim_v1.claim_version,
      transition_step_membership,
      event_to_step_membership,
      source_membership: source,
    },
  )
}

fn claim_on(
  source: validation_claim_v1.ValidationSourceMembershipV1,
  post_utxos_root: ByteArray,
) -> (ledger_state.HeaderV1, validation_claim_v1.ValidationClaimWitnessV1) {
  claim_naming(source, event_key_for(source), post_utxos_root)
}

fn accepted_claim(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> (ledger_state.HeaderV1, validation_claim_v1.ValidationClaimWitnessV1) {
  claim_on(source, h32_b)
}

fn plutus_failure() -> rejection_reason_v1.RejectionReasonV1 {
  rejection_reason_v1.PlutusExecutionFailed {
    purpose_kind: 0,
    purpose_index: 0,
  }
}

fn forced_invalid_source() -> validation_claim_v1.ValidationSourceMembershipV1 {
  forced_source(
    native_types.TxIsValid,
    rejection_reason_v1.ForcedTxInvalid { reason: plutus_failure() },
  )
}

fn claim_is_valid(
  case: (ledger_state.HeaderV1, validation_claim_v1.ValidationClaimWitnessV1),
) -> Bool {
  let (header, witness) = case
  validation_claim_v1.committed_claim_is_valid(header, witness)
}

test honest_normal_claim_is_valid() {
  claim_is_valid(accepted_claim(normal_source(native_types.TxIsValid)))
}

test honest_forced_valid_claim_is_valid() {
  claim_is_valid(
    accepted_claim(
      forced_source(native_types.TxIsValid, rejection_reason_v1.ForcedTxValid),
    ),
  )
}

test honest_forced_invalid_claim_is_valid() {
  claim_is_valid(claim_on(forced_invalid_source(), h32_a))
}

test claim_under_another_step_count_is_refused() {
  let (header, witness) = accepted_claim(normal_source(native_types.TxIsValid))
  !validation_claim_v1.committed_claim_is_valid(
    ledger_state.HeaderV1 { ..header, transition_step_count: 2 },
    witness,
  )
}

test claim_whose_step_names_another_event_is_refused() {
  let source = normal_source(native_types.TxIsValid)
  !claim_is_valid(
    claim_naming(
      source,
      ledger_state.ForcedTransactionEventKey { tx_order_id: tx_order_id() },
      h32_b,
    ),
  )
}

test r_desc_a_normal_source_without_redeemers_has_no_trace() {
  !requires_trace(normal_source(native_types.TxIsValid))
}

test r_desc_a_forced_source_has_no_trace() {
  !requires_trace(
    forced_source(native_types.TxIsValid, rejection_reason_v1.ForcedTxValid),
  )
}

test witness_set_is_none_for_a_source_under_another_root() {
  let source = normal_source(native_types.TxIsValid)
  let header = ledger_state.HeaderV1 {
    ..header_for(source),
    transactions_root: env.empty_merkle_tree_root,
  }
  validation_claim_v1.committed_claim_source_witness_set(
    header,
    witness_for(source),
  ) == None
}

test witness_set_is_none_for_an_invalid_normal_source() {
  let source = normal_source(native_types.TxIsInvalid)
  validation_claim_v1.committed_claim_source_witness_set(
    header_for(source),
    witness_for(source),
  ) == None
}

// ### The claim's event

test claim_event_key_is_the_event_to_step_key() {
  let source = normal_source(native_types.TxIsValid)
  let (_, witness) = accepted_claim(source)
  and {
    validation_claim_v1.claim_event_key(witness) == event_key_for(source),
    validation_claim_v1.claim_event_key(witness) == witness.event_to_step_membership.key,
  }
}

test claims_on_one_event_share_their_event_key() {
  let source = normal_source(native_types.TxIsValid)
  let (_, accepted) = claim_on(source, h32_b)
  let (_, unchanged) = claim_on(source, h32_a)
  and {
    accepted != unchanged,
    validation_claim_v1.claim_event_key(accepted) == validation_claim_v1.claim_event_key(
      unchanged,
    ),
  }
}

test claims_on_two_events_have_two_event_keys() {
  let (_, normal) = accepted_claim(normal_source(native_types.TxIsValid))
  let (_, forced) = accepted_claim(
    forced_source(native_types.TxIsValid, rejection_reason_v1.ForcedTxValid),
  )
  validation_claim_v1.claim_event_key(normal) != validation_claim_v1.claim_event_key(
    forced,
  )
}

// ### The witness shape

test claim_witness_round_trips_in_its_four_field_shape() {
  let (_, witness) = accepted_claim(normal_source(native_types.TxIsValid))
  let data: Data = witness
  expect decoded: validation_claim_v1.ValidationClaimWitnessV1 = data
  and {
    decoded == witness,
    list.length(builtin.unconstr_fields(data)) == 4,
  }
}

/// The retired shape carried a validation-trace descriptor membership after
/// the version.
test claim_witness_refuses_the_retired_five_field_shape() fail {
  let (_, witness) = accepted_claim(normal_source(native_types.TxIsValid))
  let data: Data = witness
  expect [version, ..rest] = builtin.unconstr_fields(data)
  let descriptor: Data = witness.event_to_step_membership
  let retired: Data = builtin.constr_data(0, [version, descriptor, ..rest])
  expect decoded: validation_claim_v1.ValidationClaimWitnessV1 = retired
  decoded.version == validation_claim_v1.claim_version
}

// ### The derived initial state
//
// The agreed initial state is derived from committed data alone; each input
// it reads moves it.

fn derived(
  case: (ledger_state.HeaderV1, validation_claim_v1.ValidationClaimWitnessV1),
) -> validation_trace_v1.ValidationMachineStateV1 {
  let (header, witness) = case
  validation_claim_v1.derive_initial_state(
    header,
    validation_claim_v1.claim_event_key(witness),
    witness.source_membership,
    witness.transition_step_membership.value.pre_utxos_root,
  )
}

test derived_initial_state_starts_the_machine() {
  let state = derived(accepted_claim(normal_source(native_types.TxIsValid)))
  and {
    state.prior_ledger_root == h32_a,
    state.source_kind == validation_trace_v1.Normal,
    state.phase == validation_trace_v1.CanonicalDecode,
    state.program_counter == 0,
    state.verdict == validation_trace_v1.Pending,
  }
}

test derived_initial_state_reads_the_header_context() {
  let case = accepted_claim(normal_source(native_types.TxIsValid))
  let (header, witness) = case
  derived(case) != derived(
    (ledger_state.HeaderV1 { ..header, min_fee_a: 45 }, witness),
  )
}

test derived_initial_state_reads_the_source() {
  derived(accepted_claim(normal_source(native_types.TxIsValid))) != derived(
    accepted_claim(
      forced_source(native_types.TxIsValid, rejection_reason_v1.ForcedTxValid),
    ),
  )
}
