use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address.{
  Address, Credential, Inline, Pointer, Script, StakeCredential, VerificationKey,
}
use cardano/assets.{AssetName, Value}
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use env
use midgard/bounded_collection_v1
use midgard/common/types.{Proof}
use midgard/common/utils
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/components as native_components
use midgard/fraud_proofs/native_tx/preimages as native_preimages
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/transition_trace
use midgard/user_events
use midgard/user_events/tx_order_v1 as tx_order
use midgard/user_events/withdrawal
use midgard/validation_claim_v1
use midgard/validation_trace_v1

pub type TransitionFaultProof {
  challenged_header_hash: ledger_state.HeaderHash,
  header: ledger_state.HeaderV1,
  fault: TransitionFault,
}

pub type TraceBoundarySide {
  TraceStart
  TraceEnd
}

pub type TransitionFault {
  TraceBoundaryFault {
    side: TraceBoundarySide,
    trace_proof: transition_trace.IndexedTraceProof,
  }
  TraceLinkFault { adjacent: transition_trace.AdjacentTraceProof }
  EventToStepMismatch {
    trace_proof: transition_trace.IndexedTraceProof,
    event_to_step: transition_trace.EventToStepProof,
  }
  SourceMembershipMismatch { witness: SourceMembershipMismatchWitness }
  InvalidOneStepTransition { witness: InvalidOneStepTransitionWitness }
  OmittedDueL1Event { witness: OmittedDueL1EventWitness }
  DuplicateTraceEvent {
    left_trace: transition_trace.IndexedTraceProof,
    right_trace: transition_trace.IndexedTraceProof,
  }
  OutOfWindowSourceEvent { witness: OutOfWindowSourceEventWitness }
  CountFault { witness: CountFaultWitness }
  AcceptedTransactionTransitionMismatch {
    witness: AcceptedTransactionTransitionMismatchWitness,
  }
}

pub type SourceMembershipProof {
  WithdrawalSourceMembership {
    membership: transition_trace.RootMembershipProof<
      ledger_state.WithdrawalId,
      ledger_state.WithdrawalInfo,
    >,
  }
  ForcedTransactionSourceMembership {
    membership: transition_trace.RootMembershipProof<
      ledger_state.TxOrderId,
      ledger_state.ForcedInclusionTxV1,
    >,
  }
  L2TransactionSourceMembership {
    membership: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  }
  DepositSourceMembership {
    membership: transition_trace.RootMembershipProof<
      ledger_state.DepositId,
      ledger_state.DepositInfo,
    >,
  }
}

pub type SourceNonMembershipProof {
  WithdrawalSourceNonMembership {
    non_membership: transition_trace.RootNonMembershipProof<
      ledger_state.WithdrawalId,
    >,
  }
  ForcedTransactionSourceNonMembership {
    non_membership: transition_trace.RootNonMembershipProof<
      ledger_state.TxOrderId,
    >,
  }
  L2TransactionSourceNonMembership {
    non_membership: transition_trace.RootNonMembershipProof<ByteArray>,
  }
  DepositSourceNonMembership {
    non_membership: transition_trace.RootNonMembershipProof<
      ledger_state.DepositId,
    >,
  }
}

pub type SourceMembershipMismatchWitness {
  MappedEventMissingFromSource {
    trace_proof: transition_trace.IndexedTraceProof,
    event_to_step: transition_trace.RootMembershipProof<
      ledger_state.EventKey,
      ledger_state.EventToStepValue,
    >,
    source_non_membership: SourceNonMembershipProof,
  }
  SourceEventMissingTrace {
    source_membership: SourceMembershipProof,
    event_to_step_non_membership: transition_trace.RootNonMembershipProof<
      ledger_state.EventKey,
    >,
  }
  SourcePhaseMismatch {
    trace_proof: transition_trace.IndexedTraceProof,
    source_membership: SourceMembershipProof,
  }
}

pub type LedgerDeleteWitness {
  key: ByteArray,
  value: ByteArray,
  membership_proof: Proof,
  delete_proof: Proof,
}

pub type LedgerInsertWitness {
  key: ByteArray,
  value: ByteArray,
  non_membership_proof: Proof,
  insert_proof: Proof,
}

pub type InvalidOneStepTransitionWitness {
  ValidWithdrawalTransition {
    trace_proof: transition_trace.IndexedTraceProof,
    event_to_step: transition_trace.RootMembershipProof<
      ledger_state.EventKey,
      ledger_state.EventToStepValue,
    >,
    source_membership: transition_trace.RootMembershipProof<
      ledger_state.WithdrawalId,
      ledger_state.WithdrawalInfo,
    >,
    spent_utxo: LedgerDeleteWitness,
  }
  InvalidWithdrawalNoOpTransition {
    trace_proof: transition_trace.IndexedTraceProof,
    event_to_step: transition_trace.RootMembershipProof<
      ledger_state.EventKey,
      ledger_state.EventToStepValue,
    >,
    source_membership: transition_trace.RootMembershipProof<
      ledger_state.WithdrawalId,
      ledger_state.WithdrawalInfo,
    >,
  }
  InvalidForcedTransactionNoOpTransition {
    trace_proof: transition_trace.IndexedTraceProof,
    event_to_step: transition_trace.RootMembershipProof<
      ledger_state.EventKey,
      ledger_state.EventToStepValue,
    >,
    source_membership: transition_trace.RootMembershipProof<
      ledger_state.TxOrderId,
      ledger_state.ForcedInclusionTxV1,
    >,
  }
  ValidDepositTransition {
    trace_proof: transition_trace.IndexedTraceProof,
    event_to_step: transition_trace.RootMembershipProof<
      ledger_state.EventKey,
      ledger_state.EventToStepValue,
    >,
    source_membership: transition_trace.RootMembershipProof<
      ledger_state.DepositId,
      ledger_state.DepositInfo,
    >,
    event_ref_input_index: Int,
    event_asset_name: AssetName,
    projected_utxo: LedgerInsertWitness,
  }
  L2TransactionTransition {
    trace_proof: transition_trace.IndexedTraceProof,
    event_to_step: transition_trace.RootMembershipProof<
      ledger_state.EventKey,
      ledger_state.EventToStepValue,
    >,
    source_membership: transition_trace.RootMembershipProof<
      ByteArray,
      ByteArray,
    >,
    spend_inputs_preimage: ByteArray,
    outputs_preimage: ByteArray,
    spent_utxos: List<LedgerDeleteWitness>,
    produced_utxos: List<LedgerInsertWitness>,
  }
}

pub type AcceptedTransactionTransitionMismatchWitness {
  claim: validation_claim_v1.ValidationClaimWitnessV1,
  terminal_acceptance_witness_cbor: ByteArray,
}

pub type OmittedDueL1EventWitness {
  OmittedDueDeposit {
    event_ref_input_index: Int,
    event_asset_name: AssetName,
    source_non_membership: transition_trace.RootNonMembershipProof<
      ledger_state.DepositId,
    >,
  }
  OmittedDueWithdrawal {
    event_ref_input_index: Int,
    event_asset_name: AssetName,
    source_non_membership: transition_trace.RootNonMembershipProof<
      ledger_state.WithdrawalId,
    >,
  }
  OmittedDueForcedTransaction {
    event_ref_input_index: Int,
    event_asset_name: AssetName,
    validity_override: ledger_state.MidgardTxValidity,
    source_non_membership: transition_trace.RootNonMembershipProof<
      ledger_state.TxOrderId,
    >,
  }
}

pub type OutOfWindowSourceEventWitness {
  OutOfWindowDeposit {
    event_ref_input_index: Int,
    event_asset_name: AssetName,
    source_membership: transition_trace.RootMembershipProof<
      ledger_state.DepositId,
      ledger_state.DepositInfo,
    >,
  }
  OutOfWindowWithdrawal {
    event_ref_input_index: Int,
    event_asset_name: AssetName,
    validity_override: ledger_state.WithdrawalValidity,
    source_membership: transition_trace.RootMembershipProof<
      ledger_state.WithdrawalId,
      ledger_state.WithdrawalInfo,
    >,
  }
  OutOfWindowForcedTransaction {
    event_ref_input_index: Int,
    event_asset_name: AssetName,
    validity_override: ledger_state.MidgardTxValidity,
    source_membership: transition_trace.RootMembershipProof<
      ledger_state.TxOrderId,
      ledger_state.ForcedInclusionTxV1,
    >,
  }
}

pub type CountFaultWitness {
  HeaderTotalCountMismatch
  HeaderTransitionStepCountMismatch
  SourceRootCountMismatch { proof: transition_trace.RootCountProof }
  EventToStepRootCountMismatch { proof: transition_trace.RootCountProof }
  TransitionTraceRootCountMismatch { proof: transition_trace.RootCountProof }
}

type TimedDepositEvent {
  event: ledger_state.DepositEvent,
  inclusion_time: Int,
}

type AuthenticatedDepositReference {
  event: ledger_state.DepositEvent,
  inclusion_time: Int,
  value: Value,
}

const transition_trace_fraud_category_id = #"00000004"

fn header_counts_are_non_negative(header: ledger_state.HeaderV1) -> Bool {
  and {
    header.withdrawal_count >= 0,
    header.forced_transaction_count >= 0,
    header.l2_transaction_count >= 0,
    header.deposit_count >= 0,
    header.total_event_count >= 0,
    header.transition_step_count >= 0,
  }
}

fn source_count_sum(header: ledger_state.HeaderV1) -> Int {
  header.withdrawal_count + header.forced_transaction_count + header.l2_transaction_count + header.deposit_count
}

pub fn phase_for_step_index(
  header: ledger_state.HeaderV1,
  step_index: Int,
) -> ledger_state.TransitionPhase {
  expect header_counts_are_non_negative(header)
  expect step_index >= 0
  expect step_index < header.total_event_count

  let withdrawals_end = header.withdrawal_count
  let forced_end = withdrawals_end + header.forced_transaction_count
  let l2_end = forced_end + header.l2_transaction_count

  if step_index < withdrawals_end {
    ledger_state.Withdrawal
  } else if step_index < forced_end {
    ledger_state.ForcedTransaction
  } else if step_index < l2_end {
    ledger_state.L2Transaction
  } else {
    expect step_index < l2_end + header.deposit_count
    ledger_state.Deposit
  }
}

pub fn event_key_phase(
  event_key: ledger_state.EventKey,
) -> ledger_state.TransitionPhase {
  when event_key is {
    ledger_state.WithdrawalEventKey { .. } -> ledger_state.Withdrawal
    ledger_state.ForcedTransactionEventKey { .. } ->
      ledger_state.ForcedTransaction
    ledger_state.L2TransactionEventKey { .. } -> ledger_state.L2Transaction
    ledger_state.DepositEventKey { .. } -> ledger_state.Deposit
  }
}

fn event_key_from_source_membership(
  source_membership: SourceMembershipProof,
) -> ledger_state.EventKey {
  when source_membership is {
    WithdrawalSourceMembership { membership } ->
      ledger_state.WithdrawalEventKey { withdrawal_id: membership.key }
    ForcedTransactionSourceMembership { membership } ->
      ledger_state.ForcedTransactionEventKey { tx_order_id: membership.key }
    L2TransactionSourceMembership { membership } ->
      ledger_state.L2TransactionEventKey { tx_id: membership.key }
    DepositSourceMembership { membership } ->
      ledger_state.DepositEventKey { deposit_id: membership.key }
  }
}

fn source_membership_phase(
  source_membership: SourceMembershipProof,
) -> ledger_state.TransitionPhase {
  event_key_from_source_membership(source_membership) |> event_key_phase
}

fn source_non_membership_event_key(
  source_non_membership: SourceNonMembershipProof,
) -> ledger_state.EventKey {
  when source_non_membership is {
    WithdrawalSourceNonMembership { non_membership } ->
      ledger_state.WithdrawalEventKey { withdrawal_id: non_membership.key }
    ForcedTransactionSourceNonMembership { non_membership } ->
      ledger_state.ForcedTransactionEventKey { tx_order_id: non_membership.key }
    L2TransactionSourceNonMembership { non_membership } ->
      ledger_state.L2TransactionEventKey { tx_id: non_membership.key }
    DepositSourceNonMembership { non_membership } ->
      ledger_state.DepositEventKey { deposit_id: non_membership.key }
  }
}

fn verify_event_to_step_membership(
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
) -> Bool {
  transition_trace.verify_root_membership_with_bytes(
    membership,
    transition_trace.EventToStepRootDomain,
    header.event_to_step_root,
    header.total_event_count,
    cbor.serialise(membership.key),
    cbor.serialise(membership.value),
  )
}

fn verify_event_to_step_non_membership(
  header: ledger_state.HeaderV1,
  non_membership: transition_trace.RootNonMembershipProof<ledger_state.EventKey>,
) -> Bool {
  transition_trace.verify_root_non_membership_with_key_bytes(
    non_membership,
    transition_trace.EventToStepRootDomain,
    header.event_to_step_root,
    header.total_event_count,
    cbor.serialise(non_membership.key),
  )
}

fn verify_source_membership(
  header: ledger_state.HeaderV1,
  source_membership: SourceMembershipProof,
) -> Bool {
  when source_membership is {
    WithdrawalSourceMembership { membership } ->
      transition_trace.verify_root_membership_with_bytes(
        membership,
        transition_trace.WithdrawalsRootDomain,
        header.withdrawals_root,
        header.withdrawal_count,
        cbor.serialise(membership.key),
        cbor.serialise(membership.value),
      )
    ForcedTransactionSourceMembership { membership } ->
      transition_trace.verify_root_membership_with_bytes(
        membership,
        transition_trace.ForcedTransactionsV1RootDomain,
        header.forced_transactions_root,
        header.forced_transaction_count,
        cbor.serialise(membership.key),
        cbor.serialise(membership.value),
      )
    L2TransactionSourceMembership { membership } ->
      transition_trace.verify_root_membership_with_bytes(
        membership,
        transition_trace.TransactionsV1RootDomain,
        header.transactions_root,
        header.l2_transaction_count,
        membership.key,
        membership.value,
      )
    DepositSourceMembership { membership } ->
      transition_trace.verify_root_membership_with_bytes(
        membership,
        transition_trace.DepositsRootDomain,
        header.deposits_root,
        header.deposit_count,
        cbor.serialise(membership.key),
        cbor.serialise(membership.value),
      )
  }
}

fn verify_source_non_membership(
  header: ledger_state.HeaderV1,
  source_non_membership: SourceNonMembershipProof,
) -> Bool {
  when source_non_membership is {
    WithdrawalSourceNonMembership { non_membership } ->
      transition_trace.verify_root_non_membership_with_key_bytes(
        non_membership,
        transition_trace.WithdrawalsRootDomain,
        header.withdrawals_root,
        header.withdrawal_count,
        cbor.serialise(non_membership.key),
      )
    ForcedTransactionSourceNonMembership { non_membership } ->
      transition_trace.verify_root_non_membership_with_key_bytes(
        non_membership,
        transition_trace.ForcedTransactionsV1RootDomain,
        header.forced_transactions_root,
        header.forced_transaction_count,
        cbor.serialise(non_membership.key),
      )
    L2TransactionSourceNonMembership { non_membership } ->
      transition_trace.verify_root_non_membership_with_key_bytes(
        non_membership,
        transition_trace.TransactionsV1RootDomain,
        header.transactions_root,
        header.l2_transaction_count,
        non_membership.key,
      )
    DepositSourceNonMembership { non_membership } ->
      transition_trace.verify_root_non_membership_with_key_bytes(
        non_membership,
        transition_trace.DepositsRootDomain,
        header.deposits_root,
        header.deposit_count,
        cbor.serialise(non_membership.key),
      )
  }
}

fn verify_trace_binding_to_event_to_step(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
) -> Bool {
  and {
    transition_trace.verify_indexed_trace_proof(
      trace_proof,
      header.transition_trace_root,
      header.transition_step_count,
    ),
    verify_event_to_step_membership(header, event_to_step),
    event_to_step.key == trace_proof.value.event_key,
    event_to_step.value.step_index == trace_proof.value.step_index,
    event_to_step.value.phase == trace_proof.value.phase,
  }
}

fn validate_trace_boundary(
  header: ledger_state.HeaderV1,
  side: TraceBoundarySide,
  trace_proof: transition_trace.IndexedTraceProof,
) -> Bool {
  expect header.transition_step_count > 0
  expect
    transition_trace.verify_indexed_trace_proof(
      trace_proof,
      header.transition_trace_root,
      header.transition_step_count,
    )

  when side is {
    TraceStart -> and {
        trace_proof.key == 0,
        trace_proof.value.pre_utxos_root != header.prev_utxos_root,
      }
    TraceEnd -> and {
        trace_proof.key == header.transition_step_count - 1,
        trace_proof.value.post_utxos_root != header.utxos_root,
      }
  }
}

fn validate_trace_link(
  header: ledger_state.HeaderV1,
  adjacent: transition_trace.AdjacentTraceProof,
) -> Bool {
  and {
    transition_trace.verify_adjacent_trace_proof(
      adjacent,
      header.transition_trace_root,
      header.transition_step_count,
    ),
    adjacent.lower.value.post_utxos_root != adjacent.upper.value.pre_utxos_root,
  }
}

fn trace_has_bad_phase(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
) -> Bool {
  or {
    trace_proof.value.phase != phase_for_step_index(
      header,
      trace_proof.value.step_index,
    ),
    event_key_phase(trace_proof.value.event_key) != trace_proof.value.phase,
  }
}

fn validate_event_to_step_mismatch(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.EventToStepProof,
) -> Bool {
  expect
    transition_trace.verify_indexed_trace_proof(
      trace_proof,
      header.transition_trace_root,
      header.transition_step_count,
    )

  when event_to_step is {
    transition_trace.EventToStepMembership { membership } -> {
      expect verify_event_to_step_membership(header, membership)
      and {
        membership.key == trace_proof.value.event_key,
        or {
          membership.value.step_index != trace_proof.value.step_index,
          membership.value.phase != trace_proof.value.phase,
          trace_has_bad_phase(header, trace_proof),
        },
      }
    }
    transition_trace.EventToStepNonMembership { non_membership } -> and {
        verify_event_to_step_non_membership(header, non_membership),
        non_membership.key == trace_proof.value.event_key,
      }
  }
}

fn validate_source_membership_mismatch(
  header: ledger_state.HeaderV1,
  witness: SourceMembershipMismatchWitness,
) -> Bool {
  when witness is {
    MappedEventMissingFromSource {
      trace_proof,
      event_to_step,
      source_non_membership,
    } -> and {
        verify_trace_binding_to_event_to_step(
          header,
          trace_proof,
          event_to_step,
        ),
        verify_source_non_membership(header, source_non_membership),
        source_non_membership_event_key(source_non_membership) == trace_proof.value.event_key,
      }
    SourceEventMissingTrace { source_membership, event_to_step_non_membership } -> {
      let event_key = event_key_from_source_membership(source_membership)
      and {
        verify_source_membership(header, source_membership),
        verify_event_to_step_non_membership(
          header,
          event_to_step_non_membership,
        ),
        event_to_step_non_membership.key == event_key,
      }
    }
    SourcePhaseMismatch { trace_proof, source_membership } -> {
      let event_key = event_key_from_source_membership(source_membership)
      and {
        transition_trace.verify_indexed_trace_proof(
          trace_proof,
          header.transition_trace_root,
          header.transition_step_count,
        ),
        verify_source_membership(header, source_membership),
        trace_proof.value.event_key == event_key,
        trace_proof.value.phase != source_membership_phase(source_membership),
      }
    }
  }
}

fn ledger_trie(root: ByteArray) {
  if root == env.empty_merkle_tree_root {
    mpf.empty
  } else {
    mpf.from_root(root)
  }
}

fn ledger_outref_key(outref: OutputReference) -> ByteArray {
  native_components.encode_midgard_tx_input(
    native_types.MidgardTxInput {
      tx_id: outref.transaction_id,
      output_index: outref.output_index,
    },
  )
}

fn verify_ledger_membership(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: Proof,
) -> Bool {
  mpf.has(ledger_trie(root), key, value, proof)
}

fn verify_ledger_non_membership(
  root: ByteArray,
  key: ByteArray,
  proof: Proof,
) -> Bool {
  expect _inserted = mpf.insert(ledger_trie(root), key, env.empty, proof)
  True
}

fn apply_delete_witness(
  root: ByteArray,
  witness: LedgerDeleteWitness,
) -> ByteArray {
  expect
    verify_ledger_membership(
      root,
      witness.key,
      witness.value,
      witness.membership_proof,
    )
  delete_root(root, witness.key, witness.value, witness.delete_proof)
}

fn apply_insert_witness(
  root: ByteArray,
  witness: LedgerInsertWitness,
) -> ByteArray {
  expect
    verify_ledger_non_membership(
      root,
      witness.key,
      witness.non_membership_proof,
    )
  insert_root(root, witness.key, witness.value, witness.insert_proof)
}

fn validate_withdrawal_one_step_binding(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  source_membership: transition_trace.RootMembershipProof<
    ledger_state.WithdrawalId,
    ledger_state.WithdrawalInfo,
  >,
) -> Bool {
  and {
    verify_trace_binding_to_event_to_step(header, trace_proof, event_to_step),
    verify_source_membership(
      header,
      WithdrawalSourceMembership { membership: source_membership },
    ),
    trace_proof.value.event_key == ledger_state.WithdrawalEventKey {
      withdrawal_id: source_membership.key,
    },
    trace_proof.value.phase == ledger_state.Withdrawal,
  }
}

fn validate_forced_transaction_one_step_binding(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  source_membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
) -> Bool {
  and {
    verify_trace_binding_to_event_to_step(header, trace_proof, event_to_step),
    verify_source_membership(
      header,
      ForcedTransactionSourceMembership { membership: source_membership },
    ),
    trace_proof.value.event_key == ledger_state.ForcedTransactionEventKey {
      tx_order_id: source_membership.key,
    },
    trace_proof.value.phase == ledger_state.ForcedTransaction,
  }
}

fn validate_deposit_one_step_binding(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  source_membership: transition_trace.RootMembershipProof<
    ledger_state.DepositId,
    ledger_state.DepositInfo,
  >,
) -> Bool {
  and {
    verify_trace_binding_to_event_to_step(header, trace_proof, event_to_step),
    verify_source_membership(
      header,
      DepositSourceMembership { membership: source_membership },
    ),
    trace_proof.value.event_key == ledger_state.DepositEventKey {
      deposit_id: source_membership.key,
    },
    trace_proof.value.phase == ledger_state.Deposit,
  }
}

fn cardano_credential_to_midgard(
  credential: Credential,
) -> native_types.MidgardCredential {
  when credential is {
    VerificationKey(hash) -> native_types.PubKeyCredential(hash)
    Script(hash) -> native_types.ScriptCredential(hash)
  }
}

fn cardano_stake_credential_to_midgard(
  stake_credential: StakeCredential,
) -> native_types.MidgardCredential {
  when stake_credential is {
    Inline(credential) -> cardano_credential_to_midgard(credential)
    Pointer { .. } -> fail
  }
}

fn deposit_address_to_midgard(
  address: Address,
  network_id: Int,
) -> native_types.MidgardAddress {
  expect or {
      network_id == 0,
      network_id == 1,
    }
  native_types.MidgardAddress {
    protected: False,
    network_id,
    payment_credential: cardano_credential_to_midgard(
      address.payment_credential,
    ),
    stake_credential: when address.stake_credential is {
      Some(stake_credential) ->
        Some(cardano_stake_credential_to_midgard(stake_credential))
      None -> None
    },
  }
}

fn cardano_asset_pairs_to_midgard(
  flattened: List<(ByteArray, ByteArray, Int)>,
) -> List<Pair<ByteArray, Int>> {
  when flattened is {
    [] -> []
    [asset, ..rest] -> {
      let (policy_id, asset_name, quantity) = asset
      if and {
        policy_id == assets.ada_policy_id,
        asset_name == assets.ada_asset_name,
      } {
        cardano_asset_pairs_to_midgard(rest)
      } else {
        expect quantity > 0
        [
          Pair(policy_id |> bytearray.concat(asset_name), quantity),
          ..cardano_asset_pairs_to_midgard(rest)
        ]
      }
    }
  }
}

fn projected_deposit_value(
  value: Value,
  deposit_policy_id: ByteArray,
  event_asset_name: AssetName,
) -> native_types.MidgardValue {
  expect assets.quantity_of(value, deposit_policy_id, event_asset_name) == 1
  let projected_value = value
    |> assets.add(deposit_policy_id, event_asset_name, -1)
  let lovelace = assets.lovelace_of(projected_value)
  expect lovelace >= 0
  native_types.MidgardValue {
    lovelace,
    assets: projected_value
      |> assets.flatten
      |> cardano_asset_pairs_to_midgard,
  }
}

fn deposit_datum_cbor(datum: Option<Data>) -> Option<ByteArray> {
  when datum is {
    Some(datum) -> Some(cbor.serialise(datum))
    None -> None
  }
}

fn projected_deposit_output_cbor(
  deposit_info: ledger_state.DepositInfo,
  deposit_value: Value,
  deposit_policy_id: ByteArray,
  event_asset_name: AssetName,
) -> ByteArray {
  native_components.encode_midgard_tx_output(
    native_types.MidgardTxOutput {
      address: deposit_address_to_midgard(
        deposit_info.l2_address,
        deposit_info.l2_network_id,
      ),
      value: projected_deposit_value(
        deposit_value,
        deposit_policy_id,
        event_asset_name,
      ),
      datum_cbor: deposit_datum_cbor(deposit_info.l2_datum),
      script_ref: None,
    },
  )
}

fn verify_l2_source_membership(
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
) -> Bool {
  transition_trace.verify_root_membership_with_bytes(
    membership,
    transition_trace.TransactionsV1RootDomain,
    header.transactions_root,
    header.l2_transaction_count,
    membership.key,
    membership.value,
  )
}

fn validate_l2_one_step_binding(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  source_membership: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
) -> Bool {
  expect ledger_state.L2TransactionEventKey { tx_id } = trace_proof.value.event_key
  and {
    verify_trace_binding_to_event_to_step(header, trace_proof, event_to_step),
    verify_l2_source_membership(header, source_membership),
    source_membership.key == tx_id,
    trace_proof.value.phase == ledger_state.L2Transaction,
  }
}

fn apply_l2_spends(
  root: ByteArray,
  spend_inputs: List<native_types.MidgardTxInput>,
  witnesses: List<LedgerDeleteWitness>,
) -> ByteArray {
  when spend_inputs is {
    [] -> {
      expect witnesses == []
      root
    }
    [spend_input, ..rest] -> {
      expect [witness, ..remaining_witnesses] = witnesses
      let expected_key = native_components.encode_midgard_tx_input(spend_input)
      expect witness.key == expected_key
      apply_l2_spends(
        apply_delete_witness(root, witness),
        rest,
        remaining_witnesses,
      )
    }
  }
}

fn apply_l2_outputs(
  root: ByteArray,
  tx_id: ByteArray,
  output_index: Int,
  outputs: List<native_types.MidgardTxOutput>,
  witnesses: List<LedgerInsertWitness>,
) -> ByteArray {
  when outputs is {
    [] -> {
      expect witnesses == []
      root
    }
    [output, ..rest] -> {
      expect [witness, ..remaining_witnesses] = witnesses
      let expected_key = native_components.encode_midgard_tx_input(
        native_types.MidgardTxInput { tx_id, output_index },
      )
      let expected_value = native_components.encode_midgard_tx_output(output)
      expect witness.key == expected_key
      expect witness.value == expected_value
      apply_l2_outputs(
        apply_insert_witness(root, witness),
        tx_id,
        output_index + 1,
        rest,
        remaining_witnesses,
      )
    }
  }
}

fn validate_valid_withdrawal_transition(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  source_membership: transition_trace.RootMembershipProof<
    ledger_state.WithdrawalId,
    ledger_state.WithdrawalInfo,
  >,
  spent_utxo: LedgerDeleteWitness,
) -> Bool {
  expect source_membership.value.validity == ledger_state.WithdrawalIsValid
  let expected_key = ledger_outref_key(source_membership.value.body.l2_outref)
  expect spent_utxo.key == expected_key
  let expected_post_root = apply_delete_witness(
    trace_proof.value.pre_utxos_root,
    spent_utxo,
  )
  and {
    validate_withdrawal_one_step_binding(
      header,
      trace_proof,
      event_to_step,
      source_membership,
    ),
    expected_post_root != trace_proof.value.post_utxos_root,
  }
}

fn validate_invalid_withdrawal_no_op_transition(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  source_membership: transition_trace.RootMembershipProof<
    ledger_state.WithdrawalId,
    ledger_state.WithdrawalInfo,
  >,
) -> Bool {
  expect source_membership.value.validity != ledger_state.WithdrawalIsValid
  and {
    validate_withdrawal_one_step_binding(
      header,
      trace_proof,
      event_to_step,
      source_membership,
    ),
    trace_proof.value.pre_utxos_root != trace_proof.value.post_utxos_root,
  }
}

fn validate_invalid_forced_transaction_no_op_transition(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  source_membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
) -> Bool {
  expect source_membership.value.operator_validity != ledger_state.TxIsValid
  and {
    validate_forced_transaction_one_step_binding(
      header,
      trace_proof,
      event_to_step,
      source_membership,
    ),
    trace_proof.value.pre_utxos_root != trace_proof.value.post_utxos_root,
  }
}

fn validate_valid_deposit_transition(
  header: ledger_state.HeaderV1,
  hub_datum: hub.Datum,
  reference_inputs: List<Input>,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  source_membership: transition_trace.RootMembershipProof<
    ledger_state.DepositId,
    ledger_state.DepositInfo,
  >,
  event_ref_input_index: Int,
  event_asset_name: AssetName,
  projected_utxo: LedgerInsertWitness,
) -> Bool {
  let AuthenticatedDepositReference { event, value, .. } = get_authenticated_deposit_reference(
    reference_inputs,
    hub_datum.deposit,
    event_asset_name,
    event_ref_input_index,
  )
  let expected_key = ledger_outref_key(source_membership.key)
  let expected_value = projected_deposit_output_cbor(
    source_membership.value,
    value,
    hub_datum.deposit,
    event_asset_name,
  )
  let checked_projected_utxo = LedgerInsertWitness {
    key: expected_key,
    value: expected_value,
    non_membership_proof: projected_utxo.non_membership_proof,
    insert_proof: projected_utxo.insert_proof,
  }
  let expected_post_root = apply_insert_witness(
    trace_proof.value.pre_utxos_root,
    checked_projected_utxo,
  )
  and {
    validate_deposit_one_step_binding(
      header,
      trace_proof,
      event_to_step,
      source_membership,
    ),
    event.id == source_membership.key,
    event.info == source_membership.value,
    projected_utxo.key == expected_key,
    projected_utxo.value == expected_value,
    expected_post_root != trace_proof.value.post_utxos_root,
  }
}

fn validate_l2_transaction_transition(
  header: ledger_state.HeaderV1,
  trace_proof: transition_trace.IndexedTraceProof,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  source_membership: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  spend_inputs_preimage: ByteArray,
  outputs_preimage: ByteArray,
  spent_utxos: List<LedgerDeleteWitness>,
  produced_utxos: List<LedgerInsertWitness>,
) -> Bool {
  expect Some(source_data) = cbor.deserialise(source_membership.value)
  expect source: ledger_state.L2TransactionSourceV1 = source_data
  let Pair(verified, _witness_set) = native_compact.verify_native_tx_proof_source_v1(
    source_membership.key,
    source.source.compact_cbor,
    source.source.witness_set_compact_cbor,
    source.source.field_preimage_lengths_cbor,
  )
  let compact = verified.tx_compact
  expect compact.validity_code == 0
  let spend_inputs = native_preimages.decode_midgard_tx_inputs_preimage_cbor(
    spend_inputs_preimage,
  )
  let outputs = native_preimages.decode_midgard_tx_outputs_preimage_cbor(
    outputs_preimage,
  )
  expect
    bounded_collection_v1.from_items(
      0,
      spend_inputs |> list.map(native_components.encode_midgard_tx_input),
    ) == compact.body.spend_inputs_hash
  expect
    bounded_collection_v1.from_items(
      2,
      outputs |> list.map(native_components.encode_midgard_tx_output),
    ) == compact.body.outputs_hash
  let after_spends = apply_l2_spends(
    trace_proof.value.pre_utxos_root,
    spend_inputs,
    spent_utxos,
  )
  let expected_post_root = apply_l2_outputs(
    after_spends,
    source_membership.key,
    0,
    outputs,
    produced_utxos,
  )
  and {
    validate_l2_one_step_binding(
      header,
      trace_proof,
      event_to_step,
      source_membership,
    ),
    cbor.serialise(source) == source_membership.value,
    source.tx_id == source_membership.key,
    source.transaction_commitment == native_compact.native_tx_proof_commitment_v1(
      source.source.compact_cbor,
      source.source.witness_set_compact_cbor,
      source.source.field_preimage_lengths_cbor,
    ),
    verified.version == 1,
    expected_post_root != trace_proof.value.post_utxos_root,
  }
}

fn validate_invalid_one_step_transition(
  header: ledger_state.HeaderV1,
  hub_datum: hub.Datum,
  reference_inputs: List<Input>,
  witness: InvalidOneStepTransitionWitness,
) -> Bool {
  when witness is {
    ValidWithdrawalTransition {
      trace_proof,
      event_to_step,
      source_membership,
      spent_utxo,
    } ->
      validate_valid_withdrawal_transition(
        header,
        trace_proof,
        event_to_step,
        source_membership,
        spent_utxo,
      )
    InvalidWithdrawalNoOpTransition {
      trace_proof,
      event_to_step,
      source_membership,
    } ->
      validate_invalid_withdrawal_no_op_transition(
        header,
        trace_proof,
        event_to_step,
        source_membership,
      )
    InvalidForcedTransactionNoOpTransition {
      trace_proof,
      event_to_step,
      source_membership,
    } ->
      validate_invalid_forced_transaction_no_op_transition(
        header,
        trace_proof,
        event_to_step,
        source_membership,
      )
    ValidDepositTransition {
      trace_proof,
      event_to_step,
      source_membership,
      event_ref_input_index,
      event_asset_name,
      projected_utxo,
    } ->
      validate_valid_deposit_transition(
        header,
        hub_datum,
        reference_inputs,
        trace_proof,
        event_to_step,
        source_membership,
        event_ref_input_index,
        event_asset_name,
        projected_utxo,
      )
    L2TransactionTransition {
      trace_proof,
      event_to_step,
      source_membership,
      spend_inputs_preimage,
      outputs_preimage,
      spent_utxos,
      produced_utxos,
    } ->
      validate_l2_transaction_transition(
        header,
        trace_proof,
        event_to_step,
        source_membership,
        spend_inputs_preimage,
        outputs_preimage,
        spent_utxos,
        produced_utxos,
      )
  }
}

fn terminal_acceptance_post_root(witness_cbor: ByteArray) -> ByteArray {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect [version_data,
        tag_data, root_data, _frontier_data] = builtin.un_list_data(data)
  expect builtin.un_i_data(version_data) == 1
  expect builtin.un_b_data(tag_data) == #""
  let root = builtin.un_b_data(root_data)
  expect bytearray.length(root) == 32
  root
}

fn validate_accepted_transaction_transition_mismatch(
  header: ledger_state.HeaderV1,
  witness: AcceptedTransactionTransitionMismatchWitness,
) -> Bool {
  let claim = witness.claim
  let descriptor = claim.descriptor_membership.value
  let terminal = claim.terminal_state
  let post_root = terminal_acceptance_post_root(
    witness.terminal_acceptance_witness_cbor,
  )
  and {
    validation_claim_v1.committed_claim_is_valid(header, claim),
    descriptor.verdict == validation_trace_v1.Accepted,
    terminal.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      terminal.program_counter,
      witness.terminal_acceptance_witness_cbor,
    ),
    post_root != claim.transition_step_membership.value.post_utxos_root,
  }
}

fn timed_l1_event_is_due(
  header: ledger_state.HeaderV1,
  inclusion_time: Int,
) -> Bool {
  and {
    header.start_time < inclusion_time,
    inclusion_time <= header.end_time,
  }
}

fn forced_tx_is_due(
  header: ledger_state.HeaderV1,
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> Bool {
  if validity_interval_start == -1 {
    validity_interval_end == -1 || header.start_time <= validity_interval_end
  } else if validity_interval_end == -1 {
    validity_interval_start <= header.end_time
  } else {
    and {
      validity_interval_start <= validity_interval_end,
      validity_interval_start <= header.end_time,
      header.start_time <= validity_interval_end,
    }
  }
}

fn forced_event_is_due(
  header: ledger_state.HeaderV1,
  inclusion_time: Int,
  compact_body: native_types.NativeTxBodyCompact,
) -> Bool {
  and {
    timed_l1_event_is_due(header, inclusion_time),
    forced_tx_is_due(
      header,
      compact_body.validity_interval_start,
      compact_body.validity_interval_end,
    ),
  }
}

fn tx_order_compact_body(
  payload: ledger_state.TxOrderPayloadV1,
) -> native_types.NativeTxBodyCompact {
  let Pair(verified, _witness_set) = native_compact.verify_native_tx_proof_source_v1(
    payload.tx_id,
    payload.source.compact_cbor,
    payload.source.witness_set_compact_cbor,
    payload.source.field_preimage_lengths_cbor,
  )
  expect
    payload.transaction_commitment == native_compact.native_tx_proof_commitment_v1(
      payload.source.compact_cbor,
      payload.source.witness_set_compact_cbor,
      payload.source.field_preimage_lengths_cbor,
    )
  verified.tx_compact.body
}

fn get_authenticated_deposit_reference(
  reference_inputs: List<Input>,
  deposit_policy_id: ByteArray,
  event_asset_name: AssetName,
  event_ref_input_index: Int,
) -> AuthenticatedDepositReference {
  expect Some(reference_input) = list.at(
    reference_inputs,
    event_ref_input_index,
  )
  let Input { output, .. } = reference_input
  let Output { datum, value, .. } = output
  expect InlineDatum(datum_data) = datum
  expect assets.quantity_of(value, deposit_policy_id, event_asset_name) == 1
  expect [event_data,
        inclusion_time_data, ..] = builtin.unconstr_fields(datum_data)
  let Pair(deposit_id_data, deposit_info_data) = ledger_state.unsafe_event_to_key_value_pair(
    event_data,
  )
  expect deposit_id: ledger_state.DepositId = deposit_id_data
  expect deposit_info: ledger_state.DepositInfo = deposit_info_data

  AuthenticatedDepositReference {
    event: ledger_state.DepositEvent { id: deposit_id, info: deposit_info },
    inclusion_time: builtin.un_i_data(inclusion_time_data),
    value,
  }
}

fn get_deposit_event_with_nft(
  reference_inputs: List<Input>,
  deposit_policy_id: ByteArray,
  event_asset_name: AssetName,
  event_ref_input_index: Int,
) -> TimedDepositEvent {
  let AuthenticatedDepositReference { event, inclusion_time, .. } = get_authenticated_deposit_reference(
    reference_inputs,
    deposit_policy_id,
    event_asset_name,
    event_ref_input_index,
  )
  TimedDepositEvent { event, inclusion_time }
}

fn get_withdrawal_datum_with_nft(
  reference_inputs: List<Input>,
  withdrawal_policy_id: ByteArray,
  event_asset_name: AssetName,
  event_ref_input_index: Int,
) -> withdrawal.Datum {
  expect Input { output: Output { datum: InlineDatum(datum_data), .. }, .. } = utils.get_authentic_input_with_nft_at(
    reference_inputs,
    withdrawal_policy_id,
    event_asset_name,
    event_ref_input_index,
  )
  expect datum: withdrawal.Datum = datum_data
  datum
}

fn get_tx_order_datum_with_nft(
  reference_inputs: List<Input>,
  tx_order_policy_id: ByteArray,
  event_asset_name: AssetName,
  event_ref_input_index: Int,
) -> tx_order.Datum {
  expect Input { output: Output { datum: InlineDatum(datum_data), .. }, .. } = utils.get_authentic_input_with_nft_at(
    reference_inputs,
    tx_order_policy_id,
    event_asset_name,
    event_ref_input_index,
  )
  expect datum: tx_order.Datum = datum_data
  datum
}

fn validate_omitted_due_l1_event(
  header: ledger_state.HeaderV1,
  hub_datum: hub.Datum,
  reference_inputs: List<Input>,
  witness: OmittedDueL1EventWitness,
) -> Bool {
  when witness is {
    OmittedDueDeposit {
      event_ref_input_index,
      event_asset_name,
      source_non_membership,
    } -> {
      let TimedDepositEvent { event, inclusion_time } = get_deposit_event_with_nft(
        reference_inputs,
        hub_datum.deposit,
        event_asset_name,
        event_ref_input_index,
      )
      and {
        timed_l1_event_is_due(header, inclusion_time),
        source_non_membership.key == event.id,
        transition_trace.verify_root_non_membership_with_key_bytes(
          source_non_membership,
          transition_trace.DepositsRootDomain,
          header.deposits_root,
          header.deposit_count,
          cbor.serialise(source_non_membership.key),
        ),
      }
    }
    OmittedDueWithdrawal {
      event_ref_input_index,
      event_asset_name,
      source_non_membership,
    } -> {
      let withdrawal_datum = get_withdrawal_datum_with_nft(
        reference_inputs,
        hub_datum.withdrawal,
        event_asset_name,
        event_ref_input_index,
      )
      let user_events.OptimisticDatum { event, inclusion_time, .. } = withdrawal_datum
      and {
        timed_l1_event_is_due(header, inclusion_time),
        source_non_membership.key == event.id,
        transition_trace.verify_root_non_membership_with_key_bytes(
          source_non_membership,
          transition_trace.WithdrawalsRootDomain,
          header.withdrawals_root,
          header.withdrawal_count,
          cbor.serialise(source_non_membership.key),
        ),
      }
    }
    OmittedDueForcedTransaction {
      event_ref_input_index,
      event_asset_name,
      validity_override: _validity_override,
      source_non_membership,
    } -> {
      let tx_order_datum = get_tx_order_datum_with_nft(
        reference_inputs,
        hub_datum.tx_order,
        event_asset_name,
        event_ref_input_index,
      )
      let user_events.OptimisticDatum { event, inclusion_time, .. } = tx_order_datum
      let compact_body = tx_order_compact_body(event.tx)
      and {
        forced_event_is_due(header, inclusion_time, compact_body),
        source_non_membership.key == event.id,
        transition_trace.verify_root_non_membership_with_key_bytes(
          source_non_membership,
          transition_trace.ForcedTransactionsV1RootDomain,
          header.forced_transactions_root,
          header.forced_transaction_count,
          cbor.serialise(source_non_membership.key),
        ),
      }
    }
  }
}

fn validate_out_of_window_source_event(
  header: ledger_state.HeaderV1,
  hub_datum: hub.Datum,
  reference_inputs: List<Input>,
  witness: OutOfWindowSourceEventWitness,
) -> Bool {
  when witness is {
    OutOfWindowDeposit {
      event_ref_input_index,
      event_asset_name,
      source_membership,
    } -> {
      let TimedDepositEvent { event, inclusion_time } = get_deposit_event_with_nft(
        reference_inputs,
        hub_datum.deposit,
        event_asset_name,
        event_ref_input_index,
      )
      and {
        !timed_l1_event_is_due(header, inclusion_time),
        source_membership.key == event.id,
        source_membership.value == event.info,
        verify_source_membership(
          header,
          DepositSourceMembership { membership: source_membership },
        ),
      }
    }
    OutOfWindowWithdrawal {
      event_ref_input_index,
      event_asset_name,
      validity_override,
      source_membership,
    } -> {
      let withdrawal_datum = get_withdrawal_datum_with_nft(
        reference_inputs,
        hub_datum.withdrawal,
        event_asset_name,
        event_ref_input_index,
      )
      let user_events.OptimisticDatum { event, inclusion_time, .. } = withdrawal_datum
      let expected_info = ledger_state.WithdrawalInfo {
        ..event.info,
        validity: validity_override,
      }
      and {
        !timed_l1_event_is_due(header, inclusion_time),
        source_membership.key == event.id,
        source_membership.value == expected_info,
        verify_source_membership(
          header,
          WithdrawalSourceMembership { membership: source_membership },
        ),
      }
    }
    OutOfWindowForcedTransaction {
      event_ref_input_index,
      event_asset_name,
      validity_override,
      source_membership,
    } -> {
      let tx_order_datum = get_tx_order_datum_with_nft(
        reference_inputs,
        hub_datum.tx_order,
        event_asset_name,
        event_ref_input_index,
      )
      let user_events.OptimisticDatum { event, inclusion_time, .. } = tx_order_datum
      let expected_source_value = ledger_state.ForcedInclusionTxV1 {
        tx_id: event.tx.tx_id,
        transaction_commitment: event.tx.transaction_commitment,
        source: event.tx.source,
        operator_validity: validity_override,
      }
      let compact_body = tx_order_compact_body(event.tx)
      and {
        !forced_event_is_due(header, inclusion_time, compact_body),
        source_membership.key == event.id,
        source_membership.value == expected_source_value,
        verify_source_membership(
          header,
          ForcedTransactionSourceMembership { membership: source_membership },
        ),
      }
    }
  }
}

fn validate_duplicate_trace_event(
  header: ledger_state.HeaderV1,
  left: transition_trace.IndexedTraceProof,
  right: transition_trace.IndexedTraceProof,
) -> Bool {
  and {
    transition_trace.verify_indexed_trace_proof(
      left,
      header.transition_trace_root,
      header.transition_step_count,
    ),
    transition_trace.verify_indexed_trace_proof(
      right,
      header.transition_trace_root,
      header.transition_step_count,
    ),
    left.key != right.key,
    left.value.event_key == right.value.event_key,
  }
}

fn validate_count_fault(
  header: ledger_state.HeaderV1,
  witness: CountFaultWitness,
) -> Bool {
  when witness is {
    HeaderTotalCountMismatch ->
      header.total_event_count != source_count_sum(header)
    HeaderTransitionStepCountMismatch ->
      header.transition_step_count != header.total_event_count
    SourceRootCountMismatch { proof } ->
      when proof.domain is {
        transition_trace.WithdrawalsRootDomain -> and {
            transition_trace.verify_root_count_proof(
              proof,
              transition_trace.WithdrawalsRootDomain,
              header.withdrawals_root,
              proof.count,
            ),
            proof.count != header.withdrawal_count,
          }
        transition_trace.ForcedTransactionsV1RootDomain -> and {
            transition_trace.verify_root_count_proof(
              proof,
              transition_trace.ForcedTransactionsV1RootDomain,
              header.forced_transactions_root,
              proof.count,
            ),
            proof.count != header.forced_transaction_count,
          }
        transition_trace.TransactionsV1RootDomain -> and {
            transition_trace.verify_root_count_proof(
              proof,
              transition_trace.TransactionsV1RootDomain,
              header.transactions_root,
              proof.count,
            ),
            proof.count != header.l2_transaction_count,
          }
        transition_trace.DepositsRootDomain -> and {
            transition_trace.verify_root_count_proof(
              proof,
              transition_trace.DepositsRootDomain,
              header.deposits_root,
              proof.count,
            ),
            proof.count != header.deposit_count,
          }
        _ -> False
      }
    EventToStepRootCountMismatch { proof } -> and {
        transition_trace.verify_root_count_proof(
          proof,
          transition_trace.EventToStepRootDomain,
          header.event_to_step_root,
          proof.count,
        ),
        proof.count != header.total_event_count,
      }
    TransitionTraceRootCountMismatch { proof } -> and {
        transition_trace.verify_root_count_proof(
          proof,
          transition_trace.TransitionTraceRootDomain,
          header.transition_trace_root,
          proof.count,
        ),
        proof.count != header.transition_step_count,
      }
  }
}

fn validate_transition_fault(
  header: ledger_state.HeaderV1,
  fault: TransitionFault,
  hub_datum: hub.Datum,
  reference_inputs: List<Input>,
) -> Bool {
  when fault is {
    TraceBoundaryFault { side, trace_proof } ->
      validate_trace_boundary(header, side, trace_proof)
    TraceLinkFault { adjacent } -> validate_trace_link(header, adjacent)
    EventToStepMismatch { trace_proof, event_to_step } ->
      validate_event_to_step_mismatch(header, trace_proof, event_to_step)
    SourceMembershipMismatch { witness } ->
      validate_source_membership_mismatch(header, witness)
    InvalidOneStepTransition { witness } ->
      validate_invalid_one_step_transition(
        header,
        hub_datum,
        reference_inputs,
        witness,
      )
    OmittedDueL1Event { witness } ->
      validate_omitted_due_l1_event(
        header,
        hub_datum,
        reference_inputs,
        witness,
      )
    DuplicateTraceEvent { left_trace, right_trace } ->
      validate_duplicate_trace_event(header, left_trace, right_trace)
    OutOfWindowSourceEvent { witness } ->
      validate_out_of_window_source_event(
        header,
        hub_datum,
        reference_inputs,
        witness,
      )
    CountFault { witness } -> validate_count_fault(header, witness)
    AcceptedTransactionTransitionMismatch { witness } ->
      validate_accepted_transaction_transition_mismatch(header, witness)
  }
}

fn validate_transition_fault_proof_envelope(
  proof: TransitionFaultProof,
  computation_thread_token_asset_name: AssetName,
) -> Bool {
  let TransitionFaultProof { challenged_header_hash, header, .. } = proof
  let header_data: Data = header
  and {
    utils.verify_hash_28(header_data, challenged_header_hash),
    bytearray.length(computation_thread_token_asset_name) == fraud_proof_catalogue.id_byte_count + 28,
    bytearray.take(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    ) == transition_trace_fraud_category_id,
    bytearray.drop(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    ) == challenged_header_hash,
  }
}

pub fn validate_control_fault_proof(
  proof: TransitionFaultProof,
  computation_thread_token_asset_name: AssetName,
) -> Bool {
  and {
    validate_transition_fault_proof_envelope(
      proof,
      computation_thread_token_asset_name,
    ),
    when proof.fault is {
      TraceBoundaryFault { side, trace_proof } ->
        validate_trace_boundary(proof.header, side, trace_proof)
      TraceLinkFault { adjacent } -> validate_trace_link(proof.header, adjacent)
      EventToStepMismatch { trace_proof, event_to_step } ->
        validate_event_to_step_mismatch(
          proof.header,
          trace_proof,
          event_to_step,
        )
      CountFault { witness } -> validate_count_fault(proof.header, witness)
      _ -> False
    },
  }
}

pub fn validate_source_fault_proof(
  proof: TransitionFaultProof,
  computation_thread_token_asset_name: AssetName,
) -> Bool {
  and {
    validate_transition_fault_proof_envelope(
      proof,
      computation_thread_token_asset_name,
    ),
    when proof.fault is {
      SourceMembershipMismatch { witness } ->
        validate_source_membership_mismatch(proof.header, witness)
      _ -> False
    },
  }
}

pub fn validate_withdrawal_fault_proof(
  proof: TransitionFaultProof,
  computation_thread_token_asset_name: AssetName,
) -> Bool {
  and {
    validate_transition_fault_proof_envelope(
      proof,
      computation_thread_token_asset_name,
    ),
    when proof.fault is {
      InvalidOneStepTransition { witness } ->
        when witness is {
          ValidWithdrawalTransition {
            trace_proof,
            event_to_step,
            source_membership,
            spent_utxo,
          } ->
            validate_valid_withdrawal_transition(
              proof.header,
              trace_proof,
              event_to_step,
              source_membership,
              spent_utxo,
            )
          InvalidWithdrawalNoOpTransition {
            trace_proof,
            event_to_step,
            source_membership,
          } ->
            validate_invalid_withdrawal_no_op_transition(
              proof.header,
              trace_proof,
              event_to_step,
              source_membership,
            )
          _ -> False
        }
      _ -> False
    },
  }
}

pub fn validate_forced_fault_proof(
  proof: TransitionFaultProof,
  computation_thread_token_asset_name: AssetName,
) -> Bool {
  and {
    validate_transition_fault_proof_envelope(
      proof,
      computation_thread_token_asset_name,
    ),
    when proof.fault is {
      InvalidOneStepTransition { witness } ->
        when witness is {
          InvalidForcedTransactionNoOpTransition {
            trace_proof,
            event_to_step,
            source_membership,
          } ->
            validate_invalid_forced_transaction_no_op_transition(
              proof.header,
              trace_proof,
              event_to_step,
              source_membership,
            )
          _ -> False
        }
      _ -> False
    },
  }
}

pub fn validate_accepted_transaction_fault_proof(
  proof: TransitionFaultProof,
  computation_thread_token_asset_name: AssetName,
) -> Bool {
  and {
    validate_transition_fault_proof_envelope(
      proof,
      computation_thread_token_asset_name,
    ),
    when proof.fault is {
      AcceptedTransactionTransitionMismatch { witness } ->
        validate_accepted_transaction_transition_mismatch(proof.header, witness)
      InvalidOneStepTransition{witness: L2TransactionTransition {
        trace_proof,
        event_to_step,
        source_membership,
        spend_inputs_preimage,
        outputs_preimage,
        spent_utxos,
        produced_utxos,
      }} ->
        validate_l2_transaction_transition(
          proof.header,
          trace_proof,
          event_to_step,
          source_membership,
          spend_inputs_preimage,
          outputs_preimage,
          spent_utxos,
          produced_utxos,
        )
      _ -> False
    },
  }
}

pub fn validate_deposit_fault_proof(
  proof: TransitionFaultProof,
  computation_thread_token_asset_name: AssetName,
  hub_datum: hub.Datum,
  reference_inputs: List<Input>,
) -> Bool {
  and {
    validate_transition_fault_proof_envelope(
      proof,
      computation_thread_token_asset_name,
    ),
    when proof.fault is {
      InvalidOneStepTransition { witness } ->
        when witness is {
          ValidDepositTransition {
            trace_proof,
            event_to_step,
            source_membership,
            event_ref_input_index,
            event_asset_name,
            projected_utxo,
          } ->
            validate_valid_deposit_transition(
              proof.header,
              hub_datum,
              reference_inputs,
              trace_proof,
              event_to_step,
              source_membership,
              event_ref_input_index,
              event_asset_name,
              projected_utxo,
            )
          _ -> False
        }
      _ -> False
    },
  }
}

pub fn validate_l1_event_fault_proof(
  proof: TransitionFaultProof,
  computation_thread_token_asset_name: AssetName,
  hub_datum: hub.Datum,
  reference_inputs: List<Input>,
) -> Bool {
  and {
    validate_transition_fault_proof_envelope(
      proof,
      computation_thread_token_asset_name,
    ),
    when proof.fault is {
      OmittedDueL1Event { witness } ->
        validate_omitted_due_l1_event(
          proof.header,
          hub_datum,
          reference_inputs,
          witness,
        )
      OutOfWindowSourceEvent { witness } ->
        validate_out_of_window_source_event(
          proof.header,
          hub_datum,
          reference_inputs,
          witness,
        )
      _ -> False
    },
  }
}

pub fn validate_duplicate_fault_proof(
  proof: TransitionFaultProof,
  computation_thread_token_asset_name: AssetName,
) -> Bool {
  and {
    validate_transition_fault_proof_envelope(
      proof,
      computation_thread_token_asset_name,
    ),
    when proof.fault is {
      DuplicateTraceEvent { left_trace, right_trace } ->
        validate_duplicate_trace_event(proof.header, left_trace, right_trace)
      _ -> False
    },
  }
}

pub fn validate_transition_fault_proof(
  proof: TransitionFaultProof,
  computation_thread_token_asset_name: AssetName,
  hub_datum: hub.Datum,
  reference_inputs: List<Input>,
) -> Bool {
  and {
    validate_transition_fault_proof_envelope(
      proof,
      computation_thread_token_asset_name,
    ),
    validate_transition_fault(
      proof.header,
      proof.fault,
      hub_datum,
      reference_inputs,
    ),
  }
}

pub fn insert_root(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: Proof,
) -> ByteArray {
  let trie = if root == env.empty_merkle_tree_root {
    mpf.empty
  } else {
    mpf.from_root(root)
  }
  mpf.root(mpf.insert(trie, key, value, proof))
}

pub fn delete_root(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: Proof,
) -> ByteArray {
  let trie = if root == env.empty_merkle_tree_root {
    mpf.empty
  } else {
    mpf.from_root(root)
  }
  mpf.root(mpf.delete(trie, key, value, proof))
}
