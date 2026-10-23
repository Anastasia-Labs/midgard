//// Shared verdict-subject substrate for non-interactive fault-proof threads.
////
//// A family first authenticates a transaction through the existing native
//// transaction carriage, or opens a forced leaf with
//// `open_subject_v1`.  From that point on this constant-size state is
//// the canonical authority for the disputed direction, source identity, and
//// (for wrongful rejection only) exact typed rejection reason.

use aiken/cbor
use aiken/primitive/bytearray
use cardano/transaction.{Input, Redeemer, ScriptPurpose}
use midgard/common/types.{Proof} as common_types
use midgard/common/utils
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/counted_membership_carriage_v1.{
  CarriedCountedMembershipV1, verify_carried_counted_membership_v1,
}
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/native_tx/compact.{
  verify_forced_tx_proof_source_v1, verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  VerifiedTxSourceView, native_tx_version_v1,
}
use midgard/ledger_state
use midgard/rejection_reason_v1.{
  ForcedTxInvalid, ForcedTxValid, OperatorVerdictV1, RejectionReasonV1,
}
use midgard/transition_trace

pub const version: Int = 1

/// The operator accepted a transaction for which the decisive family rule
/// evaluates to a fault.
pub const direction_wrongful_acceptance: Int = 0

/// The operator force-rejected a transaction for which the exact accused
/// family rule evaluates to no fault.
pub const direction_wrongful_rejection: Int = 1

/// A leaf of the counted normal transaction root. Such a leaf is an
/// acceptance verdict by definition.
pub const source_kind_accepted: Int = 0

/// A leaf of the counted forced transaction root, carrying an explicit
/// operator verdict.
pub const source_kind_forced: Int = 1

/// Canonical verdict subject shared by every direct non-interactive family.
///
/// `source_key` is empty for an accepted-root subject and the canonical CBOR
/// of the forced leaf's `TxOrderId` for a forced-root subject.
/// `rejection_reason` is present only for wrongful rejection, copied verbatim
/// from the authenticated forced leaf.
pub type VerdictSubjectV1 {
  version: Int,
  direction: Int,
  source_kind: Int,
  transaction_id: ByteArray,
  source_key: ByteArray,
  rejection_reason: Option<RejectionReasonV1>,
}

/// Runtime source, never a redeemer containing an unchecked verified view.
/// AcceptedSource is constructed only after the shared direct/published normal
/// carriage has authenticated the source. ForcedSource opens its typed leaf here.
pub type SubjectSourceV1 {
  AcceptedSource {
    header: ledger_state.HeaderV1,
    tx_inclusion: VerifiedTxSourceView,
  }
  ForcedSource {
    header: ledger_state.HeaderV1,
    membership: transition_trace.RootMembershipProof<
      ledger_state.TxOrderId,
      ledger_state.ForcedInclusionTxV1,
    >,
  }
}

pub type EventPreStateV1 {
  step_index: Int,
  pre_root: ByteArray,
  post_root: ByteArray,
}

/// One subject-opening boundary. Normal source provenance comes from the
/// already authenticated carriage, not from a prover-labelled verified value.
///
/// An accepted source's header is the state-queue node datum the carriage
/// read: `state_queue.get_block_datum_v1` decodes it with
/// `decode_header_view` (protocol version), and the carriage binds that
/// node's key, the commit-time hash of this header, to the thread name. Only
/// the accepted scalar is checked here. A forced source's header is supplied
/// by the prover, so its thread binding is checked before its leaf is opened;
/// the committed header it names was decoded at commit.
///
/// A validator whose arms already know the source kind may call
/// `open_accepted_subject_v1` or `open_forced_subject_v1` directly; each
/// returns exactly what this function returns for that kind.
pub fn open_subject_v1(
  thread_name: ByteArray,
  source: SubjectSourceV1,
) -> VerdictSubjectV1 {
  when source is {
    AcceptedSource { tx_inclusion, .. } ->
      open_accepted_subject_v1(tx_inclusion)
    ForcedSource { header, membership } -> {
      let Pair(subject, _verified) = open_forced_subject_v1(
        thread_name,
        header,
        membership,
      )
      subject
    }
  }
}

/// `open_subject_v1` on a forced source, also returning the verified view of
/// the opened leaf's own `submitted_source`. A caller that reads body fields
/// takes them from this view instead of verifying the same bytes again.
pub fn open_forced_subject_v1(
  thread_name: ByteArray,
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
) -> Pair<VerdictSubjectV1, VerifiedTxSourceView> {
  let header_data: Data = header
  // The thread name binds this header's hash, and thread init binds that
  // hash to a committed node whose commit decoded the protocol version.
  expect
    utils.serialise_and_hash_28(header_data) == bytearray.drop(
      thread_name,
      fraud_proof_catalogue.id_byte_count,
    )
  bind_forced_subject_v1(header, membership)
}

fn is_h32(bytes: ByteArray) -> Bool {
  bytearray.length(bytes) == 32
}

/// Checks the cross-field invariants of a carried verdict subject. Family
/// validators call helpers in this module rather than trusting a datum that
/// merely decodes as `VerdictSubjectV1`.
pub fn subject_is_canonical_v1(subject: VerdictSubjectV1) -> Bool {
  and {
    subject.version == version,
    is_h32(subject.transaction_id),
    when subject.source_kind is {
      0 -> {
        let VerdictSubjectV1 { direction, source_key, rejection_reason, .. } = subject
        and {
          direction == direction_wrongful_acceptance,
          source_key == #"",
          rejection_reason == None,
        }
      }
      1 -> and {
          bytearray.length(subject.source_key) > 0,
          if subject.direction == direction_wrongful_acceptance {
            subject.rejection_reason == None
          } else if subject.direction == direction_wrongful_rejection {
            when subject.rejection_reason is {
              Some(_) -> True
              None -> False
            }
          } else {
            False
          },
        }
      _ -> False
    },
  }
}

fn encode_optional_reason(reason: Option<RejectionReasonV1>) -> ByteArray {
  when reason is {
    None -> encode_definite_array_header(0)
    Some(value) ->
      encode_definite_array_header(1)
        |> bytearray.concat(cbor.serialise(value))
  }
}

/// Stable cross-language wire: definite CBOR
/// `[version, direction, source_kind, tx_id, source_key, [] | [reason-data]]`.
/// The reason is the wire-normative `RejectionReasonV1` Plutus Data value.
pub fn encode_verdict_subject_v1(subject: VerdictSubjectV1) -> ByteArray {
  expect subject_is_canonical_v1(subject)
  encode_definite_array_header(6)
    |> bytearray.concat(cbor.serialise(subject.version))
    |> bytearray.concat(cbor.serialise(subject.direction))
    |> bytearray.concat(cbor.serialise(subject.source_kind))
    |> bytearray.concat(encode_definite_bytes(subject.transaction_id))
    |> bytearray.concat(encode_definite_bytes(subject.source_key))
    |> bytearray.concat(encode_optional_reason(subject.rejection_reason))
}

/// Binds the result of the shared accepted-transaction carriage. The caller
/// has already authenticated this verified compact transaction under the
/// header's counted `transactions_root`; this seam additionally enforces the
/// leaf-is-acceptance validity scalar before constructing canonical state.
/// `open_subject_v1` on an accepted source. The carriage that produced
/// `verified` has already bound its header to the thread.
pub fn open_accepted_subject_v1(
  verified: VerifiedTxSourceView,
) -> VerdictSubjectV1 {
  expect and {
      verified.version == native_tx_version_v1,
      is_h32(verified.tx_id),
      verified.tx_compact.validity_code == Some(0),
    }
  VerdictSubjectV1 {
    version,
    direction: direction_wrongful_acceptance,
    source_kind: source_kind_accepted,
    transaction_id: verified.tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

/// Authenticates raw forced-leaf bytes against the header's counted root.
/// Typed subjects and raw admission proofs share this membership door; raw
/// callers retain the exact bytes so their own grammar predicate can inspect
/// a committed value that does not decode as a typed leaf.
pub fn open_forced_leaf_raw_v1(
  header: ledger_state.HeaderV1,
  raw_key: ByteArray,
  raw_value: ByteArray,
  phas_root: ByteArray,
  proof: Proof,
) -> ByteArray {
  expect forced_leaf_is_member_v1(header, raw_key, raw_value, phas_root, proof)
  raw_value
}

/// The domain, root, and count come from the header itself, so only the
/// counted-root consistency and the raw membership remain to be checked.
fn forced_leaf_is_member_v1(
  header: ledger_state.HeaderV1,
  raw_key: ByteArray,
  raw_value: ByteArray,
  phas_root: ByteArray,
  proof: Proof,
) -> Bool {
  and {
    transition_trace.counted_root_is_consistent(
      transition_trace.ForcedTransactionsV1RootDomain,
      header.forced_transactions_root,
      phas_root,
      header.forced_transaction_count,
    ),
    transition_trace.verify_root_membership_raw(
      phas_root,
      header.forced_transaction_count,
      raw_key,
      raw_value,
      proof,
    ),
  }
}

/// The authenticated leaf's verdict, including the exact rejection reason.
pub fn subject_verdict_v1(subject: VerdictSubjectV1) -> OperatorVerdictV1 {
  expect subject_is_canonical_v1(subject)
  when subject.rejection_reason is {
    None -> ForcedTxValid
    Some(reason) -> ForcedTxInvalid { reason }
  }
}

/// Binds the event key to the subject independently of its proof carriage.
pub fn event_key_matches_subject_v1(
  key: ledger_state.EventKey,
  subject: VerdictSubjectV1,
) -> Bool {
  expect subject_is_canonical_v1(subject)
  let VerdictSubjectV1 { source_kind, transaction_id, source_key, .. } = subject
  when key is {
    ledger_state.L2TransactionEventKey { tx_id } -> and {
        source_kind == source_kind_accepted,
        tx_id == transaction_id,
      }
    ledger_state.ForcedTransactionEventKey { tx_order_id } -> and {
        source_kind == source_kind_forced,
        cbor.serialise(tx_order_id) == source_key,
      }
    _ -> False
  }
}

/// Opens a forced leaf against the challenged header, re-verifies its exact
/// validity-free proof source and tx id, authenticates the sole verdict,
/// and fixes the direction's reason polarity. No prover-supplied reason or
/// coordinate enters the returned state. The verified view is returned
/// unchanged with the subject.
fn bind_forced_subject_v1(
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
) -> Pair<VerdictSubjectV1, VerifiedTxSourceView> {
  let leaf = membership.value
  let source_key = cbor.serialise(membership.key)
  // The verifier hashes the body bytes against `leaf.tx_id` and returns that
  // id with version 1, so neither is compared back to the leaf.
  let Pair(verified, _witness_set) = verify_forced_tx_proof_source_v1(
    leaf.tx_id,
    leaf.submitted_source.compact_cbor,
    leaf.submitted_source.witness_set_compact_cbor,
    leaf.submitted_source.field_preimage_lengths_cbor,
  )
  // The membership is opened under the header's own domain, root and count,
  // so the carried copies of those three fields are not read.
  expect
    forced_leaf_is_member_v1(
      header,
      source_key,
      cbor.serialise(leaf),
      membership.phas_root,
      membership.proof,
    )
  let Pair(direction, rejection_reason) = when leaf.verdict is {
    ForcedTxValid -> Pair(direction_wrongful_acceptance, None)
    ForcedTxInvalid { reason } ->
      Pair(direction_wrongful_rejection, Some(reason))
  }
  Pair(
    VerdictSubjectV1 {
      version,
      direction,
      source_kind: source_kind_forced,
      transaction_id: leaf.tx_id,
      source_key,
      rejection_reason,
    },
    verified,
  )
}

/// The event phase is determined by its authenticated key.
fn phase_for_event_key_v1(
  key: ledger_state.EventKey,
) -> ledger_state.TransitionPhase {
  when key is {
    ledger_state.WithdrawalEventKey { .. } -> ledger_state.Withdrawal
    ledger_state.ForcedTransactionEventKey { .. } ->
      ledger_state.ForcedTransaction
    ledger_state.L2TransactionEventKey { .. } -> ledger_state.L2Transaction
    ledger_state.DepositEventKey { .. } -> ledger_state.Deposit
  }
}

/// Exact shared event-to-step/transition opening. The caller has already bound
/// this header to its thread; no block-root or creator inference enters here.
pub fn event_pre_state_v1(
  header: ledger_state.HeaderV1,
  event_key: ledger_state.EventKey,
  event_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  step_membership: transition_trace.RootMembershipProof<
    Int,
    ledger_state.TransitionStep,
  >,
) -> EventPreStateV1 {
  let event = event_membership.value
  let step = step_membership.value
  expect and {
      transition_trace.verify_root_membership_with_bytes(
        step_membership,
        transition_trace.TransitionTraceRootDomain,
        header.transition_trace_root,
        header.transition_step_count,
        cbor.serialise(step_membership.key),
        cbor.serialise(step),
      ),
      step_membership.key == step.step_index,
      step_membership.key >= 0,
      step_membership.key < step_membership.count,
      step.schema_version == ledger_state.transition_step_schema_version_v1,
      transition_trace.verify_root_membership_with_bytes(
        event_membership,
        transition_trace.EventToStepRootDomain,
        header.event_to_step_root,
        header.total_event_count,
        cbor.serialise(event_membership.key),
        cbor.serialise(event),
      ),
      step_membership.key == event.step_index,
      event_membership.key == event_key,
      step.event_key == event_key,
      step.phase == phase_for_event_key_v1(event_key),
      step.phase == event.phase,
    }
  EventPreStateV1 {
    step_index: step.step_index,
    pre_root: step.pre_utxos_root,
    post_root: step.post_utxos_root,
  }
}

/// Physical event door. Source/header provenance was established by step01;
/// the only returned authority is this event's authenticated transition index.
pub fn subject_event_step_index_v1(
  header: ledger_state.HeaderV1,
  subject: VerdictSubjectV1,
  event_membership: CarriedCountedMembershipV1<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Int {
  let
      ledger_state.HeaderV1 {
        event_to_step_root,
        total_event_count,
        transition_step_count,
        ..
      }
    = header
  let CarriedCountedMembershipV1 { key, value, .. } = event_membership
  let ledger_state.EventToStepValue { step_index, phase } = value
  expect and {
      event_key_matches_subject_v1(key, subject),
      verify_carried_counted_membership_v1(
        event_membership,
        transition_trace.EventToStepRootDomain,
        event_to_step_root,
        total_event_count,
        cbor.serialise(key),
        cbor.serialise(value),
        reference_inputs,
        redeemers,
      ),
      phase == phase_for_event_key_v1(key),
      step_index >= 0,
      step_index < transition_step_count,
    }
  step_index
}

/// Physical transition door. `selected_step_index` must be carried from the
/// event door's authenticated output, never read from a prover's hint. That
/// door bounds it to `[0, header.transition_step_count)`, and the counted
/// membership below binds `step_membership.count` to the same header count,
/// so the selected key needs no second range check here.
pub fn subject_selected_event_pre_state_v1(
  header: ledger_state.HeaderV1,
  subject: VerdictSubjectV1,
  selected_step_index: Int,
  step_membership: CarriedCountedMembershipV1<Int, ledger_state.TransitionStep>,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> EventPreStateV1 {
  let step = step_membership.value
  expect and {
      verify_carried_counted_membership_v1(
        step_membership,
        transition_trace.TransitionTraceRootDomain,
        header.transition_trace_root,
        header.transition_step_count,
        cbor.serialise(step_membership.key),
        cbor.serialise(step),
        reference_inputs,
        redeemers,
      ),
      selected_step_index == step_membership.key,
      step_membership.key == step.step_index,
      step.schema_version == ledger_state.transition_step_schema_version_v1,
      event_key_matches_subject_v1(step.event_key, subject),
      step.phase == phase_for_event_key_v1(step.event_key),
    }
  EventPreStateV1 {
    step_index: step.step_index,
    pre_root: step.pre_utxos_root,
    post_root: step.post_utxos_root,
  }
}

/// Returns the authenticated typed reason only on a canonical wrongful-
/// rejection subject. Pattern matching its constructor and coordinates is the
/// family bind step's responsibility.
pub fn rejection_reason_of_v1(subject: VerdictSubjectV1) -> RejectionReasonV1 {
  expect subject_is_canonical_v1(subject)
  expect Some(reason) = subject.rejection_reason
  reason
}

/// Shared exact-reason/coordinate mutation guard. Comparing the canonical
/// typed Data encoding binds both constructor and every constructor field.
pub fn bind_exact_rejection_reason_v1(
  subject: VerdictSubjectV1,
  expected: RejectionReasonV1,
) -> RejectionReasonV1 {
  let actual = rejection_reason_of_v1(subject)
  expect cbor.serialise(actual) == cbor.serialise(expected)
  actual
}

/// The typed reason of a wrongful-rejection subject that `open_subject_v1`
/// returned in the same validator run. Family Step 01 binds use this instead
/// of `rejection_reason_of_v1`: the opener builds every subject canonical, so
/// `subject_is_canonical_v1` would only repeat it. An accepted source gets
/// version 1, wrongful acceptance, an empty key and no reason, and a 32-byte
/// id (`open_accepted_subject_v1`'s `is_h32`). A forced source gets version
/// 1, a non-empty CBOR key, the direction and reason from one match on the
/// leaf verdict, and an id equal to the verified blake2b-256 body hash
/// (`verify_forced_tx_compact_cbor_v1`). A subject read from a datum must
/// use the checked accessors instead.
pub fn opened_rejection_reason_v1(
  subject: VerdictSubjectV1,
) -> RejectionReasonV1 {
  expect Some(reason) = subject.rejection_reason
  reason
}

/// `bind_exact_rejection_reason_v1` for a subject `open_subject_v1` returned
/// in the same validator run; see `opened_rejection_reason_v1`.
pub fn bind_opened_rejection_reason_v1(
  subject: VerdictSubjectV1,
  expected: RejectionReasonV1,
) -> RejectionReasonV1 {
  let actual = opened_rejection_reason_v1(subject)
  expect cbor.serialise(actual) == cbor.serialise(expected)
  actual
}

/// Shared terminal polarity. `decisive_fault_holds` must be computed on-chain
/// by the family's canonical semantic rule: a wrongful acceptance closes only
/// on `True`; a wrongful rejection closes only on `False` for the exact reason
/// carried from its forced leaf.
pub fn terminal_contradiction_v1(
  subject: VerdictSubjectV1,
  decisive_fault_holds: Bool,
) -> Bool {
  expect subject_is_canonical_v1(subject)
  if subject.direction == direction_wrongful_acceptance {
    decisive_fault_holds
  } else {
    expect _reason = rejection_reason_of_v1(subject)
    !decisive_fault_holds
  }
}
