//// Shared verdict-subject substrate for non-interactive fault-proof threads.
////
//// A family first authenticates a transaction through the existing native
//// transaction carriage, or opens a forced leaf with
//// `bind_forced_subject_v1`.  From that point on this constant-size state is
//// the canonical authority for the disputed direction, source identity, and
//// (for wrongful rejection only) exact typed rejection reason.

use aiken/cbor
use aiken/primitive/bytearray
use midgard/common/utils
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/native_tx/types.{
  VerifiedMidgardNativeTxCompact, native_tx_version_v1,
}
use midgard/ledger_state
use midgard/rejection_reason_v1.{
  ForcedTxInvalid, ForcedTxValid, RejectionReasonV1,
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
      0 -> and {
          subject.direction == direction_wrongful_acceptance,
          subject.source_key == #"",
          subject.rejection_reason == None,
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
pub fn bind_accepted_subject_v1(
  verified: VerifiedMidgardNativeTxCompact,
) -> VerdictSubjectV1 {
  expect and {
      verified.version == native_tx_version_v1,
      is_h32(verified.tx_id),
      verified.tx_compact.validity_code == 0,
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

/// Opens a forced leaf against the challenged header, re-verifies its exact
/// native proof source and tx id, checks verdict/embedded-validity coherence,
/// and fixes the direction's reason polarity. No prover-supplied reason or
/// coordinate enters the returned state.
pub fn bind_forced_subject_v1(
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
  direction: Int,
) -> VerdictSubjectV1 {
  let leaf = membership.value
  let Pair(verified, _witness_set) = verify_native_tx_proof_source_v1(
    leaf.tx_id,
    leaf.source.compact_cbor,
    leaf.source.witness_set_compact_cbor,
    leaf.source.field_preimage_lengths_cbor,
  )
  expect and {
      header.protocol_version == ledger_state.protocol_version_v1,
      transition_trace.verify_root_membership_with_bytes(
        membership,
        transition_trace.ForcedTransactionsV1RootDomain,
        header.forced_transactions_root,
        header.forced_transaction_count,
        cbor.serialise(membership.key),
        cbor.serialise(leaf),
      ),
      verified.version == native_tx_version_v1,
      verified.tx_id == leaf.tx_id,
    }
  let rejection_reason = when leaf.verdict is {
    ForcedTxValid -> {
      expect direction == direction_wrongful_acceptance
      expect verified.tx_compact.validity_code == 0
      None
    }
    ForcedTxInvalid { reason } -> {
      expect direction == direction_wrongful_rejection
      expect verified.tx_compact.validity_code == 1
      Some(reason)
    }
  }
  VerdictSubjectV1 {
    version,
    direction,
    source_kind: source_kind_forced,
    transaction_id: leaf.tx_id,
    source_key: cbor.serialise(membership.key),
    rejection_reason,
  }
}

/// Production entry point for a forced subject. In addition to opening the
/// leaf, this binds the supplied header to the computation-thread NFT's
/// category-independent 28-byte header-hash tail. The generic thread
/// continuation has already authenticated the token itself.
pub fn bind_forced_subject_to_thread_v1(
  computation_thread_token_asset_name: ByteArray,
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
  direction: Int,
) -> VerdictSubjectV1 {
  let header_data: Data = header
  expect
    utils.serialise_and_hash_28(header_data) == bytearray.drop(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    )
  bind_forced_subject_v1(header, membership, direction)
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
