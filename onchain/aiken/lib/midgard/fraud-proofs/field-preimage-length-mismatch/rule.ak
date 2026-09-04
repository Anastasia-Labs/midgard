//// Decisive rule and authenticated-field reducer for
//// `FieldPreimageLengthMismatch`. This module imports no item or subject-family
//// semantic adapter.

use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/canonical_decodability/rule.{
  BodyFieldClaim, CommittedFieldClaimV1, WitnessFieldClaim,
}
use midgard/fraud_proofs/field_opening_v1.{first_witness_set_field_index}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxFieldPreimageLengthsV1, NativeTxWitnessSetCompact,
  VerifiedMidgardNativeTxCompact,
}
use midgard/native_tx_field_access_v1.{
  authenticated_committed_preimage, field_count,
  max_transaction_aggregate_field_bytes,
}

pub type LengthEvidenceV1 {
  field_index: Int,
  declared_length: Int,
  actual_length: Int,
}

fn unread_witness_set() -> NativeTxWitnessSetCompact {
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: #"",
    script_tx_wits_hash: #"",
    redeemer_tx_wits_hash: #"",
  }
}

pub fn field_preimage_length_at_v1(
  lengths: NativeTxFieldPreimageLengthsV1,
  field_index: Int,
) -> Int {
  expect field_index >= 0
  when field_index is {
    0 -> lengths.spend_inputs
    1 -> lengths.reference_inputs
    2 -> lengths.outputs
    3 -> lengths.required_observers
    4 -> lengths.required_signers
    5 -> lengths.mint
    6 -> lengths.script_witnesses
    7 -> lengths.address_witnesses
    8 -> lengths.redeemers
    _ -> fail
  }
}

/// Authenticates exactly one positional raw field preimage and derives both
/// lengths. Item grammar is deliberately not a precondition: the canonical
/// machine decides the declared-length mismatch before interpreting items.
pub fn authenticated_length_evidence_v1(
  verified: VerifiedMidgardNativeTxCompact,
  lengths_cbor: ByteArray,
  claim: CommittedFieldClaimV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> LengthEvidenceV1 {
  let lengths = decode_native_tx_field_preimage_lengths_v1(lengths_cbor)
  let Pair(field_index, preimage) = when claim is {
    BodyFieldClaim { field_index, carriage } -> {
      expect field_index < first_witness_set_field_index
      Pair(
        field_index,
        authenticated_committed_preimage(
          verified,
          unread_witness_set(),
          field_index,
          carriage,
          reference_inputs,
          certificate_policy_id,
        ),
      )
    }
    WitnessFieldClaim { field_index, witness_set, carriage } -> {
      expect field_index >= first_witness_set_field_index
      Pair(
        field_index,
        authenticated_committed_preimage(
          verified,
          witness_set,
          field_index,
          carriage,
          reference_inputs,
          certificate_policy_id,
        ),
      )
    }
  }
  let actual_length = bytearray.length(preimage)
  expect field_index < field_count
  expect actual_length <= max_transaction_aggregate_field_bytes
  LengthEvidenceV1 {
    field_index,
    declared_length: field_preimage_length_at_v1(lengths, field_index),
    actual_length,
  }
}

pub fn decisive_fault_holds_v1(
  field_index: Int,
  declared_length: Int,
  actual_length: Int,
) -> Bool {
  expect field_index >= 0
  expect field_index < field_count
  expect declared_length >= 0
  expect actual_length >= 0
  expect actual_length <= max_transaction_aggregate_field_bytes
  declared_length != actual_length
}
