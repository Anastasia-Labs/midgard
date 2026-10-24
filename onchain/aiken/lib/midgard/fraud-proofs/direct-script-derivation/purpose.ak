//// Purpose facts derived from authenticated transaction fields and event state.

use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Redeemer, ScriptPurpose}
use midgard/fraud_proofs/common.{MembershipCarriage, verify_membership_carried}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/field_opening_v1 as opening
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}
use midgard/fraud_proofs/native_tx/codec.{
  byte_at, decode_definite_bytes_at, expect_byte,
}
use midgard/fraud_proofs/native_tx/components.{decode_midgard_address_bytes}
use midgard/fraud_proofs/native_tx/types.{PubKeyCredential,
  ScriptCredential} as native_types
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_v1
use midgard/native_tx_field_access_v1.{
  FieldViewV1, field_item_at, field_item_count,
}
use midgard/native_tx_machine_walk_v1.{spend_input_at}

pub const spend: Int = 0

pub const mint: Int = 1

pub const observe: Int = 2

pub const receive: Int = 3

/// Opening a raw item proves its ledger credential, not its redeemer ordinal.
/// The physical spend pass derives that ordinal by exhaustive spends-only rank.
pub type SpendCandidateV1 {
  script_hash: ByteArray,
  subject: ByteArray,
}

/// Preserve key credentials for claimed-purpose impossibility. The subject
/// remains the exact authenticated raw outref until a complete rank pass.
pub type SpendCredentialV1 {
  script_hash: Option<ByteArray>,
  subject: ByteArray,
}

pub fn coordinate_is_valid_v1(coordinate: PurposeCoordinateV1) -> Bool {
  and {
    coordinate.kind >= spend,
    coordinate.kind <= receive,
    coordinate.index >= 0,
  }
}

pub fn open_spend_credential_v1(
  bound: BoundV1,
  index: Int,
  field_opening: opening.FieldOpeningV1,
  descriptor_cbor: ByteArray,
  membership: MembershipCarriage,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  certificate_policy_id: PolicyId,
) -> SpendCredentialV1 {
  spend_credential_in_view_v1(
    bound,
    opening.opened_field_view(
      field_opening,
      cursor.anchor_v1(bound, opening.spend_inputs_field_index),
      opening.spend_inputs_field_index,
      reference_inputs,
      certificate_policy_id,
    ),
    index,
    descriptor_cbor,
    membership,
    reference_inputs,
    redeemers,
  )
}

/// `view` must be the authenticated spend-inputs field of `bound`'s
/// transaction; every caller opens it against `cursor.anchor_v1(bound, 0)`.
pub fn spend_credential_in_view_v1(
  bound: BoundV1,
  view: FieldViewV1,
  index: Int,
  descriptor_cbor: ByteArray,
  membership: MembershipCarriage,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> SpendCredentialV1 {
  // This index addresses the authenticated raw field. It becomes a pointer
  // only after the direct spend pass proves its rank among spent outrefs.
  let count = field_item_count(view)
  expect and {
      index >= 0,
      index < count,
    }
  let input = spend_input_at(view, index)
  let out_ref = OutputReference {
    transaction_id: input.tx_id,
    output_index: input.output_index,
  }
  expect
    verify_membership_carried(
      membership,
      bound.event_pre_root,
      ledger_outref_key(out_ref),
      descriptor_cbor,
      reference_inputs,
      redeemers,
    )
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  expect descriptor.output_index == input.output_index
  expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
    descriptor.address,
  )
  SpendCredentialV1 {
    script_hash: when address.payment_credential is {
      ScriptCredential(script_hash) -> Some(script_hash)
      PubKeyCredential(_) -> None
    },
    subject: field_item_at(view, index),
  }
}

pub fn open_spend_v1(
  bound: BoundV1,
  index: Int,
  field_opening: opening.FieldOpeningV1,
  descriptor_cbor: ByteArray,
  membership: MembershipCarriage,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  certificate_policy_id: PolicyId,
) -> Option<SpendCandidateV1> {
  let opened = open_spend_credential_v1(
    bound,
    index,
    field_opening,
    descriptor_cbor,
    membership,
    reference_inputs,
    redeemers,
    certificate_policy_id,
  )
  when opened.script_hash is {
    Some(script_hash) ->
      Some(SpendCandidateV1 { script_hash, subject: opened.subject })
    None -> None
  }
}

pub fn open_observe_v1(
  bound: BoundV1,
  index: Int,
  field_opening: opening.FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> PurposeV1 {
  let view = opening.opened_field_view(
    field_opening,
    cursor.anchor_v1(bound, opening.required_observers_field_index),
    opening.required_observers_field_index,
    reference_inputs,
    certificate_policy_id,
  )
  let script_hash = field_item_at(view, index)
  PurposeV1 {
    coordinate: PurposeCoordinateV1 { kind: observe, index },
    script_hash,
    subject: script_hash,
  }
}

/// Only the canonical output's address prefix is needed. Value, datum and
/// script-ref payloads are not materialized by a receive-purpose pass.
/// Earlier output-format faults remain governed by shared first-fault ordering.
pub fn protected_script_credential_v1(prefix: ByteArray) -> Option<ByteArray> {
  let tag = byte_at(prefix, 0)
  expect and {
      tag >= 162,
      tag <= 164,
    }
  let offset = expect_byte(prefix, 1, 0)
  expect byte_at(prefix, offset) == 88
  let Pair(_, bytes) = decode_definite_bytes_at(prefix, offset)
  let address = decode_midgard_address_bytes(bytes)
  if address.protected {
    when address.payment_credential is {
      ScriptCredential(script_hash) -> Some(script_hash)
      _ -> None
    }
  } else {
    None
  }
}
