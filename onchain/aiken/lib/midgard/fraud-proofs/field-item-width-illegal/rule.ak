//// Narrow semantic engine for `fieldItemWidthIllegal`.
////
//// This is the direct twin of
//// `validation-machine-v1.transaction_field_item_encoded_length`'s `None`
//// arms.  The selected bytes must already have been authenticated through the
//// native field door; this module decides only whether their payload width is
//// illegal for the named field.

use aiken/cbor
use aiken/primitive/bytearray
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{FieldItemWidthIllegal}

pub const outputs_field_index: Int = 2

pub const mint_field_index: Int = 5

pub const max_serialized_output_preimage_bytes: Int = 16384

pub type BoundCoordinateV1 {
  subject: substrate.VerdictSubjectV1,
  field_index: Int,
  item_index: Int,
}

pub type AuthenticatedWidthV1 {
  subject: substrate.VerdictSubjectV1,
  field_index: Int,
  item_index: Int,
  item_width: Int,
}

pub fn coordinate_is_supported_v1(field_index: Int, item_index: Int) -> Bool {
  and {
    item_index >= 0,
    or {
      field_index == outputs_field_index,
      field_index == mint_field_index,
    },
  }
}

/// Bind the family's exact typed forced reason and coordinate.  On the
/// accepted direction there is no operator-supplied reason, but the same
/// coordinate is still carried canonically to the field-opening step.
pub fn bind_coordinate_v1(
  subject: substrate.VerdictSubjectV1,
  field_index: Int,
  item_index: Int,
) -> BoundCoordinateV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect coordinate_is_supported_v1(field_index, item_index)
  expect _bound_reason = if subject.direction == substrate.direction_wrongful_rejection {
    let expected = FieldItemWidthIllegal { field_index, item_index }
    Some(substrate.bind_exact_rejection_reason_v1(subject, expected))
  } else {
    None
  }
  BoundCoordinateV1 { subject, field_index, item_index }
}

/// Derive the state that crosses from the authenticating step to the terminal
/// step. `item` is the exact payload returned by `field_item_at`; no item bytes
/// cross the boundary.
pub fn authenticate_item_width_v1(
  bound: BoundCoordinateV1,
  item: ByteArray,
) -> AuthenticatedWidthV1 {
  AuthenticatedWidthV1 {
    subject: bound.subject,
    field_index: bound.field_index,
    item_index: bound.item_index,
    item_width: bytearray.length(item),
  }
}

/// Exact V1 predicate. Field 2's output item is illegal only above the
/// serialized-output cap. Field 5's mint-policy item is illegal only when
/// empty. Other fields are outside this reason family and are refused rather
/// than interpreted using an unrelated adapter.
pub fn item_width_is_illegal_v1(field_index: Int, item_width: Int) -> Bool {
  expect item_width >= 0
  if field_index == outputs_field_index {
    item_width > max_serialized_output_preimage_bytes
  } else {
    expect field_index == mint_field_index
    item_width == 0
  }
}

pub fn terminal_contradiction_v1(state: AuthenticatedWidthV1) -> Bool {
  expect coordinate_is_supported_v1(state.field_index, state.item_index)
  substrate.terminal_contradiction_v1(
    state.subject,
    item_width_is_illegal_v1(state.field_index, state.item_width),
  )
}

/// Stable Plutus-Data ABI used for the TypeScript parity vectors.
pub fn encode_bound_coordinate_v1(bound: BoundCoordinateV1) -> ByteArray {
  cbor.serialise(bound)
}

pub fn encode_authenticated_width_v1(state: AuthenticatedWidthV1) -> ByteArray {
  cbor.serialise(state)
}
