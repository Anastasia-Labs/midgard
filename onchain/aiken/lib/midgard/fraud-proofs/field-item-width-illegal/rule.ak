//// Source-bound item legality, first-fault and in-family domain refutation.

use aiken/cbor
use aiken/primitive/bytearray
use midgard/fixed_item_legality_v1
use midgard/fraud_proofs/first_fault_v1
use midgard/fraud_proofs/native_tx/types.{TxCompactView}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_field_access_v1.{
  FieldViewV1, field_fixed_encoded_item_at, field_item_at, field_item_count,
}
use midgard/rejection_reason_v1.{FieldItemWidthIllegal}

pub const max_serialized_output_preimage_bytes: Int =
  fixed_item_legality_v1.max_serialized_output_preimage_bytes

pub type BoundCoordinateV1 {
  subject: substrate.VerdictSubjectV1,
  field_index: Int,
  item_index: Int,
  compact: TxCompactView,
}

pub type ItemObservationV1 {
  ItemOutOfDomain
  ItemLegality { item_is_legal: Bool }
}

pub type AuthenticatedItemV1 {
  subject: substrate.VerdictSubjectV1,
  field_index: Int,
  item_index: Int,
  observation: ItemObservationV1,
}

pub fn coordinate_is_supported_v1(field_index: Int, item_index: Int) -> Bool {
  and {
    item_index >= 0,
    or {
      fixed_item_legality_v1.is_fixed_field_v1(field_index),
      field_index == 2,
      field_index == 5,
    },
  }
}

fn names_observed_item(state: AuthenticatedItemV1) -> Bool {
  state.subject.rejection_reason == Some(
    FieldItemWidthIllegal {
      field_index: state.field_index,
      item_index: state.item_index,
    },
  )
}

/// An observed fault coordinate may differ from the leaf's claimed coordinate.
/// Only an exact named FIWI coordinate may use the no-field domain branch.
/// `compact` comes from the step-01 authenticated accepted/forced source.
/// `subject` is the value `open_subject_v1` returned in this Step 01 run, so it
/// is canonical by construction (see `substrate.opened_rejection_reason_v1`).
pub fn bind_coordinate_v1(
  subject: substrate.VerdictSubjectV1,
  field_index: Int,
  item_index: Int,
  compact: TxCompactView,
) -> BoundCoordinateV1 {
  expect bytearray.length(compact.witness_set_hash) == 32
  expect
    if coordinate_is_supported_v1(field_index, item_index) {
      True
    } else {
      and {
        subject.direction == substrate.direction_wrongful_rejection,
        subject.rejection_reason == Some(
          FieldItemWidthIllegal { field_index, item_index },
        ),
      }
    }
  BoundCoordinateV1 { subject, field_index, item_index, compact }
}

/// Unsupported field or negative item is statically outside this arm's
/// coordinate domain. No proof or synthetic empty-field sentinel is consumed.
pub fn authenticate_static_domain_v1(
  bound: BoundCoordinateV1,
) -> AuthenticatedItemV1 {
  expect !coordinate_is_supported_v1(bound.field_index, bound.item_index)
  AuthenticatedItemV1 {
    subject: bound.subject,
    field_index: bound.field_index,
    item_index: bound.item_index,
    observation: ItemOutOfDomain,
  }
}

/// `view` is the exact whole field opened under the source's tx-id and, for
/// field 7, authenticated compact witness hash. Its count is authenticated by
/// the same complete envelope walk as CanonicalDecode, also under Certified.
pub fn authenticate_item_legality_v1(
  bound: BoundCoordinateV1,
  view: FieldViewV1,
) -> AuthenticatedItemV1 {
  expect coordinate_is_supported_v1(bound.field_index, bound.item_index)
  let observation = if bound.item_index >= field_item_count(view) {
    ItemOutOfDomain
  } else {
    let legal = if fixed_item_legality_v1.is_fixed_field_v1(bound.field_index) {
      let previous = if bound.item_index == 0 {
        None
      } else {
        Some(field_fixed_encoded_item_at(view, bound.item_index - 1))
      }
      fixed_item_legality_v1.fixed_item_is_legal_v1(
        bound.field_index,
        previous,
        field_fixed_encoded_item_at(view, bound.item_index),
      )
    } else {
      fixed_item_legality_v1.item_width_is_legal_v1(
        bound.field_index,
        bytearray.length(field_item_at(view, bound.item_index)),
      )
    }
    ItemLegality { item_is_legal: legal }
  }
  AuthenticatedItemV1 {
    subject: bound.subject,
    field_index: bound.field_index,
    item_index: bound.item_index,
    observation,
  }
}

pub fn terminal_contradiction_v1(state: AuthenticatedItemV1) -> Bool {
  expect substrate.subject_is_canonical_v1(state.subject)
  when state.observation is {
    ItemOutOfDomain -> and {
        state.subject.direction == substrate.direction_wrongful_rejection,
        names_observed_item(state),
      }
    ItemLegality { item_is_legal: True } -> and {
        state.subject.direction == substrate.direction_wrongful_rejection,
        names_observed_item(state),
      }
    ItemLegality { item_is_legal: False } ->
      !first_fault_v1.leaf_admits_fault_v1(
        state.subject,
        None,
        first_fault_v1.CanonicalFault(
          first_fault_v1.CanonicalItem {
            field_index: state.field_index,
            item_index: state.item_index,
          },
        ),
        first_fault_v1.NotRequired,
      )
  }
}

pub fn encode_bound_coordinate_v1(bound: BoundCoordinateV1) -> ByteArray {
  cbor.serialise(bound)
}

pub fn encode_authenticated_item_v1(state: AuthenticatedItemV1) -> ByteArray {
  cbor.serialise(state)
}
