//// Named purpose/redeemer pairing over the authenticated field-8 bytes.
//// The descriptor commits the actual Data bytes and actual declared units;
//// no operator trace state or substituted execution budget participates.

use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/bounded_item_v1
use midgard/canonical_cbor_scan_v1
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, redeemers_field_index,
}
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}
use midgard/fraud_proofs/native_tx/codec.{
  decode_definite_bytes_at, decode_uint_at,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_redeemer_witness_at, encode_midgard_redeemer_witness,
}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/native_tx_field_access_v1.{FieldViewV1, field_read_range}
use midgard/native_tx_machine_walk_v1 as walk

pub type RedeemerV1 {
  item_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  data_offset: Int,
  data_length: Int,
  execution_memory: Int,
  execution_steps: Int,
}

pub type StateV1 {
  cursor: cursor.CursorV1,
  coordinate: PurposeCoordinateV1,
  selected: Option<RedeemerV1>,
  complete: Bool,
}

/// Missing/present is a pointer predicate. Reading a bounded prefix avoids
/// materializing the potentially maximal Data payload or execution units.
/// Full envelope exhaustion is certified before this semantic pass starts.
pub type PointerStateV1 {
  cursor: cursor.CursorV1,
  coordinate: PurposeCoordinateV1,
  found: Bool,
  complete: Bool,
}

pub fn pointer_from_prefix_v1(prefix: ByteArray) -> Pair<Int, Int> {
  expect Some(outer) = canonical_cbor_scan_v1.head_at_v1(prefix, 0, 4)
  expect outer.value == 4
  expect Some(tag) = canonical_cbor_scan_v1.head_at_v1(
    prefix,
    outer.next_offset,
    0,
  )
  expect Some(index) = canonical_cbor_scan_v1.head_at_v1(
    prefix,
    tag.next_offset,
    0,
  )
  Pair(tag.value, index.value)
}

pub fn pointer_matches_v1(
  coordinate: PurposeCoordinateV1,
  pointer: Pair<Int, Int>,
) -> Bool {
  pointer.2nd == coordinate.index && when coordinate.kind is {
    0 -> pointer.1st == 0
    1 -> pointer.1st == 1
    2 -> pointer.1st == 3
    3 -> pointer.1st == 6
    _ -> False
  }
}

fn fold_pointers_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  budget: Int,
  coordinate: PurposeCoordinateV1,
  found: Bool,
) -> Pair<Bool, walk.FieldWalkCheckpointV1> {
  if budget == 0 || walk.walk_is_complete(position) {
    Pair(found, position)
  } else {
    let Pair(Pair(offset, length), advanced) = walk.walk_next_extent(
      view,
      position,
    )
    // A shortest-form array-4 tag is one byte; each unsigned pointer part is
    // at most nine bytes. The remaining payload is irrelevant to this leaf.
    let prefix = field_read_range(
      view,
      offset,
      if length < 19 {
        length
      } else {
        19
      },
    )
    let pointer = pointer_from_prefix_v1(prefix)
    fold_pointers_v1(
      view,
      advanced,
      budget - 1,
      coordinate,
      found || pointer_matches_v1(coordinate, pointer),
    )
  }
}

pub fn advance_pointers_v1(
  state: PointerStateV1,
  field_opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<PointerStateV1, ByteArray> {
  expect and {
      !state.complete,
      budget > 0,
    }
  let Pair(view, position) = cursor.resume_at_v1(
    state.cursor,
    redeemers_field_index,
    field_opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let Pair(found, advanced) = fold_pointers_v1(
    view,
    position,
    budget,
    state.coordinate,
    state.found,
  )
  Pair(
    PointerStateV1 {
      ..state,
      cursor: cursor.after_v1(state.cursor, advanced),
      found,
      complete: walk.walk_is_complete(advanced),
    },
    walk.encode_field_walk_checkpoint(advanced),
  )
}

fn tag_matches_v1(
  coordinate: PurposeCoordinateV1,
  witness: native_types.MidgardRedeemerWitness,
) -> Bool {
  witness.index == coordinate.index && when coordinate.kind is {
    0 -> witness.purpose == native_types.SpendRedeemer
    1 -> witness.purpose == native_types.MintRedeemer
    2 -> witness.purpose == native_types.RewardRedeemer
    3 -> witness.purpose == native_types.ReceiveRedeemer
    _ -> False
  }
}

pub fn from_item_v1(
  coordinate: PurposeCoordinateV1,
  item_index: Int,
  item: ByteArray,
) -> Option<RedeemerV1> {
  let Pair(offset, witness) = decode_midgard_redeemer_witness_at(item, 0)
  expect and {
      offset == bytearray.length(item),
      encode_midgard_redeemer_witness(witness) == item,
    }
  if tag_matches_v1(coordinate, witness) {
    let Pair(at_index, _) = decode_uint_at(item, 1)
    let Pair(at_data, _) = decode_uint_at(item, at_index)
    let Pair(after_data, actual_data) = decode_definite_bytes_at(item, at_data)
    Some(
      RedeemerV1 {
        item_index,
        total_length: bytearray.length(item),
        item_commitment: bounded_item_v1.from_bytes(
          redeemers_field_index,
          item_index,
          item,
        ),
        data_offset: after_data - bytearray.length(actual_data),
        data_length: bytearray.length(actual_data),
        execution_memory: witness.execution_units.memory,
        execution_steps: witness.execution_units.steps,
      },
    )
  } else {
    None
  }
}

pub fn advance_v1(
  state: StateV1,
  field_opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<StateV1, ByteArray> {
  expect and {
      !state.complete,
      state.cursor.field_index == redeemers_field_index,
      budget > 0,
    }
  let Pair(view, position) = cursor.resume_v1(
    state.cursor,
    field_opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let Pair(selected, advanced) = walk.walk_fold(
    view,
    position,
    budget,
    state.selected,
    fn(selected, index, item) {
      when from_item_v1(state.coordinate, index, item) is {
        None -> selected
        Some(actual) -> {
          expect selected == None
          Some(actual)
        }
      }
    },
  )
  Pair(
    StateV1 {
      ..state,
      cursor: cursor.after_v1(state.cursor, advanced),
      selected,
      complete: walk.walk_is_complete(advanced),
    },
    walk.encode_field_walk_checkpoint(advanced),
  )
}
