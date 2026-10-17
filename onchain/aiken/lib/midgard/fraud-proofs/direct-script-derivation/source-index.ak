//// Source index built once from authenticated input/witness passes. Every
//// priority comparison uses locations already committed by the field walk.
//// The selected source for a script hash is the first one the T224 lookup
//// visits, named by the shared source-order function: the physical frontier
//// holds inline witnesses in item order, then the reference scripts of
//// resolved outputs (spent and reference inputs) in canonical out-ref order,
//// and the lookup visits the resolved-output sources first. A duplicate
//// witness stays unselected.

use aiken/cbor
use aiken/merkle_patricia_forestry.{Proof}
use aiken/primitive/bytearray
use env
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/mpf_proof_v1 as mpf
use midgard/script_source_order_v1.{script_source_physical_index}

pub type StateV1 {
  root: ByteArray,
  count: Int,
}

pub type AdmissionV1 {
  First { proof: Proof }
  Existing { previous: SourceV1, proof: Proof }
}

pub fn initial_v1() -> StateV1 {
  StateV1 { root: env.empty_merkle_tree_root, count: 0 }
}

fn is_input_v1(source: SourceV1) -> Bool {
  source.field_index == 0 || source.field_index == 1
}

/// Two same-hash candidates form a two-entry physical frontier. `left`
/// precedes when it sits at the physical slot the lookup visits first.
pub fn priority_precedes_v1(left: SourceV1, right: SourceV1) -> Bool {
  expect and {
      bytearray.length(left.script_hash) == 28,
      left.script_hash == right.script_hash,
      left.item_index >= 0,
      right.item_index >= 0,
      left.field_index == 6 || is_input_v1(left),
      right.field_index == 6 || is_input_v1(right),
    }
  // Physical frontier of the pair: inline entries first, each class in its
  // own physical order (item order inline, canonical out-ref order resolved).
  let (left_slot, inline_count) = when
    (left.field_index == 6, right.field_index == 6)
  is {
    (True, True) ->
      if left.item_index < right.item_index {
        (0, 2)
      } else {
        (1, 2)
      }
    (True, False) -> (0, 1)
    (False, True) -> (1, 1)
    (False, False) ->
      if bytearray.compare(left.source_key, right.source_key) == Less {
        (0, 0)
      } else {
        (1, 0)
      }
  }
  left_slot == script_source_physical_index(inline_count, 2, 0)
}

pub fn admit_v1(
  state: StateV1,
  candidate: SourceV1,
  admission: AdmissionV1,
) -> StateV1 {
  expect and {
      bytearray.length(candidate.script_hash) == 28,
      candidate.item_index >= 0,
      candidate.field_index == 6 || is_input_v1(candidate),
    }
  when admission is {
    First { proof } -> {
      expect Some(root) = mpf.insert_root_paired_fold(
        state.root,
        candidate.script_hash,
        cbor.serialise(candidate),
        proof,
      )
      StateV1 { root, count: state.count + 1 }
    }
    Existing { previous, proof } -> {
      expect previous.script_hash == candidate.script_hash
      if priority_precedes_v1(candidate, previous) {
        expect Some(root) = mpf.update_root_paired_fold(
          state.root,
          candidate.script_hash,
          cbor.serialise(previous),
          cbor.serialise(candidate),
          proof,
        )
        StateV1 { root, count: state.count }
      } else {
        expect
          mpf.has(
            state.root,
            candidate.script_hash,
            cbor.serialise(previous),
            proof,
          )
        state
      }
    }
  }
}

pub fn lookup_v1(
  state: StateV1,
  script_hash: ByteArray,
  selected: Option<SourceV1>,
  proof: Proof,
) -> Bool {
  expect bytearray.length(script_hash) == 28
  when selected is {
    Some(source) ->
      source.script_hash == script_hash && mpf.has(
        state.root,
        script_hash,
        cbor.serialise(source),
        proof,
      )
    None -> mpf.does_not_have(state.root, script_hash, proof)
  }
}
