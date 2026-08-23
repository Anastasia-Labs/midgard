//// The reachable DEEP worst case for the #633 one-shot tag-0 structural
//// predicate, alone in its own module.
////
//// It is alone because `scripts/run-focused-check.mjs` puts every selector it
//// is handed into ONE `aiken check` invocation, Aiken evaluates a module's
//// selected tests in PARALLEL, and this test peaks around 12.6 GB of host
//// memory on its own: run beside its wide twin, the pair exhausts a 16 GiB
//// address space. Separated, each is measurable, and a number may only be
//// published for a test that runs -- so separated is how they live. A full
//// `aiken check` over the whole tree evaluates all three modules in one
//// process and needs `RAYON_NUM_THREADS=1` to stay under 16 GiB. The reading is
//// pinned in
//// `scripts/native-script-scan-exec-ledger-v1.json` and judged by
//// `scripts/verify-native-script-scan-exec-ledger-v1.mjs`, which measures each
//// module in its own invocation.
////
//// This is the worst case, not merely a large one: at three bytes a node the
//// deep shape spends every byte of `max_reachable_payload_bytes` on a node,
//// while the wide shape spends three of them on a children-array header. Its
//// cost is about 1.07 billion memory units against a 13,200,000-unit basis --
//// **81x over** -- which is the reading decision 0005 A3's ExUnits precondition
//// fails on.

use aiken/primitive/bytearray
use midgard/native_script_payload_vectors_v1 as vectors
use midgard/native_script_scan_v1

test worst_case_deep_predicate() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.deep_payload(vectors.max_reachable_payload_bytes),
  )
}

/// Shape guard. The published figure is a reading of THIS shape, so every
/// number the ledger's note quotes about it is pinned here: the byte length at
/// the reachable cap, the node count, the peak stack depth, and the fact that
/// no further node fits. Shrink the vector to make the row cheaper and this
/// goes red first.
test worst_case_deep_vector_is_the_reachable_maximum() {
  let payload = vectors.deep_payload(vectors.max_reachable_payload_bytes)
  and {
    vectors.max_reachable_payload_bytes == 16341,
    bytearray.length(payload) == 16341,
    vectors.deep_payload_nodes(vectors.max_reachable_payload_bytes) == 5447,
    vectors.deep_payload_stack_depth(vectors.max_reachable_payload_bytes) == 5446,
    bytearray.length(payload) + vectors.min_node_bytes > vectors.max_reachable_payload_bytes,
  }
}

/// The stack depth above is the SCAN's notion of depth -- frames open at once,
/// `NativeScriptStructureControlV1.stack_depth` -- not the tree's height read
/// off the source. Pinned on a two-container analogue of the same shape,
/// because pinning it on the 5,446-container vector would mean driving the
/// staged machine, whose per-node `blake2b_256` costs strictly more than the
/// reading this module exists to take.
test worst_case_deep_shape_opens_one_frame_per_container() {
  let payload = vectors.deep_payload(9)
  let initial = native_script_scan_v1.initial_structure_control_v1(
    0,
    bytearray.length(payload),
  )
  expect
      Some(native_script_scan_v1.NativeScriptStructureAdvanced {
        control: first,
      })
    = native_script_scan_v1.structure_token_step_v1(initial, payload, 0)
  expect
      Some(native_script_scan_v1.NativeScriptStructureAdvanced {
        control: second,
      })
    = native_script_scan_v1.structure_token_step_v1(
    first,
    payload,
    first.cursor,
  )
  and {
    vectors.deep_payload_containers(9) == 2,
    first.stack_depth == 1,
    second.stack_depth == 2,
    vectors.deep_payload_stack_depth(9) == second.stack_depth,
  }
}

/// Discrimination guard: the row is a reading of a predicate that refuses
/// things, not of one that returns `True` for anything.
test worst_case_deep_predicate_refuses_a_malformed_payload() {
  !native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.malformed_payload,
  )
}
