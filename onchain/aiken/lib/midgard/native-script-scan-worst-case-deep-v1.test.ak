//// Guards for the reachable DEEP worst-case vector of the #633 one-shot tag-0
//// structural predicate.
////
//// **There is deliberately no live `worst_case_deep_predicate` test here, and
//// the deep figure is a DERIVED row rather than a measured one.** Driving the
//// predicate over the full 5,446-container vector peaks around 12.6 GB of host
//// memory, because host cost tracks the evaluator's recursion depth while
//// execution units track node count. The whole-tree `aiken check` evaluates
//// every module in one process, so a live row here took the full run to a
//// 14.62 GB peak and aborted it with SIGABRT ("memory allocation of 336 bytes
//// failed") against the 16 GiB guardrail cap -- even with
//// `RAYON_NUM_THREADS=1`, and it would be red on any comparable CI runner.
//// With this test absent the same run is green at a 10.97 GB peak. That
//// trade was ruled in favour of a runnable tree: the deep cost is published as
//// a gate-recomputed extrapolation off the measured deep curve, and the
//// headline MEASURED worst case is its wide twin
//// (`native-script-scan-worst-case-wide-v1.test.ak`, 71.9x over the basis),
//// which is live and costs 0.6 GB.
////
//// The three tests below stay live and are cheap. They are what makes the
//// derived row honest: the derivation divides by a node count, and these pin
//// the node count, the byte length and the stack depth of the exact vector the
//// derivation is quoted about.
////
//// Reproduce the direct reading on its own -- outside the tree run, which is
//// the only place it fits -- by restoring the body recorded in
//// `scripts/native-script-scan-exec-ledger-v1.json` under `derivedRows` and
//// selecting it alone; that ledger entry carries the command, the three
//// independent direct readings taken of it, and the extrapolation cross-check.
////
//// The deep shape is the worst case, not merely a large one: at three bytes a
//// node it spends every byte of `max_reachable_payload_bytes` on a node, while
//// the wide shape spends three of them on a children-array header. Its cost is
//// about 1.07 billion memory units against a 13,200,000-unit basis -- **81x
//// over** -- which is the figure decision 0005 A3's ExUnits precondition fails
//// on, and it is over-determined: the live wide row alone already fails it.

use aiken/primitive/bytearray
use midgard/native_script_payload_vectors_v1 as vectors
use midgard/native_script_scan_v1

/// Shape guard. The derived figure is quoted about THIS shape, so every number
/// the ledger's derivation divides by is pinned here: the byte length at the
/// reachable cap, the node count, the peak stack depth, and the fact that no
/// further node fits. Shrink the vector to make the derivation cheaper and this
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

/// Discrimination guard: the derived row is quoted about a predicate that
/// refuses things, not about one that returns `True` for anything. Cheap
/// because it runs on a three-byte payload, so it stays live even though the
/// full-vector drive cannot.
test worst_case_deep_predicate_refuses_a_malformed_payload() {
  !native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.malformed_payload,
  )
}
