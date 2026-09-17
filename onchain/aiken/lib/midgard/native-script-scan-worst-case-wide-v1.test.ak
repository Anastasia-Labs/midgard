//// The reachable WIDE worst case for the #633 one-shot tag-0 structural
//// predicate, and **the headline MEASURED reading the verdict rests on**:
//// 16,340 bytes, 5,446 nodes, 949,469,452 memory units against a
//// 13,200,000-unit basis -- **71.9x over**. It fails decision 0005 A3's ExUnits
//// precondition on its own, with nothing else in the transaction.
////
//// It carries that role because it is affordable to drive: about 0.6 GB of
//// host memory, against roughly 12.6 GB for its deep twin. Host cost tracks
//// the evaluator's recursion depth while execution units track node count, and
//// this vector puts 5,446 nodes at a stack depth of 1, where the deep shape
//// puts 5,447 at a depth of 5,446. A live deep test aborts the whole-tree
//// `aiken check` at the 16 GiB guardrail cap, so the deep figure (81.6x) is
//// gate-derived from the cost curve instead; this one is read off a running
//// test. The two together are also what establishes that cost tracks node
//// count and not depth, so nothing about the verdict turns on which shape an
//// operator picks.
////
//// It lives alone in its own module because `scripts/run-focused-check.mjs`
//// puts every selector it is handed into ONE `aiken check` invocation and
//// Aiken evaluates a module's selected tests in PARALLEL.
////
//// The reading is pinned in `scripts/native-script-scan-exec-ledger-v1.json`
//// and judged by `scripts/verify-native-script-scan-exec-ledger-v1.mjs`.

use aiken/primitive/bytearray
use midgard/native_script_payload_vectors_v1 as vectors
use midgard/native_script_scan_v1

test worst_case_wide_predicate() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.wide_payload(),
  )
}

/// Shape guard, and the repair of a harness defect the adversarial review
/// found: the wide vector used to be a hand-written 5,443 leaves, seven bytes
/// short of the budget, so it was not the wide maximum at all.
/// `max_wide_leaves` is derived now, and this pins the derivation --
/// `82 01` plus a three-byte children-array header plus three bytes a leaf,
/// filling `max_reachable_payload_bytes` to within one byte, with no room for
/// another node.
test worst_case_wide_vector_is_the_reachable_maximum() {
  let payload = vectors.wide_payload()
  and {
    vectors.max_wide_leaves == 5445,
    bytearray.length(payload) == 16340,
    vectors.wide_payload_nodes(vectors.max_wide_leaves) == 5446,
    bytearray.length(vectors.wide_payload_n(vectors.max_wide_leaves + 1)) > vectors.max_reachable_payload_bytes,
    vectors.max_reachable_payload_bytes - bytearray.length(payload) < vectors.min_node_bytes,
  }
}

/// The wide shape's depth is 1, not 2. The tree's HEIGHT is two -- container
/// over leaves -- but the scan counts frames open at once
/// (`NativeScriptStructureControlV1.stack_depth`), and only the container ever
/// has one: a leaf completes in the token stage without opening anything. The
/// harness said "depth 2" before the adversarial review; this pins the scan's
/// own notion instead, on a two-leaf analogue of the same shape.
test worst_case_wide_shape_never_opens_a_second_frame() {
  let payload = vectors.wide_payload_n(2)
  let initial = native_script_scan_v1.initial_structure_control_v1(
    0,
    bytearray.length(payload),
  )
  expect
      Some(native_script_scan_v1.NativeScriptStructureAdvanced {
        control: container,
      })
    = native_script_scan_v1.structure_token_step_v1(initial, payload, 0)
  expect
      Some(native_script_scan_v1.NativeScriptStructureAdvanced {
        control: first_leaf,
      })
    = native_script_scan_v1.structure_token_step_v1(
    container,
    payload,
    container.cursor,
  )
  and {
    container.stack_depth == 1,
    first_leaf.stack_depth == 1,
    first_leaf.stage == native_script_scan_v1.structure_stage_frame,
  }
}

/// Discrimination guard: the row is a reading of a predicate that refuses
/// things, not of one that returns `True` for anything.
test worst_case_wide_predicate_refuses_a_malformed_payload() {
  !native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.malformed_payload,
  )
}
