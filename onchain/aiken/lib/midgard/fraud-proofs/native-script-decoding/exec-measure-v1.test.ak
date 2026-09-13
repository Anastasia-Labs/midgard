//// Execution-cost drivers for the decoding-fault engine's budgeted fold
//// (#635, #633) — the tests
//// `scripts/native-script-decoding-engine-exec-ledger-v1.json` pins.
////
//// Each measured row folds a programmatically built worst-shape payload
//// (`midgard/native_script_payload_vectors_v1`, the same builders the scan
//// ledger's vectors use) through `engine.budgeted_scan_v1` to the exact
//// terminal, at two node counts per shape so the ledger's realized
//// mem-per-node rate is a slope rather than a whole-test reading. The
//// `fixture_only` twins price the harness (vector build, wrapper, frame
//// chain) so the slope's residual harness share is visible, and the design's
//// §6 rate comparison subtracts nothing it did not measure.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_script_decoding/engine
use midgard/fraud_proofs/native_tx/codec
use midgard/native_script_payload_vectors_v1 as vectors
use midgard/native_script_scan_v1.{NativeScriptFrameV1}

/// The canonical tag-0 item wrapper `[0, payload-bytes]` around a payload.
fn wrapped_item(payload: ByteArray) -> ByteArray {
  bytearray.concat(#"8200", codec.encode_definite_bytes(payload))
}

/// The frame consumption chain of `deep_payload`: one singleton `all` frame
/// per container, popped innermost-first, each frame's tail the stack
/// commitment beneath it.
fn deep_frame_chain(containers: Int) -> List<NativeScriptFrameV1> {
  do_deep_frame_chain(containers, #"", [])
}

fn do_deep_frame_chain(
  remaining: Int,
  root: ByteArray,
  consumption: List<NativeScriptFrameV1>,
) -> List<NativeScriptFrameV1> {
  if remaining <= 0 {
    consumption
  } else {
    let frame = NativeScriptFrameV1 {
      tail: root,
      kind: native_script_scan_v1.all_node,
      child_count: 1,
      remaining: 1,
      valid_count: 0,
      required: 0,
    }
    do_deep_frame_chain(
      remaining - 1,
      native_script_scan_v1.hash_frame_v1(frame),
      [frame, ..consumption],
    )
  }
}

/// The frame consumption chain of `wide_payload_n(n)`: the single `all` frame
/// re-committed after each child, `remaining` counting down from `n` to 1.
fn wide_frame_chain(n: Int) -> List<NativeScriptFrameV1> {
  do_wide_frame_chain(n, n, [])
}

fn do_wide_frame_chain(
  n: Int,
  remaining: Int,
  reversed: List<NativeScriptFrameV1>,
) -> List<NativeScriptFrameV1> {
  if remaining <= 0 {
    list.reverse(reversed)
  } else {
    do_wide_frame_chain(
      n,
      remaining - 1,
      [
        NativeScriptFrameV1 {
          tail: #"",
          kind: native_script_scan_v1.all_node,
          child_count: n,
          remaining,
          valid_count: 0,
          required: 0,
        },
        ..reversed
      ],
    )
  }
}

/// Bind the wrapped payload and fold it to the exact terminal in one call —
/// the work a maximally-budgeted step-03 `Scan` transaction performs, minus
/// the thread envelope (which the step-module rows in the same ledger price).
fn scan_to_terminal(
  payload: ByteArray,
  frames: List<NativeScriptFrameV1>,
  expected_nodes: Int,
) -> Bool {
  let item = wrapped_item(payload)
  expect engine.MachineBoundV1 { control } = engine.bind_machine_v1(
    item,
    bytearray.length(item),
  )
  expect engine.ScanAdvancedV1 { control: terminal } = engine.budgeted_scan_v1(
    control,
    Some(engine.ScanWindowV1 { bytes: item, start_offset: 0 }),
    frames,
    2 * expected_nodes + 2,
  )
  and {
    native_script_scan_v1.structure_terminal_is_exact_v1(terminal),
    terminal.node_count == expected_nodes,
  }
}

/// The harness twin: everything but the fold.
fn fixture_only(
  payload: ByteArray,
  frames: List<NativeScriptFrameV1>,
  expected_nodes: Int,
) -> Bool {
  let item = wrapped_item(payload)
  and {
    bytearray.length(item) > bytearray.length(payload),
    list.length(frames) == expected_nodes - 1,
  }
}

// Deep shape: byte budgets chosen so `deep_payload_nodes` lands exactly on
// the row's node count (3 bytes per node). Node counts are small because the
// rows must sit WITHIN the §3.3 basis: a first measurement put the realized
// fold at ≈745K mem/node deep / ≈656K wide — ≈3.5× the one-shot rates the
// design's §6 derived its throughput from — so 65-node vectors (48.4M /
// 42.7M mem) are not pinnable as `within` rows at all. That multiple is the
// ledger's headline finding, not a defect in these rows.
const deep_small_budget: Int = 27

const deep_large_budget: Int = 51

// Wide shape: `wide_payload_n(n)` presents n + 1 nodes.
const wide_small_children: Int = 8

const wide_large_children: Int = 16

test decoding_measure_deep_9_nodes() {
  let containers = vectors.deep_payload_containers(deep_small_budget)
  expect vectors.deep_payload_nodes(deep_small_budget) == 9
  scan_to_terminal(
    vectors.deep_payload(deep_small_budget),
    deep_frame_chain(containers),
    9,
  )
}

test decoding_measure_deep_9_nodes_fixture_only() {
  let containers = vectors.deep_payload_containers(deep_small_budget)
  fixture_only(
    vectors.deep_payload(deep_small_budget),
    deep_frame_chain(containers),
    9,
  )
}

test decoding_measure_deep_17_nodes() {
  let containers = vectors.deep_payload_containers(deep_large_budget)
  expect vectors.deep_payload_nodes(deep_large_budget) == 17
  scan_to_terminal(
    vectors.deep_payload(deep_large_budget),
    deep_frame_chain(containers),
    17,
  )
}

test decoding_measure_deep_17_nodes_fixture_only() {
  let containers = vectors.deep_payload_containers(deep_large_budget)
  fixture_only(
    vectors.deep_payload(deep_large_budget),
    deep_frame_chain(containers),
    17,
  )
}

test decoding_measure_wide_9_nodes() {
  expect vectors.wide_payload_nodes(wide_small_children) == 9
  scan_to_terminal(
    vectors.wide_payload_n(wide_small_children),
    wide_frame_chain(wide_small_children),
    9,
  )
}

test decoding_measure_wide_9_nodes_fixture_only() {
  fixture_only(
    vectors.wide_payload_n(wide_small_children),
    wide_frame_chain(wide_small_children),
    9,
  )
}

test decoding_measure_wide_17_nodes() {
  expect vectors.wide_payload_nodes(wide_large_children) == 17
  scan_to_terminal(
    vectors.wide_payload_n(wide_large_children),
    wide_frame_chain(wide_large_children),
    17,
  )
}

test decoding_measure_wide_17_nodes_fixture_only() {
  fixture_only(
    vectors.wide_payload_n(wide_large_children),
    wide_frame_chain(wide_large_children),
    17,
  )
}

/// Neutralisation: a fold that reached the terminal over garbage would make
/// every row above a price of nothing — the same driver must refuse the
/// vectors module's canonical malformed payload.
test decoding_measure_harness_refuses_a_malformed_payload() {
  let item = wrapped_item(vectors.malformed_payload)
  expect engine.MachineBoundV1 { control } = engine.bind_machine_v1(
    item,
    bytearray.length(item),
  )
  engine.budgeted_scan_v1(
    control,
    Some(engine.ScanWindowV1 { bytes: item, start_offset: 0 }),
    [],
    8,
  ) == engine.ScanRefusedV1 { refusal_class: engine.refusal_class_malformed }
}

/// Neutralisation: the node counts the ledger's slope divides by are the
/// counts the vectors actually present.
test decoding_measure_vectors_carry_the_advertised_node_counts() {
  and {
    vectors.deep_payload_nodes(deep_small_budget) == 9,
    vectors.deep_payload_nodes(deep_large_budget) == 17,
    vectors.wide_payload_nodes(wide_small_children) == 9,
    vectors.wide_payload_nodes(wide_large_children) == 17,
    bytearray.length(vectors.deep_payload(deep_large_budget)) == 51,
    bytearray.length(vectors.wide_payload_n(wide_large_children)) == 51,
  }
}
