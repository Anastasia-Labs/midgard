//// Worst-case ExUnits harness for the one-shot native-script structural
//// predicate (`native_script_scan_v1.payload_structure_is_canonical_v1`),
//// the precondition decision 0005 A3 attaches to wave item 9.
////
//// Every pathological payload here is built programmatically -- a source
//// literal of this size is what wedges the compiler, and the builder cost is
//// isolated by a paired `*_builder_only` test so the predicate's marginal
//// ExUnits can be read off the difference.
////
//// Reach, measured rather than argued: wiring this predicate into
//// `ledger_output_v1.parse_script_ref`'s tag-0 arm and diffing a scratch
//// blueprint against c98cd0bf moves three validators (six rows, each a
//// `.main.spend` / `.main.else` pair) out of 444 --
//// `fraud_proofs/transition_trace/accepted_transaction_v1`,
//// `fraud_proofs/transition_trace/deposit_v1`, and
//// `fraud_proofs/validation_trace/cek_context_step_semantic_v1`. The first two
//// are the pair decision 0005 A3 predicted. The third is NOT named in the
//// ruling and reaches the arm through
//// `validation_machine_v1` -> `script_context_v1.tx_out_data_v1` ->
//// `decode_canonical_output`; unlike `deposit_v1` (whose projected output is
//// built with `script_ref: None`, `transition-trace/proof.ak:901`, so its hash
//// moves and its behaviour does not) that is a live runtime reach, on a
//// validator that already spends its budget on a CEK step. It makes the
//// budget verdict below strictly worse, not better.

use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec
use midgard/native_script_scan_v1
use midgard/native_script_v1

/// `[1, [X]]` -- one `all` container carrying exactly one child.
const nest_step: ByteArray = #"820181"

/// `[4, 0]` -- an `after 0` timelock leaf, the smallest canonical node.
const timelock_leaf: ByteArray = #"820400"

/// Whole-output cap in `ledger_output_v1.decode_canonical_output` is 16,384
/// bytes; the enclosing map, address, value and versioned-script wrapper cost
/// 43 bytes, so this is the largest tag-0 payload the arm can ever see.
pub const max_reachable_payload_bytes: Int = 16341

/// Doubling repeat: O(log n) concatenations, so the builder never dominates
/// the measurement and the compiler never sees a large literal.
fn repeat_bytes(chunk: ByteArray, times: Int) -> ByteArray {
  if times <= 0 {
    #""
  } else if times == 1 {
    chunk
  } else {
    let half = repeat_bytes(chunk, times / 2)
    let doubled = bytearray.concat(half, half)
    if times % 2 == 0 {
      doubled
    } else {
      bytearray.concat(doubled, chunk)
    }
  }
}

/// Maximal nesting depth inside `byte_budget`: `depth` singleton containers
/// wrapping one timelock leaf.
fn deep_payload(byte_budget: Int) -> ByteArray {
  let depth = ( byte_budget - 3 ) / 3
  repeat_bytes(nest_step, depth) |> bytearray.concat(timelock_leaf)
}

/// Maximal node count inside `byte_budget` at depth 2: one `all` container
/// over `n` timelock leaves. `n` is fixed at 5,443 so the definite array
/// header is the three-byte `0x99` form.
fn wide_payload() -> ByteArray {
  #"8201"
    |> bytearray.concat(#"991543")
    |> bytearray.concat(repeat_bytes(timelock_leaf, 5443))
}

fn wide_payload_n(n: Int) -> ByteArray {
  #"8201"
    |> bytearray.concat(codec.encode_definite_array_header(n))
    |> bytearray.concat(repeat_bytes(timelock_leaf, n))
}

test curve_wide_00064() {
  native_script_scan_v1.payload_structure_is_canonical_v1(wide_payload_n(64))
}

test curve_wide_00256() {
  native_script_scan_v1.payload_structure_is_canonical_v1(wide_payload_n(256))
}

test curve_wide_01024() {
  native_script_scan_v1.payload_structure_is_canonical_v1(wide_payload_n(1024))
}

test curve_wide_04096() {
  native_script_scan_v1.payload_structure_is_canonical_v1(wide_payload_n(4096))
}

test curve_deep_00064() {
  native_script_scan_v1.payload_structure_is_canonical_v1(deep_payload(195))
}

test curve_deep_00256() {
  native_script_scan_v1.payload_structure_is_canonical_v1(deep_payload(771))
}

test curve_deep_01024() {
  native_script_scan_v1.payload_structure_is_canonical_v1(deep_payload(3075))
}

test curve_deep_04096() {
  native_script_scan_v1.payload_structure_is_canonical_v1(deep_payload(12291))
}

test worst_case_deep_builder_only() {
  bytearray.length(deep_payload(max_reachable_payload_bytes)) == 16341
}

test worst_case_wide_builder_only() {
  bytearray.length(wide_payload()) == 16334
}

// The two reachable worst-case PREDICATE readings are recorded here rather
// than run in the suite: each is measurable on its own, but the pair
// evaluated alongside the curve above exhausts a 16 GiB address-space cap, so
// leaving them in would make `aiken check` unrunnable under the memory
// guardrail. Reproduce either one on its own with
//
//   aiken check --env testnet \
//     -m 'midgard/native_script_scan_worst_case_v1.{worst_case_deep_predicate}'
//
// after restoring the body below. Readings, fork aiken v1.1.23+2a78108:
//
//   deep, `deep_payload(max_reachable_payload_bytes)`
//     16,341 bytes, 5,447 nodes, depth 5,446    1.07 B mem / 359.50 B cpu
//   wide, `wide_payload()`
//     16,334 bytes, 5,444 nodes, depth 2      944.76 M mem / 313.05 B cpu
//
// test worst_case_deep_predicate() {
//   native_script_scan_v1.payload_structure_is_canonical_v1(
//     deep_payload(max_reachable_payload_bytes),
//   )
// }
//
// test worst_case_wide_predicate() {
//   native_script_scan_v1.payload_structure_is_canonical_v1(wide_payload())
// }

// The ruling quotes a 32,768-byte field bound, which is NOT reachable through
// `decode_canonical_output` (16,384-byte whole-output cap). It was measured
// anyway and is recorded here rather than run, because it is past the tooling
// frontier: `deep_payload(32768)` -- 32,766 bytes, 10,922 nodes at depth
// 10,921 -- aborts the evaluator with "memory allocation failed" under a
// 16 GiB address-space cap, while its paired builder-only reading (98.39 K mem
// / 24.60 M cpu) completes. The largest measurable case is therefore the
// reachable one below, and the verdict does not depend on the unreachable one:
// the reachable case is already 95x over the memory ceiling.

// --- comparison arms (harness only) ---

test cmp_inspect_wide_00064() {
  native_script_v1.inspect_native_script(wide_payload_n(64), 0, 0, []) != None
}

test cmp_inspect_wide_01024() {
  native_script_v1.inspect_native_script(wide_payload_n(1024), 0, 0, []) != None
}

test cmp_inspect_deep_01024() {
  native_script_v1.inspect_native_script(deep_payload(3075), 0, 0, []) != None
}
