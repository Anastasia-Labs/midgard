//// Worst-case ExUnits harness for the one-shot native-script structural
//// predicate (`native_script_scan_v1.payload_structure_is_canonical_v1`),
//// the precondition decision 0005 A3 attaches to wave item 9.
////
//// Every pathological payload here is built programmatically -- a source
//// literal of this size is what wedges the compiler, and the builder cost is
//// isolated by a paired `*_builder_only` test so the predicate's marginal
//// ExUnits can be read off the difference.

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

test worst_case_deep_predicate() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    deep_payload(max_reachable_payload_bytes),
  )
}

test worst_case_wide_builder_only() {
  bytearray.length(wide_payload()) == 16334
}

test worst_case_wide_predicate() {
  native_script_scan_v1.payload_structure_is_canonical_v1(wide_payload())
}

/// The ruling quotes a 32,768-byte field bound. That bound is not reachable
/// through `decode_canonical_output` (16,384-byte whole-output cap), but the
/// doubled case is measured anyway so the verdict covers it.
test worst_case_deep_unreachable_32k_builder_only() {
  bytearray.length(deep_payload(32768)) == 32766
}

test worst_case_deep_unreachable_32k_predicate() {
  native_script_scan_v1.payload_structure_is_canonical_v1(deep_payload(32768))
}

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
