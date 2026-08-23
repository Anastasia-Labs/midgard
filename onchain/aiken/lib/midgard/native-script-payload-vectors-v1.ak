//// Programmatic native-script payload vectors for the #633 tag-0 structural
//// predicate's harness. Harness only: nothing here is reachable from a
//// validator, and nothing here belongs in one.
////
//// It is a module rather than a block of helpers inside one test file because
//// three test modules share it. The two reachable worst cases have to live in
//// separate modules from each other and from the cost curve, so that
//// `scripts/run-focused-check.mjs` -- which puts every selector it is given
//// into ONE `aiken check` invocation -- can take a reading from one of them
//// without evaluating the other in the same process. Aiken evaluates a
//// module's selected tests in PARALLEL, and the deep worst case alone peaks
//// around 12.6 GB of host memory, so the two in one invocation exhaust a
//// 16 GiB address space; separated, each is measurable.
////
//// Every payload is built by doubling concatenation, never written down: a
//// 16 KiB source literal is what wedges the compiler, which is guardrail 3 of
//// the session's OOM rules.

use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec
use midgard/ledger_output_v1

/// `[1, [X]]` -- one `all` container carrying exactly one child.
pub const nest_step: ByteArray = #"820181"

/// `[4, 0]` -- an `after 0` timelock leaf, the smallest canonical node.
pub const timelock_leaf: ByteArray = #"820400"

/// Three bytes: the `82 04 00` above. No node the scan admits is smaller, so
/// this is what divides a byte budget into a node budget.
pub const min_node_bytes: Int = 3

/// The CBOR the tag-0 arm's payload sits inside, and therefore the difference
/// between the whole-output cap and the largest payload `parse_script_ref` can
/// ever be handed. Derived rather than asserted, at the minimum width of every
/// part:
///
/// | bytes | part |
/// |------:|------|
/// | 1 | `a3`, the three-key output map (address, value, script reference) |
/// | 1 | `00`, the address key |
/// | 31 | `58 1d` + a 29-byte key-hash address, the narrowest §6.2 address |
/// | 1 | `01`, the value key |
/// | 3 | `82 00 a0`, a coin-only value with no assets |
/// | 1 | `03`, the script-reference key |
/// | 1 | `82`, the `[tag, bytes]` reference pair |
/// | 1 | `00`, the native-script tag |
/// | 3 | `59 xx xx`, the byte-string header at any payload length above 255 |
///
/// A datum key would only make this larger, so 43 is a floor and
/// `max_reachable_payload_bytes` below is a ceiling.
pub const tag0_payload_envelope_bytes: Int = 43

/// The largest tag-0 payload reachable through `decode_canonical_output`, and
/// therefore the byte budget every worst case here is built to. It is derived
/// from `ledger_output_v1.max_output_canonical_cbor_bytes` rather than written
/// down, so that moving the whole-output cap moves the worst cases with it --
/// and moves the shape guards in
/// `native-script-scan-worst-case-{deep,wide}-v1.test.ak`, which pin the
/// resulting byte lengths and node counts as literals and go red when it does.
///
/// Note that this is NOT the 32,768-byte per-field bound decision 0005 A3
/// quotes: `parse_script_ref` is private to `ledger_output_v1` and every route
/// to it passes the 16,384-byte whole-output cap first.
pub const max_reachable_payload_bytes: Int =
  ledger_output_v1.max_output_canonical_cbor_bytes - tag0_payload_envelope_bytes

/// `82 01` -- the `[1, …]` `ScriptAll` prefix in front of the children array.
pub const wide_prefix_bytes: Int = 2

/// `99 xx xx` -- the definite array header at any child count in 256..65,535,
/// which covers every cardinality the wide shape reaches inside the budget.
pub const wide_children_header_bytes: Int = 3

/// The child count that fills `max_reachable_payload_bytes` on the wide shape.
/// Derived, not written down: the previous hand-written 5,443 was seven bytes
/// short of the budget and therefore not the wide worst case at all.
pub const max_wide_leaves: Int =
  (
    max_reachable_payload_bytes - wide_prefix_bytes - wide_children_header_bytes
  ) / min_node_bytes

/// Doubling repeat: O(log n) concatenations, so the builder never dominates
/// the measurement and the compiler never sees a large literal.
pub fn repeat_bytes(chunk: ByteArray, times: Int) -> ByteArray {
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

/// Maximal nesting inside `byte_budget`: singleton containers wrapping one
/// timelock leaf. Every node costs exactly three bytes, so this shape is the
/// node-count maximum as well as the depth maximum.
pub fn deep_payload(byte_budget: Int) -> ByteArray {
  repeat_bytes(nest_step, deep_payload_containers(byte_budget))
    |> bytearray.concat(timelock_leaf)
}

/// The container count `deep_payload` builds at `byte_budget`.
pub fn deep_payload_containers(byte_budget: Int) -> Int {
  ( byte_budget - min_node_bytes ) / min_node_bytes
}

/// Nodes `deep_payload(byte_budget)` presents to the scan: every container plus
/// the one leaf.
pub fn deep_payload_nodes(byte_budget: Int) -> Int {
  deep_payload_containers(byte_budget) + 1
}

/// The scan's `stack_depth` at its peak on `deep_payload(byte_budget)`: one
/// frame per container, all open at once when the leaf is read.
pub fn deep_payload_stack_depth(byte_budget: Int) -> Int {
  deep_payload_containers(byte_budget)
}

/// Maximal node count at the scan's minimum stack depth: one `all` container
/// over `n` timelock leaves.
pub fn wide_payload_n(n: Int) -> ByteArray {
  #"8201"
    |> bytearray.concat(codec.encode_definite_array_header(n))
    |> bytearray.concat(repeat_bytes(timelock_leaf, n))
}

/// Nodes `wide_payload_n(n)` presents to the scan: the container plus its
/// children.
pub fn wide_payload_nodes(n: Int) -> Int {
  n + 1
}

/// The wide shape at the reachable byte budget. `max_wide_leaves` leaves plus
/// the container is one node FEWER than `deep_payload` reaches in the same
/// budget, because the wide shape spends three bytes on a children-array header
/// that the deep shape spends on another node; the deep case is therefore the
/// worst case, and this one is what says by how much.
pub fn wide_payload() -> ByteArray {
  wide_payload_n(max_wide_leaves)
}

/// A payload the scan refuses on syntax: tag 7 is neither a signature, a
/// container, nor a timelock, so `token_at_v1` cannot make a token of it.
pub const malformed_payload: ByteArray = #"820700"

/// One well-formed leaf followed by a byte nothing consumes. The scan's
/// finalize is exact-consumption, so this is a refusal rather than a prefix
/// match.
pub fn payload_with_trailing_byte() -> ByteArray {
  bytearray.concat(timelock_leaf, #"00")
}

/// An `all` container whose children array declares two children and is
/// followed by one. The scan runs out of payload with a frame still open.
pub fn payload_with_truncated_child_list() -> ByteArray {
  #"820182"
    |> bytearray.concat(timelock_leaf)
}
