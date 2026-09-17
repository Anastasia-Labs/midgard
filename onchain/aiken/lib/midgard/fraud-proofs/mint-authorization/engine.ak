//// Shared engine for the mint-authorization fault-proof thread (W-C14,
//// pre-registration; reserved emulator category id `0000001b`).
////
//// Two pieces live here, both machine twins rather than fresh semantics:
////
////   1. **The field-5 item reader.** `policy_id_of_mint_item_v1` re-walks
////      one committed mint policy item under exactly the §5.6 grammar the
////      machine's `mint_fold_chunk_window` / `script_sources_begin_mint_policy`
////      folds enforce — 28-byte policy id, non-empty asset map, canonical
////      ascending asset-name keys, non-zero quantities, no trailing bytes.
////      The claimed policy id is therefore always READ off the committed
////      item, never asserted by the prover; a zero-net entry or a malformed
////      item is unrepresentable here because it is unrepresentable there.
////
////   2. **The native-script evaluator.** `evaluate_native_script_v1` drives
////      the frozen `native_script_scan_v1` primitives (`token_at_v1`,
////      `frame_for_token_v1`, `apply_child_v1`, `empty_container_result_v1`)
////      with an explicit in-step frame stack over the full script bytes, and
////      applies the machine's own leaf predicates verbatim
////      (`validation-machine-v1.ak`, PhaseANativeScripts):
////
////        * signature (tag 0): valid iff the 28-byte key hash is a member of
////          the transaction's signer set — for an operator-ACCEPTED
////          transaction that set is exactly `blake2b_224` of every field-7
////          verification key (an invalid signature rejects the whole
////          transaction, so acceptance certifies every witness verifies);
////        * after (tag 4): `validity_interval_start >= 0 &&
////          validity_interval_start >= slot`;
////        * before (tag 5): `validity_interval_end >= 0 &&
////          validity_interval_end <= slot`;
////        * containers via `apply_child_v1` (all: every child, any: at least
////          one, at_least: `valid_count >= required`), empty containers via
////          `empty_container_result_v1` (all → True, any → False, at_least →
////          `required == 0`);
////        * node and depth limits are the scan module's
////          `max_native_script_nodes` / `max_native_script_depth` (16384 /
////          16384) — **not** `native-script-v1.ak`'s 16/32, which is not the
////          machine's evaluation semantics;
////        * the terminal requires the cursor to consume the payload exactly,
////          and an empty payload is malformed (the frozen machine's
////          `InvalidReferenceScript` precedent).
////
////      The verdict is total: `ScriptEvaluatedV1 { satisfied }` when the
////      machine would have evaluated the script, and a classed refusal
////      otherwise. **Only `satisfied: False` convicts** — a malformed or
////      limit-breaking script is the native-script-decoding family's fault
////      class, not this family's, and the step arms refuse it.

use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{decode_canonical_int_at, expect_byte}
use midgard/fraud_proofs/native_tx/preimages.{
  canonical_bytes_key_precedes, decode_canonical_bytes_at,
  decode_canonical_map_header_at,
}
use midgard/native_script_scan_v1 as scan

// ## Direction domain

/// Direction A: no script source of the claimed policy hash exists among the
/// committed transaction's script sources (inline field-6 witnesses and
/// resolved reference-input reference scripts).
pub const direction_script_absent: Int = 0

/// Direction B: the policy's native script is pinned by its versioned hash
/// and evaluates to False against the committed signer set and validity
/// interval.
pub const direction_script_unsatisfied: Int = 1

// ## Field-5 policy item reader (§5.6 twin)

/// Reads the policy id out of one committed field-5 item, enforcing the full
/// §5.6 grammar on the way through — the same rules
/// `preimages.decode_midgard_tx_mint_preimage_cbor` walks per item, applied
/// to a single door-read item. Aborts on any deviation, so a policy id this
/// function returns names a canonical, non-empty, all-non-zero mint entry.
pub fn policy_id_of_mint_item_v1(item_cbor: ByteArray) -> ByteArray {
  let offset = expect_byte(item_cbor, 0, 130)
  let Pair(offset, policy_id) = decode_canonical_bytes_at(item_cbor, offset)
  expect bytearray.length(policy_id) == 28
  let Pair(offset, asset_count) = decode_canonical_map_header_at(
    item_cbor,
    offset,
  )
  expect asset_count > 0
  let offset = check_mint_assets_at(item_cbor, offset, asset_count, None)
  expect offset == bytearray.length(item_cbor)
  policy_id
}

fn check_mint_assets_at(
  item_cbor: ByteArray,
  offset: Int,
  remaining: Int,
  previous_asset_name: Option<ByteArray>,
) -> Int {
  if remaining <= 0 {
    offset
  } else {
    let Pair(offset, asset_name) = decode_canonical_bytes_at(item_cbor, offset)
    expect bytearray.length(asset_name) <= 32
    expect
      when previous_asset_name is {
        None -> True
        Some(previous) -> canonical_bytes_key_precedes(previous, asset_name)
      }
    let Pair(offset, quantity) = decode_canonical_int_at(item_cbor, offset)
    expect quantity != 0
    check_mint_assets_at(item_cbor, offset, remaining - 1, Some(asset_name))
  }
}

// ## Machine-twin native-script evaluator

/// The total verdict over one native script payload.
pub type NativeScriptVerdictV1 {
  /// The machine would have evaluated this script, to `satisfied`.
  ScriptEvaluatedV1 { satisfied: Bool }
  /// The scan primitives refuse the bytes — the decoding family's class 0.
  ScriptMalformedV1
  /// The machine's node budget (16384) is exceeded — decoding class 1.
  ScriptNodeLimitV1
  /// The machine's depth budget (16384) is exceeded — decoding class 2.
  ScriptDepthLimitV1
}

/// Evaluates a full native-script payload (the bytes inside the versioned
/// wrapper, NOT the wrapper) under the machine's semantics. Total: never
/// aborts on prover-supplied bytes.
///
/// `signer_hashes` must be the `blake2b_224` of every field-7 verification
/// key of the disputed transaction, read through the anchored §8.8 door.
pub fn evaluate_native_script_v1(
  payload: ByteArray,
  signer_hashes: List<ByteArray>,
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> NativeScriptVerdictV1 {
  if bytearray.length(payload) == 0 {
    // The frozen machine's `InvalidReferenceScript` precedent: a tag-0
    // script with an empty payload is malformed at bind.
    ScriptMalformedV1
  } else {
    eval_token(
      payload,
      signer_hashes,
      validity_interval_start,
      validity_interval_end,
      0,
      [],
      0,
      0,
    )
  }
}

/// Read the token at `cursor` and dispatch: containers with children push a
/// frame; leaves and empty containers produce a child verdict and unwind.
fn eval_token(
  payload: ByteArray,
  signer_hashes: List<ByteArray>,
  validity_interval_start: Int,
  validity_interval_end: Int,
  cursor: Int,
  stack: List<scan.NativeScriptFrameV1>,
  depth: Int,
  node_count: Int,
) -> NativeScriptVerdictV1 {
  when scan.token_at_v1(payload, cursor, cursor) is {
    None -> ScriptMalformedV1
    Some(token) -> {
      let next_node_count = node_count + 1
      if next_node_count > scan.max_native_script_nodes {
        ScriptNodeLimitV1
      } else if token.next_offset > bytearray.length(payload) {
        // Mirrors the machine's per-token `next_offset <= item_length`.
        ScriptMalformedV1
      } else if and {
        token.kind >= scan.all_node,
        token.kind <= scan.at_least_node,
        token.child_count > 0,
      } {
        let next_depth = depth + 1
        if next_depth > scan.max_native_script_depth {
          ScriptDepthLimitV1
        } else {
          when scan.frame_for_token_v1(token, #"") is {
            None -> ScriptMalformedV1
            Some(frame) ->
              eval_token(
                payload,
                signer_hashes,
                validity_interval_start,
                validity_interval_end,
                token.next_offset,
                [frame, ..stack],
                next_depth,
                next_node_count,
              )
          }
        }
      } else {
        // Leaf or empty container: the machine's exact predicates.
        let m_valid = if token.kind == scan.signature_node {
          Some(list_has(signer_hashes, token.key_hash))
        } else if token.kind == scan.after_node {
          Some(and {
              validity_interval_start >= 0,
              validity_interval_start >= token.slot,
            })
        } else if token.kind == scan.before_node {
          Some(and {
              validity_interval_end >= 0,
              validity_interval_end <= token.slot,
            })
        } else {
          scan.empty_container_result_v1(token)
        }
        when m_valid is {
          None -> ScriptMalformedV1
          Some(valid) ->
            unwind(
              payload,
              signer_hashes,
              validity_interval_start,
              validity_interval_end,
              token.next_offset,
              stack,
              depth,
              next_node_count,
              valid,
            )
        }
      }
    }
  }
}

/// Pop completed frames, feeding each child verdict upward, until a frame is
/// still pending (read its next child) or the stack empties (terminal).
fn unwind(
  payload: ByteArray,
  signer_hashes: List<ByteArray>,
  validity_interval_start: Int,
  validity_interval_end: Int,
  cursor: Int,
  stack: List<scan.NativeScriptFrameV1>,
  depth: Int,
  node_count: Int,
  valid: Bool,
) -> NativeScriptVerdictV1 {
  when stack is {
    [] ->
      // Terminal: the payload must be consumed exactly, as the machine's
      // finalize requires.
      if cursor == bytearray.length(payload) {
        ScriptEvaluatedV1 { satisfied: valid }
      } else {
        ScriptMalformedV1
      }
    [frame, ..rest] ->
      when scan.apply_child_v1(frame, valid) is {
        None -> ScriptMalformedV1
        Some(scan.NativeFramePending { frame: pending }) ->
          eval_token(
            payload,
            signer_hashes,
            validity_interval_start,
            validity_interval_end,
            cursor,
            [pending, ..rest],
            depth,
            node_count,
          )
        Some(scan.NativeFrameComplete { valid: completed, .. }) ->
          unwind(
            payload,
            signer_hashes,
            validity_interval_start,
            validity_interval_end,
            cursor,
            rest,
            depth - 1,
            node_count,
            completed,
          )
      }
  }
}

fn list_has(items: List<ByteArray>, target: ByteArray) -> Bool {
  when items is {
    [] -> False
    [head, ..rest] ->
      if head == target {
        True
      } else {
        list_has(rest, target)
      }
  }
}
