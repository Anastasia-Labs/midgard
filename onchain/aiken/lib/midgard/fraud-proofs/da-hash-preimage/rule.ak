//// ## `da-hash-preimage` — normative rule (GOAL_SPEC.md Q44)
////
//// **Rule.** Every leaf of a committed block's `transactions_root` is a
//// `(key, value)` pair in which the key is the canonical native-V1
//// transaction id *of that value*: the block producer commits
//// `(native_tx_id_v1(body_cbor(value)), value)` and nothing else
//// (`technical-spec/1-ledger-state/6-transaction.tex:175`,
//// `docs/fault-proofs/coverage-matrix.md:174`).
////
//// **Violation `da-hash-preimage`.** A committed leaf whose key is *not* the
//// hash of its own value's body preimage. Such a leaf breaks hash/preimage
//// correspondence: no other fault-proof family can ever open it, because
//// every native family runs `verify_native_tx_compact_cbor_v1`, which
//// requires `native_tx_id_for_version(version, body_cbor) == key`
//// (`native-tx/compact.ak:459`). The block therefore hides an
//// undisputable transaction, which is why the catalogue records this row's
//// severity as *provability* (`docs/fault-proofs/catalogue-status.md:177`).
////
//// ### Why the check needs no decoder
////
//// The canonical compact encoding is fixed-framed at both ends
//// (`encode_native_tx_compact_for_version`, `native-tx/compact.ak:287-303`):
////
//// ```text
//// 0x84 || serialise(version) || body_cbor || 0x58 0x20 || wsh32 || validity
//// \___________  ____________/             \_____________  ______________/
////             \/                                        \/
////     2 bytes (version <= 23)              35 bytes (validity_code <= 1)
//// ```
////
//// so the body preimage of an honest leaf is exactly
//// `slice(value, 2, len(value) - 37)`. The verifier re-hashes that slice and
//// compares it with the committed key. For an honest leaf the slice *is* the
//// encoder's `body_cbor`, so the derived id always equals the key and the
//// challenge can never finalize — soundness does not depend on the leaf being
//// decodable. For a faulty leaf the same total computation convicts, whether
//// the value is a well-formed transaction committed under a foreign key or
//// arbitrary bytes.
////
//// Both framing constants are pinned against the real encoder by the
//// selectors in `validators/fraud-proofs/da-hash-preimage/step-01.ak`.

use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{slice_len}
use midgard/fraud_proofs/native_tx/compact.{native_tx_id_for_version}
use midgard/fraud_proofs/native_tx/types.{native_tx_version_v1}

/// `0x84` array header plus the single-byte canonical version.
pub const compact_v1_head_byte_count: Int = 2

/// Definite 32-byte witness-set hash (`0x58 0x20` + 32) plus the single-byte
/// canonical validity code (`expect_validity_code` bounds it to `0..=1`).
pub const compact_v1_tail_byte_count: Int = 35

/// Shortest byte length a canonical compact leaf can have: the framing alone,
/// with an empty body.
pub const compact_v1_frame_byte_count: Int =
  compact_v1_head_byte_count + compact_v1_tail_byte_count

/// The body-preimage slice an honest committed leaf hashes to its key.
/// Total: leaves shorter than the canonical frame clamp to the empty slice and
/// are convicted by [`committed_leaf_is_underframed_v1`](#committed_leaf_is_underframed_v1).
pub fn committed_leaf_body_cbor_v1(committed_leaf_value: ByteArray) -> ByteArray {
  let body_byte_count = bytearray.length(committed_leaf_value) - compact_v1_frame_byte_count
  if body_byte_count <= 0 {
    #""
  } else {
    slice_len(committed_leaf_value, compact_v1_head_byte_count, body_byte_count)
  }
}

/// Canonical native-V1 transaction id the committed leaf value actually
/// commits to. Never fails.
pub fn derive_committed_leaf_tx_id_v1(
  committed_leaf_value: ByteArray,
) -> ByteArray {
  native_tx_id_for_version(
    native_tx_version_v1,
    committed_leaf_body_cbor_v1(committed_leaf_value),
  )
}

/// A leaf too short to carry the canonical frame cannot be a transaction at
/// all, so its key cannot be a hash-preimage commitment of it.
pub fn committed_leaf_is_underframed_v1(committed_leaf_byte_count: Int) -> Bool {
  committed_leaf_byte_count < compact_v1_frame_byte_count
}

/// The adjudicated violation predicate, stated over the three values the
/// first step commits into the computation thread.
pub fn is_da_hash_preimage_violation_v1(
  committed_tx_id: ByteArray,
  derived_tx_id: ByteArray,
  committed_leaf_byte_count: Int,
) -> Bool {
  if committed_leaf_is_underframed_v1(committed_leaf_byte_count) {
    True
  } else {
    derived_tx_id != committed_tx_id
  }
}
