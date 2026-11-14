//// How the field grammar certificate validator computes a certificate —
//// the transition `validators/field-grammar-certificate.ak` checks its
//// outputs against. Consumers read certificates through
//// `midgard/field_grammar_certificate_v1` and never need this module.
////
//// A certificate is built by walking the field's §5.1 envelope with the
//// grammar-certification machine of `midgard/native_tx_machine_walk_v1`, one
//// batch per transaction. Each batch is split at the waypoint grid, and the
//// wrapper offset of every item `k · field_grammar_waypoint_interval` is
//// appended at the moment the walk reaches it, so a certificate's waypoints
//// do not depend on how its walk was batched.

use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use midgard/field_grammar_certificate_v1.{
  Complete, FieldGrammarCertificateV1, InProgress, field_grammar_waypoint_bytes,
  field_grammar_waypoint_interval,
}
use midgard/native_tx_field_access_v1.{FieldCarriageV1, FieldViewV1}
use midgard/native_tx_machine_walk_v1.{
  FieldGrammarCheckpointV1, certify_field_grammar_batch,
  field_grammar_checkpoint_hash, field_grammar_is_complete,
  field_grammar_next_item_index, field_grammar_next_offset,
}

/// Mint redeemer. `Open` is Constr 0 and `Retire` Constr 1.
pub type FieldGrammarCertificateMintRedeemerV1 {
  /// Opens a certificate and runs its first batch. The transaction and field
  /// are named by the new certificate's own datum (`tx_id`, `field_index`).
  Open {
    /// 0 for a normal transaction, 1 for a forced one.
    source_kind: Int,
    /// The L2 transaction's compact CBOR; its tx-id is re-derived and must be
    /// the datum's `tx_id`.
    compact_cbor: ByteArray,
    /// Its witness-set compact CBOR. Fields 6–8 require it to hash to the
    /// compact structure's `witness_set_hash`.
    witness_set_compact_cbor: ByteArray,
    /// How the field's preimage reaches this transaction.
    carriage: FieldCarriageV1,
    /// Items to certify in this transaction.
    batch: Int,
    /// Index of the output carrying the new certificate.
    output_index: Int,
  }
  /// Burn. The spend handler checks the owner; this branch refuses to mint.
  Retire
}

/// Spend redeemer. `Advance` is Constr 0 and `Reclaim` Constr 1.
pub type FieldGrammarCertificateSpendRedeemerV1 {
  /// Runs the next batch of an `InProgress` certificate. Anyone may advance.
  Advance {
    source_kind: Int,
    compact_cbor: ByteArray,
    witness_set_compact_cbor: ByteArray,
    /// The grammar checkpoint whose digest the spent datum holds.
    checkpoint: ByteArray,
    carriage: FieldCarriageV1,
    batch: Int,
    /// Index of the output carrying the advanced certificate.
    output_index: Int,
  }
  /// The owner takes the deposit back and the token is burnt.
  Reclaim
}

/// Walks up to `budget` items from `checkpoint`, appending each waypoint the
/// walk reaches before it walks that item.
pub fn certify_field_grammar_with_waypoints(
  view: FieldViewV1,
  checkpoint: FieldGrammarCheckpointV1,
  waypoints: ByteArray,
  budget: Int,
) -> Pair<FieldGrammarCheckpointV1, ByteArray> {
  if or {
    budget <= 0,
    field_grammar_is_complete(checkpoint),
  } {
    Pair(checkpoint, waypoints)
  } else {
    let within = field_grammar_next_item_index(checkpoint) % field_grammar_waypoint_interval
    let waypoints = if within == 0 {
      bytearray.concat(
        waypoints,
        bytearray.from_int_big_endian(
          field_grammar_next_offset(checkpoint),
          field_grammar_waypoint_bytes,
        ),
      )
    } else {
      waypoints
    }
    let segment = field_grammar_waypoint_interval - within
    let step = if budget < segment {
      budget
    } else {
      segment
    }
    certify_field_grammar_with_waypoints(
      view,
      certify_field_grammar_batch(view, checkpoint, step),
      waypoints,
      budget - step,
    )
  }
}

/// The certificate after one batch of `batch` items from `checkpoint`.
///
/// Fails unless `waypoints` holds exactly the entries for the items before
/// the checkpoint, and unless the batch either covers a whole waypoint
/// interval or completes the field — so every transaction certifies at least
/// `min(field_grammar_waypoint_interval, remaining)` items. Reaching the
/// field's last byte at the declared count yields `Complete`; anything short
/// of it yields `InProgress` holding the digest of the checkpoint reached.
pub fn field_grammar_certificate_after_batch(
  owner: VerificationKeyHash,
  tx_id: ByteArray,
  field_index: Int,
  field_hash: ByteArray,
  total_length: Int,
  view: FieldViewV1,
  checkpoint: FieldGrammarCheckpointV1,
  waypoints: ByteArray,
  batch: Int,
) -> FieldGrammarCertificateV1 {
  let before = field_grammar_next_item_index(checkpoint)
  expect
    bytearray.length(waypoints) == (
      before + field_grammar_waypoint_interval - 1
    ) / field_grammar_waypoint_interval * field_grammar_waypoint_bytes
  let Pair(reached, waypoints) = certify_field_grammar_with_waypoints(
    view,
    checkpoint,
    waypoints,
    batch,
  )
  if field_grammar_is_complete(reached) {
    Complete {
      owner,
      tx_id,
      field_index,
      field_hash,
      total_length,
      item_count: field_grammar_next_item_index(reached),
      waypoints,
    }
  } else {
    expect batch >= field_grammar_waypoint_interval
    InProgress {
      owner,
      tx_id,
      field_index,
      field_hash,
      total_length,
      grammar_checkpoint_hash: field_grammar_checkpoint_hash(reached),
      waypoints,
    }
  }
}
