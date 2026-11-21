//// The field grammar certificate, as a consuming step reads it.
////
//// A field grammar certificate lets a step read one item of a variable-width
//// field (2, 5, 6 or 8) without walking the whole field, so the step's cost no
//// longer grows with the field's item count. The permissionless validator
//// `validators/field-grammar-certificate.ak` builds it over one or more
//// transactions by walking the field's §5.1 envelope from its first item to
//// its exact end, and its terminal datum records two facts about the
//// committed bytes:
////
////   * the item count — the walk consumed exactly the header's declared
////     number of canonical item envelopes and ended on the last byte; and
////   * a waypoint every `field_grammar_waypoint_interval` items — the byte
////     offset of the wrapper of item `k · field_grammar_waypoint_interval`,
////     for every such item.
////
//// A consumer holds the certificate as a reference input, binds it to the
//// field commitment it authenticated itself (`bind_certified_field_grammar`),
//// and then reaches any item by starting from the nearest waypoint at or
//// before it and reading at most `field_grammar_waypoint_interval - 1` item
//// headers.
////
//// **What binds a certificate to the consumer's bytes.** The datum's
//// `field_hash` is the positional §4 commitment the validator extracted from
//// the tx-id-verified compact structures, exactly as the field-preimage
//// certificate's is (§8.6). A consumer compares it with the commitment it
//// anchored itself; for the witness-set fields that is the value its own step
//// read from the block-committed structure. Equal commitments mean the same
//// bytes, so the certificate's count and offsets are facts about the bytes
//// the consumer's view holds.
////
//// **One certificate per field, whoever builds it.** The waypoint interval is
//// a protocol constant and every value in a `Complete` datum is a function of
//// the field's bytes, so two certificates for one field are byte-identical
//// apart from `owner`. A yanked certificate is rebuilt by anyone and is
//// interchangeable with the one that was yanked.

use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input}
use midgard/native_tx_field_access_v1.{
  FieldViewV1, field_item_header_at, field_read_range, field_total_length,
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// The number of items between two waypoints. A protocol constant: changing
/// it changes every certificate's bytes, so it is part of the wire format.
pub const field_grammar_waypoint_interval: Int = 16

/// Each waypoint is an unsigned big-endian offset of this many bytes. A field
/// preimage is at most `max_transaction_aggregate_field_bytes` (32,768)
/// bytes, so every wrapper offset fits.
pub const field_grammar_waypoint_bytes: Int = 2

/// The certificate token's asset name — one constant for every certificate of
/// the policy, as for the field-preimage certificate. The name carries no
/// identity: everything that identifies a certificate is in its datum, and
/// the validator pins the name so a token of the policy is always this name
/// over a datum the validator computed.
pub const field_grammar_certificate_asset_name: AssetName =
  "MIDGARD_FIELD_GRAMMAR_CERT"

// ---------------------------------------------------------------------------
// The datum — constructor order is frozen wire format
// ---------------------------------------------------------------------------

/// A field grammar certificate. `InProgress` is Constr 0 and `Complete`
/// Constr 1.
///
/// `waypoints` is `field_grammar_waypoint_bytes`-wide offsets concatenated in
/// item order: entry `k` is the wrapper offset of item
/// `k · field_grammar_waypoint_interval`, so entry 0 is the header length.
/// A `Complete` certificate holds one entry for every such item below
/// `item_count`; an `InProgress` one holds the entries for the items it has
/// already passed.
pub type FieldGrammarCertificateV1 {
  /// A walk under way. `grammar_checkpoint_hash` is the digest of the
  /// grammar checkpoint the next advance resumes from.
  InProgress {
    owner: VerificationKeyHash,
    tx_id: ByteArray,
    field_index: Int,
    field_hash: ByteArray,
    total_length: Int,
    grammar_checkpoint_hash: ByteArray,
    waypoints: ByteArray,
  }
  /// A walk that consumed exactly `item_count` canonical item envelopes and
  /// ended on the field's last byte.
  Complete {
    owner: VerificationKeyHash,
    tx_id: ByteArray,
    field_index: Int,
    field_hash: ByteArray,
    total_length: Int,
    item_count: Int,
    waypoints: ByteArray,
  }
}

/// A `Complete` certificate bound to one field of one transaction. Opaque: the
/// only way to get one is `bind_certified_field_grammar` (or the reference
/// input form of it), so holding one means the binding was checked.
pub opaque type CertifiedFieldGrammarV1 {
  total_length: Int,
  item_count: Int,
  waypoints: ByteArray,
}

// ---------------------------------------------------------------------------
// Binding
// ---------------------------------------------------------------------------

/// Binds a certificate to the field a consumer authenticated. Fails unless the
/// certificate is `Complete` and its `tx_id`, `field_index`, `field_hash` and
/// `total_length` equal the arguments. `field_hash` must be the commitment the
/// consumer opened its view against, never a value from a redeemer.
pub fn bind_certified_field_grammar(
  certificate: FieldGrammarCertificateV1,
  tx_id: ByteArray,
  field_index: Int,
  field_hash: ByteArray,
  total_length: Int,
) -> CertifiedFieldGrammarV1 {
  expect
      Complete {
        tx_id: certified_tx_id,
        field_index: certified_field_index,
        field_hash: certified_field_hash,
        total_length: certified_total_length,
        item_count,
        waypoints,
        ..
      }
    = certificate
  expect certified_tx_id == tx_id
  expect certified_field_index == field_index
  expect certified_field_hash == field_hash
  expect certified_total_length == total_length
  CertifiedFieldGrammarV1 { total_length, item_count, waypoints }
}

/// The certificate held by reference input `cert_ref_input_index`. Fails
/// unless that input sits at the certificate policy's own script address and
/// holds exactly one of its tokens, with the certificate as inline datum.
pub fn field_grammar_certificate_at(
  reference_inputs: List<Input>,
  cert_ref_input_index: Int,
  grammar_certificate_policy_id: PolicyId,
) -> FieldGrammarCertificateV1 {
  expect cert_ref_input_index >= 0
  expect Some(cert_input) = list.at(reference_inputs, cert_ref_input_index)
  let output = cert_input.output
  expect
    output.address.payment_credential == Script(grammar_certificate_policy_id)
  expect
    assets.quantity_of(
      output.value,
      grammar_certificate_policy_id,
      field_grammar_certificate_asset_name,
    ) == 1
  expect InlineDatum(datum) = output.datum
  expect certificate: FieldGrammarCertificateV1 = datum
  certificate
}

/// `field_grammar_certificate_at` then `bind_certified_field_grammar`.
pub fn certified_field_grammar_at(
  reference_inputs: List<Input>,
  cert_ref_input_index: Int,
  grammar_certificate_policy_id: PolicyId,
  tx_id: ByteArray,
  field_index: Int,
  field_hash: ByteArray,
  total_length: Int,
) -> CertifiedFieldGrammarV1 {
  bind_certified_field_grammar(
    field_grammar_certificate_at(
      reference_inputs,
      cert_ref_input_index,
      grammar_certificate_policy_id,
    ),
    tx_id,
    field_index,
    field_hash,
    total_length,
  )
}

// ---------------------------------------------------------------------------
// Reading
// ---------------------------------------------------------------------------

/// The certified item count of the bound field.
pub fn certified_field_item_count(certified: CertifiedFieldGrammarV1) -> Int {
  certified.item_count
}

/// The `(payload offset, payload length)` of item `item_index`, the same pair
/// `field_item_extent` returns. Fails unless `0 ≤ item_index < item_count` and
/// `view` has the bound field's length. Reads at most
/// `field_grammar_waypoint_interval` item headers, whatever the item count.
pub fn certified_field_item_extent(
  view: FieldViewV1,
  certified: CertifiedFieldGrammarV1,
  item_index: Int,
) -> Pair<Int, Int> {
  expect field_total_length(view) == certified.total_length
  expect item_index >= 0
  expect item_index < certified.item_count
  let waypoint = item_index / field_grammar_waypoint_interval
  walk_item_headers(
    view,
    certified_field_waypoint_offset(certified, waypoint),
    item_index - waypoint * field_grammar_waypoint_interval,
  )
}

/// The payload bytes of item `item_index`: `certified_field_item_extent`, then
/// one read.
pub fn certified_field_item_at(
  view: FieldViewV1,
  certified: CertifiedFieldGrammarV1,
  item_index: Int,
) -> ByteArray {
  let Pair(offset, length) = certified_field_item_extent(
    view,
    certified,
    item_index,
  )
  field_read_range(view, offset, length)
}

/// The wrapper offset of item `waypoint · field_grammar_waypoint_interval`.
/// Fails unless that item exists.
pub fn certified_field_waypoint_offset(
  certified: CertifiedFieldGrammarV1,
  waypoint: Int,
) -> Int {
  expect waypoint >= 0
  expect waypoint * field_grammar_waypoint_interval < certified.item_count
  let start = waypoint * field_grammar_waypoint_bytes
  bytearray.to_int_big_endian(
    bytearray.slice(
      certified.waypoints,
      start,
      start + field_grammar_waypoint_bytes - 1,
    ),
  )
}

fn walk_item_headers(
  view: FieldViewV1,
  offset: Int,
  remaining: Int,
) -> Pair<Int, Int> {
  let Pair(payload_offset, length) = field_item_header_at(view, offset)
  if remaining == 0 {
    Pair(payload_offset, length)
  } else {
    walk_item_headers(view, payload_offset + length, remaining - 1)
  }
}
