//// Test-fixture support for the family steps that read a variable-width
//// field through the field grammar certificate
//// (`midgard/field_grammar_certificate_v1`).
////
//// It builds a field preimage of many items, the `Complete` certificate for
//// it — computed here from the preimage's own item wrappers, independently
//// of the validator that issues certificates — and the reference inputs a
//// step's L1 transaction carries: the §8.6 field-preimage certificate and its
//// chunks (`native_binding_fixture_v1.certified_reference_inputs_v1`) at slots
//// `0 ..= n`, then the grammar certificate. It is not reachable from any
//// on-chain validator entry point.

use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/field_grammar_certificate_v1.{
  Complete, FieldGrammarCertificateV1, InProgress,
  field_grammar_certificate_asset_name, field_grammar_waypoint_interval,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/native_tx_carriage_v1 as native_carriage
use midgard/native_tx_field_access_v1 as field_access

/// The field grammar certificate policy the selectors parameterise their
/// steps with: the certificate validator's script hash, which is also its
/// payment credential.
pub const test_grammar_certificate_policy_id: PolicyId =
  #"78787878787878787878787878787878787878787878787878787878"

const grammar_owner: ByteArray =
  #"79797979797979797979797979797979797979797979797979797979"

const grammar_certificate_utxo_id: ByteArray =
  #"7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a"

/// `payload` with its minimal §5.1 definite byte-string wrapper.
pub fn wrap_item_v1(payload: ByteArray) -> ByteArray {
  let length = bytearray.length(payload)
  if length <= 23 {
    bytearray.push(payload, 64 + length)
  } else if length <= 255 {
    bytearray.concat(#"58", bytearray.push(payload, length))
  } else {
    bytearray.concat(
      #"59",
      bytearray.concat(bytearray.from_int_big_endian(length, 2), payload),
    )
  }
}

/// A field preimage of `count` items: `filler` everywhere except `target` at
/// `target_index`. Both are payloads; the wrappers are added here.
pub fn field_preimage_with_item_v1(
  filler: ByteArray,
  count: Int,
  target_index: Int,
  target: ByteArray,
) -> ByteArray {
  expect target_index >= 0
  expect target_index < count
  let wrapped = wrap_item_v1(filler)
  field_access.encode_field_array_header(count)
    |> bytearray.concat(fixture.repeated_bytes_v1(wrapped, target_index))
    |> bytearray.concat(wrap_item_v1(target))
    |> bytearray.concat(
        fixture.repeated_bytes_v1(wrapped, count - target_index - 1),
      )
}

/// The item count up to which a fault-proof submitter reads a field by walking
/// it rather than through a grammar certificate (`FIELD_GRAMMAR_WALKED_ITEM_LIMIT`
/// in `demo/midgard-fault-proofs/src/field-grammar-certificate.ts`). Every step
/// that takes either route holds a budget row for its walked route at this
/// count.
pub const walked_item_limit = 64

/// The item count up to which a fault-proof submitter counts a field by
/// walking it (`FIELD_GRAMMAR_WALKED_COUNT_LIMIT` in
/// `demo/midgard-fault-proofs/src/field-grammar-certificate.ts`). A count
/// walk reads no item, so it costs far less per item than an item walk. Every
/// step that counts a field by either route holds a budget row for its walked
/// count at this count.
pub const walked_count_limit = 256

/// `walked_item_limit` items near the 32,768-byte field maximum, so under
/// tier 3: 500-byte fillers with `target` last, the item a walk reaches last.
pub fn walked_limit_preimage_v1(target: ByteArray) -> ByteArray {
  field_preimage_with_item_v1(
    fixture.repeated_bytes_v1(#"a1", 500),
    walked_item_limit,
    walked_item_limit - 1,
    target,
  )
}

/// `walked_item_limit` items under the 14,336-byte tier-1 bound: 200-byte
/// fillers with `target` last. A walk counts a field it can carry inline or
/// in one raw UTxO, but not a chunked one.
pub fn walked_limit_inline_preimage_v1(target: ByteArray) -> ByteArray {
  field_preimage_with_item_v1(
    fixture.repeated_bytes_v1(#"a1", 200),
    walked_item_limit,
    walked_item_limit - 1,
    target,
  )
}

/// `walked_count_limit` items under the 14,336-byte tier-1 bound: 52-byte
/// fillers with `target` last.
pub fn walked_count_limit_inline_preimage_v1(target: ByteArray) -> ByteArray {
  field_preimage_with_item_v1(
    fixture.repeated_bytes_v1(#"a1", 52),
    walked_count_limit,
    walked_count_limit - 1,
    target,
  )
}

/// The byte length of the wrapped item whose wrapper starts at `offset`.
fn wrapped_length_at(preimage: ByteArray, offset: Int) -> Int {
  let head = builtin.index_bytearray(preimage, offset)
  if head < 0x58 {
    1 + head - 0x40
  } else if head == 0x58 {
    2 + builtin.index_bytearray(preimage, offset + 1)
  } else {
    expect head == 0x59
    3 + bytearray.to_int_big_endian(
      bytearray.slice(preimage, offset + 1, offset + 2),
    )
  }
}

fn header_length(count: Int) -> Int {
  if count < 24 {
    1
  } else if count < 256 {
    2
  } else {
    3
  }
}

/// The waypoints of the first `count` items: the wrapper offset of every
/// `field_grammar_waypoint_interval`-th item, two bytes each, in item order.
fn waypoints_of(preimage: ByteArray, count: Int) -> ByteArray {
  do_waypoints(preimage, count, 0, header_length(count), #"")
}

fn do_waypoints(
  preimage: ByteArray,
  count: Int,
  item: Int,
  offset: Int,
  joined: ByteArray,
) -> ByteArray {
  if item >= count {
    joined
  } else {
    let next = if item % field_grammar_waypoint_interval == 0 {
      bytearray.concat(joined, bytearray.from_int_big_endian(offset, 2))
    } else {
      joined
    }
    do_waypoints(
      preimage,
      count,
      item + 1,
      offset + wrapped_length_at(preimage, offset),
      next,
    )
  }
}

/// The `Complete` certificate for field `field_index` of transaction `tx_id`
/// over `preimage`, which holds `count` items.
pub fn complete_field_grammar_certificate_v1(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
  count: Int,
) -> FieldGrammarCertificateV1 {
  Complete {
    owner: grammar_owner,
    tx_id,
    field_index,
    field_hash: field_access.field_commitment(preimage),
    total_length: bytearray.length(preimage),
    item_count: count,
    waypoints: waypoints_of(preimage, count),
  }
}

/// The same field's certificate before its walk is complete.
pub fn in_progress_field_grammar_certificate_v1(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
  count: Int,
) -> FieldGrammarCertificateV1 {
  InProgress {
    owner: grammar_owner,
    tx_id,
    field_index,
    field_hash: field_access.field_commitment(preimage),
    total_length: bytearray.length(preimage),
    grammar_checkpoint_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
    waypoints: waypoints_of(preimage, count / 2),
  }
}

/// `certificate` held at the grammar certificate policy's own address with
/// its one token, as the certificate validator leaves it.
pub fn field_grammar_certificate_input_v1(
  certificate: FieldGrammarCertificateV1,
) -> Input {
  let datum: Data = certificate
  Input {
    output_reference: OutputReference {
      transaction_id: grammar_certificate_utxo_id,
      output_index: 0,
    },
    output: Output {
      address: address.from_script(test_grammar_certificate_policy_id),
      value: assets.from_lovelace(5_000_000)
        |> assets.add(
            test_grammar_certificate_policy_id,
            field_grammar_certificate_asset_name,
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

/// The tier-3 carriage of `preimage` at slots `0 ..= n`
/// (`fixture.certified_reference_inputs_v1`) followed by `certificate`, whose
/// slot is `grammar_ref_input_index_v1(preimage)`.
pub fn certified_grammar_reference_inputs_v1(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
  certificate: FieldGrammarCertificateV1,
) -> List<Input> {
  list.concat(
    fixture.certified_reference_inputs_v1(tx_id, field_index, preimage),
    [field_grammar_certificate_input_v1(certificate)],
  )
}

/// The slot `certified_grammar_reference_inputs_v1` puts the grammar
/// certificate at.
pub fn grammar_ref_input_index_v1(preimage: ByteArray) -> Int {
  1 + list.length(native_carriage.split_field_preimage(preimage))
}
