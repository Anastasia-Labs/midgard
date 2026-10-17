use aiken/crypto.{blake2b_224}
use aiken/primitive/bytearray
use midgard/fixed_item_legality_v1.{
  fixed_item_is_legal_v1, item_width_is_legal_v1,
}

const key: ByteArray =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const signature: ByteArray =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f"

fn input(index: Int) -> ByteArray {
  #"5826825820"
    |> bytearray.concat(key)
    |> bytearray.concat(#"19")
    |> bytearray.concat(bytearray.from_int_big_endian(index, 2))
}

fn hash_item(hash: ByteArray) -> ByteArray {
  bytearray.concat(#"581c", hash)
}

fn witness(vkey: ByteArray, sig: ByteArray) -> ByteArray {
  #"5865825820"
    |> bytearray.concat(vkey)
    |> bytearray.concat(#"5840")
    |> bytearray.concat(sig)
}

test fixed_input_accepts_exact_shape_at_both_fields() {
  and {
    fixed_item_is_legal_v1(0, None, input(0)),
    fixed_item_is_legal_v1(1, Some(input(0)), input(65535)),
  }
}

test fixed_input_rejects_wrong_wrapper_without_aborting() {
  !fixed_item_is_legal_v1(
    0,
    None,
    bytearray.concat(#"ff26", bytearray.drop(input(0), 2)),
  )
}

test fixed_input_rejects_wrong_payload_constructor_without_aborting() {
  !fixed_item_is_legal_v1(
    1,
    None,
    bytearray.concat(#"582683", bytearray.drop(input(0), 3)),
  )
}

test fixed_input_rejects_nonfixed_output_index_without_aborting() {
  !fixed_item_is_legal_v1(
    0,
    None,
    bytearray.concat(bytearray.take(input(0), 37), #"180000"),
  )
}

test fixed_input_accepts_repeat_for_duplicate_input_phase() {
  fixed_item_is_legal_v1(0, Some(input(8)), input(8))
}

test fixed_spend_input_accepts_descending_unique_order() {
  fixed_item_is_legal_v1(0, Some(input(9)), input(8))
}

test fixed_reference_input_accepts_descending_unique_order() {
  fixed_item_is_legal_v1(1, Some(input(9)), input(8))
}

test fixed_reference_input_accepts_repeat_for_duplicate_input_phase() {
  fixed_item_is_legal_v1(1, Some(input(8)), input(8))
}

test fixed_observer_accepts_unordered_pair_for_phase_a_order_check() {
  let low = hash_item(bytearray.take(key, 28))
  let high = hash_item(bytearray.concat(#"ff", bytearray.take(key, 27)))
  fixed_item_is_legal_v1(3, Some(high), low)
}

test fixed_required_signer_accepts_repeat() {
  let item = hash_item(bytearray.take(key, 28))
  fixed_item_is_legal_v1(4, Some(item), item)
}

test fixed_required_signer_accepts_descending_order() {
  let low = hash_item(bytearray.take(key, 28))
  let high = hash_item(bytearray.concat(#"ff", bytearray.take(key, 27)))
  fixed_item_is_legal_v1(4, Some(high), low)
}

test fixed_required_signer_accepts_ascending_order() {
  let low = hash_item(bytearray.take(key, 28))
  let high = hash_item(bytearray.concat(#"ff", bytearray.take(key, 27)))
  fixed_item_is_legal_v1(4, Some(low), high)
}

test fixed_required_signer_rejects_malformed_current_item() {
  let item = hash_item(bytearray.take(key, 28))
  !fixed_item_is_legal_v1(4, Some(item), bytearray.take(item, 29))
}

test fixed_required_signer_rejects_malformed_previous_item() {
  let item = hash_item(bytearray.take(key, 28))
  !fixed_item_is_legal_v1(4, Some(bytearray.take(item, 29)), item)
}

test fixed_required_signer_rejects_wrong_wrapper() {
  !fixed_item_is_legal_v1(
    4,
    None,
    bytearray.concat(#"581d", bytearray.take(key, 28)),
  )
}

test fixed_hash_rejects_bad_wrapper_without_aborting() {
  !fixed_item_is_legal_v1(
    3,
    None,
    bytearray.concat(#"581d", bytearray.take(key, 28)),
  )
}

test fixed_witness_accepts_exact_shape() {
  fixed_item_is_legal_v1(7, None, witness(key, signature))
}

test fixed_witness_rejects_bad_wrapper_without_aborting() {
  !fixed_item_is_legal_v1(
    7,
    None,
    bytearray.concat(#"5864", bytearray.drop(witness(key, signature), 2)),
  )
}

test fixed_witness_rejects_bad_signature_header_without_aborting() {
  !fixed_item_is_legal_v1(
    7,
    None,
    bytearray.concat(
      bytearray.take(witness(key, signature), 37),
      bytearray.concat(#"5841", signature),
    ),
  )
}

test fixed_witness_accepts_exact_repeat() {
  let item = witness(key, signature)
  fixed_item_is_legal_v1(7, Some(item), item)
}

test fixed_witness_accepts_both_same_signer_signature_orders() {
  let low = witness(key, signature)
  let high = witness(
    key,
    bytearray.concat(#"ff", bytearray.take(signature, 63)),
  )
  and {
    fixed_item_is_legal_v1(7, Some(low), high),
    fixed_item_is_legal_v1(7, Some(high), low),
  }
}

test fixed_witness_accepts_descending_signer_hashes() {
  let other = bytearray.concat(#"01", bytearray.take(key, 31))
  let first = witness(key, signature)
  let second = witness(other, signature)
  and {
    // The signer hashes descend while the verification keys ascend.
    bytearray.compare(blake2b_224(key), blake2b_224(other)) == Greater,
    bytearray.compare(key, other) == Less,
    fixed_item_is_legal_v1(7, Some(first), second),
    fixed_item_is_legal_v1(7, Some(second), first),
  }
}

test fixed_legality_is_total_on_short_items_and_previous_items() {
  and {
    !fixed_item_is_legal_v1(0, None, #""),
    !fixed_item_is_legal_v1(3, None, #"581c"),
    !fixed_item_is_legal_v1(7, None, #"586582"),
    !fixed_item_is_legal_v1(7, Some(#"00"), witness(key, signature)),
    !fixed_item_is_legal_v1(2, None, input(0)),
  }
}

test item_width_keeps_output_cap_and_adjacent_refusal() {
  and {
    item_width_is_legal_v1(2, 16384),
    !item_width_is_legal_v1(2, 16385),
  }
}

test item_width_keeps_nonempty_mint_requirement() {
  and {
    !item_width_is_legal_v1(5, 0),
    item_width_is_legal_v1(5, 1),
  }
}

test item_width_rejects_wrong_fixed_payload_width() {
  and {
    !item_width_is_legal_v1(0, 37),
    item_width_is_legal_v1(1, 38),
    !item_width_is_legal_v1(3, 29),
    item_width_is_legal_v1(4, 28),
    item_width_is_legal_v1(7, 101),
    !item_width_is_legal_v1(7, 102),
  }
}

test item_width_is_total_on_negative_and_unsupported_fields() {
  and {
    !item_width_is_legal_v1(0, -1),
    !item_width_is_legal_v1(-1, 0),
    !item_width_is_legal_v1(9, 0),
    item_width_is_legal_v1(6, 0),
    item_width_is_legal_v1(8, 0),
  }
}

// A prior item remains shape evidence; removing ordering must not hide garbage.
test fixed_witness_rejects_a_bad_previous_wrapper() {
  !fixed_item_is_legal_v1(
    7,
    Some(bytearray.concat(#"5864", bytearray.drop(witness(key, signature), 2))),
    witness(key, signature),
  )
}

test fixed_witness_rejects_a_bad_previous_signature_header() {
  let prior = bytearray.concat(
    bytearray.take(witness(key, signature), 37),
    bytearray.concat(#"5841", signature),
  )
  !fixed_item_is_legal_v1(7, Some(prior), witness(key, signature))
}
