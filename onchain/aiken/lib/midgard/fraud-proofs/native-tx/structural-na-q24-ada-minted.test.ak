//// Executable structural-N/A closure for GOAL_SPEC §9.3 row Q24
//// (`ada-minted`).
////
//// The claim under test: in canonical V1 there is no CBOR encoding of a mint
//// entry that names ADA/lovelace. Every mint policy key must decode to a
//// 28-byte script hash, and the ADA asset class has no policy id at all (its
//// policy is the empty byte string). The tests below build the would-be
//// violating preimage bytes and prove the canonical mint encoder/verifier
//// rejects them, with adjacent positive controls whose only difference is a
//// corrected policy-id length.
////
//// Rejection sites:
////   * lib/midgard/fraud-proofs/native-tx/preimages.ak:330
////       `expect bytearray.length(policy_id) == 28` in
////       `encode_mint_policy_assets`
////   * lib/midgard/fraud-proofs/native-tx/preimages.ak:348
////       `expect bytearray.length(policy_id) == 28` in
////       `encode_mint_policy_item`
////   * lib/midgard/fraud-proofs/native-tx/preimages.ak:518
////       `expect bytearray.length(policy_id) == 28` in
////       `verify_canonical_mint_policies_at`
////   * lib/midgard/fraud-proofs/native-tx/types.ak:91
////       `MidgardTransactionBody.mint: Data` — the only mint carrier, read
////       exclusively through the encoders above.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use midgard/bounded_collection_v1
use midgard/fraud_proofs/native_tx/preimages.{
  encode_mint_preimage, mint_policy_items, verify_canonical_mint_preimage_cbor,
}

/// The ADA/lovelace asset class carries no policy id: on Cardano it is the
/// empty byte string, which is 0 bytes long rather than a 28-byte script hash.
const ada_policy_id: ByteArray = #""

/// A 27-byte policy id: the adjacent one-byte-short neighbour of a real
/// script hash.
const short_policy_id: ByteArray =
  #"0101010101010101010101010101010101010101010101010101ff"

/// A well-formed 28-byte script hash.
const real_policy_id: ByteArray =
  #"01010101010101010101010101010101010101010101010101010101"

/// Builds the mint `Data` carried by `MidgardTransactionBody.mint`
/// (types.ak:91) as a single-policy, single-asset map.
fn mint_data(policy_id: ByteArray, asset_name: ByteArray, quantity: Int) -> Data {
  let assets: Pairs<Data, Data> = [
    Pair(builtin.b_data(asset_name), builtin.i_data(quantity)),
  ]
  let policies: Pairs<Data, Data> = [
    Pair(builtin.b_data(policy_id), builtin.map_data(assets)),
  ]
  builtin.map_data(policies)
}

/// Serialised violating preimage: map{ h'' : map{ h'' : 5 } }.
/// `a1` map(1) `40` bytes(0) `a1` map(1) `40` bytes(0) `05`.
const ada_mint_preimage_cbor: ByteArray = #"a140a14005"

/// Serialised positive control: map{ h'0101..01'(28) : map{ h'01' : 5 } }.
const real_mint_preimage_cbor: ByteArray =
  #"a1581c01010101010101010101010101010101010101010101010101010101a1410105"

test q24_ada_mint_entry_serialises_to_the_expected_violating_cbor() {
  and {
    cbor.serialise(mint_data(ada_policy_id, #"", 5)) == ada_mint_preimage_cbor,
    cbor.serialise(mint_data(real_policy_id, #"01", 5)) == real_mint_preimage_cbor,
  }
}

test q24_ada_policy_mint_entry_rejected_by_preimage_encoder() fail {
  encode_mint_preimage(mint_data(ada_policy_id, #"", 5)) != #""
}

test q24_ada_policy_mint_entry_rejected_by_compact_item_encoder() fail {
  list.length(mint_policy_items(mint_data(ada_policy_id, #"", 5))) == 1
}

test q24_ada_policy_mint_preimage_bytes_rejected_by_canonical_verifier() fail {
  verify_canonical_mint_preimage_cbor(
    ada_mint_preimage_cbor,
    bounded_collection_v1.from_items(
      5,
      mint_policy_items(mint_data(real_policy_id, #"01", 5)),
    ),
  )
}

test q24_twenty_seven_byte_policy_id_rejected_by_preimage_encoder() fail {
  encode_mint_preimage(mint_data(short_policy_id, #"01", 5)) != #""
}

test q24_positive_control_28_byte_policy_encodes_and_verifies() {
  let mint = mint_data(real_policy_id, #"01", 5)
  let items = mint_policy_items(mint)
  and {
    encode_mint_preimage(mint) == real_mint_preimage_cbor,
    list.length(items) == 1,
    verify_canonical_mint_preimage_cbor(
      real_mint_preimage_cbor,
      bounded_collection_v1.from_items(5, items),
    ),
  }
}

test q24_positive_control_empty_mint_encodes_and_verifies() {
  let empty_mint: Data = []
  and {
    encode_mint_preimage(empty_mint) == #"80",
    verify_canonical_mint_preimage_cbor(
      #"80",
      bounded_collection_v1.from_items(5, mint_policy_items(empty_mint)),
    ),
  }
}
