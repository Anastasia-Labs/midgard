//// Producers and decoders for the nine field preimages of
//// `docs/spec/midgard-tx.md` §5.
////
//// Every field — all nine, including mint, script witnesses and redeemer
//// witnesses — is the same §5.1 envelope: a definite array header followed by
//// one definite byte-string-wrapped item per element, and exactly `80` when
//// empty. The uniform envelope is what buys O(1) top-level skips (one head
//// decode plus a byte jump per item) and lets a single walk implementation
//// serve every field dispute.
////
//// The commitment over these bytes is a plain `blake2b_256`
//// (`midgard/native_tx_field_access_v1.field_commitment`); this module owns
//// the bytes, not the hashing.
////
//// **This module is not an access idiom.** The one consumer-visible way to
//// open a committed field is `authenticated_field_view`; the decoders here
//// take an already-authenticated preimage and materialise it as typed items,
//// which is what `transaction.verify_midgard_transaction_field_preimage_v1`
//// needs to re-encode and so pin the canonical spelling. Nothing here
//// verifies a field hash on its own — the two helpers that used to
//// (`verify_spend_inputs_preimage`, `spend_input_cbor_at`) had no production
//// caller and are gone, because a free-standing field hash is exactly the
//// shape §4's positional-identity invariant prohibits at a dispute entry
//// point.
////
//// The §5.1 array head is read through `decode_field_array_header_at` from
//// the door module — the single decoder for that grammar. `codec`'s
//// `decode_definite_array_header_at` is the general CBOR reader and admits
//// heads (non-minimal widths, the four-byte `9a` form) that §5.1 does not;
//// using it here gave one grammar two verdicts, with the mint field the one
//// that never re-encodes to catch the difference.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  byte_at, decode_canonical_int_at, decode_definite_bytes_at,
  encode_definite_bytes, encode_definite_map_header, expect_byte, slice_len,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, decode_midgard_redeemer_witness_at,
  decode_midgard_tx_input_cbor, decode_midgard_tx_output_cbor,
  decode_midgard_versioned_script_at, encode_midgard_address_witness,
  encode_midgard_redeemer_witness, encode_midgard_tx_input,
  encode_midgard_tx_output, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardRedeemerWitness, MidgardScriptWitness,
  MidgardTxInput, MidgardTxOutput, MidgardVersionedScript,
}
use midgard/native_tx_field_access_v1.{
  decode_field_array_header_at, encode_field_preimage, field_commitment,
  hash28_item_bytes,
}

// ---------------------------------------------------------------------------
// Producers (§5.1 envelope over the §5.3 item encodings)
// ---------------------------------------------------------------------------

pub fn encode_input_preimage(inputs: List<MidgardTxInput>) -> ByteArray {
  inputs |> list.map(encode_midgard_tx_input) |> encode_field_preimage
}

pub fn encode_output_preimage(outputs: List<MidgardTxOutput>) -> ByteArray {
  outputs |> list.map(encode_midgard_tx_output) |> encode_field_preimage
}

/// Fields 3/4 (§5.3): every observer/signer item is asserted to be exactly
/// 28 bytes, which is what fixes the stride at 30 and makes item access
/// arithmetic.
pub fn encode_hash28_list_preimage(items: List<ByteArray>) -> ByteArray {
  items |> list.map(expect_hash28) |> encode_field_preimage
}

fn expect_hash28(item: ByteArray) -> ByteArray {
  expect bytearray.length(item) == hash28_item_bytes
  item
}

pub fn encode_address_witness_preimage(
  witnesses: List<MidgardAddressWitness>,
) -> ByteArray {
  witnesses |> list.map(encode_midgard_address_witness) |> encode_field_preimage
}

/// Field 6. Under the retired counted scheme this concatenated raw item CBOR
/// with no per-item wrapper; the §5.1 envelope now applies here like
/// everywhere else.
pub fn encode_script_witness_preimage(
  witnesses: List<MidgardScriptWitness>,
) -> ByteArray {
  witnesses
    |> list.map(
        fn(witness) {
          let script: MidgardVersionedScript = witness
          encode_midgard_versioned_script(script)
        },
      )
    |> encode_field_preimage
}

/// Field 8. Same reversion as field 6: enveloped items, not raw concatenation.
pub fn encode_redeemer_witness_preimage(
  witnesses: List<MidgardRedeemerWitness>,
) -> ByteArray {
  witnesses
    |> list.map(encode_midgard_redeemer_witness)
    |> encode_field_preimage
}

// ---------------------------------------------------------------------------
// Decoders
// ---------------------------------------------------------------------------

pub fn decode_midgard_tx_byte_list_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<ByteArray> {
  let Pair(offset, item_count) = decode_field_array_header_at(
    preimage_cbor,
    0,
  )
  let Pair(offset, items) = decode_byte_items_at(
    preimage_cbor,
    offset,
    item_count,
  )
  expect offset == bytearray.length(preimage_cbor)
  items
}

/// Fields 3/4: the same byte list, with the §5.3 width assertion enforced on
/// the way in so a decoder can never admit an item the encoder would refuse.
pub fn decode_midgard_tx_hash28_list_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<ByteArray> {
  decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)
    |> list.map(expect_hash28)
}

fn decode_byte_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<ByteArray>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, item_cbor) = decode_definite_bytes_at(
      preimage_cbor,
      offset,
    )
    let Pair(offset, rest) = decode_byte_items_at(
      preimage_cbor,
      offset,
      remaining - 1,
    )
    Pair(offset, [item_cbor, ..rest])
  }
}

fn decode_input_cbor_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<MidgardTxInput>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, input_cbor) = decode_definite_bytes_at(
      preimage_cbor,
      offset,
    )
    let input = decode_midgard_tx_input_cbor(input_cbor)
    let Pair(offset, rest) = decode_input_cbor_items_at(
      preimage_cbor,
      offset,
      remaining - 1,
    )
    Pair(offset, [input, ..rest])
  }
}

fn decode_output_cbor_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<MidgardTxOutput>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, output_cbor) = decode_definite_bytes_at(
      preimage_cbor,
      offset,
    )
    let output = decode_midgard_tx_output_cbor(output_cbor)
    let Pair(offset, rest) = decode_output_cbor_items_at(
      preimage_cbor,
      offset,
      remaining - 1,
    )
    Pair(offset, [output, ..rest])
  }
}

fn decode_address_witness_cbor_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<MidgardAddressWitness>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, witness_cbor) = decode_definite_bytes_at(
      preimage_cbor,
      offset,
    )
    let witness = decode_midgard_address_witness_cbor(witness_cbor)
    let Pair(offset, rest) = decode_address_witness_cbor_items_at(
      preimage_cbor,
      offset,
      remaining - 1,
    )
    Pair(offset, [witness, ..rest])
  }
}

fn decode_script_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<MidgardScriptWitness>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, item_cbor) = decode_definite_bytes_at(
      preimage_cbor,
      offset,
    )
    let Pair(item_offset, script) = decode_midgard_versioned_script_at(
      item_cbor,
      0,
    )
    expect item_offset == bytearray.length(item_cbor)
    let Pair(offset, rest) = decode_script_items_at(
      preimage_cbor,
      offset,
      remaining - 1,
    )
    Pair(offset, [script, ..rest])
  }
}

fn decode_redeemer_witness_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<MidgardRedeemerWitness>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, item_cbor) = decode_definite_bytes_at(
      preimage_cbor,
      offset,
    )
    let Pair(item_offset, witness) = decode_midgard_redeemer_witness_at(
      item_cbor,
      0,
    )
    expect item_offset == bytearray.length(item_cbor)
    let Pair(offset, rest) = decode_redeemer_witness_items_at(
      preimage_cbor,
      offset,
      remaining - 1,
    )
    Pair(offset, [witness, ..rest])
  }
}

pub fn decode_midgard_tx_inputs_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<MidgardTxInput> {
  let Pair(offset, item_count) = decode_field_array_header_at(
    preimage_cbor,
    0,
  )
  let Pair(offset, inputs) = decode_input_cbor_items_at(
    preimage_cbor,
    offset,
    item_count,
  )
  expect offset == bytearray.length(preimage_cbor)
  inputs
}

pub fn decode_midgard_tx_outputs_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<MidgardTxOutput> {
  let Pair(offset, item_count) = decode_field_array_header_at(
    preimage_cbor,
    0,
  )
  let Pair(offset, outputs) = decode_output_cbor_items_at(
    preimage_cbor,
    offset,
    item_count,
  )
  expect offset == bytearray.length(preimage_cbor)
  outputs
}

pub fn decode_midgard_tx_address_witnesses_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<MidgardAddressWitness> {
  let Pair(offset, item_count) = decode_field_array_header_at(
    preimage_cbor,
    0,
  )
  let Pair(offset, witnesses) = decode_address_witness_cbor_items_at(
    preimage_cbor,
    offset,
    item_count,
  )
  expect offset == bytearray.length(preimage_cbor)
  witnesses
}

pub fn decode_midgard_tx_script_witnesses_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<MidgardScriptWitness> {
  let Pair(offset, item_count) = decode_field_array_header_at(
    preimage_cbor,
    0,
  )
  let Pair(offset, witnesses) = decode_script_items_at(
    preimage_cbor,
    offset,
    item_count,
  )
  expect offset == bytearray.length(preimage_cbor)
  witnesses
}

pub fn decode_midgard_tx_redeemer_witnesses_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<MidgardRedeemerWitness> {
  let Pair(offset, item_count) = decode_field_array_header_at(
    preimage_cbor,
    0,
  )
  let Pair(offset, witnesses) = decode_redeemer_witness_items_at(
    preimage_cbor,
    offset,
    item_count,
  )
  expect offset == bytearray.length(preimage_cbor)
  witnesses
}

// ---------------------------------------------------------------------------
// Field 5 — mint (§5.6)
// ---------------------------------------------------------------------------

/// §5.6: asset names within a policy appear in canonical key order and
/// duplicates reject. The order is carried through the walk as
/// `previous_asset_name` rather than checked over a materialised key list,
/// which is exactly the shape `decode_mint_assets_at` uses on the way back in.
///
/// Enforced on the producer side and not merely assumed of the caller: the
/// decoder checks it, so an encoder that let a descending or duplicated run
/// past would hand back a preimage that never decodes — a producer emitting
/// uncommittable bytes with no error. The TypeScript twin
/// (`encodeMidgardMintPolicyItemV1`) rejects at the same two points.
fn encode_mint_asset_quantities(
  pairs: List<Pair<Data, Data>>,
  previous_asset_name: Option<ByteArray>,
) -> ByteArray {
  when pairs is {
    [] -> #""
    [Pair(asset_name_data, quantity_data), ..rest] -> {
      let asset_name = builtin.un_b_data(asset_name_data)
      let quantity = builtin.un_i_data(quantity_data)
      expect bytearray.length(asset_name) <= 32
      expect quantity != 0
      expect
        when previous_asset_name is {
          None -> True
          Some(previous) -> canonical_bytes_key_precedes(previous, asset_name)
        }
      encode_definite_bytes(asset_name)
        |> bytearray.concat(cbor.serialise(quantity))
        |> bytearray.concat(
            encode_mint_asset_quantities(rest, Some(asset_name)),
          )
    }
  }
}

pub fn encode_mint_policy_item(
  policy_id_data: Data,
  asset_map_data: Data,
) -> ByteArray {
  let policy_id = builtin.un_b_data(policy_id_data)
  let asset_pairs = builtin.un_map_data(asset_map_data)
  expect bytearray.length(policy_id) == 28
  expect list.length(asset_pairs) > 0
  #"82"
    |> bytearray.concat(encode_definite_bytes(policy_id))
    |> bytearray.concat(
        encode_definite_map_header(list.length(asset_pairs))
          |> bytearray.concat(encode_mint_asset_quantities(asset_pairs, None)),
      )
}

fn canonical_mint_policy_pairs(mint: Data) -> List<Pair<Data, Data>> {
  let empty_mint: Data = []
  if mint == empty_mint {
    []
  } else {
    let policy_pairs = builtin.un_map_data(mint)
    expect list.length(policy_pairs) > 0
    policy_pairs
  }
}

/// §5.6's field-level ordering rule, the twin of the asset-name one a level
/// down: the *policy items* of field 5 appear in canonical key order and
/// duplicates reject. It lives on the run rather than inside
/// `encode_mint_policy_item`, because ordering is a property of the run and no
/// single item can see it — the same split the decoder makes between
/// `decode_mint_policy_items_at` and `decode_mint_policy_item_cbor`.
fn encode_mint_policy_items(
  pairs: List<Pair<Data, Data>>,
  previous_policy_id: Option<ByteArray>,
) -> List<ByteArray> {
  when pairs is {
    [] -> []
    [Pair(policy_id_data, asset_map_data), ..rest] -> {
      let policy_id = builtin.un_b_data(policy_id_data)
      expect
        when previous_policy_id is {
          None -> True
          Some(previous) -> canonical_bytes_key_precedes(previous, policy_id)
        }
      [
        encode_mint_policy_item(policy_id_data, asset_map_data),
        ..encode_mint_policy_items(rest, Some(policy_id))
      ]
    }
  }
}

pub fn mint_policy_items(mint: Data) -> List<ByteArray> {
  canonical_mint_policy_pairs(mint) |> encode_mint_policy_items(None)
}

/// §5.6: the field-5 preimage is the enveloped list of per-policy items, and
/// an empty mint encodes as `80` like every other field. The retired raw-map
/// form (`a0` for empty, `a1 …` otherwise) is prohibited.
pub fn encode_mint_preimage(mint: Data) -> ByteArray {
  mint |> mint_policy_items |> encode_field_preimage
}

/// Rebuilds the mint `Data` map from the enveloped preimage. Ordering,
/// widths, non-empty policies and non-zero quantities are all checked on the
/// way through, so a decoded mint is a canonical mint.
pub fn decode_midgard_tx_mint_preimage_cbor(preimage_cbor: ByteArray) -> Data {
  let Pair(offset, item_count) = decode_field_array_header_at(
    preimage_cbor,
    0,
  )
  let Pair(offset, policy_pairs) = decode_mint_policy_items_at(
    preimage_cbor,
    offset,
    item_count,
    None,
  )
  expect offset == bytearray.length(preimage_cbor)
  if item_count == 0 {
    let empty_mint: Data = []
    empty_mint
  } else {
    builtin.map_data(policy_pairs)
  }
}

fn decode_mint_policy_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
  previous_policy_id: Option<ByteArray>,
) -> Pair<Int, List<Pair<Data, Data>>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, item_cbor) = decode_definite_bytes_at(
      preimage_cbor,
      offset,
    )
    let Pair(policy_id, asset_pairs) = decode_mint_policy_item_cbor(item_cbor)
    expect
      when previous_policy_id is {
        None -> True
        Some(previous) -> canonical_bytes_key_precedes(previous, policy_id)
      }
    let Pair(offset, rest) = decode_mint_policy_items_at(
      preimage_cbor,
      offset,
      remaining - 1,
      Some(policy_id),
    )
    Pair(
      offset,
      [Pair(builtin.b_data(policy_id), builtin.map_data(asset_pairs)), ..rest],
    )
  }
}

fn decode_mint_policy_item_cbor(
  item_cbor: ByteArray,
) -> Pair<ByteArray, List<Pair<Data, Data>>> {
  let offset = expect_byte(item_cbor, 0, 130)
  let Pair(offset, policy_id) = decode_canonical_bytes_at(item_cbor, offset)
  expect bytearray.length(policy_id) == 28
  let Pair(offset, asset_count) = decode_canonical_map_header_at(
    item_cbor,
    offset,
  )
  expect asset_count > 0
  let Pair(offset, asset_pairs) = decode_mint_assets_at(
    item_cbor,
    offset,
    asset_count,
    None,
  )
  expect offset == bytearray.length(item_cbor)
  Pair(policy_id, asset_pairs)
}

fn decode_mint_assets_at(
  item_cbor: ByteArray,
  offset: Int,
  remaining: Int,
  previous_asset_name: Option<ByteArray>,
) -> Pair<Int, List<Pair<Data, Data>>> {
  if remaining <= 0 {
    Pair(offset, [])
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
    let Pair(offset, rest) = decode_mint_assets_at(
      item_cbor,
      offset,
      remaining - 1,
      Some(asset_name),
    )
    Pair(
      offset,
      [Pair(builtin.b_data(asset_name), builtin.i_data(quantity)), ..rest],
    )
  }
}

pub fn decode_canonical_bytes_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, ByteArray> {
  let tag = byte_at(bytes, offset)
  if and {
    tag >= 64,
    tag <= 87,
  } {
    let length = tag - 64
    Pair(offset + 1 + length, slice_len(bytes, offset + 1, length))
  } else if tag == 88 {
    let length = byte_at(bytes, offset + 1)
    expect length >= 24
    Pair(offset + 2 + length, slice_len(bytes, offset + 2, length))
  } else if tag == 89 {
    let length = byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2)
    expect length > 0xff
    Pair(offset + 3 + length, slice_len(bytes, offset + 3, length))
  } else {
    expect tag == 90
    let length = (
      ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
        bytes,
        offset + 3,
      )
    ) * 256 + byte_at(bytes, offset + 4)
    expect length > 0xffff
    Pair(offset + 5 + length, slice_len(bytes, offset + 5, length))
  }
}

pub fn decode_canonical_map_header_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, Int> {
  let tag = byte_at(bytes, offset)
  if and {
    tag >= 160,
    tag <= 183,
  } {
    Pair(offset + 1, tag - 160)
  } else if tag == 184 {
    let item_count = byte_at(bytes, offset + 1)
    expect item_count >= 24
    Pair(offset + 2, item_count)
  } else if tag == 185 {
    let item_count = byte_at(bytes, offset + 1) * 256 + byte_at(
      bytes,
      offset + 2,
    )
    expect item_count > 0xff
    Pair(offset + 3, item_count)
  } else {
    expect tag == 186
    let item_count = (
      ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
        bytes,
        offset + 3,
      )
    ) * 256 + byte_at(bytes, offset + 4)
    expect item_count > 0xffff
    Pair(offset + 5, item_count)
  }
}

fn canonical_bytes_encoded_length(bytes: ByteArray) -> Int {
  let length = bytearray.length(bytes)
  if length < 24 {
    1 + length
  } else if length <= 0xff {
    2 + length
  } else if length <= 0xffff {
    3 + length
  } else {
    5 + length
  }
}

pub fn canonical_bytes_key_precedes(left: ByteArray, right: ByteArray) -> Bool {
  let left_length = canonical_bytes_encoded_length(left)
  let right_length = canonical_bytes_encoded_length(right)
  if left_length < right_length {
    True
  } else if left_length > right_length {
    False
  } else {
    bytearray.compare(left, right) == Less
  }
}

/// Checks the mint preimage against its flat field commitment without first
/// materialising the whole map as Plutus `Data`: the hash is taken over the
/// raw bytes, and the canonical-CBOR, ordering, non-empty-policy,
/// non-zero-quantity and width rules are then walked in place. This keeps the
/// independently revealed field inside the L1 execution-memory envelope.
pub fn verify_canonical_mint_preimage_cbor(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> Bool {
  expect field_commitment(preimage_cbor) == expected_hash
  let Pair(offset, item_count) = decode_field_array_header_at(
    preimage_cbor,
    0,
  )
  verify_canonical_mint_items_at(preimage_cbor, offset, item_count, None) == bytearray.length(
    preimage_cbor,
  )
}

fn verify_canonical_mint_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
  previous_policy_id: Option<ByteArray>,
) -> Int {
  if remaining <= 0 {
    offset
  } else {
    let Pair(offset, item_cbor) = decode_definite_bytes_at(
      preimage_cbor,
      offset,
    )
    let Pair(policy_id, _assets) = decode_mint_policy_item_cbor(item_cbor)
    expect
      when previous_policy_id is {
        None -> True
        Some(previous) -> canonical_bytes_key_precedes(previous, policy_id)
      }
    verify_canonical_mint_items_at(
      preimage_cbor,
      offset,
      remaining - 1,
      Some(policy_id),
    )
  }
}
