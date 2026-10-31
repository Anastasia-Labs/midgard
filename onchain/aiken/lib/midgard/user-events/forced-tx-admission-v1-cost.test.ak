//// Execution cost of the tx-order mint's forced-transaction admission rules
//// (`forced_tx_is_admissible_v1`) on the widest shapes they admit, read from
//// the check report. Each measured row has a fixture-only control that builds
//// the same views and runs no rule, so a row minus its control is the rules'
//// own cost. The door's cost for the same material is the tx-order mint
//// script budget's.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxWitnessSetCompact,
}
use midgard/native_tx_field_access_v1.{
  Whole, decode_field_array_header, encode_field_array_header, field_commitment,
  field_stride,
}
use midgard/user_events/tx_order_v1.{
  OrderMaterialViewsV1, forced_tx_is_admissible_v1,
}

const empty_null_hash: ByteArray =
  #"01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53"

const spend_input_item: ByteArray =
  #"5826825820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa190003"

const signer_item: ByteArray =
  #"581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const address_witness_item: ByteArray =
  #"5865825820dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd5840dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

/// The narrowest output each rule-5 path admits: two entries, the value running
/// to the item's end.
const two_entry_output_item: ByteArray = #"47a20040018200a0"

/// The narrowest output that makes the mint walk its value: three entries, an
/// empty value map and an empty inline datum.
const three_entry_output_item: ByteArray = #"49a30040018200a00240"

/// The narrowest native witness: the native tag and an empty payload.
const native_witness_item: ByteArray = #"43820040"

/// The narrowest mint item: one policy minting one unit of the empty name.
const mint_item: ByteArray =
  #"582282581ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeea14001"

/// The largest field 0 §5.4's aggregate bound admits beside the other minimal
/// non-empty fields of an admissible order.
const aggregate_bound_spend_input_count: Int = 813

fn repeated(block: ByteArray, times: Int) -> ByteArray {
  if times <= 0 {
    #""
  } else {
    let half = repeated(block, times / 2)
    let doubled = bytearray.concat(half, half)
    if times % 2 == 0 {
      doubled
    } else {
      bytearray.concat(doubled, block)
    }
  }
}

fn field(item: ByteArray, count: Int) -> ByteArray {
  bytearray.concat(encode_field_array_header(count), repeated(item, count))
}

/// An admissible order: every rule-checked field at one item, field 8 empty,
/// with `slot` widened to `filler`.
fn admissible_preimages(slot: Int, filler: ByteArray) -> List<ByteArray> {
  list.indexed_map(
    [
      field(spend_input_item, 1),
      field(spend_input_item, 1),
      field(two_entry_output_item, 1),
      field(signer_item, 1),
      field(signer_item, 1),
      field(mint_item, 1),
      field(native_witness_item, 1),
      field(address_witness_item, 1),
      #"80",
    ],
    fn(index, preimage) {
      if index == slot {
        filler
      } else {
        preimage
      }
    },
  )
}

fn views(preimages: List<ByteArray>) -> OrderMaterialViewsV1 {
  expect [p0, p1, p2, p3, p4, p5, p6, p7, p8] = preimages
  OrderMaterialViewsV1 {
    body: NativeTxBodyCompact {
      spend_inputs_hash: field_commitment(p0),
      reference_inputs_hash: field_commitment(p1),
      outputs_hash: field_commitment(p2),
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers_hash: field_commitment(p3),
      required_signers_hash: field_commitment(p4),
      mint_hash: field_commitment(p5),
      script_integrity_hash: empty_null_hash,
      auxiliary_data_hash: empty_null_hash,
      network_id: 0,
    },
    witness_set: NativeTxWitnessSetCompact {
      addr_tx_wits_hash: field_commitment(p7),
      script_tx_wits_hash: field_commitment(p6),
      redeemer_tx_wits_hash: field_commitment(p8),
    },
    fields: list.indexed_map(
      preimages,
      fn(index, preimage) {
        if preimage == #"80" {
          None
        } else {
          let Pair(_, count) = decode_field_array_header(preimage)
          Some(Whole { bytes: preimage, count, stride: field_stride(index) })
        }
      },
    ),
  }
}

/// `count` mint items, policy ids ascending, each minting one unit of the
/// empty name.
fn mint_policies(count: Int) -> ByteArray {
  bytearray.concat(
    encode_field_array_header(count),
    list.foldr(
      list.range(1, count),
      #"",
      fn(index, rest) {
        bytearray.concat(
          #"582282581c",
          bytearray.concat(
            bytearray.concat(
              bytearray.from_int_big_endian(index, 2),
              #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
            ),
            bytearray.concat(#"a14001", rest),
          ),
        )
      },
    ),
  )
}

/// One mint item of `count` assets, names the two-byte indices ascending.
fn mint_assets(count: Int) -> ByteArray {
  let assets = list.foldr(
    list.range(1, count),
    #"",
    fn(index, rest) {
      bytearray.concat(
        bytearray.concat(#"42", bytearray.from_int_big_endian(index, 2)),
        bytearray.concat(#"01", rest),
      )
    },
  )
  let map_head = if count < 256 {
    bytearray.concat(#"b8", bytearray.from_int_big_endian(count, 1))
  } else {
    bytearray.concat(#"b9", bytearray.from_int_big_endian(count, 2))
  }
  let item = bytearray.concat(
    #"82581ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
    bytearray.concat(map_head, assets),
  )
  bytearray.concat(
    encode_field_array_header(1),
    bytearray.concat(
      bytearray.concat(
        #"59",
        bytearray.from_int_big_endian(bytearray.length(item), 2),
      ),
      item,
    ),
  )
}

fn admitted(slot: Int, filler: ByteArray) -> Bool {
  forced_tx_is_admissible_v1(views(admissible_preimages(slot, filler)))
}

/// Builds the same views and runs no rule.
fn control(slot: Int, filler: ByteArray) -> Bool {
  list.length(views(admissible_preimages(slot, filler)).fields) == 9
}

test admission_minimal_order() {
  admitted(-1, #"")
}

test admission_minimal_order_control() {
  control(-1, #"")
}

test admission_at_the_aggregate_bound_spend_inputs() {
  admitted(0, field(spend_input_item, aggregate_bound_spend_input_count))
}

test admission_at_the_aggregate_bound_spend_inputs_control() {
  control(0, field(spend_input_item, aggregate_bound_spend_input_count))
}

test admission_at_250_two_entry_outputs() {
  admitted(2, field(two_entry_output_item, 250))
}

test admission_at_250_two_entry_outputs_control() {
  control(2, field(two_entry_output_item, 250))
}

test admission_at_500_two_entry_outputs() {
  admitted(2, field(two_entry_output_item, 500))
}

test admission_at_500_two_entry_outputs_control() {
  control(2, field(two_entry_output_item, 500))
}

test admission_at_250_three_entry_outputs() {
  admitted(2, field(three_entry_output_item, 250))
}

test admission_at_250_three_entry_outputs_control() {
  control(2, field(three_entry_output_item, 250))
}

test admission_at_500_three_entry_outputs() {
  admitted(2, field(three_entry_output_item, 500))
}

test admission_at_500_three_entry_outputs_control() {
  control(2, field(three_entry_output_item, 500))
}

test admission_at_250_native_witnesses() {
  admitted(6, field(native_witness_item, 250))
}

test admission_at_250_native_witnesses_control() {
  control(6, field(native_witness_item, 250))
}

test admission_at_500_native_witnesses() {
  admitted(6, field(native_witness_item, 500))
}

test admission_at_500_native_witnesses_control() {
  control(6, field(native_witness_item, 500))
}

test admission_at_the_aggregate_bound_address_witnesses() {
  admitted(7, field(address_witness_item, 316))
}

test admission_at_the_aggregate_bound_address_witnesses_control() {
  control(7, field(address_witness_item, 316))
}

test admission_at_250_mint_policies() {
  admitted(5, mint_policies(250))
}

test admission_at_250_mint_policies_control() {
  control(5, mint_policies(250))
}

test admission_at_500_mint_policies() {
  admitted(5, mint_policies(500))
}

test admission_at_500_mint_policies_control() {
  control(5, mint_policies(500))
}

test admission_at_500_mint_assets() {
  admitted(5, mint_assets(500))
}

test admission_at_500_mint_assets_control() {
  control(5, mint_assets(500))
}

test admission_at_1000_mint_assets() {
  admitted(5, mint_assets(1000))
}

test admission_at_1000_mint_assets_control() {
  control(5, mint_assets(1000))
}

// Output-value walking uses separate policy and asset iterations. Measure the
// widest byte-string and unsigned-quantity heads the output grammar supports;
// subtract each fixture-only control before taking the per-item slopes.
fn output_map_head(count: Int) -> ByteArray {
  if count < 24 {
    bytearray.from_int_big_endian(160 + count, 1)
  } else if count < 256 {
    bytearray.concat(#"b8", bytearray.from_int_big_endian(count, 1))
  } else {
    bytearray.concat(#"b9", bytearray.from_int_big_endian(count, 2))
  }
}

fn output_wide_asset(index: Int) -> ByteArray {
  bytearray.concat(
    #"5820eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
    bytearray.concat(
      bytearray.from_int_big_endian(index, 2),
      #"1bffffffffffffffff",
    ),
  )
}

fn output_wide_assets(count: Int) -> ByteArray {
  bytearray.concat(
    output_map_head(count),
    list.foldr(
      list.range(1, count),
      #"",
      fn(index, rest) { bytearray.concat(output_wide_asset(index), rest) },
    ),
  )
}

fn output_policies(policy_count: Int, asset_count: Int) -> ByteArray {
  let policies = bytearray.concat(
    output_map_head(policy_count),
    list.foldr(
      list.range(1, policy_count),
      #"",
      fn(index, rest) {
        bytearray.concat(
          #"581c",
          bytearray.concat(
            bytearray.concat(
              bytearray.from_int_big_endian(index, 2),
              #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
            ),
            bytearray.concat(output_wide_assets(asset_count), rest),
          ),
        )
      },
    ),
  )
  let item = bytearray.concat(
    #"a30040018200",
    bytearray.concat(policies, #"0240"),
  )
  field(
    bytearray.concat(
      #"59",
      bytearray.concat(
        bytearray.from_int_big_endian(bytearray.length(item), 2),
        item,
      ),
    ),
    1,
  )
}

test admission_output_at_20_policies() {
  admitted(2, output_policies(20, 1))
}

test admission_output_at_20_policies_control() {
  control(2, output_policies(20, 1))
}

test admission_output_at_40_policies() {
  admitted(2, output_policies(40, 1))
}

test admission_output_at_40_policies_control() {
  control(2, output_policies(40, 1))
}

test admission_output_at_50_assets() {
  admitted(2, output_policies(1, 50))
}

test admission_output_at_50_assets_control() {
  control(2, output_policies(1, 50))
}

test admission_output_at_100_assets() {
  admitted(2, output_policies(1, 100))
}

test admission_output_at_100_assets_control() {
  control(2, output_policies(1, 100))
}

// Controlled admitted mint widths: names and signed quantities.
fn measured_bytes(bytes: ByteArray) -> ByteArray {
  let size = bytearray.length(bytes)
  let head = if size < 24 {
    bytearray.from_int_big_endian(64 + size, 1)
  } else if size < 256 {
    bytearray.concat(#"58", bytearray.from_int_big_endian(size, 1))
  } else {
    bytearray.concat(#"59", bytearray.from_int_big_endian(size, 2))
  }
  bytearray.concat(head, bytes)
}

fn measured_name(index: Int, size: Int) -> ByteArray {
  measured_bytes(
    if size == 1 {
      bytearray.from_int_big_endian(index, 1)
    } else {
      bytearray.concat(
        repeated(#"ee", size - 2),
        bytearray.from_int_big_endian(index, 2),
      )
    },
  )
}

fn measured_mint_assets(
  count: Int,
  quantity: ByteArray,
  name_size: Int,
) -> ByteArray {
  let assets = list.foldr(
    list.range(1, count),
    #"",
    fn(index, rest) {
      bytearray.concat(
        measured_name(index, name_size),
        bytearray.concat(quantity, rest),
      )
    },
  )
  let asset_map = bytearray.concat(
    #"b8",
    bytearray.concat(bytearray.from_int_big_endian(count, 1), assets),
  )
  let item = bytearray.concat(
    #"82581ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
    asset_map,
  )
  bytearray.concat(encode_field_array_header(1), measured_bytes(item))
}

fn measured_mint_policies(
  count: Int,
  quantity: ByteArray,
  name_size: Int,
) -> ByteArray {
  let name = measured_bytes(repeated(#"ee", name_size))
  let items = list.foldr(
    list.range(1, count),
    #"",
    fn(index, rest) {
      let policy = bytearray.concat(
        #"581c",
        bytearray.concat(
          bytearray.from_int_big_endian(index, 2),
          repeated(#"ee", 26),
        ),
      )
      let item = bytearray.concat(
        #"82",
        bytearray.concat(
          policy,
          bytearray.concat(#"a1", bytearray.concat(name, quantity)),
        ),
      )
      bytearray.concat(measured_bytes(item), rest)
    },
  )
  bytearray.concat(encode_field_array_header(count), items)
}

test measured_assets_positive_direct_100() {
  admitted(5, measured_mint_assets(100, #"01", 2))
}

test measured_assets_positive_direct_100_control() {
  control(5, measured_mint_assets(100, #"01", 2))
}

test measured_assets_positive_direct_200() {
  admitted(5, measured_mint_assets(200, #"01", 2))
}

test measured_assets_positive_direct_200_control() {
  control(5, measured_mint_assets(200, #"01", 2))
}

test measured_assets_positive_1_100() {
  admitted(5, measured_mint_assets(100, #"18ff", 2))
}

test measured_assets_positive_1_100_control() {
  control(5, measured_mint_assets(100, #"18ff", 2))
}

test measured_assets_positive_1_200() {
  admitted(5, measured_mint_assets(200, #"18ff", 2))
}

test measured_assets_positive_1_200_control() {
  control(5, measured_mint_assets(200, #"18ff", 2))
}

test measured_assets_positive_2_100() {
  admitted(5, measured_mint_assets(100, #"19ffff", 2))
}

test measured_assets_positive_2_100_control() {
  control(5, measured_mint_assets(100, #"19ffff", 2))
}

test measured_assets_positive_2_200() {
  admitted(5, measured_mint_assets(200, #"19ffff", 2))
}

test measured_assets_positive_2_200_control() {
  control(5, measured_mint_assets(200, #"19ffff", 2))
}

test measured_assets_positive_4_100() {
  admitted(5, measured_mint_assets(100, #"1affffffff", 2))
}

test measured_assets_positive_4_100_control() {
  control(5, measured_mint_assets(100, #"1affffffff", 2))
}

test measured_assets_positive_4_200() {
  admitted(5, measured_mint_assets(200, #"1affffffff", 2))
}

test measured_assets_positive_4_200_control() {
  control(5, measured_mint_assets(200, #"1affffffff", 2))
}

test measured_assets_positive_8_100() {
  admitted(5, measured_mint_assets(100, #"1bffffffffffffffff", 2))
}

test measured_assets_positive_8_100_control() {
  control(5, measured_mint_assets(100, #"1bffffffffffffffff", 2))
}

test measured_assets_positive_8_200() {
  admitted(5, measured_mint_assets(200, #"1bffffffffffffffff", 2))
}

test measured_assets_positive_8_200_control() {
  control(5, measured_mint_assets(200, #"1bffffffffffffffff", 2))
}

test measured_assets_negative_direct_100() {
  admitted(5, measured_mint_assets(100, #"20", 2))
}

test measured_assets_negative_direct_100_control() {
  control(5, measured_mint_assets(100, #"20", 2))
}

test measured_assets_negative_direct_200() {
  admitted(5, measured_mint_assets(200, #"20", 2))
}

test measured_assets_negative_direct_200_control() {
  control(5, measured_mint_assets(200, #"20", 2))
}

test measured_assets_negative_1_100() {
  admitted(5, measured_mint_assets(100, #"38ff", 2))
}

test measured_assets_negative_1_100_control() {
  control(5, measured_mint_assets(100, #"38ff", 2))
}

test measured_assets_negative_1_200() {
  admitted(5, measured_mint_assets(200, #"38ff", 2))
}

test measured_assets_negative_1_200_control() {
  control(5, measured_mint_assets(200, #"38ff", 2))
}

test measured_assets_negative_2_100() {
  admitted(5, measured_mint_assets(100, #"39ffff", 2))
}

test measured_assets_negative_2_100_control() {
  control(5, measured_mint_assets(100, #"39ffff", 2))
}

test measured_assets_negative_2_200() {
  admitted(5, measured_mint_assets(200, #"39ffff", 2))
}

test measured_assets_negative_2_200_control() {
  control(5, measured_mint_assets(200, #"39ffff", 2))
}

test measured_assets_negative_4_100() {
  admitted(5, measured_mint_assets(100, #"3affffffff", 2))
}

test measured_assets_negative_4_100_control() {
  control(5, measured_mint_assets(100, #"3affffffff", 2))
}

test measured_assets_negative_4_200() {
  admitted(5, measured_mint_assets(200, #"3affffffff", 2))
}

test measured_assets_negative_4_200_control() {
  control(5, measured_mint_assets(200, #"3affffffff", 2))
}

test measured_assets_negative_8_100() {
  admitted(5, measured_mint_assets(100, #"3bffffffffffffffff", 2))
}

test measured_assets_negative_8_100_control() {
  control(5, measured_mint_assets(100, #"3bffffffffffffffff", 2))
}

test measured_assets_negative_8_200() {
  admitted(5, measured_mint_assets(200, #"3bffffffffffffffff", 2))
}

test measured_assets_negative_8_200_control() {
  control(5, measured_mint_assets(200, #"3bffffffffffffffff", 2))
}

test measured_assets_name_1_100() {
  admitted(5, measured_mint_assets(100, #"01", 1))
}

test measured_assets_name_1_100_control() {
  control(5, measured_mint_assets(100, #"01", 1))
}

test measured_assets_name_1_200() {
  admitted(5, measured_mint_assets(200, #"01", 1))
}

test measured_assets_name_1_200_control() {
  control(5, measured_mint_assets(200, #"01", 1))
}

test measured_assets_name_23_100() {
  admitted(5, measured_mint_assets(100, #"01", 23))
}

test measured_assets_name_23_100_control() {
  control(5, measured_mint_assets(100, #"01", 23))
}

test measured_assets_name_23_200() {
  admitted(5, measured_mint_assets(200, #"01", 23))
}

test measured_assets_name_23_200_control() {
  control(5, measured_mint_assets(200, #"01", 23))
}

test measured_assets_name_24_100() {
  admitted(5, measured_mint_assets(100, #"01", 24))
}

test measured_assets_name_24_100_control() {
  control(5, measured_mint_assets(100, #"01", 24))
}

test measured_assets_name_24_200() {
  admitted(5, measured_mint_assets(200, #"01", 24))
}

test measured_assets_name_24_200_control() {
  control(5, measured_mint_assets(200, #"01", 24))
}

test measured_assets_name_32_100() {
  admitted(5, measured_mint_assets(100, #"01", 32))
}

test measured_assets_name_32_100_control() {
  control(5, measured_mint_assets(100, #"01", 32))
}

test measured_assets_name_32_200() {
  admitted(5, measured_mint_assets(200, #"01", 32))
}

test measured_assets_name_32_200_control() {
  control(5, measured_mint_assets(200, #"01", 32))
}

test measured_policies_baseline_50() {
  admitted(5, measured_mint_policies(50, #"01", 0))
}

test measured_policies_baseline_50_control() {
  control(5, measured_mint_policies(50, #"01", 0))
}

test measured_policies_baseline_100() {
  admitted(5, measured_mint_policies(100, #"01", 0))
}

test measured_policies_baseline_100_control() {
  control(5, measured_mint_policies(100, #"01", 0))
}

test measured_policies_name_32_50() {
  admitted(5, measured_mint_policies(50, #"01", 32))
}

test measured_policies_name_32_50_control() {
  control(5, measured_mint_policies(50, #"01", 32))
}

test measured_policies_name_32_100() {
  admitted(5, measured_mint_policies(100, #"01", 32))
}

test measured_policies_name_32_100_control() {
  control(5, measured_mint_policies(100, #"01", 32))
}

test measured_policies_maximum_50() {
  admitted(5, measured_mint_policies(50, #"3bffffffffffffffff", 32))
}

test measured_policies_maximum_50_control() {
  control(5, measured_mint_policies(50, #"3bffffffffffffffff", 32))
}

test measured_policies_maximum_100() {
  admitted(5, measured_mint_policies(100, #"3bffffffffffffffff", 32))
}

test measured_policies_maximum_100_control() {
  control(5, measured_mint_policies(100, #"3bffffffffffffffff", 32))
}

test measured_assets_maximum_100() {
  admitted(5, measured_mint_assets(100, #"3bffffffffffffffff", 32))
}

test measured_assets_maximum_100_control() {
  control(5, measured_mint_assets(100, #"3bffffffffffffffff", 32))
}

test measured_assets_maximum_200() {
  admitted(5, measured_mint_assets(200, #"3bffffffffffffffff", 32))
}

test measured_assets_maximum_200_control() {
  control(5, measured_mint_assets(200, #"3bffffffffffffffff", 32))
}
