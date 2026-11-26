use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/ledger_output_commitment_v1

// `decode` and `decode_canonical_schema` accept the same descriptors and
// refuse the same malformed wire.

fn descriptor_fixture() -> ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
  ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: 7,
    total_length: 5000,
    item_commitment: #"13e167684e9dc284acc6ebbe972cd2cf0763d03bba558bae463825f3f35990d6",
    address: #"6011111111111111111111111111111111111111111111111111111111",
    lovelace: 5000000,
    asset_count: 0,
    asset_frontier_commitment: #"b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd",
    cardano_value_size: 5,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out: cek_data_v1.DataSummaryV1 {
      root: #"2222222222222222222222222222222222222222222222222222222222222222",
      cbor_length: 101,
      memory: 202,
    },
    midgard_tx_out: cek_data_v1.DataSummaryV1 {
      root: #"3333333333333333333333333333333333333333333333333333333333333333",
      cbor_length: 103,
      memory: 204,
    },
    cardano_spend_datum: cek_data_v1.DataSummaryV1 {
      root: #"4444444444444444444444444444444444444444444444444444444444444444",
      cbor_length: 3,
      memory: 4,
    },
  }
}

const wire =
  #"900107191388582013e167684e9dc284acc6ebbe972cd2cf0763d03bba558bae463825f3f35990d6581d60111111111111111111111111111111111111111111111111111111111a004c4b40005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358202222222222222222222222222222222222222222222222222222222222222222186518ca8358203333333333333333333333333333333333333333333333333333333333333333186718cc83582044444444444444444444444444444444444444444444444444444444444444440304"

test decoder_preserves_unsigned_head_widths_and_complete_record() {
  list.all(
    [
      0, 23, 24, 255, 256, 65535, 65536, 4294967295, 4294967296,
      18446744073709551615,
    ],
    fn(value) {
      let fixture = descriptor_fixture()
      let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
        ..fixture,
        output_index: 65535,
        total_length: value,
        lovelace: value,
        asset_count: 4294967295,
        cardano_value_size: 5000,
        cardano_tx_out: cek_data_v1.DataSummaryV1 {
          ..fixture.cardano_tx_out,
          cbor_length: value,
          memory: value,
        },
      }
      let bytes = ledger_output_commitment_v1.encode(descriptor)
      and {
        ledger_output_commitment_v1.decode(bytes) == descriptor,
        ledger_output_commitment_v1.decode(bytes) == descriptor,
        ledger_output_commitment_v1.decode_canonical_schema(bytes) == descriptor,
      }
    },
  )
}

test decoder_preserves_reference_script_languages_and_lengths() {
  list.all(
    [
      (0, 1), (0, 16384), (3, 1), (3, 18446744073709551615), (128, 1),
      (128, 18446744073709551615),
    ],
    fn(pair) {
      let (language, length) = pair
      let fixture = descriptor_fixture()
      let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
        ..fixture,
        address: #"001111111111111111111111111111111111111111111111111111111122222222222222222222222222222222222222222222222222222222",
        reference_script_language: language,
        reference_script_hash: builtin.replicate_byte(28, 55),
        reference_script_total_length: length,
        reference_script_item_commitment: builtin.replicate_byte(32, 77),
      }
      let bytes = ledger_output_commitment_v1.encode(descriptor)
      and {
        ledger_output_commitment_v1.decode(bytes) == descriptor,
        ledger_output_commitment_v1.decode(bytes) == descriptor,
        ledger_output_commitment_v1.decode_canonical_schema(bytes) == descriptor,
      }
    },
  )
}

fn replace_field(index: Int, replacement: ByteArray) -> ByteArray {
  let (start, end) = when index is {
    0 -> (1, 2)
    1 -> (2, 3)
    2 -> (3, 6)
    3 -> (6, 40)
    4 -> (40, 71)
    5 -> (71, 76)
    6 -> (76, 77)
    7 -> (77, 111)
    8 -> (111, 112)
    9 -> (112, 113)
    10 -> (113, 114)
    11 -> (114, 115)
    12 -> (115, 116)
    13 -> (116, 155)
    14 -> (155, 194)
    15 -> (194, 231)
    _ -> fail
  }
  bytearray.concat(
    bytearray.concat(builtin.slice_bytearray(0, start, wire), replacement),
    builtin.slice_bytearray(end, bytearray.length(wire) - end, wire),
  )
}

test decode_rejects_version() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(0, #"02"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_version() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(0, #"02"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_output_index() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(1, #"1a00010000"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_output_index() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(1, #"1a00010000"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_negative_total_length() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(2, #"20"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_negative_total_length() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(2, #"20"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_item_hash_width() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(3, #"40"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_item_hash_width() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(3, #"40"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_address() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(4, #"4100"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_address() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(4, #"4100"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_lovelace_negative() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(5, #"20"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_lovelace_negative() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(5, #"20"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_asset_count() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(6, #"1b0000000100000000"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_asset_count() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(6, #"1b0000000100000000"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_frontier_hash_width() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(7, #"40"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_frontier_hash_width() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(7, #"40"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_value_size() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(8, #"191389"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_value_size() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(8, #"191389"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_language_negative_two() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(9, #"21"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_language_negative_two() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(9, #"21"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_language_unsupported() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(9, #"01"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_language_unsupported() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(9, #"01"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_missing_reference_hash() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(9, #"00"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_missing_reference_hash() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(9, #"00"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_none_reference_hash() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(10, #"4100"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_none_reference_hash() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(10, #"4100"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_none_reference_length() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(11, #"01"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_none_reference_length() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(11, #"01"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_none_reference_commitment() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(12, #"4100"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_none_reference_commitment() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(12, #"4100"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_summary_count() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(13, #"824001"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_summary_count() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(13, #"824001"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_summary_hash_width() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(13, #"83400101"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_summary_hash_width() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(13, #"83400101"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_summary_negative() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(
      13,
      #"835820"
        |> bytearray.concat(builtin.replicate_byte(32, 22))
        |> bytearray.concat(#"2001"),
    ),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_summary_negative() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(
      13,
      #"835820"
        |> bytearray.concat(builtin.replicate_byte(32, 22))
        |> bytearray.concat(#"2001"),
    ),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_bignum_total_length() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(2, #"c249010000000000000000"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_bignum_reference_length() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(11, #"c249010000000000000000"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_nonminimal_integer() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(1, #"1807"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_nonminimal_integer() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(1, #"1807"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_wrong_field_type() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(1, #"40"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_wrong_field_type() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(1, #"40"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_nonminimal_bytes() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(
      3,
      #"590020" |> bytearray.concat(builtin.replicate_byte(32, 22)),
    ),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_nonminimal_bytes() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(
      3,
      #"590020" |> bytearray.concat(builtin.replicate_byte(32, 22)),
    ),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_indefinite_bytes() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    replace_field(
      3,
      #"5f5820"
        |> bytearray.concat(builtin.replicate_byte(32, 22))
        |> bytearray.concat(#"ff"),
    ),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_indefinite_bytes() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    replace_field(
      3,
      #"5f5820"
        |> bytearray.concat(builtin.replicate_byte(32, 22))
        |> bytearray.concat(#"ff"),
    ),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_indefinite_array() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    #"9f"
      |> bytearray.concat(
          builtin.slice_bytearray(1, bytearray.length(wire) - 1, wire),
        )
      |> bytearray.concat(#"ff"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_indefinite_array() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    #"9f"
      |> bytearray.concat(
          builtin.slice_bytearray(1, bytearray.length(wire) - 1, wire),
        )
      |> bytearray.concat(#"ff"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_nonminimal_array() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    #"9810"
      |> bytearray.concat(
          builtin.slice_bytearray(1, bytearray.length(wire) - 1, wire),
        ),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_nonminimal_array() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    #"9810"
      |> bytearray.concat(
          builtin.slice_bytearray(1, bytearray.length(wire) - 1, wire),
        ),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_wrong_array_count() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    #"8f"
      |> bytearray.concat(
          builtin.slice_bytearray(1, bytearray.length(wire) - 1, wire),
        ),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_wrong_array_count() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    #"8f"
      |> bytearray.concat(
          builtin.slice_bytearray(1, bytearray.length(wire) - 1, wire),
        ),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_trailing_bytes() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    bytearray.concat(wire, #"00"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_trailing_bytes() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    bytearray.concat(wire, #"00"),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_truncated() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    builtin.slice_bytearray(0, bytearray.length(wire) - 1, wire),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_truncated() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    builtin.slice_bytearray(0, bytearray.length(wire) - 1, wire),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

fn native_reference_wire(length: Int) -> ByteArray {
  let prefix = builtin.slice_bytearray(0, 112, wire)
  let reference = #"00581c"
    |> bytearray.concat(builtin.replicate_byte(28, 55))
    |> bytearray.concat(cbor.serialise(length))
    |> bytearray.concat(#"5820")
    |> bytearray.concat(builtin.replicate_byte(32, 77))
  prefix
    |> bytearray.concat(reference)
    |> bytearray.concat(
        builtin.slice_bytearray(116, bytearray.length(wire) - 116, wire),
      )
}

test decode_rejects_zero_native_reference_length() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    native_reference_wire(0),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_zero_native_reference_length() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    native_reference_wire(0),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test decode_rejects_oversized_native_reference() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode(
    native_reference_wire(16385),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}

test canonical_schema_rejects_oversized_native_reference() fail {
  let descriptor: Data = ledger_output_commitment_v1.decode_canonical_schema(
    native_reference_wire(16385),
  )
  bytearray.length(builtin.serialise_data(descriptor)) > 0
}
