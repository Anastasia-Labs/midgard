use aiken/builtin
use midgard/validation_machine/witness

test bitmap_zero_has_unique_empty_encoding() {
  and {
    witness.encode_script_discovery_bitmap(0) == #"",
    witness.decode_script_discovery_bitmap(#"") == 0,
    witness.encode_script_discovery_bitmap(256) == #"0100",
  }
}

test bitmap_maximum_roundtrip() {
  let bytes = builtin.replicate_byte(2048, 255)
  let value = builtin.bytearray_to_integer(True, bytes)
  and {
    witness.encode_script_discovery_bitmap(value) == bytes,
    witness.decode_script_discovery_bitmap(bytes) == value,
  }
}

test bitmap_above_uint64_roundtrip() {
  let bytes = #"100000000000000000"
  let value = builtin.bytearray_to_integer(True, bytes)
  and {
    witness.encode_script_discovery_bitmap(value) == bytes,
    witness.decode_script_discovery_bitmap(bytes) == value,
  }
}

test bitmap_leading_zero_refused() fail {
  witness.decode_script_discovery_bitmap(#"0001") == 1
}

test bitmap_zero_byte_refused() fail {
  witness.decode_script_discovery_bitmap(#"00") == 0
}

test bitmap_oversized_decode_refused() fail {
  witness.decode_script_discovery_bitmap(builtin.replicate_byte(2049, 1)) == 1
}

test bitmap_oversized_encode_refused() fail {
  witness.encode_script_discovery_bitmap(
    builtin.bytearray_to_integer(True, builtin.replicate_byte(2049, 1)),
  ) == #""
}

test bitmap_negative_refused() fail {
  witness.encode_script_discovery_bitmap(-1) == #""
}
