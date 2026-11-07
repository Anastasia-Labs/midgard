use aiken/builtin
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/script_decode_bits_v1.{
  empty_sink, mag_finish, mag_new, mag_push, next_set_bit, read_bits,
  sink_length, sink_push, sink_root, utf8_push,
}

fn pattern(length: Int) -> ByteArray {
  if length == 0 {
    #""
  } else {
    builtin.cons_bytearray(length % 251, pattern(length - 1))
  }
}

fn pushed_in(bytes: ByteArray, step: Int) -> script_decode_bits_v1.BlobSink {
  push_pieces(empty_sink(), bytes, step)
}

fn push_pieces(
  sink: script_decode_bits_v1.BlobSink,
  bytes: ByteArray,
  step: Int,
) -> script_decode_bits_v1.BlobSink {
  if bytearray.length(bytes) <= step {
    sink_push(sink, bytes)
  } else {
    push_pieces(
      sink_push(sink, bytearray.take(bytes, step)),
      bytearray.drop(bytes, step),
      step,
    )
  }
}

fn sink_matches(length: Int, step: Int) -> Bool {
  let bytes = pattern(length)
  let sink = pushed_in(bytes, step)
  and {
    sink_root(sink) == cek_data_v1.bounded_blob_root_v1(bytes),
    sink_length(sink) == length,
  }
}

test script_decode_bits_sink_root_matches_bounded_root() {
  and {
    sink_matches(0, 1),
    sink_matches(1, 1),
    sink_matches(255, 7),
    sink_matches(4095, 255),
    sink_matches(4096, 255),
    sink_matches(8190, 1000),
    sink_matches(8191, 4095),
    sink_matches(12285, 255),
    sink_matches(12286, 255),
  }
}

fn magnitude_pushed(negative: Bool, bytes: ByteArray, step: Int) {
  mag_pieces(mag_new(negative), bytes, step)
}

fn mag_pieces(
  mag: script_decode_bits_v1.MagSink,
  bytes: ByteArray,
  step: Int,
) -> script_decode_bits_v1.MagSink {
  if bytearray.length(bytes) <= step {
    mag_push(mag, bytes)
  } else {
    mag_pieces(
      mag_push(mag, bytearray.take(bytes, step)),
      bytearray.drop(bytes, step),
      step,
    )
  }
}

fn magnitude_matches(negative: Bool, bytes: ByteArray, step: Int) -> Bool {
  let m = if bytearray.length(bytes) == 0 {
    0
  } else {
    builtin.bytearray_to_integer(True, bytes)
  }
  let value = if negative {
    -1 - m
  } else {
    m
  }
  let (summary, words) = mag_finish(magnitude_pushed(negative, bytes, step))
  and {
    summary == cek_data_v1.semantic_data_summary_v1(builtin.i_data(value)),
    words == cek_data_v1.integer_memory_size_v1(value),
  }
}

test script_decode_bits_magnitude_matches_semantic_integer() {
  let ones = builtin.replicate_byte(200, 255)
  and {
    magnitude_matches(False, #"", 1),
    magnitude_matches(True, #"", 1),
    magnitude_matches(False, #"0000", 1),
    magnitude_matches(True, #"00ff", 1),
    magnitude_matches(False, pattern(64), 7),
    magnitude_matches(True, pattern(64), 64),
    magnitude_matches(False, bytearray.concat(#"000000", pattern(65)), 3),
    magnitude_matches(True, pattern(65), 64),
    magnitude_matches(False, pattern(128), 64),
    magnitude_matches(True, pattern(130), 17),
    magnitude_matches(False, pattern(700), 255),
    magnitude_matches(True, ones, 33),
    magnitude_matches(True, bytearray.concat(#"7f", ones), 64),
    magnitude_matches(True, bytearray.concat(#"01", ones), 9),
    magnitude_matches(False, ones, 50),
    magnitude_matches(True, builtin.replicate_byte(64, 255), 64),
    magnitude_matches(True, builtin.replicate_byte(8, 255), 3),
  }
}

test script_decode_bits_utf8_classifies() {
  and {
    utf8_push(0, #"") == 0,
    utf8_push(0, #"48656c6c6f") == 0,
    utf8_push(0, #"c3a9") == 0,
    utf8_push(utf8_push(0, #"e282"), #"ac") == 0,
    utf8_push(0, #"f09f9880") == 0,
    utf8_push(0, #"f48fbfbf") == 0,
    utf8_push(0, #"e282") > 0,
    utf8_push(0, #"c0af") == -1,
    utf8_push(0, #"e080af") == -1,
    utf8_push(0, #"eda080") == -1,
    utf8_push(0, #"f4908080") == -1,
    utf8_push(0, #"f5") == -1,
    utf8_push(0, #"80") == -1,
    utf8_push(utf8_push(0, #"ff"), #"41") == -1,
  }
}

test script_decode_bits_reads_bits_and_filler() {
  let window = #"a5c30100000100"
  and {
    read_bits(window, 0, 4) == 10,
    read_bits(window, 4, 8) == 92,
    read_bits(window, 13, 3) == 3,
    read_bits(window, 15, 1) == 1,
    next_set_bit(window, 0, 6) == Some(0),
    next_set_bit(window, 1, 6) == Some(2),
    next_set_bit(window, 16, 6) == Some(23),
    next_set_bit(window, 24, 6) == Some(47),
    next_set_bit(window, 48, 6) == None,
    next_set_bit(#"000000000000000000", 0, 8) == None,
    next_set_bit(#"00000000000000000200", 3, 9) == Some(70),
  }
}
