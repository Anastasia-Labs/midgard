use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{encode_definite_array_header}

pub const version: Int = 1

pub const stage_ready: Int = 0

pub const stage_round: Int = 1

pub const stage_finish: Int = 2

pub const stage_terminal: Int = 3

pub const block_bytes: Int = 128

pub const round_count: Int = 12

pub const digest_bytes: Int = 28

const word_modulus: Int = 18_446_744_073_709_551_616

const word_mask: Int = 18_446_744_073_709_551_615

const parameter_block_v1: Int = 16_842_780

const iv_le =
  #"08c9bcf367e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b"

const sigma_rows =
  [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3],
    [11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4],
    [7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8],
    [9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13],
    [2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9],
    [12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11],
    [13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10],
    [6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5],
    [10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0],
  ]

pub type Blake2b224TraceControlV1 {
  version: Int,
  stage: Int,
  cursor: Int,
  total_length: Int,
  chaining_value: ByteArray,
  active_block: ByteArray,
  active_block_length: Int,
  working_value: ByteArray,
  round: Int,
}

fn word_le(value: Int) -> ByteArray {
  builtin.integer_to_bytearray(False, 8, value % word_modulus)
}

fn word_at_le(bytes: ByteArray, index: Int) -> Int {
  bytes
    |> builtin.slice_bytearray(index * 8, 8, _)
    |> builtin.bytearray_to_integer(False, _)
}

fn replace_word_le(bytes: ByteArray, index: Int, value: Int) -> ByteArray {
  let start = index * 8
  let suffix_start = start + 8
  builtin.slice_bytearray(0, start, bytes)
    |> bytearray.concat(word_le(value))
    |> bytearray.concat(
        builtin.slice_bytearray(
          suffix_start,
          bytearray.length(bytes) - suffix_start,
          bytes,
        ),
      )
}

fn add64(first: Int, second: Int, third: Int) -> Int {
  ( first + second + third ) % word_modulus
}

fn xor64(first: Int, second: Int) -> Int {
  builtin.xor_bytearray(False, word_le(first), word_le(second))
    |> builtin.bytearray_to_integer(False, _)
}

fn rotate_right64(value: Int, bits: Int) -> Int {
  let divisor = if bits == 32 {
    4_294_967_296
  } else if bits == 24 {
    16_777_216
  } else if bits == 16 {
    65_536
  } else {
    expect bits == 63
    9_223_372_036_854_775_808
  }
  let multiplier = if bits == 32 {
    4_294_967_296
  } else if bits == 24 {
    1_099_511_627_776
  } else if bits == 16 {
    281_474_976_710_656
  } else {
    2
  }
  value / divisor + value % divisor * multiplier
}

fn mix(
  words: ByteArray,
  a: Int,
  b: Int,
  c: Int,
  d: Int,
  x: Int,
  y: Int,
) -> ByteArray {
  let first_a = add64(word_at_le(words, a), word_at_le(words, b), x)
  let first_d = rotate_right64(xor64(word_at_le(words, d), first_a), 32)
  let first_c = add64(word_at_le(words, c), first_d, 0)
  let first_b = rotate_right64(xor64(word_at_le(words, b), first_c), 24)
  let second_a = add64(first_a, first_b, y)
  let second_d = rotate_right64(xor64(first_d, second_a), 16)
  let second_c = add64(first_c, second_d, 0)
  let second_b = rotate_right64(xor64(first_b, second_c), 63)
  words
    |> replace_word_le(a, second_a)
    |> replace_word_le(b, second_b)
    |> replace_word_le(c, second_c)
    |> replace_word_le(d, second_d)
}

fn sigma_word(row: List<Int>, index: Int) -> Int {
  expect Some(word) = list.at(row, index)
  word
}

fn apply_round(
  words: ByteArray,
  message: ByteArray,
  row: List<Int>,
) -> ByteArray {
  words
    |> mix(
        0,
        4,
        8,
        12,
        word_at_le(message, sigma_word(row, 0)),
        word_at_le(message, sigma_word(row, 1)),
      )
    |> mix(
        1,
        5,
        9,
        13,
        word_at_le(message, sigma_word(row, 2)),
        word_at_le(message, sigma_word(row, 3)),
      )
    |> mix(
        2,
        6,
        10,
        14,
        word_at_le(message, sigma_word(row, 4)),
        word_at_le(message, sigma_word(row, 5)),
      )
    |> mix(
        3,
        7,
        11,
        15,
        word_at_le(message, sigma_word(row, 6)),
        word_at_le(message, sigma_word(row, 7)),
      )
    |> mix(
        0,
        5,
        10,
        15,
        word_at_le(message, sigma_word(row, 8)),
        word_at_le(message, sigma_word(row, 9)),
      )
    |> mix(
        1,
        6,
        11,
        12,
        word_at_le(message, sigma_word(row, 10)),
        word_at_le(message, sigma_word(row, 11)),
      )
    |> mix(
        2,
        7,
        8,
        13,
        word_at_le(message, sigma_word(row, 12)),
        word_at_le(message, sigma_word(row, 13)),
      )
    |> mix(
        3,
        4,
        9,
        14,
        word_at_le(message, sigma_word(row, 14)),
        word_at_le(message, sigma_word(row, 15)),
      )
}

fn fold_chaining_value(
  chaining_value: ByteArray,
  words: ByteArray,
  index: Int,
) -> ByteArray {
  if index == 8 {
    #""
  } else {
    word_le(
      xor64(
        xor64(word_at_le(chaining_value, index), word_at_le(words, index)),
        word_at_le(words, index + 8),
      ),
    )
      |> bytearray.concat(fold_chaining_value(chaining_value, words, index + 1))
  }
}

fn initial_chaining_value_v1() -> ByteArray {
  iv_le
    |> replace_word_le(0, xor64(word_at_le(iv_le, 0), parameter_block_v1))
}

fn initialize_working_value(
  chaining_value: ByteArray,
  bytes_compressed: Int,
  final: Bool,
) -> ByteArray {
  let initial_words = chaining_value |> bytearray.concat(iv_le)
  let with_low_counter = initial_words
    |> replace_word_le(
        12,
        xor64(word_at_le(initial_words, 12), bytes_compressed % word_modulus),
      )
  let with_counter = with_low_counter
    |> replace_word_le(
        13,
        xor64(word_at_le(with_low_counter, 13), bytes_compressed / word_modulus),
      )
  if final {
    with_counter
      |> replace_word_le(14, xor64(word_at_le(with_counter, 14), word_mask))
  } else {
    with_counter
  }
}

fn expected_active_block_length(control: Blake2b224TraceControlV1) -> Int {
  let remaining = control.total_length - control.cursor
  if remaining < block_bytes {
    remaining
  } else {
    block_bytes
  }
}

fn active_state_is_empty(control: Blake2b224TraceControlV1) -> Bool {
  and {
    control.active_block == #"",
    control.active_block_length == 0,
    control.working_value == #"",
    control.round == 0,
  }
}

fn active_block_padding_is_zero(control: Blake2b224TraceControlV1) -> Bool {
  if control.active_block_length == block_bytes {
    True
  } else {
    let padding_length = block_bytes - control.active_block_length
    builtin.slice_bytearray(
      control.active_block_length,
      padding_length,
      control.active_block,
    ) == builtin.replicate_byte(padding_length, 0)
  }
}

pub fn control_is_well_formed(control: Blake2b224TraceControlV1) -> Bool {
  and {
    control.version == version,
    control.stage >= stage_ready,
    control.stage <= stage_terminal,
    control.cursor >= 0,
    control.total_length > 0,
    control.cursor <= control.total_length,
    bytearray.length(control.chaining_value) == 64,
    if control.cursor == 0 {
      control.chaining_value == initial_chaining_value_v1()
    } else {
      True
    },
    if control.stage == stage_ready {
      and {
        control.cursor < control.total_length,
        control.cursor % block_bytes == 0,
        active_state_is_empty(control),
      }
    } else if control.stage == stage_terminal {
      and {
        control.cursor == control.total_length,
        active_state_is_empty(control),
      }
    } else {
      and {
        control.cursor < control.total_length,
        control.cursor % block_bytes == 0,
        bytearray.length(control.active_block) == block_bytes,
        control.active_block_length == expected_active_block_length(control),
        active_block_padding_is_zero(control),
        bytearray.length(control.working_value) == 128,
        if control.stage == stage_round {
          and {
            control.round >= 0,
            control.round < round_count,
          }
        } else {
          control.round == round_count
        },
      }
    },
  }
}

pub fn initial_control_v1(total_length: Int) -> Blake2b224TraceControlV1 {
  let control = Blake2b224TraceControlV1 {
    version,
    stage: stage_ready,
    cursor: 0,
    total_length,
    chaining_value: initial_chaining_value_v1(),
    active_block: #"",
    active_block_length: 0,
    working_value: #"",
    round: 0,
  }
  expect control_is_well_formed(control)
  control
}

pub fn encode_control_v1(control: Blake2b224TraceControlV1) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(9)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.cursor))
    |> bytearray.concat(cbor.serialise(control.total_length))
    |> bytearray.concat(cbor.serialise(builtin.b_data(control.chaining_value)))
    |> bytearray.concat(cbor.serialise(builtin.b_data(control.active_block)))
    |> bytearray.concat(cbor.serialise(control.active_block_length))
    |> bytearray.concat(cbor.serialise(builtin.b_data(control.working_value)))
    |> bytearray.concat(cbor.serialise(control.round))
}

pub fn control_from_data_v1(data: Data) -> Blake2b224TraceControlV1 {
  expect
      [
        version_data, stage_data, cursor_data, total_length_data,
        chaining_value_data, active_block_data, active_block_length_data,
        working_value_data, round_data,
      ]
    = builtin.un_list_data(data)
  let control = Blake2b224TraceControlV1 {
    version: builtin.un_i_data(version_data),
    stage: builtin.un_i_data(stage_data),
    cursor: builtin.un_i_data(cursor_data),
    total_length: builtin.un_i_data(total_length_data),
    chaining_value: builtin.un_b_data(chaining_value_data),
    active_block: builtin.un_b_data(active_block_data),
    active_block_length: builtin.un_i_data(active_block_length_data),
    working_value: builtin.un_b_data(working_value_data),
    round: builtin.un_i_data(round_data),
  }
  expect control_is_well_formed(control)
  control
}

pub fn decode_control_v1(control_cbor: ByteArray) -> Blake2b224TraceControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  let control = control_from_data_v1(data)
  expect encode_control_v1(control) == control_cbor
  control
}

fn begin_block_v1(
  control: Blake2b224TraceControlV1,
  block: ByteArray,
) -> Option<Blake2b224TraceControlV1> {
  let expected_length = expected_active_block_length(control)
  if bytearray.length(block) != expected_length {
    None
  } else {
    let bytes_compressed = control.cursor + expected_length
    let padded_block = block
      |> bytearray.concat(
          builtin.replicate_byte(block_bytes - expected_length, 0),
        )
    Some(
      Blake2b224TraceControlV1 {
        ..control,
        stage: stage_round,
        active_block: padded_block,
        active_block_length: expected_length,
        working_value: initialize_working_value(
          control.chaining_value,
          bytes_compressed,
          bytes_compressed == control.total_length,
        ),
        round: 0,
      },
    )
  }
}

fn round_v1(control: Blake2b224TraceControlV1) -> Blake2b224TraceControlV1 {
  expect Some(row) = list.at(sigma_rows, control.round % 10)
  let next_round = control.round + 1
  Blake2b224TraceControlV1 {
    ..control,
    stage: if next_round == round_count {
      stage_finish
    } else {
      stage_round
    },
    working_value: apply_round(control.working_value, control.active_block, row),
    round: next_round,
  }
}

fn finish_block_v1(
  control: Blake2b224TraceControlV1,
) -> Blake2b224TraceControlV1 {
  let cursor = control.cursor + control.active_block_length
  Blake2b224TraceControlV1 {
    ..control,
    stage: if cursor == control.total_length {
      stage_terminal
    } else {
      stage_ready
    },
    cursor,
    chaining_value: fold_chaining_value(
      control.chaining_value,
      control.working_value,
      0,
    ),
    active_block: #"",
    active_block_length: 0,
    working_value: #"",
    round: 0,
  }
}

pub fn step_v1(
  control: Blake2b224TraceControlV1,
  block: Option<ByteArray>,
) -> Option<Blake2b224TraceControlV1> {
  if !control_is_well_formed(control) {
    None
  } else {
    let result = if control.stage == stage_ready {
      when block is {
        Some(bytes) -> begin_block_v1(control, bytes)
        None -> None
      }
    } else if block != None {
      None
    } else if control.stage == stage_round {
      Some(round_v1(control))
    } else if control.stage == stage_finish {
      Some(finish_block_v1(control))
    } else {
      None
    }
    when result is {
      Some(next) ->
        if control_is_well_formed(next) {
          Some(next)
        } else {
          None
        }
      None -> None
    }
  }
}

pub fn digest_v1(control: Blake2b224TraceControlV1) -> Option<ByteArray> {
  if and {
    control_is_well_formed(control),
    control.stage == stage_terminal,
  } {
    Some(builtin.slice_bytearray(0, digest_bytes, control.chaining_value))
  } else {
    None
  }
}
