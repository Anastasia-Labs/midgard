use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/cek_staged_blob_v1.{flat_root_of_pieces_v1, flat_root_v1}
use midgard/data_serialise_walk_v1.{
  Advanced, Folding, Item, Leaf, ListItems, Parent, Refused, Root, append_chunks,
  bytes_leaf, empty_frontier, frontier_root, run,
}
use midgard/validation_machine_tests/data_serialise_driver.{
  whole_v1, windows_v1,
}

// ## Drivers

/// The windowings every agreement row is read under: whole, one step per
/// 68-byte window (the smallest window every step fits in), a few steps per
/// short window, and many steps per long window.
fn readings(bytes: ByteArray) -> List<Option<DataSummaryV1>> {
  [
    whole_v1(bytes),
    windows_v1(bytes, 68, 1),
    windows_v1(bytes, 100, 3),
    windows_v1(bytes, 1000, 7),
    windows_v1(bytes, 9000, 1_000),
  ]
}

fn agrees(data: Data) -> Bool {
  let expected = Some(cek_data_v1.semantic_data_summary_v1(data))
  list.all(readings(cbor.serialise(data)), fn(read) { read == expected })
}

fn refused(bytes: ByteArray) -> Bool {
  list.all(readings(bytes), fn(read) { read == None })
}

fn int(value: Int) -> Data {
  builtin.i_data(value)
}

fn block(length: Int) -> ByteArray {
  list.foldl(
    list.range(1, length),
    #"",
    fn(i, acc) { bytearray.push(acc, i % 256) },
  )
}

fn bytes_of(length: Int) -> Data {
  builtin.b_data(block(length))
}

fn nested(depth: Int) -> Data {
  if depth == 0 {
    int(0)
  } else {
    builtin.list_data([nested(depth - 1)])
  }
}

fn flat(count: Int) -> Data {
  builtin.list_data(list.map(list.range(1, count), fn(_) { int(0) }))
}

fn power(base: Int, exponent: Int) -> Int {
  if exponent == 0 {
    1
  } else {
    base * power(base, exponent - 1)
  }
}

// ## Agreement with the one-shot semantic summary

test integers_agree_at_every_head_width() {
  list.all(
    [
      0,
      1,
      23,
      24,
      255,
      256,
      65535,
      65536,
      4294967295,
      4294967296,
      18446744073709551615,
      -1,
      -24,
      -25,
      -256,
      -257,
      -18446744073709551616,
    ],
    fn(value) { agrees(int(value)) },
  )
}

test bignums_agree_at_the_definite_indefinite_and_chunk_boundaries() {
  let two_64 = power(2, 64)
  list.all(
    [
      two_64,
      -two_64 - 1,
      power(2, 512),
      -power(2, 512),
      power(2, 512) - 1,
      power(256, 64) - 1,
      power(256, 64),
      -power(256, 64) - 1,
      power(2, 1000),
      -power(2, 2000),
      // every magnitude byte 0xff, a whole number of words: tag 3 needs one
      // more word than its magnitude.
      -power(2, 128),
      -power(2, 576),
      -power(2, 520),
      // magnitudes past one 4,095-byte chunk of CBOR
      power(256, 4100),
      -power(256, 8200) + 7,
    ],
    fn(value) { agrees(int(value)) },
  )
}

test byte_strings_agree_across_chunked_and_flat_tree_boundaries() {
  list.all(
    [
      0, 1, 23, 24, 63, 64, 65, 127, 128, 129, 200, 4095, 4096, 5000, 8190, 8191,
      12285, 12286,
    ],
    fn(length) { agrees(bytes_of(length)) },
  )
}

test lists_and_maps_agree() {
  list.all(
    [
      builtin.list_data([]),
      flat(1),
      flat(30),
      flat(128),
      nested(1),
      nested(25),
      builtin.map_data([]),
      builtin.map_data([Pair(int(0), int(1))]),
      builtin.map_data(
        list.map(list.range(1, 24), fn(i) { Pair(int(i), bytes_of(i)) }),
      ),
      builtin.map_data(
        list.map(list.range(1, 256), fn(i) { Pair(int(i), int(-i)) }),
      ),
      builtin.map_data(
        [
          Pair(builtin.list_data([int(1)]), builtin.map_data([])),
          Pair(int(0), builtin.map_data([Pair(int(2), flat(3))])),
        ],
      ),
    ],
    agrees,
  )
}

test constructors_agree_at_every_tag_form() {
  let fields = [int(1), bytes_of(3), builtin.list_data([])]
  list.all(
    [
      builtin.constr_data(0, []),
      builtin.constr_data(0, fields),
      builtin.constr_data(6, fields),
      builtin.constr_data(7, []),
      builtin.constr_data(127, fields),
      builtin.constr_data(128, []),
      builtin.constr_data(1280, fields),
      builtin.constr_data(18446744073709551615, []),
      builtin.constr_data(18446744073709551615, fields),
    ],
    agrees,
  )
}

test a_mixed_datum_agrees() {
  agrees(
    builtin.constr_data(
      1280,
      [
        builtin.map_data(
          [
            Pair(bytes_of(70), nested(4)),
            Pair(int(power(2, 64)), builtin.constr_data(2, [flat(5)])),
            Pair(bytes_of(5000), int(-power(256, 100))),
          ],
        ),
        int(-power(2, 512)),
        builtin.constr_data(18446744073709551615, [nested(3)]),
      ],
    ),
  )
}

test nested_128_deep_agrees() {
  agrees(nested(128))
}

/// Eight records shaped like transaction inputs: a constructor over a
/// 32-byte id and an index.
test eight_input_records_agree() {
  agrees(
    builtin.list_data(
      list.map(
        list.range(0, 7),
        fn(i) { builtin.constr_data(0, [bytes_of(32), int(i)]) },
      ),
    ),
  )
}

// ## Leaves past the one-step bound

/// A byte string of `length` bytes read in windows: its summary is the bytes
/// leaf of its flat root, whatever the windows.
fn long_bytes_agree(length: Int) -> Bool {
  let content = block(length)
  let expected = Some(
    bytes_leaf(
      flat_root_v1(content),
      length,
      cek_data_v1.bytes_data_cbor_length_v1(length),
    ),
  )
  let bytes = cbor.serialise(builtin.b_data(content))
  and {
    whole_v1(bytes) == expected,
    windows_v1(bytes, 68, 1) == expected,
    windows_v1(bytes, 4300, 2) == expected,
    windows_v1(bytes, 30000, 1_000) == expected,
  }
}

test a_byte_string_past_the_ledger_leaf_bound_reads() {
  long_bytes_agree(20000)
}

test a_byte_string_past_a_power_of_two_chunk_count_reads() {
  long_bytes_agree(4095 * 4 + 1)
}

// ## The flat-tree frontier

test the_frontier_root_is_the_flat_root() {
  list.all(
    [1, 4094, 4095, 4096, 8190, 8191, 12285, 16380, 16381, 20475, 40000],
    fn(length) {
      let bytes = block(length)
      let chunks = chunks_of(bytes)
      frontier_root(append_chunks(empty_frontier, chunks)) == flat_root_v1(
        bytes,
      )
    },
  )
}

test the_frontier_root_is_the_flat_root_of_pieces() {
  let pieces = [block(50000), block(50000), block(30000)]
  frontier_root(append_chunks(empty_frontier, chunks_of_pieces(pieces))) == flat_root_of_pieces_v1(
    pieces,
  )
}

test refuses_a_chunk_after_a_short_one() fail {
  append_chunks(empty_frontier, [block(100), block(4095)]).count == 2
}

test refuses_an_empty_chunk() fail {
  append_chunks(empty_frontier, [#""]).count == 1
}

fn chunks_of(bytes: ByteArray) -> List<ByteArray> {
  let length = builtin.length_of_bytearray(bytes)
  if length <= 4095 {
    [bytes]
  } else {
    [
      builtin.slice_bytearray(0, 4095, bytes),
      ..chunks_of(builtin.slice_bytearray(4095, length - 4095, bytes))
    ]
  }
}

fn chunks_of_pieces(pieces: List<ByteArray>) -> List<ByteArray> {
  carry_chunks(pieces, #"")
}

fn carry_chunks(pieces: List<ByteArray>, carry: ByteArray) -> List<ByteArray> {
  when pieces is {
    [] ->
      if carry == #"" {
        []
      } else {
        [carry]
      }
    [piece, ..rest] -> {
      let length = builtin.length_of_bytearray(piece)
      let need = 4095 - builtin.length_of_bytearray(carry)
      if length < need {
        carry_chunks(rest, builtin.append_bytearray(carry, piece))
      } else {
        [
          builtin.append_bytearray(
            carry,
            builtin.slice_bytearray(0, need, piece),
          ),
          ..carry_chunks(
            [builtin.slice_bytearray(need, length - need, piece), ..rest],
            #"",
          )
        ]
      }
    }
  }
}

// ## Refused

test non_canonical_forms_are_refused() {
  list.all(
    [
      // empty indefinite list, field list and large-constructor field list
      #"9fff",
      #"d8799fff",
      #"d8668218c89fff",
      // short or leading-zero bignum magnitudes
      #"c24105",
      #"c240",
      #"c248ffffffffffffffff",
      #"c2490000000000000000ff",
      // tag-102 index below 128, non-minimal, or a bignum
      #"d866820580",
      #"d86682187f80",
      #"d866821900c880",
      #"d86682c249010000000000000000" |> bytearray.concat(#"80"),
      #"d8668318c88080",
      // definite non-empty list or field list, indefinite map, non-minimal
      // heads
      #"8100",
      #"d8798100",
      #"bfff",
      #"1800",
      #"1817",
      #"190017",
      #"3817",
      #"5817" |> bytearray.concat(block(23)),
      #"98020000",
      #"b90001" |> bytearray.concat(#"0000"),
      #"d9007a80",
      // text, simple, unknown tags
      #"60",
      #"f6",
      #"d87880",
      #"d8808080",
      #"d904ff80",
      #"d9057980",
      #"c0",
      // trailing and missing bytes
      #"0000",
      #"9f00",
      #"d87a",
      #"1b00000001",
      #"a100",
      #"a20000",
      #"",
    ],
    refused,
  )
}

fn chunk(length: Int) -> ByteArray {
  cbor.serialise(builtin.b_data(block(length)))
}

test unchunked_and_mischunked_byte_strings_are_refused() {
  list.all(
    [
      // a definite string longer than 64 bytes
      bytearray.concat(#"5841", block(65)),
      bytearray.concat(#"590100", block(256)),
      // an indefinite string of at most 64 bytes
      #"5f416aff",
      #"5f" |> bytearray.concat(chunk(64)) |> bytearray.concat(#"ff"),
      #"5fff",
      // a short chunk before the last
      #"5f"
        |> bytearray.concat(chunk(1))
        |> bytearray.concat(chunk(64))
        |> bytearray.concat(#"ff"),
      #"5f"
        |> bytearray.concat(chunk(64))
        |> bytearray.concat(chunk(63))
        |> bytearray.concat(chunk(64))
        |> bytearray.concat(#"ff"),
      // an empty final chunk
      #"5f"
        |> bytearray.concat(chunk(64))
        |> bytearray.concat(#"40ff"),
      // a missing break
      #"5f" |> bytearray.concat(chunk(64)) |> bytearray.concat(chunk(1)),
      // a nested indefinite chunk or a chunk of another type
      #"5f5f" |> bytearray.concat(chunk(64)) |> bytearray.concat(#"ffff"),
      #"5f"
        |> bytearray.concat(chunk(64))
        |> bytearray.concat(#"6161ff"),
      // a 64-byte chunk with a non-minimal head
      #"5f590040"
        |> bytearray.concat(block(64))
        |> bytearray.concat(chunk(1))
        |> bytearray.concat(#"ff"),
    ],
    refused,
  )
}

test mischunked_bignums_are_refused() {
  let magnitude = fn(length) { bytearray.concat(#"01", block(length - 1)) }
  list.all(
    [
      // an indefinite magnitude of at most 64 bytes
      #"c25f" |> bytearray.concat(chunk(64)) |> bytearray.concat(#"ff"),
      // a definite magnitude past 64 bytes
      #"c25841" |> bytearray.concat(magnitude(65)),
      // a leading zero in an indefinite magnitude
      #"c25f5840"
        |> bytearray.concat(#"00")
        |> bytearray.concat(block(63))
        |> bytearray.concat(chunk(1))
        |> bytearray.concat(#"ff"),
      // a short chunk before the last
      #"c35f"
        |> bytearray.concat(#"4101")
        |> bytearray.concat(chunk(64))
        |> bytearray.concat(#"ff"),
    ],
    refused,
  )
}

// ## Windows

/// A step that needs bytes past the window is refused, so a hop must end
/// before it; the step then succeeds in the next window.
test a_step_past_the_window_is_refused() {
  let bytes = cbor.serialise(builtin.list_data([bytes_of(40), int(1)]))
  // `9f`, then the 42-byte string: a window of 20 bytes holds the open but
  // not the string.
  expect Advanced(1, ListItems, [Parent(Root)]) = run(bytes, 0, Root, [], 20, 1)
  run(bytes, 1, ListItems, [Parent(Root)], 20, 1) == Refused
}

/// A leaf step stops before a chunk the window does not hold whole, and is
/// refused when it holds none.
test a_leaf_step_stops_at_the_window() {
  let content = block(200)
  let bytes = cbor.serialise(builtin.b_data(content))
  // `5f`, then 66 bytes per full chunk: a window of 100 holds one.
  expect Advanced(67, Leaf { length: 64, cbor_length: 67, .. }, []) = run(
    bytes,
    0,
    Root,
    [],
    100,
    1,
  )
  expect Advanced(67, frame, []) = run(bytes, 0, Root, [], 100, 1)
  run(bytes, 67, frame, [], 100, 1) == Refused
}

/// The value ends where the window ends: bytes after it are refused.
test the_value_must_end_the_window() {
  let bytes = bytearray.concat(cbor.serialise(flat(3)), #"00")
  and {
    run(bytes, 0, Root, [], bytearray.length(bytes), 100) == Refused,
    run(bytes, 0, Root, [], bytearray.length(bytes) - 1, 100) != Refused,
  }
}

// ## The stack

test a_fold_pops_the_restored_items() {
  let bytes = cbor.serialise(flat(2))
  // Open, two items, close: four steps leave both items on the stack.
  expect Advanced(4, Folding { .. } as frame, [Item(_), Item(_), Parent(Root)])
    = run(bytes, 0, Root, [], 4, 4)
  expect Advanced(4, Folding { .. }, [Item(_), Parent(Root)]) = run(
    bytes,
    4,
    frame,
    [Item(int_summary(0)), Item(int_summary(0)), Parent(Root)],
    4,
    1,
  )
  True
}

fn int_summary(value: Int) -> DataSummaryV1 {
  cek_data_v1.semantic_data_summary_v1(int(value))
}

test a_fold_below_the_restored_stack_fails() fail {
  let bytes = cbor.serialise(flat(2))
  expect Advanced(4, frame, _) = run(bytes, 0, Root, [], 4, 4)
  run(bytes, 4, frame, [Item(int_summary(0))], 4, 2) == Refused
}
