use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/data_summary_walk_v1.{
  Advanced, Finished, Item, Parent, Refused, Root, Summarised, Unreadable, Walk,
  Walking,
} as walk

// ## Drivers

/// The whole span in one run.
fn one_shot(bytes: ByteArray) -> Option<DataSummaryV1> {
  when
    walk.resume(
      walk.start(0),
      bytes,
      bytearray.length(bytes),
      [],
      walk.empty_stack_root,
      1_000_000,
    )
  is {
    Summarised(summary) -> Some(summary)
    _ -> None
  }
}

/// One item per transaction. Each transaction restores the entries its item
/// pops below its own pushes, plus `extra` more where the stack has them
/// (all of it when `extra` is negative), and chains the rest.
fn stepwise(bytes: ByteArray, extra: Int) -> Option<DataSummaryV1> {
  stepwise_from(bytes, bytearray.length(bytes), extra, walk.start(0), [])
}

fn common_prefix(left: List<a>, right: List<a>) -> Int {
  when (left, right) is {
    ([l, ..ls], [r, ..rs]) ->
      if l == r {
        1 + common_prefix(ls, rs)
      } else {
        0
      }
    _ -> 0
  }
}

/// How many top entries of `before` a step that left `after` popped.
fn popped(before: List<walk.Entry>, after: List<walk.Entry>) -> Int {
  list.length(before) - common_prefix(list.reverse(before), list.reverse(after))
}

fn stepwise_from(
  bytes: ByteArray,
  end: Int,
  extra: Int,
  checkpoint: Walk,
  stack: List<walk.Entry>,
) -> Option<DataSummaryV1> {
  when walk.run(bytes, checkpoint.offset, checkpoint.frame, stack, end, 1) is {
    Advanced(offset, frame, next) -> {
      let restore = if extra < 0 {
        stack
      } else {
        list.take(stack, popped(stack, next) + extra)
      }
      let base = walk.chain(list.drop(stack, list.length(restore)), #"")
      // The verifier's view of the same transaction.
      expect Walking(after) = walk.resume(
        checkpoint,
        bytes,
        end,
        restore,
        base,
        1,
      )
      expect after == Walk { offset, frame, stack_root: walk.chain(next, #"") }
      stepwise_from(bytes, end, extra, after, next)
    }
    Finished(summary) -> {
      expect
        Summarised(summary) == walk.resume(
          checkpoint,
          bytes,
          end,
          stack,
          #"",
          1,
        )
      Some(summary)
    }
    Refused -> None
  }
}

fn agrees(data: Data) -> Bool {
  let expected = Some(cek_data_v1.semantic_data_summary_v1(data))
  let bytes = cbor.serialise(data)
  and {
    one_shot(bytes) == expected,
    stepwise(bytes, -1) == expected,
  }
}

fn unreadable(bytes: ByteArray) -> Bool {
  walk.resume(
    walk.start(0),
    bytes,
    bytearray.length(bytes),
    [],
    walk.empty_stack_root,
    1_000_000,
  ) == Unreadable
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

test bignums_agree_on_both_sides_of_the_chunk_boundary() {
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
    ],
    fn(value) { agrees(int(value)) },
  )
}

test byte_strings_agree_across_chunked_and_blob_boundaries() {
  list.all(
    [0, 1, 23, 24, 63, 64, 65, 127, 128, 129, 200, 4095, 4096, 5000],
    fn(length) { agrees(bytes_of(length)) },
  )
}

test lists_and_maps_agree() {
  list.all(
    [
      builtin.list_data([]),
      flat(1),
      flat(30),
      nested(1),
      nested(25),
      builtin.map_data([]),
      builtin.map_data([Pair(int(0), int(1))]),
      builtin.map_data(
        list.map(list.range(1, 24), fn(i) { Pair(int(i), bytes_of(i)) }),
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

test a_mixed_datum_agrees_under_any_sufficient_restore() {
  let datum = builtin.constr_data(
    1280,
    [
      builtin.map_data(
        [
          Pair(bytes_of(70), nested(4)),
          Pair(int(power(2, 64)), builtin.constr_data(2, [flat(5)])),
        ],
      ),
      int(-power(2, 512)),
      builtin.constr_data(18446744073709551615, [nested(3)]),
    ],
  )
  let expected = Some(cek_data_v1.semantic_data_summary_v1(datum))
  let bytes = cbor.serialise(datum)
  list.all([0, 1, 2, 5, -1], fn(extra) { stepwise(bytes, extra) == expected })
}

// ## Unreadable

test non_canonical_forms_are_unreadable() {
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
      // indefinite byte string of at most 64 bytes
      #"5f416aff",
      // tag-102 index below 128, non-minimal, or a bignum
      #"d866820580",
      #"d866821900c880",
      #"d86682c249010000000000000000" |> bytearray.concat(#"80"),
      // definite non-empty list, indefinite map, non-minimal head
      #"8100",
      #"bfff",
      #"1800",
      #"5817",
      // text, simple, unknown tags
      #"60",
      #"f6",
      #"d87880",
      #"d9057980",
      #"c0",
      // trailing and missing bytes
      #"0000",
      #"9f00",
      #"d87a",
      #"1b00000001",
      #"a100",
    ],
    unreadable,
  )
}

test unchunked_and_mischunked_byte_strings_are_unreadable() {
  let chunk = fn(length) { cbor.serialise(builtin.b_data(block(length))) }
  list.all(
    [
      // a definite string longer than 64 bytes
      bytearray.concat(#"5841", block(65)),
      // one full chunk only
      #"5f" |> bytearray.concat(chunk(64)) |> bytearray.concat(#"ff"),
      // a short chunk before the last
      #"5f"
        |> bytearray.concat(chunk(1))
        |> bytearray.concat(chunk(64))
        |> bytearray.concat(#"ff"),
      // an empty final chunk
      #"5f"
        |> bytearray.concat(chunk(64))
        |> bytearray.concat(#"40ff"),
      // a missing break
      #"5f" |> bytearray.concat(chunk(64)) |> bytearray.concat(chunk(1)),
    ],
    unreadable,
  )
}

// ## Checkpoints

test a_wrong_restore_fails() fail {
  let bytes = cbor.serialise(nested(3))
  expect Advanced(offset, frame, stack) = walk.run(
    bytes,
    0,
    Root,
    [],
    bytearray.length(bytes),
    2,
  )
  let checkpoint = Walk { offset, frame, stack_root: walk.chain(stack, #"") }
  // Omits the bottom entry from both the restore and its base.
  walk.resume(
    checkpoint,
    bytes,
    bytearray.length(bytes),
    list.take(stack, 1),
    #"",
    10,
  ) != Unreadable
}

test a_short_restore_cannot_close_the_container() fail {
  let bytes = cbor.serialise(flat(3))
  let end = bytearray.length(bytes)
  expect Advanced(offset, frame, stack) = walk.run(bytes, 0, Root, [], end, 4)
  let checkpoint = Walk { offset, frame, stack_root: walk.chain(stack, #"") }
  // Restores the three items but not the frame below them.
  walk.resume(
    checkpoint,
    bytes,
    end,
    list.take(stack, 3),
    walk.chain(list.drop(stack, 3), #""),
    10,
  ) != Unreadable
}

test the_checkpoint_commits_the_suspended_parent() {
  let bytes = cbor.serialise(nested(2))
  expect Advanced(2, walk.ListItems, stack) = walk.run(
    bytes,
    0,
    Root,
    [],
    bytearray.length(bytes),
    2,
  )
  stack == [Parent(walk.ListItems), Parent(Root)]
}

test an_item_entry_carries_its_summary() {
  let bytes = cbor.serialise(flat(2))
  expect Advanced(_, _, [Item(summary), ..]) = walk.run(
    bytes,
    0,
    Root,
    [],
    bytearray.length(bytes),
    2,
  )
  summary == cek_data_v1.semantic_data_summary_v1(int(0))
}
