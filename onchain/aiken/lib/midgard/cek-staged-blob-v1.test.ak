use aiken/builtin
use aiken/collection/list
use cardano/address
use cardano/assets
use cardano/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, Output, OutputReference,
}
use midgard/cek_blob_frontier_v1
use midgard/cek_data_v1
use midgard/cek_proof_v1
use midgard/cek_staged_blob_v1.{
  bind_operand_v1, branch_root_v1, chunk_bytes, chunk_root_v1, collect_runs_v1,
  concat_balanced_v1, flat_root_of_pieces_v1, flat_root_v1,
}

// ## Fixtures

fn ascending(from: Int, acc: ByteArray) -> ByteArray {
  if from < 0 {
    acc
  } else {
    ascending(from - 1, builtin.cons_bytearray(from, acc))
  }
}

fn repeat_to(block: ByteArray, length: Int) -> ByteArray {
  if builtin.length_of_bytearray(block) >= length {
    builtin.slice_bytearray(0, length, block)
  } else {
    repeat_to(builtin.append_bytearray(block, block), length)
  }
}

/// `length` bytes where byte i is (i + shift) mod 256, so no two chunks of one
/// pattern are equal.
fn pattern(length: Int, shift: Int) -> ByteArray {
  let block = ascending(255, #"")
  repeat_to(
    builtin.slice_bytearray(shift, 256, builtin.append_bytearray(block, block)),
    length,
  )
}

/// The 4,095-byte chunks of `pattern(length, shift)`, built chunk by chunk so
/// any length works.
fn pattern_chunks(length: Int, shift: Int) -> List<ByteArray> {
  if length <= chunk_bytes {
    [pattern(length, shift)]
  } else {
    [
      pattern(chunk_bytes, shift),
      ..pattern_chunks(length - chunk_bytes, ( shift + chunk_bytes ) % 256)
    ]
  }
}

/// `pattern(length, shift)` as pieces of at most 65,536 bytes.
fn pattern_pieces(length: Int, shift: Int) -> List<ByteArray> {
  if length <= 65536 {
    [pattern(length, shift)]
  } else {
    [pattern(65536, shift), ..pattern_pieces(length - 65536, shift)]
  }
}

fn chunks_of(bytes: ByteArray) -> List<ByteArray> {
  pattern_chunks_of(bytes, builtin.length_of_bytearray(bytes))
}

fn pattern_chunks_of(bytes: ByteArray, length: Int) -> List<ByteArray> {
  if length <= chunk_bytes {
    [bytes]
  } else {
    [
      builtin.slice_bytearray(0, chunk_bytes, bytes),
      ..pattern_chunks_of(
        builtin.slice_bytearray(chunk_bytes, length - chunk_bytes, bytes),
        length - chunk_bytes,
      )
    ]
  }
}

fn runs_of(chunks: List<ByteArray>) -> List<List<ByteArray>> {
  when chunks is {
    [a, b, c, ..rest] -> [[a, b, c], ..runs_of(rest)]
    [] -> []
    short -> [short]
  }
}

fn reference(seed: Int) -> OutputReference {
  OutputReference {
    transaction_id: builtin.blake2b_256(builtin.cons_bytearray(seed, #"")),
    output_index: seed,
  }
}

fn output(datum) -> Output {
  Output {
    address: address.from_verification_key(builtin.replicate_byte(28, 7)),
    value: assets.from_lovelace(2000000),
    datum,
    reference_script: None,
  }
}

fn run_datum(chunks: List<ByteArray>) -> Data {
  builtin.constr_data(
    0,
    [builtin.list_data(list.map(chunks, fn(chunk) { builtin.b_data(chunk) }))],
  )
}

fn staging_input(seed: Int, chunks: List<ByteArray>) -> Input {
  Input {
    output_reference: reference(seed),
    output: output(InlineDatum(run_datum(chunks))),
  }
}

/// A reference input that is not a staging output (a reference script, say).
fn other_input(seed: Int) -> Input {
  Input { output_reference: reference(seed), output: output(NoDatum) }
}

/// Reference inputs for `runs` (seeds 10, 11, ...), with other reference
/// inputs before, between and after them, and the matching `refs`.
fn staged(
  runs: List<List<ByteArray>>,
) -> Pair<List<Input>, List<OutputReference>> {
  let seeds = list.indexed_map(runs, fn(index, _) { 10 + index })
  let staging = list.map2(seeds, runs, staging_input)
  let interleaved = list.foldr(
    staging,
    [other_input(200)],
    fn(input, acc) {
      [input, other_input(100 + input.output_reference.output_index), ..acc]
    },
  )
  Pair([other_input(1), ..interleaved], list.map(seeds, reference))
}

fn indices(count: Int) -> List<Int> {
  list.range(0, count - 1)
}

/// Stages `pattern(length, shift)` and binds it back: the root is the flat
/// root of the pieces and the chunks come back in order (and, up to 65,536
/// bytes, concatenate to the operand).
fn binds(length: Int, shift: Int) -> Bool {
  let chunks = pattern_chunks(length, shift)
  let runs = runs_of(chunks)
  let Pair(inputs, refs) = staged(runs)
  let bound = bind_operand_v1(
    collect_runs_v1(inputs, refs),
    indices(list.length(runs)),
  )
  and {
    bound.root == flat_root_of_pieces_v1(pattern_pieces(length, shift)),
    bound.length == length,
    bound.chunks == chunks,
    if length <= 65536 {
      concat_balanced_v1(bound.chunks) == pattern(length, shift)
    } else {
      True
    },
  }
}

/// Binds the runs of `runs` selected by `selection` and compares the root
/// with the flat root of `bytes`.
fn binds_runs(
  runs: List<List<ByteArray>>,
  selection: List<Int>,
  bytes: ByteArray,
) -> Bool {
  let Pair(inputs, refs) = staged(runs)
  bind_operand_v1(collect_runs_v1(inputs, refs), selection).root == flat_root_v1(
    bytes,
  )
}

fn flip_last_byte(bytes: ByteArray) -> ByteArray {
  let length = builtin.length_of_bytearray(bytes)
  builtin.append_bytearray(
    builtin.slice_bytearray(0, length - 1, bytes),
    builtin.cons_bytearray(
      ( builtin.index_bytearray(bytes, length - 1) + 1 ) % 256,
      #"",
    ),
  )
}

// ## Equal to the existing encoders

test chunk_root_equals_existing_chunk_hash() {
  list.all(
    [0, 1, 23, 24, 255, 256, 4094, 4095],
    fn(length) {
      let chunk = pattern(length, 3)
      chunk_root_v1(chunk) == cek_proof_v1.hash_blob_chunk_v1(chunk)
    },
  )
}

test chunk_root_refuses_a_chunk_over_4095_bytes() fail {
  builtin.length_of_bytearray(chunk_root_v1(pattern(4096, 0))) == 32
}

test branch_root_equals_existing_branch_hash() {
  let left = builtin.blake2b_256(#"01")
  let right = builtin.blake2b_256(#"02")
  list.all(
    [
      0, 23, 24, 255, 256, 4096, 65535, 65536, 4294967295, 4294967296,
      18446744073709551615,
    ],
    fn(length) {
      branch_root_v1(left, right, length) == cek_proof_v1.hash_blob_branch_v1(
        left,
        right,
        length,
      )
    },
  )
}

test flat_root_equals_bounded_root_up_to_9215_bytes() {
  list.all(
    [0, 1, 4094, 4095, 4096, 8189, 8190, 8191, 9214, 9215],
    fn(length) {
      let bytes = pattern(length, 5)
      flat_root_v1(bytes) == cek_proof_v1.bounded_blob_root_v1(bytes)
    },
  )
}

test flat_root_equals_ledger_data_root_up_to_16384_bytes() {
  list.all(
    [9216, 12284, 12285, 12286, 16379, 16380, 16381, 16384],
    fn(length) {
      let bytes = pattern(length, 9)
      flat_root_v1(bytes) == cek_data_v1.bounded_blob_root_v1(bytes)
    },
  )
}

/// One to nine chunks with a one-byte, a 4,094-byte and a full last chunk,
/// and 65,536, 100 K and 131,072 bytes, against the streaming frontier.
test flat_root_equals_frontier_root_at_any_chunk_count() {
  let lengths = list.concat(
    list.flat_map(
      list.range(1, 9),
      fn(count) {
        let full = ( count - 1 ) * chunk_bytes
        [full + 1, full + 4094, full + chunk_bytes]
      },
    ),
    [65536, 102400, 131072],
  )
  list.all(
    lengths,
    fn(length) {
      let root = flat_root_of_pieces_v1(pattern_pieces(length, 11))
      and {
        Some(root) == cek_blob_frontier_v1.root_from_chunks_v1(
          pattern_chunks(length, 11),
        ),
        if length <= 65536 {
          flat_root_v1(pattern(length, 11)) == root
        } else {
          True
        },
      }
    },
  )
}

// ## Staged operands: honest

test staged_operand_of_one_chunk_binds() {
  binds(100, 1)
}

test staged_empty_operand_binds() {
  binds(0, 0)
}

test staged_operand_of_9216_bytes_binds() {
  binds(9216, 2)
}

test staged_operand_of_65536_bytes_binds() {
  binds(65536, 3)
}

test staged_operand_of_100_kilobytes_binds() {
  binds(102400, 4)
}

test staged_operand_of_131072_bytes_binds() {
  binds(131072, 5)
}

test staged_operand_of_whole_runs_binds() {
  binds(3 * 3 * chunk_bytes, 6)
}

/// Two operands staged side by side; each binds from its own runs.
test two_staged_operands_bind_from_their_own_runs() {
  let first = pattern(20000, 7)
  let second = pattern(30000, 8)
  let first_runs = runs_of(chunks_of(first))
  let second_runs = runs_of(chunks_of(second))
  let Pair(inputs, refs) = staged(list.concat(first_runs, second_runs))
  let runs = collect_runs_v1(inputs, refs)
  let split = list.length(first_runs)
  let total = split + list.length(second_runs)
  and {
    bind_operand_v1(runs, list.range(0, split - 1)).root == flat_root_v1(first),
    bind_operand_v1(runs, list.range(split, total - 1)).root == flat_root_v1(
      second,
    ),
  }
}

/// A staged operand and the same bytes given directly have one root.
test staged_and_direct_operands_share_a_root() {
  let bytes = pattern(9215, 12)
  let Pair(inputs, refs) = staged(runs_of(chunks_of(bytes)))
  bind_operand_v1(collect_runs_v1(inputs, refs), [0]).root == cek_proof_v1.bounded_blob_root_v1(
    bytes,
  )
}

// ## Staged operands: refused

const refused_length = 65536

fn refused_runs() -> List<List<ByteArray>> {
  runs_of(chunks_of(pattern(refused_length, 13)))
}

test staged_runs_out_of_order_are_refused() fail {
  let runs = refused_runs()
  binds_runs(
    runs,
    [1, 0, ..list.range(2, list.length(runs) - 1)],
    pattern(refused_length, 13),
  )
}

test staged_operand_missing_a_run_is_refused() fail {
  let runs = refused_runs()
  binds_runs(
    runs,
    list.range(0, list.length(runs) - 2),
    pattern(refused_length, 13),
  )
}

test staged_operand_with_an_extra_run_is_refused() fail {
  let runs = refused_runs()
  binds_runs(
    list.concat(runs, [[pattern(chunk_bytes, 0)]]),
    list.range(0, list.length(runs)),
    pattern(refused_length, 13),
  )
}

test staged_operand_with_a_duplicated_run_is_refused() fail {
  let runs = refused_runs()
  binds_runs(
    runs,
    [0, ..list.range(0, list.length(runs) - 1)],
    pattern(refused_length, 13),
  )
}

test staged_operand_with_a_tampered_chunk_is_refused() fail {
  let bytes = pattern(9216, 14)
  expect [first, second, third] = chunks_of(bytes)
  binds_runs([[first, flip_last_byte(second), third]], [0], bytes)
}

test staged_operand_with_a_short_chunk_before_the_last_is_refused() fail {
  let bytes = pattern(8190, 15)
  binds_runs(
    [
      [
        builtin.slice_bytearray(0, 4094, bytes),
        builtin.slice_bytearray(4094, chunk_bytes, bytes),
        builtin.slice_bytearray(8189, 1, bytes),
      ],
    ],
    [0],
    bytes,
  )
}

test staged_operand_with_a_trailing_empty_chunk_is_refused() fail {
  let bytes = pattern(chunk_bytes, 16)
  binds_runs([[bytes, #""]], [0], bytes)
}

test staged_run_of_four_chunks_is_refused() fail {
  let bytes = pattern(4 * chunk_bytes, 17)
  binds_runs([chunks_of(bytes)], [0], bytes)
}

test staged_run_with_an_empty_chunk_list_is_refused() fail {
  binds_runs([[]], [0], #"")
}

test staged_run_with_a_non_bytes_chunk_is_refused() fail {
  let Pair(inputs, refs) = Pair(
    [
      Input {
        output_reference: reference(10),
        output: output(
          InlineDatum(
            builtin.constr_data(0, [builtin.list_data([builtin.i_data(5)])]),
          ),
        ),
      },
    ],
    [reference(10)],
  )
  bind_operand_v1(collect_runs_v1(inputs, refs), [0]).length >= 0
}

fn single_input_binds(datum) -> Bool {
  let inputs = [
    Input { output_reference: reference(10), output: output(datum) },
  ]
  bind_operand_v1(collect_runs_v1(inputs, [reference(10)]), [0]).length == 1
}

test staged_run_honest_single_input_control() {
  single_input_binds(InlineDatum(run_datum([#"2a"])))
}

test staged_run_under_another_constructor_is_refused() fail {
  single_input_binds(
    InlineDatum(
      builtin.constr_data(1, [builtin.list_data([builtin.b_data(#"2a")])]),
    ),
  )
}

test staged_run_with_an_extra_field_is_refused() fail {
  single_input_binds(
    InlineDatum(
      builtin.constr_data(
        0,
        [builtin.list_data([builtin.b_data(#"2a")]), builtin.i_data(0)],
      ),
    ),
  )
}

test staged_run_as_a_bare_list_is_refused() fail {
  single_input_binds(InlineDatum(builtin.list_data([builtin.b_data(#"2a")])))
}

test staged_run_without_an_inline_datum_is_refused() fail {
  single_input_binds(DatumHash(builtin.blake2b_256(#"2a")))
}

test staged_reference_missing_from_reference_inputs_is_refused() fail {
  let bytes = pattern(9216, 18)
  let Pair(inputs, _) = staged(runs_of(chunks_of(bytes)))
  bind_operand_v1(collect_runs_v1(inputs, [reference(99)]), [0]).root == flat_root_v1(
    bytes,
  )
}

test staged_references_out_of_ledger_order_are_refused() fail {
  let bytes = pattern(30000, 19)
  let runs = runs_of(chunks_of(bytes))
  let Pair(inputs, refs) = staged(runs)
  expect [first, second, ..rest] = refs
  let collected = collect_runs_v1(inputs, [second, first, ..rest])
  bind_operand_v1(collected, [1, 0, ..list.range(2, list.length(runs) - 1)]).root == flat_root_v1(
    bytes,
  )
}

test staged_operand_index_outside_the_runs_is_refused() fail {
  let bytes = pattern(100, 20)
  let Pair(inputs, refs) = staged([[bytes]])
  bind_operand_v1(collect_runs_v1(inputs, refs), [1]).root == flat_root_v1(
    bytes,
  )
}

test staged_operand_with_no_runs_is_refused() fail {
  let bytes = pattern(100, 21)
  let Pair(inputs, refs) = staged([[bytes]])
  bind_operand_v1(collect_runs_v1(inputs, refs), []).length >= 0
}

// ## Balanced concatenation

test balanced_concatenation_keeps_order() {
  let pieces = list.map(
    list.range(0, 6),
    fn(index) { pattern(index * 37 + 1, index) },
  )
  and {
    concat_balanced_v1([]) == #"",
    list.all(
      list.range(1, 7),
      fn(count) {
        let taken = list.take(pieces, count)
        concat_balanced_v1(taken) == list.foldr(
          taken,
          #"",
          builtin.append_bytearray,
        )
      },
    ),
  }
}

/// Pieces of any sizes, including empty ones and ones that end mid-chunk,
/// root to the flat root of their concatenation.
test flat_root_of_pieces_equals_flat_root() {
  let bytes = pattern(20000, 22)
  let cut = fn(sizes: List<Int>) {
    list.foldl(
      sizes,
      Pair([], 0),
      fn(size, acc) {
        let Pair(pieces, offset) = acc
        Pair(
          list.concat(pieces, [builtin.slice_bytearray(offset, size, bytes)]),
          offset + size,
        )
      },
    ).1st
  }
  list.all(
    [
      [20000],
      [0, 20000, 0],
      [1, 4094, 15905],
      [4095, 4095, 11810],
      [4096, 8190, 7714],
      [3000, 3000, 3000, 3000, 8000],
      [19999, 1],
    ],
    fn(sizes) { flat_root_of_pieces_v1(cut(sizes)) == flat_root_v1(bytes) },
  )
}

test flat_root_of_no_pieces_is_the_empty_root() {
  flat_root_of_pieces_v1([]) == flat_root_v1(#"")
}

test flat_root_of_pieces_refuses_a_piece_over_65536_bytes() fail {
  let piece = pattern(65536, 0)
  let carried = builtin.append_bytearray(#"01", piece)
  builtin.length_of_bytearray(flat_root_of_pieces_v1([carried])) == 32
}
