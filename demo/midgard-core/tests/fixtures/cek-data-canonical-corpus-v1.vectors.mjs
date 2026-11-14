/**
 * The **canonical Data corpus**: one list of source spans, each with the
 * verdict the streamed CEK Data traverse must give it, driven from two sides.
 *
 * A span is canonical Data iff it is in the image of Cardano's
 * `serialiseData`: minimal heads; integers in [-2^64, 2^64 - 1] as major 0/1
 * and tag 2/3 bignums outside it with a magnitude of at least 9 bytes and no
 * leading zero; byte strings (and bignum magnitudes) definite up to 64 bytes
 * and indefinite above, in 64-byte chunks with a final chunk of 1..64 bytes;
 * the empty list exactly `80` and every other list or field list indefinite;
 * definite maps; constructor tags 121..127, 1280..1400 and tag 102 over a
 * two-element array whose index is a minimal major-0 integer of at least 128;
 * exactly one item ending at the span end. Every other span is `unreadable`,
 * at the traverse stage that reads the offending byte (`stage`).
 *
 * The generator (`scripts/generate-cek-data-canonical-corpus-v1-goldens.mjs`)
 * renders `onchain/aiken/lib/midgard/cek-data-canonical-corpus-v1-golden.test.ak`
 * from this list, binding every accepted span to the summary the TypeScript
 * twin computes; the Aiken honest driver must reach the same summary, and
 * `unreadable_v1` at the named stage for every other span. The vitest suite
 * drives the same list through the TypeScript twin.
 *
 * This module holds data only and imports nothing from the package.
 */

/** `count` copies of one byte, as hex. */
const repeat = (byteHex, count) => byteHex.repeat(count);

/** A nonzero byte followed by zeros: a magnitude with no leading zero. */
const magnitude = (length) => `01${repeat("00", length - 1)}`;

/** `length` bytes of a non-repeating filler, as hex. */
const filler = (length) =>
  Buffer.from(
    Array.from({ length }, (_, index) => (index * 7 + 3) % 256),
  ).toString("hex");

const definiteBytes = (length) => {
  const header =
    length < 24
      ? (0x40 + length).toString(16).padStart(2, "0")
      : `58${length.toString(16).padStart(2, "0")}`;
  return `${header}${filler(length)}`;
};

/** An indefinite byte string of the given chunk lengths. */
const chunkedBytes = (lengths) =>
  `5f${lengths.map((length) => definiteBytes(length)).join("")}ff`;

/** The traverse stages a span can be unreadable at. */
export const stages = Object.freeze({
  head: 0,
  integer: 1,
  bytes: 2,
  largeConstructor: 3,
  largeFields: 4,
  close: 5,
  fold: 6,
});

const accepted = (name, hex) => ({ name, hex, verdict: "accepted" });
const unreadable = (name, hex, stage) => ({
  name,
  hex,
  verdict: "unreadable",
  stage,
});

export const vectors = Object.freeze([
  // Integers.
  accepted("int_zero", "00"),
  accepted("int_23", "17"),
  accepted("int_24", "1818"),
  accepted("int_256", "190100"),
  accepted("int_2_pow_32", "1b0000000100000000"),
  accepted("int_2_pow_64_minus_1", "1bffffffffffffffff"),
  accepted("int_minus_1", "20"),
  accepted("int_minus_2_pow_64", "3bffffffffffffffff"),
  accepted("bignum_2_pow_64", `c249${magnitude(9)}`),
  accepted("bignum_minus_2_pow_64_minus_1", `c349${magnitude(9)}`),
  accepted("bignum_64_byte_magnitude", `c25840${magnitude(64)}`),
  accepted(
    "bignum_65_byte_magnitude_chunked",
    `c25f5840${magnitude(64)}4100ff`,
  ),
  accepted(
    "bignum_negative_129_byte_magnitude_chunked",
    `c35f5840${magnitude(64)}5840${repeat("00", 64)}4100ff`,
  ),
  unreadable("int_non_minimal_one_byte", "1817", stages.head),
  unreadable("int_non_minimal_two_byte", "190017", stages.head),
  unreadable("int_non_minimal_eight_byte", "1b00000000ffffffff", stages.head),
  unreadable("int_reserved_additional_28", "1c", stages.head),
  unreadable("int_reserved_additional_30", "3e", stages.head),
  unreadable("int_indefinite_additional", "1f", stages.head),
  unreadable("int_truncated_argument", "1a0001", stages.head),
  unreadable("bignum_one_byte_magnitude", "c24105", stages.integer),
  unreadable("bignum_empty_magnitude", "c240", stages.integer),
  unreadable(
    "bignum_eight_byte_magnitude",
    `c248${magnitude(8)}`,
    stages.integer,
  ),
  unreadable("bignum_leading_zero", `c24900${magnitude(8)}`, stages.integer),
  unreadable("negative_bignum_one_byte", "c34101", stages.integer),
  unreadable(
    "bignum_indefinite_nine_bytes",
    `c25f49${magnitude(9)}ff`,
    stages.integer,
  ),
  unreadable(
    "bignum_indefinite_one_full_chunk",
    `c25f5840${magnitude(64)}ff`,
    stages.integer,
  ),
  unreadable("bignum_over_integer", "c200", stages.head),
  unreadable("bignum_magnitude_past_end", `c249${magnitude(8)}`, stages.head),

  // Byte strings.
  accepted("bytes_empty", "40"),
  accepted("bytes_one", "4100"),
  accepted("bytes_32", definiteBytes(32)),
  accepted("bytes_64", definiteBytes(64)),
  accepted("bytes_65_chunked", chunkedBytes([64, 1])),
  accepted("bytes_128_chunked", chunkedBytes([64, 64])),
  accepted("bytes_129_chunked", chunkedBytes([64, 64, 1])),
  unreadable("bytes_definite_65", definiteBytes(65), stages.head),
  unreadable("bytes_indefinite_empty", "5fff", stages.bytes),
  unreadable("bytes_indefinite_short", "5f416aff", stages.bytes),
  unreadable(
    "bytes_indefinite_one_full_chunk",
    chunkedBytes([64]),
    stages.bytes,
  ),
  unreadable("bytes_short_chunk_first", chunkedBytes([1, 64]), stages.bytes),
  unreadable("bytes_empty_final_chunk", chunkedBytes([64, 0]), stages.bytes),
  unreadable("bytes_empty_first_chunk", chunkedBytes([0, 64, 1]), stages.bytes),
  unreadable("bytes_oversized_chunk", chunkedBytes([65, 1]), stages.bytes),
  unreadable("bytes_integer_chunk", `5f5840${filler(64)}00ff`, stages.bytes),
  unreadable(
    "bytes_nested_indefinite_chunk",
    `5f5f5840${filler(64)}4101ffff`,
    stages.bytes,
  ),
  unreadable("bytes_missing_break", `5f5840${filler(64)}4101`, stages.bytes),
  unreadable("bytes_past_end", `5820${filler(31)}`, stages.head),
  unreadable("text_string", "6161", stages.head),
  unreadable("text_empty", "60", stages.head),

  // Simple values and floats.
  unreadable("simple_false", "f4", stages.head),
  unreadable("simple_null", "f6", stages.head),
  unreadable("float_half", "f93c00", stages.head),

  // Lists.
  accepted("list_empty", "80"),
  accepted("list_one", "9f00ff"),
  accepted("list_two", "9f0001ff"),
  accepted("list_nested", "9f9f00ff80ff"),
  accepted("list_of_bytes", `9f${chunkedBytes([64, 2])}40ff`),
  unreadable("list_definite_one", "8100", stages.head),
  unreadable("list_definite_two", "820001", stages.head),
  unreadable("list_indefinite_empty", "9fff", stages.head),
  unreadable("list_open_only", "9f", stages.head),
  unreadable("list_missing_break", "9f00", stages.integer),
  unreadable("list_nested_missing_break", "9f9f00ff", stages.fold),
  unreadable("list_inner_empty_indefinite", "9f9fffff", stages.head),

  // Maps.
  accepted("map_empty", "a0"),
  accepted("map_one", "a10000"),
  accepted("map_duplicate_keys_kept", "a200000000"),
  accepted("map_out_of_order_kept", "a201000000"),
  accepted("map_nested", `a1${definiteBytes(3)}a1009f01ff`),
  unreadable("map_indefinite", "bf0000ff", stages.head),
  unreadable("map_indefinite_empty", "bfff", stages.head),
  unreadable("map_missing_value", "a100", stages.integer),
  unreadable("map_missing_pair", "a2000000", stages.integer),

  // Small constructors.
  accepted("constr_0_empty", "d87980"),
  accepted("constr_0_one", "d8799f00ff"),
  accepted("constr_6_empty", "d87f80"),
  accepted("constr_7_empty", "d9050080"),
  accepted("constr_127_one", "d905789f01ff"),
  accepted("constr_nested", "d8799fd87a9f9f00ffffa0ff"),
  unreadable("constr_empty_indefinite_fields", "d8799fff", stages.head),
  unreadable("constr_definite_fields", "d879810000", stages.head),
  unreadable("constr_fields_missing", "d879", stages.head),
  unreadable("constr_fields_open_only", "d8799f", stages.head),
  unreadable("constr_fields_missing_break", "d8799f00", stages.integer),
  unreadable("tag_120", "d87880", stages.head),
  unreadable("tag_128", "d88080", stages.head),
  unreadable("tag_1279", "d904ff80", stages.head),
  unreadable("tag_1401", "d9057980", stages.head),
  unreadable("tag_constr_7_non_minimal", "d9000580", stages.head),
  unreadable("tag_1", "c100", stages.head),
  unreadable("tag_4", "c400", stages.head),

  // Large constructors (tag 102).
  accepted("large_constr_128_empty", "d86682188080"),
  accepted("large_constr_128_one", "d8668218809f00ff"),
  accepted("large_constr_256", "d8668219010080"),
  accepted("large_constr_max_index", "d866821bffffffffffffffff80"),
  accepted("large_constr_nested", "d8668218809fd866821901009f01ffff"),
  unreadable("large_constr_index_5", "d866820580", stages.head),
  unreadable("large_constr_index_127", "d86682187f80", stages.head),
  unreadable("large_constr_index_non_minimal", "d866821900c880", stages.head),
  unreadable("large_constr_index_negative", "d86682388080", stages.head),
  unreadable(
    "large_constr_index_bignum",
    `d86682c249${magnitude(9)}9f01ff`,
    stages.head,
  ),
  unreadable("large_constr_indefinite_outer", "d8669f188080ff", stages.head),
  unreadable("large_constr_outer_length_3", "d8668318808000", stages.head),
  unreadable("large_constr_outer_length_1", "d866811880", stages.head),
  unreadable("large_constr_fields_missing", "d866821880", stages.head),
  unreadable(
    "large_constr_empty_indefinite_fields",
    "d8668218c89fff",
    stages.head,
  ),
  unreadable(
    "large_constr_definite_fields",
    "d8668218c88100",
    stages.largeFields,
  ),
  unreadable("large_constr_map_fields", "d8668218c8a0", stages.largeFields),

  // Extent.
  unreadable("trailing_after_integer", "0000", stages.integer),
  unreadable("trailing_after_bytes", "4000", stages.bytes),
  unreadable("trailing_after_list", "8000", stages.fold),
  unreadable("trailing_after_indefinite_list", "9f00ff00", stages.fold),
  unreadable("trailing_after_map", "a000", stages.fold),
  unreadable("trailing_break", "00ff", stages.integer),
]);
