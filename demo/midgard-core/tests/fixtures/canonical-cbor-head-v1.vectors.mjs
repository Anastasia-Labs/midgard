/**
 * The **canonical CBOR head vectors**: one list of heads, each with the result
 * every head reader must give it, so that the separate readers cannot drift
 * apart at head level.
 *
 * A head is one initial byte (major type `byte >> 5`, additional information
 * `byte & 31`) and the argument it announces. A canonical reader accepts it
 * exactly when it is definite and shortest-form: additional information below
 * 24 is the argument itself, and 24, 25, 26 and 27 announce a 1-, 2-, 4- or
 * 8-byte big-endian argument that must not fit the next shorter form (at
 * least 24, 256, 65536 and 2^32). It reads the argument as an unsigned
 * integer whatever the major type, so for major type 7 it does not apply the
 * simple-value and float rules. Every other head is refused, with one reason
 * class:
 *
 * - `out-of-range`: the offset is outside the bytes;
 * - `truncated`: the bytes end inside the argument;
 * - `non-minimal`: a shorter form holds the argument;
 * - `reserved`: additional information 28, 29 or 30;
 * - `indefinite`: additional information 31.
 *
 * Where `cbor_deserialise` (the ledger's permissive Data decoder) applies, a
 * vector also records `permissive`: the bytes are exactly one integer head,
 * the head of an empty byte string, list or map, or an indefinite head of one
 * of those closed at once by its break, and `cbor_deserialise` either accepts
 * them, giving `serialised` (what `serialiseData` writes for the decoded
 * value), or refuses them. Non-minimal heads and the indefinite empty
 * containers are where the two kinds part: the permissive decoder accepts
 * them.
 *
 * The generator (`scripts/generate-cek-data-canonical-corpus-v1-goldens.mjs`)
 * renders `onchain/aiken/lib/midgard/canonical-cbor-head-v1-golden.test.ak`
 * from this list. Every expected result here follows from how the vector is
 * built, never from running a reader.
 *
 * This module holds data only and imports nothing from the package.
 */

/** The refusal reason classes. */
export const reasons = Object.freeze({
  outOfRange: "out-of-range",
  truncated: "truncated",
  nonMinimal: "non-minimal",
  reserved: "reserved",
  indefinite: "indefinite",
});

/** The argument width each additional-information value announces. */
const widths = Object.freeze({ 24: 1, 25: 2, 26: 4, 27: 8 });

const byteHex = (value) => value.toString(16).padStart(2, "0");

/** A head of `major` with additional information `info` over `argument`. */
const headHex = (major, info, argument = 0n) => {
  const width = widths[info] ?? 0;
  const written =
    width === 0 ? "" : argument.toString(16).padStart(2 * width, "0");
  return `${byteHex(major * 32 + info)}${written}`;
};

/** The additional information of the shortest head holding `value`. */
const shortestInfo = (value) =>
  value < 24n
    ? Number(value)
    : value < 256n
      ? 24
      : value < 65536n
        ? 25
        : value < 2n ** 32n
          ? 26
          : 27;

const minimalHex = (major, value) => headHex(major, shortestInfo(value), value);

const accept = (major, value, next) =>
  Object.freeze({ verdict: "accept", major, value, next });
const refuse = (reason) => Object.freeze({ verdict: "refuse", reason });

/**
 * What `cbor_deserialise` gives `hex` read whole, for the vectors it applies
 * to: an integer head (major 0 or 1) or the head of an empty byte string,
 * list or map. Any other head alone is not one complete item.
 */
const permissiveOf = (major, info, value, complete) => {
  if (!complete || info > 27) {
    return major <= 1 ? Object.freeze({ verdict: "refuse" }) : undefined;
  }
  if (major <= 1) {
    return Object.freeze({
      verdict: "accept",
      serialised: minimalHex(major, value),
    });
  }
  if ([2, 4, 5].includes(major) && value === 0n) {
    return Object.freeze({
      verdict: "accept",
      serialised: minimalHex(major, 0n),
    });
  }
  return undefined;
};

const vector = (name, hex, offset, canonical, permissive) =>
  Object.freeze({
    name,
    hex,
    offset,
    canonical,
    ...(permissive === undefined ? {} : { permissive }),
  });

const majors = [0, 1, 2, 3, 4, 5, 6, 7];

/** Each boundary of the shortest form, and the extremes. */
const boundaryValues = Object.freeze([
  ["value_0", 0n],
  ["value_23", 23n],
  ["value_24", 24n],
  ["value_255", 255n],
  ["value_256", 256n],
  ["value_65535", 65535n],
  ["value_65536", 65536n],
  ["value_2_pow_32_minus_1", 2n ** 32n - 1n],
  ["value_2_pow_32", 2n ** 32n],
  ["value_2_pow_64_minus_1", 2n ** 64n - 1n],
]);

/** Arguments written wider than they need: zero, and the largest value the
 * next shorter form holds, at each width. */
const nonMinimalForms = Object.freeze([
  ["one_byte_0", 24, 0n],
  ["one_byte_23", 24, 23n],
  ["two_byte_0", 25, 0n],
  ["two_byte_255", 25, 255n],
  ["four_byte_0", 26, 0n],
  ["four_byte_65535", 26, 65535n],
  ["eight_byte_0", 27, 0n],
  ["eight_byte_2_pow_32_minus_1", 27, 2n ** 32n - 1n],
]);

const minimalVectors = majors.flatMap((major) =>
  boundaryValues.map(([label, value]) => {
    const hex = minimalHex(major, value);
    return vector(
      `major_${major}_${label}`,
      hex,
      0,
      accept(major, value, hex.length / 2),
      permissiveOf(major, shortestInfo(value), value, true),
    );
  }),
);

const nonMinimalVectors = majors.flatMap((major) =>
  nonMinimalForms.map(([label, info, value]) =>
    vector(
      `major_${major}_non_minimal_${label}`,
      headHex(major, info, value),
      0,
      refuse(reasons.nonMinimal),
      permissiveOf(major, info, value, true),
    ),
  ),
);

const reservedVectors = majors.flatMap((major) => [
  ...[28, 29, 30].map((info) =>
    vector(
      `major_${major}_reserved_${info}`,
      headHex(major, info),
      0,
      refuse(reasons.reserved),
      permissiveOf(major, info, 0n, true),
    ),
  ),
  vector(
    `major_${major}_indefinite`,
    headHex(major, 31),
    0,
    refuse(reasons.indefinite),
    permissiveOf(major, 31, 0n, true),
  ),
]);

/** The empty byte string, list and map written indefinite: the indefinite head
 * and its break. The permissive decoder reads the empty value, which
 * `serialiseData` writes definite. */
const indefiniteEmptyVectors = [2, 4, 5].map((major) =>
  vector(
    `major_${major}_indefinite_empty`,
    `${headHex(major, 31)}ff`,
    0,
    refuse(reasons.indefinite),
    Object.freeze({ verdict: "accept", serialised: minimalHex(major, 0n) }),
  ),
);

/** Every proper prefix of a canonical head that keeps its initial byte. */
const truncatedVectors = [0, 4].flatMap((major) =>
  [
    ["one_byte", 24, 24n],
    ["two_byte", 25, 256n],
    ["four_byte", 26, 65536n],
    ["eight_byte", 27, 2n ** 32n],
  ].flatMap(([label, info, value]) => {
    const whole = headHex(major, info, value);
    return Array.from({ length: widths[info] }, (_, kept) =>
      vector(
        `major_${major}_${label}_truncated_after_${kept}`,
        whole.slice(0, 2 + 2 * kept),
        0,
        refuse(reasons.truncated),
        permissiveOf(major, info, value, false),
      ),
    );
  }),
);

/** Heads read at a nonzero offset, with bytes after them left unread. */
const offsetVectors = [
  vector("at_offset_1_one_byte", "ff1818", 1, accept(0, 24n, 3)),
  vector("at_offset_1_trailing", "ff19010000", 1, accept(0, 256n, 4)),
  vector(
    "at_offset_2_eight_byte_trailing",
    "00001b0000000100000000ff",
    2,
    accept(0, 2n ** 32n, 11),
  ),
  vector("at_offset_1_immediate_array", "ff8300", 1, accept(4, 3n, 2)),
  vector("at_offset_1_non_minimal", "ff1817", 1, refuse(reasons.nonMinimal)),
  vector("at_offset_1_truncated", "ff1a0001", 1, refuse(reasons.truncated)),
  vector("at_offset_1_reserved", "001c00", 1, refuse(reasons.reserved)),
  vector("at_offset_1_indefinite", "009f00", 1, refuse(reasons.indefinite)),
];

const outOfRangeVectors = [
  vector("empty_at_offset_0", "", 0, refuse(reasons.outOfRange), {
    verdict: "refuse",
  }),
  vector("offset_at_length", "00", 1, refuse(reasons.outOfRange)),
  vector("offset_past_length", "00", 2, refuse(reasons.outOfRange)),
  vector("offset_far_past_length", "1818", 9, refuse(reasons.outOfRange)),
  vector("offset_negative", "00", -1, refuse(reasons.outOfRange)),
  vector("offset_negative_two", "1818", -2, refuse(reasons.outOfRange)),
];

export const vectors = Object.freeze([
  ...minimalVectors,
  ...nonMinimalVectors,
  ...reservedVectors,
  ...indefiniteEmptyVectors,
  ...truncatedVectors,
  ...offsetVectors,
  ...outOfRangeVectors,
]);
