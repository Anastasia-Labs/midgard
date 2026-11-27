/**
 * The Data values the serialise loop (`cek_serialise_loop_v1`) is driven
 * over, as the canonical CBOR `serialiseData` gives them: the vitest suite
 * runs their hops through the TypeScript twin, and
 * `scripts/generate-cek-serialise-loop-v1-goldens.mjs` renders the same hops
 * as Aiken rows the loop validator must accept with the same states.
 *
 * Every value is built by hand from canonical heads, so a builder bug shows up
 * as a value the canonical traverse refuses.
 *
 * This module holds data only and imports nothing from the package.
 */

/** The canonical head of `major` with argument `value`, as hex. */
export const head = (major, value) => {
  const argument = BigInt(value);
  const top = major << 5;
  const byte = (bits) => bits.toString(16).padStart(2, "0");
  if (argument < 24n) return byte(top + Number(argument));
  for (const [extra, width] of [
    [24, 1],
    [25, 2],
    [26, 4],
    [27, 8],
  ]) {
    if (argument < 1n << BigInt(8 * width))
      return byte(top + extra) + argument.toString(16).padStart(2 * width, "0");
  }
  throw new RangeError("argument past 64 bits");
};

/** A non-negative integer below 2^64. */
export const int = (value) => head(0, value);

/** `length` bytes of a non-repeating filler, as hex. */
export const filler = (length, seed = 0) =>
  Buffer.from(
    Array.from({ length }, (_, index) => (index * 7 + 3 + seed) % 256),
  ).toString("hex");

/** A byte string of `payload` hex: definite up to 64 bytes, else chunked. */
export const bytesOf = (payload) => {
  const length = payload.length / 2;
  if (length <= 64) return head(2, length) + payload;
  let chunks = "";
  for (let offset = 0; offset < payload.length; offset += 128) {
    const chunk = payload.slice(offset, offset + 128);
    chunks += head(2, chunk.length / 2) + chunk;
  }
  return `5f${chunks}ff`;
};

export const bytes = (length, seed = 0) => bytesOf(filler(length, seed));

/** A tag 2 bignum of `length` magnitude bytes (at least 9, no leading 0). */
export const bignum = (length) => `c2${bytesOf(`01${filler(length - 1)}`)}`;

export const list = (items) =>
  items.length === 0 ? "80" : `9f${items.join("")}ff`;

/** Constructor `index`: tags 121..127, 1280..1400, else tag 102. */
export const constr = (index, fields) => {
  if (index < 7) return head(6, 121 + index) + list(fields);
  if (index < 128) return head(6, 1280 + index - 7) + list(fields);
  return `d866${head(4, 2)}${int(index)}${list(fields)}`;
};

export const map = (pairs) =>
  head(5, pairs.length) + pairs.map(([key, value]) => key + value).join("");

const range = (count) => Array.from({ length: count }, (_, index) => index);

const nested = (depth, core) =>
  range(depth).reduce((inner) => list([inner]), core);

/** A transaction-input-like record: `Constr 0 [Constr 0 [id], index]`. */
const input = (seed) => constr(0, [constr(0, [bytes(32, seed)]), int(seed)]);

/** A list of 64-byte strings and one shorter one, `total` bytes in all. */
const exactly = (total) => {
  const items = Math.floor((total - 2) / 66);
  const rest = total - 2 - 66 * items;
  return list([
    ...range(items).map((seed) => bytes(64, seed)),
    bytes(rest - 2),
  ]);
};

const mixed = () =>
  constr(3, [
    int(1_000_000),
    bytes(100),
    map([
      [int(1), bytes(5)],
      [bytes(3), list([int(2), int(3)])],
    ]),
    constr(200, [bignum(70), list([])]),
    constr(9, [`c3${bytesOf(`02${filler(15)}`)}`]),
    list([constr(0, []), int(23), int(24)]),
  ]);

/**
 * A value too long for one Aiken literal, as parts: hex, or `[hex, count]`
 * for `count` copies of `hex`.
 */
export const expand = (parts) =>
  parts
    .map((part) => (typeof part === "string" ? part : part[0].repeat(part[1])))
    .join("");

/** A byte string of `length` bytes 0x5a, as parts. */
const leafParts = (length) => {
  const full = Math.floor(length / 64);
  const rest = length - 64 * full;
  return [
    "5f",
    [`5840${"5a".repeat(64)}`, full],
    rest === 0 ? "" : head(2, rest) + "5a".repeat(rest),
    "ff",
  ];
};

/**
 * `name`, the value's CBOR as parts and whether it is one of the honest
 * maximum shapes the loop must still end within its window and fee cap.
 */
const shapes = [
  { name: "small_mixed", parts: [mixed()], maximum: false },
  {
    name: "list_of_128_items",
    parts: [list(range(128).map((i) => int(i * 997)))],
    maximum: false,
  },
  { name: "nested_128_deep", parts: [nested(128, int(7))], maximum: false },
  {
    name: "eight_input_records",
    parts: [list(range(8).map(input))],
    maximum: false,
  },
  { name: "past_the_bound_9216", parts: [exactly(9_216)], maximum: false },
  { name: "bignum_of_5000_bytes", parts: [bignum(5_000)], maximum: false },
  { name: "leaf_of_20000_bytes", parts: [bytes(20_000)], maximum: false },
  {
    name: "flat_11700_empty_constrs",
    parts: ["9f", [constr(0, []), 11_700], "ff"],
    maximum: true,
  },
  { name: "leaf_of_370000_bytes", parts: leafParts(370_000), maximum: true },
];

export const values = shapes.map((shape) => ({
  ...shape,
  hex: expand(shape.parts),
}));
