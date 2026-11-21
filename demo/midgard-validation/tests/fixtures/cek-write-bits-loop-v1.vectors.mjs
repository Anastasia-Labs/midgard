/**
 * The writeBits steps the writeBits loop (`cek_write_bits_loop_v1`) is driven
 * over: an input, an index list and the value written. The vitest suite runs
 * their hops through the TypeScript twin, and
 * `scripts/generate-cek-write-bits-loop-v1-goldens.mjs` renders the same hops
 * as Aiken rows the loop validator must accept with the same states.
 *
 * A step with an index outside its input is a failure, whose walk ends on the
 * hop that opens that index.
 *
 * This module holds data only and imports nothing from the package.
 */

/** `length` bytes of a fixed pattern from `salt`. */
export const pattern = (length, salt = 0) =>
  Buffer.from(
    Uint8Array.from({ length }, (_, index) => (index * 13 + salt) & 0xff),
  );

/** `count` indices spread over an input of `length` bytes, from `salt`. */
export const spread = (count, length, salt = 0) =>
  Array.from({ length: count }, (_, index) =>
    BigInt((index * 37 + salt) % (8 * length)),
  );

/** `indices` with `bad` at position `at`. */
const withBad = (indices, at, bad) =>
  indices.map((index, position) => (position === at ? bad : index));

/** The highest `count` bits of an input of `length` bytes, from the top. */
const top = (count, length) =>
  Array.from({ length: count }, (_, index) =>
    BigInt(8 * length - 1 - ((index * 5) % (8 * length))),
  );

export const steps = [
  {
    name: "nine_of_one_byte",
    length: 1,
    indices: [0n, 7n, 3n, 3n, 1n, 6n, 2n, 5n, 0n],
    set: true,
  },
  {
    name: "sixty_five_of_64_bytes",
    length: 64,
    indices: spread(65, 64),
    set: true,
  },
  {
    name: "clear_200_of_512_bytes",
    length: 512,
    indices: spread(200, 512, 5),
    set: false,
  },
  {
    name: "top_300_of_4096_bytes",
    length: 4_096,
    indices: top(300, 4_096),
    set: true,
  },
  {
    name: "spread_400_of_4096_bytes",
    length: 4_096,
    indices: spread(400, 4_096, 9),
    set: false,
  },
  {
    name: "fail_past_64_bytes",
    length: 64,
    indices: withBad(spread(100, 64), 70, 512n),
    set: true,
  },
  {
    name: "fail_negative_first",
    length: 1,
    indices: withBad(spread(9, 1), 0, -1n),
    set: true,
  },
  {
    name: "fail_past_4096_bytes",
    length: 4_096,
    indices: withBad(spread(300, 4_096), 250, 32_768n),
    set: false,
  },
].map((step) => ({
  ...step,
  input: pattern(step.length, step.indices.length),
}));
