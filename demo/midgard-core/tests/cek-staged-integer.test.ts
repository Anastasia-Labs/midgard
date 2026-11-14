import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isCanonicalMidgardDataStream } from "../src/canonical-plutus-data-stream.js";
import { midgardCekIntegerMemoryWords } from "../src/cek-memory-size.js";
import {
  addMidgardCekLimbs,
  addMidgardCekSigned,
  cmpMidgardCekMagnitude,
  encodeMidgardCekCanonicalIntegerCbor,
  fromMidgardCekLimbs,
  MIDGARD_CEK_STAGED_LIMB_BYTES,
  midgardCekCanonicalIntegerMagnitudeWords,
  midgardCekCanonicalIntegerOf,
  midgardCekCanonicalIntegerValue,
  midgardCekIntegerCborFromMagnitude,
  midgardCekSignedLimbs,
  parseMidgardCekCanonicalIntegerCbor,
  subMidgardCekLimbs,
  subtractMidgardCekSigned,
  toMidgardCekLimbs,
} from "../src/cek-staged-integer.js";

const power = (bits: number) => 1n << BigInt(bits);
const repeated = (size: number, byte: number) =>
  size === 0 ? 0n : BigInt(`0x${Buffer.alloc(size, byte).toString("hex")}`);
const counting = (size: number) =>
  BigInt(
    `0x${Buffer.from(Array.from({ length: size }, (_, i) => ((i + 1) % 255) + 1)).toString("hex")}`,
  );
const LARGEST = power(262_143) - 1n;
const SMALLEST = -power(262_143);

const cborOf = (value: bigint) =>
  encodeMidgardCekCanonicalIntegerCbor(midgardCekCanonicalIntegerOf(value));
const limbsOf = (value: bigint) =>
  midgardCekSignedLimbs(parseMidgardCekCanonicalIntegerCbor(cborOf(value)));

const accepted: bigint[] = [
  0n,
  1n,
  23n,
  24n,
  255n,
  256n,
  65_535n,
  65_536n,
  power(32) - 1n,
  power(32),
  power(64) - 1n,
  power(64),
  power(64) + 1n,
  power(512) - 1n,
  power(512),
  ...[64, 65, 128, 129].flatMap((size) => [
    counting(size),
    repeated(size, 0xff),
    power(8 * size - 1),
  ]),
  LARGEST,
  LARGEST - 1n,
].flatMap((value) => (value === 0n ? [0n] : [value, -value]));
accepted.push(SMALLEST, -power(64), -power(4_096), -power(8 * 4_096));

describe("canonical integer CBOR", () => {
  it("reads back every value it writes, as canonical Data", () => {
    for (const value of accepted) {
      const cbor = cborOf(value);
      expect(cbor.toString("hex")).toBe(Data.to(value));
      expect(isCanonicalMidgardDataStream(cbor)).toBe(true);
      const parsed = parseMidgardCekCanonicalIntegerCbor(cbor);
      expect(midgardCekCanonicalIntegerValue(parsed)).toBe(value);
      expect(encodeMidgardCekCanonicalIntegerCbor(parsed)).toEqual(cbor);
      expect(midgardCekCanonicalIntegerMagnitudeWords(parsed)).toBe(
        midgardCekIntegerMemoryWords(value),
      );
    }
  });

  it("spells 64-byte bignums definitely and 65-byte ones in 64-byte segments", () => {
    expect(cborOf(repeated(64, 0xab)).subarray(0, 3)).toEqual(
      Buffer.from("c25840", "hex"),
    );
    const longer = cborOf(-repeated(65, 0xab) - 1n);
    expect(longer.subarray(0, 4)).toEqual(Buffer.from("c35f5840", "hex"));
    expect(longer.subarray(68, 70)).toEqual(Buffer.from("41ab", "hex"));
    expect(longer[longer.length - 1]).toBe(0xff);
  });

  const segment = (byte: number) =>
    Buffer.concat([Buffer.from("5840", "hex"), Buffer.alloc(64, byte)]);
  const refused: [string, Buffer][] = [
    ["a leading zero", Buffer.from("c249000100000000000000", "hex")],
    ["an eight-byte bignum", Buffer.from("c2480100000000000000", "hex")],
    ["a one-byte bignum", Buffer.from("c24105", "hex")],
    [
      "a definite form above 64 bytes",
      Buffer.concat([Buffer.from("c25841", "hex"), Buffer.alloc(65, 1)]),
    ],
    [
      "a non-minimal definite head",
      Buffer.concat([Buffer.from("c2580a", "hex"), Buffer.alloc(10, 1)]),
    ],
    [
      "a 63-byte middle segment",
      Buffer.concat([
        Buffer.from("c25f583f", "hex"),
        Buffer.alloc(63, 1),
        Buffer.from("42aaaaff", "hex"),
      ]),
    ],
    [
      "an empty final segment",
      Buffer.concat([
        Buffer.from("c25f", "hex"),
        segment(1),
        Buffer.from("40ff", "hex"),
      ]),
    ],
    [
      "a missing break",
      Buffer.concat([
        Buffer.from("c25f", "hex"),
        segment(1),
        Buffer.from("4101", "hex"),
      ]),
    ],
    [
      "a full segment over the break",
      Buffer.concat([
        Buffer.from("c25f", "hex"),
        segment(1),
        Buffer.from("5840", "hex"),
        Buffer.alloc(63, 1),
        Buffer.from("ff", "hex"),
      ]),
    ],
    [
      "an indefinite form of 64 bytes",
      Buffer.concat([
        Buffer.from("c25f", "hex"),
        segment(1),
        Buffer.from("ff", "hex"),
      ]),
    ],
    [
      "an oversized final segment",
      Buffer.concat([
        Buffer.from("c25f5841", "hex"),
        Buffer.alloc(65, 1),
        Buffer.from("ff", "hex"),
      ]),
    ],
    [
      "a short segment before the last",
      Buffer.concat([
        Buffer.from("c25f4101", "hex"),
        segment(1),
        Buffer.from("ff", "hex"),
      ]),
    ],
    [
      "a non-minimal segment head",
      Buffer.concat([
        Buffer.from("c25f", "hex"),
        segment(1),
        Buffer.from("5802aaaaff", "hex"),
      ]),
    ],
    [
      "a long segment head",
      Buffer.concat([
        Buffer.from("c25f590041", "hex"),
        Buffer.alloc(65, 1),
        Buffer.from("ff", "hex"),
      ]),
    ],
    ["a non-minimal one-byte head", Buffer.from("1805", "hex")],
    ["a non-minimal one-byte head at the boundary", Buffer.from("1817", "hex")],
    [
      "a non-minimal negative one-byte head at the boundary",
      Buffer.from("3817", "hex"),
    ],
    ["a non-minimal two-byte head", Buffer.from("1900ff", "hex")],
    ["a non-minimal four-byte head", Buffer.from("1a0000ffff", "hex")],
    ["a non-minimal eight-byte head", Buffer.from("1b00000000ffffffff", "hex")],
    ["a non-minimal negative head", Buffer.from("3805", "hex")],
    ["a reserved head", Buffer.from("1c", "hex")],
    ["trailing bytes", Buffer.from("0100", "hex")],
    [
      "trailing bytes after a bignum",
      Buffer.concat([cborOf(power(64)), Buffer.from("00", "hex")]),
    ],
    ["another tag", Buffer.from("c449010000000000000000", "hex")],
    ["a byte string", Buffer.from("4101", "hex")],
    ["nothing", Buffer.alloc(0)],
  ];
  it.each(refused)("refuses %s", (_, cbor) => {
    expect(() => parseMidgardCekCanonicalIntegerCbor(cbor)).toThrow(
      /non-canonical integer CBOR/u,
    );
  });

  it("refuses a leading zero in the first segment", () => {
    const honest = cborOf(counting(65));
    const tampered = Buffer.from(honest);
    tampered[4] = 0;
    expect(() => parseMidgardCekCanonicalIntegerCbor(tampered)).toThrow(
      /leading zero/u,
    );
  });
});

describe("limb arithmetic", () => {
  const check = (left: bigint, right: bigint) => {
    const a = limbsOf(left);
    const b = limbsOf(right);
    expect(
      midgardCekIntegerCborFromMagnitude(addMidgardCekSigned(a, b)),
    ).toEqual(cborOf(left + right));
    expect(
      midgardCekIntegerCborFromMagnitude(subtractMidgardCekSigned(a, b)),
    ).toEqual(cborOf(left - right));
    expect(
      midgardCekIntegerCborFromMagnitude(subtractMidgardCekSigned(b, a)),
    ).toEqual(cborOf(right - left));
  };

  it("round-trips limbs and drops zero top limbs", () => {
    for (const size of [0, 1, 255, 511, 512, 513, 1_023, 1_024, 1_025, 8_192]) {
      const bytes =
        size === 0
          ? Buffer.alloc(0)
          : Buffer.from(
              counting(size)
                .toString(16)
                .padStart(2 * size, "0"),
              "hex",
            );
      const limbs = toMidgardCekLimbs(bytes);
      expect(limbs.length).toBe(
        Math.ceil(size / MIDGARD_CEK_STAGED_LIMB_BYTES),
      );
      expect(fromMidgardCekLimbs(limbs)).toEqual(bytes);
    }
    expect(fromMidgardCekLimbs([5n, 0n, 0n])).toEqual(Buffer.from("05", "hex"));
    expect(fromMidgardCekLimbs([0n, 0n])).toEqual(Buffer.alloc(0));
    expect(addMidgardCekLimbs([], [])).toEqual([]);
    expect(subMidgardCekLimbs([7n], [7n])).toEqual([]);
  });

  it("refuses a larger subtrahend and a negative zero", () => {
    expect(() => subMidgardCekLimbs([1n], [2n])).toThrow(RangeError);
    expect(() => subMidgardCekLimbs([0n, 1n], [1n, 1n])).toThrow(RangeError);
    expect(() =>
      midgardCekIntegerCborFromMagnitude({ negative: true, magnitude: [] }),
    ).toThrow(RangeError);
  });

  it.each([
    [0n, 0n],
    [0n, 5n],
    [5n, 0n],
    [-5n, 5n],
    [-1n, -1n],
    [1n, -2n],
    [power(64), -1n],
    [-power(64), 1n],
  ])("adds and subtracts %s and %s", (left, right) => check(left, right));

  it.each([511, 512, 513, 1_023, 1_024, 1_025])(
    "adds and subtracts at %i-byte limb boundaries",
    (size) => {
      check(counting(size), counting(size));
      check(counting(size), -counting(size - 1));
      check(power(8 * size), -1n);
      check(-power(8 * size), 1n);
      check(power(8 * size) - 1n, 1n);
    },
  );

  it("carries through all-0xff chains and negative growth", () => {
    check(repeated(1_536, 0xff), 1n);
    check(repeated(1_536, 0xff), repeated(1_536, 0xff));
    check(-repeated(1_536, 0xff), -1n);
    check(-repeated(1_024, 0xff), repeated(1_536, 0xff));
    check(-(power(4_096) - 1n), -1n);
    check(-power(4_096), -power(4_096));
  });

  it("adds and subtracts at 8,192 and 32,768 bytes", () => {
    check(counting(8_192), -repeated(8_191, 0x5a));
    check(LARGEST - 1n, 1n);
    check(SMALLEST + 1n, -1n);
    check(repeated(32_767, 0xff), 1n);
    check(counting(32_768), -repeated(32_768, 0x7f));
  });

  it("compares magnitudes", () => {
    const sign = (value: bigint) => (value < 0n ? -1 : value > 0n ? 1 : 0);
    const abs = (value: bigint) => (value < 0n ? -value : value);
    for (const [left, right] of [
      [0n, 0n],
      [0n, 1n],
      [-3n, 2n],
      [power(4_096), power(4_096) - 1n],
      [counting(600), counting(600) + 256n],
    ] as const) {
      expect(
        cmpMidgardCekMagnitude(
          limbsOf(left).magnitude,
          limbsOf(right).magnitude,
        ),
      ).toBe(sign(abs(left) - abs(right)));
    }
  });
});
