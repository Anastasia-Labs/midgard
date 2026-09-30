import { makeFuzzRng } from "@al-ft/midgard-test-support/plutus-data-fuzz";
import { describe, expect, it } from "vitest";

import {
  assertCanonicalCbor,
  decodeSingleCbor,
  encodeCbor,
  skipCborItem,
} from "../src/codec/cbor.js";
import { MidgardTxCodecError } from "../src/codec/errors.js";
import {
  legacyAssertCanonicalCbor,
  legacyDecodeSingleCbor,
  legacyEncodeCbor,
  legacySkipCborItem,
} from "./cbor-iterative.legacy-codec.js";
import {
  describeValue,
  randomDecoderInput,
  randomEncodableValue,
} from "./cbor-iterative.random-values.js";

/**
 * The iterative codec against the recursive one it replaced (vendored
 * verbatim, with `cborg` as its engine): the same accept set, the same value
 * shapes, the same bytes and the same error code, message and detail.
 */

const SEEDED_CASES = 20_000;

const outcome = (run: () => unknown): string => {
  try {
    const result = run();
    return `ok ${
      Buffer.isBuffer(result)
        ? `Buffer:${result.toString("hex")}`
        : typeof result === "object" && result !== null && "start" in result
          ? JSON.stringify(result)
          : describeValue(result)
    }`;
  } catch (e) {
    if (e instanceof MidgardTxCodecError) {
      return `err ${e.code} ${e.message} | ${String(e.detail)}`;
    }
    return `raw ${String(e)}`;
  }
};

const expectSameDecode = (bytes: Uint8Array, label: string): void => {
  const hex = Buffer.from(bytes).toString("hex");
  expect(
    outcome(() => decodeSingleCbor(bytes)),
    `${label} decode ${hex}`,
  ).toBe(outcome(() => legacyDecodeSingleCbor(bytes)));
  expect(
    outcome(() => assertCanonicalCbor(bytes, "field")),
    `${label} assert ${hex}`,
  ).toBe(outcome(() => legacyAssertCanonicalCbor(bytes, "field")));
  const offset = bytes.length === 0 ? 0 : bytes.length >> 1;
  expect(
    outcome(() => skipCborItem(bytes, offset)),
    `${label} skip@${offset} ${hex}`,
  ).toBe(outcome(() => legacySkipCborItem(bytes, offset)));
};

const expectSameEncode = (value: unknown, label: string): void => {
  expect(
    outcome(() => encodeCbor(value)),
    label,
  ).toBe(outcome(() => legacyEncodeCbor(value)));
};

const DECODE_EDGES: readonly string[] = [
  "",
  "00",
  "17",
  "1817",
  "1818",
  "190017",
  "19ffff",
  "1a0000ffff",
  "1a00010000",
  "1b00000000ffffffff",
  "1b0000000100000000",
  "1b001fffffffffffff",
  "1b0020000000000000",
  "1bffffffffffffffff",
  "3b001ffffffffffffe",
  "3b001fffffffffffff",
  "3bffffffffffffffff",
  "20",
  "37",
  "3818",
  "40",
  "4100",
  "5f41ff",
  "5b0020000000000000",
  "5b0000000000000001",
  "60",
  "6161",
  "63efbbbf",
  "64efbbbf61",
  "62c328",
  "63eda080",
  "80",
  "8100",
  "820001",
  "8200",
  "9f00ff",
  "9b0000000000000001",
  "a0",
  "a10000",
  "a2000001 00",
  "a201000000",
  "a200000000",
  "a2616200616100",
  "a2616100616200",
  "a20000181800",
  "a21818000000",
  "a20000616100",
  "a18000",
  "a1a000",
  "a1810000",
  "a2810000820000",
  "a2820000810000",
  "c000",
  "d87980",
  "81d87980",
  "f4",
  "f5",
  "f6",
  "f7",
  "f8ff",
  "f93c00",
  "fa3f800000",
  "fb3ff0000000000000",
  "ff",
  "1c",
  "1d",
  "1e",
  "1f",
  "0000",
  "8100ff",
  "83f4f5f6",
];

describe("iterative CBOR decoder vs the recursive cborg-backed decoder", () => {
  it("agrees on the edge list", () => {
    for (const hex of DECODE_EDGES) {
      expectSameDecode(Buffer.from(hex.replace(/ /g, ""), "hex"), "edge");
    }
  });

  it("agrees on non-Buffer Uint8Array input and returns plain byte copies", () => {
    const input = new Uint8Array([0x82, 0x41, 0xaa, 0x61, 0x61]);
    expectSameDecode(input, "u8");
    const decoded = decodeSingleCbor(Buffer.from(input)) as unknown[];
    expect(Object.getPrototypeOf(decoded[0])).toBe(Uint8Array.prototype);
  });

  it(`agrees on ${SEEDED_CASES} seeded inputs`, () => {
    let accepted = 0;
    for (let i = 0; i < SEEDED_CASES; i += 1) {
      const bytes = randomDecoderInput(makeFuzzRng(0xdec0de00 + i));
      expectSameDecode(bytes, `seed ${i}`);
      try {
        decodeSingleCbor(bytes);
        accepted += 1;
      } catch {
        // rejections are compared above
      }
    }
    // Both polarities must be well represented for the comparison to mean much.
    expect(accepted).toBeGreaterThan(SEEDED_CASES / 5);
    expect(accepted).toBeLessThan((SEEDED_CASES * 4) / 5);
  });
});

const circularArray = (): unknown[] => {
  const array: unknown[] = [1];
  array.push(array);
  return array;
};

const ENCODE_EDGES: readonly [string, () => unknown][] = [
  ["uint boundaries", () => [0, 23, 24, 255, 256, 65535, 65536, 2 ** 32]],
  ["negative boundaries", () => [-1, -24, -25, -256, -257, -(2 ** 32) - 1]],
  ["floats", () => [0.5, -0, NaN, Infinity, -Infinity, 2 ** 53, 1e300]],
  ["bigint bounds", () => [2n ** 64n - 1n, -(2n ** 64n), 2n ** 53n]],
  ["bigint too large", () => 2n ** 64n],
  ["bigint too small", () => -(2n ** 64n) - 1n],
  ["range before unsupported", () => [2n ** 64n, Symbol("s")]],
  ["map range before sibling", () => [new Map([[1, 2n ** 64n]]), Symbol("s")]],
  ["unsupported inside map", () => [new Map([[1, Symbol("s")]]), 2n ** 64n]],
  [
    "range in two-key map key",
    () =>
      new Map<unknown, unknown>([
        [2n ** 64n, 1],
        ["a", Symbol("s")],
      ]),
  ],
  ["range in one-key map key", () => new Map([[2n ** 64n, 1]])],
  [
    "complex key sort",
    () =>
      new Map<unknown, unknown>([
        [[1], 1],
        ["a", 2],
      ]),
  ],
  ["complex key alone", () => new Map([[[1], 1]])],
  [
    "map-in-map key",
    () =>
      new Map<unknown, unknown>([
        [new Map([[1, 1]]), 1],
        [2, 2],
      ]),
  ],
  [
    "empty container keys",
    () =>
      new Map<unknown, unknown>([
        [[], 1],
        [new Map(), 2],
        [0, 3],
        [{}, 4],
      ]),
  ],
  [
    "equal key bytes",
    () =>
      new Map<unknown, unknown>([
        [1, "a"],
        [1n, "b"],
        [2, "c"],
      ]),
  ],
  [
    "string and byte keys",
    () =>
      new Map<unknown, unknown>([
        ["ab", 1],
        ["a", 2],
        [new Uint8Array([0x61]), 3],
        ["", 4],
        [new Uint8Array(0), 5],
      ]),
  ],
  ["object key order", () => ({ b: 1, aa: 2, a: 3, 10: 4, 9: 5 })],
  [
    "null-prototype object",
    () => Object.assign(Object.create(null) as object, { x: 1 }),
  ],
  [
    "class instance",
    () =>
      new (class Probe {
        value = [1, 2];
      })(),
  ],
  [
    "string tags",
    () => [
      "",
      "\ud800",
      "a\udc00b",
      "\u{1f600}",
      "x".repeat(24),
      "y".repeat(70000),
    ],
  ],
  [
    "byte views",
    () => [
      new DataView(new ArrayBuffer(3)),
      new Uint16Array([1, 2]),
      new ArrayBuffer(2),
      Buffer.from("ff", "hex"),
      new Uint8Array(300),
    ],
  ],
  ["simple values", () => [null, undefined, true, false, [], {}, new Map()]],
  ["array holes", () => [1, , 3]], // eslint-disable-line no-sparse-arrays
  ["circular array", circularArray],
  [
    "circular map value",
    () => {
      const m = new Map<unknown, unknown>();
      m.set(1, m);
      return m;
    },
  ],
  [
    "circular map key",
    () => {
      const m = new Map<unknown, unknown>();
      m.set(m, 1);
      return m;
    },
  ],
  [
    "circular object in array",
    () => {
      const o: Record<string, unknown> = {};
      o.self = [o];
      return [o];
    },
  ],
  [
    "shared non-circular child",
    () => {
      const c = [1, new Map([[1, 2]])];
      return [
        c,
        c,
        new Map([
          [1, c],
          [2, c],
        ]),
      ];
    },
  ],
  ["unsupported types", () => [Symbol("s")]],
  ["function", () => new Map([[1, () => 1]])],
  ["date", () => ({ d: new Date(0) })],
  ["set", () => [new Set([1])]],
  ["top-level undefined", () => undefined],
];

describe("iterative CBOR encoder vs cborg rfc8949 encode", () => {
  it("agrees on the edge list", () => {
    for (const [label, make] of ENCODE_EDGES) {
      expectSameEncode(make(), label);
    }
  });

  it(`agrees on ${SEEDED_CASES} seeded values`, () => {
    let accepted = 0;
    for (let i = 0; i < SEEDED_CASES; i += 1) {
      const value = randomEncodableValue(makeFuzzRng(0xe4c0de00 + i));
      expectSameEncode(value, `seed ${i}`);
      try {
        encodeCbor(value);
        accepted += 1;
      } catch {
        // rejections are compared above
      }
    }
    expect(accepted).toBeGreaterThan(SEEDED_CASES / 5);
    expect(accepted).toBeLessThan(SEEDED_CASES);
  });

  it("round-trips every decodable encoding", () => {
    for (let i = 0; i < 2_000; i += 1) {
      const value = randomEncodableValue(
        makeFuzzRng(0x7017_0000 + i),
        0,
        false,
      );
      let bytes: Buffer;
      try {
        bytes = encodeCbor(value);
      } catch {
        continue;
      }
      expect(outcome(() => encodeCbor(decodeSingleCbor(bytes)))).toBe(
        outcome(() => legacyEncodeCbor(legacyDecodeSingleCbor(bytes))),
      );
    }
  });
});
