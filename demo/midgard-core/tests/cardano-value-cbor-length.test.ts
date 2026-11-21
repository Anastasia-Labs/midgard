import { describe, expect, it } from "vitest";

import { MidgardTxCodecError } from "../src/codec/errors.js";
import {
  midgardCardanoValueCborLength,
  type MidgardValue,
  midgardValueToCmlValue,
} from "../src/codec/index.js";

// `cardano_value_size` feeds the value-size cap and the withdrawal payout
// size, so the shape-derived length must equal the byte length CML encodes.
const cmlLength = (value: MidgardValue): number => {
  const cml = midgardValueToCmlValue(value);
  try {
    return cml.to_cbor_bytes().length;
  } finally {
    cml.free();
  }
};

// Every CBOR head boundary an unsigned argument can cross.
const UINT_BOUNDARIES = [
  0n,
  1n,
  23n,
  24n,
  255n,
  256n,
  65_535n,
  65_536n,
  0xffff_ffffn,
  0x1_0000_0000n,
  0xffff_ffff_ffff_ffffn,
] as const;
const QUANTITY_BOUNDARIES = UINT_BOUNDARIES.filter((value) => value > 0n);
const ASSET_NAME_LENGTHS = [0, 1, 23, 24, 32] as const;

const policyHex = (index: number): string =>
  index.toString(16).padStart(8, "0").repeat(7);
const assetNameHex = (index: number, length: number): string =>
  Buffer.alloc(length, index).toString("hex");
const indexedNameHex = (index: number): string =>
  index.toString(16).padStart(4, "0");

const value = (
  lovelace: bigint,
  policies: readonly (readonly [
    string,
    readonly (readonly [string, bigint])[],
  ])[],
): MidgardValue => ({
  lovelace,
  assets: new Map(
    policies.map(([policy, assets]) => [policy, new Map(assets)] as const),
  ),
});

// A deterministic generator, so a failing case reproduces.
const random = (seed: number): (() => number) => {
  let state = seed >>> 0;
  return () => {
    state = (Math.imul(state, 1_664_525) + 1_013_904_223) >>> 0;
    return state / 0x1_0000_0000;
  };
};

describe("midgardCardanoValueCborLength", () => {
  it("equals CML's length for lovelace-only values at every coin head boundary", () => {
    for (const lovelace of UINT_BOUNDARIES) {
      const candidate = value(lovelace, []);
      expect(midgardCardanoValueCborLength(candidate)).toBe(
        cmlLength(candidate),
      );
    }
  });

  it("equals CML's length for one asset at every name length and quantity head boundary", () => {
    for (const lovelace of [0n, 24n, 0x1_0000_0000n]) {
      for (const nameLength of ASSET_NAME_LENGTHS) {
        for (const quantity of QUANTITY_BOUNDARIES) {
          const candidate = value(lovelace, [
            [policyHex(1), [[assetNameHex(7, nameLength), quantity]]],
          ]);
          expect(midgardCardanoValueCborLength(candidate)).toBe(
            cmlLength(candidate),
          );
        }
      }
    }
  });

  it("equals CML's length across the map head boundaries for policies and asset names", () => {
    for (const policyCount of [1, 2, 23, 24, 25]) {
      for (const assetCount of [1, 23, 24, 25]) {
        const candidate = value(
          2_000_000n,
          Array.from({ length: policyCount }, (_, policy) => [
            policyHex(policy),
            Array.from(
              { length: assetCount },
              (_, asset) => [indexedNameHex(asset), BigInt(asset + 1)] as const,
            ),
          ]),
        );
        expect(midgardCardanoValueCborLength(candidate)).toBe(
          cmlLength(candidate),
        );
      }
    }
  });

  it("equals CML's length over generated values", () => {
    const next = random(0x5eed);
    const pick = <T>(items: readonly T[]): T =>
      items[Math.floor(next() * items.length)]!;
    for (let sample = 0; sample < 400; sample += 1) {
      const policyCount = pick([0, 0, 1, 1, 2, 3, 5, 12, 24, 30]);
      const candidate = value(
        // A boundary or just past it; the top boundary is u64's maximum.
        pick(UINT_BOUNDARIES.slice(0, -1)) + BigInt(Math.floor(next() * 3)),
        Array.from({ length: policyCount }, () => [
          policyHex(Math.floor(next() * 0xffff_ffff)),
          Array.from({ length: pick([1, 1, 2, 4, 23, 24, 26]) }, () => [
            assetNameHex(Math.floor(next() * 256), pick(ASSET_NAME_LENGTHS)),
            pick(QUANTITY_BOUNDARIES),
          ]),
        ]),
      );
      expect(midgardCardanoValueCborLength(candidate)).toBe(
        cmlLength(candidate),
      );
    }
  });

  it("counts two spellings of one key once and leaves out a policy with no assets, as CML does", () => {
    const candidate = value(5n, [
      [policyHex(3).toUpperCase(), [[assetNameHex(1, 4), 9n]]],
      [
        policyHex(3),
        [
          ["aabb", 1n],
          ["AABB", 70_000n],
        ],
      ],
      [policyHex(4), []],
    ]);
    expect(midgardCardanoValueCborLength(candidate)).toBe(cmlLength(candidate));
    expect(midgardCardanoValueCborLength(value(5n, [[policyHex(4), []]]))).toBe(
      1,
    );
  });

  it("refuses integers outside the unsigned 64-bit range", () => {
    expect(() =>
      midgardCardanoValueCborLength(value(0x1_0000_0000_0000_0000n, [])),
    ).toThrow(MidgardTxCodecError);
    expect(() =>
      midgardCardanoValueCborLength(
        value(1n, [[policyHex(1), [["", 0x1_0000_0000_0000_0000n]]]]),
      ),
    ).toThrow(MidgardTxCodecError);
  });
});
