import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  FIELD_CARRIAGE_CONSTRUCTOR_INDEXES,
  FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_CONSTRUCTOR_INDEXES,
  FIELD_VIEW_CONSTRUCTOR_INDEXES,
  FieldCarriage,
  FieldPreimageCertificate,
  FieldPreimageCertificateMintRedeemer,
  FieldView,
} from "../src/native-tx-field-access-v1.js";

/**
 * The TypeScript half of the §8.6/§8.8 wire golden channel.
 *
 * The generator emits the fixture from `dist/`; this suite recomputes every
 * vector from `src/`, so a schema edit that has not been rebuilt into a fixture
 * fails here rather than shipping. The Aiken half —
 * `onchain/aiken/lib/midgard/native-tx-carriage-wire-v1-golden.test.ak` —
 * decodes the same bytes and re-serialises them under the fork runner. Neither
 * half is authoritative on its own: the fixture is what they are both held to.
 */

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/native-tx-carriage-wire-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as {
  readonly schema: string;
  readonly version: number;
  readonly specDocument: string;
  readonly generator: string;
  readonly constructorIndexes: Readonly<
    Record<string, Readonly<Record<string, number>>>
  >;
  readonly vectors: readonly {
    readonly label: string;
    readonly aikenType: string;
    readonly value: unknown;
    readonly cborHex: string;
  }[];
  readonly negativeVectors: readonly {
    readonly label: string;
    readonly aikenType: string;
    readonly cborHex: string;
    readonly reason: string;
    readonly rejectedBy: {
      readonly aiken: "cbor-parse" | "data-cast";
      readonly typescript: "throws" | "tolerates-trailing-bytes";
    };
  }[];
};

const byLabel = (label: string) => {
  const vector = golden.vectors.find((entry) => entry.label === label);
  if (vector === undefined) {
    throw new Error(`the fixture no longer carries a vector named ${label}`);
  }
  return vector;
};

const inlinePreimageOf = (label: string): string => {
  const { value } = byLabel(label);
  const preimage = (value as { Inline?: { preimage?: unknown } }).Inline
    ?.preimage;
  if (typeof preimage !== "string") {
    throw new Error(`${label} is no longer an Inline carriage vector`);
  }
  return preimage;
};

/**
 * The fixture stores bigints as `"123n"` strings, because JSON has no bigint.
 * Rehydrating rather than storing decimal numbers keeps the distinction between
 * a §8.8 `Data.Integer()` field and a byte string visible in the fixture.
 */
const rehydrate = (value: unknown): unknown => {
  if (typeof value === "string") {
    return /^-?\d+n$/u.test(value) ? BigInt(value.slice(0, -1)) : value;
  }
  if (Array.isArray(value)) {
    return value.map(rehydrate);
  }
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(
      Object.entries(value).map(([key, entry]) => [key, rehydrate(entry)]),
    );
  }
  return value;
};

const schemaFor = (aikenType: string): unknown => {
  switch (aikenType) {
    case "FieldCarriageV1":
      return FieldCarriage;
    case "FieldViewV1":
      return FieldView;
    case "FieldPreimageCertificateV1":
      return FieldPreimageCertificate;
    case "FieldPreimageCertificateMintRedeemerV1":
      return FieldPreimageCertificateMintRedeemer;
    default:
      throw new Error(`unknown golden vector type ${aikenType}`);
  }
};

describe("§8.6/§8.8 carriage wire goldens — provenance", () => {
  it("carries the channel's identity", () => {
    expect(golden.schema).toBe("midgard-native-tx-carriage-wire-golden");
    expect(golden.version).toBe(1);
    expect(golden.specDocument).toBe("docs/spec/midgard-tx.md");
    expect(golden.generator).toBe(
      "demo/midgard-sdk/scripts/generate-native-tx-carriage-wire-v1-goldens.mjs",
    );
  });

  /**
   * The expectations are written out as literals rather than compared to the
   * imported constants, because the generator writes the fixture *from* those
   * constants: comparing the two would be comparing a value to itself, and a
   * reordered `pub type` — the exact change no type checker sees — would sail
   * through. The numbers are the declaration order of the Aiken sums in
   * `lib/midgard/native-tx-field-access-v1.ak` and
   * `lib/midgard/native-tx-carriage-v1.ak`; the array index is the tag.
   */
  const FROZEN_CONSTRUCTOR_INDEXES = {
    FieldCarriageV1: { Inline: 0, RawUtxo: 1, Certified: 2 },
    FieldViewV1: { Whole: 0, Chunked: 1, ProvisionalWhole: 2 },
    FieldPreimageCertificateMintRedeemerV1: { Certify: 0, Retire: 1 },
  } as const;

  it("pins the frozen constructor indexes the Aiken sums declare", () => {
    expect(golden.constructorIndexes.FieldCarriageV1).toEqual(
      FROZEN_CONSTRUCTOR_INDEXES.FieldCarriageV1,
    );
    expect(golden.constructorIndexes.FieldViewV1).toEqual(
      FROZEN_CONSTRUCTOR_INDEXES.FieldViewV1,
    );
    expect(
      golden.constructorIndexes.FieldPreimageCertificateMintRedeemerV1,
    ).toEqual(
      FROZEN_CONSTRUCTOR_INDEXES.FieldPreimageCertificateMintRedeemerV1,
    );
  });

  it("holds the source constants to the same frozen tags", () => {
    // The fixture and the constants it was built from are pinned separately, so
    // neither can absorb a reorder by agreeing with the other.
    expect(FIELD_CARRIAGE_CONSTRUCTOR_INDEXES).toEqual(
      FROZEN_CONSTRUCTOR_INDEXES.FieldCarriageV1,
    );
    expect(FIELD_VIEW_CONSTRUCTOR_INDEXES).toEqual(
      FROZEN_CONSTRUCTOR_INDEXES.FieldViewV1,
    );
    expect(
      FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_CONSTRUCTOR_INDEXES,
    ).toEqual(
      FROZEN_CONSTRUCTOR_INDEXES.FieldPreimageCertificateMintRedeemerV1,
    );
  });

  it("covers all four wire types and both mint-redeemer arms", () => {
    const types = new Set(golden.vectors.map((vector) => vector.aikenType));
    expect([...types].sort()).toEqual([
      "FieldCarriageV1",
      "FieldPreimageCertificateMintRedeemerV1",
      "FieldPreimageCertificateV1",
      "FieldViewV1",
    ]);
    const labels = golden.vectors.map((vector) => vector.label);
    expect(labels).toContain("mint_redeemer_certify_chunked_arguments");
    expect(labels).toContain("mint_redeemer_retire");
    // Both tier-1 and tier-3 carriage, so the vector set spans the ladder.
    expect(labels).toContain("carriage_inline_empty_field");
    expect(labels).toContain("carriage_certified_three_chunks");
  });

  /**
   * The three vectors whose widest byte string is strictly above 64 bytes, and
   * which must therefore be an indefinite-length string of 64-byte definite
   * chunks. Named rather than counted: "at least two vectors chunk somewhere"
   * stays green if the one type that lost its wide vector was the one that
   * needed it.
   */
  const MUST_CHUNK = [
    "carriage_inline_chunked_preimage",
    "view_chunked_three_chunk_corner",
    "mint_redeemer_certify_chunked_arguments",
  ];

  it("chunk-encodes exactly the vectors that are above the 64-byte boundary", () => {
    for (const label of MUST_CHUNK) {
      expect(byLabel(label).cborHex).toContain("5f5840");
    }
    const chunked = golden.vectors
      .filter((vector) => vector.cborHex.includes("5f5840"))
      .map((vector) => vector.label);
    // Every other vector's byte strings are 64 or fewer, so any `5f5840` in one
    // of them would be an encoder chunking something it must leave definite.
    expect(chunked.sort()).toEqual([...MUST_CHUNK].sort());
  });

  it("keeps 63 and 64 bytes definite, and 80 chunked, byte for byte", () => {
    // The boundary is `> 64`, not `>= 64`. These three are the whole statement
    // of it: one below, one exactly at it, one above. Each is asserted as its
    // complete encoding rather than as a substring search, so the head that
    // carries the distinction (`583f` / `5840` / `5f 5840`) is pinned and not
    // merely implied.
    const below = inlinePreimageOf("carriage_inline_63_byte_preimage");
    expect(below).toHaveLength(63 * 2);
    expect(byLabel("carriage_inline_63_byte_preimage").cborHex).toBe(
      `d8799f583f${below}ff`,
    );

    const at = inlinePreimageOf("carriage_inline_64_byte_preimage");
    expect(at).toHaveLength(64 * 2);
    expect(byLabel("carriage_inline_64_byte_preimage").cborHex).toBe(
      `d8799f5840${at}ff`,
    );

    // 80 bytes: a 64-byte chunk then a 16-byte one (`50`), inside `5f … ff`.
    const above = inlinePreimageOf("carriage_inline_chunked_preimage");
    expect(above).toHaveLength(80 * 2);
    expect(byLabel("carriage_inline_chunked_preimage").cborHex).toBe(
      `d8799f5f5840${above.slice(0, 64 * 2)}50${above.slice(64 * 2)}ffff`,
    );
  });

  it("covers every wire type with both a trailing-bytes and a wrong-shape refusal", () => {
    for (const aikenType of [
      "FieldCarriageV1",
      "FieldViewV1",
      "FieldPreimageCertificateV1",
      "FieldPreimageCertificateMintRedeemerV1",
    ]) {
      const forType = golden.negativeVectors.filter(
        (vector) => vector.aikenType === aikenType,
      );
      expect(
        forType.filter((vector) => vector.rejectedBy.aiken === "cbor-parse")
          .length,
      ).toBeGreaterThanOrEqual(1);
      expect(
        forType.filter((vector) => vector.rejectedBy.aiken === "data-cast")
          .length,
      ).toBeGreaterThanOrEqual(2);
    }
  });
});

/**
 * §9 clause 2's half of the channel: the payloads a fail-closed decoder must
 * refuse. The Aiken module refuses all of them — trailing bytes at the CBOR
 * layer, wrong shapes at the cast. `Data.from` does not: it decodes the leading
 * item and discards whatever follows, so the off-chain decoder is tolerant
 * exactly where the on-chain one is strict. These tests assert what is true of
 * each side rather than what one would like to be true of both, and the
 * trailing-bytes cases are held to the weaker property that still catches them
 * — the bytes the decoded value re-encodes to are not the bytes it was given.
 */
describe("§8.6/§8.8 carriage wire goldens — refusals", () => {
  for (const vector of golden.negativeVectors) {
    if (vector.rejectedBy.typescript === "throws") {
      it(`refuses ${vector.label}: ${vector.reason}`, () => {
        expect(() =>
          Data.from(vector.cborHex, schemaFor(vector.aikenType) as never),
        ).toThrow();
      });
      continue;
    }

    it(`tolerates but never re-emits ${vector.label}: ${vector.reason}`, () => {
      const decoded = Data.from(
        vector.cborHex,
        schemaFor(vector.aikenType) as never,
      );
      const reEncoded = Data.to(
        decoded as never,
        schemaFor(vector.aikenType) as never,
      );
      // Not producer output, and provably so: the trailing item is dropped, so
      // what comes back is strictly shorter than what went in.
      expect(reEncoded).not.toBe(vector.cborHex);
      expect(vector.cborHex.startsWith(reEncoded)).toBe(true);
      expect(reEncoded.length).toBeLessThan(vector.cborHex.length);
    });
  }
});

describe("§8.6/§8.8 carriage wire goldens — encodings", () => {
  for (const vector of golden.vectors) {
    it(`re-encodes ${vector.label} from src to the fixture's bytes`, () => {
      const value = rehydrate(vector.value);
      expect(
        Data.to(value as never, schemaFor(vector.aikenType) as never),
      ).toBe(vector.cborHex);
    });

    it(`decodes ${vector.label} back to the value it was built from`, () => {
      const value = rehydrate(vector.value);
      expect(
        Data.from(vector.cborHex, schemaFor(vector.aikenType) as never),
      ).toEqual(value);
    });
  }
});
