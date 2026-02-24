import { describe, expect, it } from "vitest";
import fc from "fast-check";
import { encode } from "cborg";
import fs from "node:fs";
import path from "node:path";
import {
  cardanoTxBytesToMidgardCompactBytes,
  cardanoTxBytesToMidgardFullBytes,
  decodeMidgardTransactionBodyCompact,
  decodeMidgardTransactionCompact,
  decodeMidgardTransactionFull,
  decodeMidgardTransactionWitnessSetCompact,
  encodeMidgardTransactionBodyCompact,
  encodeMidgardTransactionCompact,
  encodeMidgardTransactionWitnessSetCompact,
  midgardCompactBytesToCardanoTxBytes,
  midgardFullBytesToCardanoTxBytes,
  MidgardTxCodecError,
  verifyCompactHashesAgainstFull,
} from "@/midgard-tx-codec/index.js";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const txFixtures = JSON.parse(
  fs.readFileSync(fixturePath, "utf8"),
) as readonly TxFixture[];
const sampleTxBytes = txFixtures
  .slice(0, 20)
  .map((tx) => Buffer.from(tx.cborHex, "hex"));

const hash32Arb = fc
  .uint8Array({ minLength: 32, maxLength: 32 })
  .map((bytes) => Buffer.from(bytes));

const bodyCompactArb = fc.record({
  inputsHash: hash32Arb,
  outputsHash: hash32Arb,
  fee: fc.bigInt({ min: 0n, max: 10_000_000n }),
  validityIntervalEnd: fc.option(fc.bigInt({ min: 0n, max: 10_000_000n }), {
    nil: undefined,
  }),
  auxiliaryDataHash: fc.option(hash32Arb, { nil: undefined }),
  validityIntervalStart: fc.option(fc.bigInt({ min: 0n, max: 10_000_000n }), {
    nil: undefined,
  }),
  mintHash: fc.option(hash32Arb, { nil: undefined }),
  scriptDataHash: fc.option(hash32Arb, { nil: undefined }),
  requiredSignersHash: fc.option(hash32Arb, { nil: undefined }),
  networkId: fc.option(fc.bigInt({ min: 0n, max: 100n }), { nil: undefined }),
  referenceInputsHash: fc.option(hash32Arb, { nil: undefined }),
  requiredObserversHash: fc.option(hash32Arb, { nil: undefined }),
});

const witnessCompactArb = fc.record({
  vkeyWitnessesHash: fc.option(hash32Arb, { nil: undefined }),
  nativeScriptsHash: fc.option(hash32Arb, { nil: undefined }),
  redeemersHash: fc.option(hash32Arb, { nil: undefined }),
  plutusV3ScriptsHash: fc.option(hash32Arb, { nil: undefined }),
});

describe("midgard tx codec - golden", () => {
  it("decode(encode(x)) roundtrips for full tx fixtures", () => {
    for (const txBytes of sampleTxBytes) {
      const decoded = decodeMidgardTransactionFull(txBytes);
      const reencoded = cardanoTxBytesToMidgardFullBytes(
        midgardFullBytesToCardanoTxBytes(
          cardanoTxBytesToMidgardFullBytes(Buffer.from(txBytes)),
        ),
      );
      const normalizedOriginal = cardanoTxBytesToMidgardFullBytes(txBytes);
      expect(
        Buffer.from(reencoded).equals(Buffer.from(normalizedOriginal)),
      ).toBe(true);
      expect(decoded.auxiliaryData).toBeNull();
    }
  });

  it("decode(encode(x)) roundtrips for compact tx fixtures generated from cardano", () => {
    for (const txBytes of sampleTxBytes) {
      const compactBytes = cardanoTxBytesToMidgardCompactBytes(txBytes);
      const decoded = decodeMidgardTransactionCompact(compactBytes, "strict");
      const reencoded = encodeMidgardTransactionCompact(decoded);
      expect(Buffer.from(compactBytes).equals(Buffer.from(reencoded))).toBe(
        true,
      );
    }
  });

  it("verifies compact hashes match full tx body and witness hashes", () => {
    for (const txBytes of sampleTxBytes.slice(0, 10)) {
      const fullBytes = cardanoTxBytesToMidgardFullBytes(txBytes);
      const compactBytes = cardanoTxBytesToMidgardCompactBytes(txBytes);
      expect(() =>
        verifyCompactHashesAgainstFull(compactBytes, fullBytes),
      ).not.toThrow();
    }
  });
});

describe("midgard tx codec - strict rejection", () => {
  it("rejects compact map with invalid keys", () => {
    const invalidCompact = Buffer.from(
      encode(
        new Map<unknown, unknown>([
          ["transaction_body_hash", Buffer.alloc(32, 1)],
          ["transaction_witness_set_hash", Buffer.alloc(32, 2)],
          ["is_valid", true],
          ["unexpected", 1],
        ]),
      ),
    );

    expect(() =>
      decodeMidgardTransactionCompact(invalidCompact, "strict"),
    ).toThrow(MidgardTxCodecError);
  });

  it("rejects compact map with invalid field types", () => {
    const invalidCompact = Buffer.from(
      encode(
        new Map<unknown, unknown>([
          ["transaction_body_hash", "not-bytes"],
          ["transaction_witness_set_hash", Buffer.alloc(32, 2)],
          ["is_valid", true],
        ]),
      ),
    );

    expect(() =>
      decodeMidgardTransactionCompact(invalidCompact, "strict"),
    ).toThrow(MidgardTxCodecError);
  });

  it("rejects compact map with wrong hash length", () => {
    const invalidCompact = Buffer.from(
      encode(
        new Map<unknown, unknown>([
          ["transaction_body_hash", Buffer.alloc(31, 1)],
          ["transaction_witness_set_hash", Buffer.alloc(32, 2)],
          ["is_valid", true],
        ]),
      ),
    );

    expect(() =>
      decodeMidgardTransactionCompact(invalidCompact, "strict"),
    ).toThrow(MidgardTxCodecError);
  });

  it("rejects transaction_body_compact with invalid key/type", () => {
    const invalidBody = Buffer.from(
      encode(
        new Map<unknown, unknown>([
          [0, Buffer.alloc(32, 1)],
          [1, Buffer.alloc(32, 2)],
          [2, -1],
        ]),
      ),
    );

    expect(() => decodeMidgardTransactionBodyCompact(invalidBody)).toThrow(
      MidgardTxCodecError,
    );
  });

  it("rejects transaction_witness_set_compact with invalid key", () => {
    const invalidWits = Buffer.from(
      encode(
        new Map<unknown, unknown>([
          [0, Buffer.alloc(32, 1)],
          [9, Buffer.alloc(32, 2)],
        ]),
      ),
    );

    expect(() =>
      decodeMidgardTransactionWitnessSetCompact(invalidWits),
    ).toThrow(MidgardTxCodecError);
  });
});

describe("midgard tx codec - dual mode compatibility", () => {
  it("accepts legacy compact map in dual mode and rejects in strict", () => {
    const legacyCompact = Buffer.from(
      encode(
        new Map<unknown, unknown>([
          ["body", Buffer.alloc(32, 1)],
          ["wits", Buffer.alloc(32, 2)],
          ["validity", "TxIsValid"],
        ]),
      ),
    );

    expect(() =>
      decodeMidgardTransactionCompact(legacyCompact, "strict"),
    ).toThrow(MidgardTxCodecError);

    const decodedDual = decodeMidgardTransactionCompact(legacyCompact, "dual");
    expect(decodedDual.isValid).toBe(true);
  });
});

describe("midgard tx codec - conversion", () => {
  it("Cardano -> Midgard full -> Cardano roundtrips on fixtures", () => {
    for (const txBytes of sampleTxBytes.slice(0, 10)) {
      const fullBytes = cardanoTxBytesToMidgardFullBytes(txBytes);
      const backToCardano = midgardFullBytesToCardanoTxBytes(fullBytes);
      const normalized = cardanoTxBytesToMidgardFullBytes(backToCardano);
      expect(Buffer.from(normalized).equals(Buffer.from(fullBytes))).toBe(true);
    }
  });

  it("compact -> cardano remains unsupported", () => {
    const compactBytes = cardanoTxBytesToMidgardCompactBytes(sampleTxBytes[0]);
    expect(() => midgardCompactBytesToCardanoTxBytes(compactBytes)).toThrow(
      MidgardTxCodecError,
    );
  });
});

describe("midgard tx codec - property", () => {
  it("compact tx codec property roundtrip", () => {
    fc.assert(
      fc.property(
        hash32Arb,
        hash32Arb,
        fc.boolean(),
        (bodyHash, witnessHash, isValid) => {
          const encoded = encodeMidgardTransactionCompact({
            transactionBodyHash: bodyHash,
            transactionWitnessSetHash: witnessHash,
            isValid,
          });
          const decoded = decodeMidgardTransactionCompact(encoded, "strict");
          expect(decoded.transactionBodyHash.equals(bodyHash)).toBe(true);
          expect(decoded.transactionWitnessSetHash.equals(witnessHash)).toBe(
            true,
          );
          expect(decoded.isValid).toBe(isValid);
        },
      ),
      { numRuns: 200 },
    );
  });

  it("transaction_body_compact codec property roundtrip", () => {
    fc.assert(
      fc.property(bodyCompactArb, (body) => {
        const encoded = encodeMidgardTransactionBodyCompact(body);
        const decoded = decodeMidgardTransactionBodyCompact(encoded);
        expect(decoded).toEqual(body);
      }),
      { numRuns: 150 },
    );
  });

  it("transaction_witness_set_compact codec property roundtrip", () => {
    fc.assert(
      fc.property(witnessCompactArb, (wits) => {
        const encoded = encodeMidgardTransactionWitnessSetCompact(wits);
        const decoded = decodeMidgardTransactionWitnessSetCompact(encoded);
        expect(decoded).toEqual(wits);
      }),
      { numRuns: 150 },
    );
  });

  it("conversion property on real tx corpus subset", () => {
    fc.assert(
      fc.property(
        fc.integer({ min: 0, max: sampleTxBytes.length - 1 }),
        (index) => {
          const txBytes = sampleTxBytes[index];
          const fullBytes = cardanoTxBytesToMidgardFullBytes(txBytes);
          const back = midgardFullBytesToCardanoTxBytes(fullBytes);
          const finalFull = cardanoTxBytesToMidgardFullBytes(back);
          expect(Buffer.from(fullBytes).equals(Buffer.from(finalFull))).toBe(
            true,
          );
        },
      ),
      { numRuns: 80 },
    );
  });
});
