import { createHash } from "node:crypto";

import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { decodeCanonicalTransactionInput } from "../src/ledger-tx/canonical-cardano-fast-path.js";
import { decodeMidgardOutRefBytes } from "../src/ledger-tx/codec.js";

type Outcome<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false };

const capture = <T>(decode: () => T): Outcome<T> => {
  try {
    return { ok: true, value: decode() };
  } catch {
    return { ok: false };
  }
};

type ComparableOutRef = { readonly txIdHex: string; readonly index: bigint };

const cmlOutRef = (bytes: Uint8Array): ComparableOutRef => {
  const input = CML.TransactionInput.from_cbor_bytes(bytes);
  const transactionId = input.transaction_id();
  try {
    return {
      txIdHex: Buffer.from(transactionId.to_raw_bytes()).toString("hex"),
      index: BigInt(input.index()),
    };
  } finally {
    transactionId.free();
    input.free();
  }
};

const runtimeOutRef = (bytes: Uint8Array): ComparableOutRef => {
  const decoded = decodeMidgardOutRefBytes(bytes);
  return { txIdHex: decoded.txId.toString("hex"), index: decoded.index };
};

const expectFallbackOutcomeMatches = <T>(
  bytes: Uint8Array,
  runtime: (value: Uint8Array) => T,
  oracle: (value: Uint8Array) => T,
): void => {
  const actual = capture(() => runtime(bytes));
  const expected = capture(() => oracle(bytes));
  expect(actual.ok).toBe(expected.ok);
  if (actual.ok && expected.ok) {
    expect(actual.value).toStrictEqual(expected.value);
  }
};

describe("canonical Cardano shape fast paths", () => {
  it("matches CML for randomized canonical TransactionInput values and uint64 boundaries", () => {
    const indexes = [
      0n,
      23n,
      24n,
      255n,
      256n,
      65_535n,
      65_536n,
      0xffff_ffffn,
      0x1_0000_0000n,
      0xffff_ffff_ffff_ffffn,
    ];
    for (let sample = 0; sample < 256; sample += 1) {
      const txId = createHash("sha256")
        .update(`canonical-input-${sample.toString()}`)
        .digest();
      const index =
        indexes[sample % indexes.length]! ^
        BigInt((sample * 0x9e37_79b1) >>> 0);
      const boundedIndex = index & 0xffff_ffff_ffff_ffffn;
      const hash = CML.TransactionHash.from_raw_bytes(txId);
      const input = CML.TransactionInput.new(hash, boundedIndex);
      try {
        const bytes = Buffer.from(input.to_cbor_bytes());
        const fast = decodeCanonicalTransactionInput(bytes);
        expect(fast).toBeDefined();
        expect(
          fast === undefined
            ? undefined
            : { txIdHex: fast.txId.toString("hex"), index: fast.index },
        ).toStrictEqual(cmlOutRef(bytes));
      } finally {
        input.free();
        hash.free();
      }
    }
  });

  it("falls back to CML for malformed, non-minimal, tagged, indefinite, trailing, and out-of-shape inputs", () => {
    const txId = Buffer.alloc(32, 0x42);
    const canonical = encodeCbor([txId, 0n]);
    const malformed = [
      encodeCbor([txId]),
      encodeCbor([txId, 0n, 1n]),
      encodeCbor([Buffer.alloc(31, 0x42), 0n]),
      Buffer.concat([
        Buffer.from([0x82, 0x58, 0x20]),
        txId,
        Buffer.from([0x18, 0x00]),
      ]),
      Buffer.concat([
        Buffer.from([0x98, 0x02, 0x58, 0x20]),
        txId,
        Buffer.from([0x00]),
      ]),
      Buffer.concat([canonical, Buffer.from([0x00])]),
      Buffer.concat([
        Buffer.from([0x82, 0x58, 0x20]),
        txId,
        Buffer.from([0xc0, 0x00]),
      ]),
      Buffer.concat([
        Buffer.from([0x9f, 0x58, 0x20]),
        txId,
        Buffer.from([0x00, 0xff]),
      ]),
      encodeCbor([txId, -1n]),
      Buffer.concat([
        Buffer.from([0x82, 0x58, 0x20]),
        txId,
        Buffer.from("c249010000000000000000", "hex"),
      ]),
    ];
    for (const bytes of malformed) {
      expect(decodeCanonicalTransactionInput(bytes)).toBeUndefined();
      expectFallbackOutcomeMatches(bytes, runtimeOutRef, cmlOutRef);
    }

    for (let sample = 0; sample < 128; sample += 1) {
      const txIdSample = createHash("sha256")
        .update(`malformed-input-${sample.toString()}`)
        .digest();
      const valid = encodeCbor([txIdSample, BigInt(sample)]);
      const bytes =
        sample % 2 === 0
          ? Buffer.concat([valid, Buffer.from([sample & 0xff])])
          : valid.subarray(0, valid.length - 1);
      expect(decodeCanonicalTransactionInput(bytes)).toBeUndefined();
      expectFallbackOutcomeMatches(bytes, runtimeOutRef, cmlOutRef);
    }
  });
});
