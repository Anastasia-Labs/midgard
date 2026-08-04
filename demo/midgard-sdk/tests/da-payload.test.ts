import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  daPayloadEntriesEncodedSizeFromAggregate,
  type DaPayloadEntry,
  daPayloadEntryEncodedSize,
  DaPayloadV2,
  type DaPayloadV2 as DaPayloadV2Type,
  daPayloadV2EncodedSize,
  daPayloadV2EncodedSizeFromUtxoAggregate,
  decodeDaPayloadV2,
  decodeDaPayloadV2Canonical,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayloadV2,
  encodeDaPayloadV2Protocol,
} from "../src/index.js";

const bytes = (value: number, length: number): string =>
  value.toString(16).padStart(2, "0").repeat(length);

const entries = (seed: number, count: number): DaPayloadEntry[] =>
  Array.from({ length: count }, (_, index) => [
    bytes((seed + index) % 256, 28 + (index % 5)),
    bytes((seed * 3 + index) % 256, index % 2 === 0 ? 0 : 40 + index),
  ]);

const payload = (seed: number): DaPayloadV2Type => {
  const utxos = entries(seed, seed % 4);
  const withdrawals = entries(seed + 1, (seed + 1) % 3);
  const forcedTransactions = entries(seed + 2, (seed + 2) % 3);
  const transactions = entries(seed + 3, (seed + 3) % 5);
  const deposits = entries(seed + 4, (seed + 4) % 3);
  const transitionTrace = entries(seed + 5, (seed + 5) % 6);
  const eventToStep = entries(seed + 6, transitionTrace.length);
  const totalEvents =
    withdrawals.length +
    forcedTransactions.length +
    transactions.length +
    deposits.length;
  const counts = {
    withdrawalCount: BigInt(withdrawals.length),
    forcedTransactionCount: BigInt(forcedTransactions.length),
    l2TransactionCount: BigInt(transactions.length),
    depositCount: BigInt(deposits.length),
    totalEventCount: BigInt(totalEvents),
    transitionStepCount: BigInt(transitionTrace.length),
  };
  return {
    version: 2n,
    block_body: {
      header_hash: bytes(seed, 28),
      header: {
        prevUtxosRoot: bytes(seed + 1, 32),
        utxosRoot: bytes(seed + 2, 32),
        withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: bytes(seed + 3, 32),
        depositsRoot: EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: bytes(seed + 4, 32),
        eventToStepRoot: bytes(seed + 5, 32),
        ...counts,
        startTime: BigInt(seed * 1_000),
        endTime: BigInt(seed * 1_000 + 999),
        prevHeaderHash: bytes(seed + 6, 28),
        operatorVkey: bytes(seed + 7, 28),
        protocolVersion: BigInt(seed % 4),
      },
      utxos,
      withdrawals,
      forced_transactions: forcedTransactions,
      transactions,
      deposits,
      transition_trace: transitionTrace,
      event_to_step: eventToStep,
      counts,
    },
  };
};

const replaceFirst = (
  input: Buffer,
  canonical: Buffer,
  replacement: Buffer,
): Buffer => {
  const start = input.indexOf(canonical);
  expect(start).toBeGreaterThanOrEqual(0);
  return Buffer.concat([
    input.subarray(0, start),
    replacement,
    input.subarray(start + canonical.length),
  ]);
};

describe("DaPayloadV2 byte-oriented encoder", () => {
  it("is byte-identical to Lucid across deterministic varied payloads", () => {
    for (let seed = 0; seed < 100; seed += 1) {
      const value = payload(seed);
      const legacy = Buffer.from(Data.to(value, DaPayloadV2), "hex");
      expect(encodeDaPayloadV2(value)).toEqual(legacy);
      expect(decodeDaPayloadV2(legacy)).toEqual(value);
      expect(decodeDaPayloadV2Canonical(legacy)).toEqual(value);
    }
  });

  it("matches Lucid at Plutus bytes chunk boundaries", () => {
    for (const length of [0, 1, 23, 24, 63, 64, 65, 127, 128, 129]) {
      const value = payload(length);
      const boundaryEntry: DaPayloadEntry = [
        bytes(1, length),
        bytes(2, length),
      ];
      const withBoundary = {
        ...value,
        block_body: {
          ...value.block_body,
          utxos: [boundaryEntry],
        },
      };
      expect(encodeDaPayloadV2(withBoundary)).toEqual(
        Buffer.from(Data.to(withBoundary, DaPayloadV2), "hex"),
      );
      expect(daPayloadV2EncodedSize(withBoundary)).toBe(
        encodeDaPayloadV2(withBoundary).length,
      );
      expect(
        daPayloadV2EncodedSizeFromUtxoAggregate(withBoundary, {
          entryCount: 1,
          encodedTupleBytes: daPayloadEntryEncodedSize(boundaryEntry),
        }),
      ).toBe(encodeDaPayloadV2(withBoundary).length);
    }
  });

  it("sizes an empty maintained UTxO aggregate exactly and rejects drift", () => {
    const value = payload(0);
    expect(
      daPayloadV2EncodedSizeFromUtxoAggregate(value, {
        entryCount: 0,
        encodedTupleBytes: 0,
      }),
    ).toBe(
      encodeDaPayloadV2({
        ...value,
        block_body: { ...value.block_body, utxos: [] },
      }).length,
    );
    expect(() =>
      daPayloadEntriesEncodedSizeFromAggregate({
        entryCount: 0,
        encodedTupleBytes: 1,
      }),
    ).toThrow("must have zero tuple bytes");
  });

  it("preserves Lucid bignum encoding through the compatibility fallback", () => {
    const value = payload(1);
    const withBignum = {
      ...value,
      version: 1n << 80n,
    };
    expect(encodeDaPayloadV2(withBignum)).toEqual(
      Buffer.from(Data.to(withBignum, DaPayloadV2), "hex"),
    );
    expect(() =>
      decodeDaPayloadV2Canonical(
        Buffer.from(Data.to(withBignum, DaPayloadV2), "hex"),
      ),
    ).toThrow();
    expect(() => encodeDaPayloadV2Protocol(withBignum)).toThrow(
      /native CBOR integer range/u,
    );
  });

  it("bounds every production payload integer to native CBOR", () => {
    const base = payload(1);
    for (const startTime of [
      -0x1_0000_0000_0000_0000n,
      0xffff_ffff_ffff_ffffn,
    ]) {
      const atBoundary = {
        ...base,
        block_body: {
          ...base.block_body,
          header: { ...base.block_body.header, startTime },
        },
      };
      expect(encodeDaPayloadV2Protocol(atBoundary)).toEqual(
        encodeDaPayloadV2(atBoundary),
      );
    }
    for (const startTime of [
      -0x1_0000_0000_0000_0001n,
      0x1_0000_0000_0000_0000n,
    ]) {
      const outsideBoundary = {
        ...base,
        block_body: {
          ...base.block_body,
          header: { ...base.block_body.header, startTime },
        },
      };
      expect(() => encodeDaPayloadV2Protocol(outsideBoundary)).toThrow(
        /native CBOR integer range/u,
      );
    }
  });

  it("rejects malformed byte fields instead of truncating invalid hex", () => {
    const value = payload(1);
    expect(() =>
      encodeDaPayloadV2({
        ...value,
        block_body: { ...value.block_body, header_hash: "not-hex" },
      }),
    ).toThrow("even-length hexadecimal");
  });

  it("rejects every non-minimal integer and byte-length argument boundary", () => {
    const value = payload(1);
    const canonical = encodeDaPayloadV2(value);
    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          canonical,
          Buffer.from([0xd8, 0x79, 0x9f, 0x02]),
          Buffer.from([0xd8, 0x79, 0x9f, 0x18, 0x02]),
        ),
      ),
    ).toThrow("not minimally encoded");

    const integerBoundaries = [
      [0n, "00", "1800"],
      [23n, "17", "1817"],
      [24n, "1818", "190018"],
      [255n, "18ff", "1900ff"],
      [256n, "190100", "1a00000100"],
      [65_535n, "19ffff", "1a0000ffff"],
      [65_536n, "1a00010000", "1b0000000000010000"],
      [0xffff_ffffn, "1affffffff", "1b00000000ffffffff"],
      [-1n, "20", "3800"],
      [-24n, "37", "3817"],
      [-25n, "3818", "390018"],
      [-256n, "38ff", "3900ff"],
      [-257n, "390100", "3a00000100"],
      [-65_536n, "39ffff", "3a0000ffff"],
      [-65_537n, "3a00010000", "3b0000000000010000"],
    ] as const;
    for (const [version, canonicalHex, nonMinimalHex] of integerBoundaries) {
      const encoded = encodeDaPayloadV2({ ...value, version });
      expect(encoded.subarray(3, 3 + canonicalHex.length / 2)).toEqual(
        Buffer.from(canonicalHex, "hex"),
      );
      expect(() =>
        decodeDaPayloadV2Canonical(
          Buffer.concat([
            encoded.subarray(0, 3),
            Buffer.from(nonMinimalHex, "hex"),
            encoded.subarray(3 + canonicalHex.length / 2),
          ]),
        ),
      ).toThrow("not minimally encoded");
    }

    const headerHash = Buffer.from(value.block_body.header_hash, "hex");
    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          canonical,
          Buffer.concat([Buffer.from([0x58, 0x1c]), headerHash]),
          Buffer.concat([Buffer.from([0x59, 0x00, 0x1c]), headerHash]),
        ),
      ),
    ).toThrow("not minimally encoded");

    for (const [length, canonicalLength, nonMinimalLength] of [
      [0, "40", "5800"],
      [23, "57", "5817"],
      [24, "5818", "590018"],
      [64, "5840", "590040"],
    ] as const) {
      const key = Buffer.alloc(length, 0xed);
      const entry = Buffer.concat([
        Buffer.from([0x9f]),
        Buffer.from(canonicalLength, "hex"),
        key,
        Buffer.from("44deadbeefff", "hex"),
      ]);
      const nonMinimalEntry = Buffer.concat([
        Buffer.from([0x9f]),
        Buffer.from(nonMinimalLength, "hex"),
        key,
        Buffer.from("44deadbeefff", "hex"),
      ]);
      const withBoundary = {
        ...value,
        block_body: {
          ...value.block_body,
          utxos: [[key.toString("hex"), "deadbeef"]] as DaPayloadEntry[],
        },
      };
      expect(() =>
        decodeDaPayloadV2Canonical(
          replaceFirst(encodeDaPayloadV2(withBoundary), entry, nonMinimalEntry),
        ),
      ).toThrow("not minimally encoded");
    }
  });

  it("rejects non-canonical definite and indefinite byte-string choices", () => {
    const base = payload(1);
    const shortBytes = Buffer.from(base.block_body.header_hash, "hex");
    const canonical = encodeDaPayloadV2(base);
    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          canonical,
          Buffer.concat([Buffer.from([0x58, 0x1c]), shortBytes]),
          Buffer.concat([
            Buffer.from([0x5f, 0x58, 0x1c]),
            shortBytes,
            Buffer.from([0xff]),
          ]),
        ),
      ),
    ).toThrow("must use definite framing");

    const longBytes = Buffer.alloc(65, 0xa1);
    const withLongEntry = {
      ...base,
      block_body: {
        ...base.block_body,
        utxos: [[longBytes.toString("hex"), ""]] as DaPayloadEntry[],
      },
    };
    const longCanonical = encodeDaPayloadV2(withLongEntry);
    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          longCanonical,
          Buffer.concat([
            Buffer.from([0x5f, 0x58, 0x40]),
            longBytes.subarray(0, 64),
            Buffer.from([0x41]),
            longBytes.subarray(64),
            Buffer.from([0xff]),
          ]),
          Buffer.concat([Buffer.from([0x58, 0x41]), longBytes]),
        ),
      ),
    ).toThrow("must use indefinite framing");
  });

  it("rejects non-canonical 64-byte chunk boundaries", () => {
    const base = payload(1);
    const longBytes = Buffer.alloc(65, 0xb2);
    const value = {
      ...base,
      block_body: {
        ...base.block_body,
        utxos: [[longBytes.toString("hex"), ""]] as DaPayloadEntry[],
      },
    };
    const canonical = encodeDaPayloadV2(value);
    const canonicalBytes = Buffer.concat([
      Buffer.from([0x5f, 0x58, 0x40]),
      longBytes.subarray(0, 64),
      Buffer.from([0x41]),
      longBytes.subarray(64),
      Buffer.from([0xff]),
    ]);

    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          canonical,
          canonicalBytes,
          Buffer.concat([
            Buffer.from([0x5f, 0x58, 0x3f]),
            longBytes.subarray(0, 63),
            Buffer.from([0x42]),
            longBytes.subarray(63),
            Buffer.from([0xff]),
          ]),
        ),
      ),
    ).toThrow("non-final indefinite byte chunks");

    const exactChunk = Buffer.alloc(64, 0xc3);
    const withExactChunk = {
      ...base,
      block_body: {
        ...base.block_body,
        utxos: [[exactChunk.toString("hex"), ""]] as DaPayloadEntry[],
      },
    };
    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          encodeDaPayloadV2(withExactChunk),
          Buffer.concat([Buffer.from([0x58, 0x40]), exactChunk]),
          Buffer.concat([
            Buffer.from([0x5f, 0x58, 0x40]),
            exactChunk,
            Buffer.from([0xff]),
          ]),
        ),
      ),
    ).toThrow("must use definite framing");
  });

  it("rejects non-canonical list, tuple, and constructor framing", () => {
    const empty = {
      ...payload(0),
      block_body: {
        ...payload(0).block_body,
        utxos: [],
        withdrawals: [],
        forced_transactions: [],
        transactions: [],
        deposits: [],
        transition_trace: [],
        event_to_step: [],
      },
    };
    const canonicalEmpty = encodeDaPayloadV2(empty);
    const emptyLists = Buffer.alloc(7, 0x80);
    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          canonicalEmpty,
          emptyLists,
          Buffer.concat([Buffer.from([0x9f, 0xff]), emptyLists.subarray(1)]),
        ),
      ),
    ).toThrow("empty list must use definite framing");

    const nonEmpty = {
      ...payload(1),
      block_body: {
        ...payload(1).block_body,
        utxos: [["01", "02"]] as DaPayloadEntry[],
      },
    };
    const canonicalNonEmpty = encodeDaPayloadV2(nonEmpty);
    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          canonicalNonEmpty,
          Buffer.from([0x9f, 0x9f, 0x41, 0x01, 0x41, 0x02, 0xff, 0xff]),
          Buffer.from([0x81, 0x9f, 0x41, 0x01, 0x41, 0x02, 0xff]),
        ),
      ),
    ).toThrow("non-empty list must be indefinite");
    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          canonicalNonEmpty,
          Buffer.from([0x9f, 0x9f, 0x41, 0x01, 0x41, 0x02, 0xff, 0xff]),
          Buffer.from([0x9f, 0x82, 0x41, 0x01, 0x41, 0x02, 0xff]),
        ),
      ),
    ).toThrow("tuple must be indefinite");
    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          canonicalNonEmpty,
          Buffer.from([0xd8, 0x79, 0x9f, 0x02]),
          Buffer.from([0xd8, 0x79, 0x82, 0x02]),
        ),
      ),
    ).toThrow("constructor fields must be indefinite");
    expect(() =>
      decodeDaPayloadV2Canonical(
        replaceFirst(
          canonicalNonEmpty,
          Buffer.from([0xd8, 0x79, 0x9f, 0x02]),
          Buffer.from([0xd9, 0x00, 0x79, 0x9f, 0x02]),
        ),
      ),
    ).toThrow("constructor tag is not minimally encoded");
  });
});
