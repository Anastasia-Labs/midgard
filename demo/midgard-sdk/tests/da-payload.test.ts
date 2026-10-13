import {
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
  MIDGARD_CEK_MIN_PROGRAM_MATERIAL_DA_TUPLE_BYTES,
  MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES,
  MIDGARD_MAX_DA_PAYLOAD_BYTES,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";
import { describe, expect, it } from "vitest";

import {
  DA_PAYLOAD_VERSION,
  DaPayload,
  type DaPayload as DaPayloadType,
  DaPayloadCounts,
  daPayloadEncodedSize,
  daPayloadEncodedSizeFromEntryAggregates,
  daPayloadEntriesEncodedSizeFromAggregate,
  type DaPayloadEntry,
  daPayloadEntryEncodedSize,
  type DaPayloadEntryField,
  type DaPayloadEntrySizeAggregate,
  decodeDaPayload,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayload,
  Header,
} from "../src/index.js";

const bytes = (value: number, length: number): string =>
  value.toString(16).padStart(2, "0").repeat(length);

const entries = (seed: number, count: number): DaPayloadEntry[] =>
  Array.from({ length: count }, (_, index) => [
    bytes((seed + index) % 256, 28 + (index % 5)),
    bytes((seed * 3 + index) % 256, index % 2 === 0 ? 0 : 40 + index),
  ]);

const payload = (seed: number): DaPayloadType => {
  const withdrawals = entries(seed + 1, (seed + 1) % 3);
  const forcedTransactions = entries(seed + 2, (seed + 2) % 3);
  const transactions = entries(seed + 3, (seed + 3) % 5);
  const transactionPreimages = entries(seed + 9, transactions.length);
  const forcedTransactionPreimages = entries(
    seed + 10,
    forcedTransactions.length,
  );
  const deposits = entries(seed + 4, (seed + 4) % 3);
  const transitionTrace = entries(seed + 5, (seed + 5) % 6);
  const eventToStep = entries(seed + 6, transitionTrace.length);
  const validationTraces = entries(
    seed + 7,
    withdrawals.length +
      forcedTransactions.length +
      transactions.length +
      deposits.length,
  );
  const counts = {
    withdrawalCount: BigInt(withdrawals.length),
    forcedTransactionCount: BigInt(forcedTransactions.length),
    l2TransactionCount: BigInt(transactions.length),
    depositCount: BigInt(deposits.length),
    totalEventCount: BigInt(validationTraces.length),
    transitionStepCount: BigInt(transitionTrace.length),
    validationTraceCount: BigInt(validationTraces.length),
  };
  return {
    version: DA_PAYLOAD_VERSION,
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
        validationTracesRoot: bytes(seed + 8, 32),
        ...counts,
        startTime: BigInt(seed * 1_000),
        endTime: BigInt(seed * 1_000 + 999),
        blockSlot: BigInt(seed),
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: bytes(seed + 6, 28),
        operatorVkey: bytes(seed + 7, 28),
        protocolVersion: 1n,
      },
      withdrawals,
      forced_transactions: forcedTransactions,
      transactions,
      transaction_preimages: transactionPreimages,
      forced_transaction_preimages: forcedTransactionPreimages,
      cek_program_material: [],
      deposits,
      transition_trace: transitionTrace,
      event_to_step: eventToStep,
      validation_traces: validationTraces,
      validation_trace_witnesses: [],
      counts,
    },
  };
};

const emptyPayload = (): DaPayloadType => {
  const value = payload(0);
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  return {
    version: DA_PAYLOAD_VERSION,
    block_body: {
      ...value.block_body,
      header: {
        ...value.block_body.header,
        ...counts,
        startTime: 0n,
        endTime: 0n,
      },
      withdrawals: [],
      forced_transactions: [],
      transactions: [],
      transaction_preimages: [],
      forced_transaction_preimages: [],
      cek_program_material: [],
      deposits: [],
      transition_trace: [],
      event_to_step: [],
      validation_traces: [],
      validation_trace_witnesses: [],
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

describe("DaPayloadV1 canonical codec", () => {
  it("requires an empty retained validation witness section", () => {
    const value = payload(1);
    const encoded = encodeDaPayload(value);
    expect(
      decodeDaPayload(encoded).block_body.validation_trace_witnesses,
    ).toEqual([]);
    const witness: DaPayloadEntry = [bytes(1, 1), bytes(2, 1)];
    expect(() =>
      encodeDaPayload({
        ...value,
        block_body: {
          ...value.block_body,
          validation_trace_witnesses: [witness],
        },
      }),
    ).toThrow(/validation_trace_witnesses must be empty/u);
    // The section is the empty list just before the counts constructor and
    // the two closing breaks.
    const tail =
      Buffer.from(Data.to(value.block_body.counts, DaPayloadCounts), "hex")
        .length + 2;
    const section = encoded.length - tail - 1;
    expect(encoded[section]).toBe(0x80);
    const withWitness = Buffer.concat([
      encoded.subarray(0, section),
      Buffer.from("9f9f41014102ffff", "hex"),
      encoded.subarray(section + 1),
    ]);
    expect(() => decodeDaPayload(withWitness)).toThrow(
      /validation_trace_witnesses \(must be empty\)/u,
    );
  });

  it("pins the exact V1 body/count field order and constructor arities", () => {
    const value = emptyPayload();
    const encoded = encodeDaPayload(value);

    expect(encoded).toEqual(Buffer.from(Data.to(value, DaPayload), "hex"));
    expect(encoded).toHaveLength(445);
    expect(Buffer.from(sha256(encoded)).toString("hex")).toBe(
      "2f3caa41a48bbf5a5609accecd95ddf8e27af29a2bf9a3043f3394e18e2b334c",
    );

    for (const trailingBreaks of [1, 2, 3]) {
      const insertion = encoded.length - trailingBreaks;
      const extraField = Buffer.concat([
        encoded.subarray(0, insertion),
        Buffer.from([0]),
        encoded.subarray(insertion),
      ]);
      expect(() => decodeDaPayload(extraField)).toThrow(
        /unexpected CBOR framing/u,
      );
    }
  });

  it("uses the newest payload shape and is byte-identical to its schema", () => {
    for (let seed = 0; seed < 20; seed += 1) {
      const value = payload(seed);
      const expected = Buffer.from(Data.to(value, DaPayload), "hex");
      const encoded = encodeDaPayload(value);
      expect(encoded).toEqual(expected);
      expect(daPayloadEncodedSize(value)).toBe(encoded.length);
      expect(decodeDaPayload(encoded)).toEqual(value);
    }
  });

  it("rejects entry aggregate drift", () => {
    expect(() =>
      daPayloadEntriesEncodedSizeFromAggregate({
        entryCount: 0,
        encodedTupleBytes: 1,
      }),
    ).toThrow("must have zero tuple bytes");
  });

  it("refuses a payload that carries a UTxO section", () => {
    for (const utxoSection of [
      Buffer.from([0x80]),
      Buffer.from(`9f9f581c${bytes(9, 28)}5820${bytes(10, 32)}ffff`, "hex"),
    ]) {
      for (const value of [emptyPayload(), payload(1), payload(3)]) {
        const encoded = encodeDaPayload(value);
        // The header constructor closes right before the first entry list.
        const header = Buffer.from(
          Data.to(value.block_body.header, Header),
          "hex",
        );
        const afterHeader = encoded.indexOf(header) + header.length;
        expect(afterHeader).toBeGreaterThan(header.length);
        const withUtxos = Buffer.concat([
          encoded.subarray(0, afterHeader),
          utxoSection,
          encoded.subarray(afterHeader),
        ]);
        expect(() => decodeDaPayload(withUtxos)).toThrow(
          /unexpected CBOR framing/u,
        );
      }
    }
  });

  it("sizes every entry list from its aggregate exactly", () => {
    const fields: readonly DaPayloadEntryField[] = [
      "withdrawals",
      "forced_transactions",
      "transactions",
      "transaction_preimages",
      "forced_transaction_preimages",
      "cek_program_material",
      "deposits",
      "transition_trace",
      "event_to_step",
      "validation_traces",
    ];
    for (let seed = 0; seed < 20; seed += 1) {
      const value = payload(seed);
      const aggregates = Object.fromEntries(
        fields.map((field) => [
          field,
          {
            entryCount: value.block_body[field].length,
            encodedTupleBytes: value.block_body[field].reduce(
              (sum, entry) => sum + daPayloadEntryEncodedSize(entry),
              0,
            ),
          },
        ]),
      ) as Record<DaPayloadEntryField, DaPayloadEntrySizeAggregate>;
      const emptied = {
        ...value,
        block_body: {
          ...value.block_body,
          ...Object.fromEntries(fields.map((field) => [field, []])),
        },
      } as DaPayloadType;
      expect(daPayloadEncodedSizeFromEntryAggregates(emptied, aggregates)).toBe(
        encodeDaPayload(value).length,
      );
    }
  });

  it("fails closed on every version other than 1", () => {
    const value = payload(1);
    expect(() => encodeDaPayload({ ...value, version: 23n })).toThrow(
      /version must equal 1/u,
    );
    const encoded = encodeDaPayload(value);
    const wrongVersion = Buffer.from(encoded);
    wrongVersion[3] = 23;
    expect(() => decodeDaPayload(wrongVersion)).toThrow(
      /version must equal 1/u,
    );
  });

  it("bounds every protocol integer to native CBOR", () => {
    const base = payload(1);
    for (const startTime of [
      -0x1_0000_0000_0000_0000n,
      0xffff_ffff_ffff_ffffn,
    ]) {
      expect(() =>
        encodeDaPayload({
          ...base,
          block_body: {
            ...base.block_body,
            header: { ...base.block_body.header, startTime },
          },
        }),
      ).not.toThrow();
    }
    for (const startTime of [
      -0x1_0000_0000_0000_0001n,
      0x1_0000_0000_0000_0000n,
    ]) {
      expect(() =>
        encodeDaPayload({
          ...base,
          block_body: {
            ...base.block_body,
            header: { ...base.block_body.header, startTime },
          },
        }),
      ).toThrow(/native CBOR integer range/u);
    }
  });

  it("rejects malformed fields and non-canonical framing", () => {
    const value = payload(1);
    expect(() =>
      encodeDaPayload({
        ...value,
        block_body: { ...value.block_body, header_hash: "not-hex" },
      }),
    ).toThrow("even-length hexadecimal");

    const canonical = encodeDaPayload(value);
    expect(() =>
      decodeDaPayload(
        replaceFirst(
          canonical,
          Buffer.from([0xd8, 0x79, 0x9f, 0x01]),
          Buffer.from([0xd8, 0x79, 0x9f, 0x18, 0x01]),
        ),
      ),
    ).toThrow("not minimally encoded");

    const shortBytes = Buffer.from(value.block_body.header_hash, "hex");
    expect(() =>
      decodeDaPayload(
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
  });

  it("pins the exact DA-derived CEK material ceilings", () => {
    const empty = emptyPayload();
    const minimumMaterialEntry: DaPayloadEntry = [bytes(0, 32), "8301004100"];
    const tupleBytes = daPayloadEntryEncodedSize(minimumMaterialEntry);
    const nonEmptyBytes = daPayloadEncodedSize({
      ...empty,
      block_body: {
        ...empty.block_body,
        cek_program_material: [minimumMaterialEntry],
      },
    });

    expect(daPayloadEncodedSize(empty)).toBe(445);
    expect(tupleBytes).toBe(MIDGARD_CEK_MIN_PROGRAM_MATERIAL_DA_TUPLE_BYTES);
    // The published fixed part was measured when the body also carried a
    // one-byte empty UTxO list; it stays one byte above the exact framing so
    // the consensus limits do not move, which keeps the ceilings conservative.
    expect(nonEmptyBytes).toBe(
      MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES - 1 + tupleBytes,
    );
    expect(MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT).toBe(
      BigInt(
        Math.floor(
          (MIDGARD_MAX_DA_PAYLOAD_BYTES -
            MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES) /
            tupleBytes,
        ),
      ),
    );
    expect(MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES).toBe(
      BigInt(
        MIDGARD_MAX_DA_PAYLOAD_BYTES -
          MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES,
      ),
    );
  });
});
