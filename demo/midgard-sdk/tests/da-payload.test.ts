import {
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1,
  MIDGARD_CEK_MIN_PROGRAM_MATERIAL_DA_TUPLE_BYTES_V1,
  MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES_V1,
  MIDGARD_MAX_DA_PAYLOAD_BYTES_V1,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";
import { describe, expect, it } from "vitest";

import {
  DA_PAYLOAD_V1_VERSION,
  daPayloadEntriesEncodedSizeFromAggregate,
  type DaPayloadEntry,
  daPayloadEntryEncodedSize,
  DaPayloadV1,
  type DaPayloadV1 as DaPayloadV1Type,
  daPayloadV1EncodedSize,
  daPayloadV1EncodedSizeFromUtxoAggregate,
  decodeDaPayloadV1,
  decodeRetainedValidationWitnessKeyV1,
  decodeRetainedValidationWitnessV1,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayloadV1,
  encodeRetainedValidationWitnessKeyV1,
  encodeRetainedValidationWitnessV1,
} from "../src/index.js";

const bytes = (value: number, length: number): string =>
  value.toString(16).padStart(2, "0").repeat(length);

const entries = (seed: number, count: number): DaPayloadEntry[] =>
  Array.from({ length: count }, (_, index) => [
    bytes((seed + index) % 256, 28 + (index % 5)),
    bytes((seed * 3 + index) % 256, index % 2 === 0 ? 0 : 40 + index),
  ]);

const payload = (seed: number): DaPayloadV1Type => {
  const utxos = entries(seed, seed % 4);
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
    version: DA_PAYLOAD_V1_VERSION,
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
      utxos,
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

const emptyPayload = (): DaPayloadV1Type => {
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
    version: DA_PAYLOAD_V1_VERSION,
    block_body: {
      ...value.block_body,
      header: {
        ...value.block_body.header,
        ...counts,
        startTime: 0n,
        endTime: 0n,
      },
      utxos: [],
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
  it("round-trips a chronological ScriptSources retained witness canonically", () => {
    const key = {
      event_key: { L2TransactionEventKey: { tx_id: bytes(1, 32) } },
      execution_index: -2n,
    } as const;
    const value = {
      machine_state: {
        machine_version: 1n,
        event_key_hash: bytes(2, 32),
        transaction_id: bytes(3, 32),
        transaction_commitment: bytes(4, 32),
        validation_context_hash: bytes(5, 32),
        source_kind: "Normal" as const,
        prior_ledger_root: bytes(6, 32),
        phase: "ScriptSources" as const,
        program_counter: 9n,
        work_root: bytes(7, 32),
        execution_cpu: 1n,
        execution_memory: 2n,
        verdict: "Pending" as const,
        rejection_code_hash: bytes(8, 32),
        ledger_delta_root: bytes(9, 32),
      },
      trace_proof: {
        state_index: 9n,
        state_hash: bytes(10, 32),
        siblings: [bytes(11, 32)],
      },
      phase: 8n,
      program_counter: 9n,
      witness_cbor: "80",
      auxiliary: "NoAuxiliaryWitness" as const,
    };
    const keyCbor = encodeRetainedValidationWitnessKeyV1(key);
    const valueCbor = encodeRetainedValidationWitnessV1(value);
    expect(decodeRetainedValidationWitnessKeyV1(keyCbor)).toEqual(key);
    expect(decodeRetainedValidationWitnessV1(valueCbor)).toEqual(value);
    const withWitness = payload(1);
    withWitness.block_body.validation_trace_witnesses = [
      [keyCbor.toString("hex"), valueCbor.toString("hex")],
    ];
    expect(daPayloadV1EncodedSize(withWitness)).toBe(
      encodeDaPayloadV1(withWitness).length,
    );
  });

  it("pins the exact V1 body/count field order and constructor arities", () => {
    const value = emptyPayload();
    const encoded = encodeDaPayloadV1(value);

    expect(encoded).toEqual(Buffer.from(Data.to(value, DaPayloadV1), "hex"));
    expect(encoded).toHaveLength(446);
    expect(Buffer.from(sha256(encoded)).toString("hex")).toBe(
      "e033dde650d6160e438b2b061adc17d481260f95b875eb8634a6eeefb7f23b71",
    );

    for (const trailingBreaks of [1, 2, 3]) {
      const insertion = encoded.length - trailingBreaks;
      const extraField = Buffer.concat([
        encoded.subarray(0, insertion),
        Buffer.from([0]),
        encoded.subarray(insertion),
      ]);
      expect(() => decodeDaPayloadV1(extraField)).toThrow(
        /unexpected CBOR framing/u,
      );
    }
  });

  it("uses the newest payload shape and is byte-identical to its schema", () => {
    for (let seed = 0; seed < 20; seed += 1) {
      const value = payload(seed);
      const expected = Buffer.from(Data.to(value, DaPayloadV1), "hex");
      const encoded = encodeDaPayloadV1(value);
      expect(encoded).toEqual(expected);
      expect(daPayloadV1EncodedSize(value)).toBe(encoded.length);
      expect(decodeDaPayloadV1(encoded)).toEqual(value);
    }
  });

  it("sizes maintained UTxO aggregates exactly and rejects aggregate drift", () => {
    const value = payload(1);
    const encodedTupleBytes = value.block_body.utxos.reduce(
      (sum, entry) => sum + daPayloadEntryEncodedSize(entry),
      0,
    );
    expect(
      daPayloadV1EncodedSizeFromUtxoAggregate(value, {
        entryCount: value.block_body.utxos.length,
        encodedTupleBytes,
      }),
    ).toBe(encodeDaPayloadV1(value).length);
    expect(() =>
      daPayloadEntriesEncodedSizeFromAggregate({
        entryCount: 0,
        encodedTupleBytes: 1,
      }),
    ).toThrow("must have zero tuple bytes");
  });

  it("fails closed on every version other than 1", () => {
    const value = payload(1);
    expect(() => encodeDaPayloadV1({ ...value, version: 23n })).toThrow(
      /version must equal 1/u,
    );
    const encoded = encodeDaPayloadV1(value);
    const wrongVersion = Buffer.from(encoded);
    wrongVersion[3] = 23;
    expect(() => decodeDaPayloadV1(wrongVersion)).toThrow(
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
        encodeDaPayloadV1({
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
        encodeDaPayloadV1({
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
      encodeDaPayloadV1({
        ...value,
        block_body: { ...value.block_body, header_hash: "not-hex" },
      }),
    ).toThrow("even-length hexadecimal");

    const canonical = encodeDaPayloadV1(value);
    expect(() =>
      decodeDaPayloadV1(
        replaceFirst(
          canonical,
          Buffer.from([0xd8, 0x79, 0x9f, 0x01]),
          Buffer.from([0xd8, 0x79, 0x9f, 0x18, 0x01]),
        ),
      ),
    ).toThrow("not minimally encoded");

    const shortBytes = Buffer.from(value.block_body.header_hash, "hex");
    expect(() =>
      decodeDaPayloadV1(
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
    const nonEmptyBytes = daPayloadV1EncodedSize({
      ...empty,
      block_body: {
        ...empty.block_body,
        cek_program_material: [minimumMaterialEntry],
      },
    });

    expect(daPayloadV1EncodedSize(empty)).toBe(446);
    expect(tupleBytes).toBe(MIDGARD_CEK_MIN_PROGRAM_MATERIAL_DA_TUPLE_BYTES_V1);
    expect(nonEmptyBytes).toBe(
      MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES_V1 + tupleBytes,
    );
    expect(MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1).toBe(
      BigInt(
        Math.floor(
          (MIDGARD_MAX_DA_PAYLOAD_BYTES_V1 -
            MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES_V1) /
            tupleBytes,
        ),
      ),
    );
    expect(MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1).toBe(
      BigInt(
        MIDGARD_MAX_DA_PAYLOAD_BYTES_V1 -
          MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES_V1,
      ),
    );
  });
});
