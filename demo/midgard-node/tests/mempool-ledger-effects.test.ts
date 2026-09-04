import { describe, expect, it } from "vitest";

import { compactLedgerEffects } from "../src/database/mempool.js";
import * as Ledger from "../src/database/utils/ledger.js";
import type { ProcessedTx } from "../src/utils.js";

const bytes = (value: number, length = 32): Buffer =>
  Buffer.alloc(length, value);

const entry = (txIdByte: number, outRef: Buffer): Ledger.Entry => ({
  [Ledger.Columns.TX_ID]: bytes(txIdByte),
  [Ledger.Columns.OUTREF]: outRef,
  [Ledger.Columns.OUTPUT]: bytes(txIdByte, 64),
  [Ledger.Columns.ADDRESS]: `addr_test_${txIdByte}`,
});

describe("accepted-batch ledger effect compaction", () => {
  it("persists only final outputs and spends that predate the batch", () => {
    const externalA = bytes(1, 36);
    const externalB = bytes(2, 36);
    const intermediateA = bytes(3, 36);
    const finalA = bytes(4, 36);
    const finalB = bytes(5, 36);
    const processed: readonly ProcessedTx[] = [
      {
        txId: bytes(10),
        txCbor: bytes(10, 64),
        spent: [externalA],
        produced: [entry(10, intermediateA)],
      },
      {
        txId: bytes(11),
        txCbor: bytes(11, 64),
        spent: [intermediateA],
        produced: [entry(11, finalA)],
      },
      {
        txId: bytes(12),
        txCbor: bytes(12, 64),
        spent: [externalB],
        produced: [entry(12, finalB)],
      },
    ];

    const compacted = compactLedgerEffects(processed);

    expect(
      compacted.produced.map((output) =>
        output[Ledger.Columns.OUTREF].toString("hex"),
      ),
    ).toStrictEqual([finalA.toString("hex"), finalB.toString("hex")]);
    expect(
      compacted.spent.map((outRef) => outRef.toString("hex")),
    ).toStrictEqual([externalA.toString("hex"), externalB.toString("hex")]);
  });

  it("deduplicates repeated external spends without masking final outputs", () => {
    const external = bytes(20, 36);
    const final = bytes(21, 36);
    const compacted = compactLedgerEffects([
      {
        txId: bytes(20),
        txCbor: bytes(20, 64),
        spent: [external, external],
        produced: [entry(20, final)],
      },
    ]);

    expect(compacted.spent).toHaveLength(1);
    expect(compacted.produced).toHaveLength(1);
  });
});
