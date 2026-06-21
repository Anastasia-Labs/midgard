import { describe, expect, it } from "vitest";

import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "@/database/utils/tx.js";
import {
  buildSuccessfulCommitBatches,
  selectCommitRoots,
  selectCommitTxCandidates,
} from "@/workers/utils/commit-block-planner.js";

const mkTxEntry = (seed: number): EntryWithTimeStamp => ({
  [TxColumns.TX_ID]: Buffer.from(seed.toString(16).padStart(64, "0"), "hex"),
  [TxColumns.TX]: Buffer.from(`tx-${seed}`),
  [TxColumns.TIMESTAMPTZ]: new Date("2026-01-01T00:00:00.000Z"),
});

describe("commit block planner", () => {
  it("includes processed mempool txs even when mempool is empty", () => {
    const processed = [mkTxEntry(1), mkTxEntry(2), mkTxEntry(3)];
    const batches = buildSuccessfulCommitBatches([], [], processed, 2);

    const txIds = batches
      .flatMap((b) => b.blockTxHashes)
      .map((b) => b.toString("hex"));
    expect(txIds).toStrictEqual(processed.map((p) => p.tx_id.toString("hex")));
    expect(batches.every((b) => b.clearMempoolTxHashes.length === 0)).toBe(
      true,
    );
  });

  it("includes both mempool and processed txs in block insertion batches", () => {
    const mempool = [mkTxEntry(10), mkTxEntry(11)];
    const mempoolHashes = mempool.map((m) => m.tx_id);
    const processed = [mkTxEntry(12), mkTxEntry(13)];

    const batches = buildSuccessfulCommitBatches(
      mempool,
      mempoolHashes,
      processed,
      2,
    );

    const txIds = batches
      .flatMap((b) => b.blockTxHashes)
      .map((b) => b.toString("hex"));
    expect(txIds).toStrictEqual([
      ...mempoolHashes.map((h) => h.toString("hex")),
      ...processed.map((p) => p.tx_id.toString("hex")),
    ]);

    const cleared = batches
      .flatMap((b) => b.clearMempoolTxHashes)
      .map((b) => b.toString("hex"));
    expect(cleared).toStrictEqual(mempoolHashes.map((h) => h.toString("hex")));
  });

  it("uses computed roots in deposits-only commit path", () => {
    const roots = selectCommitRoots({
      hasTxRequests: false,
      computedUtxoRoot: "0xutxo",
      computedTxRoot: "0xtx",
      emptyRoot: "0xempty",
    });

    expect(roots.utxoRoot).toBe("0xutxo");
    expect(roots.txRoot).toBe("0xtx");
  });

  it("selects durable processed transactions before newer mempool transactions", () => {
    const processed = [mkTxEntry(1)];
    const mempool = [mkTxEntry(2)];

    const selection = selectCommitTxCandidates({
      mempoolTxs: mempool,
      processedMempoolTxs: processed,
    });

    expect(selection.sourceTable).toBe("processed_mempool");
    expect(selection.candidateTxs).toStrictEqual(processed);
    expect(selection.candidateTxHashes.map((h) => h.toString("hex"))).toEqual([
      processed[0][TxColumns.TX_ID].toString("hex"),
    ]);
    expect(selection.candidateTxsSize).toBe(processed[0][TxColumns.TX].length);
  });

  it("selects mempool transactions when there is no deferred processed payload", () => {
    const mempool = [mkTxEntry(3), mkTxEntry(4)];

    const selection = selectCommitTxCandidates({
      mempoolTxs: mempool,
      processedMempoolTxs: [],
    });

    expect(selection.sourceTable).toBe("mempool");
    expect(selection.candidateTxs).toStrictEqual(mempool);
    expect(selection.candidateTxHashes.map((h) => h.toString("hex"))).toEqual(
      mempool.map((entry) => entry[TxColumns.TX_ID].toString("hex")),
    );
  });

  it("selects no tx source when both pending queues are empty", () => {
    const selection = selectCommitTxCandidates({
      mempoolTxs: [],
      processedMempoolTxs: [],
    });

    expect(selection).toMatchObject({
      candidateTxs: [],
      candidateTxHashes: [],
      candidateTxsSize: 0,
      sourceTable: "none",
    });
  });
});
