/**
 * What the import checker stores per block: each transaction once (its
 * bytes, carrying its scripts) and each output it spends or references
 * once per block, so a block's stored bytes grow linearly with its
 * transactions' sizes, however many runs read the same outputs. Each test
 * prints what it measured.
 */
import {
  makeMinAdaFundedExactSizeOutputItem,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  db,
  processBlock,
  useCheckerDatabase,
} from "./import-check.checker.fixture.js";
import {
  committedBlock,
  fundedBy,
  type LedgerEntry,
  type Spend,
} from "./import-check.fixture.js";
import { resetApplicationTables } from "./utils.js";

useCheckerDatabase();

/** The bytes the set stores: entries and outputs. */
const stored = () =>
  db(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ checks: string; outputs: string; outputRows: string }>`
        SELECT
          (SELECT COALESCE(SUM(octet_length(tx_cbor)
            + octet_length(script_hashes)), 0) FROM node_import_checks)
            AS checks,
          (SELECT COALESCE(SUM(octet_length(out_ref) + octet_length(output)), 0)
            FROM node_import_check_outputs) AS outputs,
          (SELECT COUNT(*) FROM node_import_check_outputs) AS "outputRows"`,
    ),
  ).then(([row]) => ({
    bytes: Number(row!.checks) + Number(row!.outputs),
    outputs: Number(row!.outputs),
    outputRows: Number(row!.outputRows),
  }));

const txBytes = (block: Awaited<ReturnType<typeof committedBlock>>) =>
  block.payload.block_body.transaction_preimages.reduce(
    (sum, [, cbor]) => sum + cbor.length / 2,
    0,
  );

/** One block of `spends` on `parent`, processed on an empty set. */
const storedFor = async (
  parent: readonly LedgerEntry[],
  spends: readonly Spend[],
) => {
  const block = await committedBlock(parent, spends);
  await db(resetApplicationTables);
  await processBlock(block);
  return { block, ...(await stored()) };
};

describe("import check storage", () => {
  it("grows linearly with a transaction's spent inputs, one run each", async () => {
    const rows: { inputs: number; txBytes: number; stored: number }[] = [];
    for (const inputs of [1, 8, 32, 64]) {
      const parent = Array.from({ length: inputs }, (_, index) =>
        fundedBy(0x10 + index, "accept"),
      );
      const { block, bytes } = await storedFor(parent, [
        {
          input: parent[0]!.outRef,
          extra: parent.slice(1).map((entry) => entry.outRef),
          body: "accept",
          next: "accept",
          // The per-transaction cpu limit, shared between the runs.
          cpu: 10_000_000_000n / BigInt(inputs),
        },
      ]);
      expect(block.runs[0]).toHaveLength(inputs);
      rows.push({ inputs, txBytes: txBytes(block), stored: bytes });
    }
    console.table(rows);
    // Every input adds the same bytes, its transaction input and its
    // output, within the encoding's integer widths.
    const perInput = (from: number, to: number) =>
      (rows[to]!.stored - rows[from]!.stored) /
      (rows[to]!.inputs - rows[from]!.inputs);
    const slope = perInput(0, 1);
    expect(perInput(1, 2)).toBeLessThan(slope * 1.1);
    expect(perInput(2, 3)).toBeLessThan(slope * 1.1);
    expect(rows[3]!.stored).toBeLessThan(rows[0]!.stored + 63 * slope * 1.1);
  }, 120_000);

  it("stores a block's 16 KB reference outputs once however many transactions and runs read them, within bounded import memory", async () => {
    const OUTPUT_BYTES = 16 * 1024;
    // Transactions reading the same references: the measured max shape,
    // 8; the environment may only raise it.
    const TRANSACTIONS = Math.max(
      8,
      Number(process.env.IMPORT_CHECK_MAX_SHAPE_TRANSACTIONS ?? 8),
    );
    // The reference inputs one transaction fits within the size cap: the
    // bytes each one adds, from two small transactions.
    const refs = (count: number, firstByte: number) =>
      Array.from({ length: count }, (_, index) =>
        outRefFromByte(firstByte, BigInt(index)),
      );
    const sized = async (count: number) => {
      const own = fundedBy(0x01, "accept");
      const parent = [
        own,
        ...refs(count, 0x02).map((outRef) => ({
          outRef,
          output: fundedBy(0x03, "accept").output,
        })),
      ];
      const block = await committedBlock(parent, [
        {
          input: own.outRef,
          references: refs(count, 0x02),
          body: "accept",
          next: "accept",
        },
      ]);
      return txBytes(block);
    };
    const base = await sized(0);
    const perReference = ((await sized(300)) - base) / 300;
    let references = Math.floor((16_384 - base) / perReference);
    while ((await sized(references)) > 16_384) references -= 1;

    const referenced: LedgerEntry[] = refs(references, 0x04).map((outRef) => ({
      outRef,
      output: makeMinAdaFundedExactSizeOutputItem(OUTPUT_BYTES),
    }));
    const own = Array.from({ length: TRANSACTIONS }, (_, index) =>
      fundedBy(0x40 + index, "accept"),
    );
    const block = await committedBlock(
      [...own, ...referenced],
      own.map(
        (entry): Spend => ({
          input: entry.outRef,
          references: referenced.map((item) => item.outRef),
          body: "accept",
          next: "accept",
        }),
      ),
    );
    const maxTxBytes = Math.max(
      ...block.payload.block_body.transaction_preimages.map(
        ([, cbor]) => cbor.length / 2,
      ),
    );
    expect(maxTxBytes).toBeLessThanOrEqual(16_384);
    const contextBytes = block.runs[0]![0]!.contextCbor.byteLength;

    global.gc?.();
    const baseline = process.memoryUsage().rss;
    let peak = baseline;
    const sampler = setInterval(() => {
      peak = Math.max(peak, process.memoryUsage().rss);
    }, 5);
    try {
      await processBlock(block);
    } finally {
      clearInterval(sampler);
    }
    peak = Math.max(peak, process.memoryUsage().rss);
    const set = await stored();
    const report = {
      references,
      maxTxBytes,
      transactions: TRANSACTIONS,
      runs: block.runs.flat().length,
      contextBytesPerRun: contextBytes,
      contextBytesAllRuns: contextBytes * block.runs.flat().length,
      storedBytes: set.bytes,
      storedOutputBytes: set.outputs,
      baselineRssMiB: Math.round(baseline / 2 ** 20),
      peakRssMiB: Math.round(peak / 2 ** 20),
      peakGrowthMiB: Math.round((peak - baseline) / 2 ** 20),
    };
    console.table(report);
    // Each output once: the references, and each transaction's own input.
    expect(set.outputRows).toBe(references + TRANSACTIONS);
    expect(set.outputs).toBeLessThan(
      (references + TRANSACTIONS) * (OUTPUT_BYTES + 64),
    );
    // Less than one run's context per transaction beyond the outputs.
    expect(set.bytes - set.outputs).toBeLessThan(TRANSACTIONS * 20_000);
    // The import builds one run's context at a time: its memory stays
    // within a small multiple of one context and the stored outputs, far
    // below every run's contexts together.
    expect(peak - baseline).toBeLessThan(
      Math.max(256 * 2 ** 20, 8 * (contextBytes + set.outputs)),
    );
  }, 300_000);
});
