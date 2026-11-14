import { CML } from "@lucid-evolution/lucid";
import { ConfigProvider, Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "../src/database/utils/tx.js";
import { blockExUnitsBudgetConfig } from "../src/services/config.block-ex-units-budget.js";
import {
  declaredTxExUnits,
  DEFAULT_BLOCK_EX_UNITS_BUDGET,
  DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
  planCommitBatchBudgets,
  selectCommitTxCandidates,
} from "../src/workers/utils/commit-block-planner.js";
import { buildNativeTx } from "./native-transaction-integration.build-native-tx.js";
import { makeRedeemersPreimageCbor } from "./native-transaction-integration.make-mint-preimage.js";

/** A normal transaction declaring one redeemer of the given ex-units. */
const scriptTx = (memory: bigint, steps: bigint): EntryWithTimeStamp => {
  const { txId, txCbor } = buildNativeTx({
    redeemerTxWitsPreimageCbor: makeRedeemersPreimageCbor([
      { tag: CML.RedeemerTag.Spend, index: 0n, exUnits: [memory, steps] },
    ]),
  });
  return {
    [TxColumns.TX_ID]: Buffer.from(txId),
    [TxColumns.TX]: Buffer.from(txCbor),
    [TxColumns.TIMESTAMPTZ]: new Date("2026-01-01T00:00:00.000Z"),
  };
};

const plan = (txs: readonly EntryWithTimeStamp[]) =>
  planCommitBatchBudgets({
    candidateSelection: selectCommitTxCandidates({
      mempoolTxs: txs,
      processedMempoolTxs: [],
    }),
    limits: DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
  });

describe("commit planner per-block ex-units budget", () => {
  it("defaults to twice the per-transaction cap", () => {
    expect(DEFAULT_BLOCK_EX_UNITS_BUDGET).toEqual({
      memory: 33_000_000n,
      steps: 20_000_000_000n,
    });
    expect(DEFAULT_COMMIT_BATCH_BUDGET_LIMITS.blockExUnitsBudget).toBe(
      DEFAULT_BLOCK_EX_UNITS_BUDGET,
    );
  });

  it("reads the declared ex-units of a transaction's redeemers", () => {
    const tx = scriptTx(7_000_000n, 3_000_000_000n);
    expect(declaredTxExUnits(tx[TxColumns.TX])).toEqual({
      memory: 7_000_000n,
      steps: 3_000_000_000n,
    });
    expect(declaredTxExUnits(Buffer.from("not a transaction"))).toEqual({
      memory: 0n,
      steps: 0n,
    });
  });

  it("fills the memory budget exactly and stops before exceeding it", () => {
    const planned = plan([
      scriptTx(16_500_000n, 1n),
      scriptTx(16_500_000n, 1n),
      scriptTx(1n, 1n),
    ]);
    expect(planned.plan.selectedTxCount).toBe(2);
    expect(planned.plan.stopReason).toBe("ex_units_budget");
    expect(planned.prunedTxCount).toBe(1);
  });

  it("fills the steps budget exactly and stops before exceeding it", () => {
    const planned = plan([
      scriptTx(1n, 10_000_000_000n),
      scriptTx(1n, 10_000_000_000n),
      scriptTx(1n, 1n),
    ]);
    expect(planned.plan.selectedTxCount).toBe(2);
    expect(planned.plan.stopReason).toBe("ex_units_budget");
  });

  it("does not budget-gate transactions without redeemers", () => {
    const planned = plan([
      scriptTx(16_500_000n, 10_000_000_000n),
      scriptTx(16_500_000n, 10_000_000_000n),
      ...Array.from({ length: 3 }, () => {
        const { txId, txCbor } = buildNativeTx();
        return {
          [TxColumns.TX_ID]: Buffer.from(txId),
          [TxColumns.TX]: Buffer.from(txCbor),
          [TxColumns.TIMESTAMPTZ]: new Date("2026-01-01T00:00:00.000Z"),
        };
      }),
    ]);
    expect(planned.plan.selectedTxCount).toBe(5);
    expect(planned.plan.stopReason).toBe("mempool_exhausted");
  });

  it("honours a configured budget in each component", () => {
    const txs = [scriptTx(10n, 10n), scriptTx(10n, 10n)];
    const withBudget = (memory: bigint, steps: bigint) =>
      planCommitBatchBudgets({
        candidateSelection: selectCommitTxCandidates({
          mempoolTxs: txs,
          processedMempoolTxs: [],
        }),
        limits: {
          ...DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
          blockExUnitsBudget: { memory, steps },
        },
      }).plan;
    expect(withBudget(20n, 20n).selectedTxCount).toBe(2);
    expect(withBudget(19n, 20n)).toMatchObject({
      selectedTxCount: 1,
      stopReason: "ex_units_budget",
    });
    expect(withBudget(20n, 19n)).toMatchObject({
      selectedTxCount: 1,
      stopReason: "ex_units_budget",
    });
  });

  it("reads the budget from node config, defaulting to twice the cap", async () => {
    const read = (entries: readonly (readonly [string, string])[]) =>
      Effect.runPromise(
        Effect.withConfigProvider(
          blockExUnitsBudgetConfig,
          ConfigProvider.fromMap(new Map(entries)),
        ),
      );
    await expect(read([])).resolves.toEqual(DEFAULT_BLOCK_EX_UNITS_BUDGET);
    await expect(
      read([
        ["BLOCK_EX_UNITS_BUDGET_MEMORY", "100"],
        ["BLOCK_EX_UNITS_BUDGET_STEPS", "200"],
      ]),
    ).resolves.toEqual({ memory: 100n, steps: 200n });
    for (const bad of ["0", "-1", "1.5", "abc"]) {
      await expect(
        read([["BLOCK_EX_UNITS_BUDGET_STEPS", bad]]),
      ).rejects.toThrow(
        "BLOCK_EX_UNITS_BUDGET_STEPS must be a positive integer",
      );
    }
  });
});
