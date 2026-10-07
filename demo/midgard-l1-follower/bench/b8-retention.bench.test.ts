import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import {
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
} from "../src/index.js";
import { followerMigrations } from "../src/schema/follower-migrations.js";
import { declaredTables } from "../src/schema/lint.js";
import { testDatabases } from "../tests/support/postgres.js";
import {
  benchMigrations,
  linearFit,
  now,
  Workload,
  writeReport,
} from "./support/workload.js";

/**
 * B8 (plan §16.2): the retention soak, follower tables on a synthetic chain.
 * A constant flow holds the tracked live set fixed (each qualifying tx
 * spends one tracked output and creates one, with a token, a tracked mint
 * every fifth tx and a reference script every tenth) for 10 × (k + window)
 * blocks, pruning one budgeted step per block. The follower's only window
 * is k (no DA or archive window applies to its tables), so window = 0.
 *
 * The gating runs spend oldest-first, as deposits and orders are consumed:
 * every output lives LIVE / FLOW_TXS = 200 blocks, the live set is preloaded
 * in the flow's own shape, and the steady state starts after k + 200 blocks.
 * With uniform-random spends the rows a live output retains (its tx, block
 * and script) converge over ~ln(LIVE) × 200 blocks instead, independent of
 * k; that variant is reported at full k, not gated.
 *
 * The scaled k defaults to 500 so the last third spans more than eight
 * output lifetimes. A versioned D-t table (here `bench_address_live`)
 * oscillates within a few percent as spends land on random addresses; at
 * k = 100 the last third is under two lifetimes and that bounded swing fits
 * as a 1.2% slope although the final count is below the starting one.
 *
 * Target: every table's row count plateaus, slope ≤ 1% over the last third.
 * `l1_event_keys` is the §11 exception: it only ever grows, by one small row
 * per event, so it is reported, not held to the plateau.
 */
const LIVE = 2_000;
const FLOW_TXS = 10;
const PRUNE_BUDGET = 500;
const SAMPLES = 1_000;
const GROWS_BY_DESIGN = new Set(["l1_event_keys"]);

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-b8-"));
afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const TABLES = [
  ...declaredTables([
    followerMigrations("postgres"),
    benchMigrations("postgres"),
  ]).map(({ table, tableClass }) => ({
    table,
    tableClass,
  })),
];

const counts = (store: FactStore): Promise<Record<string, number>> =>
  store.transaction("read", async (tx) => {
    const out: Record<string, number> = {};
    for (const { table } of TABLES)
      out[table] = Number(
        (await tx.query(`SELECT count(*) AS n FROM ${table}`))[0]?.n as
          | string
          | number,
      );
    return out;
  });

type Run = Readonly<{
  adapter: "postgres" | "sqlite";
  k: number;
  spendOrder: "fifo" | "random";
}>;

const soak = async ({ adapter, k, spendOrder }: Run) => {
  const workload = new Workload(0xb8_0000 + k, spendOrder);
  const options = workload.options(k, adapter);
  const store =
    adapter === "postgres"
      ? openPostgresFactStore({
          ...options,
          connection: { connectionString: (await databases.create()).url },
        })
      : openSqliteFactStore({
          ...options,
          path: join(scratch, `b8-${String(k)}-${spendOrder}.db`),
        });
  const blocks = 10 * k;
  const every = Math.max(1, Math.floor(blocks / SAMPLES));
  const samples: { block: number; rows: Record<string, number> }[] = [];
  let pruneNotDone = 0;
  const start = now();
  try {
    expect(await store.start()).toMatchObject({ kind: "ready" });
    expect(await store.initialize(workload.origin)).toMatchObject({
      kind: "initialized",
    });
    for (let loaded = 0; loaded < LIVE; loaded += FLOW_TXS)
      expect(
        await store.applyBlock(workload.preloadBlock(FLOW_TXS, 1)),
      ).toMatchObject({ kind: "applied" });
    for (let block = 1; block <= blocks; block += 1) {
      const applied = await store.applyBlock(
        workload.flowBlock(FLOW_TXS, 1, 1),
      );
      if (applied.kind !== "applied")
        throw new Error(
          `apply failed: ${JSON.stringify(applied).slice(0, 300)}`,
        );
      const pruned = await store.prune(PRUNE_BUDGET);
      if ("kind" in pruned) throw pruned.error;
      if (!pruned.done) pruneNotDone += 1;
      if (block % every === 0)
        samples.push({ block, rows: await counts(store) });
    }
    expect((await store.checkInvariants()).ok).toBe(true);
  } finally {
    await store.close();
  }
  const lastThird = samples.filter((sample) => sample.block > (2 * blocks) / 3);
  const span = (lastThird.at(-1)?.block ?? 0) - (lastThird[0]?.block ?? 0);
  const tables = TABLES.map(({ table, tableClass }) => {
    const series = lastThird.map(
      (sample) => [sample.block, sample.rows[table] ?? 0] as const,
    );
    const mean =
      series.reduce((sum, [, rows]) => sum + rows, 0) /
      Math.max(1, series.length);
    const fit = linearFit(series);
    const growth = mean === 0 ? 0 : (fit.slope * span) / mean;
    return {
      table,
      class: tableClass,
      atOneThird: lastThird[0]?.rows[table] ?? 0,
      final: lastThird.at(-1)?.rows[table] ?? 0,
      max: Math.max(...samples.map((sample) => sample.rows[table] ?? 0)),
      rowsPerBlock: Math.round(fit.slope * 1e4) / 1e4,
      growthOverLastThirdPct: Math.round(growth * 1e4) / 100,
      plateau: growth <= 0.01,
      exempt: GROWS_BY_DESIGN.has(table),
    };
  });
  return {
    adapter,
    k,
    spendOrder,
    blocks,
    liveOutRefs: LIVE,
    flowTxsPerBlock: FLOW_TXS,
    pruneBudget: PRUNE_BUDGET,
    pruneNotDone,
    seconds: Math.round(now() - start) / 1000,
    tables,
    samples,
  };
};

const SCALED_K = Number(process.env.L1_FOLLOWER_B8_K ?? "500");
const FULL_K = process.env.L1_FOLLOWER_B8_FULL_K === "1";

const RUNS: readonly Run[] = [
  { adapter: "postgres", k: SCALED_K, spendOrder: "fifo" },
  { adapter: "sqlite", k: SCALED_K, spendOrder: "fifo" },
  ...(FULL_K
    ? ([
        { adapter: "postgres", k: 2_160, spendOrder: "fifo" },
        { adapter: "sqlite", k: 2_160, spendOrder: "fifo" },
      ] as const)
    : []),
];

const report = async (run: Run) => {
  const result = await soak(run);
  console.info(
    `B8 report: ${writeReport(`b8-retention-${run.adapter}-k${String(run.k)}-${run.spendOrder}`, result)}\n${JSON.stringify({ ...result, samples: undefined }, null, 2)}`,
  );
  return result;
};

describe("B8: retention soak (follower tables, synthetic chain)", () => {
  it.each(RUNS)(
    "every table plateaus over 10 × k blocks ($adapter, k = $k, $spendOrder spends)",
    async (run) => {
      const result = await report(run);
      for (const table of result.tables.filter((row) => !row.exempt))
        expect({ table: table.table, plateau: table.plateau }).toEqual({
          table: table.table,
          plateau: true,
        });
    },
  );

  it.skipIf(!FULL_K)(
    "reports uniform-random spends at full k (postgres, not gated)",
    async () => {
      await report({ adapter: "postgres", k: 2_160, spendOrder: "random" });
    },
  );
});
