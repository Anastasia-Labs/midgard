import { afterAll, describe, expect, it } from "vitest";

import {
  type BlockSummary,
  type FactStore,
  openPostgresFactStore,
} from "../src/index.js";
import { runInvariantChecks } from "../src/store/invariants.js";
import { testDatabases } from "../tests/support/postgres.js";
import {
  linearFit,
  median,
  now,
  Workload,
  writeReport,
} from "./support/workload.js";

/**
 * B3 (plan §16.2): rewind plus recompute, MPF excluded, on Postgres at
 * N = 10^6 tracked live outputs. "Recompute" at the follower is the
 * registered D-t truncation the rewind runs; the bench projections are a
 * versioned per-address live count and an append-only spend log.
 *
 * Targets: depth 30 ≤ 200 ms; depth 2,160 ≤ 5 s; time linear in the rows
 * above the target and independent of N.
 */
const K = 2_160;
const N = Number(process.env.L1_FOLLOWER_B3_N ?? "1000000");
const SMALL_N = 10_000;
const FLOW_TXS = 10;
const PRELOAD_TXS = 20;
const PRELOAD_OUTPUTS = 100;
const PLAN: readonly (readonly [depth: number, reps: number])[] = [
  [30, 7],
  [100, 3],
  [300, 3],
  [1_000, 2],
  [2_160, 3],
];

const databases = testDatabases();
afterAll(async () => {
  await databases.dropAll();
});

const count = async (
  store: FactStore,
  sql: string,
  slot: number,
): Promise<number> =>
  store.transaction("read", async (tx) =>
    Number(
      (
        await tx.query(
          sql,
          sql
            .split("?")
            .slice(1)
            .map(() => slot),
        )
      )[0]?.n as string | number,
    ),
  );

/** Every row the rewind to `slot` deletes or reopens. */
const rowsAbove = async (
  store: FactStore,
  slot: number,
): Promise<Record<string, number>> => ({
  l1_blocks: await count(
    store,
    "SELECT count(*) AS n FROM l1_blocks WHERE slot > ?",
    slot,
  ),
  l1_txs: await count(
    store,
    "SELECT count(*) AS n FROM l1_txs WHERE block_slot > ?",
    slot,
  ),
  l1_tx_mint_policies: await count(
    store,
    "SELECT count(*) AS n FROM l1_tx_mint_policies m JOIN l1_txs t ON t.tx_hash = m.tx_hash WHERE t.block_slot > ?",
    slot,
  ),
  l1_outputs_deleted: await count(
    store,
    "SELECT count(*) AS n FROM l1_outputs WHERE created_slot > ?",
    slot,
  ),
  l1_outputs_unspent: await count(
    store,
    "SELECT count(*) AS n FROM l1_outputs WHERE spent_slot > ? AND created_slot <= ?",
    slot,
  ),
  l1_output_assets: await count(
    store,
    "SELECT count(*) AS n FROM l1_output_assets a JOIN l1_outputs o USING (tx_hash, output_index) WHERE o.created_slot > ?",
    slot,
  ),
  l1_event_keys: await count(
    store,
    "SELECT count(*) AS n FROM l1_event_keys WHERE first_canonical_slot > ?",
    slot,
  ),
  bench_spend_log: await count(
    store,
    "SELECT count(*) AS n FROM bench_spend_log WHERE spent_slot > ?",
    slot,
  ),
  bench_address_live_deleted: await count(
    store,
    "SELECT count(*) AS n FROM bench_address_live WHERE from_slot > ?",
    slot,
  ),
  bench_address_live_reopened: await count(
    store,
    "SELECT count(*) AS n FROM bench_address_live WHERE to_slot > ? AND from_slot <= ?",
    slot,
  ),
});

type Sample = {
  depth: number;
  rows: number;
  rewindMs: number;
  checkMs: number;
  reapplyMs: number;
};

const applyAll = async (
  store: FactStore,
  blocks: readonly BlockSummary[],
): Promise<number> => {
  const start = now();
  for (const block of blocks) {
    const result = await store.applyBlock(block);
    if (result.kind !== "applied")
      throw new Error(`apply failed: ${JSON.stringify(result).slice(0, 300)}`);
  }
  return now() - start;
};

const run = async (n: number, seed: number, plan: typeof PLAN) => {
  const workload = new Workload(seed);
  const { url } = await databases.create();
  const store = openPostgresFactStore({
    ...workload.options(K, "postgres"),
    connection: { connectionString: url },
  });
  try {
    expect(await store.start()).toMatchObject({ kind: "ready" });
    expect(await store.initialize(workload.origin)).toMatchObject({
      kind: "initialized",
    });
    const perBlock = PRELOAD_TXS * PRELOAD_OUTPUTS;
    const preloadStart = now();
    for (let loaded = 0; loaded < n; loaded += perBlock)
      await applyAll(store, [
        workload.preloadBlock(
          PRELOAD_TXS,
          Math.min(PRELOAD_OUTPUTS, Math.ceil((n - loaded) / PRELOAD_TXS)),
        ),
      ]);
    const preloadMs = now() - preloadStart;
    const flow = Array.from({ length: K + 40 }, () =>
      workload.flowBlock(FLOW_TXS),
    );
    const flowMs = await applyAll(store, flow);
    await store.transaction("write", (tx) => tx.query("ANALYZE"));
    const live = store.liveOutRefCount();
    const samples: Sample[] = [];
    for (const [depth, reps] of plan)
      for (let rep = 0; rep < reps; rep += 1) {
        const target = flow[flow.length - 1 - depth];
        if (target === undefined) throw new Error("depth beyond the flow");
        const rows = Object.values(
          await rowsAbove(store, target.point.slot),
        ).reduce((a, b) => a + b, 0);
        const start = now();
        const result = await store.rewind(target.point);
        const rewindMs = now() - start;
        expect(result).toMatchObject({ kind: "rewound", depth });
        const checkStart = now();
        await store.transaction("write", (tx) =>
          runInvariantChecks(tx, store.dialect, store.registry, "post_rewind"),
        );
        const checkMs = now() - checkStart;
        const reapplyMs = await applyAll(
          store,
          flow.slice(flow.length - depth),
        );
        samples.push({ depth, rows, rewindMs, checkMs, reapplyMs });
      }
    const full = await store.checkInvariants();
    expect(full.ok).toBe(true);
    return { n, live, preloadMs, flowBlocks: flow.length, flowMs, samples };
  } finally {
    await store.close();
  }
};

const summarise = (samples: readonly Sample[]) =>
  [...new Set(samples.map((sample) => sample.depth))].map((depth) => {
    const at = samples.filter((sample) => sample.depth === depth);
    return {
      depth,
      rows: median(at.map((sample) => sample.rows)),
      rewindMsMedian:
        Math.round(median(at.map((sample) => sample.rewindMs)) * 10) / 10,
      rewindMsMax:
        Math.round(Math.max(...at.map((sample) => sample.rewindMs)) * 10) / 10,
      scopedCheckMs:
        Math.round(median(at.map((sample) => sample.checkMs)) * 10) / 10,
      reapplyMsPerBlock:
        Math.round(
          (median(at.map((sample) => sample.reapplyMs)) / depth) * 100,
        ) / 100,
    };
  });

describe("B3: rewind cost on Postgres (MPF excluded)", () => {
  it(`depth 30 ≤ 200 ms and depth 2,160 ≤ 5 s at N = ${N}, linear in rows above the target`, async () => {
    const large = await run(N, 0xb3_0001, PLAN);
    const small = await run(SMALL_N, 0xb3_0002, [
      [30, 7],
      [300, 3],
      [2_160, 1],
    ]);
    const byDepth = summarise(large.samples);
    // Linearity is fitted on the per-depth medians (one autovacuum or
    // checkpoint stall in a single repetition is not a property of the rewind).
    const fit = linearFit(
      byDepth.map((row) => [row.rows, row.rewindMsMedian] as const),
    );
    const smallByDepth = summarise(small.samples);
    const at = (rows: ReturnType<typeof summarise>, depth: number) =>
      rows.find((row) => row.depth === depth);
    const report = {
      bench: "B3",
      adapter: "postgres",
      k: K,
      flowTxsPerBlock: FLOW_TXS,
      large: {
        n: large.n,
        live: large.live,
        preloadMs: Math.round(large.preloadMs),
        byDepth,
      },
      small: { n: small.n, live: small.live, byDepth: smallByDepth },
      linearFit: {
        msPerRow: fit.slope,
        interceptMs: fit.intercept,
        r2: fit.r2,
      },
      nIndependence: [30, 300, 2_160].map((depth) => ({
        depth,
        ratioLargeOverSmall:
          (at(byDepth, depth)?.rewindMsMedian ?? 0) /
          (at(smallByDepth, depth)?.rewindMsMedian ?? 1),
      })),
      targets: {
        depth30Ms: { target: 200, measured: at(byDepth, 30)?.rewindMsMax },
        depth2160Ms: {
          target: 5_000,
          measured: at(byDepth, 2_160)?.rewindMsMax,
        },
      },
      samples: large.samples,
    };
    console.info(
      `B3 report: ${writeReport("b3-rewind", report)}\n${JSON.stringify({ ...report, samples: undefined }, null, 2)}`,
    );
    expect(at(byDepth, 30)?.rewindMsMax).toBeLessThanOrEqual(200);
    expect(at(byDepth, 2_160)?.rewindMsMax).toBeLessThanOrEqual(5_000);
    expect(fit.r2).toBeGreaterThanOrEqual(0.95);
  });
});
