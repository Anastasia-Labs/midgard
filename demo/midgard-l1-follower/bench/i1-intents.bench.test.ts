import { afterAll, describe, expect, it } from "vitest";

import {
  appendIntentEventIn,
  createIntentReconciler,
  decodeBlock,
  type FactStore,
  intentJournalProjection,
  openPostgresFactStore,
  type OutRef,
  projectionStoreOptions,
  recordIntentIn,
} from "../src/index.js";
import {
  encodeSimTx,
  SIM_ORIGIN,
  SimChain,
  type SimTx,
  simTxHash,
  simUniverse,
} from "../src/testing/index.js";
import { testDatabases } from "../tests/support/postgres.js";
import { now, writeReport } from "./support/workload.js";

/**
 * I1-fix F4 (plan §3.4, §16.2): one S6 pass and one prune step against the
 * size of the intent journal, on Postgres. The journal holds `TERMINAL`
 * intents terminal for less than k (a third landed, a third expired, a third
 * abandoned, so none is prunable yet) plus `LIVE` live intents in the
 * mempool. The per-block target is B2's: p99 ≤ 50 ms, and p99 at the large
 * journal at most 1.2 × p99 at the small one.
 */
const SIZES = [100, 10_000] as const;
const LIVE = 10;
const K = 50;
const SAMPLES = 30;
const P99_MS = 50;
const RATIO = 1.2;
const OUTPUTS_PER_FUNDING = 100;
const RECORDS_PER_TX = 500;
const TXS_PER_BLOCK = 500;

const databases = testDatabases();
afterAll(async () => {
  await databases.dropAll();
});

const u = simUniverse();

const percentile = (values: readonly number[], p: number): number => {
  const sorted = [...values].sort((a, b) => a - b);
  return sorted[Math.min(sorted.length - 1, Math.ceil(p * sorted.length) - 1)]!;
};

type Measured = Readonly<{
  terminal: number;
  s6: Readonly<{ p50: number; p99: number; max: number }>;
  prune: Readonly<{ p50: number; p99: number; max: number }>;
}>;

const stats = (samples: readonly number[]) => ({
  p50: percentile(samples, 0.5),
  p99: percentile(samples, 0.99),
  max: Math.max(...samples),
});

const measure = async (terminal: number): Promise<Measured> => {
  const database = await databases.create();
  const store: FactStore = openPostgresFactStore({
    ...projectionStoreOptions(
      [intentJournalProjection],
      { securityParameter: K, trackedSet: u.tracked },
      "postgres",
    ),
    connection: { connectionString: database.url },
  });
  await store.start();
  try {
    await store.initialize(SIM_ORIGIN);
    const chain = new SimChain(u, SIM_ORIGIN);
    const forward = async (txs: readonly SimTx[] = []) => {
      const { encoded } = chain.forward(txs);
      const applied = await store.applyBlock(decodeBlock(encoded.raw));
      if (applied.kind !== "applied") throw new Error(applied.kind);
    };
    // Funding: enough tracked outputs for every intent.
    const needed = terminal + LIVE;
    const funded: OutRef[] = [];
    while (funded.length < needed) {
      const fundings: SimTx[] = [];
      for (let i = 0; i < 20 && funded.length < needed; i += 1) {
        const tx: SimTx = {
          inputs: [chain.outsideInput()],
          outputs: Array.from({ length: OUTPUTS_PER_FUNDING }, () => ({
            address: u.trackedAddress,
            lovelace: 5_000_000n,
          })),
          nonce: chain.nonce(),
        };
        fundings.push(tx);
        for (let index = 0; index < OUTPUTS_PER_FUNDING; index += 1)
          funded.push({ txHash: simTxHash(tx), index });
      }
      await forward(fundings);
    }
    // Enough blocks that the prune boundary (k below the tip) exists and
    // lies below every terminal slot.
    for (let i = 0; i < K + 2; i += 1) await forward();
    const spendOf = (input: OutRef, extra: Partial<SimTx> = {}): SimTx => ({
      inputs: [input],
      outputs: [{ address: u.trackedAddress, lovelace: 4_000_000n }],
      nonce: chain.nonce(),
      ...extra,
    });
    const third = Math.floor(terminal / 3);
    const landed = funded.slice(0, third).map((o) => spendOf(o));
    const expiresAt = chain.nextSlot() + 1;
    const expired = funded
      .slice(third, 2 * third)
      .map((o) => spendOf(o, { invalidAfter: expiresAt }));
    const abandoned = funded.slice(2 * third, terminal).map((o) => spendOf(o));
    const live = funded.slice(terminal, needed).map((o) => spendOf(o));
    const all = [...landed, ...expired, ...abandoned, ...live];
    for (let start = 0; start < all.length; start += RECORDS_PER_TX)
      await store.transaction("write", async (tx) => {
        for (const sim of all.slice(start, start + RECORDS_PER_TX)) {
          const result = await recordIntentIn(tx, store.dialect, {
            family: "commit",
            workflowKey: `bench:${simTxHash(sim).toString("hex")}`,
            txCbor: encodeSimTx(sim),
            isOwnOutput: (output) => output.address.equals(u.trackedAddress),
          });
          if (result.kind !== "recorded") throw new Error(result.kind);
        }
      });
    for (let start = 0; start < landed.length; start += TXS_PER_BLOCK)
      await forward(landed.slice(start, start + TXS_PER_BLOCK));
    await forward();
    await forward();
    const tipSlot = chain.tip.point.slot;
    await store.transaction("write", async (tx) => {
      for (const sim of abandoned)
        await appendIntentEventIn(
          tx,
          store.dialect,
          simTxHash(sim),
          "abandoned",
          { tipSlot },
        );
    });
    const reconciler = createIntentReconciler({
      dialect: store.dialect,
      transaction: (mode, run) => store.transaction(mode, run),
      securityParameter: K,
      inMempool: () => Promise.resolve(true),
      wanted: () => Promise.resolve(true),
      submit: () => Promise.resolve({ kind: "accepted" }),
    });
    const first = await reconciler.reconcile();
    expect(first.intents).toHaveLength(terminal + LIVE);
    const s6: number[] = [];
    const prune: number[] = [];
    for (let i = 0; i < SAMPLES; i += 1) {
      const start = now();
      await reconciler.reconcile();
      s6.push(now() - start);
      const before = now();
      const step = await store.prune();
      prune.push(now() - before);
      // The hook ran (its table is in the step) and deleted nothing.
      if (!("deleted" in step)) throw new Error(JSON.stringify(step));
      expect(step.deleted.l1_intents).toBe(0);
    }
    return { terminal, s6: stats(s6), prune: stats(prune) };
  } finally {
    await store.close();
  }
};

describe("I1-fix F4: S6 pass and prune step against the journal size", () => {
  it("reports p50/p99 at 10^2 and 10^4 terminal intents against B2's bound", async () => {
    const results: Measured[] = [];
    for (const size of SIZES) results.push(await measure(size));
    const [small, large] = results as [Measured, Measured];
    const report = {
      target: { p99Ms: P99_MS, ratio: RATIO },
      results,
      ratio: {
        s6: large.s6.p99 / small.s6.p99,
        prune: large.prune.p99 / small.prune.p99,
      },
    };
    const path = writeReport("i1-intents", report);
    console.info(JSON.stringify({ ...report, path }));
    expect(large.s6.p99).toBeLessThanOrEqual(P99_MS);
    expect(large.prune.p99).toBeLessThanOrEqual(P99_MS);
    expect(report.ratio.s6).toBeLessThanOrEqual(RATIO);
    expect(report.ratio.prune).toBeLessThanOrEqual(RATIO);
  });
});
