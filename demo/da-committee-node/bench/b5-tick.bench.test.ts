import type { DialectName } from "@al-ft/midgard-l1-follower";
import {
  applyChainSyncEvent,
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
  stepSettled,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  simStoreOptions,
} from "@al-ft/midgard-l1-follower/testing";
import { afterAll, describe, expect, it } from "vitest";

import { type SignedHeader } from "../src/l1/follower/obligations.js";
import {
  committeeProjection,
  readCommitteeView,
} from "../src/l1/follower/projection.js";
import { postgresTestDatabases } from "../tests/helpers/postgres-database.js";
import { QueueChain } from "../tests/l1-follower/queue-chain.js";
import { SIM_QUEUE, SIM_SLOT_TIME } from "../tests/l1-follower/queue-sim.js";

/**
 * B5 (plan §16.2), the tick half: one committee tick on the follower
 * projections at Q = 1,000 queue nodes. A tick applies the next block (its
 * projection maintenance included) and reads the committee view (landed
 * queue, headers awaiting attestation, obligations) in one snapshot, with
 * every queue header signed by the member. Target: p99 ≤ 50 ms.
 *
 * Before the queue is built, a history prefix appends and merges HISTORY
 * headers, one block each, and nothing prunes: every closed queue row and
 * spent output of that history stays in the store. The tick must not slow
 * down with that history.
 *
 * The mutation and readiness halves of B5 belong to C2.
 */
const K = 2_160;
const PARAMETERS = { confirmationDepth: 15, securityParameter: K } as const;
const Q = Number(process.env.COMMITTEE_B5_Q ?? "1000");
const TICKS = Number(process.env.COMMITTEE_B5_TICKS ?? "400");
const HISTORY = Number(process.env.COMMITTEE_B5_HISTORY ?? "5000");
const SIGNED = process.env.COMMITTEE_B5_SIGNED ?? "retained";
const TARGET_P99_MS = 50;

const projection = committeeProjection(SIM_QUEUE);

const now = (): number => Number(process.hrtime.bigint()) / 1e6;

const percentile = (values: readonly number[], p: number): number => {
  const sorted = [...values].sort((a, b) => a - b);
  return sorted[Math.min(sorted.length - 1, Math.ceil(p * sorted.length) - 1)]!;
};

/**
 * The member's signed decisions: every queue header, and each merged header
 * while its merge is at most k deep (plan §11: a decision is kept to k after
 * its header is gone). `COMMITTEE_B5_SIGNED=all` keeps every merged header's
 * decision instead.
 */
const signedOf = (queue: QueueChain): SignedHeader[] => {
  const tip = queue.chain.tip.height;
  const retained =
    SIGNED === "all"
      ? queue.merged
      : queue.merged.filter(({ mergedAt }) => tip - mergedAt + 1 <= K);
  return [...retained, ...queue.nodes].map((node) => ({
    headerHash: node.hash,
    endTimeMs: node.header.endTime,
  }));
};

const apply = async (
  store: FactStore,
  event: Parameters<typeof applyChainSyncEvent>[1],
): Promise<void> => {
  const step = await applyChainSyncEvent(store, event);
  if (!stepSettled(step))
    throw new Error(`apply failed: ${JSON.stringify(step).slice(0, 300)}`);
};

const run = async (store: FactStore, dialect: DialectName) => {
  expect(await store.start()).toMatchObject({ kind: "ready" });
  expect(
    await store.initialize({
      point: SIM_ORIGIN.point,
      height: SIM_ORIGIN.height,
    }),
  ).toMatchObject({ kind: "initialized" });
  const queue = new QueueChain();
  const preloadStart = now();
  await apply(store, queue.init());
  for (let i = 0; i < HISTORY; i += 1) {
    await apply(store, queue.append());
    await apply(store, queue.merge());
  }
  for (let i = 0; i < Q; i += 1) await apply(store, queue.append());
  const preloadMs = now() - preloadStart;
  if (dialect === "postgres")
    await store.transaction("write", (tx) => tx.query("ANALYZE"));
  const options = { parameters: PARAMETERS, slotTime: SIM_SLOT_TIME };
  const ticks: number[] = [];
  const applies: number[] = [];
  const views: number[] = [];
  let lastView = await readCommitteeView(store, {
    ...options,
    signed: signedOf(queue),
  });
  for (let i = 0; i < TICKS; i += 1) {
    // Two attestations per append: the queue grows slowly past Q.
    const event = i % 3 === 0 ? queue.append() : queue.attest();
    const signed = signedOf(queue);
    const start = now();
    await apply(store, event);
    const applied = now();
    lastView = await readCommitteeView(store, { ...options, signed });
    const end = now();
    ticks.push(end - start);
    applies.push(applied - start);
    views.push(end - applied);
  }
  expect(lastView?.queue).toMatchObject({ healthy: true });
  const summary = (values: readonly number[]) => ({
    p50: Number(percentile(values, 0.5).toFixed(2)),
    p99: Number(percentile(values, 0.99).toFixed(2)),
    max: Number(Math.max(...values).toFixed(2)),
  });
  return {
    dialect,
    q: { start: Q, end: queue.nodes.length },
    history: HISTORY,
    signed: signedOf(queue).length,
    signedRule: SIGNED,
    awaitingAtEnd: lastView?.awaiting.length,
    obligationsAtEnd: lastView?.obligations.length,
    preloadMs: Math.round(preloadMs),
    ticks: TICKS,
    tickMs: summary(ticks),
    applyMs: summary(applies),
    viewMs: summary(views),
  };
};

const databases = postgresTestDatabases("midgard_test_committee_b5");
afterAll(async () => {
  await databases.dropAll();
});

describe("B5: committee tick at Q = 1,000", () => {
  it("SQLite", async () => {
    const store = openSqliteFactStore({
      ...simStoreOptions([projection], K, "sqlite"),
      path: ":memory:",
    });
    try {
      const report = await run(store, "sqlite");
      console.log(`B5 ${JSON.stringify(report)}`);
      expect(report.tickMs.p99).toBeLessThanOrEqual(TARGET_P99_MS);
    } finally {
      await store.close();
    }
  });

  it("Postgres", async () => {
    const database = await databases.create();
    const store = openPostgresFactStore({
      ...simStoreOptions([projection], K, "postgres"),
      connection: { connectionString: database.url },
    });
    try {
      const report = await run(store, "postgres");
      console.log(`B5 ${JSON.stringify(report)}`);
      expect(report.tickMs.p99).toBeLessThanOrEqual(TARGET_P99_MS);
    } finally {
      await store.close();
    }
  });
});
