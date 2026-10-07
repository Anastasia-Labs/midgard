import type { DialectName } from "@al-ft/midgard-l1-follower";
import {
  applyChainSyncEvent,
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
  type OutRef,
  stepSettled,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  type SimTx,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, describe, expect, it } from "vitest";

import {
  type SignedHeader,
  slotTimeMs,
} from "../src/l1/follower/obligations.js";
import {
  committeeProjection,
  readCommitteeView,
} from "../src/l1/follower/projection.js";
import { headerHashOf } from "../src/l1/follower/queue-derivation.js";
import { postgresTestDatabases } from "../tests/helpers/postgres-database.js";
import {
  nodeDatum,
  queueOutput,
  rootDatum,
  SIM_QUEUE,
  SIM_SLOT_TIME,
  simHeader,
} from "../tests/l1-follower/queue-sim.js";

/**
 * B5 (plan §16.2), the tick half: one committee tick on the follower
 * projections at Q = 1,000 queue nodes. A tick applies the next block (its
 * projection maintenance included) and reads the committee view (landed
 * queue, headers awaiting attestation, obligations) in one snapshot, with
 * every queue header signed by the member. Target: p99 ≤ 50 ms.
 *
 * The mutation and readiness halves of B5 belong to C2.
 */
const K = 2_160;
const PARAMETERS = { confirmationDepth: 15, securityParameter: K } as const;
const Q = Number(process.env.COMMITTEE_B5_Q ?? "1000");
const TICKS = Number(process.env.COMMITTEE_B5_TICKS ?? "400");
const TARGET_P99_MS = 50;

const GENESIS_HASH = "00".repeat(28);
const projection = committeeProjection(SIM_QUEUE);

const now = (): number => Number(process.hrtime.bigint()) / 1e6;

const percentile = (values: readonly number[], p: number): number => {
  const sorted = [...values].sort((a, b) => a - b);
  return sorted[Math.min(sorted.length - 1, Math.ceil(p * sorted.length) - 1)]!;
};

type Node = {
  outRef: OutRef;
  header: SDK.Header;
  hash: string;
  status: SDK.DaAvailabilityStateQueueStatus;
  link: string | null;
};

/**
 * A linear state queue on the simulator's chain: a root, then one append or
 * one attestation per block. The tail is relinked by each append, as the
 * validator requires.
 */
class QueueChain {
  readonly chain = new SimChain(
    simUniverse(),
    SIM_ORIGIN,
    simStoreOptions([projection], K, "sqlite").trackedSet,
  );
  private root: { outRef: OutRef; link: string | null } | null = null;
  readonly nodes: Node[] = [];
  private attested = 0;

  private forward(tx: SimTx) {
    const step = this.chain.forward([tx]);
    return { event: step.event, txHash: step.encoded.txHashes[0] as Buffer };
  }

  init() {
    const step = this.forward({
      inputs: [this.chain.outsideInput()],
      outputs: [
        queueOutput(
          SDK.STATE_QUEUE_ROOT_ASSET_NAME,
          rootDatum(GENESIS_HASH, 0, null),
        ),
      ],
      nonce: this.chain.nonce(),
    });
    this.root = { outRef: { txHash: step.txHash, index: 0 }, link: null };
    return step.event;
  }

  append() {
    const root = this.root!;
    const tail = this.nodes.at(-1);
    const nonce = this.chain.nonce();
    const header = simHeader(
      nonce,
      tail?.hash ?? GENESIS_HASH,
      slotTimeMs(this.chain.tip.point.slot + 3, SIM_SLOT_TIME),
    );
    const hash = headerHashOf(header);
    const relinked =
      tail === undefined
        ? queueOutput(
            SDK.STATE_QUEUE_ROOT_ASSET_NAME,
            rootDatum(GENESIS_HASH, 0, hash),
          )
        : queueOutput(
            `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${tail.hash}`,
            nodeDatum(tail.header, tail.status, hash),
          );
    const step = this.forward({
      inputs: [tail?.outRef ?? root.outRef],
      outputs: [
        relinked,
        queueOutput(
          `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${hash}`,
          nodeDatum(header, "Unattested", null),
        ),
      ],
      nonce,
    });
    if (tail === undefined)
      this.root = { outRef: { txHash: step.txHash, index: 0 }, link: hash };
    else {
      tail.outRef = { txHash: step.txHash, index: 0 };
      tail.link = hash;
    }
    this.nodes.push({
      outRef: { txHash: step.txHash, index: 1 },
      header,
      hash,
      status: "Unattested",
      link: null,
    });
    return step.event;
  }

  /** Attests the oldest unattested node. */
  attest() {
    const node = this.nodes[this.attested]!;
    this.attested += 1;
    const nonce = this.chain.nonce();
    node.status = {
      Attested: { commitment_hash: nonce.toString(16).padStart(64, "0") },
    };
    const step = this.forward({
      inputs: [node.outRef],
      outputs: [
        queueOutput(
          `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${node.hash}`,
          nodeDatum(node.header, node.status, node.link),
        ),
      ],
      nonce,
    });
    node.outRef = { txHash: step.txHash, index: 0 };
    return step.event;
  }

  signed(): SignedHeader[] {
    return this.nodes.map((node) => ({
      headerHash: node.hash,
      endTimeMs: node.header.endTime,
    }));
  }
}

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
    signed: queue.signed(),
  });
  for (let i = 0; i < TICKS; i += 1) {
    // Two attestations per append: the queue grows slowly past Q.
    const event = i % 3 === 0 ? queue.append() : queue.attest();
    const signed = queue.signed();
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
    signed: queue.nodes.length,
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
