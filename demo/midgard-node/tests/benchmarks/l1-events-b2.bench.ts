/**
 * B2 (plan §16.2): per-block apply at tip, including every projection, at
 * N = 10^4, 10^5 and 10^6 live events on Postgres. The node's projection in
 * N1 is the event projection, so a block is decoded and applied by the
 * follower with it plugged in, exactly as the soak and the node apply one.
 *
 * Workload: N live list Orders preloaded through the ordinary chain-sync
 * apply path (1,000 admissions per block), then blocks of 10 qualifying
 * txs: 4 admissions, 3 continuations (an Order spent and re-created) and 3
 * retirements. Continued and retired Orders come from a uniform sample of
 * every live event, so the measured block touches rows anywhere in N.
 *
 * Targets: p99 ≤ 50 ms at every N; p99(10^6) / p99(10^4) ≤ 1.2. A miss is
 * reported with its numbers and fails the bench; the targets never move.
 * `L1_EVENTS_B2_MAX_N` stops the run at a smaller N (quick runs).
 */
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  decodeBlock,
  type FactStore,
  type OutRef,
} from "@al-ft/midgard-l1-follower";
import {
  encodeBlock,
  Rng,
  SIM_ORIGIN,
  type SimTx,
} from "@al-ft/midgard-l1-follower/testing";
import { afterAll, describe, expect, it } from "vitest";

import { eventProjection, EVENTS_TABLE } from "../../src/l1-events/index.js";
import {
  admissionTx,
  eventOrder,
  EVENTS_CONFIG,
  retirementTx,
} from "../helpers/l1-events-chain.js";
import {
  DROP_ALL_TIMEOUT_MS,
  storeOpener,
  testDatabases,
} from "../helpers/l1-events-store.js";
import {
  buildBenchmarkMeta,
  quantile,
  writeBenchmarkJson,
} from "./benchmark-utils.js";

const K = 2_160;
const STAGES = [10_000, 100_000, 1_000_000].filter(
  (n) => n <= Number(process.env.L1_EVENTS_B2_MAX_N ?? "1000000"),
);
const PRELOAD_PER_BLOCK = 1_000;
const WARMUP_BLOCKS = 30;
const MEASURED_BLOCKS = 300;
const SAMPLE = 4_000;
const P99_TARGET_MS = 50;
const RATIO_TARGET = 1.2;

const outputPath = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "./output/l1-events-b2.json",
);

type Kind = "deposit" | "withdrawal";
type Live = {
  kind: Kind;
  key: string;
  outRef: OutRef;
  order: SimTx["outputs"][number];
};

/** A chain that keeps only its tip: the bench's N would not fit a SimChain. */
class TipChain {
  private slot = SIM_ORIGIN.point.slot;
  private height = SIM_ORIGIN.height;
  private hash = SIM_ORIGIN.point.hash;
  private nonces = 0;

  nonce(): number {
    this.nonces += 1;
    return this.nonces;
  }

  /** A pre-origin input nothing tracks (the nonce an admission consumes). */
  outsideInput(): OutRef {
    const txHash = Buffer.alloc(32);
    txHash.writeUInt32BE(this.nonce(), 28);
    txHash[0] = 0xef;
    return { txHash, index: 0 };
  }

  /** The next block of `txs` as the node serves it (raw CBOR). */
  forward(txs: readonly SimTx[]): { raw: Buffer; txHashes: readonly Buffer[] } {
    const prevHash = this.hash;
    this.height += 1;
    this.slot += 1 + (this.height % 2);
    const encoded = encodeBlock({
      height: this.height,
      slot: this.slot,
      prevHash,
      branch: 0,
      txs,
    });
    this.hash = encoded.hash;
    return { raw: encoded.raw, txHashes: encoded.txHashes };
  }
}

/** A uniform sample of every live event, kept current as the flow runs. */
class LiveSample {
  readonly items: Live[] = [];
  private seen = 0;

  constructor(private readonly rng: Rng) {}

  offer(item: Live): void {
    this.seen += 1;
    if (this.items.length < SAMPLE) this.items.push(item);
    else {
      const slot = this.rng.int(this.seen);
      if (slot < SAMPLE) this.items[slot] = item;
    }
  }

  take(): Live {
    const index = this.rng.int(this.items.length);
    const [item] = this.items.splice(index, 1);
    if (item === undefined) throw new Error("the live sample ran dry");
    return item;
  }
}

const admit = (chain: TipChain, kind: Kind) => {
  const order = eventOrder(kind, chain.outsideInput());
  return { order, tx: admissionTx(order, chain.nonce()) };
};

const liveEvents = async (store: FactStore): Promise<number> =>
  store.transaction("read", async (tx) =>
    Number(
      (
        await tx.query(
          `SELECT count(*) AS n FROM ${EVENTS_TABLE} WHERE retired_slot IS NULL`,
          [],
        )
      )[0]?.n as string | number,
    ),
  );

const round = (value: number): number => Math.round(value * 100) / 100;

type Stage = {
  n: number;
  liveEvents: number;
  liveTrackedOutputs: number;
  preloadMs: number;
  p50Ms: number;
  p95Ms: number;
  p99Ms: number;
  maxMs: number;
  decodeP99Ms: number;
  applyP99Ms: number;
};

const databases = testDatabases();
afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

describe("B2: per-block event apply at tip on Postgres", () => {
  it(
    `p99 ≤ ${P99_TARGET_MS} ms at N = ${STAGES.join(", ")} and flat in N`,
    async () => {
      const store = await storeOpener("postgres", databases)(
        [eventProjection(EVENTS_CONFIG)],
        K,
      );
      const stages: Stage[] = [];
      try {
        const init = await store.initialize(SIM_ORIGIN);
        if (init.kind !== "initialized") throw new Error(`init: ${init.kind}`);
        const rng = new Rng(0xb2_0001);
        const sample = new LiveSample(rng);
        const chain = new TipChain();
        let live = 0;
        let preloadMs = 0;
        let generateMs = 0;

        const block = async (
          txs: SimTx[],
          admitted: readonly {
            order: ReturnType<typeof eventOrder>;
            at: number;
          }[],
        ) => {
          // What applyChainSyncEvent does with a roll-forward, timed in parts.
          const { raw, txHashes } = chain.forward(txs);
          const decodeStart = performance.now();
          const summary = decodeBlock(raw);
          const decodeMs = performance.now() - decodeStart;
          const applyStart = performance.now();
          const result = await store.applyBlock(summary);
          const applyMs = performance.now() - applyStart;
          if (result.kind !== "applied")
            throw new Error(`apply: ${result.kind}`);
          for (const { order, at } of admitted)
            sample.offer({
              kind: order.kind as Kind,
              key: order.key,
              order: order.order,
              outRef: { txHash: txHashes[at] as Buffer, index: 0 },
            });
          return { decodeMs, applyMs, txHashes };
        };

        for (const n of STAGES) {
          const start = performance.now();
          while (live < n) {
            const count = Math.min(PRELOAD_PER_BLOCK, n - live);
            const generateStart = performance.now();
            const admitted = Array.from({ length: count }, (_, at) => ({
              ...admit(chain, at % 2 === 0 ? "deposit" : "withdrawal"),
              at,
            }));
            generateMs += performance.now() - generateStart;
            await block(
              admitted.map((entry) => entry.tx),
              admitted,
            );
            live += count;
          }
          await store.transaction("write", (tx) => tx.query("ANALYZE"));
          preloadMs += performance.now() - start;

          const totals: number[] = [];
          const decodes: number[] = [];
          const applies: number[] = [];
          for (let i = 0; i < WARMUP_BLOCKS + MEASURED_BLOCKS; i += 1) {
            const txs: SimTx[] = [];
            const admitted: {
              order: ReturnType<typeof eventOrder>;
              at: number;
            }[] = [];
            for (let a = 0; a < 4; a += 1) {
              const entry = admit(
                chain,
                a % 2 === 0 ? "deposit" : "withdrawal",
              );
              admitted.push({ order: entry.order, at: txs.length });
              txs.push(entry.tx);
            }
            const continued: { item: Live; at: number }[] = [];
            for (let c = 0; c < 3; c += 1) {
              const item = sample.take();
              continued.push({ item, at: txs.length });
              txs.push({
                inputs: [item.outRef],
                outputs: [item.order],
                nonce: chain.nonce(),
              });
            }
            for (let r = 0; r < 3; r += 1) {
              const item = sample.take();
              txs.push(
                retirementTx(
                  item,
                  item.outRef,
                  item.kind === "deposit" ? "absorbed" : "payout_initialized",
                  chain.nonce(),
                ),
              );
            }
            const { decodeMs, applyMs, txHashes } = await block(txs, admitted);
            for (const { item, at } of continued)
              sample.items.push({
                ...item,
                outRef: { txHash: txHashes[at] as Buffer, index: 0 },
              });
            live += 4 - 3;
            if (i >= WARMUP_BLOCKS) {
              totals.push(decodeMs + applyMs);
              decodes.push(decodeMs);
              applies.push(applyMs);
            }
          }
          const counted = await liveEvents(store);
          expect(counted).toBe(live);
          stages.push({
            n,
            liveEvents: counted,
            liveTrackedOutputs: store.liveOutRefCount(),
            preloadMs: Math.round(preloadMs),
            p50Ms: round(quantile(totals, 0.5)),
            p95Ms: round(quantile(totals, 0.95)),
            p99Ms: round(quantile(totals, 0.99)),
            maxMs: round(Math.max(...totals)),
            decodeP99Ms: round(quantile(decodes, 0.99)),
            applyP99Ms: round(quantile(applies, 0.99)),
          });
          console.info(
            `B2 stage: ${JSON.stringify(stages[stages.length - 1])}`,
          );
        }
        const first = stages[0];
        const last = stages[stages.length - 1];
        const ratio =
          first !== undefined && last !== undefined && last.n === 1_000_000
            ? round(last.p99Ms / first.p99Ms)
            : null;
        const report = {
          meta: buildBenchmarkMeta("l1-events-b2/v1"),
          bench: "B2",
          adapter: "postgres",
          k: K,
          workload: {
            qualifyingTxsPerBlock: 10,
            mix: { admissions: 4, continuations: 3, retirements: 3 },
            preloadAdmissionsPerBlock: PRELOAD_PER_BLOCK,
            warmupBlocks: WARMUP_BLOCKS,
            measuredBlocks: MEASURED_BLOCKS,
            analyzeAfterPreload: true,
            generateMs: Math.round(generateMs),
          },
          stages,
          targets: {
            p99Ms: {
              target: P99_TARGET_MS,
              measured: stages.map((s) => s.p99Ms),
            },
            ratio: { target: RATIO_TARGET, measured: ratio },
          },
        };
        writeBenchmarkJson(outputPath, report, { trailingNewline: true });
        console.info(
          `B2 report: ${outputPath}\n${JSON.stringify(report, null, 2)}`,
        );
        for (const stage of stages)
          expect(stage.p99Ms).toBeLessThanOrEqual(P99_TARGET_MS);
        if (ratio !== null) expect(ratio).toBeLessThanOrEqual(RATIO_TARGET);
      } finally {
        await store.close();
      }
    },
    6 * 3_600_000,
  );
});
