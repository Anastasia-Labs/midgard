/**
 * I1-H1: a refusal the intent journal raises in a worker thread (the commit
 * and settlement workers each run their own journal) reaches the main
 * process's `/readyz` under its named reason. The worker is a real
 * `worker_threads` thread running the workers' journal stack, bundled from
 * source; the main process reads the holds through its own journal's
 * `refresh` (run at every tip by the follower) and the follower readiness.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { Worker } from "node:worker_threads";

import { type FollowStatus } from "@al-ft/midgard-l1-follower";
import { decodeTransaction } from "@al-ft/midgard-l1-follower";
import { encodeSimTx } from "@al-ft/midgard-l1-follower/testing";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { build as bundleWithTsup } from "tsup";
import { afterAll, beforeAll, expect, it } from "vitest";

import {
  INTENT_CONTENT_REF_MISSING,
  intentJournalOver,
} from "../src/services/intent-journal.js";
import {
  type L1FollowerHandle,
  l1FollowerReadiness,
} from "../src/services/l1-follower.readiness.js";
import { db } from "./helpers/forced-orders-node-store.js";
import type {
  IntentJournalWorkerProbeInput,
  IntentJournalWorkerProbeResult,
} from "./helpers/intent-journal-worker-probe.js";
import { QUEUE_ADDRESS } from "./helpers/state-queue-sim.fixtures.js";
import { resetApplicationTables } from "./utils.js";

const packageRoot = join(import.meta.dirname, "..");
let bundleDir: string;

beforeAll(async () => {
  bundleDir = await mkdtemp(join(packageRoot, ".intent-journal-worker-"));
  // A worker thread resolves workspace packages through their dist; bundle
  // them from source, as tests/settlement-worker-heap.test.ts does.
  await bundleWithTsup({
    entry: [join(packageRoot, "tests/helpers/intent-journal-worker-probe.ts")],
    format: ["esm"],
    platform: "node",
    target: "node22",
    outDir: bundleDir,
    config: false,
    splitting: false,
    silent: true,
    noExternal: [/^@al-ft\//],
    loader: { ".sql": "text" },
    banner: {
      js: 'import { createRequire as __createRequire } from "node:module"; const require = __createRequire(import.meta.url);',
    },
    esbuildOptions(options) {
      options.conditions = ["midgard-source", ...(options.conditions ?? [])];
    },
  });
}, 300_000);
afterAll(async () => {
  if (bundleDir !== undefined)
    await rm(bundleDir, { recursive: true, force: true });
});

const runProbe = async (
  input: IntentJournalWorkerProbeInput,
): Promise<IntentJournalWorkerProbeResult> => {
  const worker = new Worker(join(bundleDir, "intent-journal-worker-probe.js"), {
    workerData: input,
  });
  try {
    return await new Promise((resolve, reject) => {
      worker.once("message", resolve);
      worker.once("error", reject);
      worker.once("exit", (code) =>
        reject(new Error(`probe exited (${code}) without a result`)),
      );
    });
  } finally {
    await worker.terminate();
  }
};

const following: FollowStatus = {
  state: "following",
  interventions: [],
  waiting: null,
  stuck: null,
  protocolInit: "unknown",
  cursor: null,
  tip: null,
  atTip: true,
  node: null,
  replaying: false,
  events: 0,
  lastError: null,
  prune: {
    steps: 0,
    prunedThroughSlot: null,
    lastError: null,
    failures: 0,
    floorLagSlots: null,
  },
  readiness: [],
};

const signed = (nonce: number) => {
  const cbor = encodeSimTx({
    inputs: [{ txHash: Buffer.alloc(32, nonce), index: 0 }],
    outputs: [{ address: QUEUE_ADDRESS, lovelace: 2_000_000n }],
    nonce,
  });
  return {
    signedTxCbor: Buffer.from(cbor).toString("hex"),
    txHash: decodeTransaction(cbor).hash.toString("hex"),
  };
};

it("commit and settlement refusals raised in a worker thread fail the main process's /readyz by name", async () => {
  await db(resetApplicationTables);
  const commit = signed(1);
  const settlement = signed(2);
  // Each family names its content; these name none, so the journal refuses.
  expect(
    await runProbe([
      { family: "commit", workflowKey: "commit:probe", ...commit },
      { family: "settlement", workflowKey: "settlement:probe", ...settlement },
    ]),
  ).toEqual([INTENT_CONTENT_REF_MISSING, INTENT_CONTENT_REF_MISSING]);

  await db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const main = intentJournalOver(sql, () => false);
      const handle: L1FollowerHandle = {
        kind: "running",
        status: () => following,
        holds: main.holds,
        planCurrent: () => Promise.resolve({ kind: "none", detail: "" }),
      };
      // The worker's refusals live in its own memory until the main process
      // re-reads the table, which the follower does at every tip.
      expect(l1FollowerReadiness(handle).reasons).toEqual([]);
      yield* main.refresh();
      const readiness = l1FollowerReadiness(handle);
      expect(readiness.reasons).toEqual([INTENT_CONTENT_REF_MISSING]);
      expect(
        (readiness.report.readiness as readonly { detail: string }[]).map(
          ({ detail }) => detail.split(":")[0],
        ),
      ).toEqual(["commit commit", "settlement settlement"]);
    }),
  );
}, 300_000);
